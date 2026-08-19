module Poreus.Doctor
  ( -- * Cross-checks between poreus, the OS, and the host
    Severity (..)
  , Finding (..)
  , renderFinding
  , diagnose
  , runDoctor
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (sortOn)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.SQLite.Simple (Connection, Only (..), query_)

import System.Exit (ExitCode (..), exitWith)

import Poreus.Config (dbPath)
import Poreus.DB (withDB)
import Poreus.Deliver (pendingCount)
import Poreus.Effects.Env (CanEnv)
import Poreus.Effects.FileSystem (CanFileSystem, getFileSize)
import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.HostSession (HostSession (..), listHostSessions)
import Poreus.Name (NameRow (..), listNames)
import Poreus.Session (SessionRow (..), hostPidsByAddress, listSessions, sessionLive)
import Poreus.Time (parseUtcLoose)
import Poreus.Types

-- | Note [What doctor is for]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Every fact two parties can answer is a fact that can drift, and
-- v0.3's failures were all drift nobody watched for: a stored
-- heartbeat that disagreed with the operating system for 45 hours, a
-- name binding orphaned by every resume, and a retention sweep that
-- had silently stopped and surfaced days later as a 4.1 MB
-- write-ahead log.
--
-- ADR-0017 deletes the facts poreus should never have stored. Doctor
-- covers what is left by asking both parties and reporting the
-- disagreement, naming both sides. It reports; it never repairs. A
-- check that fixes things quietly is how drift becomes invisible
-- again.
--
-- It is a CLI subcommand rather than an MCP tool. The original
-- argument for a tool was that reading host state needed a model in
-- the loop to call ListAgents; the host session file removed that
-- constraint, since `status` is readable from disk. An operator check
-- belongs where an operator is looking.
data Severity = SevOk | SevWarn | SevError
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Finding = Finding
  { fSeverity :: !Severity
  , fCheck :: !Text
  , fDetail :: !Text
  }
  deriving stock (Show, Eq)

severityLabel :: Severity -> Text
severityLabel = \case
  SevOk -> "ok   "
  SevWarn -> "warn "
  SevError -> "ERROR"

renderFinding :: Finding -> Text
renderFinding f = severityLabel (fSeverity f) <> "  " <> pad (fCheck f) <> "  " <> fDetail f
  where
    pad t = t <> T.replicate (max 0 (10 - T.length t)) " "

-- | `poreus doctor`: one line per finding, worst first, and a non-zero
-- exit when anything disagreed.
runDoctor :: IO ()
runDoctor = do
  findings <- withDB diagnose
  mapM_ (TIO.putStrLn . renderFinding) findings
  when (any ((== SevError) . fSeverity) findings) (exitWith (ExitFailure 1))

-- | How stale the host's own status may get on a live process before
-- it is worth mentioning. Generous: an idle session legitimately sits
-- untouched for hours.
statusStaleSeconds :: Double
statusStaleSeconds = 86400

-- | A write-ahead log this large means checkpoints are not happening,
-- which in v0.3 meant the sweep had died.
walWarnBytes :: Integer
walWarnBytes = 4 * 1024 * 1024

diagnose ::
  (CanTime m, CanSystemInfo m, CanEnv m, CanFileSystem m, MonadIO m) =>
  Connection ->
  m [Finding]
diagnose c = do
  now <- currentTime
  hostRows <- listHostSessions
  identityMap <- hostPidsByAddress c
  sessions <- filter (isNothing . sessEndedAt) <$> listSessions c
  live <- mapM (\r -> (,) r <$> sessionLive r) sessions
  names <- listNames c
  backlog <- mapM (\nr -> (,) nr <$> pendingCount c (MailboxRole (nameName nr))) names
  sweepF <- sweepFinding c now
  walF <- walFinding
  let hostFileOf row = do
        pid <- lookup (sessAddress row) identityMap
        (,) pid <$> lookup pid hostRows
  pure . sortOn (negate . fromEnum . fSeverity) . concat $
    [ concatMap (presenceFindings hostFileOf) live
    , [statusFinding now hostFileOf r | (r, True) <- live]
    , [backlogFinding nr n | (nr, n) <- backlog, n > 0]
    , [sweepF, walF]
    ]

presenceFindings ::
  (SessionRow -> Maybe (Int, HostSession)) ->
  (SessionRow, Bool) ->
  [Finding]
presenceFindings hostFileOf (row, alive)
  | not alive = []
  | otherwise = case (sessPid row, hostFileOf row) of
      (Nothing, _) ->
        [ Finding
            SevWarn
            "presence"
            ( label (currentName row) row
                <> " reads live, but no serving process ever recorded a pid — only a hook has spoken for it, so poreus cannot corroborate it against the OS"
            )
        ]
      (Just pid, Nothing) ->
        [ Finding
            SevError
            "presence"
            ( label Nothing row
                <> " reads live on serve pid "
                <> T.pack (show pid)
                <> ", but the host publishes no session file for the claude process it belongs to"
            )
        ]
      (Just _, Just _) -> []
  where
    currentName r = hsName . snd =<< hostFileOf r

-- | Replaces v0.3's stale-heartbeat check, with the important
-- difference that the staleness now belongs to the host: poreus writes
-- no heartbeat, so a stalled `statusUpdatedAt` is the host's to
-- explain rather than a symptom poreus produced.
--
-- The host-name lease is checked here too, on the same file read.
statusFinding ::
  UTCTime ->
  (SessionRow -> Maybe (Int, HostSession)) ->
  SessionRow ->
  Finding
statusFinding now hostFileOf row = case snd <$> hostFileOf row of
  Nothing -> Finding SevOk "host-name" (label Nothing row <> " has no host session file to compare against")
  Just hs
    | hsName hs /= sessHostName row ->
        Finding
          SevError
          "host-name"
          ( label (hsName hs) row
              <> " has a stale lease: poreus stored "
              <> shown (sessHostName row)
              <> ", so the doorbell would ring that name until the next hook invocation renews the lease"
          )
    | otherwise -> case hsStatusUpdatedAt hs of
        Nothing -> Finding SevOk "status" (label (hsName hs) row <> " publishes no status timestamp")
        Just ms
          | age ms > statusStaleSeconds ->
              Finding
                SevWarn
                "status"
                ( label (hsName hs) row
                    <> " is alive but the host last updated its status "
                    <> T.pack (show (round (age ms / 3600) :: Integer))
                    <> " h ago"
                )
          | otherwise -> Finding SevOk "status" (label (hsName hs) row <> " agrees with the host")
  where
    age ms = realToFrac (diffUTCTime now (posixSecondsToUTCTime (fromIntegral ms / 1000)))
    shown = maybe "(none)" (\t -> "'" <> t <> "'")

-- | Queued mail nobody is draining. Only an error when no session
-- holds the role at all — a role with a live holder that has not read
-- yet is the ordinary case between turns.
backlogFinding :: NameRow -> Int -> Finding
backlogFinding nr n =
  Finding
    (if unheld then SevWarn else SevOk)
    "backlog"
    ( "role '"
        <> unAgentName (nameName nr)
        <> "' has "
        <> T.pack (show n)
        <> " undelivered message(s)"
        <> (if unheld then " and no session holds it" else "; its holder has not read them yet")
    )
  where
    unheld = isNothing (nameBoundSession nr)

sweepFinding :: MonadIO m => Connection -> UTCTime -> m Finding
sweepFinding c now = do
  rows <- liftIO $ query_ c "SELECT value FROM maintenance WHERE key = 'last_sweep'"
  pure $ case rows of
    [] -> Finding SevWarn "retention" "no sweep has ever run against this store"
    (Only t : _) -> case parseUtcLoose t of
      Nothing -> Finding SevWarn "retention" ("last_sweep is unparseable: " <> t)
      Just prev ->
        let hours = realToFrac (diffUTCTime now prev) / 3600 :: Double
         in if hours > 24
              then
                Finding
                  SevWarn
                  "retention"
                  ("the last sweep ran " <> T.pack (show (round hours :: Integer)) <> " h ago; the hook sweeps hourly, so nothing has run a hook since")
              else Finding SevOk "retention" "a sweep ran within the last day"

walFinding :: (CanEnv m, CanFileSystem m) => m Finding
walFinding = do
  path <- dbPath
  msize <- getFileSize (path <> "-wal")
  pure $ case msize of
    Nothing -> Finding SevOk "wal" "no write-ahead log on disk"
    Just n
      | n > walWarnBytes ->
          Finding
            SevWarn
            "wal"
            ( "the write-ahead log is "
                <> T.pack (show (n `div` (1024 * 1024)))
                <> " MB; checkpoints are not keeping up"
            )
      | otherwise -> Finding SevOk "wal" (T.pack (show (n `div` 1024)) <> " KB")

-- | How a finding names the session it is about.
--
-- Note [Never identify a session by the lease]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The name comes from the host file, never from `sessions.host_name`.
-- The first version read the lease, so the one finding whose entire
-- job is to report "this stored name is stale" opened with the stale
-- name — while the correct one sat in scope on the same line. A wrong
-- label that happens to agree with the reader's current guess is worse
-- than an absent one: it hands over the confirmation they were already
-- looking for.
label :: Maybe Text -> SessionRow -> Text
label mcurrent row =
  "session "
    <> unSessionAddress (sessAddress row)
    <> maybe "" (\n -> " (the host calls it '" <> n <> "')") mcurrent
