module Poreus.CLI
  ( run
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Monad (unless)
import Database.SQLite.Simple (Connection)
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as OAP
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (BufferMode (..), hFlush, hPutStrLn, hSetBuffering, stderr, stdout)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)

import Poreus.Config (poreusHome)
import qualified Poreus.DB as DB
import Poreus.Effects.Env (getCurrentDir)
import Poreus.Effects.Random (randomHex4)
import Poreus.Effects.Time (currentTime)
import qualified Poreus.Endpoint as Endpoint
import qualified Poreus.Exit as Exit
import qualified Poreus.History as History
import qualified Poreus.Inbox as Inbox
import qualified Poreus.Inspect as Inspect
import qualified Poreus.JSON as J
import qualified Poreus.Lock as Lock
import qualified Poreus.Message as Msg
import Poreus.Message (Message (..), MessageKind (..), parseMessageKind)
import qualified Poreus.Profile as Profile
import qualified Poreus.Repo as Repo
import Poreus.Time (Timestamp (..), parseUtcLoose)
import Poreus.Types

-- ---------------------------------------------------------------------
-- Command algebra
-- ---------------------------------------------------------------------

data Cmd
  = CmdInit
  | CmdRegister Alias Text
  | CmdPutProfile Alias
  | CmdInspectRepo (Maybe FilePath)
  | CmdDiscover (Maybe Text) (Maybe Text) (Maybe Alias)
  | CmdMatch Text (Maybe Text) (Maybe Text)
  | CmdSend
  | CmdInbox InboxOpts
  | CmdHistory (Maybe Alias) Int Bool
  deriving stock (Show, Eq)

data InboxOpts = InboxOpts
  { ioFollow :: !Bool
  , ioTakeover :: !Bool
  , ioKind :: !(Maybe MessageKind)
  , ioInReplyTo :: !(Maybe TaskId)
  , ioFrom :: !(Maybe Alias)
  , ioSince :: !(Maybe Timestamp)
  , ioAlias :: !(Maybe Alias)
  }
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------
-- Send input
-- ---------------------------------------------------------------------

data SendInput = SendInput
  { siTo :: !Alias
  , siKind :: !MessageKind
  , siInReplyTo :: !(Maybe TaskId)
  , siSubscribe :: !(Maybe [Text])
  , siPayload :: !(Maybe A.Value)
  }
  deriving stock (Show, Eq)

instance A.FromJSON SendInput where
  parseJSON = A.withObject "SendInput" $ \o ->
    SendInput
      <$> o A..: "to"
      <*> o A..: "kind"
      <*> o A..:? "in_reply_to"
      <*> o A..:? "subscribe"
      <*> o A..:? "payload"

-- ---------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------

parser :: Parser Cmd
parser =
  hsubparser $
    mconcat
      [ command "init" (info (pure CmdInit) (progDesc "Create $POREUS_HOME and apply schema"))
      , command "register" (info registerP (progDesc "Register an agent alias at a path"))
      , command "put-profile" (info putProfileP (progDesc "Replace summary/tags/endpoints from stdin JSON" <> footerDoc (Just putProfileFooter)))
      , command "inspect-repo" (info inspectRepoP (progDesc "Emit signals about the target repo"))
      , command "discover" (info discoverP (progDesc "List agents with their endpoints"))
      , command "match-endpoint" (info matchP (progDesc "Find agents offering a given verb"))
      , command "send" (info (pure CmdSend) (progDesc "Send a message (read JSON from stdin)" <> footerDoc (Just sendFooter)))
      , command "inbox" (info inboxP (progDesc "Read messages addressed to an alias (snapshot or follow)" <> footerDoc (Just inboxFooter)))
      , command "history" (info historyP (progDesc "Show recent messages (table or JSON)"))
      ]

-- ---------------------------------------------------------------------
-- Footers — JSON shapes for stdin-consuming subcommands
-- ---------------------------------------------------------------------

literalDoc :: [String] -> OAP.Doc
literalDoc = OAP.vsep . map OAP.pretty

sendFooter :: OAP.Doc
sendFooter = literalDoc
  [ "stdin JSON (SendInput):"
  , "  { \"to\":          \"<alias>\","
  , "    \"kind\":        \"request\" | \"notice\","
  , "    \"in_reply_to\": \"<message-id>\" | null,           // optional"
  , "    \"subscribe\":   [\"started\", \"completed\", ...] // optional, request only"
  , "    \"payload\":     { ... }                            // optional, default {}"
  , "  }"
  , ""
  , "Prints the created message JSON on stdout."
  , "Validation: subscribe is only allowed when kind=\"request\"."
  ]

putProfileFooter :: OAP.Doc
putProfileFooter = literalDoc
  [ "stdin JSON (ProfileInput):"
  , "  { \"summary\": \"one-line agent description\","
  , "    \"tags\":    [\"tag1\", \"tag2\"],"
  , "    \"endpoints\": ["
  , "      { \"verb\":        \"<verb>\","
  , "        \"description\": \"...\","
  , "        \"autonomy\":    \"auto\" | \"confirm\","
  , "        \"arg_schema\":  null,                              // optional JSON Schema"
  , "        \"param_schema\":null                               // optional JSON Schema"
  , "      }"
  , "    ]"
  , "  }"
  , ""
  , "Fully replaces the agent's profile (summary + tags + endpoint set)."
  ]

inboxFooter :: OAP.Doc
inboxFooter = literalDoc
  [ "Snapshot mode (default):"
  , "  Lists messages addressed to <alias>. Filters: --kind, --in-reply-to,"
  , "  --from, --since. JSON array on stdout."
  , ""
  , "Follow mode (-f, --follow):"
  , "  Long-running watcher. Single instance per (alias, Claude session)"
  , "  enforced by flock + $CLAUDE_CODE_SSE_PORT. Emits [POREUS:IN] ..."
  , "  lines. Exit codes:"
  , "    64 EX_FOLLOW_ALREADY  — already running for this Claude session"
  , "    65 EX_FOLLOW_FOREIGN  — held by another session; pass --takeover"
  ]

-- ---------------------------------------------------------------------
-- Sub-parsers
-- ---------------------------------------------------------------------

registerP :: Parser Cmd
registerP =
  CmdRegister
    <$> argument aliasR (metavar "ALIAS")
    <*> argument textR (metavar "PATH")

putProfileP :: Parser Cmd
putProfileP = CmdPutProfile <$> argument aliasR (metavar "ALIAS")

inspectRepoP :: Parser Cmd
inspectRepoP =
  CmdInspectRepo
    <$> optional (strOption (long "path" <> metavar "DIR" <> help "Target repo (default: cwd)"))

discoverP :: Parser Cmd
discoverP =
  CmdDiscover
    <$> optional (strOption (long "tag" <> metavar "T"))
    <*> optional (strOption (long "verb" <> metavar "V"))
    <*> optional (aliasOption (long "agent" <> metavar "A"))

matchP :: Parser Cmd
matchP =
  CmdMatch
    <$> strOption (long "verb" <> metavar "V" <> help "Verb to match")
    <*> optional (strOption (long "arg" <> metavar "A"))
    <*> optional (strOption (long "tag" <> metavar "T"))

inboxP :: Parser Cmd
inboxP = CmdInbox <$> inboxOptsP

inboxOptsP :: Parser InboxOpts
inboxOptsP =
  InboxOpts
    <$> switch (short 'f' <> long "follow" <> help "Long-running stream mode")
    <*> switch (long "takeover" <> help "Reclaim a follow lock held by another session")
    <*> optional (option kindR (long "kind" <> metavar "request|notice"))
    <*> optional (option taskIdR (long "in-reply-to" <> metavar "MSG-ID"))
    <*> optional (aliasOption (long "from" <> metavar "A"))
    <*> optional (option timestampR (long "since" <> metavar "ISO8601"))
    <*> optional (aliasOption (long "alias" <> metavar "A" <> help "Override alias (default: cwd)"))

historyP :: Parser Cmd
historyP =
  CmdHistory
    <$> optional (aliasOption (long "alias" <> metavar "A"))
    <*> option
          auto
          ( long "limit"
              <> metavar "N"
              <> value 10
              <> showDefault
              <> help "Maximum rows (default 10)"
          )
    <*> switch (long "json" <> help "Emit JSON array instead of markdown table")

aliasR :: ReadM Alias
aliasR = Alias <$> textR

taskIdR :: ReadM TaskId
taskIdR = TaskId <$> textR

textR :: ReadM Text
textR = T.pack <$> str

aliasOption :: Mod OptionFields Alias -> Parser Alias
aliasOption m = option aliasR m

kindR :: ReadM MessageKind
kindR = do
  t <- T.pack <$> str
  case parseMessageKind t of
    Just k -> pure k
    Nothing -> readerError ("invalid kind: " <> T.unpack t)

timestampR :: ReadM Timestamp
timestampR = do
  t <- T.pack <$> str
  case parseUtcLoose t of
    Just v -> pure (Timestamp v)
    Nothing -> readerError ("invalid timestamp: " <> T.unpack t)

opts :: ParserInfo Cmd
opts =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Deterministic CLI for agent-to-agent message transport"
        <> header "poreus — single binary for local message-bus delivery"
    )

-- ---------------------------------------------------------------------
-- Entry
-- ---------------------------------------------------------------------

run :: IO ()
run = do
  args <- getArgs
  case execParserPure defaultPrefs opts args of
    Success cmd -> dispatch cmd
    Failure f -> do
      progn <- getProgName
      let (msg, ec) = renderFailure f progn
      case ec of
        ExitSuccess -> do
          putStrLn msg
          exitSuccess
        ExitFailure _ -> do
          hPutStrLn stderr msg
          exitWith (ExitFailure (Exit.exitCodeOf Exit.ExitBadArgs))
    CompletionInvoked cr -> do
      progn <- getProgName
      msg <- execCompletion cr progn
      putStr msg
      exitSuccess

dispatch :: Cmd -> IO ()
dispatch = \case
  CmdInit -> cmdInit
  CmdRegister alias path -> cmdRegister alias path
  CmdPutProfile alias -> cmdPutProfile alias
  CmdInspectRepo mp -> cmdInspectRepo mp
  CmdDiscover tag verb mAgent -> cmdDiscover tag verb mAgent
  CmdMatch verb marg mtag -> cmdMatch verb marg mtag
  CmdSend -> cmdSend
  CmdInbox o -> cmdInbox o
  CmdHistory malias limit jsonF -> cmdHistory malias limit jsonF

-- ---------------------------------------------------------------------
-- Command implementations
-- ---------------------------------------------------------------------

now :: IO Timestamp
now = Timestamp <$> currentTime

cmdInit :: IO ()
cmdInit = do
  home <- poreusHome
  cwd <- getCurrentDir
  root <- Repo.repoRoot cwd
  alias <- Alias <$> Repo.repoAlias cwd
  ts <- now
  DB.withDB $ \c -> do
    DB.migrate c
    _ <- Profile.registerAgent c alias (T.pack root) ts
    J.emitJSON $
      A.object
        [ "home" A..= home
        , "alias" A..= alias
        , "path" A..= T.pack root
        ]

cmdRegister :: Alias -> Text -> IO ()
cmdRegister alias path = do
  ts <- now
  DB.withDB $ \c -> do
    DB.migrate c
    (reg, upd) <- Profile.registerAgent c alias path ts
    J.emitJSON $
      A.object
        [ "alias" A..= alias
        , "path" A..= path
        , "registered_at" A..= reg
        , "updated_at" A..= upd
        ]

cmdPutProfile :: Alias -> IO ()
cmdPutProfile alias = do
  raw <- BL.getContents
  case A.eitherDecode' raw of
    Left err -> Exit.exitJsonError Exit.ExitBadArgs (T.pack ("invalid profile JSON: " <> err))
    Right (profile :: Profile.ProfileInput) -> do
      ts <- now
      DB.withDB $ \c -> do
        DB.migrate c
        magent <- Endpoint.loadAgent c alias
        case magent of
          Nothing ->
            Exit.exitJsonError
              Exit.ExitNotFound
              ("agent not registered: " <> unAlias alias)
          Just _ -> do
            (n, upd) <- Profile.putProfile c alias profile ts
            J.emitJSON $
              A.object
                [ "alias" A..= alias
                , "endpoints_count" A..= n
                , "updated_at" A..= upd
                ]

cmdInspectRepo :: Maybe FilePath -> IO ()
cmdInspectRepo mp = do
  dir <- maybe getCurrentDir pure mp
  r <- Inspect.inspectRepo dir
  J.emitJSON r

cmdDiscover :: Maybe Text -> Maybe Text -> Maybe Alias -> IO ()
cmdDiscover mtag mverb mAgent = DB.withDB $ \c -> do
  DB.migrate c
  agents <- case mAgent of
    Just a -> maybe [] (: []) <$> Endpoint.loadAgent c a
    Nothing -> Endpoint.loadAllAgents c
  let filtered = filter byVerb . filter byTag $ agents
  J.emitJSON filtered
  where
    byTag a = case mtag of
      Nothing -> True
      Just t -> t `elem` agentTags a
    byVerb a = case mverb of
      Nothing -> True
      Just v -> any (\e -> endpointVerb e == v) (agentEndpoints a)

cmdMatch :: Text -> Maybe Text -> Maybe Text -> IO ()
cmdMatch verb _marg mtag = DB.withDB $ \c -> do
  DB.migrate c
  agents <- Endpoint.agentsMatchingVerb c verb
  let filtered = case mtag of
        Nothing -> agents
        Just t -> filter (\a -> t `elem` agentTags a) agents
      candidates =
        [ A.object
            [ "alias" A..= agentAlias a
            , "endpoint" A..= e
            , "summary" A..= agentSummary a
            , "tags" A..= agentTags a
            ]
        | a <- filtered
        , e <- Endpoint.matchEndpoints a verb
        ]
  J.emitJSON candidates

-- | Validate input → generate id → register sender (idempotent) →
-- insert message → emit JSON.
cmdSend :: IO ()
cmdSend = do
  raw <- BL.getContents
  case A.eitherDecode' raw of
    Left err -> Exit.exitJsonError Exit.ExitBadArgs (T.pack ("invalid send JSON: " <> err))
    Right input -> validateAndSend input
  where
    validateAndSend SendInput {..} = do
      case (siKind, siSubscribe) of
        (MKNotice, Just _) ->
          Exit.exitJsonError
            Exit.ExitBadArgs
            "subscribe is only allowed on kind=request"
        _ -> pure ()
      ts <- now
      hex <- randomHex4
      cwd <- getCurrentDir
      root <- Repo.repoRoot cwd
      from <- Alias <$> Repo.repoAlias cwd
      let mid = Msg.newMessageId from ts hex
          payload = fromMaybe (A.Object mempty) siPayload
          message =
            Message
              { msgId = mid
              , msgFrom = from
              , msgTo = siTo
              , msgKind = siKind
              , msgInReplyTo = siInReplyTo
              , msgPayload = payload
              , msgSubscribe = siSubscribe
              , msgCreatedAt = ts
              }
      DB.withDB $ \c -> do
        DB.migrate c
        _ <- Profile.registerAgent c from (T.pack root) ts
        Msg.insertMessage c message
        J.emitJSON message

-- | Snapshot: read messages with filters; or follow: long-running
-- single-instance stream.
cmdInbox :: InboxOpts -> IO ()
cmdInbox o = do
  alias <- case ioAlias o of
    Just a -> pure a
    Nothing -> Alias <$> Repo.cwdAlias
  if ioFollow o
    then cmdInboxFollow alias (ioTakeover o)
    else cmdInboxSnapshot alias o

cmdInboxSnapshot :: Alias -> InboxOpts -> IO ()
cmdInboxSnapshot alias o = DB.withDB $ \c -> do
  DB.migrate c
  msgs <-
    Inbox.inboxSnapshot
      c
      alias
      Inbox.InboxFilters
        { Inbox.ifKind = ioKind o
        , Inbox.ifInReplyTo = ioInReplyTo o
        , Inbox.ifFrom = ioFrom o
        , Inbox.ifSince = ioSince o
        }
  J.emitJSON msgs

-- | Long-running follow mode: acquire flock+session-token, install
-- signal handlers, loop until SIGTERM/SIGINT, release lock on exit.
cmdInboxFollow :: Alias -> Bool -> IO ()
cmdInboxFollow alias takeover = do
  result <- Lock.acquireFollowLock alias takeover
  case result of
    Lock.OwnedByMe owner ->
      Exit.exitJsonError
        Exit.ExitFollowAlready
        ( "inbox -f already running for this Claude session (pid="
            <> T.pack (show (Lock.ownerPid owner))
            <> ")"
        )
    Lock.OwnedByOther owner ->
      Exit.exitJsonError
        Exit.ExitFollowForeign
        ( "inbox -f held by session token="
            <> Lock.ownerToken owner
            <> " (pid="
            <> T.pack (show (Lock.ownerPid owner))
            <> "); pass --takeover to claim"
        )
    Lock.Acquired handle ->
      bracket_
        (pure ())
        (Lock.releaseFollowLock handle)
        (DB.withDB $ \c -> DB.migrate c >> followLoop c alias)

-- | Stream loop: tick every 5 seconds, emit `[POREUS:IN]` lines for
-- new messages, exit on SIGTERM/SIGINT.
followLoop :: Connection -> Alias -> IO ()
followLoop c alias = do
  hSetBuffering stdout LineBuffering
  shutdownRef <- newIORef False
  let onSignal = atomicWriteIORef shutdownRef True
  _ <- installHandler sigTERM (Catch onSignal) Nothing
  _ <- installHandler sigINT (Catch onSignal) Nothing
  loop shutdownRef
  where
    loop ref = do
      shutting <- readIORef ref
      unless shutting $ do
        ts <- now
        msgs <- Inbox.inboxStreamTick c alias ts
        mapM_ (TIO.putStrLn . Inbox.formatInboxLine) msgs
        hFlush stdout
        sleepInterruptibly ref 5_000_000
        loop ref

sleepInterruptibly :: IORef Bool -> Int -> IO ()
sleepInterruptibly ref totalUs = go totalUs
  where
    go remaining
      | remaining <= 0 = pure ()
      | otherwise = do
          shutting <- readIORef ref
          unless shutting $ do
            let step = min 100_000 remaining
            threadDelay step
            go (remaining - step)

cmdHistory :: Maybe Alias -> Int -> Bool -> IO ()
cmdHistory malias limit jsonF = DB.withDB $ \c -> do
  DB.migrate c
  alias <- case malias of
    Just a -> pure a
    Nothing -> Alias <$> Repo.cwdAlias
  msgs <- History.historyMessages c alias limit
  let rows = map (History.toHistoryRow alias) msgs
  if jsonF
    then J.emitJSON rows
    else TIO.putStr (History.formatHistoryTable rows)
