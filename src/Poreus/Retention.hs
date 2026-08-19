module Poreus.Retention
  ( -- * Unified retention (MAINT-1, simplification E)
    defaultRetentionDays
  , retentionDays
  , SweepResult (..)
  , sweep
  , sweepIfDue
  , sweepIntervalSeconds
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Data.Time (addUTCTime, diffUTCTime)
import Database.SQLite.Simple (Connection, Only (..), changes, execute, query)
import Text.Read (readMaybe)

import Poreus.Effects.Env (CanEnv, lookupEnvVar)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.Time (Timestamp (..), formatUtc, parseUtcLoose)

-- | One age-based window governs everything ephemeral: messages and
-- ended sessions' records expire together. Generous by default so late
-- replies stay inspectable and role successors can adopt stranded
-- requests (RECV-4) across typical succession gaps. Names and profiles
-- are never retained away — only explicitly retired (REG-6).
defaultRetentionDays :: Int
defaultRetentionDays = 30

-- | \$POREUS_RETENTION_DAYS override; non-numeric or non-positive values
-- fall back to the default.
retentionDays :: CanEnv m => m Int
retentionDays = do
  env <- lookupEnvVar "POREUS_RETENTION_DAYS"
  pure $ case env >>= readMaybe of
    Just n | n > 0 -> n
    _ -> defaultRetentionDays

data SweepResult = SweepResult
  { swMessagesDeleted :: !Int
  , swSessionsDeleted :: !Int
  , swHostSessionsDeleted :: !Int
  , swCursorsDeleted :: !Int
  }
  deriving stock (Show, Eq)

instance ToJSON SweepResult where
  toJSON r =
    object
      [ "messages_deleted" .= swMessagesDeleted r
      , "sessions_deleted" .= swSessionsDeleted r
      , "host_sessions_deleted" .= swHostSessionsDeleted r
      , "cursors_deleted" .= swCursorsDeleted r
      ]

-- | Delete everything older than the window: messages by creation
-- time; sessions ended (or last heard from) before the cutoff — their
-- name bindings reset to NULL (the name and profile survive); stale
-- host-session identity mappings; and cursors whose mailbox no longer
-- exists in either namespace.
--
-- The cursor cleanup is new in v0.4 and is not cosmetic. `cursors` lost
-- its foreign key when mailboxes moved to roles, because a role
-- mailbox has no `sessions` row for a cascade to follow. Without this
-- delete, every retired role and every swept session would leave a row
-- behind forever.
--
-- Runs from `admin purge`, from the purge tool, and — hourly at most —
-- from the hook path via 'sweepIfDue'.
sweep :: (CanTime m, MonadIO m) => Connection -> Int -> m SweepResult
sweep c days = do
  now <- currentTime
  let cutoff = Timestamp (addUTCTime (negate (fromIntegral days * 86400)) now)
  liftIO $ do
    execute c "DELETE FROM messages WHERE created_at < ?" (Only cutoff)
    nMsgs <- changes c
    execute
      c
      "DELETE FROM sessions WHERE (ended_at IS NOT NULL AND ended_at < ?) OR last_seen_at < ?"
      (cutoff, cutoff)
    nSessions <- changes c
    execute c "DELETE FROM host_sessions WHERE updated_at < ?" (Only cutoff)
    nHost <- changes c
    execute
      c
      "DELETE FROM cursors\n\
      \WHERE mailbox NOT IN (SELECT address FROM sessions)\n\
      \  AND mailbox NOT IN (SELECT name FROM names)"
      ()
    nCursors <- changes c
    pure (SweepResult nMsgs nSessions nHost nCursors)

-- | At most one sweep an hour across the whole host.
sweepIntervalSeconds :: Double
sweepIntervalSeconds = 3600

-- | The hook's version: sweep only when the last one is old enough,
-- and record the attempt.
--
-- Note [The sweep lives on the hook path now]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- v0.3 swept from the server's 5 s tick. When that thread died the
-- sweep stopped with it, silently, and a 4.1 MB write-ahead log was
-- the first visible symptom — days later. ADR-0017 deletes the thread,
-- so the sweep moves to a path that runs because a person is working:
-- the hook, on every prompt.
--
-- The `last_sweep` row is what keeps "every prompt" from meaning
-- "every prompt". It is written before the sweep runs, so a sweep that
-- throws still pushes the next attempt an hour out rather than
-- retrying on every keystroke.
sweepIfDue :: (CanTime m, MonadIO m) => Connection -> Int -> m (Maybe SweepResult)
sweepIfDue c days = do
  now <- currentTime
  last_ <- liftIO $ query c "SELECT value FROM maintenance WHERE key = 'last_sweep'" ()
  let previous = case last_ of
        (Only t : _) -> parseUtcLoose (t :: Text)
        [] -> Nothing
      due = case previous of
        Nothing -> True
        Just t -> realToFrac (diffUTCTime now t) >= sweepIntervalSeconds
  if not due
    then pure Nothing
    else do
      liftIO $
        execute
          c
          "INSERT INTO maintenance (key, value) VALUES ('last_sweep', ?)\n\
          \ON CONFLICT(key) DO UPDATE SET value = excluded.value"
          (Only (formatUtc now))
      Just <$> sweep c days
