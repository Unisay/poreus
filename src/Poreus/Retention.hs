module Poreus.Retention
  ( -- * Unified retention (MAINT-1, simplification E)
    defaultRetentionDays
  , retentionDays
  , SweepResult (..)
  , sweep
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Time (addUTCTime)
import Database.SQLite.Simple (Connection, Only (..), changes, execute)
import Text.Read (readMaybe)

import Poreus.Effects.Env (CanEnv, lookupEnvVar)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.Time (Timestamp (..))

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
  }
  deriving stock (Show, Eq)

instance ToJSON SweepResult where
  toJSON r =
    object
      [ "messages_deleted" .= swMessagesDeleted r
      , "sessions_deleted" .= swSessionsDeleted r
      , "host_sessions_deleted" .= swHostSessionsDeleted r
      ]

-- | Delete everything older than the window: messages by creation
-- time; sessions ended (or last heard from) before the cutoff — their
-- cursors cascade and any name binding resets to NULL (the name and
-- profile survive); stale host-session identity mappings. Runs
-- periodically from the server tick and on demand from `admin purge` /
-- the purge tool.
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
    pure (SweepResult nMsgs nSessions nHost)
