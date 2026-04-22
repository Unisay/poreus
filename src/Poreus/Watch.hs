module Poreus.Watch
  ( watchCheck
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query)

import Poreus.Message (Message (..))
import Poreus.Time (Timestamp, formatUtc, unTimestamp)
import Poreus.Types

-- | Unified watch — one per-alias cursor (stored in the `watch_cursors`
-- table), flat array of messages addressed to the current alias that
-- arrived strictly after the last seen cursor. First call returns every
-- message in the inbox; subsequent calls with no new traffic return
-- `[]`.
--
-- The cursor is advanced to the `created_at` of the latest message
-- returned so that re-runs are idempotent and no message is skipped.
watchCheck
  :: MonadIO m
  => Connection
  -> Alias
  -> Timestamp -- ^ now — used only as a floor when no messages
  -> m [Message]
watchCheck c alias now = liftIO $ do
  cursor <- readCursor c alias
  msgs <- fetchSince c alias cursor
  let advanceTo = case msgs of
        [] -> formatUtc (unTimestamp now)
        _ -> latestCreatedAt msgs
  writeCursor c alias advanceTo
  pure msgs

-- | Latest `created_at` across a non-empty list of messages — strings
-- compare correctly because `formatUtc` produces fixed-width ISO 8601.
latestCreatedAt :: [Message] -> Text
latestCreatedAt = maximum . map (formatUtc . unTimestamp . msgCreatedAt)

messageFields :: Text
messageFields =
  "id, from_alias, to_alias, kind, in_reply_to, payload, created_at"

fetchSince :: Connection -> Alias -> Maybe Text -> IO [Message]
fetchSince c alias = \case
  Nothing ->
    query
      c
      (Query
        ( "SELECT " <> messageFields
          <> " FROM messages WHERE to_alias = ? ORDER BY created_at, id"
        )
      )
      (Only alias)
  Just ts ->
    query
      c
      (Query
        ( "SELECT " <> messageFields
          <> " FROM messages WHERE to_alias = ? AND created_at > ? ORDER BY created_at, id"
        )
      )
      (alias, ts)

readCursor :: Connection -> Alias -> IO (Maybe Text)
readCursor c alias = do
  rows <-
    query
      c
      "SELECT last_seen FROM watch_cursors WHERE alias = ?"
      (Only alias) ::
      IO [Only Text]
  case rows of
    [] -> pure Nothing
    Only ts : _ -> pure (Just ts)

writeCursor :: Connection -> Alias -> Text -> IO ()
writeCursor c alias ts =
  execute
    c
    "INSERT INTO watch_cursors (alias, last_seen) VALUES (?, ?)\n\
    \ON CONFLICT(alias) DO UPDATE SET last_seen = excluded.last_seen"
    (alias, ts)
