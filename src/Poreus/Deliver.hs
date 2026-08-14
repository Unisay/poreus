module Poreus.Deliver
  ( -- * Attendance delivery (RECV-1)
    Delivered (..)
  , deliverPending
  , peekPendingSince
  , cursorOf
  , replyDuty
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query)

import Poreus.DB (withImmediateTransaction)
import Poreus.Types

-- | A delivered message plus the reply duty carried in-band (RECV-1):
-- the receiving model needs no external document to act correctly.
data Delivered = Delivered
  { dMessage :: !Message
  , dReplyDuty :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

instance ToJSON Delivered where
  toJSON d =
    object $
      ["message" .= dMessage d]
        <> maybe [] (\duty -> ["reply_duty" .= duty]) (dReplyDuty d)

-- | The POL-1 emission duty, attached to every delivered request.
replyDuty :: MessageId -> Text
replyDuty mid =
  "This is a request: reply with the poreus reply tool (in_reply_to: '"
    <> unMessageId mid
    <> "'). Always emit exactly one terminal notice (event completed, failed, or aborted) with a summary; emit started first when the work is more than momentary; emit stuck when blocked."

-- | Advance-and-deliver: everything in the session's mailbox past its
-- cursor, in seq order, exactly once per attendance stream (C-5). The
-- read and the cursor write share one immediate transaction so the
-- server and a concurrently running hook cannot double-deliver.
-- Snapshots (Poreus.Query) never touch the cursor (ADR-0005).
deliverPending :: MonadIO m => Connection -> SessionAddress -> m [Delivered]
deliverPending c me = liftIO . withImmediateTransaction c $ do
  execute c "INSERT OR IGNORE INTO cursors (session_address, last_seq) VALUES (?, 0)" (Only me)
  cur <- query c "SELECT last_seq FROM cursors WHERE session_address = ?" (Only me)
  let floor_ = case cur of
        (Only n : _) -> n
        [] -> 0 :: Int64
  msgs <- pendingSince c me floor_
  case reverse msgs of
    [] -> pure ()
    (newest : _) ->
      execute
        c
        "UPDATE cursors SET last_seq = ? WHERE session_address = ?"
        (msgSeq newest, me)
  pure (map attachDuty msgs)
  where
    attachDuty m =
      Delivered
        { dMessage = m
        , dReplyDuty = case msgKind m of
            MKRequest -> Just (replyDuty (msgId m))
            MKNotice -> Nothing
        }

-- | Read past a caller-supplied floor without touching the cursor —
-- the channel-push path: pushes are best-effort and unacknowledged, so
-- they must never advance the cursor (the acknowledged paths — tool
-- results, hook digests — do). The caller tracks its own pushed floor.
peekPendingSince :: MonadIO m => Connection -> SessionAddress -> Int64 -> m [Message]
peekPendingSince c me floor_ = liftIO (pendingSince c me floor_)

-- | The session's current acknowledged-delivery high-water mark. The
-- channel pusher reads it so it never re-pushes something an
-- acknowledged path already delivered.
cursorOf :: MonadIO m => Connection -> SessionAddress -> m Int64
cursorOf c me = liftIO $ do
  rows <- query c "SELECT last_seq FROM cursors WHERE session_address = ?" (Only me)
  pure $ case rows of
    (Only n : _) -> n
    [] -> 0

pendingSince :: Connection -> SessionAddress -> Int64 -> IO [Message]
pendingSince c me floor_ =
  query
    c
    ( Query
        ( "SELECT "
            <> messageColumns
            <> " FROM messages\n\
               \WHERE to_address = ? AND seq > ? ORDER BY seq"
        )
    )
    (me, floor_)
