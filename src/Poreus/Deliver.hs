module Poreus.Deliver
  ( -- * Attendance delivery (RECV-1)
    Delivered (..)
  , deliverPending
  , cursorOf
  , pendingCount
  , replyDuty
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.List (sortOn)
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

-- | Advance-and-deliver across every mailbox this session drains: its
-- own, plus the mailbox of the role it holds. Each mailbox keeps its
-- own cursor, and the whole read-and-advance runs in one immediate
-- transaction, so a server and a concurrently running hook cannot
-- double-deliver. Snapshots ("Poreus.Query") never touch a cursor
-- (ADR-0005).
--
-- Note [Two mailboxes, two cursors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The role cursor belongs to the role, not to the holder, so a
-- successor picks up exactly where its predecessor stopped — including
-- requests that arrived while no session held the role at all. That is
-- RECV-4 adoption, and it needs no query flag any more.
--
-- The results are merged and re-sorted by `seq`, which is the total
-- order across both mailboxes, so a reader sees one stream in the
-- order the messages were actually posted.
deliverPending :: MonadIO m => Connection -> [Mailbox] -> m [Delivered]
deliverPending c boxes = liftIO . withImmediateTransaction c $ do
  msgs <- concat <$> mapM drain boxes
  pure (map attachDuty (sortOn msgSeq msgs))
  where
    drain box = do
      let key = mailboxKey box
      execute c "INSERT OR IGNORE INTO cursors (mailbox, last_seq) VALUES (?, 0)" (Only key)
      cur <- query c "SELECT last_seq FROM cursors WHERE mailbox = ?" (Only key)
      let floor_ = case cur of
            (Only n : _) -> n
            [] -> 0 :: Int64
      msgs <- pendingSince c box floor_
      case reverse msgs of
        [] -> pure ()
        (newest : _) ->
          execute c "UPDATE cursors SET last_seq = ? WHERE mailbox = ?" (msgSeq newest, key)
      pure msgs

    attachDuty m =
      Delivered
        { dMessage = m
        , dReplyDuty = case msgKind m of
            MKRequest -> Just (replyDuty (msgId m))
            MKNotice -> Nothing
        }

-- | One mailbox's acknowledged-delivery high-water mark.
cursorOf :: MonadIO m => Connection -> Mailbox -> m Int64
cursorOf c box = liftIO $ do
  rows <- query c "SELECT last_seq FROM cursors WHERE mailbox = ?" (Only (mailboxKey box))
  pure $ case rows of
    (Only n : _) -> n
    [] -> 0

-- | How much is queued in a mailbox and not yet delivered. Read-only:
-- `discover` reports it so a sender can see that a role is accruing a
-- backlog nobody is draining, and `retire_name` refuses on it.
pendingCount :: MonadIO m => Connection -> Mailbox -> m Int
pendingCount c box = do
  floor_ <- cursorOf c box
  liftIO $ do
    rows <-
      query
        c
        "SELECT COUNT(*) FROM messages WHERE to_mailbox = ? AND to_kind = ? AND seq > ?"
        (mailboxKey box, mailboxKindText box, floor_)
    pure $ case rows of
      (Only n : _) -> n
      [] -> 0

pendingSince :: Connection -> Mailbox -> Int64 -> IO [Message]
pendingSince c box floor_ =
  query
    c
    ( Query
        ( "SELECT "
            <> messageColumns
            <> " FROM messages\n\
               \WHERE to_mailbox = ? AND to_kind = ? AND seq > ? ORDER BY seq"
        )
    )
    (mailboxKey box, mailboxKindText box, floor_)
