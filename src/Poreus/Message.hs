{-# OPTIONS_GHC -Wno-orphans #-}

module Poreus.Message
  ( -- * Types
    Message (..)
  , MessageKind (..)
  , messageKindText
  , parseMessageKind
  , RequestPayload (..)
  , ReplyPayload (..)
    -- * Construction
  , requestPayloadFromTask
  , replyPayloadFromResult
    -- * Persistence
  , insertMessage
  , lookupMessage
  , messagesTo
  , messagesToKind
  ) where

import qualified Data.Aeson as A
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query)
import Database.SQLite.Simple.FromRow (FromRow (..), field)

import Poreus.Profile (jsonToText, textToJson)
import Poreus.Time (Timestamp)
import Poreus.Types

-- ---------------------------------------------------------------------
-- Kind
-- ---------------------------------------------------------------------

data MessageKind = MKRequest | MKReply
  deriving stock (Show, Eq)

messageKindText :: MessageKind -> Text
messageKindText = \case
  MKRequest -> "request"
  MKReply -> "reply"

parseMessageKind :: Text -> Maybe MessageKind
parseMessageKind = \case
  "request" -> Just MKRequest
  "reply" -> Just MKReply
  _ -> Nothing

instance A.ToJSON MessageKind where
  toJSON = A.String . messageKindText

instance A.FromJSON MessageKind where
  parseJSON = A.withText "MessageKind" $ \t -> case parseMessageKind t of
    Just v -> pure v
    Nothing -> fail ("invalid message kind: " <> T.unpack t)

-- ---------------------------------------------------------------------
-- Payloads
-- ---------------------------------------------------------------------

-- | Payload shape for a `request` message. Mirrors the fields of the
-- legacy `SendInput` so the two are convertible.
data RequestPayload = RequestPayload
  { rpRequestKind :: !TaskKind
  , rpUrl :: !(Maybe Text)
  , rpDescription :: !(Maybe Text)
  , rpExpected :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

instance A.ToJSON RequestPayload where
  toJSON p =
    A.object
      [ "request_kind" A..= rpRequestKind p
      , "url" A..= rpUrl p
      , "description" A..= rpDescription p
      , "expected" A..= rpExpected p
      ]

instance A.FromJSON RequestPayload where
  parseJSON = A.withObject "RequestPayload" $ \o ->
    RequestPayload
      <$> o A..: "request_kind"
      <*> o A..:? "url"
      <*> o A..:? "description"
      <*> o A..:? "expected"

-- | Payload shape for a `reply` message. Mirrors `CompleteInput` +
-- an optional `reason` for rejections.
data ReplyPayload = ReplyPayload
  { rplStatus :: !ResultStatus
  , rplSummary :: !(Maybe Text)
  , rplArtifacts :: !A.Value
  , rplReason :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

instance A.ToJSON ReplyPayload where
  toJSON p =
    A.object
      [ "status" A..= rplStatus p
      , "summary" A..= rplSummary p
      , "artifacts" A..= rplArtifacts p
      , "reason" A..= rplReason p
      ]

instance A.FromJSON ReplyPayload where
  parseJSON = A.withObject "ReplyPayload" $ \o ->
    ReplyPayload
      <$> o A..: "status"
      <*> o A..:? "summary"
      <*> o A..:? "artifacts" A..!= A.Array mempty
      <*> o A..:? "reason"

-- ---------------------------------------------------------------------
-- Message row
-- ---------------------------------------------------------------------

data Message = Message
  { msgId :: !TaskId
  , msgFrom :: !Alias
  , msgTo :: !Alias
  , msgKind :: !MessageKind
  , msgInReplyTo :: !(Maybe TaskId)
  , msgPayload :: !A.Value
  , msgCreatedAt :: !Timestamp
  }
  deriving stock (Show, Eq)

-- | JSON emission uses `message_id` at the top level (per spec). The
-- SQLite PK column stays `id`.
instance A.ToJSON Message where
  toJSON m =
    A.object
      [ "message_id" A..= msgId m
      , "from" A..= msgFrom m
      , "to" A..= msgTo m
      , "kind" A..= msgKind m
      , "in_reply_to" A..= msgInReplyTo m
      , "payload" A..= msgPayload m
      , "created_at" A..= msgCreatedAt m
      ]

instance FromRow Message where
  fromRow = do
    mid <- field
    from <- field
    to <- field
    kind <- field
    inReply <- field
    payloadT <- field
    createdAt <- field
    pure
      Message
        { msgId = TaskId mid
        , msgFrom = Alias from
        , msgTo = Alias to
        , msgKind = maybe MKRequest id (parseMessageKind kind)
        , msgInReplyTo = TaskId <$> inReply
        , msgPayload = maybe A.Null id (textToJson payloadT)
        , msgCreatedAt = createdAt
        }

-- ---------------------------------------------------------------------
-- Construction helpers
-- ---------------------------------------------------------------------

requestPayloadFromTask
  :: TaskKind
  -> Maybe Text -- ^ url
  -> Maybe Text -- ^ description
  -> Maybe Text -- ^ expected
  -> RequestPayload
requestPayloadFromTask = RequestPayload

replyPayloadFromResult
  :: ResultStatus
  -> Maybe Text -- ^ summary
  -> A.Value -- ^ artifacts
  -> Maybe Text -- ^ reason (rejected only)
  -> ReplyPayload
replyPayloadFromResult = ReplyPayload

-- ---------------------------------------------------------------------
-- Persistence
-- ---------------------------------------------------------------------

insertMessage :: MonadIO m => Connection -> Message -> m ()
insertMessage c m = liftIO $
  execute
    c
    "INSERT OR IGNORE INTO messages\n\
    \  (id, from_alias, to_alias, kind, in_reply_to, payload, created_at)\n\
    \VALUES (?, ?, ?, ?, ?, ?, ?)"
    ( msgId m
    , msgFrom m
    , msgTo m
    , messageKindText (msgKind m)
    , msgInReplyTo m
    , jsonToText (msgPayload m)
    , msgCreatedAt m
    )

messageFields :: Text
messageFields =
  "id, from_alias, to_alias, kind, in_reply_to, payload, created_at"

lookupMessage :: MonadIO m => Connection -> TaskId -> m (Maybe Message)
lookupMessage c mid = liftIO $ do
  rows <-
    query
      c
      (Query ("SELECT " <> messageFields <> " FROM messages WHERE id = ?"))
      (Only mid)
  case rows of
    [] -> pure Nothing
    (r : _) -> pure (Just r)

messagesTo :: MonadIO m => Connection -> Alias -> m [Message]
messagesTo c alias = liftIO $
  query
    c
    (Query ("SELECT " <> messageFields <> " FROM messages WHERE to_alias = ? ORDER BY created_at, id"))
    (Only alias)

messagesToKind :: MonadIO m => Connection -> Alias -> MessageKind -> m [Message]
messagesToKind c alias kind = liftIO $
  query
    c
    (Query ("SELECT " <> messageFields <> " FROM messages WHERE to_alias = ? AND kind = ? ORDER BY created_at, id"))
    (alias, messageKindText kind)
