{-# OPTIONS_GHC -Wno-orphans #-}

module Poreus.Message
  ( -- * Types
    Message (..)
  , MessageKind (..)
  , messageKindText
  , parseMessageKind
    -- * Construction
  , newMessageId
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
import qualified Data.Vector as V
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query)
import Database.SQLite.Simple.FromRow (FromRow (..), field)

import Poreus.Profile (jsonToText, textToJson)
import Poreus.Time (Timestamp, formatTaskStamp, unTimestamp)
import Poreus.Types

-- | Pure formatter: "YYYYMMDD-HHmmss-<from>-<4hex>". Caller supplies
-- the wall-clock stamp and random hex tail. Used by `send`.
newMessageId :: Alias -> Timestamp -> Text -> TaskId
newMessageId (Alias from) ts hex =
  TaskId (T.concat [formatTaskStamp (unTimestamp ts), "-", from, "-", hex])

-- ---------------------------------------------------------------------
-- Kind
-- ---------------------------------------------------------------------

data MessageKind = MKRequest | MKNotice
  deriving stock (Show, Eq)

messageKindText :: MessageKind -> Text
messageKindText = \case
  MKRequest -> "request"
  MKNotice -> "notice"

-- | Accepts the canonical names plus the legacy alias `"reply"` →
-- `MKNotice` for transition tolerance. The schema CHECK only allows
-- `request` / `notice`, so reading `reply` from the DB is impossible
-- post-cutover; the alias only matters for callers still posting JSON
-- with `"kind":"reply"`.
parseMessageKind :: Text -> Maybe MessageKind
parseMessageKind = \case
  "request" -> Just MKRequest
  "notice" -> Just MKNotice
  "reply" -> Just MKNotice
  _ -> Nothing

instance A.ToJSON MessageKind where
  toJSON = A.String . messageKindText

instance A.FromJSON MessageKind where
  parseJSON = A.withText "MessageKind" $ \t -> case parseMessageKind t of
    Just v -> pure v
    Nothing -> fail ("invalid message kind: " <> T.unpack t)

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
  , msgSubscribe :: !(Maybe [Text])
  , msgCreatedAt :: !Timestamp
  }
  deriving stock (Show, Eq)

-- | JSON emission uses `message_id` at the top level (per spec). The
-- SQLite PK column stays `id`. `subscribe` is omitted when null and
-- only meaningful on requests (see ADR-0003).
instance A.ToJSON Message where
  toJSON m =
    A.object
      [ "message_id" A..= msgId m
      , "from" A..= msgFrom m
      , "to" A..= msgTo m
      , "kind" A..= msgKind m
      , "in_reply_to" A..= msgInReplyTo m
      , "payload" A..= msgPayload m
      , "subscribe" A..= msgSubscribe m
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
    subscribeT <- field
    createdAt <- field
    pure
      Message
        { msgId = TaskId mid
        , msgFrom = Alias from
        , msgTo = Alias to
        , msgKind = maybe MKRequest id (parseMessageKind kind)
        , msgInReplyTo = TaskId <$> inReply
        , msgPayload = maybe A.Null id (textToJson payloadT)
        , msgSubscribe = subscribeT >>= textToJson >>= subscribeFromJson
        , msgCreatedAt = createdAt
        }

subscribeFromJson :: A.Value -> Maybe [Text]
subscribeFromJson (A.Array xs) =
  traverse (\v -> case v of A.String s -> Just s; _ -> Nothing) (V.toList xs)
subscribeFromJson _ = Nothing

-- ---------------------------------------------------------------------
-- Persistence
-- ---------------------------------------------------------------------

insertMessage :: MonadIO m => Connection -> Message -> m ()
insertMessage c m = liftIO $
  execute
    c
    "INSERT OR IGNORE INTO messages\n\
    \  (id, from_alias, to_alias, kind, in_reply_to, payload, subscribe, created_at)\n\
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ( msgId m
    , msgFrom m
    , msgTo m
    , messageKindText (msgKind m)
    , msgInReplyTo m
    , jsonToText (msgPayload m)
    , subscribeColumn (msgSubscribe m)
    , msgCreatedAt m
    )

subscribeColumn :: Maybe [Text] -> Maybe Text
subscribeColumn = fmap (jsonToText . A.toJSON)

messageFields :: Text
messageFields =
  "id, from_alias, to_alias, kind, in_reply_to, payload, subscribe, created_at"

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
