module Poreus.Types
  ( -- * Identifiers
    SessionAddress (..)
  , AgentName (..)
  , MessageId (..)
  , sessionAddressPrefix
  , isSessionAddressText
  , Target (..)
  , parseTarget
  , Mailbox (..)
  , mailboxKey
  , mailboxKindText
  , mailboxFromRow

    -- * Enumerations
  , Autonomy (..)
  , autonomyText
  , parseAutonomy
  , MessageKind (..)
  , messageKindText
  , parseMessageKind

    -- * Message
  , Message (..)
  , messageColumns
  , newMessageId
  , messageEvent
  , terminalEvents
  , isTerminalEvent

    -- * Errors
  , ErrorCode (..)
  , errorCodeText
  , PoreusError (..)
  , mkError
  , mkErrorWithAction
  , PoreusException (..)

    -- * Warnings
  , Warning (..)
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withText, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Generics (Generic)

import Poreus.Time (Timestamp, formatTaskStamp, unTimestamp)

-- ---------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------

-- | A session address: the sole delivery key. Always carries the
-- @s-@ prefix ("s-<session id>"). Auto-provisioned at a session's
-- first contact (REG-2); never typed by hand.
newtype SessionAddress = SessionAddress {unSessionAddress :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSON, FromJSON, ToField, FromField)

-- | A short, unique, human-friendly name voluntarily claimed by a
-- session (REG-3) — the durable principal. A name owns a mailbox, a
-- profile, and a delivery cursor, all of which outlive the process
-- that currently holds it (ADR-0017).
newtype AgentName = AgentName {unAgentName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSON, FromJSON, ToField, FromField)

-- | Message identifier: "YYYYMMDD-HHmmss-<tag>-<4hex>". Unique and
-- human-scannable; the total order and the cursor key is the `seq`
-- column, not this id.
newtype MessageId = MessageId {unMessageId :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSON, FromJSON, ToField, FromField)

sessionAddressPrefix :: Text
sessionAddressPrefix = "s-"

isSessionAddressText :: Text -> Bool
isSessionAddressText = T.isPrefixOf sessionAddressPrefix

-- | What a sender may write in @to@: a session address (recognized by
-- its @s-@ prefix; names may not start with @s-@, enforced at claim
-- time) or a name resolved at post time (SEND-5).
data Target = TargetSession SessionAddress | TargetName AgentName
  deriving stock (Show, Eq)

parseTarget :: Text -> Target
parseTarget t
  | isSessionAddressText t = TargetSession (SessionAddress t)
  | otherwise = TargetName (AgentName t)

-- | Where a message is stored and from where it is drained.
--
-- Note [Mailboxes belong to roles]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- v0.3 keyed every mailbox on the session address (ADR-0012), so a
-- post to a role resolved to whichever process held the role at that
-- instant. Two failures followed. A role with no live holder could not
-- be written to at all, which turned every restart into a hard error
-- for the sender. And a request left in a dead holder's mailbox needed
-- a special query mode to recover.
--
-- ADR-0017 moves the mailbox to the role: the role is the durable
-- principal, the session is the process that currently serves it. A
-- session mailbox still exists, for replies to senders that hold no
-- role and for peers addressing a specific process.
data Mailbox = MailboxRole !AgentName | MailboxSession !SessionAddress
  deriving stock (Show, Eq, Ord)

-- | The stored delivery key: the role name, or the session address.
-- The two spaces cannot collide — a name may not start with @s-@.
mailboxKey :: Mailbox -> Text
mailboxKey = \case
  MailboxRole (AgentName n) -> n
  MailboxSession (SessionAddress a) -> a

mailboxKindText :: Mailbox -> Text
mailboxKindText = \case
  MailboxRole{} -> "role"
  MailboxSession{} -> "session"

-- | Rebuild a mailbox from its two stored columns. An unrecognised
-- kind reads as a session mailbox, which is the inert choice: a role
-- mailbox drained by the wrong holder would misdeliver.
mailboxFromRow :: Text -> Text -> Mailbox
mailboxFromRow key kind
  | kind == "role" = MailboxRole (AgentName key)
  | otherwise = MailboxSession (SessionAddress key)

instance ToJSON Mailbox where
  toJSON = toJSON . mailboxKey

-- ---------------------------------------------------------------------
-- Enumerations
-- ---------------------------------------------------------------------

data Autonomy = AutonomyAuto | AutonomyConfirm
  deriving stock (Show, Eq, Generic)

autonomyText :: Autonomy -> Text
autonomyText = \case
  AutonomyAuto -> "auto"
  AutonomyConfirm -> "confirm"

parseAutonomy :: Text -> Maybe Autonomy
parseAutonomy = \case
  "auto" -> Just AutonomyAuto
  "confirm" -> Just AutonomyConfirm
  _ -> Nothing

instance ToJSON Autonomy where
  toJSON = toJSON . autonomyText

instance FromJSON Autonomy where
  parseJSON = withText "Autonomy" $ \t ->
    case parseAutonomy t of
      Just v -> pure v
      Nothing -> fail ("invalid autonomy: " <> T.unpack t)

data MessageKind = MKRequest | MKNotice
  deriving stock (Show, Eq)

messageKindText :: MessageKind -> Text
messageKindText = \case
  MKRequest -> "request"
  MKNotice -> "notice"

parseMessageKind :: Text -> Maybe MessageKind
parseMessageKind = \case
  "request" -> Just MKRequest
  "notice" -> Just MKNotice
  _ -> Nothing

instance ToJSON MessageKind where
  toJSON = A.String . messageKindText

instance FromJSON MessageKind where
  parseJSON = withText "MessageKind" $ \t -> case parseMessageKind t of
    Just v -> pure v
    Nothing -> fail ("invalid message kind: " <> T.unpack t)

-- ---------------------------------------------------------------------
-- Message
-- ---------------------------------------------------------------------

-- | The atomic delivery unit (spec §5). Flat record (ADR-0008),
-- immutable once posted. `from` is always the sending session — the
-- process that can be held to the reply duty; `to` is a 'Mailbox', so
-- one message is addressed either to a role or to a single session.
data Message = Message
  { msgSeq :: !Int64
  , msgId :: !MessageId
  , msgFrom :: !SessionAddress
  , msgFromName :: !(Maybe AgentName)
  , msgTo :: !Mailbox
  , msgKind :: !MessageKind
  , msgInReplyTo :: !(Maybe MessageId)
  , msgPayload :: !Value
  , msgCreatedAt :: !Timestamp
  }
  deriving stock (Show, Eq)

-- | Column list matching the 'FromRow' instance — every message SELECT
-- uses this exact order.
messageColumns :: Text
messageColumns =
  "seq, id, from_address, from_name, to_mailbox, to_kind, kind, in_reply_to, payload, created_at"

instance FromRow Message where
  fromRow = do
    seq_ <- field
    mid <- field
    from <- field
    fromName <- field
    toKey <- field
    toKind <- field
    kindT <- field
    inReply <- field
    payloadT <- field
    createdAt <- field
    pure
      Message
        { msgSeq = seq_
        , msgId = mid
        , msgFrom = from
        , msgFromName = fromName
        , msgTo = mailboxFromRow toKey toKind
        , msgKind = fromMaybe MKRequest (parseMessageKind kindT)
        , msgInReplyTo = inReply
        , msgPayload = fromMaybe A.Null (A.decodeStrict' (TE.encodeUtf8 payloadT))
        , msgCreatedAt = createdAt
        }

instance ToJSON Message where
  toJSON m =
    object
      [ "message_id" .= msgId m
      , "from" .= msgFrom m
      , "from_name" .= msgFromName m
      , "to" .= msgTo m
      , "to_kind" .= mailboxKindText (msgTo m)
      , "kind" .= msgKind m
      , "in_reply_to" .= msgInReplyTo m
      , "payload" .= msgPayload m
      , "created_at" .= msgCreatedAt m
      ]

-- | Pure formatter: "YYYYMMDD-HHmmss-<tag>-<4hex>". The tag is the
-- sender's bound name when it has one, else the session-id fragment of
-- its address — keeps ids human-scannable either way.
newMessageId :: SessionAddress -> Maybe AgentName -> Timestamp -> Text -> MessageId
newMessageId (SessionAddress addr) mname ts hex =
  MessageId (T.concat [formatTaskStamp (unTimestamp ts), "-", tag, "-", hex])
  where
    tag = case mname of
      Just (AgentName n) -> n
      Nothing -> T.take 8 (T.drop (T.length sessionAddressPrefix) addr)

-- | The @event@ field of a notice payload, when present.
messageEvent :: Message -> Maybe Text
messageEvent m = case msgPayload m of
  A.Object o -> case KM.lookup "event" o of
    Just (A.String s) -> Just s
    _ -> Nothing
  _ -> Nothing

-- | Recommended terminal lifecycle events (ADR-0007). Used only for
-- the derived, convention-based thread status (THRD-2) — never stored,
-- never an input to any other behavior.
terminalEvents :: [Text]
terminalEvents = ["completed", "failed", "aborted"]

isTerminalEvent :: Text -> Bool
isTerminalEvent = (`elem` terminalEvents)

-- ---------------------------------------------------------------------
-- Errors (spec §9)
-- ---------------------------------------------------------------------

data ErrorCode
  = InvalidInput
  | UnknownAgent
  | UnknownRecipient
  | NameUnbound
  | UnknownMessage
  | NameHeld
  | StorageFailure
  | InternalError
  deriving stock (Show, Eq, Generic)

errorCodeText :: ErrorCode -> Text
errorCodeText = \case
  InvalidInput -> "invalid-input"
  UnknownAgent -> "unknown-agent"
  UnknownRecipient -> "unknown-recipient"
  NameUnbound -> "name-unbound"
  UnknownMessage -> "unknown-message"
  NameHeld -> "name-held"
  StorageFailure -> "storage-failure"
  InternalError -> "internal"

instance ToJSON ErrorCode where
  toJSON = toJSON . errorCodeText

-- | A structured domain error: stable machine-readable code, an
-- agent-readable message, and — where applicable — the corrective
-- action (C-7).
data PoreusError = PoreusError
  { errCode :: !ErrorCode
  , errMessage :: !Text
  , errAction :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PoreusError where
  toJSON e =
    object $
      [ "code" .= errCode e
      , "message" .= errMessage e
      ]
        <> maybe [] (\a -> ["action" .= a]) (errAction e)

mkError :: ErrorCode -> Text -> PoreusError
mkError c m = PoreusError c m Nothing

mkErrorWithAction :: ErrorCode -> Text -> Text -> PoreusError
mkErrorWithAction c m a = PoreusError c m (Just a)

-- | Exception wrapper for the rare places that cannot return
-- @Either PoreusError@ directly (e.g. storage-layer failures below the
-- domain functions). The tool dispatcher catches it and renders the
-- carried error.
newtype PoreusException = PoreusException PoreusError
  deriving stock (Show)

instance Exception PoreusException

-- ---------------------------------------------------------------------
-- Warnings
-- ---------------------------------------------------------------------

-- | Non-blocking guardrail attached to a successful result (spec §9):
-- endpoint not found at call time, thread already terminal, recipient
-- session ended, and others as guardrails accrue.
data Warning = Warning
  { warnCode :: !Text
  , warnMessage :: !Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Warning where
  toJSON w = object ["code" .= warnCode w, "message" .= warnMessage w]
