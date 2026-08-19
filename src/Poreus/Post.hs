module Poreus.Post
  ( -- * Sender identity as stamped on every post (SEND-5)
    Sender (..)

    -- * Operations
  , postRequest
  , postCall
  , postReply
  , postNotify

    -- * Lookup
  , lookupMessage

    -- * Internals exposed for tests
  , resolveTarget
  , Resolved (..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, lastInsertRowId, query)

import Poreus.Effects.Random (CanRandom, randomHex4)
import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.JSON (jsonToText)
import Poreus.Name (resolveRole)
import qualified Poreus.Name as Name
import Poreus.Profile (EndpointRow (..), endpointsOf)
import Poreus.Session (getSession, sessionLive)
import Poreus.Time (Timestamp (..))
import Poreus.Types

-- | Who is posting. Both fields are server-derived — the caller can
-- forge neither (SEND-5(1)): the address comes from session identity
-- (REG-2), the name annotation from the current binding (OQ-10).
data Sender = Sender
  { senderAddress :: !SessionAddress
  , senderName :: !(Maybe AgentName)
  }
  deriving stock (Show, Eq)

-- | The result of send-time target resolution (SEND-5(4)).
data Resolved = Resolved
  { resMailbox :: !Mailbox
  , resWarnings :: ![Warning]
  }
  deriving stock (Show, Eq)

-- | Resolve a `to` designator at post time.
--
-- A role designator yields that role's mailbox — it is written to
-- whether or not a session is serving the role right now, and the
-- warnings say which case applied. A session address must exist in the
-- catalog; posting to an ended session is accepted with a warning,
-- because the send\/end race makes rejection wrong and the mailbox
-- persists until retention.
resolveTarget ::
  (CanTime m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  Text ->
  -- | create the role when it does not exist
  Bool ->
  m (Either PoreusError Resolved)
resolveTarget c to create
  | to == "" =
      pure (Left (mkErrorWithAction InvalidInput "'to' must not be empty" "pass a role name or a session address from discover"))
  | otherwise = case parseTarget to of
      TargetSession addr -> do
        row <- getSession c addr
        case row of
          Nothing ->
            pure . Left $
              PoreusError
                UnknownRecipient
                ("session address '" <> unSessionAddress addr <> "' is not in the catalog")
                (Just "run discover to list addressable roles and sessions")
          Just sess -> do
            live <- sessionLive sess
            let warnings =
                  [ Warning
                      "recipient-process-gone"
                      "the addressed session's process is no longer running; the message is stored and stays inspectable, but a session address dies with its process — address the role instead so a successor can pick the work up"
                  | not live
                  ]
            pure (Right (Resolved (MailboxSession addr) warnings))
      TargetName name -> do
        resolved <- resolveRole c name create
        pure (fmap (uncurry Resolved) resolved)

-- | SEND-1: free-text request.
postRequest ::
  (CanTime m, CanRandom m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  Sender ->
  -- | to
  Text ->
  -- | description
  Text ->
  -- | expected outcome
  Maybe Text ->
  -- | additional payload fields
  Maybe Value ->
  -- | create the role when it does not exist
  Bool ->
  m (Either PoreusError (Message, [Warning]))
postRequest c sender to description expected extra create
  | description == "" =
      pure (Left (mkError InvalidInput "'description' must not be empty"))
  | otherwise =
      insertPost c sender to create MKRequest Nothing payload []
  where
    payload =
      object $
        [ "request_kind" .= ("freetext" :: Text)
        , "description" .= description
        ]
          <> maybe [] (\e -> ["expected_outcome" .= e]) expected
          <> maybe [] (\d -> ["data" .= d]) extra

-- | SEND-2: typed endpoint call. The endpoint need not exist at send
-- time, but the operation warns when it does not.
postCall ::
  (CanTime m, CanRandom m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  Sender ->
  -- | to
  Text ->
  -- | verb
  Text ->
  -- | named-arguments object
  Maybe Value ->
  -- | create the role when it does not exist
  Bool ->
  m (Either PoreusError (Message, [Warning]))
postCall c sender to verb args create
  | verb == "" = pure (Left (mkError InvalidInput "'verb' must not be empty"))
  | otherwise = do
      warn <- endpointWarning c to verb
      insertPost c sender to create MKRequest Nothing payload warn
  where
    payload =
      object
        [ "request_kind" .= ("rpc" :: Text)
        , "verb" .= verb
        , "args" .= fromMaybe (object []) args
        ]

endpointWarning ::
  MonadIO m =>
  Connection ->
  Text ->
  Text ->
  m [Warning]
endpointWarning c to verb = do
  mname <- case parseTarget to of
    TargetName n -> pure (Just n)
    TargetSession addr -> Name.boundNameOf c addr
  known <- case mname of
    Nothing -> pure False
    Just n -> any ((== verb) . epVerb) <$> endpointsOf c n
  pure
    [ Warning
        "endpoint-not-found"
        ("no endpoint '" <> verb <> "' is registered for '" <> to <> "'; the request is posted anyway — the target may have it unpublished")
    | not known
    ]

-- | SEND-3: lifecycle reply — a notice that requires correlation and an
-- event. Warns when the thread already has a terminal notice.
--
-- The reply goes to the requester's ROLE when the request carried one,
-- and only to the exact session when the requester held no role. A
-- reply is often minutes or hours behind its request, by which time
-- the asking process may be gone; routing to the role means its
-- successor reads the answer to work the role started. An unnamed
-- sender has no successor to route to, so its own mailbox is the only
-- correct target.
postReply ::
  (CanTime m, CanRandom m, MonadIO m) =>
  Connection ->
  Sender ->
  -- | in_reply_to
  MessageId ->
  -- | event (recommended vocabulary: started, stuck, completed, failed, aborted)
  Text ->
  -- | summary
  Maybe Text ->
  -- | artifacts (list of {type, value, description} records by convention)
  Maybe Value ->
  m (Either PoreusError (Message, [Warning]))
postReply c sender inReplyTo event summary artifacts
  | event == "" = pure (Left (mkError InvalidInput "'event' must not be empty"))
  | otherwise = do
      mroot <- lookupMessage c inReplyTo
      case mroot of
        Nothing ->
          pure . Left $
            PoreusError
              UnknownMessage
              ("message '" <> unMessageId inReplyTo <> "' does not exist")
              (Just "check the id; use messages scope: history to list recent traffic")
        Just root -> do
          terminalWarn <- terminalWarning c inReplyTo
          insertPostTo
            c
            sender
            (replyMailbox root)
            MKNotice
            (Just inReplyTo)
            payload
            terminalWarn
  where
    payload =
      object $
        ["event" .= event]
          <> maybe [] (\s -> ["summary" .= s]) summary
          <> maybe [] (\a -> ["artifacts" .= a]) artifacts

terminalWarning :: MonadIO m => Connection -> MessageId -> m [Warning]
terminalWarning c root = do
  replies <-
    liftIO $
      query
        c
        (Query ("SELECT " <> messageColumns <> " FROM messages WHERE in_reply_to = ? AND kind = 'notice' ORDER BY seq"))
        (Only root)
  let terminals = mapMaybe describeTerminal replies
  pure $ case terminals of
    (w : _) -> [w]
    [] -> []
  where
    describeTerminal m = do
      ev <- messageEvent m
      if isTerminalEvent ev
        then
          Just $
            Warning
              "thread-already-terminal"
              ("thread '" <> unMessageId root <> "' already has terminal notice " <> unMessageId (msgId m) <> " (event '" <> ev <> "') from " <> senderLabel m)
        else Nothing
    senderLabel m = case msgFromName m of
      Just n -> unAgentName n
      Nothing -> unSessionAddress (msgFrom m)

-- | SEND-4: uncorrelated notice — broadcast-style information or an
-- unsolicited ping. A summary or event is recommended but not required.
postNotify ::
  (CanTime m, CanRandom m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  Sender ->
  -- | to
  Text ->
  -- | event
  Maybe Text ->
  -- | summary
  Maybe Text ->
  -- | additional payload fields
  Maybe Value ->
  -- | create the role when it does not exist
  Bool ->
  m (Either PoreusError (Message, [Warning]))
postNotify c sender to event summary extra create =
  insertPost c sender to create MKNotice Nothing payload []
  where
    payload =
      object $
        maybe [] (\e -> ["event" .= e]) event
          <> maybe [] (\s -> ["summary" .= s]) summary
          <> maybe [] (\d -> ["data" .= d]) extra

-- ---------------------------------------------------------------------
-- Shared insertion path (SEND-5)
-- ---------------------------------------------------------------------

-- | Where a reply to this message belongs: the requester's role when
-- it had one, else the requester's own session mailbox.
replyMailbox :: Message -> Mailbox
replyMailbox root = case msgFromName root of
  Just n -> MailboxRole n
  Nothing -> MailboxSession (msgFrom root)

insertPost ::
  (CanTime m, CanRandom m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  Sender ->
  Text ->
  Bool ->
  MessageKind ->
  Maybe MessageId ->
  Value ->
  [Warning] ->
  m (Either PoreusError (Message, [Warning]))
insertPost c sender to create kind inReplyTo payload extraWarnings = do
  resolved <- resolveTarget c to create
  case resolved of
    Left e -> pure (Left e)
    Right Resolved{resMailbox, resWarnings} -> do
      r <- storeMessage c sender resMailbox kind inReplyTo payload
      pure (Right (r, resWarnings <> extraWarnings))

insertPostTo ::
  (CanTime m, CanRandom m, MonadIO m) =>
  Connection ->
  Sender ->
  Mailbox ->
  MessageKind ->
  Maybe MessageId ->
  Value ->
  [Warning] ->
  m (Either PoreusError (Message, [Warning]))
insertPostTo c sender box kind inReplyTo payload warnings = do
  r <- storeMessage c sender box kind inReplyTo payload
  pure (Right (r, warnings))

-- | Stamp id + timestamp, insert, and return the stored record.
-- Messages are immutable once posted (SEND-5(2)).
storeMessage ::
  (CanTime m, CanRandom m, MonadIO m) =>
  Connection ->
  Sender ->
  Mailbox ->
  MessageKind ->
  Maybe MessageId ->
  Value ->
  m Message
storeMessage c Sender{senderAddress, senderName} box kind inReplyTo payload = do
  now <- Timestamp <$> currentTime
  hex <- randomHex4
  let mid = newMessageId senderAddress senderName now hex
  seq_ <- liftIO $ do
    execute
      c
      "INSERT INTO messages\n\
      \  (id, from_address, from_name, to_mailbox, to_kind, kind, in_reply_to, payload, created_at)\n\
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ( mid
      , senderAddress
      , senderName
      , mailboxKey box
      , mailboxKindText box
      , messageKindText kind
      , inReplyTo
      , jsonToText payload
      , now
      )
    lastInsertRowId c
  pure
    Message
      { msgSeq = seq_
      , msgId = mid
      , msgFrom = senderAddress
      , msgFromName = senderName
      , msgTo = box
      , msgKind = kind
      , msgInReplyTo = inReplyTo
      , msgPayload = payload
      , msgCreatedAt = now
      }

lookupMessage :: MonadIO m => Connection -> MessageId -> m (Maybe Message)
lookupMessage c mid = liftIO $ do
  rows <-
    query
      c
      (Query ("SELECT " <> messageColumns <> " FROM messages WHERE id = ?"))
      (Only mid)
  pure $ case rows of
    (r : _) -> Just r
    [] -> Nothing
