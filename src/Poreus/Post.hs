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
import Poreus.Name (resolveName)
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
  { resAddress :: !SessionAddress
  , resName :: !(Maybe AgentName)
  -- ^ The as-written name designator, preserved as the `to_name`
  -- annotation; Nothing when the sender wrote a session address.
  , resWarnings :: ![Warning]
  }
  deriving stock (Show, Eq)

-- | Resolve a `to` designator at post time. Names resolve to the
-- session currently bound (rejecting unclaimed and unbound names);
-- session addresses must exist in the catalog, and posting to an ended
-- session is accepted with a warning (the send/session-end race makes
-- rejection wrong — the mailbox persists until retention).
resolveTarget ::
  (CanTime m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  Text ->
  m (Either PoreusError Resolved)
resolveTarget c to
  | to == "" =
      pure (Left (mkErrorWithAction InvalidInput "'to' must not be empty" "pass a name or a session address from discover"))
  | otherwise = case parseTarget to of
      TargetSession addr -> do
        row <- getSession c addr
        case row of
          Nothing ->
            pure . Left $
              PoreusError
                UnknownRecipient
                ("session address '" <> unSessionAddress addr <> "' is not in the catalog")
                (Just "run discover to list addressable sessions and names")
          Just sess -> do
            live <- sessionLive sess
            let warnings =
                  [ Warning
                      "recipient-session-ended"
                      ("session " <> unSessionAddress addr <> " is not live; the message is stored and remains inspectable, but may never be consumed")
                  | not live
                  ]
            pure (Right (Resolved addr Nothing warnings))
      TargetName name -> do
        resolved <- resolveName c name
        pure $ case resolved of
          Left e -> Left e
          Right addr -> Right (Resolved addr (Just name) [])

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
  m (Either PoreusError (Message, [Warning]))
postRequest c sender to description expected extra
  | description == "" =
      pure (Left (mkError InvalidInput "'description' must not be empty"))
  | otherwise =
      insertPost c sender to MKRequest Nothing payload []
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
  m (Either PoreusError (Message, [Warning]))
postCall c sender to verb args
  | verb == "" = pure (Left (mkError InvalidInput "'verb' must not be empty"))
  | otherwise = do
      warn <- endpointWarning c to verb
      insertPost c sender to MKRequest Nothing payload warn
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
-- event. The reply routes to the exact session that posted the
-- referenced message; warns when the thread already has a terminal
-- notice.
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
            (msgFrom root)
            (msgFromName root)
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
              ("thread '" <> unMessageId root <> "' already has terminal notice " <> unMessageId (msgId m) <> " (event '" <> ev <> "') from " <> unSessionAddress (msgFrom m))
        else Nothing

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
  m (Either PoreusError (Message, [Warning]))
postNotify c sender to event summary extra =
  insertPost c sender to MKNotice Nothing payload []
  where
    payload =
      object $
        maybe [] (\e -> ["event" .= e]) event
          <> maybe [] (\s -> ["summary" .= s]) summary
          <> maybe [] (\d -> ["data" .= d]) extra

-- ---------------------------------------------------------------------
-- Shared insertion path (SEND-5)
-- ---------------------------------------------------------------------

insertPost ::
  (CanTime m, CanRandom m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  Sender ->
  Text ->
  MessageKind ->
  Maybe MessageId ->
  Value ->
  [Warning] ->
  m (Either PoreusError (Message, [Warning]))
insertPost c sender to kind inReplyTo payload extraWarnings = do
  resolved <- resolveTarget c to
  case resolved of
    Left e -> pure (Left e)
    Right Resolved{resAddress, resName, resWarnings} -> do
      r <- storeMessage c sender resAddress resName kind inReplyTo payload
      pure (Right (r, resWarnings <> extraWarnings))

insertPostTo ::
  (CanTime m, CanRandom m, MonadIO m) =>
  Connection ->
  Sender ->
  SessionAddress ->
  Maybe AgentName ->
  MessageKind ->
  Maybe MessageId ->
  Value ->
  [Warning] ->
  m (Either PoreusError (Message, [Warning]))
insertPostTo c sender toAddr toName kind inReplyTo payload warnings = do
  r <- storeMessage c sender toAddr toName kind inReplyTo payload
  pure (Right (r, warnings))

-- | Stamp id + timestamp, insert, and return the stored record.
-- Messages are immutable once posted (SEND-5(2)).
storeMessage ::
  (CanTime m, CanRandom m, MonadIO m) =>
  Connection ->
  Sender ->
  SessionAddress ->
  Maybe AgentName ->
  MessageKind ->
  Maybe MessageId ->
  Value ->
  m Message
storeMessage c Sender{senderAddress, senderName} toAddr toName kind inReplyTo payload = do
  now <- Timestamp <$> currentTime
  hex <- randomHex4
  let mid = newMessageId senderAddress senderName now hex
  seq_ <- liftIO $ do
    execute
      c
      "INSERT INTO messages\n\
      \  (id, from_address, to_address, from_name, to_name, kind, in_reply_to, payload, created_at)\n\
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ( mid
      , senderAddress
      , toAddr
      , senderName
      , toName
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
      , msgTo = toAddr
      , msgFromName = senderName
      , msgToName = toName
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
