module Poreus.Query
  ( -- * The one query surface (simplification D)
    QueryScope (..)
  , parseScope
  , QueryFilters (..)
  , noQueryFilters
  , QueryResult (..)
  , ThreadStatus (..)
  , runQuery

    -- * Internals exposed for tests
  , deriveThreadStatus
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, NamedParam (..), Query (..), queryNamed)

import Poreus.Time (Timestamp)
import Poreus.Types

-- | The four read scenarios served by one operation: inbox snapshot
-- (RECV-3), open-requests sweep (RECV-4), activity history (RECV-6),
-- and thread view with derived closure (THRD-1/2).
data QueryScope = ScopeInbox | ScopeOpen | ScopeHistory | ScopeThread
  deriving stock (Show, Eq)

-- | Note [Adoption is not a query mode any more]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- `open` used to take an `adoption` flag that widened the view to
-- requests addressed to my name but sitting in a dead session's
-- mailbox. With mailboxes owned by roles (ADR-0017) there is nothing
-- to widen: a request to a role is in the role's mailbox from the
-- start, and whoever holds the role reads it. The flag is gone rather
-- than deprecated, because a flag that silently does nothing is worse
-- than one that fails.
parseScope :: Text -> Maybe QueryScope
parseScope = \case
  "inbox" -> Just ScopeInbox
  "open" -> Just ScopeOpen
  "history" -> Just ScopeHistory
  "thread" -> Just ScopeThread
  _ -> Nothing

-- | Composable filters. Not every filter applies to every scope;
-- inapplicable ones are ignored.
data QueryFilters = QueryFilters
  { qfThread :: !(Maybe MessageId)
  -- ^ Required for 'ScopeThread'.
  , qfFrom :: !(Maybe Text)
  -- ^ Sender: matches address or name annotation.
  , qfInvolving :: !(Maybe Text)
  -- ^ History: address or name to look at (defaults to me).
  , qfKind :: !(Maybe MessageKind)
  , qfSince :: !(Maybe Timestamp)
  , qfLimit :: !(Maybe Int)
  }
  deriving stock (Show, Eq)

noQueryFilters :: QueryFilters
noQueryFilters = QueryFilters Nothing Nothing Nothing Nothing Nothing Nothing

-- | Derived, convention-based closure state (THRD-2): recomputed on
-- read, never stored, never an input to any other behavior. Consumers
-- with custom vocabularies ignore it and read the raw thread.
data ThreadStatus = ThreadStatus
  { thsState :: !Text
  -- ^ "open" | "active" | "terminal"
  , thsTerminalNotice :: !(Maybe MessageId)
  , thsTerminalEvent :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

instance ToJSON ThreadStatus where
  toJSON t =
    object
      [ "state" .= thsState t
      , "terminal_notice" .= thsTerminalNotice t
      , "terminal_event" .= thsTerminalEvent t
      ]

data QueryResult = QueryResult
  { qrMessages :: ![Message]
  , qrThreadStatus :: !(Maybe ThreadStatus)
  }
  deriving stock (Show, Eq)

defaultHistoryLimit :: Int
defaultHistoryLimit = 10

-- | Run one query. Side-effect-free: never touches a cursor
-- (ADR-0005) — callers wanting "new since my last look" pass their own
-- `since`. The first mailbox in the list is the caller's own session,
-- and `history` defaults to it when `involving` is absent.
runQuery ::
  MonadIO m =>
  Connection ->
  -- | the mailboxes I read: my own, plus my role's if I hold one
  [Mailbox] ->
  QueryScope ->
  QueryFilters ->
  m (Either PoreusError QueryResult)
runQuery c boxes scope filters = case scope of
  ScopeInbox -> Right . flip QueryResult Nothing <$> inbox
  ScopeOpen -> Right . flip QueryResult Nothing <$> open
  ScopeHistory -> Right . flip QueryResult Nothing <$> history
  ScopeThread -> thread
  where
    QueryFilters{qfThread, qfFrom, qfInvolving, qfKind, qfSince, qfLimit} = filters

    -- Shared SQL fragments. All order by seq — the total order (C-5).
    commonFilters =
      concat
        [ maybe [] (const [" AND (from_address = :from OR from_name = :from)"]) qfFrom
        , maybe [] (const [" AND kind = :kind"]) qfKind
        , maybe [] (const [" AND created_at > :since"]) qfSince
        ]
    commonParams =
      concat
        [ maybe [] (\v -> [":from" := v]) qfFrom
        , maybe [] (\k -> [":kind" := messageKindText k]) qfKind
        , maybe [] (\s -> [":since" := s]) qfSince
        ]
    limitClause = maybe "" (const " LIMIT :limit") qfLimit
    limitParam = maybe [] (\n -> [":limit" := n]) qfLimit

    -- One named parameter per mailbox, so the IN list is still fully
    -- bound rather than spliced. An empty mailbox list yields a clause
    -- that matches nothing, which is the honest answer.
    boxNames = [":box" <> T.pack (show i) | i <- [0 .. length boxes - 1]]
    boxClause
      | null boxes = "(NULL)"
      | otherwise = "(" <> T.intercalate ", " boxNames <> ")"
    boxParams = [n := mailboxKey b | (n, b) <- zip boxNames boxes]

    inbox = liftIO $ do
      let sql =
            "SELECT "
              <> messageColumns
              <> " FROM messages WHERE to_mailbox IN "
              <> boxClause
              <> mconcat commonFilters
              <> " ORDER BY seq"
              <> limitClause
      queryNamed c (Query sql) (boxParams <> commonParams <> limitParam)

    open = liftIO $ do
      let sql =
            "SELECT "
              <> messageColumns
              <> " FROM messages m\n\
                 \WHERE m.kind = 'request'\n\
                 \  AND NOT EXISTS (SELECT 1 FROM messages n WHERE n.in_reply_to = m.id AND n.kind = 'notice')\n\
                 \  AND m.to_mailbox IN "
              <> boxClause
              <> mconcat commonFilters
              <> " ORDER BY seq"
              <> limitClause
      queryNamed c (Query sql) (boxParams <> commonParams <> limitParam)

    history = liftIO $ do
      let who = case (qfInvolving, boxes) of
            (Just w, _) -> w
            (Nothing, b : _) -> mailboxKey b
            (Nothing, []) -> ""
          sql =
            "SELECT "
              <> messageColumns
              <> " FROM messages\n\
                 \WHERE (from_address = :who OR from_name = :who OR to_mailbox = :who)"
              <> mconcat commonFilters
              <> " ORDER BY seq DESC LIMIT :limit"
          params =
            [":who" := who, ":limit" := fromMaybe defaultHistoryLimit qfLimit]
              <> commonParams
      queryNamed c (Query sql) params

    thread = case qfThread of
      Nothing ->
        pure . Left $
          mkErrorWithAction InvalidInput "scope 'thread' requires a thread id" "pass thread: <message_id>"
      Just rootId -> do
        rows <-
          liftIO $
            queryNamed
              c
              ( Query
                  ( "SELECT "
                      <> messageColumns
                      <> " FROM messages\n\
                         \WHERE id = :root OR in_reply_to = :root ORDER BY seq"
                  )
              )
              [":root" := rootId]
        let isRoot m = msgId m == rootId
        if not (any isRoot rows)
          then
            pure . Left $
              PoreusError
                UnknownMessage
                ("message '" <> unMessageId rootId <> "' does not exist")
                (Just "check the id; use messages scope: history to list recent traffic")
          else do
            let replies = filter (not . isRoot) rows
            pure (Right (QueryResult rows (Just (deriveThreadStatus replies))))

-- | THRD-2 projection over the reply notices of one thread. `terminal`
-- reports the first notice carrying a recommended terminal event.
deriveThreadStatus :: [Message] -> ThreadStatus
deriveThreadStatus replies =
  let notices = [m | m <- replies, msgKind m == MKNotice]
      terminals =
        [ (msgId m, ev)
        | m <- notices
        , Just ev <- [messageEvent m]
        , isTerminalEvent ev
        ]
   in case (notices, terminals) of
        ([], _) -> ThreadStatus "open" Nothing Nothing
        (_, []) -> ThreadStatus "active" Nothing Nothing
        (_, (mid, ev) : _) -> ThreadStatus "terminal" (Just mid) (Just ev)
