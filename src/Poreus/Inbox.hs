module Poreus.Inbox
  ( -- * Filters
    InboxFilters (..)
  , noFilters
    -- * Reading
  , inboxSnapshot
  , inboxStreamTick
  , inboxOpenRequests
    -- * Formatting
  , formatInboxLine
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query)

import Poreus.Message (Message (..), MessageKind (..), messageKindText)
import Poreus.Time (Timestamp, formatUtc, unTimestamp)
import Poreus.Types

-- ---------------------------------------------------------------------
-- Filters
-- ---------------------------------------------------------------------

data InboxFilters = InboxFilters
  { ifKind :: !(Maybe MessageKind)
  , ifInReplyTo :: !(Maybe TaskId)
  , ifFrom :: !(Maybe Alias)
  , ifSince :: !(Maybe Timestamp)
  , ifOpenOnly :: !Bool
    -- ^ When True: keep only requests for which this alias has not yet
    -- sent any notice in reply (regardless of `payload.event`). Implies
    -- `ifKind = Just MKRequest`.
  }
  deriving stock (Show, Eq)

noFilters :: InboxFilters
noFilters = InboxFilters Nothing Nothing Nothing Nothing False

-- ---------------------------------------------------------------------
-- Reading
-- ---------------------------------------------------------------------

messageFields :: Text
messageFields =
  "id, from_alias, to_alias, kind, in_reply_to, payload, subscribe, created_at"

-- | Snapshot — side-effect free SELECT against `messages` filtered by
-- `to_alias = alias`. Does NOT touch `watch_cursors` (see ADR-0005).
--
-- If `ifOpenOnly` is set, narrows to requests with no outgoing notice
-- from `alias` in reply (see `inboxOpenRequests`).
inboxSnapshot
  :: MonadIO m
  => Connection
  -> Alias
  -> InboxFilters
  -> m [Message]
inboxSnapshot c alias f
  | ifOpenOnly f = inboxOpenRequests c alias f
  | otherwise = liftIO $ do
      let (whereSql, params) = buildWhere alias f
          sql =
            Query
              ( "SELECT " <> messageFields
                  <> " FROM messages WHERE "
                  <> whereSql
                  <> " ORDER BY created_at, id"
              )
      query c sql params

-- | Return only requests addressed to `alias` for which `alias` has
-- **never** sent a notice in reply — regardless of whether prior
-- replies used a formal `payload.event` or freeform summary. This is
-- the source of truth for "what still needs my attention."
--
-- Honours the rest of the filters in `f` except `ifKind` (forced to
-- `request`).
inboxOpenRequests
  :: MonadIO m
  => Connection
  -> Alias
  -> InboxFilters
  -> m [Message]
inboxOpenRequests c alias f = liftIO $ do
  -- Force kind=request; preserve in_reply_to / from / since.
  let (whereSql, params) = buildWhere alias (f { ifKind = Just MKRequest })
      sql =
        Query
          ( "SELECT " <> messageFields
              <> " FROM messages req"
              <> " WHERE " <> whereSql
              <> " AND NOT EXISTS ("
              <> "   SELECT 1 FROM messages r"
              <> "    WHERE r.kind = 'notice'"
              <> "      AND r.from_alias = ?"
              <> "      AND r.in_reply_to = req.id"
              <> " )"
              <> " ORDER BY req.created_at, req.id"
          )
  -- buildWhere already qualified columns as bare names; SQLite is happy
  -- to resolve them against the aliased `req` table.
  query c sql (params ++ [unAlias alias])

-- | Build the WHERE clause and parameters. All values are flattened to
-- `Text` because `messages` columns are TEXT and SQLite coerces
-- transparently. This sidesteps heterogenous-list ToRow plumbing.
buildWhere :: Alias -> InboxFilters -> (Text, [Text])
buildWhere alias InboxFilters {..} =
  let baseClause = "to_alias = ?"
      base = [unAlias alias]
      addKind = case ifKind of
        Just k -> [(" AND kind = ?", [messageKindText k])]
        Nothing -> []
      addReply = case ifInReplyTo of
        Just t -> [(" AND in_reply_to = ?", [unTaskId t])]
        Nothing -> []
      addFrom = case ifFrom of
        Just a -> [(" AND from_alias = ?", [unAlias a])]
        Nothing -> []
      addSince = case ifSince of
        Just ts -> [(" AND created_at > ?", [formatUtc (unTimestamp ts)])]
        Nothing -> []
      extras = addKind ++ addReply ++ addFrom ++ addSince
      clause = baseClause <> T.concat (map fst extras)
      params = base ++ concatMap snd extras
   in (clause, params)

-- ---------------------------------------------------------------------
-- Stream tick (one round of follow-mode)
-- ---------------------------------------------------------------------

-- | Per-alias cursor: emit every message addressed to `alias` with
-- `created_at > last_seen`, then advance `last_seen` to the latest
-- emitted (or to `now` when no messages). First call returns the
-- entire inbox; subsequent ticks return only new arrivals.
--
-- This is the single tick that `cmdInbox --follow` calls in a loop.
-- Snapshot mode does not call this.
inboxStreamTick
  :: MonadIO m
  => Connection
  -> Alias
  -> Timestamp -- ^ now — used as floor when no messages
  -> m [Message]
inboxStreamTick c alias now = liftIO $ do
  cursor <- readCursor c alias
  msgs <- fetchSince c alias cursor
  let advanceTo = case msgs of
        [] -> formatUtc (unTimestamp now)
        _ -> latestCreatedAt msgs
  writeCursor c alias advanceTo
  pure msgs

latestCreatedAt :: [Message] -> Text
latestCreatedAt = maximum . map (formatUtc . unTimestamp . msgCreatedAt)

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

-- ---------------------------------------------------------------------
-- Formatting (Monitor stdout)
-- ---------------------------------------------------------------------

-- | Format one message as the single-line `[POREUS:IN] ...` output for
-- the Claude Code Monitor tool. Format is intentionally stable:
--
--   [POREUS:IN] <from> request (<request_kind>): <desc-or-url>
--   [POREUS:IN] <from> notice [in-reply-to: <id>]: <summary or event>
--
-- `<request_kind>` reads from `payload.request_kind` (`freetext` /
-- `rpc` by convention); `<summary>` reads `payload.summary` and falls
-- back to `payload.event`.
formatInboxLine :: Message -> Text
formatInboxLine m = case msgKind m of
  MKRequest ->
    let rk = lookupTextField "request_kind" (msgPayload m) `orElse` "?"
        body = case rk of
          "rpc" -> lookupTextField "url" (msgPayload m) `orElse` "?"
          _ -> trim120 (lookupTextField "description" (msgPayload m) `orElse` "")
     in T.concat
          [ "[POREUS:IN] "
          , unAlias (msgFrom m)
          , " request ("
          , rk
          , "): "
          , body
          ]
  MKNotice ->
    let summary =
          (lookupTextField "summary" (msgPayload m) >>= nonEmpty)
            `orElse` (lookupTextField "event" (msgPayload m) `orElse` "")
        replyPart = case msgInReplyTo m of
          Just (TaskId tid) -> T.concat [" [in-reply-to: ", tid, "]"]
          Nothing -> ""
     in T.concat
          [ "[POREUS:IN] "
          , unAlias (msgFrom m)
          , " notice"
          , replyPart
          , ": "
          , trim120 summary
          ]

orElse :: Maybe Text -> Text -> Text
orElse (Just x) _ = x
orElse Nothing y = y

nonEmpty :: Text -> Maybe Text
nonEmpty t
  | T.null t = Nothing
  | otherwise = Just t

trim120 :: Text -> Text
trim120 = T.take 120

lookupTextField :: Text -> A.Value -> Maybe Text
lookupTextField k = \case
  A.Object o -> case KM.lookup (AK.fromText k) o of
    Just (A.String s) -> Just s
    _ -> Nothing
  _ -> Nothing
