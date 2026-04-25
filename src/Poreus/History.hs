-- | `poreus history` — recent message history for the current alias,
-- rendered either as a markdown table or a structured JSON array.
--
-- Queries `messages` directly (the new transport-only schema has no
-- `tasks` table). For each row we display: when, direction, peer, kind,
-- and a one-line summary distilled from the payload.
module Poreus.History
  ( -- * Row
    HistoryRow (..)
  , toHistoryRow
    -- * DB
  , historyMessages
    -- * Formatters
  , formatHistoryTable
    -- * Pure helpers (exposed for tests)
  , formatWhen
  , firstLine
  , truncateSummary
  , summaryOf
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Database.SQLite.Simple (Connection, Query (..), query)

import Poreus.Message (Message (..), MessageKind (..), messageKindText)
import Poreus.Time (Timestamp (..))
import Poreus.Types

data HistoryRow = HistoryRow
  { hrId :: !TaskId
  , hrWhen :: !Text       -- ^ "MM-DD HH:MM" in UTC
  , hrDir :: !Text        -- ^ "->" (outgoing) or "<-" (incoming)
  , hrPeer :: !Alias
  , hrKind :: !MessageKind
  , hrInReplyTo :: !(Maybe TaskId)
  , hrSummary :: !Text
  , hrCreatedAt :: !Timestamp
  }
  deriving stock (Show, Eq)

instance A.ToJSON HistoryRow where
  toJSON r =
    A.object
      [ "id" A..= hrId r
      , "when" A..= hrWhen r
      , "dir" A..= hrDir r
      , "peer" A..= hrPeer r
      , "kind" A..= hrKind r
      , "in_reply_to" A..= hrInReplyTo r
      , "summary" A..= hrSummary r
      , "created_at" A..= hrCreatedAt r
      ]

historyMessages :: MonadIO m => Connection -> Alias -> Int -> m [Message]
historyMessages c alias n = liftIO $
  query
    c
    ( Query
        ( T.concat
            [ "SELECT id, from_alias, to_alias, kind, in_reply_to, payload,"
            , " subscribe, created_at"
            , " FROM messages WHERE from_alias = ? OR to_alias = ?"
            , " ORDER BY created_at DESC LIMIT ?"
            ]
        )
    )
    (alias, alias, n)

toHistoryRow :: Alias -> Message -> HistoryRow
toHistoryRow me m =
  HistoryRow
    { hrId = msgId m
    , hrWhen = formatWhen (unTimestamp (msgCreatedAt m))
    , hrDir = if msgFrom m == me then "->" else "<-"
    , hrPeer = if msgFrom m == me then msgTo m else msgFrom m
    , hrKind = msgKind m
    , hrInReplyTo = msgInReplyTo m
    , hrSummary = summaryOf m
    , hrCreatedAt = msgCreatedAt m
    }

-- | "MM-DD HH:MM" in UTC, matching the previous jq‑based slash command.
formatWhen :: UTCTime -> Text
formatWhen = T.pack . formatTime defaultTimeLocale "%m-%d %H:%M"

-- | First line of a text (everything up to the first `\n`).
firstLine :: Text -> Text
firstLine = T.takeWhile (/= '\n')

-- | Truncate to at most `n` Unicode code points.
truncateSummary :: Int -> Text -> Text
truncateSummary n = T.take n

-- | Compute the summary cell for a message. Conventional payload keys:
-- `description` (request), `summary`/`event` (notice), `url` (rpc
-- request). First non-empty wins; result is first-line + 70-codepoint
-- truncation.
summaryOf :: Message -> Text
summaryOf m =
  case pick of
    Just s | not (T.null s) -> truncateSummary 70 (firstLine s)
    _ -> ""
  where
    pl = msgPayload m
    pick = case msgKind m of
      MKRequest ->
        textField "description" pl
          `orElseM` textField "url" pl
      MKNotice ->
        textField "summary" pl
          `orElseM` textField "event" pl

textField :: Text -> A.Value -> Maybe Text
textField k = \case
  A.Object o -> case KM.lookup (AK.fromText k) o of
    Just (A.String s) -> Just s
    _ -> Nothing
  _ -> Nothing

orElseM :: Maybe a -> Maybe a -> Maybe a
orElseM (Just x) _ = Just x
orElseM Nothing y = y

formatHistoryTable :: [HistoryRow] -> Text
formatHistoryTable [] = "No messages.\n"
formatHistoryTable rows =
  T.unlines (header : separator : map rowLine rows)
  where
    header = "| When | Dir | Peer | Kind | Summary |"
    separator = "|---|---|---|---|---|"
    rowLine r =
      T.concat
        [ "| "
        , hrWhen r
        , " | "
        , hrDir r
        , " | "
        , unAlias (hrPeer r)
        , " | "
        , messageKindText (hrKind r)
        , " | "
        , escapeCell (hrSummary r)
        , " |"
        ]

-- | Escape characters that would corrupt a markdown table cell.
escapeCell :: Text -> Text
escapeCell = T.replace "|" "\\|"
