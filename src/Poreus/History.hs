-- | `poreus history` — recent message history for the current alias,
-- rendered either as a markdown table or a structured JSON array.
--
-- Query surface mirrors `poreus status` with no flags (all tasks where
-- the alias is on the from OR to side) but sorted newest‑first and
-- capped to a caller‑supplied row limit.
module Poreus.History
  ( -- * Row
    HistoryRow (..)
  , toHistoryRow
    -- * DB
  , historyTasks
    -- * Formatters
  , formatHistoryTable
    -- * Pure helpers (exposed for tests)
  , formatWhen
  , firstLine
  , truncateSummary
  , summaryOf
  ) where

import qualified Data.Aeson as A
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Database.SQLite.Simple (Connection, Query (..), query)

import Poreus.Task (taskFields)
import Poreus.Time (Timestamp (..))
import Poreus.Types

data HistoryRow = HistoryRow
  { hrId :: !TaskId
  , hrWhen :: !Text          -- ^ "MM-DD HH:MM" in UTC
  , hrDir :: !Text           -- ^ "->" (outgoing) or "<-" (incoming)
  , hrPeer :: !Alias
  , hrKind :: !TaskKind
  , hrStatus :: !TaskStatus
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
      , "status" A..= hrStatus r
      , "summary" A..= hrSummary r
      , "created_at" A..= hrCreatedAt r
      ]

historyTasks :: MonadIO m => Connection -> Alias -> Int -> m [Task]
historyTasks c alias n = liftIO $
  query
    c
    ( Query
        ( T.concat
            [ "SELECT "
            , taskFields
            , " FROM tasks WHERE from_alias = ? OR to_alias = ?"
            , " ORDER BY created_at DESC LIMIT ?"
            ]
        )
    )
    (alias, alias, n)

toHistoryRow :: Alias -> Task -> HistoryRow
toHistoryRow me t =
  HistoryRow
    { hrId = taskId t
    , hrWhen = formatWhen (unTimestamp (taskCreatedAt t))
    , hrDir = if taskFrom t == me then "->" else "<-"
    , hrPeer = if taskFrom t == me then taskTo t else taskFrom t
    , hrKind = taskKind t
    , hrStatus = taskStatus t
    , hrSummary = summaryOf t
    , hrCreatedAt = taskCreatedAt t
    }

-- | "MM-DD HH:MM" in UTC, matching the previous jq‑based slash command.
formatWhen :: UTCTime -> Text
formatWhen = T.pack . formatTime defaultTimeLocale "%m-%d %H:%M"

-- | First line of a text (everything up to the first `\n`).
firstLine :: Text -> Text
firstLine = T.takeWhile (/= '\n')

-- | Truncate to at most `n` Unicode code points. `Data.Text.take`
-- operates on `Char` (a code point), not on bytes or UTF-16 code units,
-- so Cyrillic / CJK content is never split mid-character.
truncateSummary :: Int -> Text -> Text
truncateSummary n = T.take n

-- | Compute the summary cell for a task: first line of the description
-- truncated to 70 code points; fall back to the URL when the
-- description is empty (common for RPC tasks); empty string if neither
-- is set.
summaryOf :: Task -> Text
summaryOf t =
  case pick of
    Just s | not (T.null s) -> truncateSummary 70 (firstLine s)
    _ -> ""
  where
    pick = case taskDescription t of
      Just d | not (T.null d) -> Just d
      _ -> taskUrl t

formatHistoryTable :: [HistoryRow] -> Text
formatHistoryTable [] = "No messages.\n"
formatHistoryTable rows =
  T.unlines (header : separator : map rowLine rows)
  where
    header = "| When | Dir | Peer | Kind | Status | Summary |"
    separator = "|---|---|---|---|---|---|"
    rowLine r =
      T.concat
        [ "| "
        , hrWhen r
        , " | "
        , hrDir r
        , " | "
        , unAlias (hrPeer r)
        , " | "
        , taskKindText (hrKind r)
        , " | "
        , taskStatusText (hrStatus r)
        , " | "
        , escapeCell (hrSummary r)
        , " |"
        ]

-- | Escape characters that would corrupt a markdown table cell.
escapeCell :: Text -> Text
escapeCell = T.replace "|" "\\|"
