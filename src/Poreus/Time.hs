module Poreus.Time
  ( -- * Newtype
    Timestamp (..)
    -- * Pure formatters / parsers
  , formatUtc
  , formatTaskStamp
  , parseUtcLoose
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.Time.Format as TF
import Database.SQLite.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))

-- | A wall-clock timestamp. All Poreus timestamps are UTC and render as
-- ISO 8601 with a `Z` suffix ("2026-04-22T12:34:56Z"). Storage format
-- (SQLite TEXT column) matches the JSON format, so the two round-trip
-- cleanly.
newtype Timestamp = Timestamp {unTimestamp :: UTCTime}
  deriving stock (Eq, Ord, Show)

instance ToJSON Timestamp where
  toJSON (Timestamp t) = String (formatUtc t)

instance FromJSON Timestamp where
  parseJSON = withText "Timestamp" $ \t ->
    case parseUtcLoose t of
      Just v -> pure (Timestamp v)
      Nothing -> fail ("invalid timestamp: " <> T.unpack t)

instance ToField Timestamp where
  toField (Timestamp t) = toField (formatUtc t)

instance FromField Timestamp where
  fromField f = do
    txt <- fromField f
    case parseUtcLoose txt of
      Just v -> Ok (Timestamp v)
      Nothing ->
        returnError ConversionFailed f ("bad timestamp: " <> T.unpack txt)

-- | ISO 8601 UTC with Z suffix: 2026-04-22T13:45:07Z
formatUtc :: UTCTime -> Text
formatUtc = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Compact timestamp for task ids: YYYYMMDD-HHmmss (no separators).
formatTaskStamp :: UTCTime -> Text
formatTaskStamp = T.pack . formatTime defaultTimeLocale "%Y%m%d-%H%M%S"

-- | Parse ISO8601 in a few shapes (Z suffix, offset, or naive). Used by
-- migrations where the legacy store mixed formats.
parseUtcLoose :: Text -> Maybe UTCTime
parseUtcLoose t =
  let s = T.unpack t
      attempts =
        [ "%Y-%m-%dT%H:%M:%SZ"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%dT%H:%M:%S%z"
        , "%Y-%m-%dT%H:%M:%S%Q%z"
        , "%Y-%m-%dT%H:%M:%S%Q"
        ]
   in foldr
        (\fmt acc -> case TF.parseTimeM True defaultTimeLocale fmt s of
          Just v -> Just v
          Nothing -> acc)
        Nothing
        attempts
