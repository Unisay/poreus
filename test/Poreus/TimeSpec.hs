module Poreus.TimeSpec (spec) where

import qualified Data.Aeson as A
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Test.Hspec

import Poreus.Time

parseTs :: String -> UTCTime
parseTs = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z"

parseTsMs :: String -> UTCTime
parseTsMs = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

spec :: Spec
spec = do
  describe "formatUtc" $ do
    it "renders ISO 8601 UTC with millisecond precision and Z suffix" $ do
      formatUtc (parseTs "2026-04-22T12:34:56Z")
        `shouldBe` ("2026-04-22T12:34:56.000Z" :: Text)

    it "zero-pads month/day/hour" $ do
      formatUtc (parseTs "2026-01-02T03:04:05Z")
        `shouldBe` ("2026-01-02T03:04:05.000Z" :: Text)

    it "preserves a sub-second component (milliseconds)" $ do
      formatUtc (parseTsMs "2026-04-22T12:34:56.789Z")
        `shouldBe` ("2026-04-22T12:34:56.789Z" :: Text)

    it "renders ms strings that compare correctly with lexicographic order" $ do
      let earlier = formatUtc (parseTsMs "2026-04-22T12:34:56.100Z")
          later   = formatUtc (parseTsMs "2026-04-22T12:34:56.900Z")
      compare earlier later `shouldBe` LT

  describe "formatTaskStamp" $ do
    it "emits YYYYMMDD-HHmmss (no fractional seconds)" $ do
      formatTaskStamp (parseTs "2026-04-22T12:34:56Z")
        `shouldBe` ("20260422-123456" :: Text)

  describe "parseUtcLoose" $ do
    it "accepts Z-suffixed ISO 8601 (legacy second precision)" $ do
      parseUtcLoose "2026-04-22T12:34:56Z" `shouldBe` Just (parseTs "2026-04-22T12:34:56Z")

    it "accepts Z-suffixed ISO 8601 with milliseconds" $ do
      parseUtcLoose "2026-04-22T12:34:56.789Z"
        `shouldBe` Just (parseTsMs "2026-04-22T12:34:56.789Z")

    it "accepts offset-suffixed ISO 8601" $ do
      parseUtcLoose "2026-04-22T14:34:56+02:00"
        `shouldBe` Just (parseTs "2026-04-22T12:34:56Z")

    it "rejects garbage" $ do
      parseUtcLoose "tomorrow" `shouldBe` Nothing

  describe "Timestamp round-trips" $ do
    it "JSON encodes to the same text formatUtc produces" $ do
      let t = parseTs "2026-04-22T12:34:56Z"
      A.encode (Timestamp t) `shouldBe` "\"2026-04-22T12:34:56.000Z\""

    it "JSON decodes both legacy and ms-precision timestamps" $ do
      let t = parseTs "2026-04-22T12:34:56Z"
      A.decode "\"2026-04-22T12:34:56Z\"" `shouldBe` Just (Timestamp t)
      A.decode "\"2026-04-22T12:34:56.000Z\"" `shouldBe` Just (Timestamp t)
