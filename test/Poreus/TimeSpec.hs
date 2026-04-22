module Poreus.TimeSpec (spec) where

import qualified Data.Aeson as A
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Test.Hspec

import Poreus.Time

parseTs :: String -> UTCTime
parseTs = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z"

spec :: Spec
spec = do
  describe "formatUtc" $ do
    it "renders ISO 8601 UTC with Z suffix" $ do
      formatUtc (parseTs "2026-04-22T12:34:56Z") `shouldBe` ("2026-04-22T12:34:56Z" :: Text)

    it "zero-pads month/day/hour" $ do
      formatUtc (parseTs "2026-01-02T03:04:05Z") `shouldBe` ("2026-01-02T03:04:05Z" :: Text)

  describe "formatTaskStamp" $ do
    it "emits YYYYMMDD-HHmmss" $ do
      formatTaskStamp (parseTs "2026-04-22T12:34:56Z")
        `shouldBe` ("20260422-123456" :: Text)

  describe "parseUtcLoose" $ do
    it "accepts Z-suffixed ISO 8601" $ do
      parseUtcLoose "2026-04-22T12:34:56Z" `shouldBe` Just (parseTs "2026-04-22T12:34:56Z")

    it "accepts offset-suffixed ISO 8601" $ do
      parseUtcLoose "2026-04-22T14:34:56+02:00"
        `shouldBe` Just (parseTs "2026-04-22T12:34:56Z")

    it "rejects garbage" $ do
      parseUtcLoose "tomorrow" `shouldBe` Nothing

  describe "Timestamp round-trips" $ do
    it "JSON encodes to the same text formatUtc produces" $ do
      let t = parseTs "2026-04-22T12:34:56Z"
      A.encode (Timestamp t) `shouldBe` "\"2026-04-22T12:34:56Z\""

    it "JSON decodes back to the same UTCTime" $ do
      let t = parseTs "2026-04-22T12:34:56Z"
      A.decode "\"2026-04-22T12:34:56Z\"" `shouldBe` Just (Timestamp t)
