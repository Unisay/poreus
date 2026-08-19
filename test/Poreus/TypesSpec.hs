module Poreus.TypesSpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import Test.Hspec

import Poreus.Time (Timestamp (..))
import Poreus.Types

import Data.Time (defaultTimeLocale, parseTimeOrError)

spec :: Spec
spec = do
  describe "parseTarget" $ do
    it "recognizes session addresses by the s- prefix" $ do
      parseTarget "s-abc123" `shouldBe` TargetSession (SessionAddress "s-abc123")

    it "treats everything else as a name" $ do
      parseTarget "nixos" `shouldBe` TargetName (AgentName "nixos")

  describe "Mailbox" $ do
    it "round-trips through its two stored columns" $ do
      let role = MailboxRole (AgentName "nixos")
          sess = MailboxSession (SessionAddress "s-abc")
      mailboxFromRow (mailboxKey role) (mailboxKindText role) `shouldBe` role
      mailboxFromRow (mailboxKey sess) (mailboxKindText sess) `shouldBe` sess

    it "reads an unrecognised kind as a session mailbox, never a role" $ do
      -- The inert direction: a role mailbox drained by the wrong
      -- holder would misdeliver; a session mailbox nobody holds does
      -- nothing.
      mailboxFromRow "nixos" "wat" `shouldBe` MailboxSession (SessionAddress "nixos")

  describe "newMessageId" $ do
    let ts = Timestamp (parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" "2026-04-22T13:45:07Z")
    it "tags with the bound name when present" $ do
      newMessageId (SessionAddress "s-abcdef123456") (Just (AgentName "nixos")) ts "beef"
        `shouldBe` MessageId "20260422-134507-nixos-beef"

    it "falls back to the session-id fragment" $ do
      newMessageId (SessionAddress "s-abcdef123456") Nothing ts "beef"
        `shouldBe` MessageId "20260422-134507-abcdef12-beef"

  describe "lifecycle vocabulary (ADR-0007)" $ do
    it "knows the terminal set" $ do
      map isTerminalEvent ["completed", "failed", "aborted", "started", "stuck", "custom"]
        `shouldBe` [True, True, True, False, False, False]

  describe "PoreusError JSON (spec §9)" $ do
    it "carries code, message, and the corrective action" $ do
      A.toJSON (mkErrorWithAction NameHeld "held" "take over")
        `shouldBe` object
          [ "code" .= ("name-held" :: String)
          , "message" .= ("held" :: String)
          , "action" .= ("take over" :: String)
          ]

    it "omits the action when there is none" $ do
      A.toJSON (mkError StorageFailure "boom")
        `shouldBe` object ["code" .= ("storage-failure" :: String), "message" .= ("boom" :: String)]

  describe "messageEvent" $ do
    it "reads the event field of a payload" $ do
      let m =
            Message
              { msgSeq = 1
              , msgId = MessageId "x"
              , msgFrom = SessionAddress "s-a"
              , msgFromName = Nothing
              , msgTo = MailboxSession (SessionAddress "s-b")
              , msgKind = MKNotice
              , msgInReplyTo = Nothing
              , msgPayload = object ["event" .= ("completed" :: String)]
              , msgCreatedAt = Timestamp (parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" "2026-01-01T00:00:00Z")
              }
      messageEvent m `shouldBe` Just "completed"
      messageEvent m{msgPayload = A.Null} `shouldBe` Nothing
