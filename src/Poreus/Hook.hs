module Poreus.Hook
  ( runHook

    -- * Pure pieces exposed for tests
  , HookInput (..)
  , parseHookInput
  , renderDigest
  , renderClaim
  , hookOutput
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.SQLite.Simple (Connection)

import Poreus.DB (withDB)
import Poreus.Deliver (Delivered (..), deliverPending)
import Poreus.Identity (Identity (..), resolveIdentityFrom)
import Poreus.JSON (jsonToText)
import Poreus.Mcp.Digest (messageDigest)
import Poreus.Name (ClaimOutcome (..), claimName, mailboxesOf, suggestRoleName)
import Poreus.Retention (retentionDays, sweepIfDue)
import Poreus.Session (ensureSession)
import Poreus.Types (AgentName (..), Message, SessionAddress)

-- | What the Claude Code hook protocol hands us on stdin. Only three
-- fields matter; the rest of the record is ignored.
data HookInput = HookInput
  { hiSessionId :: !Text
  , hiCwd :: !Text
  , hiEventName :: !Text
  }
  deriving stock (Show, Eq)

parseHookInput :: BL.ByteString -> Maybe HookInput
parseHookInput raw = do
  Object o <- A.decode raw
  String sid <- KM.lookup "session_id" o
  let textField k = case KM.lookup k o of
        Just (String t) -> t
        _ -> ""
  pure
    HookInput
      { hiSessionId = sid
      , hiCwd = textField "cwd"
      , hiEventName = textField "hook_event_name"
      }

-- | The short-lived hook companion (ADR-0013/0014/0017): reads the hook
-- record from stdin, does the four things a session needs done at the
-- edges of a turn, and exits 0 — always, so a poreus hiccup never
-- breaks the user's session.
--
-- The hook resolves its address through the SAME chain as the server
-- (ADR-0016), with its stdin session_id playing the role of the
-- host-provided id. This is what keeps the two on one mailbox: the
-- host rotates session ids across compactions and re-spawns servers
-- with fresh ids while the original connection keeps serving, so
-- deriving the address from the stdin id alone silently split
-- delivery between two addresses.
--
-- Note [Why the hook carries this much]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Deleting the server's background thread (ADR-0017) left three duties
-- without a home. Two of them land here, because the hook is the only
-- poreus code that runs on a schedule set by a person working rather
-- than by a timer nobody watches:
--
--   * the retention sweep, behind an hourly guard;
--   * the role claim at SessionStart.
--
-- The claim is the fix for a specific two-day failure: every
-- @--resume@ minted a fresh address, the old session row was marked
-- ended, its name went unbound, and nothing said so. The session was
-- unreachable by role until a human noticed. Claiming here makes the
-- rebind automatic and, because the claim happens before the mailbox
-- drain below, the role's backlog arrives in the same turn.
runHook :: IO ()
runHook = do
  outcome <- try run
  case outcome :: Either SomeException () of
    Right () -> pure ()
    Left _ -> pure ()
  where
    run = do
      raw <- BL.getContents
      case parseHookInput raw of
        Nothing -> pure ()
        Just hi -> do
          (delivered, claimed) <- withDB $ \c -> do
            identity <- resolveIdentityFrom c (Just (hiSessionId hi)) (hiCwd hi)
            let addr = idAddress identity
            -- No pid/boot: the hook is not the serving process and
            -- must not masquerade as one.
            _ <- ensureSession c addr (hiCwd hi) Nothing Nothing
            claimed <-
              if hiEventName hi == "SessionStart"
                then autoClaim c addr (T.unpack (hiCwd hi))
                else pure Nothing
            boxes <- mailboxesOf c addr
            d <- deliverPending c boxes
            days <- retentionDays
            _ <- sweepIfDue c days
            pure (d, claimed)
          for_ (hookOutput (hiEventName hi) delivered claimed) TIO.putStr

-- | Claim the workspace's role when it is free or its holder's process
-- is gone. 'suggestRoleName' answers exactly that question already —
-- it returns Nothing when this session holds a name, when the
-- workspace is not a repository, when the derived name is invalid, or
-- when a live session holds the role. A live holder is left alone: the
-- hook never takes a role away from a working session.
autoClaim :: Connection -> SessionAddress -> FilePath -> IO (Maybe AgentName)
autoClaim c me workspace = do
  suggestion <- suggestRoleName c me workspace
  case suggestion of
    Nothing -> pure Nothing
    Just nm -> do
      outcome <- claimName c me (unAgentName nm) False
      pure $ case outcome of
        Right ClaimOutcome{coName} -> Just coName
        -- A concurrent claimant won the race. Nothing to announce and
        -- nothing to fix: the other session holds the role.
        Left _ -> Nothing

-- | Per-event output shape: SessionStart and UserPromptSubmit add
-- plain stdout to context; every other event uses the
-- hookSpecificOutput.additionalContext envelope. Nothing pending and
-- nothing claimed → no output at all (silence is the common case).
hookOutput :: Text -> [Delivered] -> Maybe AgentName -> Maybe Text
hookOutput event delivered claimed
  | T.null body = Nothing
  | event `elem` ["SessionStart", "UserPromptSubmit"] = Just body
  | otherwise =
      Just . jsonToText $
        object
          [ "hookSpecificOutput"
              .= object
                [ "hookEventName" .= event
                , "additionalContext" .= body
                ]
          ]
  where
    body =
      T.concat . catMaybes $
        [ renderClaim <$> claimed
        , if null delivered then Nothing else Just (renderDigest (map dMessage delivered))
        ]

renderDigest :: [Message] -> Text
renderDigest msgs =
  T.unlines $
    ("[poreus] " <> T.pack (show (length msgs)) <> " message(s) delivered:")
      : map messageDigest msgs

-- | One line telling the model which role it now answers to. The claim
-- already happened — this is a statement, not a suggestion, because a
-- session that does not know its own role cannot describe itself to a
-- peer.
renderClaim :: AgentName -> Text
renderClaim (AgentName nm) =
  "[poreus] This session now holds the role '"
    <> nm
    <> "'. Peers address it by that name, and its mailbox outlives this process. Use release_name to hand the role over.\n"
