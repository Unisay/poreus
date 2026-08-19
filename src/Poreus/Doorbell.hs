module Poreus.Doorbell
  ( Doorbell (..)
  , doorbellFor
  , doorbellBody
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Database.SQLite.Simple (Connection)

import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Name (NameRow (..), getName)
import Poreus.Session (SessionRow (..), getSession, sessionLive)
import Poreus.Types

-- | What the posting model should do next to shorten delivery latency:
-- ring the recipient's host session once.
--
-- Note [The doorbell]
-- ~~~~~~~~~~~~~~~~~~~
-- poreus stores the message; the host wakes the session. Those are two
-- different systems and only the first one makes a promise. The ledger
-- guarantees delivery at the recipient's next prompt or tool call. The
-- doorbell only makes that happen sooner, and it can be held, refused,
-- or lost to a permission-class mismatch without the sender learning
-- anything it can branch on.
--
-- Three rules follow, and all three are in the text handed to the
-- model, because the model is the one holding the SendMessage tool:
--
--   * Ring once. A retried poke turns a latency optimisation into a
--     denial of service against a person's attention.
--   * Never wait on it, never branch on it. The post already
--     succeeded; the doorbell cannot change that.
--   * Never put content in it. The native transport has a size
--     ceiling, a held poke parks a second copy of the message where
--     neither side controls it, and an own-child socket write arrives
--     framed as `role: user` — indistinguishable from something the
--     human typed.
--
-- The target is the host's own session name, not a workspace guess.
-- Two live sessions shared one repository on 2026-08-18 and a
-- workspace match picked the wrong one; a latency layer must not be
-- able to reintroduce that.
data Doorbell = Doorbell
  { dbAgent :: !Text
  -- ^ The host session name to pass to SendMessage.
  , dbBody :: !Text
  }
  deriving stock (Show, Eq)

instance ToJSON Doorbell where
  toJSON d =
    object
      [ "agent" .= dbAgent d
      , "body" .= dbBody d
      , "how"
          .= ( "Optional latency step: call SendMessage once with to: '"
                 <> dbAgent d
                 <> "' and exactly this body. Do not retry it, do not wait for it, do not \
                    \change the body, and do not act on whether it succeeds — the message \
                    \is already stored and will be delivered regardless." ::
                 Text
             )
      ]

-- | The fixed, payload-free body. Roughly 100 bytes, the same every
-- time, so a host that drops duplicates drops the right thing.
doorbellBody :: Text
doorbellBody = "poreus: you have new mail. Check it with the poreus messages tool (scope: inbox)."

-- | The doorbell for a mailbox, when there is a live process to ring
-- and the host published a name for it. Nothing otherwise — an unheld
-- role, a dead holder, or a session the host does not name are all
-- ordinary, and all mean the message simply waits in the ledger.
doorbellFor ::
  (CanSystemInfo m, MonadIO m) =>
  Connection ->
  Mailbox ->
  m (Maybe Doorbell)
doorbellFor c box = do
  msess <- case box of
    MailboxSession addr -> getSession c addr
    MailboxRole name -> do
      row <- getName c name
      case row >>= nameBoundSession of
        Nothing -> pure Nothing
        Just holder -> getSession c holder
  case msess of
    Nothing -> pure Nothing
    Just sess -> do
      live <- sessionLive sess
      pure $ case (live, sessHostName sess) of
        (True, Just nm) -> Just (Doorbell nm doorbellBody)
        _ -> Nothing
