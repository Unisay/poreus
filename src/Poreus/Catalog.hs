module Poreus.Catalog
  ( -- * Filters
    DiscoverFilters (..)
  , noFilters

    -- * Entries
  , CatalogName (..)
  , CatalogSession (..)
  , Catalog (..)

    -- * Discovery (DISC-1/2/4)
  , discover
  , processStateText
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)

import Poreus.Deliver (pendingCount)
import Poreus.Effects.Env (CanEnv)
import Poreus.Effects.FileSystem (CanFileSystem)
import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Name (NameRow (..), listNames)
import Poreus.Profile (EndpointRow (..), endpointsOf)
import Poreus.Session (SessionRow (..), getSession, hostNamesByAddress, listSessions, sessionLive)
import Poreus.Time (Timestamp)
import Poreus.Types

import Database.SQLite.Simple (Connection)

data DiscoverFilters = DiscoverFilters
  { dfTag :: !(Maybe Text)
  , dfVerb :: !(Maybe Text)
  , dfAddress :: !(Maybe Text)
  -- ^ Restrict to one address: a role name or a session address.
  }
  deriving stock (Show, Eq)

noFilters :: DiscoverFilters
noFilters = DiscoverFilters Nothing Nothing Nothing

-- | "alive" or "dead" — an operating-system fact about a process,
-- computed at the moment of the call.
--
-- Note [Presence annotates, it does not filter]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- `discover` used to take `live_only`. On 2026-08-18 it returned an
-- empty name list while a named session was in fact serving, the
-- caller read the empty list as "no such role", and fell back to
-- guessing a session by workspace — which picked the wrong one of two
-- sessions sharing a repo. A filter turns a wrong presence reading
-- into a wrong routing decision; an annotation leaves the routing
-- decision on the role, where it belongs.
--
-- The word is deliberately narrow. It says a process exists, not that
-- anyone is reading. A wedged session reads "alive", and mail to a
-- role with a dead holder is queued rather than refused, so a sender
-- rarely needs this field at all.
processStateText :: Bool -> Text
processStateText alive = if alive then "alive" else "dead"

-- | A role in the catalog: name, profile, endpoints, the current
-- holder's process state, and how much mail is queued and undrained.
data CatalogName = CatalogName
  { cnName :: !AgentName
  , cnSummary :: !(Maybe Text)
  , cnTags :: ![Text]
  , cnEndpoints :: ![EndpointRow]
  , cnHolderProcess :: !(Maybe Text)
  -- ^ Nothing when no session holds the role at all.
  , cnHolderHostName :: !(Maybe Text)
  -- ^ What the host calls the holder session right now — the ring
  -- target, and what a human needs to find the right window. Resolved
  -- per call, never stored.
  , cnQueued :: !Int
  , cnProfileUpdatedAt :: !(Maybe Timestamp)
  }
  deriving stock (Show, Eq)

instance ToJSON CatalogName where
  toJSON n =
    object
      [ "name" .= cnName n
      , "summary" .= cnSummary n
      , "tags" .= cnTags n
      , "endpoints" .= cnEndpoints n
      , "holder_process" .= cnHolderProcess n
      , "holder_host_name" .= cnHolderHostName n
      , "queued" .= cnQueued n
      , "profile_updated_at" .= cnProfileUpdatedAt n
      ]

-- | A session in the catalog: auto-provisioned entries appear without
-- any registration (REG-2).
data CatalogSession = CatalogSession
  { csAddress :: !SessionAddress
  , csWorkspace :: !Text
  , csName :: !(Maybe AgentName)
  , csHostName :: !(Maybe Text)
  , csProcess :: !Text
  , csFirstSeenAt :: !Timestamp
  }
  deriving stock (Show, Eq)

instance ToJSON CatalogSession where
  toJSON s =
    object
      [ "address" .= csAddress s
      , "workspace" .= csWorkspace s
      , "name" .= csName s
      , "host_name" .= csHostName s
      , "process" .= csProcess s
      , "first_seen_at" .= csFirstSeenAt s
      ]

data Catalog = Catalog
  { catNames :: ![CatalogName]
  , catSessions :: ![CatalogSession]
  }
  deriving stock (Show, Eq)

instance ToJSON Catalog where
  toJSON c = object ["names" .= catNames c, "sessions" .= catSessions c]

-- | Browse the catalog (DISC-1) — both kinds of entry, filterable by
-- tag, by offered verb (DISC-2: exact match, no fuzz), or restricted
-- to one address. There is no presence filter, on purpose: see
-- 'processStateText'.
discover ::
  (CanSystemInfo m, CanEnv m, CanFileSystem m, MonadIO m) =>
  Connection ->
  DiscoverFilters ->
  m Catalog
discover c DiscoverFilters{dfTag, dfVerb, dfAddress} = do
  -- Read once for the whole listing. `holder_host_name` is the ring
  -- target peers act on, so it must be the host's name NOW, not a
  -- stored copy: see Note [The host's name is not stored].
  hostNames <- hostNamesByAddress c
  let hostNameOf addr = lookup addr hostNames
  nameRows <- listNames c
  names <- forM nameRows $ \nr -> do
    eps <- endpointsOf c (nameName nr)
    holder <- case nameBoundSession nr of
      Nothing -> pure Nothing
      -- A binding to a session the store no longer knows counts dead.
      Just h -> getSession c h
    state <- case holder of
      Nothing -> pure (if isJust (nameBoundSession nr) then Just (processStateText False) else Nothing)
      Just row -> Just . processStateText <$> sessionLive row
    queued <- pendingCount c (MailboxRole (nameName nr))
    pure
      CatalogName
        { cnName = nameName nr
        , cnSummary = nameSummary nr
        , cnTags = nameTags nr
        , cnEndpoints = eps
        , cnHolderProcess = state
        , cnHolderHostName = hostNameOf . sessAddress =<< holder
        , cnQueued = queued
        , cnProfileUpdatedAt = nameProfileUpdatedAt nr
        }
  sessionRows <- listSessions c
  sessions <- forM sessionRows $ \sr -> do
    live <- sessionLive sr
    let bound = [nameName nr | nr <- nameRows, nameBoundSession nr == Just (sessAddress sr)]
    pure
      CatalogSession
        { csAddress = sessAddress sr
        , csWorkspace = sessWorkspace sr
        , csName = case bound of (n : _) -> Just n; [] -> Nothing
        , csHostName = hostNameOf (sessAddress sr)
        , csProcess = processStateText live
        , csFirstSeenAt = sessFirstSeenAt sr
        }
  let capabilityFiltered = isJust dfTag || isJust dfVerb
      names' =
        [ n
        | n <- names
        , maybe True (`elem` cnTags n) dfTag
        , maybe True (\v -> v `elem` map epVerb (cnEndpoints n)) dfVerb
        , maybe True (== unAgentName (cnName n)) dfAddress
        ]
      matchedNames = map cnName names'
      sessions' =
        [ s
        | s <- sessions
        , maybe
            True
            (\a -> a == unSessionAddress (csAddress s) || Just (AgentName a) == csName s)
            dfAddress
        , not capabilityFiltered || maybe False (`elem` matchedNames) (csName s)
        ]
  pure (Catalog names' sessions')
