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
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)

import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Effects.Time (CanTime)
import Poreus.Name (NameRow (..), listNames)
import Poreus.Profile (EndpointRow (..), endpointsOf)
import Poreus.Session (SessionRow (..), getSession, listSessions, sessionLive)
import Poreus.Time (Timestamp)
import Poreus.Types

import Database.SQLite.Simple (Connection)

data DiscoverFilters = DiscoverFilters
  { dfTag :: !(Maybe Text)
  , dfVerb :: !(Maybe Text)
  , dfAddress :: !(Maybe Text)
  -- ^ Restrict to one address: a name or a session address.
  , dfLiveOnly :: !Bool
  }
  deriving stock (Show, Eq)

noFilters :: DiscoverFilters
noFilters = DiscoverFilters Nothing Nothing Nothing False

-- | A named agent in the catalog: name, profile, endpoints, and the
-- current binding with its liveness (DISC-4: presence is the pre-flight
-- check before delegating to a role, because posts to an unbound name
-- fail fast).
data CatalogName = CatalogName
  { cnName :: !AgentName
  , cnSummary :: !(Maybe Text)
  , cnTags :: ![Text]
  , cnEndpoints :: ![EndpointRow]
  , cnBoundSession :: !(Maybe SessionAddress)
  , cnLive :: !Bool
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
      , "bound_session" .= cnBoundSession n
      , "live" .= cnLive n
      , "profile_updated_at" .= cnProfileUpdatedAt n
      ]

-- | A session in the catalog: auto-provisioned entries appear without
-- any registration (REG-2); liveness means attending (RECV-1).
data CatalogSession = CatalogSession
  { csAddress :: !SessionAddress
  , csWorkspace :: !Text
  , csName :: !(Maybe AgentName)
  , csLive :: !Bool
  , csFirstSeenAt :: !Timestamp
  }
  deriving stock (Show, Eq)

instance ToJSON CatalogSession where
  toJSON s =
    object
      [ "address" .= csAddress s
      , "workspace" .= csWorkspace s
      , "name" .= csName s
      , "live" .= csLive s
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
-- tag, by offered verb (DISC-2: exact match, no fuzz), restricted to
-- one address, or narrowed to live sessions.
discover ::
  (CanTime m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  DiscoverFilters ->
  m Catalog
discover c DiscoverFilters{dfTag, dfVerb, dfAddress, dfLiveOnly} = do
  nameRows <- listNames c
  names <- forM nameRows $ \nr -> do
    eps <- endpointsOf c (nameName nr)
    live <- case nameBoundSession nr of
      Nothing -> pure False
      Just holder -> do
        -- A binding to a session the store no longer knows counts dead.
        msess <- getSession c holder
        maybe (pure False) sessionLive msess
    pure
      CatalogName
        { cnName = nameName nr
        , cnSummary = nameSummary nr
        , cnTags = nameTags nr
        , cnEndpoints = eps
        , cnBoundSession = nameBoundSession nr
        , cnLive = live
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
        , csLive = live
        , csFirstSeenAt = sessFirstSeenAt sr
        }
  let capabilityFiltered = isJust dfTag || isJust dfVerb
      names' =
        [ n
        | n <- names
        , maybe True (`elem` cnTags n) dfTag
        , maybe True (\v -> v `elem` map epVerb (cnEndpoints n)) dfVerb
        , maybe True (\a -> a == unAgentName (cnName n)) dfAddress
            || maybe False (\a -> Just (SessionAddress a) == cnBoundSession n) dfAddress
        , not dfLiveOnly || cnLive n
        ]
      matchedBindings = [b | n <- names', Just b <- [cnBoundSession n]]
      sessions' =
        [ s
        | s <- sessions
        , maybe
            True
            (\a -> a == unSessionAddress (csAddress s) || Just (AgentName a) == csName s)
            dfAddress
        , not capabilityFiltered || csAddress s `elem` matchedBindings
        , not dfLiveOnly || csLive s
        ]
  pure (Catalog names' sessions')
