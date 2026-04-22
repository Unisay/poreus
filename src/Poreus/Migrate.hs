module Poreus.Migrate
  ( migrateFromLegacy
  , MigrateStats (..)
    -- * Pure extractors (exposed for testing)
  , extractRepos
  , extractTask
  , extractResult
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection, Only (..), SQLError, execute, execute_, query)
import System.FilePath ((</>))

import Poreus.Config (legacyA2aQueue)
import Poreus.Effects.Env (CanEnv)
import Poreus.Effects.FileSystem
  ( CanFileSystem
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , readFileBytes
  )
import Poreus.Profile (jsonToText)
import Poreus.Time (formatUtc)

data MigrateStats = MigrateStats
  { msAgents :: !Int
  , msTasks :: !Int
  , msResults :: !Int
  , msSkipped :: !Int
  }
  deriving stock (Show, Eq)

instance A.ToJSON MigrateStats where
  toJSON s =
    A.object
      [ "agents" A..= msAgents s
      , "tasks" A..= msTasks s
      , "results" A..= msResults s
      , "skipped" A..= msSkipped s
      ]

-- | One-shot migration from the legacy file-based store.
-- Effects are split: reading fixtures goes through `CanFileSystem`, env
-- lookup through `CanEnv`, DB writes through `MonadIO`.
migrateFromLegacy
  :: (MonadIO m, CanEnv m, CanFileSystem m)
  => Connection
  -> UTCTime -- ^ fallback "now" for rows without a legacy timestamp
  -> m MigrateStats
migrateFromLegacy c fallbackNow = do
  base <- legacyA2aQueue
  existsBase <- doesDirectoryExist base
  if not existsBase
    then do
      liftIO (backfillMessages c)
      pure (MigrateStats 0 0 0 0)
    else do
      (ag, sk1) <- migrateRegistry c fallbackNow (base </> "registry.json")
      (tk, sk2) <- migrateInbox c (base </> "inbox")
      (rs, sk3) <- migrateResults c (base </> "results")
      liftIO (backfillMessages c)
      pure (MigrateStats ag tk rs (sk1 + sk2 + sk3))

-- | Backfill `messages` from the legacy `tasks` + `results` projections.
-- Idempotent: `INSERT OR IGNORE` on the message id plus a `WHERE NOT
-- EXISTS` guard skips rows that were already migrated.
--
-- Message id convention for backfilled replies: `<task_id>-reply`.
-- New replies generated at runtime by `poreus complete`/`reject` use
-- the canonical `YYYYMMDD-HHmmss-<from>-<hex>` format instead.
backfillMessages :: Connection -> IO ()
backfillMessages c = do
  execute_
    c
    "INSERT OR IGNORE INTO messages\n\
    \  (id, from_alias, to_alias, kind, in_reply_to, payload, created_at)\n\
    \SELECT t.id, t.from_alias, t.to_alias, 'request', NULL,\n\
    \       json_object(\n\
    \         'request_kind', t.kind,\n\
    \         'url',          t.url,\n\
    \         'description',  t.description,\n\
    \         'expected',     t.expected),\n\
    \       t.created_at\n\
    \FROM tasks t\n\
    \WHERE NOT EXISTS (SELECT 1 FROM messages m WHERE m.id = t.id)"
  execute_
    c
    "INSERT OR IGNORE INTO messages\n\
    \  (id, from_alias, to_alias, kind, in_reply_to, payload, created_at)\n\
    \SELECT r.task_id || '-reply', t.to_alias, t.from_alias, 'reply', r.task_id,\n\
    \       json_object(\n\
    \         'status',    r.status,\n\
    \         'summary',   r.summary,\n\
    \         'artifacts', CASE\n\
    \           WHEN r.artifacts IS NULL OR r.artifacts = '' THEN json('[]')\n\
    \           ELSE json(r.artifacts)\n\
    \         END,\n\
    \         'reason',    CASE WHEN r.status = 'rejected' THEN r.summary ELSE NULL END),\n\
    \       r.completed_at\n\
    \FROM results r\n\
    \JOIN tasks t ON t.id = r.task_id\n\
    \WHERE NOT EXISTS\n\
    \  (SELECT 1 FROM messages m WHERE m.id = r.task_id || '-reply')"

migrateRegistry
  :: (MonadIO m, CanFileSystem m)
  => Connection
  -> UTCTime
  -> FilePath
  -> m (Int, Int)
migrateRegistry c fallbackNow path = do
  exists <- doesFileExist path
  if not exists
    then pure (0, 0)
    else do
      res <- readFileBytes path
      case res of
        Left _ -> pure (0, 1)
        Right bs -> case A.eitherDecodeStrict' bs :: Either String A.Value of
          Left _ -> pure (0, 1)
          Right v -> case extractRepos v of
            Nothing -> pure (0, 1)
            Just xs -> do
              counts <- mapM (insertAgent c (formatUtc fallbackNow)) xs
              pure (sum (map fst counts), sum (map snd counts))

-- | Pure extractor: turn the legacy `registry.json` object into a list of
-- (alias, path, registered_at) triples. Rows that fail the shape are
-- dropped.
extractRepos :: A.Value -> Maybe [(Text, Text, Text)]
extractRepos (A.Object o) = case KM.lookup "repos" o of
  Just (A.Object r) -> Just (concatMap extract (KM.toList r))
  _ -> Nothing
  where
    extract (k, A.Object v) = case KM.lookup "path" v of
      Just (A.String p) ->
        [ ( T.pack (AK.toString k)
          , p
          , case KM.lookup "registered_at" v of
              Just (A.String s) -> s
              _ -> ""
          )
        ]
      _ -> []
    extract _ = []
extractRepos _ = Nothing

insertAgent
  :: MonadIO m
  => Connection
  -> Text -- ^ fallback now (already formatted)
  -> (Text, Text, Text)
  -> m (Int, Int)
insertAgent c fallbackNow (alias, path, regAt) = liftIO $ do
  let registered = if T.null regAt then fallbackNow else regAt
  exists <-
    query
      c
      "SELECT 1 FROM agents WHERE alias = ?"
      (Only alias) ::
      IO [Only Int]
  case exists of
    _ : _ -> pure (0, 1)
    [] -> do
      execute
        c
        "INSERT OR IGNORE INTO agents (alias, path, summary, tags, registered_at, updated_at)\n\
        \VALUES (?, ?, NULL, NULL, ?, ?)"
        (alias, path, registered, registered)
      pure (1, 0)

migrateInbox
  :: (MonadIO m, CanFileSystem m)
  => Connection
  -> FilePath
  -> m (Int, Int)
migrateInbox c root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure (0, 0)
    else do
      subs <- listDirectory root
      pairs <-
        mapM
          (\d -> do
            files <- listDirectory (root </> d)
            pure [root </> d </> f | f <- files, ".json" `isSuffixOf` f]
          )
          subs
      let paths = concat pairs
      counts <- mapM (migrateTaskFile c) paths
      pure (sum (map fst counts), sum (map snd counts))

migrateTaskFile
  :: (MonadIO m, CanFileSystem m)
  => Connection
  -> FilePath
  -> m (Int, Int)
migrateTaskFile c path = do
  res <- readFileBytes path
  case res of
    Left _ -> pure (0, 1)
    Right bs -> case A.eitherDecodeStrict' bs :: Either String A.Value of
      Left _ -> pure (0, 1)
      Right v -> case extractTask v of
        Nothing -> pure (0, 1)
        Just row -> liftIO (insertTaskRow c row)

-- | Insert a task row, returning `(inserted, skipped)`. A SQLite error —
-- FK violation, CHECK violation, ... — is treated as a skip rather than
-- aborting the whole migration.
insertTaskRow
  :: Connection
  -> (Text, Text, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)
  -> IO (Int, Int)
insertTaskRow c (tid, from, to, kind, createdAt, desc, expected, url, stat) = do
  r <-
    try
      ( execute
          c
          "INSERT OR IGNORE INTO tasks\n\
          \(id, from_alias, to_alias, kind, url, description, expected,\n\
          \ status, created_at, claimed_at, completed_at)\n\
          \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, NULL)"
          (tid, from, to, kind, url, desc, expected, stat, createdAt)
      ) ::
      IO (Either SQLError ())
  pure (either (const (0, 1)) (const (1, 0)) r)

-- | Pure extractor for a legacy task JSON file. Returns the tuple in the
-- order needed by `insertTaskRow`: (id, from, to, kind, created_at,
-- description, expected, url, status).
--
-- Tolerates the drift observed in the field: the id lives under either
-- `"id"` (newer files) or `"task_id"` (older ones); unknown fields like
-- `"updated_at"` are ignored.
extractTask
  :: A.Value
  -> Maybe (Text, Text, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)
extractTask (A.Object o) = do
  tid <- textAt o "id" `orElse` textAt o "task_id"
  from <- aliasAt o "from"
  to <- aliasAt o "to"
  let stat = textAt o "status"
      createdAt = textAt o "created_at"
      desc = textAt o "description"
      expected = textAt o "expected_result" `orElse` textAt o "expected"
      url = textAt o "url"
      kind = maybe "freetext" id (textAt o "kind")
  pure
    ( tid
    , from
    , to
    , kind
    , maybe "" id createdAt
    , desc
    , expected
    , url
    , maybe "pending" id stat
    )
extractTask _ = Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

textAt :: A.Object -> Text -> Maybe Text
textAt o k = case KM.lookup (AK.fromText k) o of
  Just (A.String s) -> Just s
  _ -> Nothing

-- | `from`/`to` accept either a plain string or {"alias": "..."}.
aliasAt :: A.Object -> Text -> Maybe Text
aliasAt o k = case KM.lookup (AK.fromText k) o of
  Just (A.String s) -> Just s
  Just (A.Object inner) -> case KM.lookup "alias" inner of
    Just (A.String s) -> Just s
    _ -> Nothing
  _ -> Nothing

migrateResults
  :: (MonadIO m, CanFileSystem m)
  => Connection
  -> FilePath
  -> m (Int, Int)
migrateResults c root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure (0, 0)
    else do
      files <- listDirectory root
      counts <- mapM (migrateResultFile c . (root </>)) (filter (".json" `isSuffixOf`) files)
      pure (sum (map fst counts), sum (map snd counts))

migrateResultFile
  :: (MonadIO m, CanFileSystem m)
  => Connection
  -> FilePath
  -> m (Int, Int)
migrateResultFile c path = do
  res <- readFileBytes path
  case res of
    Left _ -> pure (0, 1)
    Right bs -> case A.eitherDecodeStrict' bs :: Either String A.Value of
      Left _ -> pure (0, 1)
      Right v -> case extractResult v of
        Nothing -> pure (0, 1)
        Just (tid, stat, summary, arts, doneAt) -> liftIO $ do
          r <-
            try
              ( execute
                  c
                  "INSERT OR IGNORE INTO results (task_id, status, summary, artifacts, completed_at)\n\
                  \VALUES (?, ?, ?, ?, ?)"
                  (tid, stat, summary, jsonToText arts, doneAt)
              ) ::
              IO (Either SQLError ())
          pure (either (const (0, 1)) (const (1, 0)) r)

-- | Pure extractor for a legacy result JSON file.
extractResult
  :: A.Value
  -> Maybe (Text, Text, Maybe Text, A.Value, Text)
extractResult (A.Object o) = do
  tid <- textAt o "task_id" `orElse` textAt o "id"
  let stat = maybe "completed" id (textAt o "status")
      summary = textAt o "summary"
      arts = maybe (A.Array mempty) id (KM.lookup "artifacts" o)
      doneAt = maybe "" id (textAt o "completed_at")
  pure (tid, stat, summary, arts, doneAt)
extractResult _ = Nothing
