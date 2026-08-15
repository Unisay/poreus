module Poreus.DB
  ( withDB
  , withConnection'
  , migrate
  , withImmediateTransaction
  ) where

import Control.Exception (Handler (..), catches, onException, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Database.SQLite.Simple
  ( Connection
  , Only (..)
  , SQLError
  , execute_
  , query_
  , withConnection
  )

import Poreus.Config (dbPath, ensureHome)
import qualified Poreus.Schema as Schema
import Poreus.Types (ErrorCode (..), PoreusException (..), mkError)

-- | Open the user's $POREUS_HOME/db.sqlite, enable FKs, apply the
-- schema (implicit bootstrap, REG-1: no operation ever fails with "not
-- initialized"), run the block. Narrow exceptions (SQLError, IOError)
-- become a `storage-failure` domain error carried by
-- `PoreusException`; the caller (tool dispatcher, hook, admin) decides
-- how to render it. Everything else propagates.
withDB :: (Connection -> IO a) -> IO a
withDB k = do
  _ <- ensureHome
  path <- dbPath
  withConnection' path (\c -> migrate c >> k c)

-- | Open a specific DB path (or ":memory:"), set pragmas, run the block.
-- Useful for tests that want an isolated in-memory connection.
withConnection' :: FilePath -> (Connection -> IO a) -> IO a
withConnection' path k =
  withConnection
    path
    ( \c -> do
        -- busy_timeout FIRST: many concurrent server instances (one
        -- per Claude session) open and write simultaneously. Every
        -- later pragma and statement — including journal_mode below —
        -- can hit a locked database; without the timeout already in
        -- place it fails immediately with ErrorBusy instead of
        -- waiting. 10s is plenty for any write transaction to finish.
        execute_ c "PRAGMA busy_timeout = 10000"
        execute_ c "PRAGMA foreign_keys = ON"
        execute_ c "PRAGMA journal_mode = WAL"
        k c
    )
    `catches` [ Handler (\e -> dbError (e :: SQLError))
              , Handler (\e -> dbError (e :: IOError))
              ]
  where
    dbError :: Show e => e -> IO a
    dbError e =
      throwIO (PoreusException (mkError StorageFailure (T.pack (show e))))

-- | Apply schema DDL. Idempotent: every DDL statement uses
-- `IF NOT EXISTS`, so implicit bootstrap (REG-1) costs near zero on
-- repeated calls.
--
-- One exception to the pure-IF-NOT-EXISTS posture: `host_sessions` is
-- a disposable identity cache (worst case a session re-seeds its id),
-- so when its shape changes across versions the old table is simply
-- dropped and recreated instead of migrated.
migrate :: MonadIO m => Connection -> m ()
migrate c = liftIO $ do
  stale <-
    query_
      c
      "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'host_sessions'\n\
      \AND NOT EXISTS (SELECT 1 FROM pragma_table_info('host_sessions') WHERE name = 'proc_start')" ::
      IO [Only Int]
  case stale of
    (Only n : _) | n > 0 -> execute_ c "DROP TABLE host_sessions"
    _ -> pure ()
  mapM_ (execute_ c) Schema.schemaStatements

-- | Run an action inside BEGIN IMMEDIATE … COMMIT. Used for every
-- read-modify-write sequence that must not interleave with another
-- process (cursor advance, name claim). IMMEDIATE takes the write
-- lock up front so the reads inside see a stable snapshot that the
-- subsequent writes are consistent with.
withImmediateTransaction :: Connection -> IO a -> IO a
withImmediateTransaction c action = do
  execute_ c "BEGIN IMMEDIATE"
  r <- action `onException` execute_ c "ROLLBACK"
  execute_ c "COMMIT"
  pure r
