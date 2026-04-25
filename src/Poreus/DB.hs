module Poreus.DB
  ( withDB
  , withConnection'
  , migrate
  ) where

import Control.Exception (Handler (..), catches)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Database.SQLite.Simple
  ( Connection
  , SQLError
  , execute_
  , withConnection
  )

import Poreus.Config (dbPath, ensureHome)
import qualified Poreus.Exit as Exit
import qualified Poreus.Schema as Schema

-- | Open the user's $POREUS_HOME/db.sqlite, enable FKs, run the block.
-- Narrow exceptions (SQLError, IOError) become exit-5; everything else
-- (including `ExitException` thrown by the CLI layer) propagates.
withDB :: (Connection -> IO a) -> IO a
withDB k = do
  _ <- ensureHome
  path <- dbPath
  withConnection' path k

-- | Open a specific DB path (or ":memory:"), set pragmas, run the block.
-- Useful for tests that want an isolated in-memory connection.
withConnection' :: FilePath -> (Connection -> IO a) -> IO a
withConnection' path k =
  withConnection path
    ( \c -> do
        execute_ c "PRAGMA foreign_keys = ON"
        execute_ c "PRAGMA journal_mode = WAL"
        k c
    )
    `catches` [ Handler (\e -> dbError (e :: SQLError))
              , Handler (\e -> dbError (e :: IOError))
              ]
  where
    dbError :: Show e => e -> IO a
    dbError e = Exit.exitJsonError Exit.ExitDB (T.pack (show e))

-- | Apply schema DDL idempotently. The redesign removed `schema_version`
-- — versioning will return when a real migration is needed (see
-- ADR-0009). Callers pay zero overhead on repeated calls because every
-- statement uses `IF NOT EXISTS`.
migrate :: MonadIO m => Connection -> m ()
migrate c = liftIO $ mapM_ (execute_ c) Schema.schemaStatements
