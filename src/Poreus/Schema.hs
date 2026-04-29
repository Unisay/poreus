module Poreus.Schema
  ( schemaStatements
  , dataMigrationStatements
  ) where

import Database.SQLite.Simple (Query)

-- | DDL statements executed on every `migrate`. Every statement uses
-- `IF NOT EXISTS` so re-running is a no-op. There is no
-- `schema_version` table — the protocol redesign committed a clean
-- slate (see ADR-0009). Versioning will return when the first real
-- migration is needed.
--
-- Tables: agents, endpoints, messages, watch_cursors.
schemaStatements :: [Query]
schemaStatements =
  [ "CREATE TABLE IF NOT EXISTS agents (\n\
    \  alias         TEXT PRIMARY KEY,\n\
    \  path          TEXT NOT NULL,\n\
    \  summary       TEXT,\n\
    \  tags          TEXT,\n\
    \  registered_at TEXT NOT NULL,\n\
    \  updated_at    TEXT NOT NULL\n\
    \)"
  , "CREATE TABLE IF NOT EXISTS endpoints (\n\
    \  agent_alias   TEXT NOT NULL REFERENCES agents(alias) ON DELETE CASCADE,\n\
    \  verb          TEXT NOT NULL,\n\
    \  arg_schema    TEXT,\n\
    \  param_schema  TEXT,\n\
    \  autonomy      TEXT NOT NULL CHECK (autonomy IN ('auto','confirm')),\n\
    \  description   TEXT,\n\
    \  PRIMARY KEY (agent_alias, verb)\n\
    \)"
  , "CREATE TABLE IF NOT EXISTS messages (\n\
    \  id           TEXT PRIMARY KEY,\n\
    \  from_alias   TEXT NOT NULL,\n\
    \  to_alias     TEXT NOT NULL,\n\
    \  kind         TEXT NOT NULL CHECK (kind IN ('request','notice')),\n\
    \  in_reply_to  TEXT,\n\
    \  payload      TEXT NOT NULL,\n\
    \  subscribe    TEXT,\n\
    \  created_at   TEXT NOT NULL,\n\
    \  CHECK (kind = 'request' OR subscribe IS NULL)\n\
    \)"
  , "CREATE INDEX IF NOT EXISTS idx_messages_to_created\n\
    \  ON messages (to_alias, created_at)"
  , "CREATE INDEX IF NOT EXISTS idx_messages_in_reply_to\n\
    \  ON messages (in_reply_to)"
  , "CREATE TABLE IF NOT EXISTS watch_cursors (\n\
    \  alias       TEXT PRIMARY KEY,\n\
    \  last_seen   TEXT NOT NULL\n\
    \)"
  ]

-- | Idempotent UPDATE statements that rewrite legacy second-precision
-- timestamp columns into the millisecond-precision form
-- (`...HH:MM:SSZ` → `...HH:MM:SS.000Z`). Required because the follower
-- compares strings with `>`, and lexicographically `.` (0x2E) sorts
-- before `Z` (0x5A) — so a mixed store would silently break ordering
-- between old and new rows. The `GLOB` predicate matches only the
-- legacy shape; once a row has been rewritten, subsequent runs leave
-- it alone.
dataMigrationStatements :: [Query]
dataMigrationStatements =
  [ "UPDATE messages SET created_at = REPLACE(created_at, 'Z', '.000Z')\n\
    \  WHERE created_at GLOB '????-??-??T??:??:??Z'"
  , "UPDATE agents SET registered_at = REPLACE(registered_at, 'Z', '.000Z')\n\
    \  WHERE registered_at GLOB '????-??-??T??:??:??Z'"
  , "UPDATE agents SET updated_at = REPLACE(updated_at, 'Z', '.000Z')\n\
    \  WHERE updated_at GLOB '????-??-??T??:??:??Z'"
  , "UPDATE watch_cursors SET last_seen = REPLACE(last_seen, 'Z', '.000Z')\n\
    \  WHERE last_seen GLOB '????-??-??T??:??:??Z'"
  ]
