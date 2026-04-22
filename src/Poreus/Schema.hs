module Poreus.Schema
  ( currentVersion
  , schemaStatements
  ) where

import Database.SQLite.Simple (Query)

currentVersion :: Int
currentVersion = 2

-- | DDL statements executed on every `migrate`. Every statement uses
-- `IF NOT EXISTS` so re-running is a no-op and upgrades (v1 → v2)
-- amount to creating the tables added by v2.
--
-- Schema v1 tables: schema_version, agents, endpoints, tasks, results.
-- Schema v2 additions:
--   * messages          — unified request/reply transport row
--   * watch_cursors     — per-alias last-seen message cursor
schemaStatements :: [Query]
schemaStatements =
  [ "CREATE TABLE IF NOT EXISTS schema_version (\n\
    \  version INTEGER NOT NULL\n\
    \)"
  , "CREATE TABLE IF NOT EXISTS agents (\n\
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
  , "CREATE TABLE IF NOT EXISTS tasks (\n\
    \  id            TEXT PRIMARY KEY,\n\
    \  from_alias    TEXT NOT NULL,\n\
    \  to_alias      TEXT NOT NULL,\n\
    \  kind          TEXT NOT NULL CHECK (kind IN ('freetext','rpc')),\n\
    \  url           TEXT,\n\
    \  description   TEXT,\n\
    \  expected      TEXT,\n\
    \  status        TEXT NOT NULL,\n\
    \  created_at    TEXT NOT NULL,\n\
    \  claimed_at    TEXT,\n\
    \  completed_at  TEXT\n\
    \)"
  , "CREATE TABLE IF NOT EXISTS results (\n\
    \  task_id       TEXT PRIMARY KEY REFERENCES tasks(id) ON DELETE CASCADE,\n\
    \  status        TEXT NOT NULL,\n\
    \  summary       TEXT,\n\
    \  artifacts     TEXT,\n\
    \  completed_at  TEXT NOT NULL\n\
    \)"
  , "CREATE INDEX IF NOT EXISTS idx_tasks_to_status ON tasks (to_alias, status)"
  , "CREATE INDEX IF NOT EXISTS idx_tasks_from ON tasks (from_alias)"
  , -- v2: messages is the new single source of truth for the transport.
    -- tasks + results stay as projections for backward-compatible views
    -- (poreus status / inbox --kind request / migrate).
    "CREATE TABLE IF NOT EXISTS messages (\n\
    \  id           TEXT PRIMARY KEY,\n\
    \  from_alias   TEXT NOT NULL,\n\
    \  to_alias     TEXT NOT NULL,\n\
    \  kind         TEXT NOT NULL CHECK (kind IN ('request','reply')),\n\
    \  in_reply_to  TEXT,\n\
    \  payload      TEXT NOT NULL,\n\
    \  created_at   TEXT NOT NULL\n\
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
