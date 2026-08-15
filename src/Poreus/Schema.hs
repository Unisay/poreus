module Poreus.Schema
  ( schemaStatements
  ) where

import Database.SQLite.Simple (Query)

-- | DDL statements executed on every `migrate`. Every statement uses
-- `IF NOT EXISTS` so re-running is a no-op. There is no
-- `schema_version` table — v0.3 is a clean slate (ADR-0009 posture,
-- re-adopted by ADR-0012); versioning will return when the first real
-- migration is needed.
--
-- Tables: sessions, cursors, names, endpoints, messages, host_sessions.
--
-- Ordering/cursor key is `messages.seq` (AUTOINCREMENT), not the
-- timestamp — this kills the v0.2 lexicographic-precision hazard
-- (ADR-0012). `created_at` stays for display, `since` filters, and
-- retention.
schemaStatements :: [Query]
schemaStatements =
  [ "CREATE TABLE IF NOT EXISTS sessions (\n\
    \  address        TEXT PRIMARY KEY,\n\
    \  workspace      TEXT NOT NULL,\n\
    \  pid            INTEGER,\n\
    \  boot_id        TEXT,\n\
    \  first_seen_at  TEXT NOT NULL,\n\
    \  heartbeat_at   TEXT NOT NULL,\n\
    \  ended_at       TEXT\n\
    \)"
  , "CREATE TABLE IF NOT EXISTS cursors (\n\
    \  session_address TEXT PRIMARY KEY\n\
    \    REFERENCES sessions(address) ON DELETE CASCADE,\n\
    \  last_seq        INTEGER NOT NULL DEFAULT 0\n\
    \)"
  , "CREATE TABLE IF NOT EXISTS names (\n\
    \  name               TEXT PRIMARY KEY,\n\
    \  summary            TEXT,\n\
    \  tags               TEXT,\n\
    \  bound_session      TEXT REFERENCES sessions(address) ON DELETE SET NULL,\n\
    \  bound_at           TEXT,\n\
    \  created_at         TEXT NOT NULL,\n\
    \  profile_updated_at TEXT\n\
    \)"
  , "CREATE TABLE IF NOT EXISTS endpoints (\n\
    \  name        TEXT NOT NULL REFERENCES names(name) ON DELETE CASCADE,\n\
    \  verb        TEXT NOT NULL,\n\
    \  description TEXT NOT NULL,\n\
    \  autonomy    TEXT NOT NULL CHECK (autonomy IN ('auto','confirm')),\n\
    \  usage_hint  TEXT,\n\
    \  PRIMARY KEY (name, verb)\n\
    \)"
  , "CREATE TABLE IF NOT EXISTS messages (\n\
    \  seq          INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  id           TEXT NOT NULL UNIQUE,\n\
    \  from_address TEXT NOT NULL,\n\
    \  to_address   TEXT NOT NULL,\n\
    \  from_name    TEXT,\n\
    \  to_name      TEXT,\n\
    \  kind         TEXT NOT NULL CHECK (kind IN ('request','notice')),\n\
    \  in_reply_to  TEXT,\n\
    \  payload      TEXT NOT NULL,\n\
    \  created_at   TEXT NOT NULL\n\
    \)"
  , "CREATE INDEX IF NOT EXISTS idx_messages_to_seq\n\
    \  ON messages (to_address, seq)"
  , "CREATE INDEX IF NOT EXISTS idx_messages_from_seq\n\
    \  ON messages (from_address, seq)"
  , "CREATE INDEX IF NOT EXISTS idx_messages_in_reply_to\n\
    \  ON messages (in_reply_to)"
  , "CREATE INDEX IF NOT EXISTS idx_messages_to_name\n\
    \  ON messages (to_name)"
  , "CREATE INDEX IF NOT EXISTS idx_messages_created_at\n\
    \  ON messages (created_at)"
  , "CREATE TABLE IF NOT EXISTS host_sessions (\n\
    \  host_pid   INTEGER NOT NULL,\n\
    \  boot_id    TEXT NOT NULL,\n\
    \  proc_start INTEGER NOT NULL DEFAULT 0,\n\
    \  session_id TEXT NOT NULL,\n\
    \  workspace  TEXT,\n\
    \  updated_at TEXT NOT NULL,\n\
    \  PRIMARY KEY (host_pid, boot_id, proc_start)\n\
    \)"
  ]
