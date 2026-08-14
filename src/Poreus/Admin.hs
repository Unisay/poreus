module Poreus.Admin
  ( runPurge
  ) where

import Poreus.DB (withDB)
import Poreus.JSON (emitJSON)
import Poreus.Retention (retentionDays, sweep)

-- | `poreus admin purge [--older-than DAYS]` (MAINT-1): the operator's
-- explicit early trim. Emits the sweep counts as pretty JSON.
runPurge :: Maybe Int -> IO ()
runPurge mdays = withDB $ \c -> do
  days <- maybe retentionDays pure mdays
  result <- sweep c days
  emitJSON result
