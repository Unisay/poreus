module Poreus
  ( main
  , versionString
  ) where

import Data.Version (showVersion)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Paths_poreus (version)
import qualified Poreus.Admin as Admin
import qualified Poreus.Doctor as Doctor
import qualified Poreus.Hook as Hook
import qualified Poreus.Server as Server

-- | One binary, four entry modes (ADR-0013/0017): `serve` (the MCP
-- server, spawned by the host per session over stdio), `hook`
-- (short-lived hook companion), `doctor` (operator cross-check), and
-- `admin` (operator commands). Argv dispatch is a hand-rolled match —
-- there is no CLI surface left to warrant a parser library.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["serve"] -> Server.runServer
    ["hook"] -> Hook.runHook
    ["doctor"] -> Doctor.runDoctor
    ("admin" : "purge" : rest) -> case rest of
      [] -> Admin.runPurge Nothing
      ["--older-than", d] | Just n <- readMaybe d, n > 0 -> Admin.runPurge (Just n)
      _ -> usage
    ["version"] -> putStrLn versionString
    ["--version"] -> putStrLn versionString
    _ -> usage

versionString :: String
versionString = "poreus " <> showVersion version

usage :: IO ()
usage = do
  hPutStrLn stderr "usage: poreus (serve | hook | doctor | admin purge [--older-than DAYS] | version)"
  exitFailure
