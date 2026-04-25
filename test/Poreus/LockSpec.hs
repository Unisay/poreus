module Poreus.LockSpec (spec) where

import System.Environment (setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Poreus.Lock
import Poreus.Types (Alias (..))

-- | Run an action with `$POREUS_HOME` set to a fresh temp dir and the
-- session token set / unset as requested. Restores the surrounding env
-- afterwards.
withPoreusHome :: Maybe String -> (FilePath -> IO a) -> IO a
withPoreusHome mToken k =
  withSystemTempDirectory "poreus-lock" $ \tmp -> do
    setEnv "POREUS_HOME" tmp
    case mToken of
      Just t -> setEnv "CLAUDE_CODE_SSE_PORT" t
      Nothing -> unsetEnv "CLAUDE_CODE_SSE_PORT"
    r <- k tmp
    unsetEnv "POREUS_HOME"
    unsetEnv "CLAUDE_CODE_SSE_PORT"
    pure r

spec :: Spec
spec = do
  describe "acquireFollowLock / releaseFollowLock" $ do
    it "first acquire succeeds" $
      withPoreusHome (Just "55555") $ \_ -> do
        result <- acquireFollowLock (Alias "alice") False
        case result of
          Acquired h -> releaseFollowLock h
          other -> expectationFailure ("expected Acquired, got " <> show other)

    -- fcntl advisory locks are process-level: a second `setLock` from
    -- the same process always succeeds (the lock is replaced in
    -- place). So the OwnedByMe path can only be exercised across
    -- processes — the production Monitor scenario where two
    -- Claude-Code-spawned subprocesses share `$CLAUDE_CODE_SSE_PORT`.
    -- We don't fork in this test (would require careful child-process
    -- lifecycle handling); the path is covered by manual smoke and
    -- end-to-end verification in Phase 6.

    it "release allows re-acquire" $
      withPoreusHome (Just "55555") $ \_ -> do
        first <- acquireFollowLock (Alias "alice") False
        case first of
          Acquired h -> do
            releaseFollowLock h
            second <- acquireFollowLock (Alias "alice") False
            case second of
              Acquired h2 -> releaseFollowLock h2
              other ->
                expectationFailure
                  ("re-acquire failed: " <> show other)
          other ->
            expectationFailure ("first acquire failed: " <> show other)

    it "different aliases do not collide" $
      withPoreusHome (Just "55555") $ \_ -> do
        first <- acquireFollowLock (Alias "alice") False
        second <- acquireFollowLock (Alias "bob") False
        case (first, second) of
          (Acquired h1, Acquired h2) -> do
            releaseFollowLock h1
            releaseFollowLock h2
          _ ->
            expectationFailure
              ("expected both Acquired, got " <> show first <> ", " <> show second)
