module Test.Mafia.IO (
    testIO
  , withCPUTime
  ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           System.CPUTime (getCPUTime)

testIO :: Testable a => IO a -> Property
testIO = monadicIO . run

-- | Perform an action and return the CPU time it takes, in picoseconds
-- (actual precision varies with implementation).
withCPUTime :: MonadIO m => m a -> m (Integer, a)
withCPUTime a = do
  t1 <- liftIO getCPUTime
  r <- a
  t2 <- liftIO getCPUTime
  return (t2 - t1, r)
