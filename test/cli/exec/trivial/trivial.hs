-- Trivial program that will not compile unless the `async` package is installed.
-- `async` was chosen because its not installed as part of GHC *and* because
-- it has very few dependencies other than base packages.
import Control.Concurrent.Async

main :: IO ()
main = print =<< wait =<< async (return (3 :: Int))
