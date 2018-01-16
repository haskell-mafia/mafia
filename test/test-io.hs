import           Test.Mafia.Main

import qualified Test.IO.Mafia.Chaos
import qualified Test.IO.Mafia.Flock

main :: IO ()
main =
  disorderMain [
      Test.IO.Mafia.Flock.tests
    , Test.IO.Mafia.Chaos.tests
    ]
