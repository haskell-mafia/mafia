import           Disorder.Core.Main

import qualified Test.IO.Mafia.Chaos

main :: IO ()
main =
  disorderMain [
      Test.IO.Mafia.Chaos.tests
    ]
