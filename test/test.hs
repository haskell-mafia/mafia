import           Disorder.Core.Main

import qualified Test.Mafia.Process

main :: IO ()
main =
  disorderMain [
      Test.Mafia.Process.tests
    ]
