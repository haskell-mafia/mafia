import           Disorder.Core.Main

import qualified Test.IO.Project.IO

main :: IO ()
main =
  disorderMain [
      Test.IO.Project.IO.tests
    ]
