import           Disorder.Core.Main

import qualified Test.Mafia.Cabal.Constraint
import qualified Test.Mafia.Cabal.Dependencies
import qualified Test.Mafia.Cabal.Types
import qualified Test.Mafia.Hoogle
import qualified Test.Mafia.Package
import qualified Test.Mafia.Process

main :: IO ()
main =
  disorderMain [
      Test.Mafia.Cabal.Constraint.tests
    , Test.Mafia.Cabal.Dependencies.tests
    , Test.Mafia.Cabal.Types.tests
    , Test.Mafia.Hoogle.tests
    , Test.Mafia.Package.tests
    , Test.Mafia.Process.tests
    ]
