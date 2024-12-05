module Main (main) where

import Test.Tasty (defaultMain)
import Unit.Prelude
import Unit.Running qualified
import Unit.Running.Data.Distance qualified
import Unit.Running.Data.Distance.Units qualified
import Unit.Running.Data.Duration qualified

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Unit"
      [ Unit.Running.tests,
        Unit.Running.Data.Distance.tests,
        Unit.Running.Data.Distance.Units.tests,
        Unit.Running.Data.Duration.tests
      ]
