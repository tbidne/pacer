module Main (main) where

import Test.Tasty (defaultMain)
import Unit.Pacer qualified
import Unit.Pacer.Data.Distance qualified
import Unit.Pacer.Data.Distance.Units qualified
import Unit.Pacer.Data.Duration qualified
import Unit.Pacer.Data.Pace qualified
import Unit.Prelude

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Unit"
      [ Unit.Pacer.tests,
        Unit.Pacer.Data.Distance.tests,
        Unit.Pacer.Data.Distance.Units.tests,
        Unit.Pacer.Data.Duration.tests,
        Unit.Pacer.Data.Pace.tests
      ]
