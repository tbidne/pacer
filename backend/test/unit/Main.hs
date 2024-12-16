module Main (main) where

import Test.Tasty (defaultMain, localOption)
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Unit.Pacer.Chart.Data.ChartRequest qualified
import Unit.Pacer.Chart.Data.Run qualified
import Unit.Pacer.Data.Distance qualified
import Unit.Pacer.Data.Distance.Units qualified
import Unit.Pacer.Data.Duration qualified
import Unit.Pacer.Data.Pace qualified
import Unit.Pacer.Derive qualified
import Unit.Prelude

main :: IO ()
main =
  defaultMain
    $ localOption OnPass
    $ testGroup
      "Unit"
      [ Unit.Pacer.Chart.Data.ChartRequest.tests,
        Unit.Pacer.Chart.Data.Run.tests,
        Unit.Pacer.Data.Distance.tests,
        Unit.Pacer.Data.Distance.Units.tests,
        Unit.Pacer.Data.Duration.tests,
        Unit.Pacer.Data.Pace.tests,
        Unit.Pacer.Derive.tests
      ]
