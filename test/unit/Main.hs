module Main (main) where

import Test.Tasty (defaultMain)
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Unit.Pacer.Class.Parser qualified
import Unit.Pacer.Command.Chart qualified
import Unit.Pacer.Command.Chart.Data.Activity qualified
import Unit.Pacer.Command.Chart.Data.Activity.ActivityLabel qualified
import Unit.Pacer.Command.Chart.Data.ChartData qualified
import Unit.Pacer.Command.Chart.Data.ChartRequest qualified
import Unit.Pacer.Command.Chart.Data.Expr qualified
import Unit.Pacer.Command.Chart.Data.Garmin qualified
import Unit.Pacer.Command.Chart.Data.Time.Moment qualified
import Unit.Pacer.Command.Chart.Data.Time.Timestamp qualified
import Unit.Pacer.Command.Chart.Params qualified
import Unit.Pacer.Command.Derive qualified
import Unit.Pacer.Data.Distance qualified
import Unit.Pacer.Data.Distance.Units qualified
import Unit.Pacer.Data.Duration qualified
import Unit.Pacer.Data.Pace qualified
import Unit.Pacer.Driver qualified
import Unit.Pacer.Utils qualified
import Unit.Pacer.Utils.FileSearch qualified
import Unit.Pacer.Utils.Show qualified
import Unit.Prelude

main :: IO Unit
main =
  defaultMain
    $ localOption OnPass
    $ testGroup
      "Unit"
      [ Unit.Pacer.Class.Parser.tests,
        Unit.Pacer.Command.Chart.tests,
        Unit.Pacer.Command.Chart.Data.Activity.tests,
        Unit.Pacer.Command.Chart.Data.Activity.ActivityLabel.tests,
        Unit.Pacer.Command.Chart.Data.ChartData.tests,
        Unit.Pacer.Command.Chart.Data.ChartRequest.tests,
        Unit.Pacer.Command.Chart.Data.Expr.tests,
        Unit.Pacer.Command.Chart.Data.Garmin.tests,
        Unit.Pacer.Command.Chart.Data.Time.Moment.tests,
        Unit.Pacer.Command.Chart.Data.Time.Timestamp.tests,
        Unit.Pacer.Command.Chart.Params.tests,
        Unit.Pacer.Command.Derive.tests,
        Unit.Pacer.Data.Distance.tests,
        Unit.Pacer.Data.Distance.Units.tests,
        Unit.Pacer.Data.Duration.tests,
        Unit.Pacer.Data.Pace.tests,
        Unit.Pacer.Driver.tests,
        Unit.Pacer.Utils.tests,
        Unit.Pacer.Utils.FileSearch.tests,
        Unit.Pacer.Utils.Show.tests
      ]
