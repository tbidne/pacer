module Main (main) where

import Test.Tasty (defaultMain)
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Unit.Pacer.Class.Parser qualified
import Unit.Pacer.Command.Chart qualified
import Unit.Pacer.Command.Chart.Data.ChartRequest qualified
import Unit.Pacer.Command.Chart.Data.Expr qualified
import Unit.Pacer.Command.Chart.Data.Run qualified
import Unit.Pacer.Command.Chart.Data.RunLabel qualified
import Unit.Pacer.Command.Chart.Data.Time.Moment qualified
import Unit.Pacer.Command.Chart.Data.Time.Timestamp qualified
import Unit.Pacer.Command.Chart.Params qualified
import Unit.Pacer.Command.Derive qualified
import Unit.Pacer.Data.Distance qualified
import Unit.Pacer.Data.Distance.Units qualified
import Unit.Pacer.Data.Duration qualified
import Unit.Pacer.Data.Pace qualified
import Unit.Pacer.Utils qualified
import Unit.Prelude

main :: IO ()
main =
  defaultMain
    $ localOption OnPass
    $ testGroup
      "Unit"
      [ Unit.Pacer.Class.Parser.tests,
        Unit.Pacer.Command.Chart.tests,
        Unit.Pacer.Command.Chart.Data.ChartRequest.tests,
        Unit.Pacer.Command.Chart.Data.Expr.tests,
        Unit.Pacer.Command.Chart.Data.Run.tests,
        Unit.Pacer.Command.Chart.Data.RunLabel.tests,
        Unit.Pacer.Command.Chart.Data.Time.Moment.tests,
        Unit.Pacer.Command.Chart.Data.Time.Timestamp.tests,
        Unit.Pacer.Command.Chart.Params.tests,
        Unit.Pacer.Command.Derive.tests,
        Unit.Pacer.Data.Distance.tests,
        Unit.Pacer.Data.Distance.Units.tests,
        Unit.Pacer.Data.Duration.tests,
        Unit.Pacer.Data.Pace.tests,
        Unit.Pacer.Utils.tests
      ]
