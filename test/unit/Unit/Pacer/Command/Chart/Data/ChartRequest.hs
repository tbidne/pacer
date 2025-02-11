{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.ChartRequest (tests) where

import FileSystem.IO (readBinaryFileIO)
import Pacer.Command.Chart.Data.ChartRequest (ChartRequests)
import TOML (decode)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.ChartRequest"
    [ testParseExampleChartRequestsToml
    ]

testParseExampleChartRequestsToml :: TestTree
testParseExampleChartRequestsToml = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example chart-requests.toml",
          testName = [osp|testParseExampleChartRequestsToml|],
          runner = do
            contents <- decodeUtf8ThrowM =<< readBinaryFileIO path
            case decode @(ChartRequests Double) contents of
              Right result -> pure $ pShowBS result
              Left err -> throwM err
        }
    path = [ospPathSep|examples/chart-requests.toml|]
