{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Chart.Data.ChartRequest (tests) where

import Pacer.Chart.Data.ChartRequest
import TOML (decode)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Chart.Data.ChartRequest"
    [ testParseExampleChartRequestsToml
    ]

testParseExampleChartRequestsToml :: TestTree
testParseExampleChartRequestsToml = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example runs.toml",
          testName = [osp|testParseExampleChartRequestsToml|],
          runner = do
            contents <- decodeUtf8ThrowM =<< readBinaryFile path
            case decode @(ChartRequests Double) contents of
              Right result -> pure $ pShowBS result
              Left err -> throwM err
        }
    path = [ospPathSep|examples/chart-requests.toml|]
