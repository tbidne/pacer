{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.ChartRequest (tests) where

import FileSystem.IO (readBinaryFileIO)
import Pacer.Command.Chart.Data.ChartRequest (ChartRequests)
import Pacer.Utils qualified as Utils
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.ChartRequest"
    [ testParseExampleChartRequestsJson
    ]

testParseExampleChartRequestsJson :: TestTree
testParseExampleChartRequestsJson = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example chart-requests.jsonc",
          testName = [osp|testParseExampleChartRequestsJson|],
          runner = do
            contents <- readBinaryFileIO path
            case Utils.decodeJson @(ChartRequests Double) contents of
              Ok result -> pure $ pShowBS result
              Err err -> throwM err
        }
    path = [ospPathSep|examples/chart-requests.jsonc|]
