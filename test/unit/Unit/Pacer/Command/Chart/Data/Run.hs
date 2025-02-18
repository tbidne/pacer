{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.Run (tests) where

import FileSystem.IO (readBinaryFileIO)
import Pacer.Command.Chart.Data.Run (SomeRuns)
import Pacer.Utils qualified as Utils
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Run"
    [ testParseExampleRunsJson
    ]

testParseExampleRunsJson :: TestTree
testParseExampleRunsJson = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example runs.jsonc",
          testName = [osp|testParseExampleRunsJson|],
          runner = do
            contents <- readBinaryFileIO path
            case Utils.decodeJson @(SomeRuns Double) contents of
              Ok result -> pure $ pShowBS result
              Err err -> throwM err
        }
    path = [ospPathSep|examples/runs.jsonc|]
