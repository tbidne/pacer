{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.Run (tests) where

import FileSystem.IO (readBinaryFileIO)
import Pacer.Command.Chart.Data.Run (SomeRuns)
import TOML (decode)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Run"
    [ testParseExampleRunsToml
    ]

testParseExampleRunsToml :: TestTree
testParseExampleRunsToml = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example runs.toml",
          testName = [osp|testParseExampleRunsToml|],
          runner = do
            contents <- decodeUtf8ThrowM =<< readBinaryFileIO path
            case decode @(SomeRuns Double) contents of
              Right result -> pure $ pShowBS result
              Left err -> throwM err
        }
    path = [ospPathSep|examples/runs.toml|]
