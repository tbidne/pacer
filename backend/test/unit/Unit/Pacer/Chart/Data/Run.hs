{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Chart.Data.Run (tests) where

import Pacer.Chart.Data.Run (SomeRuns)
import TOML (decode)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Chart.Data.Run"
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
              Left err -> throwIO err
        }
    path = [ospPathSep|data/input/example/runs.toml|]
