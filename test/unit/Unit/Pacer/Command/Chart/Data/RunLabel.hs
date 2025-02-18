{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.RunLabel (tests) where

import FileSystem.IO (readBinaryFileIO)
import Pacer.Command.Chart.Data.RunLabel (RunLabels)
import Pacer.Utils qualified as Utils
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.RunLabel"
    [ testParseExampleRunLabelsJson
    ]

testParseExampleRunLabelsJson :: TestTree
testParseExampleRunLabelsJson = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example run-labels.jsonc",
          testName = [osp|testParseExampleRunLabelsJson|],
          runner = do
            contents <- readBinaryFileIO path
            case Utils.decodeJson @RunLabels contents of
              Ok result -> pure $ pShowBS result
              Err err -> throwM err
        }
    path = [ospPathSep|examples/run-labels.jsonc|]
