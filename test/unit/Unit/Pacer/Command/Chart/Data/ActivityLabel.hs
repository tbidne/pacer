{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.ActivityLabel (tests) where

import FileSystem.IO (readBinaryFileIO)
import Pacer.Command.Chart.Data.ActivityLabel (ActivityLabels)
import Pacer.Utils qualified as Utils
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.ActivityLabel"
    [ testParseExampleActivityLabelsJson
    ]

testParseExampleActivityLabelsJson :: TestTree
testParseExampleActivityLabelsJson = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example activity-labels.jsonc",
          testName = [osp|testParseExampleActivityLabelsJson|],
          runner = do
            contents <- readBinaryFileIO path
            case Utils.decodeJson @ActivityLabels contents of
              Ok result -> pure $ pShowBS result
              Err err -> throwM err
        }
    path = [ospPathSep|examples/activity-labels.jsonc|]
