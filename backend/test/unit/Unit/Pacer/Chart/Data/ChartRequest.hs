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
    [ testParseExampleToml
    ]

testParseExampleToml :: TestTree
testParseExampleToml = testProp1 "testParseExampleToml" desc $ do
  contents <- liftIO $ decodeUtf8ThrowM =<< readBinaryFileIO path
  case decode @ChartRequests contents of
    Right result -> expected === result
    Left err -> do
      annotate $ displayException err
      failure
  where
    desc = "Parses example toml"
    path = [ospPathSep|data/input/example/chart-requests.toml|]

    expected =
      MkChartRequests
        [ MkChartRequest
            { filters = [],
              title = "Runs by distance",
              yAxis = YAxisDistance,
              yAxis1 = Nothing
            },
          MkChartRequest
            { filters = [Atom $ FilterLabel "label1"],
              title = "Runs by duration",
              yAxis = YAxisDuration,
              yAxis1 = Just YAxisDistance
            },
          MkChartRequest
            { filters =
                [ Atom $ FilterLabel "label1",
                  Or
                    (Atom $ FilterLabel "label2")
                    (Not $ Atom $ FilterLabel "label3")
                ],
              title = "Runs by pace",
              yAxis = YAxisPace,
              yAxis1 = Nothing
            }
        ]
