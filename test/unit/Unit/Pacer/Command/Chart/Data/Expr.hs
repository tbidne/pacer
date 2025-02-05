{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.Expr (tests) where

import Pacer.Command.Chart.Data.Expr qualified as Expr
import Pacer.Data.Result (Result (Err, Ok))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Expr"
    [ parseTests
    ]

parseTests :: TestTree
parseTests =
  testGroup
    "Parsing"
    [ atomTests,
      exprTests
    ]

atomTests :: TestTree
atomTests =
  testGroup
    "Atoms"
    $ testGoldenParams
    <$> [ MkGoldenParams
            { testDesc = "Datetime",
              testName = [osp|testParseExprAtomDatetime|],
              runner = pure $ parseExpr "datetime = 2024"
            },
          MkGoldenParams
            { testDesc = "Distance",
              testName = [osp|testParseExprAtomDistance|],
              runner = pure $ parseExpr "distance >= 4 km"
            },
          MkGoldenParams
            { testDesc = "Duration",
              testName = [osp|testParseExprAtomDuration|],
              runner = pure $ parseExpr "duration /= 3h20m"
            },
          MkGoldenParams
            { testDesc = "Label",
              testName = [osp|testParseExprAtomLabel|],
              runner = pure $ parseExpr "label some label"
            },
          MkGoldenParams
            { testDesc = "Pace",
              testName = [osp|testParseExprAtomPace|],
              runner = pure $ parseExpr "pace <= 4m24s /km"
            }
        ]

exprTests :: TestTree
exprTests =
  testGroup
    "Expressions"
    $ testGoldenParams
    <$> [ MkGoldenParams
            { testDesc = "Expr1",
              testName = [osp|testParseExpr1|],
              runner = pure $ parseExpr "label foo and pace <= 4m24s /km"
            },
          MkGoldenParams
            { testDesc = "Expr2",
              testName = [osp|testParseExpr2|],
              runner = pure $ parseExpr "distance = 20 km or label foo bar and pace <= 4m24s /km"
            },
          MkGoldenParams
            { testDesc = "Expr3",
              testName = [osp|testParseExpr3|],
              runner = pure $ parseExpr "(distance = 20 km or label foo bar) and pace <= 4m24s /km)"
            }
        ]

parseExpr :: Text -> ByteString
parseExpr txt = case Expr.lexParse @Double txt of
  Err err -> encodeUtf8 $ packText err
  Ok x -> encodeUtf8 $ display x
