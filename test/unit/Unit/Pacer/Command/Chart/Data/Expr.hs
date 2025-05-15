{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.Expr (tests) where

import Pacer.Command.Chart.Data.Expr (FilterType)
import Pacer.Command.Chart.Data.Expr qualified as Expr
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
            { testDesc = "Label eq",
              testName = [osp|testParseExprAtomLabelEq|],
              runner = pure $ parseExpr "label = some label"
            },
          MkGoldenParams
            { testDesc = "Label neq",
              testName = [osp|testParseExprAtomLabelNeq|],
              runner = pure $ parseExpr "label /= some label"
            },
          MkGoldenParams
            { testDesc = "Labels member",
              testName = [osp|testParseExprAtomLabelsMember|],
              runner = pure $ parseExpr "labels ∋ some label"
            },
          MkGoldenParams
            { testDesc = "Labels member fail",
              testName = [osp|testParseExprAtomLabelsMemberFail|],
              runner = pure $ parseExpr "labels ∋ {some label}"
            },
          MkGoldenParams
            { testDesc = "Labels nmember",
              testName = [osp|testParseExprAtomLabelsNmember|],
              runner = pure $ parseExpr "labels ∌ some label"
            },
          MkGoldenParams
            { testDesc = "Labels nmember fail",
              testName = [osp|testParseExprAtomLabelsNmemberFail|],
              runner = pure $ parseExpr "labels ∌ {a}"
            },
          MkGoldenParams
            { testDesc = "Labels eq",
              testName = [osp|testParseExprAtomLabelsEq|],
              runner = pure $ parseExpr "labels = {}"
            },
          MkGoldenParams
            { testDesc = "Labels eq fail",
              testName = [osp|testParseExprAtomLabelsEqFail|],
              runner = pure $ parseExpr "labels = some label"
            },
          MkGoldenParams
            { testDesc = "Labels neq",
              testName = [osp|testParseExprAtomLabelsNeq|],
              runner = pure $ parseExpr "labels /= {a, b, c}"
            },
          MkGoldenParams
            { testDesc = "Labels neq fail",
              testName = [osp|testParseExprAtomLabelsNeqFail|],
              runner = pure $ parseExpr "labels /= lbl"
            },
          MkGoldenParams
            { testDesc = "Labels gt 1",
              testName = [osp|testParseExprAtomLabelsGt1|],
              runner = pure $ parseExpr "labels > {a, b, c}"
            },
          MkGoldenParams
            { testDesc = "Labels gt 2",
              testName = [osp|testParseExprAtomLabelsGt2|],
              runner = pure $ parseExpr "labels ⊋ { a, b }"
            },
          MkGoldenParams
            { testDesc = "Labels gte 1",
              testName = [osp|testParseExprAtomLabelsGte1|],
              runner = pure $ parseExpr "labels >= {a, b, c}"
            },
          MkGoldenParams
            { testDesc = "Labels gte 2",
              testName = [osp|testParseExprAtomLabelsGte2|],
              runner = pure $ parseExpr "labels ⊇ { a, b }"
            },
          MkGoldenParams
            { testDesc = "Labels lt 1",
              testName = [osp|testParseExprAtomLabelsLt1|],
              runner = pure $ parseExpr "labels < {a, b, c}"
            },
          MkGoldenParams
            { testDesc = "Labels lt 2",
              testName = [osp|testParseExprAtomLabelsLt2|],
              runner = pure $ parseExpr "labels ⊊ { a, b }"
            },
          MkGoldenParams
            { testDesc = "Labels lte 1",
              testName = [osp|testParseExprAtomLabelsLte1|],
              runner = pure $ parseExpr "labels <= {a, b, c}"
            },
          MkGoldenParams
            { testDesc = "Labels lte 2",
              testName = [osp|testParseExprAtomLabelsLte2|],
              runner = pure $ parseExpr "labels ⊆ { a, b }"
            },
          MkGoldenParams
            { testDesc = "Pace",
              testName = [osp|testParseExprAtomPace|],
              runner = pure $ parseExpr "pace <= 4m24s /km"
            },
          MkGoldenParams
            { testDesc = "Atom failure",
              testName = [osp|testParseExprAtomUnknown|],
              runner = pure $ parseExpr "unknown <= 4m24s"
            },
          MkGoldenParams
            { testDesc = "Op failure",
              testName = [osp|testParseExprAtomUnknownOp|],
              runner = pure $ parseExpr "duration % 4m24s"
            },
          MkGoldenParams
            { testDesc = "Value failure",
              testName = [osp|testParseExprAtomUnknownValue|],
              runner = pure $ parseExpr "duration = blah"
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
              runner = pure $ parseExpr "label = foo and pace <= 4m24s /km"
            },
          MkGoldenParams
            { testDesc = "Expr2",
              testName = [osp|testParseExpr2|],
              runner = pure $ parseExpr "distance = 20 km or labels ∋ foo bar and pace <= 4m24s /km"
            },
          MkGoldenParams
            { testDesc = "Expr3",
              testName = [osp|testParseExpr3|],
              runner = pure $ parseExpr "(distance = 20 km or labels⊇{foo bar, baz}) and pace <= 4m24s /km)"
            },
          MkGoldenParams
            { testDesc = "Expr Failure 1",
              testName = [osp|testParseExprFailure1|],
              runner = pure $ parseExpr "distance = 20 km or or foo"
            }
        ]

parseExpr :: Text -> ByteString
parseExpr txt = case Expr.lexParse @(FilterType Double) txt of
  Err err -> encodeUtf8 $ packText err
  Ok x -> encodeUtf8 $ display x
