module Functional.Pacer.Command.Scale (tests) where

import Functional.Prelude
import Pacer.Exception (CommandScaleE)

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Scale"
    [ scaleDistanceTests,
      scaleDurationTests,
      scalePaceTests,
      argsErrorTests
    ]

scaleDistanceTests :: TestTree
scaleDistanceTests =
  testGroup
    "Distance"
    [ testScaleDistance,
      testScaleDistanceUnit
    ]

testScaleDistance :: TestTree
testScaleDistance = testCase "Scales distance" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (d, k) = ["scale", "--distance", d, "-k", k]
    vals =
      zip
        [1 ..]
        [ (("10 km", "2"), "20.00 km"),
          (("10 km", "0.5"), "5.00 km")
        ]

testScaleDistanceUnit :: TestTree
testScaleDistanceUnit = testCase "Scales distance with unit" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (d, k, u) = ["scale", "--distance", d, "-k", k, "--unit", u]
    vals =
      zip
        [1 ..]
        [ (("10 km", "2", "km"), "20.00 km"),
          (("10 km", "2", "mi"), "12.43 mi"),
          (("10 mi", "0.5", "kilometers"), "8.04 km"),
          (("10 mi", "0.5", "miles"), "5.00 mi")
        ]

scaleDurationTests :: TestTree
scaleDurationTests =
  testGroup
    "Duration"
    [ testScaleDuration,
      testScaleDurationUnitError
    ]

testScaleDuration :: TestTree
testScaleDuration = testCase "Scales duration" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (d, k) = ["scale", "--duration", d, "-k", k]
    vals =
      zip
        [1 ..]
        [ (("4m15s", "1.1"), "4'40\""),
          (("4m15s", "0.8"), "3'24\"")
        ]

testScaleDurationUnitError :: TestTree
testScaleDurationUnitError = runException @CommandScaleE desc expected args
  where
    desc = "Scale duration error with -u m"
    args = ["scale", "--duration", "5m", "-k", "2", "-u", "km"]
    expected = "--unit is not used when scaling duration."

scalePaceTests :: TestTree
scalePaceTests =
  testGroup
    "Pace"
    [ testScalePace,
      testScalePaceUnit,
      testScalePaceUnitError
    ]

testScalePace :: TestTree
testScalePace = testCase "Scales pace" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (p, k) = ["scale", "--pace", p, "-k", k]
    vals =
      zip
        [1 ..]
        [ (("4m15s", "1.1"), "4'40\""),
          (("4m15s /km", "0.8"), "3'24\" /km")
        ]

testScalePaceUnit :: TestTree
testScalePaceUnit = testCase "Scales distance with unit" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (p, k, u) = ["scale", "--pace", p, "-k", k, "--unit", u]
    vals =
      zip
        [1 ..]
        [ (("4m15s /km", "2", "km"), "8'30\" /km"),
          (("4m15s /km", "2", "mi"), "13'41\" /mi"),
          (("4m15s /mi", "2", "kilometers"), "5'17\" /km"),
          (("4m15s /mi", "2", "miles"), "8'30\" /mi")
        ]

testScalePaceUnitError :: TestTree
testScalePaceUnitError = runException @CommandScaleE desc expected args
  where
    desc = "Scale pace error with no unit and --unit"
    args = ["scale", "--pace", "4m15s", "-k", "2", "-u", "km"]
    expected =
      mconcat
        [ "Scaling pace with --unit requires that the original units are ",
          "given e.g. --pace '4m15s /km'."
        ]

argsErrorTests :: TestTree
argsErrorTests =
  testGroup
    "General args errors"
    [ testScaleError
        "Empty quantity error"
        "Scale requires exactly 1 quantity, received 0."
        [],
      testScaleError
        "Distance and Duration"
        "Scale requires exactly 1 quantity, received 2."
        ["--distance", "5 km", "--duration", "4m30s"],
      testScaleError
        "Distance and Pace"
        "Scale requires exactly 1 quantity, received 2."
        ["--distance", "5 km", "--pace", "4m30s"],
      testScaleError
        "Duration and Pace"
        "Scale requires exactly 1 quantity, received 2."
        ["--duration", "8h", "--pace", "4m30s"],
      testScaleError
        "3 args error"
        "Scale requires exactly 1 quantity, received 3."
        ["--distance", "marathon", "--duration", "3h", "--pace", "4m30s"]
    ]
  where
    testScaleError desc expected args =
      runException @CommandScaleE desc expected args'
      where
        args' = "scale" : "-k" : "2" : args
