module Functional.Scale (tests) where

import Functional.Prelude

tests :: TestTree
tests =
  testGroup
    "scale"
    [ testScaleDistance,
      testScaleDuration,
      testScalePace,
      testScaleError
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
      runException @TextException desc expected args'
      where
        args' = "scale" : "-k" : "2" : args

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
