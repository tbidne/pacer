module Functional.Convert (tests) where

import Functional.Prelude
import Pacer.Exception (CommandConvertE)

tests :: TestTree
tests =
  testGroup
    "Convert"
    [ convertDistanceTests,
      convertPaceTests,
      argsErrorTests
    ]

convertDistanceTests :: TestTree
convertDistanceTests =
  testGroup
    "Distance"
    [ testConvertDistance
    ]

testConvertDistance :: TestTree
testConvertDistance = testCase "Converts distance" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (d, u) = ["convert", "--distance", d, "-u", u]
    vals =
      zip
        [1 ..]
        [ (("10 km", "km"), "10.00 km"),
          (("10 kilometers", "mi"), "6.22 mi"),
          (("10 mi", "kilometers"), "16.09 km"),
          (("10 miles", "miles"), "10.00 mi")
        ]

convertPaceTests :: TestTree
convertPaceTests =
  testGroup
    "Pace"
    [ testConvertPace,
      testConvertPaceError
    ]

testConvertPace :: TestTree
testConvertPace = testCase "Converts pace" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (p, u) = ["convert", "--pace", p, "-u", u]
    vals =
      zip
        [1 ..]
        [ (("4m15s /km", "km"), "4'15\" /km"),
          (("4m15s /kilometer", "mi"), "6'50\" /mi"),
          (("4m15s /mi", "kilometers"), "2'38\" /km"),
          (("4m15s /mile", "miles"), "4'15\" /mi")
        ]

testConvertPaceError :: TestTree
testConvertPaceError = runException @CommandConvertE desc expected args
  where
    desc = "Convert pace error with -u m"
    args = ["convert", "--pace", "4m30s /km", "-u", "m"]
    expected = "Meters are disallowed in Pace; use km or mi."

argsErrorTests :: TestTree
argsErrorTests =
  testGroup
    "General args errors"
    [ testConvertError
        "Empty quantity error"
        "Convert requires exactly 1 quantity, received 0."
        [],
      testConvertError
        "Distance and Pace"
        "Convert requires exactly 1 quantity, received 2."
        ["--distance", "5 km", "--pace", "4m30s /km"]
    ]
  where
    testConvertError desc expected args =
      runException @CommandConvertE desc expected args'
      where
        args' = "convert" : "-u" : "mi" : args
