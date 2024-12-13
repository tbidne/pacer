module Functional.Derive (tests) where

import Functional.Prelude

tests :: TestTree
tests =
  testGroup
    "derive"
    [ testDeriveToDistance,
      testDeriveToDuration,
      testDeriveToPace,
      testDeriveError
        "Empty args error"
        "Derive requires exactly 2 options, received 0."
        [],
      testDeriveError
        "Distance only error"
        singleArgErr
        ["--distance", "marathon"],
      testDeriveError
        "Duration only error"
        singleArgErr
        ["--duration", "3h"],
      testDeriveError
        "Pace only error"
        singleArgErr
        ["--pace", "4m30s"],
      testDeriveError
        "Derive distance requires pace units"
        "Deriving distance requires that pace has units."
        ["--duration", "3h", "--pace", "4m30s"],
      testDeriveError
        "3 args error"
        "Derive requires exactly 2 options, received 3."
        ["--distance", "marathon", "--duration", "3h", "--pace", "4m30s"]
    ]
  where
    singleArgErr = "Derive requires exactly 2 options, received 1."

    testDeriveError desc expected args =
      runException @TextException desc expected args'
      where
        args' = "derive" : args

testDeriveToDistance :: TestTree
testDeriveToDistance = testCase "Derives Distance" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (d, p) = ["derive", "--duration", d, "--pace", p]
    vals =
      zip
        [1 ..]
        [ (("3h", "4m30s /km"), "40.00 km"),
          (("2h20s", "4m10s /km"), "28.88 km"),
          (("20m", "4m /kilometer"), "5.00 km"),
          (("2h30m", "5m /mi"), "30.00 mi"),
          (("3h", "5m /mile"), "36.00 mi")
        ]

testDeriveToDuration :: TestTree
testDeriveToDuration = testCase "Derives Duration" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (d, p) = ["derive", "--distance", d, "--pace", p]
    vals =
      zip
        [1 ..]
        [ (("42 km", "4m30s"), "3h 9'00\""),
          (("42 kilometers", "4m30s /km"), "3h 9'00\""),
          (("42 km", "4m30s /mi"), "1h 57'28\""),
          (("10 mi", "4m30s"), "45'00\""),
          (("10 miles", "4m30s /km"), "1h 12'24\""),
          (("10 mi", "4m30s /mi"), "45'00\""),
          (("marathon", "5m"), "3h 30'58\""),
          (("marathon", "5m /km"), "3h 30'58\""),
          (("marathon", "5m /mi"), "2h 11'07\""),
          (("half-marathon", "5m"), "1h 45'29\""),
          (("half-marathon", "5m /km"), "1h 45'29\""),
          (("half-marathon", "5m /mi"), "1h 5'34\"")
        ]

testDeriveToPace :: TestTree
testDeriveToPace = testCase "Derives Pace" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (di, du) = ["derive", "--distance", di, "--duration", du]
    vals =
      zip
        [1 ..]
        [ (("42 m", "3h30s"), "71h 37'37\" /km"),
          (("42 km", "3h30s"), "4'18\" /km"),
          (("42 mi", "3h30s"), "4'18\" /mi"),
          (("10 meters", "4h"), "400h 0'00\" /km"),
          (("10 kilometers", "4h"), "24'00\" /km"),
          (("10 miles", "4h"), "24'00\" /mi"),
          (("marathon", "4h20s"), "5'42\" /km"),
          (("half-marathon", "3h30s"), "8'33\" /km")
        ]
