{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.Word (Word8)
import Functional.Prelude
import Pacer.Driver (runAppWith)
import System.Environment (withArgs)
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Functional"
      [ convertTests,
        scaleTests
      ]

convertTests :: TestTree
convertTests =
  testGroup
    "convert"
    [ testConvertToDistance,
      testConvertToDuration,
      testConvertToPace,
      testConvertError
        "Empty args error"
        "Convert requires exactly 2 options, received 0."
        [],
      testConvertError
        "Distance only error"
        singleArgErr
        ["--distance", "marathon"],
      testConvertError
        "Duration only error"
        singleArgErr
        ["--duration", "3h"],
      testConvertError
        "Pace only error"
        singleArgErr
        ["--pace", "4m30s"],
      testConvertError
        "Convert distance requires pace units"
        "Converting duration and pace to distance requires that pace has units."
        ["--duration", "3h", "--pace", "4m30s"],
      testConvertError
        "3 args error"
        "Convert requires exactly 2 options, received 3."
        ["--distance", "marathon", "--duration", "3h", "--pace", "4m30s"]
    ]
  where
    singleArgErr = "Convert requires exactly 2 options, received 1."

    testConvertError :: TestName -> Text -> List String -> TestTree
    testConvertError desc expected args = testCase desc $ do
      eResult <- try @TextException $ withArgs args' $ runAppWith pure

      case eResult of
        Right r -> assertFailure $ unpackText $ "Expected exception, received: " <> r
        Left ex -> expected @=? displayExceptiont ex
      where
        args' = "convert" : args

testConvertToDistance :: TestTree
testConvertToDistance = testCase "Converts to Distance" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (d, p) = ["convert", "--duration", d, "--pace", p]
    vals =
      zip
        [1 ..]
        [ (("3h", "4m30s /km"), "40.00 km"),
          (("2h20s", "4m10s /km"), "28.88 km"),
          (("20m", "4m /kilometer"), "5.00 km"),
          (("2h30m", "5m /mi"), "30.00 mi"),
          (("3h", "5m /mile"), "36.00 mi")
        ]

testConvertToDuration :: TestTree
testConvertToDuration = testCase "Converts to Duration" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (d, p) = ["convert", "--distance", d, "--pace", p]
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

testConvertToPace :: TestTree
testConvertToPace = testCase "Converts to Pace" $ do
  runMultiArgs mkArgs vals
  where
    mkArgs (di, du) = ["convert", "--distance", di, "--duration", du]
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

scaleTests :: TestTree
scaleTests =
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
    testScaleError :: TestName -> Text -> List String -> TestTree
    testScaleError desc expected args = testCase desc $ do
      eResult <- try @TextException $ withArgs args' $ runAppWith pure

      case eResult of
        Right r -> assertFailure $ unpackText $ "Expected exception, received: " <> r
        Left ex -> expected @=? displayExceptiont ex
      where
        -- NOTE: We always supply the scale factor i.e. do not test empty args,
        -- because while we can successfully test it (catch ExitCode), the test
        -- output is polluted.
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

runMultiArgs :: (a -> List String) -> List (Word8, (a, Text)) -> IO ()
runMultiArgs mkArgs vals =
  for_ vals $ \(idx, (a, e)) -> do
    let args = mkArgs a
    runArgs (Just idx) args e

runArgs :: Maybe Word8 -> List String -> Text -> IO ()
runArgs mIdx args expected = do
  result <- withArgs args $ runAppWith pure

  let (idxTxt, indentTxt) = case mIdx of
        Just idx -> (showt idx <> ". ", "   ")
        Nothing -> ("", "")

  let msg =
        mconcat
          [ idxTxt,
            "Args: ",
            showt args,
            "\n",
            indentTxt,
            "Diff: ",
            expected,
            " /= ",
            result
          ]
  assertBool (unpackText msg) (expected == result)
