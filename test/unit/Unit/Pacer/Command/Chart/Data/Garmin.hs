{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.Garmin (tests) where

import Data.IORef qualified as Ref
import Effectful.FileSystem.FileReader.Dynamic
  ( decodeUtf8Lenient,
  )
import Effectful.Logger.Dynamic
  ( Logger (LoggerLog),
    ToLogStr (toLogStr),
    fromLogStr,
  )
import Pacer.Command.Chart.Data.Garmin qualified as Garmin
import Pacer.Command.Chart.Params
  ( RunsType (RunsDefault, RunsGarmin),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Garmin"
    [ getRunsTypeTests
    ]

getRunsTypeTests :: TestTree
getRunsTypeTests =
  testGroup
    "getRunsType"
    [ testCsvGarmin,
      testTomlDefault,
      testNameGarmin,
      testNameActivities,
      testSpecifiedSameGuess,
      testSpecifiedDifferentGuess
    ]

testCsvGarmin :: TestTree
testCsvGarmin = testCase ".csv returns garmin" $ do
  (logs, result) <- runGetRunsType Nothing [osp|file.csv|]
  RunsGarmin @=? result
  [] @=? logs

testTomlDefault :: TestTree
testTomlDefault = testCase ".toml returns default" $ do
  (logs, result) <- runGetRunsType Nothing [osp|file.toml|]
  RunsDefault @=? result
  [] @=? logs

testNameGarmin :: TestTree
testNameGarmin = testCase "name with 'garmin' returns default" $ do
  (logs, result) <- runGetRunsType Nothing [osp|file_garmin|]
  RunsGarmin @=? result
  [] @=? logs

testNameActivities :: TestTree
testNameActivities = testCase "name with 'activities' default" $ do
  (logs, result) <- runGetRunsType Nothing [osp|some_activities_thing|]
  RunsGarmin @=? result
  [] @=? logs

  (logs2, result2) <- runGetRunsType Nothing [osp|some_Activities_thing|]
  RunsGarmin @=? result2
  [] @=? logs2

testSpecifiedSameGuess :: TestTree
testSpecifiedSameGuess = testCase "Specified matches guess" $ do
  (logs, result) <- runGetRunsType (Just RunsDefault) [osp|file.toml|]
  RunsDefault @=? result
  [] @=? logs

  (logs2, result2) <- runGetRunsType (Just RunsGarmin) [osp|file.csv|]
  RunsGarmin @=? result2
  [] @=? logs2

testSpecifiedDifferentGuess :: TestTree
testSpecifiedDifferentGuess = testCase "Specified does not match guess" $ do
  (logs, result) <- runGetRunsType (Just RunsGarmin) [osp|file.toml|]
  RunsGarmin @=? result
  [expected1] @=? logs

  (logs2, result2) <- runGetRunsType (Just RunsDefault) [osp|file.csv|]
  RunsDefault @=? result2
  [expected2] @=? logs2
  where
    expected1 = msg "garmin (csv)" "default (toml)"
    expected2 = msg "default (toml)" "garmin (csv)"

    msg x y =
      mconcat
        [ "Specified runs type '",
          x,
          "', but guessed '",
          y,
          "'; assuming the former."
        ]

runGetRunsType :: Maybe RunsType -> OsPath -> IO (Tuple2 (List Text) RunsType)
runGetRunsType mRunsType mRunsPath = do
  logsRef <- Ref.newIORef []
  result <- runner logsRef $ Garmin.getRunsType mRunsType mRunsPath
  logs <- Ref.readIORef logsRef
  pure (logs, result)
  where
    runner ref =
      runEff
        . runIORef
        . runLoggerNS mempty
        . runLoggerCapture ref

runLoggerCapture ::
  (IORefE :> es) =>
  IORef (List Text) ->
  Eff (Logger : es) a ->
  Eff es a
runLoggerCapture ref = interpret_ $ \case
  LoggerLog _ _ _ msg -> do
    modifyIORef' ref (toText msg :)
    pure ()
    where
      toText = decodeUtf8Lenient . fromLogStr . toLogStr
