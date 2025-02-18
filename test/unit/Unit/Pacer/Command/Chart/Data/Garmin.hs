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
      testNameActivities
    ]

testCsvGarmin :: TestTree
testCsvGarmin = testCase ".csv returns garmin" $ do
  (logs, result) <- runGetRunsType [osp|file.csv|]
  RunsGarmin @=? result
  [] @=? logs

testTomlDefault :: TestTree
testTomlDefault = testCase ".toml returns default" $ do
  (logs, result) <- runGetRunsType [osp|file.toml|]
  RunsDefault @=? result
  [] @=? logs

testNameGarmin :: TestTree
testNameGarmin = testCase "name with 'garmin' returns default" $ do
  (logs, result) <- runGetRunsType [osp|file_garmin|]
  RunsGarmin @=? result
  [] @=? logs

testNameActivities :: TestTree
testNameActivities = testCase "name with 'activities' default" $ do
  (logs, result) <- runGetRunsType [osp|some_activities_thing|]
  RunsGarmin @=? result
  [] @=? logs

  (logs2, result2) <- runGetRunsType [osp|some_Activities_thing|]
  RunsGarmin @=? result2
  [] @=? logs2

runGetRunsType :: OsPath -> IO (Tuple2 (List Text) RunsType)
runGetRunsType mRunsPath = do
  logsRef <- Ref.newIORef []
  result <- runner logsRef $ Garmin.getRunsType mRunsPath
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
