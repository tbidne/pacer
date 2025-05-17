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
import FileSystem.Path qualified as Path
import Pacer.Command.Chart.Data.Garmin qualified as Garmin
import Pacer.Command.Chart.Params
  ( ActivitiesType (ActivitiesDefault, ActivitiesGarmin),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Garmin"
    [ getActivitiesTypeTests
    ]

getActivitiesTypeTests :: TestTree
getActivitiesTypeTests =
  testGroup
    "getActivitiesType"
    [ testCsvGarmin,
      testJsonDefault,
      testNameGarmin,
      testNameActivities
    ]

testCsvGarmin :: TestTree
testCsvGarmin = testCase ".csv returns garmin" $ do
  (logs, result) <- runGetActivitiesType [osp|file.csv|]
  ActivitiesGarmin @=? result
  [] @=? logs

testJsonDefault :: TestTree
testJsonDefault = testCase ".json returns default" $ do
  (logs, result) <- runGetActivitiesType [osp|file.json|]
  ActivitiesDefault @=? result
  [] @=? logs

testNameGarmin :: TestTree
testNameGarmin = testCase "name with 'garmin' returns default" $ do
  (logs, result) <- runGetActivitiesType [osp|file_garmin|]
  ActivitiesGarmin @=? result
  [] @=? logs

testNameActivities :: TestTree
testNameActivities = testCase "name with 'activities' default" $ do
  (logs, result) <- runGetActivitiesType [osp|some_activities_thing|]
  ActivitiesGarmin @=? result
  [] @=? logs

  (logs2, result2) <- runGetActivitiesType [osp|some_Activities_thing|]
  ActivitiesGarmin @=? result2
  [] @=? logs2

runGetActivitiesType :: OsPath -> IO (Tuple2 (List Text) ActivitiesType)
runGetActivitiesType activitiesOsPath = do
  activitiesPath <- Path.parseAbsFile activitiesOsPath
  logsRef <- Ref.newIORef []
  result <- runner logsRef $ Garmin.getActivitiesType activitiesPath
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
