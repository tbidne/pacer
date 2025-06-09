{-# LANGUAGE CPP #-}
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
  ( ActivitiesType (ActivitiesDefault, ActivitiesGarmin),
  )
import Pacer.Utils.Show qualified as Utils.Show
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
  (logs, result) <- runGetActivitiesType [relfilePathSep|file.csv|]
  ActivitiesGarmin @=? result
  [] @=? logs

testJsonDefault :: TestTree
testJsonDefault = testCase ".json returns default" $ do
  (logs, result) <- runGetActivitiesType [relfilePathSep|file.json|]
  ActivitiesDefault @=? result
  [] @=? logs

testNameGarmin :: TestTree
testNameGarmin = testCase "name with 'garmin' returns default" $ do
  (logs, result) <- runGetActivitiesType [relfilePathSep|file_garmin|]
  ActivitiesGarmin @=? result
  [] @=? logs

testNameActivities :: TestTree
testNameActivities = testCase "name with 'activities' default" $ do
  (logs, result) <- runGetActivitiesType [relfilePathSep|some_activities_thing|]
  ActivitiesDefault @=? result
  mkExpected "some_activities_thing" @=? logs

  (logs2, result2) <- runGetActivitiesType [relfilePathSep|some_Activities_thing|]
  ActivitiesDefault @=? result2
  mkExpected "some_Activities_thing" @=? logs2
  where
    mkExpected name =
      [ mconcat
          [ "Unknown file type: '",
            Utils.Show.showtPath root,
            name,
            "'. Guessing custom json format."
          ]
      ]

runGetActivitiesType :: Path Rel File -> IO (Tuple2 (List Text) ActivitiesType)
runGetActivitiesType activitiesFile = do
  let activitiesPath = root <</>> activitiesFile
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

{- ORMOLU_DISABLE -}

root :: Path Abs Dir
root =
#if POSIX
  [absdir|/|]
#else
  [absdir|C:\|]
#endif

{- ORMOLU_ENABLE -}
