{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Chart.Data.Run (tests) where

import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    TimeZone (TimeZone),
    ZonedTime (ZonedTime),
  )
import Pacer.Chart.Data.Run
  ( Run (MkRun),
    RunTimestamp (RunDay, RunLocalTime, RunZonedTime),
    SomeRun (MkSomeRun),
    SomeRuns (MkSomeRuns),
  )
import Pacer.Data.Distance (Distance (MkDistance))
import Pacer.Data.Distance qualified as Dist
import Pacer.Data.Distance.Units (SDistanceUnit (SKilometer, SMile))
import Pacer.Data.Duration (Duration (MkDuration), Seconds)
import TOML (decode)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Chart.Data.Run"
    [ testParseExampleToml
    ]

testParseExampleToml :: TestTree
testParseExampleToml = testProp1 "testParseExampleToml" desc $ do
  contents <- liftIO $ decodeUtf8ThrowM =<< readBinaryFileIO path
  case decode @(SomeRuns Double) contents of
    Right result -> expected === result
    Left err -> do
      annotate $ displayException err
      failure
  where
    desc = "Parses example toml"
    path = [ospPathSep|data/input/example/runs.toml|]

    expected =
      MkSomeRuns
        $ listToNESeq
          [ MkSomeRun
              SKilometer
              $ MkRun
                (RunDay $ ModifiedJulianDay 60_603)
                (mkDist 5)
                (mkDur 1230)
                []
                Nothing,
            MkSomeRun
              SMile
              $ MkRun
                (RunLocalTime $ mkLt 60_603 14 30 0)
                (mkDist 20)
                (mkDur 9654)
                []
                Nothing,
            MkSomeRun
              SKilometer
              $ MkRun
                (RunZonedTime $ ZonedTime (mkLt 60_608 12 0 0) (TimeZone -480 False ""))
                Dist.marathon
                (mkDur 12000)
                ["official", "marathon"]
                (Just "Some Marathon")
          ]

    mkLt d h m s = LocalTime (ModifiedJulianDay d) (TimeOfDay h m s)

    mkDist :: Integer -> Distance d PDouble
    mkDist = MkDistance . fromℤ

    mkDur :: Integer -> Seconds PDouble
    mkDur = MkDuration . fromℤ
