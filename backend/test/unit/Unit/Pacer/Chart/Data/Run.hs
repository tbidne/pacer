{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Chart.Data.Run (tests) where

import Data.Time.Format (ParseTime)
import Data.Time.Format qualified as Format
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

-- TODO: This (and ChartRequest.hs) should be golden tests because the diff
-- is horrendous.

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
                (RunDay $ unsafeParseDay "2024-10-15")
                (mkDist 5)
                (mkDur 1230)
                []
                Nothing,
            MkSomeRun
              SMile
              $ MkRun
                (RunLocalTime $ unsafeParseLocalTime "2024-10-20T14:30:00")
                (mkDist 20)
                (mkDur 9654)
                ["label1"]
                Nothing,
            MkSomeRun
              SKilometer
              $ MkRun
                (RunZonedTime $ unsafeParseZonedTime "2024-10-25T12:00:00-08:00")
                Dist.marathon
                (mkDur 12000)
                ["official", "marathon"]
                (Just "Some Marathon"),
            MkSomeRun
              SKilometer
              $ MkRun
                (RunZonedTime $ unsafeParseZonedTime "2024-10-28T12:00:00-08:00")
                Dist.marathon
                (mkDur 18900)
                ["official", "marathon"]
                (Just "Another marathon"),
            MkSomeRun
              SKilometer
              $ MkRun
                (RunZonedTime $ unsafeParseZonedTime "2024-10-10T12:00:00-08:00")
                Dist.halfMarathon
                (mkDur 900)
                ["official", "half-marathon"]
                (Just "Some half marathon")
          ]

    mkDist :: Integer -> Distance d PDouble
    mkDist = MkDistance . fromℤ

    mkDur :: Integer -> Seconds PDouble
    mkDur = MkDuration . fromℤ

    unsafeParseDay = unsafeParseTimeFmt @Day "%Y-%m-%d"
    unsafeParseLocalTime = unsafeParseTimeFmt @LocalTime "%Y-%m-%dT%H:%M:%S"
    unsafeParseZonedTime = unsafeParseTimeFmt @ZonedTime "%Y-%m-%dT%H:%M:%S%z"

    unsafeParseTimeFmt :: (ParseTime t) => String -> String -> t
    unsafeParseTimeFmt =
      Format.parseTimeOrError False Format.defaultTimeLocale
