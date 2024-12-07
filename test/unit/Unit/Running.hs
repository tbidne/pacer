module Unit.Running
  ( -- * Tests
    tests,
  )
where

import Running qualified
import Running.Data.Distance
  ( SomeDistance (MkSomeDistance),
    convertDistance,
  )
import Running.Data.Distance.Units
  ( DistanceUnit (Kilometer),
    SDistanceUnit
      ( SKilometer,
        SMeter,
        SMile
      ),
  )
import Running.Data.Duration
  ( Duration,
    TimeUnit (Hour, Minute, Second),
  )
import Unit.Prelude
import Unit.Running.Data.Distance qualified as Unit.Distance
import Unit.Running.Data.Duration qualified as Unit.Duration

tests :: TestTree
tests =
  testGroup
    "Running"
    [ calculatePaceTests
    ]

calculatePaceTests :: TestTree
calculatePaceTests =
  testGroup
    "calculatePace"
    [ testCalculatePace,
      testPaceTimeInvariance
    ]

testCalculatePace :: TestTree
testCalculatePace =
  testGroup
    "Expected paces"
    (go <$> vals)
  where
    vals =
      [ ("4'50\" /km", "42.195 km", "3h23m57s"),
        ("5'00\" /km", "42.195 km", "3h30m59s"),
        ("5'00\" /km", "42195 m", "3h30m59s"),
        ("8'03\" /mi", "26.2188 mi", "3h30m59s"),
        ("5'30\" /km", "42.195 km", "3h52m04s"),
        ("4'30\" /km", "21.0975 km", "1h34m56s"),
        ("4'45\" /km", "21.0975 km", "1h40m13s"),
        ("4'45\" /km", "21097.5 m", "1h40m13s"),
        ("7'39\" /mi", "13.1094 mi", "1h40m13s"),
        ("5'00\" /km", "21.0975 km", "1h45m29s")
      ]

    go (expected, distTxt, durationTxt) = testCase desc $ do
      let dist :: SomeDistance PDouble
          dist = parseOrDie distTxt

          tSec = parseOrDie @(Duration Second PDouble) durationTxt
          tMin = parseOrDie @(Duration Minute PDouble) durationTxt
          tHr = parseOrDie @(Duration Hour PDouble) durationTxt

      let rSec = displaySomePace dist tSec
          rMin = displaySomePace dist tMin
          rHr = displaySomePace dist tHr

      expected @=? rSec
      expected @=? rMin
      expected @=? rHr
      where
        desc =
          unpackText
            $ mconcat
              [ "'",
                distTxt,
                "' in '",
                durationTxt,
                "' -> ",
                expected
              ]

displaySomePace :: (SingI t) => SomeDistance PDouble -> Duration t PDouble -> Text
displaySomePace dist = display . Running.calculateSomePace dist

testPaceTimeInvariance :: TestTree
testPaceTimeInvariance = testPropertyNamed name desc $ property $ do
  distTxt <- forAll Unit.Distance.genSomeDistancePosText
  durationTxt <- forAll Unit.Duration.genDurationPosText

  let dist = parseOrDie @(SomeDistance PDouble) distTxt

  let tSec = parseOrDie @(Duration Second PDouble) durationTxt
      tMin = parseOrDie @(Duration Minute PDouble) durationTxt
      tHr = parseOrDie @(Duration Hour PDouble) durationTxt

  annotateShow tSec
  annotateShow tMin
  annotateShow tHr

  let rSec = calcPace tSec dist
      rMin = calcPace tMin dist
      rHr = calcPace tHr dist

  rSec === rMin
  rMin === rHr
  rSec === rHr
  where
    name = "testPaceTimeInvariance"
    desc = "calculatePace is time-invariant"

    calcPace ::
      (SingI t) =>
      Duration t PDouble ->
      SomeDistance PDouble ->
      Duration Second PDouble
    calcPace duration (MkSomeDistance s d) = case s of
      SMeter ->
        withSingI
          s
          ( Running.calculatePace
              (convertDistance @Kilometer d)
              duration
          ).unPace
      SKilometer ->
        withSingI s (Running.calculatePace d duration).unPace
      SMile ->
        withSingI s (Running.calculatePace d duration).unPace
