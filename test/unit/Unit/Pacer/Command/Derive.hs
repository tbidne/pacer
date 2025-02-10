module Unit.Pacer.Command.Derive
  ( -- * Tests
    tests,
  )
where

import Pacer.Command.Derive qualified as Derive
import Pacer.Data.Distance
  ( SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer),
    SDistanceUnit
      ( SKilometer,
        SMeter
      ),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration)
import Pacer.Data.Pace (SomePace)
import Unit.Pacer.Data.Distance qualified as Unit.Distance
import Unit.Pacer.Data.Duration qualified as Unit.Duration
import Unit.Pacer.Data.Pace qualified as Unit.Pace
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Derive"
    [ testDeriveDistance,
      testDeriveDuration,
      testDerivePace,
      testDeriveDistanceInvariance,
      testDeriveDurationInvariance,
      testDerivePaceInvariance
    ]

testDeriveDistance :: TestTree
testDeriveDistance =
  testGroup
    "Expected distances"
    (go <$> quantities)
  where
    go (paceTxt, distTxt, durationTxt) = testCase desc $ do
      let pace :: SomePace Double
          pace = parseOrDie paceTxt

          -- When calculating distance, pace must be given units, and the
          -- only allowed units are kilometers or miles. Hence, while one of
          -- our arguments in quantities may be given in meters (for other
          -- testing), here it must be km. We therefore convert meters to
          -- kilometers.
          distOut = parseOrDie @(SomeDistance Double) distTxt
          distOut' = case distOut of
            MkSomeDistance s dist -> case s of
              SMeter ->
                MkSomeDistance SKilometer
                  $ DistU.convertDistance Kilometer dist
              _ -> MkSomeDistance s dist

          distDispTxt = display distOut'

          tSec = parseOrDie @(Duration Double) durationTxt

      let rSec = displaySomeDistance tSec pace

      distDispTxt @=? rSec
      where
        desc =
          unpackText
            $ mconcat
              [ "'",
                paceTxt,
                "' for '",
                durationTxt,
                "' -> ",
                distTxt
              ]

testDeriveDuration :: TestTree
testDeriveDuration =
  testGroup
    "Expected durations"
    (go <$> quantities)
  where
    go (paceTxt, distTxt, durationTxt) = testCase desc $ do
      let pace = parseOrDie @(SomePace Double) paceTxt
          dist = parseOrDie @(SomeDistance Double) distTxt

          durationDispTxt = display $ parseOrDie @(Duration Double) durationTxt

      let r = displaySomeDuration dist pace

      durationDispTxt @=? r
      where
        desc =
          unpackText
            $ mconcat
              [ "'",
                paceTxt,
                "' for '",
                durationTxt,
                "' -> ",
                distTxt
              ]

testDerivePace :: TestTree
testDerivePace =
  testGroup
    "Expected paces"
    (go <$> quantities)
  where
    go (paceTxt, distTxt, durationTxt) = testCase desc $ do
      let dist :: SomeDistance Double
          dist = parseOrDie distTxt

          paceDispTxt = display $ parseOrDie @(SomePace Double) paceTxt

          tSec = parseOrDie @(Duration Double) durationTxt

      let rSec = displaySomePace dist tSec

      paceDispTxt @=? rSec
      where
        desc =
          unpackText
            $ mconcat
              [ "'",
                distTxt,
                "' in '",
                durationTxt,
                "' -> ",
                paceTxt
              ]

displaySomeDistance :: Duration Double -> SomePace Double -> Text
displaySomeDistance duration = display . Derive.deriveSomeDistance duration

displaySomeDuration :: SomeDistance Double -> SomePace Double -> Text
displaySomeDuration dist = display . Derive.deriveSomeDuration dist

displaySomePace :: SomeDistance Double -> Duration Double -> Text
displaySomePace dist = display . Derive.deriveSomePace dist

-- Pace, Distance, Time for testing calculations. In general, these values
-- are __very__ fragile, in the sense that it is easy for rounding differences
-- to, say, cause a calculatePace value to differ from calculateDistance.
--
-- The inital list (chosen semi-randomly) was massaged into values that
-- happened to work for all 3 calculateX functions (i.e. no rounding
-- differences).
--
-- This is slightly unsatisfactory, but it comes w/ the territory when
-- comparing text versions of floating points. It would be nice if we could
-- come up with a more robust method e.g. parsing the double and doing an
-- epsilon check.
quantities :: List (Tuple3 Text Text Text)
quantities =
  [ ("4m50s /km", "42.20 km", "3h23m58s"),
    ("5m00s /km", "42.20 km", "3h31m00s"),
    ("5m00s /km", "42190 m", "3h30m57s"),
    ("8m03s /mi", "26.21 mi", "3h30m59s"),
    ("5m30s /km", "42.19 km", "3h52m03s"),
    ("4m30s /km", "21.10 km", "1h34m57s"),
    ("4m30s /km", "21100 m", "1h34m57s"),
    ("4m45s /km", "21.10 km", "1h40m14s"),
    ("7m39s /mi", "13.10 mi", "1h40m13s"),
    ("5m00s /km", "21.10 km", "1h45m30s")
  ]

testDeriveDistanceInvariance :: TestTree
testDeriveDistanceInvariance = testPropertyNamed name desc $ property $ do
  -- Setup
  duration <- forAll Unit.Duration.genDuration
  pace <- forAll Unit.Pace.genSomePace

  -- Derive Distance
  let distDerived = Derive.deriveSomeDistance duration pace

  -- Use derived Distance to derive duration and pace
  let durationDerived = Derive.deriveSomeDuration distDerived pace
      paceDerived = Derive.deriveSomePace distDerived duration

  -- Comparisons
  duration === durationDerived
  pace === paceDerived
  where
    name = "testDeriveDistanceInvariance"
    desc = "Tests derived distance invariants"

testDeriveDurationInvariance :: TestTree
testDeriveDurationInvariance = testPropertyNamed name desc $ property $ do
  -- Setup
  dist <- forAll Unit.Distance.genSomeDistance
  pace <- forAll Unit.Pace.genSomePace

  -- Derive Duration
  let durationDerived = Derive.deriveSomeDuration dist pace

  -- Use derived Duration to derive distance and pace
  let distDerived = Derive.deriveSomeDistance durationDerived pace

  let paceDerived = Derive.deriveSomePace distDerived durationDerived

  -- Comparisons
  dist === distDerived
  pace === paceDerived
  where
    name = "testDeriveDurationInvariance"
    desc = "Tests derived distance invariants"

testDerivePaceInvariance :: TestTree
testDerivePaceInvariance = testPropertyNamed name desc $ property $ do
  -- Setup
  dist <- forAll Unit.Distance.genSomeDistance
  duration <- forAll Unit.Duration.genDuration

  -- Derive Pace
  let paceDerived = Derive.deriveSomePace dist duration

  -- Use derived Pace to derive distance and duration
  let distDerived = Derive.deriveSomeDistance duration paceDerived
      durationDerived = Derive.deriveSomeDuration dist paceDerived

  -- Comparisons
  dist === distDerived
  duration === durationDerived
  where
    name = "testDerivePaceInvariance"
    desc = "Tests derived pace invariants"
