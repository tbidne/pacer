module Unit.Pacer.Command.Derive
  ( -- * Tests
    tests,
  )
where

import Pacer.Command.Derive qualified as Derive
import Pacer.Data.Distance
  ( Distance (MkDistance),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer),
    SDistanceUnit
      ( SKilometer,
        SMeter
      ),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Data.Pace (Pace (MkPace), SomePace (MkSomePace))
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
      let pace :: SomePace PDouble
          pace = parseOrDie paceTxt

          -- When calculating distance, pace must be given units, and the
          -- only allowed units are kilometers or miles. Hence, while one of
          -- our arguments in quantities may be given in meters (for other
          -- testing), here it must be km. We therefore convert meters to
          -- kilometers.
          distOut = parseOrDie @(SomeDistance PDouble) distTxt
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
      let dist :: SomeDistance PDouble
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

displaySomeDistance :: Duration Double -> SomePace PDouble -> Text
displaySomeDistance duration = display . Derive.deriveSomeDistance duration

displaySomeDuration :: SomeDistance Double -> SomePace Double -> Text
displaySomeDuration dist = display . Derive.deriveSomeDuration dist

displaySomePace :: SomeDistance PDouble -> Duration Double -> Text
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
  durationPos <- forAll Unit.Duration.genSecondsPos
  pacePos <- forAll Unit.Pace.genSomePacePos

  let durationN = (.unPositive) <$> durationPos
      paceN = (.unPositive) <$> pacePos

  -- Derive Distance
  let distDerivedN = Derive.deriveSomeDistance durationN pacePos
  distDerivedPos <- case mkPositive <$> distDerivedN of
    MkSomeDistance _ (MkDistance Nothing) -> do
      annotate $ "Derived distance should be non-zero: " ++ show distDerivedN
      failure
    MkSomeDistance s (MkDistance (Just d)) ->
      pure $ MkSomeDistance s (MkDistance d)

  -- Use derived Distance to derive duration and pace
  let durationDerivedN = Derive.deriveSomeDuration distDerivedN paceN
      paceDerivedN = Derive.deriveSomePace distDerivedPos durationN

  -- Comparisons
  durationN === durationDerivedN
  paceN === paceDerivedN
  where
    name = "testDeriveDistanceInvariance"
    desc = "Tests derived distance invariants"

testDeriveDurationInvariance :: TestTree
testDeriveDurationInvariance = testPropertyNamed name desc $ property $ do
  -- Setup
  distPos <- forAll Unit.Distance.genSomeDistancePos
  pacePos <- forAll Unit.Pace.genSomePacePos

  let distN = (.unPositive) <$> distPos
      paceN = (.unPositive) <$> pacePos

  -- Derive Duration
  let durationDerivedN = Derive.deriveSomeDuration distN paceN

  -- Use derived Duration to derive distance and pace
  let distDerivedN = Derive.deriveSomeDistance durationDerivedN pacePos
  distDerivedPos <- case mkPositive <$> distDerivedN of
    MkSomeDistance _ (MkDistance Nothing) -> do
      annotate $ "Derived distance should be non-zero: " ++ show distDerivedN
      failure
    MkSomeDistance s (MkDistance (Just d)) ->
      pure $ MkSomeDistance s (MkDistance d)

  let paceDerivedN = Derive.deriveSomePace distDerivedPos durationDerivedN

  -- Comparisons
  distN === distDerivedN
  paceN === paceDerivedN
  where
    name = "testDeriveDurationInvariance"
    desc = "Tests derived distance invariants"

testDerivePaceInvariance :: TestTree
testDerivePaceInvariance = testPropertyNamed name desc $ property $ do
  -- Setup
  distPos <- forAll Unit.Distance.genSomeDistancePos
  durationPos <- forAll Unit.Duration.genSecondsPos

  let distN = (.unPositive) <$> distPos
      durationN = (.unPositive) <$> durationPos

  -- Derive Pace
  let paceDerivedN = Derive.deriveSomePace distPos durationN

  paceDerivedPos <- case mkPositive <$> paceDerivedN of
    MkSomePace _ (MkPace (MkDuration Nothing)) -> do
      annotate $ "Derived pace should be non-zero: " ++ show paceDerivedN
      failure
    MkSomePace s (MkPace (MkDuration (Just seconds))) ->
      pure $ MkSomePace s (MkPace (MkDuration seconds))

  -- Use derived Pace to derive distance and duration
  let distDeriveN =
        Derive.deriveSomeDistance durationN paceDerivedPos
      durationDerivedN = Derive.deriveSomeDuration distN paceDerivedN

  -- Comparisons
  distN === distDeriveN
  durationN === durationDerivedN
  where
    name = "testDerivePaceInvariance"
    desc = "Tests derived pace invariants"
