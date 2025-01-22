module Unit.Pacer.Command.Derive
  ( -- * Tests
    tests,
  )
where

import Pacer.Command.Derive qualified as Derive
import Pacer.Data.Distance (SomeDistance (MkSomeDistance))
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer),
    SDistanceUnit
      ( SKilometer,
        SMeter,
        SMile
      ),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration (MkDuration), Hours, Minutes, Seconds)
import Pacer.Data.Pace (Pace (MkPace), SomePace (MkSomePace))
import Unit.Pacer.Data.Distance qualified as Unit.Distance
import Unit.Pacer.Data.Duration qualified as Unit.Duration
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Derive"
    [ calculateTests,
      testRelation
    ]

calculateTests :: TestTree
calculateTests =
  testGroup
    "calculate"
    [ testCalculateDistance,
      testCalculateDuration,
      testCalculatePace,
      testPaceTimeInvariance
    ]

testCalculateDistance :: TestTree
testCalculateDistance =
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

          tSec = parseOrDie @(Seconds Double) durationTxt
          tMin = parseOrDie @(Minutes Double) durationTxt
          tHr = parseOrDie @(Hours Double) durationTxt

      let rSec = displaySomeDistance tSec pace
          rMin = displaySomeDistance tMin pace
          rHr = displaySomeDistance tHr pace

      distDispTxt @=? rSec
      distDispTxt @=? rMin
      distDispTxt @=? rHr
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

testCalculateDuration :: TestTree
testCalculateDuration =
  testGroup
    "Expected durations"
    (go <$> quantities)
  where
    go (paceTxt, distTxt, durationTxt) = testCase desc $ do
      let pace = parseOrDie @(SomePace Double) paceTxt
          dist = parseOrDie @(SomeDistance Double) distTxt

          durationDispTxt = display $ parseOrDie @(Seconds Double) durationTxt

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

testCalculatePace :: TestTree
testCalculatePace =
  testGroup
    "Expected paces"
    (go <$> quantities)
  where
    go (paceTxt, distTxt, durationTxt) = testCase desc $ do
      let dist :: SomeDistance PDouble
          dist = parseOrDie distTxt

          paceDispTxt = display $ parseOrDie @(SomePace Double) paceTxt

          tSec = parseOrDie @(Seconds Double) durationTxt
          tMin = parseOrDie @(Minutes Double) durationTxt
          tHr = parseOrDie @(Hours Double) durationTxt

      let rSec = displaySomePace dist tSec
          rMin = displaySomePace dist tMin
          rHr = displaySomePace dist tHr

      paceDispTxt @=? rSec
      paceDispTxt @=? rMin
      paceDispTxt @=? rHr
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

displaySomeDistance :: (SingI t) => Duration t Double -> SomePace PDouble -> Text
displaySomeDistance duration = display . Derive.deriveSomeDistance duration

displaySomeDuration :: SomeDistance Double -> SomePace Double -> Text
displaySomeDuration dist = display . Derive.deriveSomeDuration dist

displaySomePace :: (SingI t) => SomeDistance PDouble -> Duration t Double -> Text
displaySomePace dist = display . Derive.deriveSomePace dist

testPaceTimeInvariance :: TestTree
testPaceTimeInvariance = testPropertyNamed name desc $ property $ do
  distTxt <- forAll Unit.Distance.genSomeDistancePosText
  durationTxt <- forAll Unit.Duration.genDurationPosText

  let dist = parseOrDie @(SomeDistance PDouble) distTxt

  let tSec = parseOrDie @(Seconds Double) durationTxt
      tMin = parseOrDie @(Minutes Double) durationTxt
      tHr = parseOrDie @(Hours Double) durationTxt

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
      Duration t Double ->
      SomeDistance PDouble ->
      Seconds Double
    calcPace duration (MkSomeDistance s d) = case s of
      SMeter ->
        withSingI
          s
          ( Derive.derivePace
              (DistU.convertDistance Kilometer d)
              duration
          ).unPace
      SKilometer ->
        withSingI s (Derive.derivePace d duration).unPace
      SMile ->
        withSingI s (Derive.derivePace d duration).unPace

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

testRelation :: TestTree
testRelation = testPropertyNamed name desc $ property $ do
  dist <- forAll Unit.Distance.genSomeDistancePos
  duration <- forAll Unit.Duration.genSeconds

  let distN = (.unPositive) <$> dist
      durationN = (.unPositive) <$> duration

  let paceDerivedN = Derive.deriveSomePace dist durationN

  paceDerived <- case mkPositive <$> paceDerivedN of
    MkSomePace _ (MkPace (MkDuration Nothing)) -> do
      annotate $ "Derived pace should be non-zero: " ++ show paceDerivedN
      failure
    MkSomePace s (MkPace (MkDuration (Just seconds))) ->
      pure $ MkSomePace s (MkPace (MkDuration seconds))

  let distDerive =
        Derive.deriveSomeDistance durationN paceDerived
      durationDerived = Derive.deriveSomeDuration distN paceDerivedN

  distN === distDerive
  durationN === durationDerived
  where
    name = "testRelation"
    desc = "Tests distance, duration, pace relationships"
