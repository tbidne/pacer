{-# LANGUAGE AllowAmbiguousTypes #-}

module Unit.Running.Data.Distance
  ( -- * Tests
    tests,

    -- * Generators
    genSomeDistance,
    genSomeDistancePos,
    genSomeDistanceText,
    genSomeDistancePosText,
  )
where

import Running.Class.Parser qualified as Parser
import Running.Data.Distance
  ( Distance (MkDistance),
    SomeDistance (MkSomeDistance),
  )
import Running.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Unit.Prelude
import Unit.Running.Data.Distance.Units qualified as Units
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Running.Data.Distance"
    [ parseTests,
      equalityTests,
      displayTests
    ]

parseTests :: TestTree
parseTests =
  testGroup
    "Parsing"
    [ testParseDistanceGenText,
      testParseDisplayRoundtrip,
      testParseDistanceCases,
      testParseSomeDistanceCases,
      testParseDistanceFailureCases
    ]

testParseDistanceGenText :: TestTree
testParseDistanceGenText = testPropertyNamed "testParseDistanceGenText" desc $ property $ do
  d <- forAll genDistanceText
  parseOrDieM_ @(Distance Meter Double) d
  parseOrDieM_ @(Distance Kilometer Double) d
  parseOrDieM_ @(Distance Mile Double) d

  sd <- forAll genSomeDistanceText
  parseOrDieM_ @(SomeDistance Double) sd
  where
    desc = "Parses text to (Some)Distance"

-- NOTE:
--
--   - Injectivity is relative to short/long units, but we already test that
--     in the Units tests, so not much reason to re-do them here.
--
--   - display . parse is __not__ a round trip even on short units, because
--     we currently allow 0 or more whitespace between the number and the
--     units.

testParseDisplayRoundtrip :: TestTree
testParseDisplayRoundtrip = testPropertyNamed name desc $ property $ do
  -- NOTE: Distance is __not__ a round trip because the display includes the
  -- units, but we don't parse units.
  r <- forAll genSomeDistance

  let t = display r
  annotateUnpack t

  r2 <- parseOrDieM @(SomeDistance Double) t
  annotateShow r

  r === r2
  where
    name = "testParseDisplayRoundtrip"
    desc = "parse . display is a round trip"

testParseDistanceCases :: TestTree
testParseDistanceCases = testCase "Parses expected text" $ do
  Right (mkDistanceD @Meter 7) @=? Parser.parse "7"
  Right (mkDistancePD @Meter 7) @=? Parser.parse "7"
  Right (mkDistanceD @Kilometer 4.83) @=? Parser.parse "4.83"
  Right (mkDistancePD @Kilometer 4.83) @=? Parser.parse "4.83"
  Right (mkDistanceD @Mile 452.5301) @=? Parser.parse "452.5301"
  Right (mkDistancePD @Mile 452.5301) @=? Parser.parse "452.5301"

  Right (mkDistanceD @Meter 0) @=? Parser.parse "0"
  assertLeft "PDouble zero" $ Parser.parse @(Distance Meter PDouble) "0"
  Right (mkDistanceD @Meter 0) @=? Parser.parse "0"
  assertLeft "PDouble zero" $ Parser.parse @(Distance Kilometer PDouble) "0"
  Right (mkDistanceD @Meter 0) @=? Parser.parse "0"
  assertLeft "PDouble zero" $ Parser.parse @(Distance Mile PDouble) "0"

testParseSomeDistanceCases :: TestTree
testParseSomeDistanceCases = testCase "Parses expected text" $ do
  Right (mkSomeDistanceD SMeter 7) @=? Parser.parse "7 m"
  Right (mkSomeDistanceD SMeter 7) @=? Parser.parse "7 meters"
  Right (mkSomeDistancePD SMeter 7) @=? Parser.parse "7 m"
  Right (mkSomeDistancePD SMeter 7) @=? Parser.parse "7 meters"

  Right (mkSomeDistanceD SKilometer 4.83) @=? Parser.parse "4.83 km"
  Right (mkSomeDistanceD SKilometer 4.83) @=? Parser.parse "4.83 kilometers"
  Right (mkSomeDistancePD SKilometer 4.83) @=? Parser.parse "4.83 km"
  Right (mkSomeDistancePD SKilometer 4.83) @=? Parser.parse "4.83 kilometers"

  Right (mkSomeDistanceD SMile 452.5301) @=? Parser.parse "452.5301 mi"
  Right (mkSomeDistanceD SMile 452.5301) @=? Parser.parse "452.5301 miles"
  Right (mkSomeDistancePD SMile 452.5301) @=? Parser.parse "452.5301 mi"
  Right (mkSomeDistancePD SMile 452.5301) @=? Parser.parse "452.5301 miles"

  Right (mkSomeDistanceD SMeter 0) @=? Parser.parse "0 m"
  Right (mkSomeDistanceD SMeter 0) @=? Parser.parse "0 meters"
  assertLeft "PDouble zero" $ Parser.parse @(SomeDistance PDouble) "0 m"
  assertLeft "PDouble zero" $ Parser.parse @(SomeDistance PDouble) "0 meters"

  Right (mkSomeDistanceD SMeter 0) @=? Parser.parse "0 km"
  Right (mkSomeDistanceD SMeter 0) @=? Parser.parse "0 kilometers"
  assertLeft "PDouble zero" $ Parser.parse @(SomeDistance PDouble) "0 km"
  assertLeft "PDouble zero" $ Parser.parse @(SomeDistance PDouble) "0 kilometers"

  Right (mkSomeDistanceD SMeter 0) @=? Parser.parse "0 mi"
  Right (mkSomeDistanceD SMeter 0) @=? Parser.parse "0 miles"
  assertLeft "PDouble zero" $ Parser.parse @(SomeDistance PDouble) "0 mi"
  assertLeft "PDouble zero" $ Parser.parse @(SomeDistance PDouble) "0 miles"

testParseDistanceFailureCases :: TestTree
testParseDistanceFailureCases = testCase "Parse failures" $ do
  for_ bothVals $ \(d, t) -> do
    assertLeft d $ Parser.parse @(Distance Meter Double) t
    assertLeft d $ Parser.parse @(Distance Meter PDouble) t
    assertLeft d $ Parser.parse @(SomeDistance Double) t
    assertLeft d $ Parser.parse @(SomeDistance PDouble) t

  for_ vals $ \(d, t) -> do
    assertLeft d $ Parser.parse @(Distance Meter Double) t
    assertLeft d $ Parser.parse @(Distance Meter PDouble) t

  for_ someVals $ \(d, t) -> do
    assertLeft d $ Parser.parse @(SomeDistance Double) t
    assertLeft d $ Parser.parse @(SomeDistance PDouble) t
  where
    bothVals =
      [ ("Empty", ""),
        ("Text 'word'", "word"),
        ("Bad units", "2 feet"),
        ("Multiple units", "2 km meters"),
        ("Wrong order", "km 2")
      ]
    vals = [("With units", "2 km")]
    someVals = [("No units", "2")]

equalityTests :: TestTree
equalityTests =
  testGroup
    "Equality"
    [ testEquivClass,
      testEqualityCases
    ]

testEquivClass :: TestTree
testEquivClass = testPropertyNamed "testEquivClass" desc $ property $ do
  t <- forAll genSomeDistance

  let tgt = t .+. mkSomeDistanceD SMeter 1.1
      tgte = t .+. mkSomeDistanceD SMeter 0.9
      tlt = t .-. mkSomeDistanceD SMeter 1.1
      tlte = t .-. mkSomeDistanceD SMeter 0.9

  t === tlte
  t === tgte
  t /== tgt
  t /== tlt
  where
    desc = "Tests equality equivalence class"

testEqualityCases :: TestTree
testEqualityCases = testCase "Tests expected equality cases" $ do
  mkDistanceD @Meter 10_000 @=? mkDistanceD 10_000.9
  mkDistanceD @Meter 10_000 @/=? mkDistanceD 10_001.1
  mkDistanceD @Meter 10_000 @=? mkDistanceD 9_999.1
  mkDistanceD @Meter 10_000 @/=? mkDistanceD 9_998.9

  -- In contrast to the above, none of these should be equal since equality
  -- should be based on __meters__, not raw value.
  notEq @Kilometer
  notEq @Mile
  where
    notEq :: forall (d :: DistanceUnit). (SingI d) => Assertion
    notEq = do
      mkDistanceD @d 10_000 @/=? mkDistanceD 10_000.9
      mkDistanceD @d 10_000 @/=? mkDistanceD 10_001.1
      mkDistanceD @d 10_000 @/=? mkDistanceD 9_999.1
      mkDistanceD @d 10_000 @/=? mkDistanceD 9_998.9

displayTests :: TestTree
displayTests =
  testGroup
    "Display"
    [ testDisplayCases
    ]

testDisplayCases :: TestTree
testDisplayCases = testCase "Displays expected" $ do
  "10000.9 m" @=? display (mkDistanceD @Meter 10_000.9)
  "10000.9 km" @=? display (mkDistanceD @Kilometer 10_000.9)
  "10000.9 mi" @=? display (mkDistanceD @Mile 10_000.9)

genSomeDistance :: Gen (SomeDistance Double)
genSomeDistance = do
  d <- Utils.genDoubleNN
  u <- Units.genDistanceUnit

  pure $ case toSing u of
    SomeSing su -> MkSomeDistance su (MkDistance d)

genSomeDistancePos :: Gen (SomeDistance PDouble)
genSomeDistancePos = do
  d <- Utils.genDoublePos
  u <- Units.genDistanceUnit

  pure $ case toSing u of
    SomeSing su -> MkSomeDistance su (MkDistance d)

genDistanceText :: Gen Text
genDistanceText = Utils.genTextDouble

genSomeDistanceText :: Gen Text
genSomeDistanceText = do
  d <- Utils.genTextDouble
  t <- Units.genDistanceUnitText

  pure
    $ mconcat
      [ d,
        " ",
        t
      ]

genSomeDistancePosText :: Gen Text
genSomeDistancePosText = do
  d <- Utils.genTextDoublePos
  t <- Units.genDistanceUnitText

  pure
    $ mconcat
      [ d,
        " ",
        t
      ]
