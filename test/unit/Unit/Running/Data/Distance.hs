{-# LANGUAGE AllowAmbiguousTypes #-}

module Unit.Running.Data.Distance
  ( -- * Tests
    tests,

    -- * Generators
    genDistance,
    genDistancePos,
    genDistanceText,
    genDistancePosText,
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
    [ testParseText,
      testParseDisplayRoundtrip,
      testParseExpectedDouble
    ]

testParseText :: TestTree
testParseText = testPropertyNamed "testParseText" desc $ property $ do
  t <- forAll genDistanceText

  void $ parseOrDieM @(SomeDistance Double) t
  where
    desc = "Parses text to distance"

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
  r <- forAll genDistance

  let t = display r
  annotateUnpack t

  r2 <- parseOrDieM @(SomeDistance Double) t
  annotateShow r

  r === r2
  where
    name = "testParseDisplayRoundtrip"
    desc = "parse . display is a round trip"

testParseExpectedDouble :: TestTree
testParseExpectedDouble = testCase "Parses expected text" $ do
  Right (MkSomeDistance SMeter (mkDistanceD 7)) @=? Parser.parse "7 m"
  Right (MkSomeDistance SMeter (mkDistanceD 7)) @=? Parser.parse "7 meters"

  Right (MkSomeDistance SKilometer (mkDistanceD 4.83)) @=? Parser.parse "4.83 km"
  Right (MkSomeDistance SKilometer (mkDistanceD 4.83)) @=? Parser.parse "4.83 kilometers"

  Right (MkSomeDistance SMile (mkDistanceD 452.5301)) @=? Parser.parse "452.5301 mi"
  Right (MkSomeDistance SMile (mkDistanceD 452.5301)) @=? Parser.parse "452.5301 miles"

equalityTests :: TestTree
equalityTests =
  testGroup
    "Equality"
    [ testEquivClass,
      testEqualityCases
    ]

testEquivClass :: TestTree
testEquivClass = testPropertyNamed "testEquivClass" desc $ property $ do
  t <- forAll genDistance

  let tgt = t .+. MkSomeDistance SMeter (MkDistance 1.1)
      tgte = t .+. MkSomeDistance SMeter (MkDistance 0.9)
      tlt = t .-. MkSomeDistance SMeter (MkDistance 1.1)
      tlte = t .-. MkSomeDistance SMeter (MkDistance 0.9)

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

genDistance :: Gen (SomeDistance Double)
genDistance = do
  d <- Utils.genDoubleNN
  u <- Units.genDistanceUnit

  pure $ case toSing u of
    SomeSing su -> MkSomeDistance su (MkDistance d)

genDistancePos :: Gen (SomeDistance PDouble)
genDistancePos = do
  d <- Utils.genDoublePos
  u <- Units.genDistanceUnit

  pure $ case toSing u of
    SomeSing su -> MkSomeDistance su (MkDistance d)

genDistanceText :: Gen Text
genDistanceText = do
  d <- Utils.genTextDouble
  t <- Units.genDistanceUnitText

  pure
    $ mconcat
      [ d,
        " ",
        t
      ]

genDistancePosText :: Gen Text
genDistancePosText = do
  d <- Utils.genTextDoublePos
  t <- Units.genDistanceUnitText

  pure
    $ mconcat
      [ d,
        " ",
        t
      ]
