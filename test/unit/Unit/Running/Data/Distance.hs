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

import Hedgehog.Gen qualified as G
import Running.Class.Parser qualified as Parser
import Running.Data.Distance
  ( Distance (MkDistance),
    SomeDistance (MkSomeDistance),
  )
import Running.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SMeter),
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
testParseDistanceCases = testCase "Parses Distance" $ do
  for_ [(v, u) | v <- vals, u <- units] $ \(val, dunit) -> do
    case toSing dunit of
      SomeSing @DistanceUnit @d sdist -> withSingI sdist $ do
        Right (mkDistanceD @d val) @=? Parser.parse (showt val)
        Right (mkDistancePD @d val) @=? Parser.parse (showt val)

  for_ hvals $ \(val, dunit, txt) -> do
    case toSing dunit of
      SomeSing @DistanceUnit @d sdist -> withSingI sdist $ do
        Right (mkDistanceD @d val) @=? Parser.parse txt
        Right (mkDistancePD @d val) @=? Parser.parse txt

  for_ units $ \dunit -> do
    case toSing dunit of
      SomeSing @DistanceUnit @d sdist -> withSingI sdist $ do
        Right (mkDistanceD @d 0) @=? Parser.parse "0"
        assertLeft "PDouble zero" $ Parser.parse @(Distance d PDouble) "0"
  where
    vals = [7, 4.83, 452.5301]

    units = [Meter, Kilometer, Mile]

    hvals =
      [ (42_195, Meter, "marathon"),
        (42.195, Kilometer, "marathon"),
        (26.2188, Mile, "marathon"),
        (21_097.5, Meter, "half-marathon"),
        (21.0975, Kilometer, "half-marathon"),
        (13.1094, Mile, "half-marathon"),
        (21_097.5, Meter, "hmarathon"),
        (21.0975, Kilometer, "hmarathon"),
        (13.1094, Mile, "hmarathon")
      ]

testParseSomeDistanceCases :: TestTree
testParseSomeDistanceCases = testCase "Parses SomeDistance" $ do
  let args :: List (Tuple4 Double DistanceUnit Text Text)
      args = [(v, u, utxt, s) | v <- vals, (u, utxt) <- units, s <- ["", " "]]
  for_ args $ \(val, dist, unitTxt, spc) -> do
    case toSing dist of
      SomeSing sdist -> do
        Right (mkSomeDistanceD sdist val) @=? Parser.parse (showt val <> spc <> unitTxt)
        Right (mkSomeDistancePD sdist val) @=? Parser.parse (showt val <> spc <> unitTxt)

  for_ hvals $ \(dist, val, txt) -> do
    case toSing dist of
      SomeSing sdist -> do
        Right (mkSomeDistanceD sdist val) @=? Parser.parse txt
        Right (mkSomeDistancePD sdist val) @=? Parser.parse txt

  for_ units $ \(dist, unitTxt) -> do
    case toSing dist of
      SomeSing sdist -> do
        Right (mkSomeDistanceD sdist 0) @=? Parser.parse ("0" <> unitTxt)
        assertLeft "PDouble zero" $ Parser.parse @(SomeDistance PDouble) ("0" <> unitTxt)
  where
    vals = [7, 4.83, 452.5301]

    units =
      [ (Meter, "m"),
        (Meter, "meters"),
        (Kilometer, "km"),
        (Kilometer, "kilometers"),
        (Mile, "mi"),
        (Mile, "miles")
      ]

    hvals =
      [ (Kilometer, 42.195, "marathon"),
        (Kilometer, 21.0975, "half-marathon"),
        (Kilometer, 21.0975, "hmarathon")
      ]

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
genDistanceText =
  G.choice
    [ Utils.genTextDouble,
      pure "marathon",
      pure "half-marathon",
      pure "hmarathon"
    ]

genSomeDistanceText :: Gen Text
genSomeDistanceText = do
  G.choice
    [ do
        d <- Utils.genTextDouble
        t <- Units.genDistanceUnitText
        s <- Utils.genAffineSpace

        pure
          $ mconcat
            [ d,
              s,
              t
            ],
      pure "marathon",
      pure "half-marathon",
      pure "hmarathon"
    ]

genSomeDistancePosText :: Gen Text
genSomeDistancePosText = do
  d <- Utils.genTextDoublePos
  t <- Units.genDistanceUnitText
  s <- Utils.genAffineSpace

  pure
    $ mconcat
      [ d,
        s,
        t
      ]
