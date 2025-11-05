{-# LANGUAGE AllowAmbiguousTypes #-}

module Unit.Pacer.Data.Distance
  ( -- * Tests
    tests,

    -- * Generators
    genSomeDistance,
    genSomeDistanceText,
  )
where

import Hedgehog.Gen qualified as G
import Pacer.Class.Parser qualified as Parser
import Pacer.Data.Distance
  ( Distance (MkDistance),
    Kilometers,
    Meters,
    Miles,
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance qualified as Dist
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SMeter),
  )
import Unit.Pacer.Data.Distance.Units qualified as Units
import Unit.Prelude
import Unit.TestUtils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Pacer.Data.Distance"
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
testParseDistanceGenText =
  testPropertyNamed
    "testParseDistanceGenText"
    desc
    $ property
    $ do
      d <- forAll genDistanceText
      parseOrDieM_ @(Meters Double) d
      parseOrDieM_ @(Kilometers Double) d
      parseOrDieM_ @(Miles Double) d

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

  r === r2
  where
    name = "testParseDisplayRoundtrip"
    desc = "parse . display is a round trip"

testParseDistanceCases :: TestTree
testParseDistanceCases = testCase "Parses Distance" $ do
  for_ [(v, u) | v <- vals, u <- units] $ \(val, dunit) -> do
    case toSing dunit of
      SomeSing @DistanceUnit @d sdist -> withSingI sdist $ do
        Ok (mkDistanceD @d val) @=? Parser.parse (showt val)

  for_ hvals $ \(val, dunit, txt) -> do
    case toSing dunit of
      SomeSing @DistanceUnit @d sdist -> withSingI sdist $ do
        Ok (mkDistanceD @d val) @=? Parser.parse txt

  for_ units $ \dunit -> do
    case toSing dunit of
      SomeSing @DistanceUnit @d sdist ->
        withSingI sdist
          $ assertErr "PDouble zero"
          $ Parser.parse @(Distance d Double) "0"
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
        (21_097.5, Meter, "half"),
        (21.0975, Kilometer, "half"),
        (13.1094, Mile, "half")
      ]

testParseSomeDistanceCases :: TestTree
testParseSomeDistanceCases = testCase "Parses SomeDistance" $ do
  let args :: List (Tuple4 Double DistanceUnit Text Text)
      args = [(v, u, utxt, s) | v <- vals, (u, utxt) <- units, s <- ["", " "]]
  for_ args $ \(val, dist, unitTxt, spc) -> do
    case toSing dist of
      SomeSing sdist -> do
        Ok (mkSomeDistanceD sdist val) @=? Parser.parse (showt val <> spc <> unitTxt)

  for_ hvals $ \(dist, val, txt) -> do
    case toSing dist of
      SomeSing sdist -> do
        Ok (mkSomeDistanceD sdist val) @=? Parser.parse txt

  for_ units $ \(_, unitTxt) -> do
    assertErr "PDouble zero" $ Parser.parse @(SomeDistance Double) ("0" <> unitTxt)
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
        (Kilometer, 21.0975, "half")
      ]

testParseDistanceFailureCases :: TestTree
testParseDistanceFailureCases = testCase "Parse failures" $ do
  for_ bothVals $ \(d, t) -> do
    assertErr d $ Parser.parseAll @(Meters Double) t
    assertErr d $ Parser.parseAll @(SomeDistance Double) t

  for_ vals $ \(d, t) -> do
    assertErr d $ Parser.parseAll @(Meters Double) t

  for_ someVals $ \(d, t) -> do
    assertErr d $ Parser.parseAll @(SomeDistance Double) t
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
  t <- forAll (genSomeDistanceGtN 1.1)

  let tgt = t .+. mkSomeDistanceD SMeter 1.1
      tgte = t .+. mkSomeDistanceD SMeter 0.9
      tlt = unsafeSub 1.1 t
      tlte = unsafeSub 0.9 t

  t === tlte
  t === tgte
  t /== tgt
  t /== tlt
  where
    desc = "Tests equality equivalence class"

    unsafeSub n =
      Dist.liftSomeDist
        (Dist.liftDist (\x -> unsafePositive $ (x ^. #unPositive) - n))

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
    notEq @d = do
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
  "10001 m" @=? display (mkDistanceD @Meter 10_000.9)
  "10000.90 km" @=? display (mkDistanceD @Kilometer 10_000.9)
  "10000.90 mi" @=? display (mkDistanceD @Mile 10_000.9)

genSomeDistance :: Gen (SomeDistance Double)
genSomeDistance = do
  d <- Utils.genDoublePos
  u <- Units.genDistanceUnit

  pure $ case toSing u of
    SomeSing su -> MkSomeDistance su (MkDistance d)

genSomeDistanceGtN :: Double -> Gen (SomeDistance Double)
genSomeDistanceGtN x = do
  d <- Utils.genDoublePos
  u <- Units.genDistanceUnit

  pure $ case toSing u of
    SomeSing su -> MkSomeDistance su (MkDistance (unsafePositive x .+. d))

genDistanceText :: Gen Text
genDistanceText =
  G.choice
    [ Utils.genTextDoublePos,
      pure "marathon",
      pure "half-marathon",
      pure "half"
    ]

genSomeDistanceText :: Gen Text
genSomeDistanceText = do
  G.choice
    [ do
        d <- Utils.genTextDoublePos
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
      pure "half"
    ]
