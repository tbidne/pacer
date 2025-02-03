module Unit.Pacer.Data.Distance.Units
  ( -- * Tests
    tests,

    -- * Generators
    genDistanceUnit,
    genDistanceUnitText,
    genTimeUnit,

    -- * Misc
    shortDistanceUnitText,
    longDistanceUnitText,
  )
where

import Hedgehog.Gen qualified as G
import Pacer.Class.Parser qualified as Parser
import Pacer.Data.Distance.Units (DistanceUnit (Kilometer, Meter, Mile))
import Pacer.Data.Duration (TimeUnit)
import Pacer.Data.Result (Result (Ok))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Data.Distance.Units"
    [ parseTests
    ]

parseTests :: TestTree
parseTests =
  testGroup
    "Parsing"
    [ testParseText,
      testParseInjectiveShort,
      testParseInjectiveLong,
      testDisplayParseRoundtrip,
      testParseDisplayRoundtrip,
      testParseExpected
    ]

testParseText :: TestTree
testParseText = testPropertyNamed "testParseText" desc $ property $ do
  t <- forAll genDistanceUnitText

  parseOrDieM_ @DistanceUnit t
  where
    desc = "Parses text to units"

testParseInjectiveShort :: TestTree
testParseInjectiveShort = testPropertyNamed name desc $ property $ do
  t1 <- forAll $ G.element shortDistanceUnitText
  t2 <- forAll $ G.element shortDistanceUnitText

  r1 <- parseOrDieM @DistanceUnit t1
  r2 <- parseOrDieM @DistanceUnit t2

  annotateShow r1
  annotateShow r2

  when (r1 == r2) $ t1 === t2
  where
    name = "testParseInjectiveShort"
    desc = "Parsing short units is injective"

testParseInjectiveLong :: TestTree
testParseInjectiveLong = testPropertyNamed name desc $ property $ do
  t1 <- forAll $ G.element longDistanceUnitText
  t2 <- forAll $ G.element longDistanceUnitText

  r1 <- parseOrDieM @DistanceUnit t1
  r2 <- parseOrDieM @DistanceUnit t2

  annotateShow r1
  annotateShow r2

  when (r1 == r2) $ t1 === t2
  where
    name = "testParseInjectiveLong"
    desc = "Parsing long units is injective"

testDisplayParseRoundtrip :: TestTree
testDisplayParseRoundtrip = testPropertyNamed name desc $ property $ do
  t <- forAll $ G.element shortDistanceUnitText

  r <- parseOrDieM @DistanceUnit t
  annotateShow r

  t === display r
  where
    name = "testDisplayParseRoundtrip"
    desc = "display . parse (short) is a round trip"

testParseDisplayRoundtrip :: TestTree
testParseDisplayRoundtrip = testPropertyNamed name desc $ property $ do
  r <- forAll genDistanceUnit

  let t = display r

  r2 <- parseOrDieM @DistanceUnit t
  annotateShow r

  r === r2
  where
    name = "testParseDisplayRoundtrip"
    desc = "parse . display is a round trip"

testParseExpected :: TestTree
testParseExpected = testCase "Parses expected text" $ do
  Ok Meter @=? Parser.parse "m"
  Ok Meter @=? Parser.parse "meters"
  Ok Kilometer @=? Parser.parse "km"
  Ok Kilometer @=? Parser.parse "kilometers"
  Ok Mile @=? Parser.parse "mi"
  Ok Mile @=? Parser.parse "miles"

genDistanceUnit :: Gen DistanceUnit
genDistanceUnit = G.enumBounded

genTimeUnit :: Gen TimeUnit
genTimeUnit = G.enumBounded

genDistanceUnitText :: Gen Text
genDistanceUnitText =
  G.element
    $ shortDistanceUnitText
    <> longDistanceUnitText

shortDistanceUnitText :: List Text
shortDistanceUnitText = ["m", "km", "mi"]

longDistanceUnitText :: List Text
longDistanceUnitText = ["meters", "kilometers", "miles"]
