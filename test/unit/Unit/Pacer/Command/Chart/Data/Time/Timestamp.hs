module Unit.Pacer.Command.Chart.Data.Time.Timestamp (tests) where

import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time.Timestamp.Internal
  ( Timestamp (TimestampDate, TimestampTime, TimestampZoned),
  )
import Unit.Prelude
import Unit.TestUtils qualified as TUtils

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Time.Timestamp"
    [ testParseTimestampProp,
      testParseTimestampCases,
      testTimestampEqLaws,
      testTimestampOrdLaws,
      testTimestampOrdCases,
      testTimestampIEqLaws
    ]

testParseTimestampProp :: TestTree
testParseTimestampProp = testPropertyNamed name desc $ property $ do
  txt <- forAll TUtils.genTimestampTxt
  case P.parse @Timestamp txt of
    Ok _ -> pure ()
    Err err -> do
      annotate err
      failure
  where
    name = "testParseTimestamp"
    desc = "Parses text"

testParseTimestampCases :: TestTree
testParseTimestampCases = testCase "Parses cases" $ do
  assertDate "1950-01-01"
  assertTime "1950-01-01T00:00:00"
  assertTime "1950-01-01 00:00:00"
  assertZoned "1970-01-01T12:00:00-0800"
  assertZoned "1970-01-01 12:00:00-0800"
  assertZoned "1970-01-01T12:00:00 -0800"
  assertZoned "1970-01-01 12:00:00 -0800"
  where
    assertDate t = case P.parseAll @Timestamp t of
      Err err ->
        assertFailure $ "Failed parsing date: " ++ err
      Ok (TimestampDate _) -> pure ()
      Ok bad ->
        assertFailure $ "Expected date, received: " ++ show bad

    assertTime t = case P.parseAll @Timestamp t of
      Err err ->
        assertFailure $ "Failed parsing time: " ++ err
      Ok (TimestampTime _) -> pure ()
      Ok bad ->
        assertFailure $ "Expected time, received: " ++ show bad

    assertZoned t = case P.parseAll @Timestamp t of
      Err err ->
        assertFailure $ "Failed parsing zoned: " ++ err
      Ok (TimestampZoned _) -> pure ()
      Ok bad ->
        assertFailure $ "Expected zoned, received: " ++ show bad

testTimestampEqLaws :: TestTree
testTimestampEqLaws = testPropertyNamed name desc $ property $ do
  x <- forAll TUtils.genTimestamp
  y <- forAll TUtils.genTimestamp
  z <- forAll TUtils.genTimestamp

  TUtils.testEqLaws x y z
  where
    name = "testTimestampEqLaws"
    desc = "Eq laws"

testTimestampOrdLaws :: TestTree
testTimestampOrdLaws = testPropertyNamed name desc $ property $ do
  x <- forAll TUtils.genTimestamp
  y <- forAll TUtils.genTimestamp
  z <- forAll TUtils.genTimestamp

  TUtils.testOrdLaws x y z
  where
    name = "testTimestampOrdLaws"
    desc = "Ord laws"

testTimestampOrdCases :: TestTree
testTimestampOrdCases = testProp1 "testTimestampOrdCases" desc $ do
  go
    [ ("2099-12-31T02:00:00+0800", (<=), "2099-12-31T00:00:00+0000"),
      ("2013-08-12T14:30:00", (>), "2013-08-12")
    ]
  where
    desc = "Ord cases"

    go vals = for_ (zip [1 ..] vals) $ \(i, (x, op, y)) ->
      assertDiff @Timestamp i x op y

testTimestampIEqLaws :: TestTree
testTimestampIEqLaws = testPropertyNamed name desc $ property $ do
  x <- forAll TUtils.genTimestamp
  y <- forAll TUtils.genTimestamp

  TUtils.testIEqLaws x y
  where
    name = "testTimestampIEqLaws"
    desc = "IEq laws"
