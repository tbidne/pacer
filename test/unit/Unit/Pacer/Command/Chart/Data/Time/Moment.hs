module Unit.Pacer.Command.Chart.Data.Time.Moment (tests) where

import Hedgehog.Gen qualified as G
import Pacer.Class.IOrd (IEq ((~~)), (/~), (<.), (<~), (>.))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time.Moment
  ( Moment (MomentTimestamp, MomentYear, MomentYearMonth),
  )
import Unit.Prelude
import Unit.TestUtils qualified as TUtils

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Time.Moment"
    [ testParseMomentProp,
      testMomentParseCases,
      testMomentEqLaws,
      testMomentIEqLaws,
      testMomentIEqCases,
      testMomentIOrdLaws,
      testMomentIOrdCases
    ]

testParseMomentProp :: TestTree
testParseMomentProp = testPropertyNamed name desc $ property $ do
  txt <- forAll TUtils.genMomentTxt
  case P.parse @Moment txt of
    Ok _ -> pure ()
    Err err -> do
      annotate err
      failure
  where
    name = "testParseMomentProp"
    desc = "Parses text"

testMomentParseCases :: TestTree
testMomentParseCases = testCase desc $ do
  isYear "2024"
  isMonth "2023-12"
  isTimestamp "2020-06-01"
  isTimestamp "2021-08-21"
  isTimestamp "2021-08-21T06:23:46"
  isTimestamp "2021-08-21T06:23:46+0800"
  where
    desc = "Parses test cases"

    isYear t = case P.parse @Moment t of
      Ok (MomentYear _) -> pure ()
      Ok other ->
        assertFailure $ "Expected year, received: " ++ show other
      Err err ->
        assertFailure $ "Unexpected parse failure: " ++ err

    isMonth t = case P.parse @Moment t of
      Ok (MomentYearMonth _ _) -> pure ()
      Ok other ->
        assertFailure $ "Expected year/month, received: " ++ show other
      Err err ->
        assertFailure $ "Unexpected parse failure: " ++ err

    isTimestamp t = case P.parse @Moment t of
      Ok (MomentTimestamp _) -> pure ()
      Ok other ->
        assertFailure $ "Expected timestamp, received: " ++ show other
      Err err ->
        assertFailure $ "Unexpected parse failure: " ++ err

testMomentEqLaws :: TestTree
testMomentEqLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genMoment
  y <- forAll genMoment
  z <- forAll genMoment

  -- We are not doing anything interesting with Moment's Eq instance, but
  -- since we have it we may as well test it...

  TUtils.testEqLaws x y z
  where
    name = "testMomentEqLaws"
    desc = "Eq laws"

testMomentIEqLaws :: TestTree
testMomentIEqLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genMoment
  y <- forAll genMoment

  TUtils.testIEqLaws x y
  where
    name = "testMomentIEqLaws"
    desc = "IEq laws"

testMomentIEqCases :: TestTree
testMomentIEqCases = testProp1 "testMomentIEqCases" desc $ do
  go
    [ -- Year / Year
      ("2023", (/~), "2022"),
      ("2023", (~~), "2023"),
      ("2023", (/~), "2024"),
      -- Year / Month
      ("2023", (/~), "2022-12"),
      ("2023", (~~), "2023-12"),
      ("2023", (/~), "2024-12"),
      -- Year / Day
      ("2023", (/~), "2022-12-08"),
      ("2023", (~~), "2023-12-08"),
      ("2023", (/~), "2024-12-08"),
      -- Year / LocalTime
      ("2023", (/~), "2022-12-08T14:13:20"),
      ("2023", (~~), "2023-12-08T14:13:20"),
      ("2023", (/~), "2024-12-08T14:13:20"),
      -- Year / Zoned
      ("2023", (/~), "2022-12-08T14:13:20+0800"),
      ("2023", (~~), "2023-12-08T14:13:20+0800"),
      ("2023", (/~), "2024-12-08T14:13:20+0800"),
      -- Month / Month
      ("2023-12", (/~), "2023-11"),
      ("2023-12", (~~), "2023-12"),
      ("2023-12", (/~), "2024-01"),
      -- Month / Day
      ("2023-12", (/~), "2023-11-08"),
      ("2023-12", (~~), "2023-12-08"),
      ("2023-12", (/~), "2024-01-08"),
      -- Month / LocalTime
      ("2023-12", (/~), "2023-11-08T14:13:20"),
      ("2023-12", (~~), "2023-12-08T14:13:20"),
      ("2023-12", (/~), "2024-01-08T14:13:20"),
      -- Month / Zoned
      ("2023-12", (/~), "2023-11-08T14:13:20+0800"),
      ("2023-12", (~~), "2023-12-08T14:13:20+0800"),
      ("2023-12", (/~), "2024-01-08T14:13:20+0800")
    ]
  where
    desc = "IEq cases"

    go vals = for_ (zip [1 ..] vals) $ \(i, (x, op, y)) ->
      assertDiff @Moment i x op y

testMomentIOrdLaws :: TestTree
testMomentIOrdLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genMoment
  y <- forAll genMoment

  TUtils.testIOrdLaws x y
  where
    name = "testMomentIOrdLaws"
    desc = "IOrd laws"

testMomentIOrdCases :: TestTree
testMomentIOrdCases = testProp1 "testMomentIOrdCases" desc $ do
  go
    [ ("2023", (<~), "2023"),
      ("2023", (<~), "2024"),
      ("2023", (<.), "2024"),
      ("2023", (<.), "2024"),
      ("2020-06-01", (<.), "2023"),
      ("2020-06-01", (<~), "2023"),
      ("2023", (>.), "2020-06-01"),
      ("2020-06-01", (\x y -> not (x >. y)), "2023"),
      ("2013-08-12T14:30:00", (\x y -> not (x >. y)), "2013-08-12")
    ]
  where
    desc = "IOrd cases"

    go vals = for_ (zip [1 ..] vals) $ \(i, (x, op, y)) ->
      assertDiff @Moment i x op y

genMoment :: Gen Moment
genMoment =
  G.choice
    [ MomentYear <$> TUtils.genYear,
      MomentYearMonth <$> TUtils.genYear <*> TUtils.genMonth,
      MomentTimestamp <$> TUtils.genTimestamp
    ]
