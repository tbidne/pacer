{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Unit.Running.Data.Duration
  ( -- * Tests
    tests,

    -- * Generators
    genDuration,
    genDurationPos,
    genDurationPosText,
    genDurationText,
  )
where

import Data.List (take)
import Data.Word (Word8)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Running.Class.Parser qualified as Parser
import Running.Data.Duration
  ( Duration (MkDuration),
    SomeDuration (MkSomeDuration),
  )
import Running.Data.Duration qualified as Duration
import Running.Data.Duration.Units
  ( STimeUnit (SHour, SMinute, SSecond),
    TimeUnit (Hour, Minute, Second),
  )
import Unit.Prelude
import Unit.Running.Data.Distance.Units qualified as Units
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Running.Data.Duration"
    [ parseTests,
      equalityTests,
      displayTests,
      conversionTests
    ]

parseTests :: TestTree
parseTests =
  testGroup
    "Parsing"
    [ testParseProps,
      testParseDurationCases,
      testParseSomeDurationCases,
      testParseDurationFailureCases
    ]

testParseProps :: TestTree
testParseProps = testPropertyNamed "testParseProps" desc $ property $ do
  txt <- forAll genDurationText

  t1 <- parseOrDieM @(Duration Second Double) txt
  t2 <- parseOrDieM @(Duration Minute Double) txt
  t3 <- parseOrDieM @(Duration Hour Double) txt

  MkSomeDuration SSecond t1 === MkSomeDuration SMinute t2
  MkSomeDuration SMinute t2 === MkSomeDuration SHour t3

  -- doubles are not transitive, so let's check manually
  MkSomeDuration SSecond t1 === MkSomeDuration SHour t3
  where
    desc = "Time string invariant under requested parse type"

testParseDurationCases :: TestTree
testParseDurationCases = testCase "Parses Duration" $ do
  for_ [(s, uv) | s <- strs, uv <- unitVals] $ \(s, (v, dunit)) -> do
    case toSing dunit of
      SomeSing @TimeUnit @d stime -> withSingI stime $ do
        Right (mkDurationD @d v) @=? Parser.parse s
        Right (mkDurationPD @d v) @=? Parser.parse s

  for_ [(s, u) | s <- zstrs, u <- units] $ \(s, dunit) -> do
    case toSing dunit of
      SomeSing @TimeUnit @d stime -> withSingI stime $ do
        Right (mkDurationD @d 0) @=? Parser.parse s
        assertLeft "PDouble zero" $ Parser.parse @(Duration d PDouble) s
  where
    -- All the same time value
    strs =
      [ "12600",
        "12600s",
        "210m",
        "208m120s",
        "3h30m",
        "3h30m0s",
        "2h15m4500s"
      ]

    units = [Second, Minute, Hour]
    unitVals = zip [12_600, 210, 3.5] units

    zstrs =
      [ "0h",
        "0h0m",
        "0h0m0s",
        "0m",
        "0m0s",
        "0s",
        "0"
      ]

testParseSomeDurationCases :: TestTree
testParseSomeDurationCases = testCase "Parses SomeDuration" $ do
  for_ strs $ \s -> do
    -- SomeDuration always stores as seconds
    Right (mkSomeDurationD SSecond 12600) @=? Parser.parse s
    Right (mkSomeDurationPD SSecond 12600) @=? Parser.parse s

  for_ zstrs $ \s -> do
    -- SomeDuration always stores as seconds
    Right (mkSomeDurationD SSecond 0) @=? Parser.parse s
    assertLeft "PDouble zero" $ Parser.parse @(SomeDuration PDouble) s
  where
    -- All the same time value
    strs =
      [ "12600",
        "12600s",
        "210m",
        "208m120s",
        "3h30m",
        "3h30m0s",
        "2h15m4500s"
      ]

    zstrs =
      [ "0h",
        "0h0m",
        "0h0m0s",
        "0m",
        "0m0s",
        "0s",
        "0"
      ]

testParseDurationFailureCases :: TestTree
testParseDurationFailureCases = testCase "Parse failures" $ do
  for_ vals $ \(d, t) -> do
    assertLeft d $ Parser.parse @(Duration Second Double) t
    assertLeft d $ Parser.parse @(Duration Second PDouble) t
    assertLeft d $ Parser.parse @(SomeDuration Double) t
    assertLeft d $ Parser.parse @(SomeDuration PDouble) t
  where
    vals =
      [ ("Empty", ""),
        ("Text 'word'", "word"),
        ("Wrong order", "2s3m")
      ]

equalityTests :: TestTree
equalityTests =
  testGroup
    "Equality"
    [ testEquivClass,
      testEqualityCases
    ]

testEquivClass :: TestTree
testEquivClass = testPropertyNamed "testEquivClass" desc $ property $ do
  t <- forAll genDuration

  let tgt = t .+. MkSomeDuration SSecond (MkDuration 1.1)
      tgte = t .+. MkSomeDuration SSecond (MkDuration 0.9)
      tlt = t .-. MkSomeDuration SSecond (MkDuration 1.1)
      tlte = t .-. MkSomeDuration SSecond (MkDuration 0.9)

  t === tlte
  t === tgte
  t /== tgt
  t /== tlt
  where
    desc = "Tests equality equivalence class"

testEqualityCases :: TestTree
testEqualityCases = testCase "Tests expected equality cases" $ do
  mkDurationD @Second 10_000 @=? mkDurationD 10_000.9
  mkDurationD @Second 10_000 @/=? mkDurationD 10_001.1
  mkDurationD @Second 10_000 @=? mkDurationD 9_999.1
  mkDurationD @Second 10_000 @/=? mkDurationD 9_998.9

  -- In contrast to the above, none of these should be equal since equality
  -- should be based on __seconds__, not raw value.
  notEq @Minute
  notEq @Hour
  where
    notEq :: forall (t :: TimeUnit). (SingI t) => Assertion
    notEq = do
      mkDurationD @t 10_000 @/=? mkDurationD 10_000.9
      mkDurationD @t 10_000 @/=? mkDurationD 10_001.1
      mkDurationD @t 10_000 @/=? mkDurationD 9_999.1
      mkDurationD @t 10_000 @/=? mkDurationD 9_998.9

displayTests :: TestTree
displayTests =
  testGroup
    "Display"
    [ testDisplayCases
    ]

testDisplayCases :: TestTree
testDisplayCases = testCase "Displays expected" $ do
  "0'07\"" @=? display (mkDurationD @Second 7)
  "0'32\"" @=? display (mkDurationD @Second 32)
  "1'04\"" @=? display (mkDurationD @Second 64)
  "1h 28'45\"" @=? display (mkDurationD @Second 5_325)

  "7'00\"" @=? display (mkDurationD @Minute 7)
  "32'00\"" @=? display (mkDurationD @Minute 32)
  "1h 4'12\"" @=? display (mkDurationD @Minute 64.2)

  "30'43\"" @=? display (mkDurationD @Hour 0.512)
  "36'00\"" @=? display (mkDurationD @Hour 0.6)
  "2h 18'54\"" @=? display (mkDurationD @Hour 2.315)

conversionTests :: TestTree
conversionTests =
  testGroup
    "Conversions"
    [ testHrMinSecNormalized,
      testHrMinSecCasesTxt,
      testHrMinSecCasesData
    ]

testHrMinSecNormalized :: TestTree
testHrMinSecNormalized = testPropertyNamed "testHrMinSecNormalized" desc $ property $ do
  d <- forAll genDuration

  let (_, m, s) = case d of
        MkSomeDuration sd x -> withSingI sd Duration.toHrMinSec x

  hdiff m (<) 60
  hdiff s (<) 60
  where
    desc = "testHrMinSec is normalized"

testHrMinSecCasesTxt :: TestTree
testHrMinSecCasesTxt = testPropertyNamed name desc $ withTests 1 $ property $ do
  for_ vals $ \(e, v) -> do
    go @Second (e, v)
    go @Minute (e, v)
    go @Hour (e, v)
  where
    name = "testHrMinSecCasesTxt"
    desc = "testHrMinSec text examples"

    vals :: [((Word32, Word32, Word32), Text)]
    vals =
      [ ((5, 1, 0), "5h1m"),
        ((1000, 0, 0), "1000h")
      ]

    go ::
      forall (t :: TimeUnit).
      (SingI t) =>
      ((Word32, Word32, Word32), Text) ->
      PropertyT IO ()
    go (e, txt) = do
      d <- parseOrDieM @(Duration t Double) txt

      annotateShow d

      let r = Duration.toHrMinSec d

      e === r

testHrMinSecCasesData :: TestTree
testHrMinSecCasesData = testPropertyNamed name desc $ withTests 1 $ property $ do
  for_ vals go
  where
    name = "testHrMinSecCases"
    desc = "testHrMinSec data examples"

    vals :: [((Word32, Word32, Word32), SomeDuration Double)]
    vals =
      [ ((6, 0, 0), MkSomeDuration SSecond $ MkDuration 21599.5)
      ]

    go ::
      ((Word32, Word32, Word32), SomeDuration Double) ->
      PropertyT IO ()
    go (e, sd) = do
      annotateShow sd

      let r = case sd of
            MkSomeDuration s d -> withSingI s Duration.toHrMinSec d

      e === r

genDuration :: Gen (SomeDuration Double)
genDuration = do
  t <- Utils.genDoubleNN
  u <- Units.genTimeUnit

  pure $ case toSing u of
    SomeSing su -> MkSomeDuration su (MkDuration t)

genDurationPos :: Gen (SomeDuration PDouble)
genDurationPos = do
  t <- Utils.genDoublePos
  u <- Units.genTimeUnit

  pure $ case toSing u of
    SomeSing su -> MkSomeDuration su (MkDuration t)

-- Generates a time string like "1d2h3m4s", where we have between 1 and 4
-- units.
genDurationText :: Gen Text
genDurationText = do
  toTake <- G.integral (R.linear 1 4)
  fullStrShuffled <- genAllUnitsShuffled
  let someUnits = snd <$> sortOn fst (take toTake fullStrShuffled)

  pure $ mconcat someUnits

genDurationPosText :: Gen Text
genDurationPosText = do
  toTake <- G.integral (R.linear 0 3)
  (nonZero, maybeZeroes) <- genAllUnitsShuffledPos
  let someUnits = snd <$> sortOn fst (nonZero : take toTake maybeZeroes)

  pure $ mconcat someUnits

-- Because unit order matters, we need to pair time strings w/ an index for
-- sorting after shuffling.
type TimeUnitIndexed = Tuple2 Word8 Text

genAllUnitsShuffled :: Gen (List TimeUnitIndexed)
genAllUnitsShuffled = do
  d <- (<> "d") <$> Utils.genTextℕ
  h <- (<> "h") <$> Utils.genTextℕ
  m <- (<> "m") <$> Utils.genTextℕ
  s <- (<> "s") <$> Utils.genTextℕ
  G.shuffle $ zip [0 ..] [d, h, m, s]

genAllUnitsShuffledPos :: Gen (Tuple2 TimeUnitIndexed (List TimeUnitIndexed))
genAllUnitsShuffledPos = do
  n1 <- Utils.genTextℕ
  n2 <- Utils.genTextℕ
  n3 <- Utils.genTextℕ
  p <- Utils.genTextℕ1

  (y : xs) <- G.shuffle $ zip [0 ..] ["d", "h", "m", "s"]

  pure (second (p <>) y, zipWith (\t -> second (t <>)) [n1, n2, n3] xs)
