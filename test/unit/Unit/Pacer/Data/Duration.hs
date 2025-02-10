{-# LANGUAGE AllowAmbiguousTypes #-}

module Unit.Pacer.Data.Duration
  ( -- * Tests
    tests,

    -- * Generators
    genSeconds,
    genSecondsPos,
    genDuration,
    genDurationPos,
    genDurationPosText,
    genDurationText,
  )
where

import Data.List (take)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Pacer.Class.Parser qualified as Parser
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Data.Duration qualified as Duration
import Pacer.Data.Result (Result (Ok))
import Unit.Prelude
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Pacer.Data.Duration"
    [ parseTests,
      equalityTests,
      displayTests,
      conversionTests
    ]

parseTests :: TestTree
parseTests =
  testGroup
    "Parsing"
    [ testParseDurationCases,
      testParseDurationFailureCases
    ]

testParseDurationCases :: TestTree
testParseDurationCases = testCase "Parses Duration" $ do
  for_ strs $ \s -> do
    Ok (mkDurationD 12_600) @=? Parser.parse s
    Ok (mkDurationPD 12_600) @=? Parser.parse s

  for_ zstrs $ \s -> do
    Ok (mkDurationD 0) @=? Parser.parse s
    assertErr "PDouble zero" $ Parser.parse @(Duration PDouble) s
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
    assertErr d $ Parser.parse @(Duration Double) t
    assertErr d $ Parser.parse @(Duration PDouble) t
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

  let tgt = t .+. MkDuration 1.1
      tgte = t .+. MkDuration 0.9
      tlt = t .-. MkDuration 1.1
      tlte = t .-. MkDuration 0.9

  t === tlte
  t === tgte
  t /== tgt
  t /== tlt
  where
    desc = "Tests equality equivalence class"

testEqualityCases :: TestTree
testEqualityCases = testCase "Tests expected equality cases" $ do
  mkDurationD 10_000 @=? mkDurationD 10_000.9
  mkDurationD 10_000 @/=? mkDurationD 10_001.1
  mkDurationD 10_000 @=? mkDurationD 9_999.1
  mkDurationD 10_000 @/=? mkDurationD 9_998.9

displayTests :: TestTree
displayTests =
  testGroup
    "Display"
    [ testDisplayCases
    ]

testDisplayCases :: TestTree
testDisplayCases = testCase "Displays expected" $ do
  "0'07\"" @=? display (mkDurationD 7)
  "0'32\"" @=? display (mkDurationD 32)
  "1'04\"" @=? display (mkDurationD 64)
  "1h 28'45\"" @=? display (mkDurationD 5_325)

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

  let (_, m, s) = Duration.toHrMinSec d

  hdiff m (<) 60
  hdiff s (<) 60
  where
    desc = "testHrMinSec is normalized"

testHrMinSecCasesTxt :: TestTree
testHrMinSecCasesTxt = testPropertyNamed name desc $ withTests 1 $ property $ do
  for_ vals $ \(e, v) -> do
    go (e, v)
  where
    name = "testHrMinSecCasesTxt"
    desc = "testHrMinSec text examples"

    vals :: [((Word32, Word32, Word32), Text)]
    vals =
      [ ((5, 1, 0), "5h1m"),
        ((1000, 0, 0), "1000h")
      ]

    go ::
      ((Word32, Word32, Word32), Text) ->
      PropertyT IO ()
    go (e, txt) = do
      d <- parseOrDieM @(Duration Double) txt

      annotateShow d

      let r = Duration.toHrMinSec d

      e === r

testHrMinSecCasesData :: TestTree
testHrMinSecCasesData = testPropertyNamed name desc $ withTests 1 $ property $ do
  for_ vals go
  where
    name = "testHrMinSecCases"
    desc = "testHrMinSec data examples"

    vals :: [((Word32, Word32, Word32), Duration Double)]
    vals =
      [ ((6, 0, 0), MkDuration 21599.5)
      ]

    go ::
      ((Word32, Word32, Word32), Duration Double) ->
      PropertyT IO ()
    go (e, sd) = do
      annotateShow sd

      let r = Duration.toHrMinSec sd

      e === r

genDuration :: Gen (Duration Double)
genDuration = do
  t <- Utils.genDoubleNN

  pure $ MkDuration t

genSecondsPos :: Gen (Duration PDouble)
genSecondsPos = MkDuration <$> Utils.genDoublePos

genSeconds :: Gen (Duration Double)
genSeconds = MkDuration <$> Utils.genDouble

genDurationPos :: Gen (Duration PDouble)
genDurationPos = do
  t <- Utils.genDoublePos
  pure $ MkDuration t

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
