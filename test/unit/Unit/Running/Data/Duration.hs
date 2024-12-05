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

import Data.List (sort, take)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Running.Class.Parser qualified as Parser
import Running.Class.Singleton
  ( SingI,
    SingKind (toSing),
    SomeSing (MkSomeSing),
    withSingI,
  )
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
    [ testParseText,
      testParseExpectedDouble
    ]

testParseText :: TestTree
testParseText = testPropertyNamed "testParseText" desc $ property $ do
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

testParseExpectedDouble :: TestTree
testParseExpectedDouble = testCase "Parses expected text" $ do
  for_ strs $ \s -> do
    Right (mkDurationD @Second 12600) @=? Parser.parse s
    Right (mkDurationD @Minute 210) @=? Parser.parse s
    Right (mkDurationD @Hour 3.5) @=? Parser.parse s
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
    MkSomeSing su -> MkSomeDuration su (MkDuration t)

genDurationPos :: Gen (SomeDuration PDouble)
genDurationPos = do
  t <- Utils.genDoublePos
  u <- Units.genTimeUnit

  pure $ case toSing u of
    MkSomeSing su -> MkSomeDuration su (MkDuration t)

-- Generates a time string like "1d2h3m4s", where we have between 1 and 4
-- units.
genDurationText :: Gen Text
genDurationText = do
  toTake <- G.integral (R.linear 1 4)
  fullStrShuffled <- genAllUnitsShuffled
  let someUnits = sort $ take toTake fullStrShuffled

  pure $ mconcat $ unitToTxt <$> someUnits

genDurationPosText :: Gen Text
genDurationPosText = do
  toTake <- G.integral (R.linear 0 3)
  (nonZero, maybeZeroes) <- genAllUnitsShuffledPos
  let someUnits = sort $ nonZero : take toTake maybeZeroes

  pure $ mconcat $ unitToTxt <$> someUnits

data TimeStrUnit
  = Days Natural
  | Hours Natural
  | Minutes Natural
  | Seconds Natural
  deriving stock (Eq, Show)

instance Ord TimeStrUnit where
  Days _ `compare` Days _ = EQ
  Days _ `compare` _ = LT
  _ `compare` Days _ = GT
  Hours _ `compare` Hours _ = EQ
  Hours _ `compare` _ = LT
  _ `compare` Hours _ = GT
  Minutes _ `compare` Minutes _ = EQ
  Minutes _ `compare` _ = LT
  _ `compare` Minutes _ = GT
  Seconds _ `compare` Seconds _ = EQ

genAllUnitsShuffled :: Gen [TimeStrUnit]
genAllUnitsShuffled = do
  d <- Utils.genℕ
  h <- Utils.genℕ
  m <- Utils.genℕ
  s <- Utils.genℕ
  G.shuffle [Days d, Hours h, Minutes m, Seconds s]

genAllUnitsShuffledPos :: Gen (TimeStrUnit, [TimeStrUnit])
genAllUnitsShuffledPos = do
  n1 <- Utils.genℕ
  n2 <- Utils.genℕ
  n3 <- Utils.genℕ
  p <- Utils.genℕ1

  (d : [c1, c2, c3]) <- G.shuffle [Days, Hours, Minutes, Seconds]

  pure (d p, [c1 n1, c2 n2, c3 n3])

unitToTxt :: TimeStrUnit -> Text
unitToTxt (Days d) = showt d <> "d"
unitToTxt (Hours h) = showt h <> "h"
unitToTxt (Minutes m) = showt m <> "m"
unitToTxt (Seconds m) = showt m <> "s"
