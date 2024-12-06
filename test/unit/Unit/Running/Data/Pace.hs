module Unit.Running.Data.Pace
  ( tests,
  )
where

import Data.List (sortOn, take)
import Data.Tuple (fst, snd)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Running.Class.Parser qualified as Parser
import Running.Data.Distance (DistanceUnit (Kilometer, Mile))
import Running.Data.Distance.Units (SDistanceUnit (SKilometer, SMile))
import Running.Data.Duration (Duration (MkDuration))
import Running.Data.Pace (Pace (MkPace), SomePace (MkSomePace))
import Unit.Prelude
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Running.Data.Pace"
    [ parsingTests,
      displayTests
    ]

parsingTests :: TestTree
parsingTests =
  testGroup
    "Parsing"
    [ testParsePace,
      testParseSomePace,
      testParsePaceCases
    ]

testParsePace :: TestTree
testParsePace = testPropertyNamed "testParsePace" desc $ property $ do
  t <- forAll genPaceText

  void $ parseOrDieM @(Pace Kilometer Double) t
  void $ parseOrDieM @(Pace Mile Double) t
  where
    desc = "Parses text to Pace"

testParseSomePace :: TestTree
testParseSomePace = testPropertyNamed "testParseSomePace" desc $ property $ do
  t <- forAll genSomePaceText

  void $ parseOrDieM @(SomePace Double) t
  where
    desc = "Parses text to SomePace"

testParsePaceCases :: TestTree
testParsePaceCases = testCase "Parses text to expected SomePace" $ do
  mk 3600 @=? Parser.parse @(Pace Kilometer Double) "1h"
  mk 3840 @=? Parser.parse @(Pace Kilometer Double) "1h 4'"
  mk 3870 @=? Parser.parse @(Pace Kilometer Double) "1h 4'30\""
  mk 240 @=? Parser.parse @(Pace Kilometer Double) "4'"
  mk 270 @=? Parser.parse @(Pace Kilometer Double) "4'30\""
  mk 30 @=? Parser.parse @(Pace Kilometer Double) "30\""

  mks SKilometer 3600 @=? Parser.parse @(SomePace Double) "1h /km"
  mks SKilometer 3840 @=? Parser.parse @(SomePace Double) "1h 4' /km"
  mks SKilometer 3870 @=? Parser.parse @(SomePace Double) "1h 4'30\" /km"
  mks SKilometer 240 @=? Parser.parse @(SomePace Double) "4' /km"
  mks SKilometer 270 @=? Parser.parse @(SomePace Double) "4'30\" /km"
  mks SKilometer 30 @=? Parser.parse @(SomePace Double) "30\" /km"
  mks SMile 495 @=? Parser.parse @(SomePace Double) "8'15\" /mi"
  where
    mk y = Right $ MkPace (MkDuration y)
    mks s y = Right $ MkSomePace s (MkPace (MkDuration y))

displayTests :: TestTree
displayTests =
  testGroup
    "Display"
    [ testDisplayCases
    ]

testDisplayCases :: TestTree
testDisplayCases = testCase "Displays expected" $ do
  "5'20\" /km" @=? display (MkPace @Kilometer @Double (MkDuration 320))
  "5'20\" /mi" @=? display (MkPace @Mile @Double (MkDuration 320))
  "5'20\" /km" @=? display (MkSomePace @_ @Double SKilometer (MkPace $ MkDuration 320))
  "5'20\" /mi" @=? display (MkSomePace @_ @Double SMile (MkPace $ MkDuration 320))

genPaceText :: Gen Text
genPaceText = do
  toTake <- G.integral (R.linear 1 3)

  h <- genHours
  m <- genMinutes
  s <- genSeconds

  mconcat . fmap snd . sortOn fst . take toTake <$> G.shuffle [h, m, s]
  where
    genHours :: Gen (Int, Text)
    genHours = (0,) . (<> "h") <$> Utils.genTextℕ
    genMinutes = (1,) . (<> "'") <$> Utils.genTextℕ
    genSeconds = (2,) . (<> "\"") <$> Utils.genTextℕ

genSomePaceText :: Gen Text
genSomePaceText = do
  p <- genPaceText

  u <- G.element ["km", "kilometers", "mi", "miles"]

  pure
    $ mconcat
      [ p,
        " /",
        u
      ]
