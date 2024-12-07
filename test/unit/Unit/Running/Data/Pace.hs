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
    [ testParsePaceGenText,
      testParseSomePaceGenText,
      testParsePaceCases,
      testParsePaceFailureCases
    ]

testParsePaceGenText :: TestTree
testParsePaceGenText = testPropertyNamed "testParsePaceGenText" desc $ property $ do
  t <- forAll genPaceText

  parseOrDieM_ @(Pace Kilometer Double) t
  parseOrDieM_ @(Pace Mile Double) t
  where
    desc = "Parses text to Pace"

testParseSomePaceGenText :: TestTree
testParseSomePaceGenText = testPropertyNamed "testParseSomePaceGenText" desc $ property $ do
  t <- forAll genSomePaceText

  parseOrDieM_ @(SomePace Double) t
  where
    desc = "Parses text to SomePace"

testParsePaceCases :: TestTree
testParsePaceCases = testCase "Parses text to expected SomePace" $ do
  for_ vals $ \(e, t) -> do
    Right (mkPaceD @Kilometer e) @=? Parser.parse t
    Right (mkPacePD @Kilometer e) @=? Parser.parse t
    Right (mkPaceD @Mile e) @=? Parser.parse t
    Right (mkPacePD @Mile e) @=? Parser.parse t

    Right (mkSomePaceD SKilometer e) @=? Parser.parse (t <> " /km")
    Right (mkSomePacePD SKilometer e) @=? Parser.parse (t <> " /km")
    Right (mkSomePaceD SKilometer e) @=? Parser.parse (t <> " /kilometer")
    Right (mkSomePacePD SKilometer e) @=? Parser.parse (t <> " /kilometer")
    Right (mkSomePaceD SMile e) @=? Parser.parse (t <> " /mi")
    Right (mkSomePacePD SMile e) @=? Parser.parse (t <> " /mi")
    Right (mkSomePaceD SMile e) @=? Parser.parse (t <> " /mile")
    Right (mkSomePacePD SMile e) @=? Parser.parse (t <> " /mile")

  for_ zVals $ \(e, t) -> do
    Right (mkPaceD @Kilometer e) @=? Parser.parse t
    assertLeft "PDouble zero" $ Parser.parse @(Pace Kilometer PDouble) t
    Right (mkPaceD @Mile e) @=? Parser.parse t
    assertLeft "PDouble zero" $ Parser.parse @(Pace Mile PDouble) t

    Right (mkSomePaceD SKilometer e) @=? Parser.parse (t <> " /km")
    assertLeft "PDouble zero" $ Parser.parse @(SomePace PDouble) (t <> " /km")
    Right (mkSomePaceD SMile e) @=? Parser.parse (t <> " /mi")
    assertLeft "PDouble zero" $ Parser.parse @(Pace Mile PDouble) (t <> " /mi")
  where
    vals =
      [ (3_600, "1h"),
        (3_840, "1h 4'"),
        (3_870, "1h 4'30\""),
        (240, "4'"),
        (270, "4'30\""),
        (30, "30\"")
      ]

    zVals =
      [ (0, "0h"),
        (0, "0h 0'"),
        (0, "0h 0'00\""),
        (0, "0'"),
        (0, "0'0\""),
        (0, "0\"")
      ]

testParsePaceFailureCases :: TestTree
testParsePaceFailureCases = testCase "Parse failures" $ do
  for_ bothVals $ \(d, t) -> do
    assertLeft d $ Parser.parse @(Pace Kilometer Double) t
    assertLeft d $ Parser.parse @(Pace Mile Double) t
    assertLeft d $ Parser.parse @(Pace Kilometer PDouble) t
    assertLeft d $ Parser.parse @(Pace Mile PDouble) t

    assertLeft d $ Parser.parse @(SomePace Double) t
    assertLeft d $ Parser.parse @(SomePace PDouble) t

  for_ vals $ \(d, t) -> do
    assertLeft d $ Parser.parse @(Pace Kilometer Double) t
    assertLeft d $ Parser.parse @(Pace Kilometer PDouble) t
    assertLeft d $ Parser.parse @(Pace Mile Double) t
    assertLeft d $ Parser.parse @(Pace Mile PDouble) t

  for_ someVals $ \(d, t) -> do
    assertLeft d $ Parser.parse @(SomePace Double) t
    assertLeft d $ Parser.parse @(SomePace PDouble) t
  where
    bothVals =
      [ ("Empty", ""),
        ("Text 'word'", "word"),
        ("Bad units", "4'30\" /m"), -- not allowed
        ("Bad units", "4'30\" /meter"), -- not allowed
        ("Bad units", "4'30\" /meters"), -- not allowed
        ("Bad units", "4'30\" /kilometers"), -- plural
        ("Bad units", "4'30\" /miles"), -- plural
        ("Multiple units", "4'30\" /km /mi"),
        ("Wrong order", "/km 4'30\"")
      ]
    vals = [("With units", "4'30\" /km")]
    someVals = [("No units", "4'30\"")]

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

  u <- G.element ["km", "kilometer", "mi", "mile"]

  pure
    $ mconcat
      [ p,
        " /",
        u
      ]
