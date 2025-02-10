module Unit.Pacer.Data.Pace
  ( -- * Tests
    tests,

    -- * Generators
    genSomePace,
  )
where

import Hedgehog.Gen qualified as G
import Pacer.Class.Parser qualified as Parser
import Pacer.Data.Distance (DistanceUnit (Kilometer, Meter, Mile))
import Pacer.Data.Distance.Units (SDistanceUnit (SKilometer, SMile))
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Data.Pace (Pace (MkPace), SomePace (MkSomePace))
import Pacer.Data.Result (Result (Ok))
import Unit.Pacer.Data.Distance.Units qualified as Units
import Unit.Pacer.Data.Duration qualified as D
import Unit.Pacer.Data.Duration qualified as Duration
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Data.Pace"
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
  -- NOTE: [Unit Loop]
  --
  -- Cannot (easily) loop over the units like in Duration and Distance
  -- because of the PaceDistF constraint.
  for_ vals $ \(e, t) -> do
    Ok (mkPaceD @Kilometer e) @=? Parser.parse t
    Ok (mkPaceD @Mile e) @=? Parser.parse t

    Ok (mkSomePaceD SKilometer e) @=? Parser.parse (t <> " /km")
    Ok (mkSomePaceD SKilometer e) @=? Parser.parse (t <> " /kilometer")
    Ok (mkSomePaceD SMile e) @=? Parser.parse (t <> " /mi")
    Ok (mkSomePaceD SMile e) @=? Parser.parse (t <> " /mile")

  for_ zVals $ \t -> do
    assertErr "PDouble zero" $ Parser.parse @(Pace Kilometer Double) t
    assertErr "PDouble zero" $ Parser.parse @(Pace Mile Double) t

    assertErr "PDouble zero" $ Parser.parse @(SomePace Double) (t <> " /km")
    assertErr "PDouble zero" $ Parser.parse @(Pace Mile Double) (t <> " /mi")
  where
    vals =
      [ (3_600, "1h"),
        (3_840, "1h4m"),
        (3_870, "1h4m30s"),
        (240, "4m"),
        (270, "4m30s"),
        (30, "30s"),
        (30, "30")
      ]

    zVals =
      [ "0h",
        "0h0m",
        "0h0m00s",
        "0m",
        "0m0s",
        "0s",
        "0"
      ]

testParsePaceFailureCases :: TestTree
testParsePaceFailureCases = testCase "Parse failures" $ do
  -- see NOTE: [Unit Loop]
  for_ bothVals $ \(d, t) -> do
    assertErr d $ Parser.parseAll @(Pace Kilometer Double) t
    assertErr d $ Parser.parseAll @(Pace Mile Double) t

    assertErr d $ Parser.parseAll @(SomePace Double) t

  for_ vals $ \(d, t) -> do
    assertErr d $ Parser.parseAll @(Pace Kilometer Double) t
    assertErr d $ Parser.parseAll @(Pace Mile Double) t

  for_ someVals $ \(d, t) -> do
    assertErr d $ Parser.parseAll @(SomePace Double) t
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
  "5'20\" /km" @=? display (MkPace @Kilometer @Double (MkDuration $ fromℤ 320))
  "5'20\" /mi" @=? display (MkPace @Mile @Double (MkDuration $ fromℤ 320))
  "5'20\" /km" @=? display (MkSomePace @_ @Double SKilometer (MkPace $ MkDuration $ fromℤ 320))
  "5'20\" /mi" @=? display (MkSomePace @_ @Double SMile (MkPace $ MkDuration $ fromℤ 320))

genPaceText :: Gen Text
genPaceText = D.genDurationPosText

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

genSomePace :: Gen (SomePace Double)
genSomePace = do
  u <- Units.genDistanceUnit
  seconds <- Duration.genDuration

  case u of
    Meter -> pure $ MkSomePace SKilometer (MkPace seconds)
    Kilometer -> pure $ MkSomePace SKilometer (MkPace seconds)
    Mile -> pure $ MkSomePace SMile (MkPace seconds)
