module Unit.Pacer.Class.Parser (tests) where

import Data.ByteString qualified as BS
import Data.Char qualified as Ch
import Data.List qualified as L
import Pacer.Class.Parser qualified as P
import Unit.Prelude
import Unit.TestUtils qualified as TestUtils

tests :: TestTree
tests =
  testGroup
    "Pacer.Class.Parser"
    [ stripCommentTests
    ]

stripCommentTests :: TestTree
stripCommentTests =
  testGroup
    "stripComments"
    [ noCommentTests,
      lineCommentTests,
      blockCommentTests,
      allCommentTests
    ]

noCommentTests :: TestTree
noCommentTests =
  testGroup
    "No comments"
    [ testStripCommentNoneCases,
      testStripCommentNoneProps
    ]

testStripCommentNoneCases :: TestTree
testStripCommentNoneCases = testCase "Unmodified cases" $ do
  happyParse "" ""
  happyParse "some text" "some text"

testStripCommentNoneProps :: TestTree
testStripCommentNoneProps = testStripProps name desc $ do
  bs <- forAll TestUtils.genNonComment
  case P.stripComments bs of
    Err e -> do
      annotate e
      failure
    Ok r -> bs === r
  where
    name = "testStripCommentNoneProps"
    desc = "Strings without comments should not be modified"

lineCommentTests :: TestTree
lineCommentTests =
  testGroup
    "Line comments"
    [ testStripCommentLineCases,
      testStripLineCommentProps
    ]

testStripCommentLineCases :: TestTree
testStripCommentLineCases = testCase "Line comment cases" $ do
  happyParse "basicexample" "basic// should be removed\nexample"
  happyParse "basic  example" "basic // should be removed\n example"
  happyParse "more  stuff/ that here" "more //\n stuff/ that//blah\n here"
  happyParse "some / here" "some / ///* stuff */ /*\nhere"
  unhappyParse "unterminated // /* / */ blah"

testStripLineCommentProps :: TestTree
testStripLineCommentProps = testStripProps name desc $ do
  bs <- forAll TestUtils.genWithLineComments
  case P.stripComments bs of
    Err e -> annotate e *> failure
    Ok r -> do
      annotateShow r
      when (hasLineCommentStart r) failure
  where
    name = "testStripLineCommentProps"
    desc = "Strings with line comments are parsed"

s2W8List :: String -> List Word8
s2W8List = fmap (fromIntegral @Int @Word8 . Ch.ord)

blockCommentTests :: TestTree
blockCommentTests =
  testGroup
    "Block comments"
    [ testStripCommentBlockCases,
      testStripBlockCommentProps
    ]

testStripCommentBlockCases :: TestTree
testStripCommentBlockCases = testCase "Block comment cases" $ do
  happyParse "basicexample" "basic/*should be removed*/example"
  happyParse "basic  example" "basic /*should be removed*/ example"
  happyParse "more /  stuff/  here" "more / /**/ stuff/ /*that//blah*/ here"
  happyParse "blah  */ here" "blah /* foo */ */ here"
  unhappyParse "unterminated /* //\n blah"

testStripBlockCommentProps :: TestTree
testStripBlockCommentProps = testStripProps name desc $ do
  bs <- forAll TestUtils.genWithBlockComments
  case P.stripComments bs of
    Err e -> annotate e *> failure
    Ok r -> do
      annotateShow r
      when (hasBlockCommentStart r) failure
  where
    name = "testStripBlockCommentProps"
    desc = "Strings with block comments are parsed"

allCommentTests :: TestTree
allCommentTests =
  testGroup
    "Any comment"
    [ testStripAllCommentCases,
      testStripAllCommentProps
    ]

testStripAllCommentCases :: TestTree
testStripAllCommentCases = testCase "All comment cases" $ do
  happyParse "basicexample" "basic/*should // be removed*/example"
  happyParse "basicexample" "basic//should // /* be /* */ removed\nexample"

testStripAllCommentProps :: TestTree
testStripAllCommentProps = testStripProps name desc $ do
  bs <- forAll TestUtils.genWithAllComments
  case P.stripComments bs of
    Err e -> annotate e *> failure
    Ok r -> do
      annotateShow r
      when (hasLineCommentStart r || hasBlockCommentStart r) failure
  where
    name = "testStripAllCommentProps"
    desc = "Strings with arbitrary comments are parsed"

-- These generators are pretty slow, therefore we cap the tests so that CI
-- does not cry. Arguably, it'd be nice to have this conditional on being
-- run on CI i.e. CI can handle more than 1000 tests, but running them as
-- a user is less fun. We'd have to add a "--ci" flag to tasty, however.
testStripProps :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testStripProps = testPropMaxN 1_000

hasLineCommentStart :: ByteString -> Bool
hasLineCommentStart = L.isInfixOf (s2W8List "//") . BS.unpack

hasBlockCommentStart :: ByteString -> Bool
hasBlockCommentStart = L.isInfixOf (s2W8List "/*") . BS.unpack

happyParse :: ByteString -> ByteString -> Assertion
happyParse expected txt = do
  case P.stripComments txt of
    Err e -> assertFailure $ "Expected success, received:\n" ++ e
    Ok r -> expected @=? r

unhappyParse :: ByteString -> Assertion
unhappyParse txt = case P.stripComments txt of
  -- It would be nice to verify that we get the nice megaparsec errors,
  -- but testing equality directly is probably too flaky.
  Err _ -> pure ()
  Ok r -> assertFailure $ "Expected failure, received: " ++ show r
