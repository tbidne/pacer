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
testStripCommentNoneCases = testProp1 "testStripCommentNoneCases" desc $ do
  happyParse "" ""
  happyParse "some text" "some text"
  where
    desc = "Unmodified cases"

testStripCommentNoneProps :: TestTree
testStripCommentNoneProps = testStripProps name desc $ do
  bs <- forAll TestUtils.genNonComment
  runStripComments bs $ \case
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
testStripCommentLineCases = testProp1 "testStripCommentLineCases" desc $ do
  happyParse "basicexample" "basic// should be removed\nexample"
  happyParse "basic  example" "basic // should be removed\n example"
  happyParse "more  stuff/ that here" "more //\n stuff/ that//blah\n here"
  happyParse "some / here" "some / ///* stuff */ /*\nhere"
  unhappyParse "unterminated // /* / */ blah"
  where
    desc = "Line comment cases"

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
testStripCommentBlockCases = testProp1 "testStripCommentBlockCases" desc $ do
  happyParse "basicexample" "basic/*should be removed*/example"
  happyParse "basic  example" "basic /*should be removed*/ example"
  happyParse "more /  stuff/  here" "more / /**/ stuff/ /*that//blah*/ here"
  happyParse "blah  */ here" "blah /* foo */ */ here"
  happyParse "foo" "/***/foo"
  happyParse "bar" "/**sometext**/bar"
  unhappyParse "unterminated /* //\n blah"
  where
    desc = "Block comment cases"

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
testStripAllCommentCases = testProp1 "testStripAllCommentCases" desc $ do
  happyParse "basicexample" "basic/*should // be removed*/example"
  happyParse "basicexample" "basic//should // /* be /* */ removed\nexample"
  where
    desc = "All comment cases"

testStripAllCommentProps :: TestTree
testStripAllCommentProps = testStripProps name desc $ do
  bs <- forAll TestUtils.genWithAllComments
  runStripComments bs $ \case
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

happyParse :: ByteString -> ByteString -> PropertyT IO ()
happyParse expected txt =
  runStripComments txt $ \case
    Err e -> do
      annotate $ "Expected success, received:\n" ++ e
      failure
    Ok r -> expected === r

unhappyParse :: ByteString -> PropertyT IO ()
unhappyParse txt =
  runStripComments txt $ \case
    -- It would be nice to verify that we get the nice megaparsec errors,
    -- but testing equality directly is probably too flaky.
    Err _ -> pure ()
    Ok r -> do
      annotate $ "Expected failure, received: " ++ show r
      failure

runStripComments ::
  ByteString ->
  (ResultDefault ByteString -> PropertyT IO ()) ->
  PropertyT IO ()
runStripComments bs m = do
  for_ stripCommentsFns $ \(a, p) -> do
    annotateShow bs
    annotate a
    m (p bs)

stripCommentsFns :: List (Tuple2 String (ByteString -> ResultDefault ByteString))
stripCommentsFns =
  [ ("ByteString", P.stripCommentsBS),
    ("Megaparsec auto", P.stripCommentsMpAuto),
    ("Megaparsec manual", P.stripCommentsMpManual)
  ]
