module Unit.Pacer.Utils (tests) where

import Data.Aeson (Value)
import Data.Aeson qualified as Asn
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as C8
import GHC.Real (Integral (div))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pacer.Utils.Json qualified as Json
import Unit.Prelude
import Unit.TestUtils qualified as TestUtils

tests :: TestTree
tests =
  testGroup
    "Pacer.Utils"
    [ testJsoncDecodeSpecs,
      testJsoncDecode
    ]

testJsoncDecodeSpecs :: TestTree
testJsoncDecodeSpecs = testProp1 "testJsoncDecodeSpecs" desc $ do
  for_ vals assertSuccess
  where
    desc = "Decodes json specs"

    assertSuccess j = case Json.decodeJson @Value j of
      Ok _ -> pure ()
      Err err -> do
        annotate $ displayException err
        annotate $ show j
        failure

    vals =
      [ "/***/[{}, {\"a\": {\"a\": {\"a\": {},\"a\": \"a\"}},\"a\": {},\"a\": [{\"a\": \"a\",\"a\": \"a\"}, {\"a\": \"a\",\"a\": \"a\"}]}, {\"a\": \"a\",\"a\": \"a\",\"a\": \"a\"}]",
        "/***/[{\"a\": [{\"a\": \"a\",\"a\": [{}, {}]}]}]"
      ]

testJsoncDecode :: TestTree
testJsoncDecode = testPropMaxN 100_000 "testJsoncDecode" desc $ do
  (json, jsonc) <- forAll genJsonAndJsonc

  case review #eitherIso (Asn.eitherDecodeStrict @Value json) of
    Err asnErr -> do
      annotate "Generated invalid json; this should never happen."
      annotate asnErr
      failure
    Ok asnResult ->
      case Json.decodeJson @Value jsonc of
        Err utilsResult -> do
          annotate "Json is valid but failed to parse jsonc."
          annotate (displayException utilsResult)
          failure
        Ok utilsResult -> do
          annotate "Jsonc decoded but differed from json decoding"
          asnResult === utilsResult
  where
    desc = "Decodes generated jsonc"

genJsonAndJsonc :: Gen (Tuple2 ByteString ByteString)
genJsonAndJsonc = do
  json <- genJson
  let jsonLines = C8.lines json
      numLines = length jsonLines
      numEachComment = numLines `div` 2

  -- Generate comments. Since these comments are intercalated into the json,
  -- we are capped by numLines / 2. No sense generating more than we need and
  -- slowing down the program.
  lineComments <- Gen.list (Range.linearFrom 0 0 numEachComment) TestUtils.genLineComment
  blockComments <- Gen.list (Range.linearFrom 0 0 numEachComment) TestUtils.genBlockComment
  allComments <- Gen.shuffle (lineComments ++ blockComments)

  -- Insert comments in between the json's newlines. We previously shuffled
  -- the json w/ the comments, but that can produce invalid json, as the order
  -- obviously matters there.
  --
  -- This is morally intercalate, though we use a fold as the intercalated
  -- value is not static (the allComments list).
  let jsonc =
        toStrictBS
          . builderToLazyBS
          . fst
          . foldl' go ("", allComments)
          $ jsonLines

  pure (json, jsonc)
  where
    -- For each line, we insert a comment. If the (finite) comment list is
    -- empty, we simply insert an empty string, per unconsBS. We briefly
    -- attempted writing our own stream type, but that led to an infinite
    -- loop. At least this way is simple and reasonably fast.
    go ::
      Tuple2 ByteStringBuilder (List ByteString) ->
      ByteString ->
      Tuple2 ByteStringBuilder (List ByteString)
    go (acc, comments) bs =
      let (c, cs) = unconsBS comments
          c' = BSB.byteString c
          bs' = BSB.byteString bs
       in (acc <> c' <> bs', cs)

unconsBS :: List ByteString -> Tuple2 ByteString (List ByteString)
unconsBS (x : xs) = (x, xs)
unconsBS [] = ("", [])

genJson :: Gen ByteString
genJson =
  Gen.frequency
    [ (1, genJsonString),
      (1, genJsonBool),
      (1, genJsonNum),
      (4, genJsonObject),
      (3, genJsonArray)
    ]

genJsonObject :: Gen ByteString
genJsonObject = do
  kvs <- Gen.list (Range.linearFrom 0 0 6) genJsonObjectOne
  pure
    $ mconcat
      [ "{\n",
        BS.intercalate ",\n" ((\(k, v) -> k <> ": " <> v) <$> kvs),
        "\n}"
      ]
  where
    genJsonObjectOne :: Gen (Tuple2 ByteString ByteString)
    genJsonObjectOne = do
      Gen.recursive
        Gen.choice
        [ withKey genJsonString,
          withKey genJsonBool,
          withKey genJsonNum,
          withKey genJsonArray
        ]
        [ withKey genJsonObject
        ]

genJsonArray :: Gen ByteString
genJsonArray = do
  es <- Gen.list (Range.linearFrom 0 0 6) genArrayElement
  pure
    $ mconcat
      [ "[",
        BS.intercalate ", " es,
        "]"
      ]
  where
    genArrayElement :: Gen ByteString
    genArrayElement = do
      Gen.recursive
        Gen.choice
        [ genJsonString,
          genJsonBool,
          genJsonNum,
          genJsonObject
        ]
        [ genJsonArray
        ]

genJsonString :: Gen ByteString
genJsonString = withNull $ do
  txt <- genNonQuotedTxt (Range.linearFrom 1 1 50)
  pure $ quote (encodeUtf8 txt)

genJsonNum :: Gen ByteString
genJsonNum = withNull $ encodeUtf8 <$> TestUtils.genTextDoublePrecision True

genJsonBool :: Gen ByteString
genJsonBool = withNull $ Gen.element ["true", "false"]

genJsonKey :: Gen ByteString
genJsonKey = do
  txt <- genNonQuotedTxt (Range.linearFrom 1 1 10)
  pure $ quote (encodeUtf8 txt)

-- Might need to retrict control characters here
genNonQuotedTxt :: Range Int -> Gen Text
genNonQuotedTxt r = Gen.text r (Gen.filter (/= '\"') Gen.alphaNum)

withKey :: Gen ByteString -> Gen (Tuple2 ByteString ByteString)
withKey g = (,) <$> genJsonKey <*> g

withNull :: Gen ByteString -> Gen ByteString
withNull g =
  Gen.frequency
    [ (4, g),
      (1, pure "null")
    ]

quote :: (IsString a, Semigroup a) => a -> a
quote s = "\"" <> s <> "\""
