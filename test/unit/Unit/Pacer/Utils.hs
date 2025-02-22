module Unit.Pacer.Utils (tests) where

import Data.Aeson (Value)
import Data.Aeson qualified as Asn
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pacer.Utils qualified as Utils
import Unit.Prelude
import Unit.TestUtils qualified as TestUtils

tests :: TestTree
tests =
  testGroup
    "Pacer.Utils"
    [ testJsoncDecode
    ]

testJsoncDecode :: TestTree
testJsoncDecode = testPropMaxN 10_000 "testJsoncDecode" "Decodes jsonc" $ do
  (json, jsonc) <- forAll genJsonAndJsonc

  case review #eitherIso (Asn.eitherDecodeStrict @Value json) of
    Err asnErr -> do
      annotate "Generated invalid json; this should never happen."
      annotate asnErr
      failure
    Ok asnResult ->
      case Utils.decodeJson @Value jsonc of
        Err utilsResult -> do
          annotate "Json is valid but failed to parse jsonc."
          annotate (displayException utilsResult)
          failure
        Ok utilsResult -> do
          annotate "Jsonc decoded but differed from json decoding"
          asnResult === utilsResult

genJsonAndJsonc :: Gen (Tuple2 ByteString ByteString)
genJsonAndJsonc = do
  json <- genJson
  let jsonLines = C8.lines json
  lineComments <- Gen.list (Range.linearFrom 0 0 10) TestUtils.genLineComment
  blockComments <- Gen.list (Range.linearFrom 0 0 10) TestUtils.genBlockComment
  jsonc <- mconcat <$> Gen.shuffle (jsonLines ++ lineComments ++ blockComments)
  pure (json, jsonc)

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
      [ "{ ",
        BS.intercalate "," ((\(k, v) -> k <> ": " <> v) <$> kvs),
        " }"
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
        BS.intercalate "," es,
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
