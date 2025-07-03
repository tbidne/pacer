-- | Provides Json utilities. Because consistency is tricky when we are using
-- mixing manual decoding with FromJSON instances, this module should be
-- preferred over aeson, as we attempt to provide consistent functions here
-- (e.g. errors should include some sort of location info, which is not
-- guaranteed with all aeson decode functions).
module Pacer.Utils.Json
  ( -- * Encoding
    encodeMaybe,
    encodeMonoid,
    encodeMaybes,
    encodePretty,

    -- ** Re-exports
    ToJSON (..),
    KeyValue (..),
    Asn.object,

    -- * Decoding

    -- ** Parser
    JsonParser,
    JsonValue,
    (.:?:),
    AsnT.explicitParseField,
    AsnT.listParser,
    failUnknownFields,

    -- *** Re-exports
    FromJSON (..),
    (.:),
    (.:?),
    Asn.withObject,
    Asn.withText,

    -- ** Result
    parseResult,

    -- ** ByteString
    decodeJson,
    decodeJsonP,

    -- ** Files
    readDecodeJson,
    readDecodeJsonP,

    -- * Errors
    AesonE (..),
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    Key,
    KeyValue ((.=)),
    ToJSON (toJSON),
    (.:),
    (.:?),
    (<?>),
  )
import Data.Aeson qualified as Asn
import Data.Aeson.Encode.Pretty
  ( Config (confIndent, confTrailingNewline),
    Indent (Spaces),
  )
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (JSONPathElement (Key), Pair)
import Data.Aeson.Types qualified as AsnT
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.String (IsString (fromString))
import FileSystem.OsPath (decodeLenient)
import GHC.IsList (IsList (Item))
import GHC.IsList qualified as IL
import Pacer.Class.Parser qualified as P
import Pacer.Prelude

type JsonParser = AsnT.Parser

type JsonValue = AsnT.Value

-- | Encodes 'Maybe's to a possibly empty list.
encodeMaybes :: (ToJSON v) => List (Tuple2 Key (Maybe v)) -> List Pair
encodeMaybes = (>>= encodeMaybe)

-- | Encodes a 'Monoid' to a possibly empty list.
encodeMonoid :: (Eq v, Monoid v, ToJSON v) => Tuple2 Key v -> List Pair
encodeMonoid = encodeMaybe . second toMaybe
  where
    toMaybe x
      | x == mempty = Nothing
      | otherwise = Just x

-- | Encodes a 'Maybe' to a possibly empty list.
encodeMaybe :: (ToJSON v) => Tuple2 Key (Maybe v) -> List Pair
encodeMaybe (_, Nothing) = []
encodeMaybe (k, Just v) = [k .= v]

-- | We use this rather than aeson's AesonException for two reasons:
--
-- 1. AesonException does not have an NFData instance, which we require for
--    the benchmarks.
--
-- 2. Optional path improves the error message.
data AesonE = MkAesonE (Maybe OsPath) String
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Exception AesonE where
  displayException (MkAesonE mPath s) =
    case mPath of
      Just p ->
        mconcat
          [ "Error decoding json path '",
            decodeLenient p,
            "': ",
            s
          ]
      Nothing -> "Error decoding json: " ++ s

instance IsString AesonE where
  fromString = MkAesonE Nothing

-- | Decodes a json(c) file.
readDecodeJson ::
  forall a es.
  ( FileReader :> es,
    FromJSON a,
    HasCallStack
  ) =>
  Path Abs File ->
  Eff es a
readDecodeJson = readDecodeJsonP parseJSON

-- | Decodes a json(c) file using the explicitly given parser.
readDecodeJsonP ::
  forall a es.
  ( FileReader :> es,
    HasCallStack
  ) =>
  (Asn.Value -> AsnT.Parser a) ->
  Path Abs File ->
  Eff es a
readDecodeJsonP p path = do
  contents <- readBinaryFile osPath
  throwErr
    $ first toAesonPathE
    $ decodeJsonP @a p contents
  where
    osPath = toOsPath path
    toAesonPathE (MkAesonE _ s) = MkAesonE (Just osPath) s

-- | Fails if there are any unknown fields in the object.
failUnknownFields ::
  -- | Key label to improve error message.
  Key ->
  -- | Known keys.
  List Key ->
  -- | Actual keys.
  KeyMap Asn.Value ->
  AsnT.Parser Unit
failUnknownFields name knownKeys kmap = do
  case HSet.toList unknownKeys of
    [] -> pure ()
    unknownKeyList@(_ : _) ->
      fail
        ( mconcat
            [ "Encountered unknown keys: ",
              showKeys unknownKeyList
            ]
        )
        <?> Key name
  where
    actualKeyMap = KM.toHashMap kmap
    actualKeySet = HMap.keysSet actualKeyMap

    knownKeySet = HSet.fromList knownKeys
    unknownKeys = HSet.difference actualKeySet knownKeySet

    showKeys :: List Key -> String
    showKeys = show . L.sort . fmap Key.toString

-- | Decodes json(c).
decodeJson :: (FromJSON a) => ByteString -> Result AesonE a
decodeJson = decodeJsonP parseJSON

-- | Decodes json(c) using the explicitly given parser.
decodeJsonP :: (Asn.Value -> AsnT.Parser a) -> ByteString -> Result AesonE a
decodeJsonP p bs = do
  -- Strip comments.
  bs' <- first mkAesonE $ P.stripComments bs
  first mkAesonE $ review #eitherIso $ do
    -- Decode to Value first. Obviously this is not the most efficient thing in
    -- the world.
    v <- Asn.eitherDecodeStrict @Asn.Value bs'
    -- parseEither over parse, as the former formats the error message i.e.
    -- includes location info, whereas parse does not.
    AsnT.parseEither p v
  where
    mkAesonE = MkAesonE Nothing

-- | Runs the parser on the value, lifting the result to Either. Uses
-- aeson's error formatter i.e. error locations are included, so this
-- should be preferred to direct aeson usage.
parseResult :: (Asn.Value -> AsnT.Parser a) -> Asn.Value -> ResultDefault a
parseResult p = review #eitherIso . AsnT.parseEither p

-- | Decodes a json list into an 'IsList'. Like '(.:?)', omitted keys are fine
-- and decode to an empty list.
(.:?:) ::
  forall l.
  (FromJSON (Item l), IsList l) =>
  Asn.Object ->
  Key ->
  AsnT.Parser l
(.:?:) o k = do
  o .:? k <&> \case
    Nothing -> IL.fromList []
    Just xs -> IL.fromList xs

infixl 9 .:?:

encodePretty :: (ToJSON a) => a -> LazyByteString
encodePretty = AsnPretty.encodePretty' cfg
  where
    cfg =
      AsnPretty.defConfig
        { confIndent = Spaces 2,
          confTrailingNewline = True
        }
