module Pacer.Utils
  ( -- * JSON
    encodeMaybe,
    encodeMaybes,

    -- * TOML
    getFieldOptArrayOf,
    getFieldOptArrayOfWith,

    -- * Show
    showtOsPath,
    showPath,
    showtPath,
    showListF,

    -- * Seq
    seqGroupBy,

    -- * Misc
    PaceMetersErrMsg,
  )
where

import Data.Aeson (Key, KeyValue ((.=)), ToJSON)
import Data.Aeson.Types (Pair)
import Data.Sequence qualified as Seq
import FileSystem.OsPath qualified as OsPath
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder), Decoder)
import TOML qualified

getFieldOptArrayOf :: (DecodeTOML a) => Text -> Decoder (List a)
getFieldOptArrayOf = getFieldOptArrayOfWith tomlDecoder

getFieldOptArrayOfWith :: Decoder a -> Text -> Decoder (List a)
getFieldOptArrayOfWith decoder =
  fmap (fromMaybe [])
    . TOML.getFieldOptWith (TOML.getArrayOf decoder)

encodeMaybes :: (ToJSON v) => List (Tuple2 Key (Maybe v)) -> List Pair
encodeMaybes = (>>= encodeMaybe)

encodeMaybe :: (ToJSON v) => Tuple2 Key (Maybe v) -> List Pair
encodeMaybe (_, Nothing) = []
encodeMaybe (k, Just v) = [k .= v]

type PaceMetersErrMsg = "Meters are disallowed in Pace; use km or mi."

showtOsPath :: OsPath -> Text
showtOsPath = packText . OsPath.decodeLenient

showPath :: Path b t -> String
showPath = OsPath.decodeLenient . pathToOsPath

showtPath :: Path b t -> Text
showtPath = showtOsPath . pathToOsPath

showListF :: (IsString b, Semigroup b) => (a -> b) -> List a -> b
showListF _ [] = "[]"
showListF f xs@(_ : _) = "[" <> go xs
  where
    go [] = "]"
    go [y] = f y <> "]"
    go (y : ys) = f y <> ", " <> go ys

seqGroupBy :: forall a. (a -> a -> Bool) -> Seq a -> Seq (NESeq a)
seqGroupBy p = go
  where
    go :: Seq a -> Seq (NESeq a)
    go Seq.Empty = Seq.Empty
    go (x :<| xs) = (x :<|| ys) :<| go zs
      where
        (ys, zs) = Seq.spanl (p x) xs
