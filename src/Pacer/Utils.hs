module Pacer.Utils
  ( -- * JSON
    encodeMaybe,
    encodeMaybes,

    -- * TOML
    getFieldOptArrayOf,

    -- * Show
    showtOsPath,
    showPath,
    showtPath,
    showListF,

    -- * Misc
    EitherString (..),
    PaceMetersErrMsg,
  )
where

import Data.Aeson (Key, KeyValue ((.=)), ToJSON)
import Data.Aeson.Types (Pair)
import FileSystem.OsPath qualified as OsPath
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder), Decoder)
import TOML qualified

getFieldOptArrayOf :: (DecodeTOML a) => Text -> Decoder (List a)
getFieldOptArrayOf =
  fmap (fromMaybe [])
    . TOML.getFieldOptWith (TOML.getArrayOf tomlDecoder)

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

-- | Either, specializing Left to String, for the purposes of MonadFail.
data EitherString a
  = EitherLeft String
  | EitherRight a
  deriving stock (Eq, Functor, Show)

instance Applicative EitherString where
  pure = EitherRight

  EitherRight f <*> EitherRight x = EitherRight (f x)
  EitherLeft x <*> _ = EitherLeft x
  _ <*> EitherLeft x = EitherLeft x

instance Monad EitherString where
  EitherRight x >>= f = f x
  EitherLeft x >>= _ = EitherLeft x

instance Foldable EitherString where
  foldr _ e (EitherLeft _) = e
  foldr f e (EitherRight x) = f x e

instance Traversable EitherString where
  sequenceA (EitherLeft x) = pure (EitherLeft x)
  sequenceA (EitherRight x) = EitherRight <$> x

  traverse _ (EitherLeft x) = pure (EitherLeft x)
  traverse f (EitherRight x) = EitherRight <$> f x

instance MonadFail EitherString where
  fail = EitherLeft
