module Pacer.Utils
  ( -- * JSON
    encodeMaybe,
    encodeMaybes,

    -- * TOML
    getFieldOptArrayOf,

    -- * Misc
    PaceMetersErrMsg,
  )
where

import Data.Aeson (Key, KeyValue ((.=)), ToJSON)
import Data.Aeson.Types (Pair)
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder), Decoder)
import TOML qualified

getFieldOptArrayOf :: (DecodeTOML a) => Text -> Decoder (List a)
getFieldOptArrayOf =
  fmap (fromMaybe [])
    . TOML.getFieldOptWith (TOML.getArrayOf tomlDecoder)

encodeMaybes :: (ToJSON v) => [Tuple2 Key (Maybe v)] -> [Pair]
encodeMaybes = (>>= encodeMaybe)

encodeMaybe :: (ToJSON v) => Tuple2 Key (Maybe v) -> [Pair]
encodeMaybe (_, Nothing) = []
encodeMaybe (k, Just v) = [k .= v]

type PaceMetersErrMsg = "Meters are disallowed in Pace; use km or mi."
