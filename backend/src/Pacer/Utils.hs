module Pacer.Utils
  ( -- * TOML
    getFieldOptArrayOf,
  )
where

import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder), Decoder)
import TOML qualified

getFieldOptArrayOf :: (DecodeTOML a) => Text -> Decoder (List a)
getFieldOptArrayOf =
  fmap (fromMaybe [])
    . TOML.getFieldOptWith (TOML.getArrayOf tomlDecoder)
