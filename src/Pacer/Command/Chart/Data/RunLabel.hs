{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.RunLabel
  ( RunLabel (..),
    RunLabels (..),
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MP
import Data.Set.NonEmpty qualified as NESet
import Pacer.Command.Chart.Data.Time (Timestamp)
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder))
import TOML qualified

data RunLabel = MkRunLabel
  { datetime :: Timestamp,
    labels :: NESet Text
  }
  deriving stock (Eq, Show)

instance DecodeTOML RunLabel where
  tomlDecoder = do
    datetime <- TOML.getFieldWith tomlDecoder "datetime"
    labelsList <- TOML.getFieldWith (TOML.getArrayOf tomlDecoder) "labels"

    labels <- case labelsList of
      [] -> fail $ "Received empty labels for run: " ++ show datetime
      (l : ls) -> pure $ NESet.fromList (l :| ls)

    pure
      $ MkRunLabel
        { datetime,
          labels
        }

newtype RunLabels = MkRunLabels {unRunLabels :: Map Timestamp (NESet Text)}
  deriving stock (Eq, Show)

instance DecodeTOML RunLabels where
  tomlDecoder = do
    xs <- TOML.getFieldWith (TOML.getArrayOf @RunLabel tomlDecoder) "run-labels"

    pure
      $ MkRunLabels
        { unRunLabels = MP.fromList $ fmap (\(MkRunLabel r l) -> (r, l)) xs
        }

makeFieldLabelsNoPrefix ''RunLabel
makeFieldLabelsNoPrefix ''RunLabels
