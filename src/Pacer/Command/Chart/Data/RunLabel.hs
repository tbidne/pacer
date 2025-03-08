{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.RunLabel
  ( RunLabel (..),
    RunLabels (..),
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MP
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Prelude
import Pacer.Utils qualified as Utils

data RunLabel = MkRunLabel
  { datetime :: Timestamp,
    labels :: NESet Text
  }
  deriving stock (Eq, Show)

instance FromJSON RunLabel where
  parseJSON = asnWithObject "RunLabel" $ \v -> do
    datetime <- v .: "datetime"
    labels <- v .: "labels"
    Utils.failUnknownFields "RunLabel" ["datetime", "labels"] v
    pure
      $ MkRunLabel
        { datetime,
          labels
        }

newtype RunLabels = MkRunLabels {unRunLabels :: Map Timestamp (NESet Text)}
  deriving stock (Eq, Show)

instance FromJSON RunLabels where
  parseJSON = asnWithObject "RunLabels" $ \v -> do
    xs <- v .: "run-labels"
    Utils.failUnknownFields "RunLabels" ["run-labels"] v
    pure
      $ MkRunLabels
        { unRunLabels = MP.fromList $ fmap (\(MkRunLabel r l) -> (r, l)) xs
        }

makeFieldLabelsNoPrefix ''RunLabel
makeFieldLabelsNoPrefix ''RunLabels
