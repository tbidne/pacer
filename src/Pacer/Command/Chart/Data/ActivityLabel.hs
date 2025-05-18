{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.ActivityLabel
  ( ActivityLabel (..),
    ActivityLabels (..),
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MP
import Pacer.Command.Chart.Data.Activity (Label)
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Prelude
import Pacer.Utils qualified as Utils

data ActivityLabel = MkActivityLabel
  { datetime :: Timestamp,
    labels :: NESet Label
  }
  deriving stock (Eq, Show)

instance FromJSON ActivityLabel where
  parseJSON = asnWithObject "ActivityLabel" $ \v -> do
    datetime <- v .: "datetime"
    labels <- v .: "labels"
    Utils.failUnknownFields "ActivityLabel" ["datetime", "labels"] v
    pure
      $ MkActivityLabel
        { datetime,
          labels
        }

newtype ActivityLabels = MkActivityLabels
  {unActivityLabels :: Map Timestamp (NESet Label)}
  deriving stock (Eq, Show)

instance FromJSON ActivityLabels where
  parseJSON = asnWithObject "ActivityLabels" $ \v -> do
    xs <- v .: "activity-labels"
    Utils.failUnknownFields "ActivityLabels" ["activity-labels"] v
    pure
      $ MkActivityLabels
        { unActivityLabels =
            MP.fromList $ fmap (\(MkActivityLabel r l) -> (r, l)) xs
        }

makeFieldLabelsNoPrefix ''ActivityLabel
makeFieldLabelsNoPrefix ''ActivityLabels
