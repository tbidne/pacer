{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.Activity.ActivityLabel
  ( Label (..),
    ActivityLabel (..),
    ActivityLabels (..),
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MP
import Pacer.Class.Parser (Parser)
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Prelude
import Pacer.Utils.Json (FromJSON (parseJSON), ToJSON, (.:))
import Pacer.Utils.Json qualified as Json

-- | Activity label.
newtype Label = MkLabel {unLabel :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (Parser, NFData)
  deriving newtype (Display, Eq, FromJSON, IsString, Ord, ToJSON)

-- | Label defined in an external file, tied to a timestamp.
data ActivityLabel = MkActivityLabel
  { datetime :: Timestamp,
    labels :: NESet Label
  }
  deriving stock (Eq, Show)

instance FromJSON ActivityLabel where
  parseJSON = Json.withObject "ActivityLabel" $ \v -> do
    datetime <- v .: "datetime"
    labels <- v .: "labels"
    Json.failUnknownFields "ActivityLabel" ["datetime", "labels"] v
    pure
      $ MkActivityLabel
        { datetime,
          labels
        }

newtype ActivityLabels = MkActivityLabels
  {unActivityLabels :: Map Timestamp (NESet Label)}
  deriving stock (Eq, Show)

instance FromJSON ActivityLabels where
  parseJSON = Json.withObject "ActivityLabels" $ \v -> do
    xs <- v .: "activity-labels"
    Json.failUnknownFields "ActivityLabels" ["activity-labels"] v
    pure
      $ MkActivityLabels
        { unActivityLabels =
            MP.fromList $ fmap (\(MkActivityLabel r l) -> (r, l)) xs
        }

makeFieldLabelsNoPrefix ''ActivityLabel
makeFieldLabelsNoPrefix ''ActivityLabels
