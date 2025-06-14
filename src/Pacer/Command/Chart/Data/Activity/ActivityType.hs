{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.Activity.ActivityType
  ( ActivityType (..),
  )
where

import Pacer.Class.Parser (Parser)
import Pacer.Prelude
import Pacer.Utils.Json (FromJSON, ToJSON)

-- | Activity type.
newtype ActivityType = MkActivityType {unActivityType :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (Parser, NFData)
  deriving newtype (Display, Eq, FromJSON, IsString, Ord, ToJSON)

makeFieldLabelsNoPrefix ''ActivityType
