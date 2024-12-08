module Pacer.Config.Data
  ( Either3 (..),
  )
where

import Pacer.Prelude

data Either3 a b c
  = Either1 a
  | Either2 b
  | Either3 c
  deriving stock (Eq, Show)
