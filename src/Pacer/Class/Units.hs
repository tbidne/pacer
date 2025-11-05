{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides common units functionality.
module Pacer.Class.Units
  ( Units (..),
    singFactor,
  )
where

import Pacer.Prelude

-- | Class for units.
class Units a where
  -- | Retrieve the factor relative to the base unit i.e. for some quantity
  -- @x@ and unit @u@,
  --
  -- @
  --   x u === x * (baseFactor u) base
  -- @
  --
  -- For instance, @baseFactor Kilometer === 1_000@, since
  --
  -- @
  --   x kilometers === x * 1_000 meters
  -- @
  baseFactor :: (Fromℤ b) => a -> b

-- | Retrieves the 'baseFactor' from the singleton witness.
singFactor ::
  forall k (a :: k) b.
  (Fromℤ b, Units (Demote k), SingI a, SingKind k) =>
  b
singFactor @_ @a = baseFactor $ fromSing (sing @a)
