{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides common units functionality.
module Running.Class.Units
  ( Units (..),
    singFactor,
  )
where

import Running.Class.Singleton
  ( SingI (..),
    SingKind (Demote, fromSing),
  )
import Running.Prelude

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
  baseFactor :: (FromInteger b) => a -> b

-- | Retrieves the 'baseFactor' from the singleton witness.
singFactor ::
  forall k (a :: k) b.
  (FromInteger b, Units (Demote k), SingI a, SingKind k) =>
  b
singFactor = baseFactor $ fromSing (sing @a)
