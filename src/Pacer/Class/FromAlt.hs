module Pacer.Class.FromAlt
  ( FromAlt (..),
    isNonEmpty,
  )
where

import Data.Foldable qualified as F
import Data.Functor.Identity (Identity (Identity))
import Pacer.Prelude

-- | Extension to 'Alternative' class.
class (Alternative f) => FromAlt f where
  -- | Non-empty type.
  type Alt1 f :: Type -> Type

  -- | Determines if the type is empty. The advantage over
  -- @== 'empty'@ is that the latter requires an Eq a constraint. Should
  -- always agree w/ 'empty'.
  isEmpty :: f a -> Bool

  -- | Map to the non-empty type.
  toAlt1 :: f a -> Maybe (Alt1 f a)

isNonEmpty :: (FromAlt f) => f a -> Bool
isNonEmpty = not . isEmpty

instance FromAlt Maybe where
  type Alt1 Maybe = Identity

  isEmpty = F.null
  toAlt1 = fmap Identity

instance FromAlt List where
  type Alt1 List = NonEmpty

  isEmpty = F.null
  toAlt1 [] = Nothing
  toAlt1 (x : xs) = Just (x :| xs)
