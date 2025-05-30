module Pacer.Class.FromAlt
  ( FromAlt (..),
    isNonEmpty,
    asum1M,
    alt1M,
    (<+<|>+>),
  )
where

import Data.Foldable qualified as F
import Data.Functor.Identity (Identity (Identity))
import Data.Maybe qualified as M
import Data.Sequence qualified as Seq
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

  listToAlt :: List a -> f a

isNonEmpty :: (FromAlt f) => f a -> Bool
isNonEmpty = not . isEmpty

instance FromAlt Maybe where
  type Alt1 Maybe = Identity

  isEmpty = F.null
  toAlt1 = fmap Identity

  listToAlt = M.listToMaybe

instance FromAlt List where
  type Alt1 List = NonEmpty

  isEmpty = F.null
  toAlt1 [] = Nothing
  toAlt1 (x : xs) = Just (x :| xs)

  listToAlt = identity

instance FromAlt Seq where
  type Alt1 Seq = NESeq

  isEmpty = F.null

  toAlt1 Seq.Empty = Nothing
  toAlt1 (x :<| xs) = Just (x :<|| xs)

  listToAlt = Seq.fromList

asum1M :: forall t m f a. (Foldable t, FromAlt f, Monad m) => t (m (f a)) -> m (f a)
asum1M = foldr alt1M (pure empty)

-- Like '(<|>)', except utilizes the monad constraint to avoid effects run
-- in the RHS if the LHS is non-empty.
alt1M :: forall m f a. (FromAlt f, Monad m) => m (f a) -> m (f a) -> m (f a)
alt1M mx my = do
  x <- mx
  if isNonEmpty x
    then mx
    else my

-- | Operator for 'alt1M'.
(<+<|>+>) :: forall m f a. (FromAlt f, Monad m) => m (f a) -> m (f a) -> m (f a)
(<+<|>+>) = alt1M
