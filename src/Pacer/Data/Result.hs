module Pacer.Data.Result
  ( Result (..),
    ResultDefault,
    errorErr,
    failErr,
  )
where

import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.String (IsString (fromString))
import Pacer.Prelude

-- | General type for error handling, with convenient MonadFail instance.
data Result e a
  = Err e
  | Ok a
  deriving stock (Eq, Functor, Show)

type ResultDefault = Result String

instance Applicative (Result e) where
  pure = Ok

  Ok f <*> Ok x = Ok (f x)
  Err x <*> _ = Err x
  _ <*> Err x = Err x

instance Monad (Result e) where
  Ok x >>= f = f x
  Err x >>= _ = Err x

instance Foldable (Result e) where
  foldr _ e (Err _) = e
  foldr f e (Ok x) = f x e

instance Traversable (Result e) where
  sequenceA (Err x) = pure (Err x)
  sequenceA (Ok x) = Ok <$> x

  traverse _ (Err x) = pure (Err x)
  traverse f (Ok x) = Ok <$> f x

instance (IsString e) => MonadFail (Result e) where
  fail = Err . fromString

instance Bifunctor Result where
  bimap f _ (Err x) = Err (f x)
  bimap _ g (Ok x) = Ok (g x)

instance Bifoldable Result where
  bifoldMap f _ (Err x) = f x
  bifoldMap _ g (Ok x) = g x

instance Bitraversable Result where
  bitraverse f _ (Err a) = Err <$> f a
  bitraverse _ g (Ok b) = Ok <$> g b

errorErr :: (HasCallStack) => ResultDefault a -> a
errorErr (Ok x) = x
errorErr (Err err) = error err

failErr :: (MonadFail m) => ResultDefault a -> m a
failErr (Ok x) = pure x
failErr (Err err) = fail err
