module Pacer.Data.Result
  ( Result (..),
    ResultDefault,

    -- * Construction
    fromEither,

    -- * Elimination
    onResult,
    onErr,
    onOk,
    errorErr,
    failErr,
    throwErr,
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.String (IsString (fromString))
import GHC.Stack (HasCallStack)
import Prelude

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
errorErr = onErr error

failErr :: (MonadFail m) => ResultDefault a -> m a
failErr = onResult fail pure

throwErr ::
  ( Exception e,
    HasCallStack,
    MonadThrow m
  ) =>
  Result e a -> m a
throwErr = onResult throwM pure

onResult :: (e -> b) -> (a -> b) -> Result e a -> b
onResult f _ (Err err) = f err
onResult _ g (Ok x) = g x

onErr :: (e -> a) -> Result e a -> a
onErr f = onResult f id

onOk :: (a -> e) -> Result e a -> e
onOk g = onResult id g

fromEither :: Either e a -> Result e a
fromEither = either Err Ok
