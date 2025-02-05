module Pacer.Data.Result
  ( Result (..),
    errorErr,
    failErr,
  )
where

import Pacer.Prelude

-- TODO: Probably generalize this.

-- | General type for error handling, with convenient MonadFail instance.
-- Isomorphic to Either String.
data Result a
  = Err String
  | Ok a
  deriving stock (Eq, Functor, Show)

instance Applicative Result where
  pure = Ok

  Ok f <*> Ok x = Ok (f x)
  Err x <*> _ = Err x
  _ <*> Err x = Err x

instance Monad Result where
  Ok x >>= f = f x
  Err x >>= _ = Err x

instance Alternative Result where
  Ok x <|> _ = Ok x
  Err _ <|> Ok y = Ok y
  Err x <|> Err y
    | x == altId = Err y
    | otherwise = Err x

  empty = Err altId

altId :: String
altId = "<empty>"

instance Foldable Result where
  foldr _ e (Err _) = e
  foldr f e (Ok x) = f x e

instance Traversable Result where
  sequenceA (Err x) = pure (Err x)
  sequenceA (Ok x) = Ok <$> x

  traverse _ (Err x) = pure (Err x)
  traverse f (Ok x) = Ok <$> f x

instance MonadFail Result where
  fail = Err

errorErr :: (HasCallStack) => Result a -> a
errorErr (Ok x) = x
errorErr (Err err) = error err

failErr :: (MonadFail m) => Result a -> m a
failErr (Ok x) = pure x
failErr (Err err) = fail err
