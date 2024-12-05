{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides singleton functionality. This is essentially the singletons
-- library, with the functionality we need vendored.
module Running.Class.Singleton
  ( -- * Primary class
    SingI (..),
    Sing,
    withSingI,

    -- * Promotion / Demotion
    SingKind (..),
    SomeSing (..),
    fromSingI,
  )
where

import GHC.Exts (withDict)
import Running.Prelude

-- | Sing type family.
type Sing :: k -> Type
type family Sing @k :: k -> Type

-- | Singleton class.
class SingI s where
  -- | Retrieves the singleton witness for case analysis.
  sing :: Sing s

-- | Helpful pattern for calling a singleton-constrained function with a
-- term-level witness.
withSingI :: Sing n -> ((SingI n) => r) -> r
withSingI sn r =
  case singInstance sn of
    SingInstance -> r

-- | GADT for implementing the 'withSingI' convenience function.
type SingInstance :: k -> Type
data SingInstance (a :: k) where
  SingInstance :: (SingI a) => SingInstance a

-- | Used for implementing 'withSingI'.
singInstance :: forall k (a :: k). Sing a -> SingInstance a
singInstance s = withSingI' SingInstance
  where
    withSingI' :: ((SingI a) => SingInstance a) -> SingInstance a
    withSingI' = withDict @(SingI a) @(Sing a) s

-- | Existentially quantifies a singleton.
type SomeSing :: Type -> Type
data SomeSing k where
  MkSomeSing :: Sing (a :: k) -> SomeSing k

-- | Class for singleton promotion/demotion.
class SingKind k where
  type Demote k = (r :: Type) | r -> k

  -- | Demotes a singleton to its term-level equivalent.
  fromSing :: Sing (a :: k) -> Demote k

  -- | Promotes a term to its type-level singleton equivalent.
  toSing :: Demote k -> SomeSing k

-- | Convenience function for retrieving the demoted value from a type
-- parameter.
fromSingI :: forall k (a :: k). (SingI a, SingKind k) => Demote k
fromSingI = fromSing @k (sing @a)
