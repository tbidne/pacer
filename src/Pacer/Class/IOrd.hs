module Pacer.Class.IOrd
  ( -- * IEq
    IEq (..),
    (/~),

    -- * IOrd
    IOrd (..),
    (>~),
    (<.),
    (>.),
    imin,
    imax,
  )
where

import Pacer.Prelude

-- | Class for "intransitive equals" i.e. equality that is not transitive.
class IEq a where
  -- | Equals that is not necessarily transitive. Required laws are:
  --
  -- * Reflexivity: @x ~~ x@.
  -- * Symmetry: @x ~~ y <=> y ~~ x@.
  -- * Negation: @x /~ y <=> not (x ~~ y)@.
  (~~) :: a -> a -> Bool

infix 4 ~~

-- | Not equal-to.
(/~) :: (IEq a) => a -> a -> Bool
(/~) x = not . (~~) x

infix 4 /~

-- | Class for "instransitive order" i.e. order that is not transitive.
class (IEq a) => IOrd a where
  -- | (<=) that is not necessarily transitive. Required laws are:
  --
  -- * Comparability: @x <~ y <=> y <~ x@.
  -- * Reflexivity: @x <~ x@.
  -- * Anti-symmetry: @x <~ y and y <~ x ==> x ~~ y@.
  -- * Operator coherence.
  (<~) :: a -> a -> Bool

infix 4 <~

-- | Strict less-than.
(<.) :: (IOrd a) => a -> a -> Bool
x <. y = x <~ y && x /~ y

infix 4 <.

-- | Greater-than or equal-to.
(>~) :: (IOrd a) => a -> a -> Bool
x >~ y = y <~ x

infix 4 >~

-- | Strict greater-than.
(>.) :: (IOrd a) => a -> a -> Bool
x >. y = y <~ x && x /~ y

-- | Min for 'IOrd'.
imin :: (IOrd a) => a -> a -> a
imin x y = if x <~ y then x else y

-- | Max for 'IOrd'.
imax :: (IOrd a) => a -> a -> a
imax x y = if x >~ y then x else y
