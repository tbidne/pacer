-- | Provides filters over generic set.
module Pacer.Command.Chart.Data.Expr.Filter.Set
  ( -- * FilterSet
    FilterSet (..),
    FilterSetOpElem (..),
    FilterSetOpSet (..),
    applyFilterSet,
    compFun,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Pacer.Class.Parser (MParser, Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Expr.Eq (FilterOpEq)
import Pacer.Command.Chart.Data.Expr.Eq qualified as Eq
import Pacer.Command.Chart.Data.Expr.Filter.Utils qualified as Filter.Utils
import Pacer.Prelude
import Text.Megaparsec ((<?>))
import Text.Megaparsec.Char qualified as MPC

-- See NOTE: [Filter negative operators].

-------------------------------------------------------------------------------
--                                 FilterSet                                 --
-------------------------------------------------------------------------------

-- | @FilterSet X@ compares a LHS set against RHS element or set.
type FilterSet :: Symbol -> Type -> Type
data FilterSet p a
  = -- | Set membership.
    FilterSetHasElem FilterSetOpElem a
  | -- | Set comparisons e.g. @labels ⊇ {label_1, label_2}@. This tests that
    -- all labels exist within labels ("AND").
    FilterSetComp FilterSetOpSet (Set a)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance (Display a, Eq a, KnownSymbol s) => Display (FilterSet s a) where
  displayBuilder :: forall t. (KnownSymbol t) => FilterSet t a -> Builder
  displayBuilder @t = \case
    FilterSetHasElem op t ->
      mconcat
        [ symsStrSpc,
          displayBuilder op,
          " ",
          displayBuilder t
        ]
    FilterSetComp op set ->
      mconcat
        [ symsStrSpc,
          displayBuilder op,
          " ",
          Filter.Utils.displaySet set
        ]
    where
      symStr = displayBuilder $ symbolVal (Proxy :: Proxy t)
      symsStrSpc = symStr <> "s "

-------------------------------------------------------------------------------
--                                   Parsing                                 --
-------------------------------------------------------------------------------

instance (KnownSymbol s, Ord a, Parser a) => Parser (FilterSet s a) where
  parser :: (KnownSymbol t) => MParser (FilterSet t a)
  parser @t = parseFilterSet symsStr <?> symsStr
    where
      symStr = symbolVal (Proxy :: Proxy t)
      symsStr = symStr <> "s"

parseFilterSet :: (Ord a, Parser a) => String -> MParser (FilterSet s a)
parseFilterSet symsStr = do
  void $ MPC.string symTxt
  MPC.space
  asum
    [ parseOne,
      parseMany
    ]
  where
    symTxt = T.pack symsStr

    parseOne = do
      op <- parser
      MPC.space
      txt <- Filter.Utils.parseSetElem
      pure $ FilterSetHasElem op txt

    parseMany = do
      op <- parser
      MPC.space
      set <- Filter.Utils.parseSet symsStr
      pure $ FilterSetComp op set

-------------------------------------------------------------------------------
--                                  Operators                                --
-------------------------------------------------------------------------------

-- | Operatoror for LHS set and RHS element.
data FilterSetOpElem
  = -- | Membership.
    FilterSetContainsElem
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterSetOpElem where
  displayBuilder FilterSetContainsElem = "∋"

instance Parser FilterSetOpElem where
  parser =
    asum
      [ P.char '∋' $> FilterSetContainsElem,
        -- While plural is slightly jarring given that the set name is plural
        -- ("labels includes ..."), it is consistent with intersects
        -- ("labels intersect ..." does not sound right), and singular arguably
        -- makes sense because sets are really a single entity ("set X"),
        -- so the fact that the name X may itself be pluralized does not
        -- change the singular/plural distinction.
        P.string "includes" $> FilterSetContainsElem
      ]

-- | Operator for set comparisons.
data FilterSetOpSet
  = -- | Activity set X must equal the given set.
    FilterSetOpSetEq FilterOpEq
  | -- | Activity set X must be a proper superset of the given set.
    FilterSetOpSetPSuper
  | -- | Activity set X must be a superset of the given set.
    FilterSetOpSetSuper
  | -- | Activity set X must be a proper subset of the given set.
    FilterSetOpSetPSub
  | -- | Activity set X must be a subset of given set.
    FilterSetOpSetSub
  | -- | Activity set X must intersect the given set.
    FilterSetOpSetIntersects
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- See NOTE: [Operators]
--
-- For Super(sub)set operators, there are three choices. Pros/cons for
-- superset, proper superset:
--
-- 1. ⊇, ⊃
--
--   Pros:
--     - Consistent w/ inequalities (≥, >).
--     - Visually distinct.
--
--   Cons:
--     - Clashes with historical meaning of ⊃.
--
-- 2. ⊇, ⊋
--
--   Pros:
--     - Unambiguous.
--
--   Cons:
--     - Inconsistent with inequalities.
--     - Visually confusing.
--
-- 3. ⊃, ⊋
--
--   Pros:
--     - Consistent w/ historial meaning of ⊃.
--     - Visually distinct.
--
--   Cons:
--     - Inconsistent with inequalities.
--     - Very idiosyncratic.
--
-- 1 is clearly the most logical, with the only downside being the clash with
-- historical ⊃.
--
-- 2 is okay, but it is hard to read at a glance.
--
-- 3 is madness, sure to confuse.
--
-- We go with 1, due to the clear analogy with inequalities, and that we
-- explain it in the faq. Also John Baez says it is becoming more popular
-- :-).
--
-- https://math.stackexchange.com/a/581820

instance Display FilterSetOpSet where
  displayBuilder = \case
    FilterSetOpSetEq op -> displayBuilder op
    FilterSetOpSetPSuper -> "⊃"
    FilterSetOpSetSuper -> "⊇"
    FilterSetOpSetPSub -> "⊂"
    FilterSetOpSetSub -> "⊆"
    FilterSetOpSetIntersects -> "∩"

instance Parser FilterSetOpSet where
  parser =
    asum
      [ parser <&> FilterSetOpSetEq,
        P.string ">=" $> FilterSetOpSetSuper,
        P.char '⊇' $> FilterSetOpSetSuper,
        P.char '>' $> FilterSetOpSetPSuper,
        P.char '⊃' $> FilterSetOpSetPSuper,
        P.string "<=" $> FilterSetOpSetSub,
        P.char '⊆' $> FilterSetOpSetSub,
        P.char '<' $> FilterSetOpSetPSub,
        P.char '⊂' $> FilterSetOpSetPSub,
        P.char '∩' $> FilterSetOpSetIntersects,
        P.string "intersects" $> FilterSetOpSetIntersects
      ]

-------------------------------------------------------------------------------
--                                 Functions                                 --
-------------------------------------------------------------------------------

applyFilterSet :: forall b p. (Ord b) => Set b -> FilterSet p b -> Bool
applyFilterSet set = \case
  FilterSetHasElem op t -> hasElemFun op set t
  FilterSetComp op t -> compFun op set t

-- LHS always set i.e. FilterSet functions.

hasElemFun :: (Ord a) => FilterSetOpElem -> Set a -> a -> Bool
hasElemFun FilterSetContainsElem = flip Set.member

compFun :: (Ord b) => FilterSetOpSet -> (Set b -> Set b -> Bool)
compFun (FilterSetOpSetEq op) = Eq.toFun op
compFun FilterSetOpSetSuper = isSuperSetOf
compFun FilterSetOpSetPSuper = isProperSuperSetOf
compFun FilterSetOpSetSub = Set.isSubsetOf
compFun FilterSetOpSetPSub = Set.isProperSubsetOf
compFun FilterSetOpSetIntersects = \ls rs -> Set.intersection ls rs /= mempty

isSuperSetOf :: forall a. (Ord a) => Set a -> Set a -> Bool
isSuperSetOf = flip Set.isSubsetOf

isProperSuperSetOf :: forall a. (Ord a) => Set a -> Set a -> Bool
isProperSuperSetOf = flip Set.isProperSubsetOf
