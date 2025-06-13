-- | Provides filters over generic set.
module Pacer.Command.Chart.Data.Expr.Set
  ( -- * FilterElem
    FilterElem (..),
    FilterElemOpSet (..),
    memberFun,
    applyFilterElem,

    -- * FilterSet
    FilterSet (..),
    FilterSetOpElem (..),
    FilterSetOpSet (..),
    applyFilterSet,
    compFun,

    -- * Misc
    parseTextNonEmpty,
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
import Pacer.Prelude
import Pacer.Utils.Show qualified as Utils.Show
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- NOTE: While we could (and did, at one point) include negation for some
-- operators, we choose to only allow "not equals" (/=). Examples of
-- negative operators we currently do not allow:
--
--   ∉, ∌, ⊈, etc.
--
-- We do this for:
--
--   - Consistency: Not all operators have a negative variant (intersects),
--     and others look poor (≯).
--
--   - Avoid awkward words: We generally try to include a familiar operator
--     (<= for ⊆) or english word ("includes" for ∋), and this can lead to
--     awkward phrasing (what is the english for ∌? "does_not_include"?)

-------------------------------------------------------------------------------
--                                 FilterElem                                --
-------------------------------------------------------------------------------

-- | @FilterSet X@ compares a LHS element against RHS element or set e.g.
--
-- - @x = y@
-- - @x ∈ Y@
--
-- For instance:
--
-- - @"type = some_type"@
-- - @"type ∈ {t1, t2}"@
type FilterElem :: Symbol -> Type -> Type
data FilterElem p a
  = -- | Equality test.
    FilterElemEq FilterOpEq a
  | -- | Membership test.
    FilterElemExists FilterElemOpSet (Set a)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance (Display a, Eq a, KnownSymbol s) => Display (FilterElem s a) where
  displayBuilder = \case
    FilterElemEq op t ->
      mconcat
        [ symStr,
          " ",
          displayBuilder op,
          " ",
          displayBuilder t
        ]
    FilterElemExists op set ->
      mconcat
        [ symStr,
          " ",
          displayBuilder op,
          " ",
          displaySet set
        ]
    where
      symStr = displayBuilder $ symbolVal (Proxy :: Proxy s)

-------------------------------------------------------------------------------
--                                  Parsing                                  --
-------------------------------------------------------------------------------

instance (KnownSymbol s, Ord a, Parser a) => Parser (FilterElem s a) where
  parser = parseFilterElem symStr <?> symStr
    where
      symStr = symbolVal (Proxy :: Proxy s)

parseFilterElem :: (Ord a, Parser a) => String -> MParser (FilterElem s a)
parseFilterElem symStr = do
  void $ MPC.string (T.pack symStr)
  MPC.space
  asum
    [ parseOne,
      parseMany
    ]
  where
    parseOne = do
      op <- parser
      MPC.space
      lbl <- parseSetElem
      pure $ FilterElemEq op lbl

    parseMany = do
      op <- parser
      MPC.space
      set <- parseSet symStr
      pure $ FilterElemExists op set

parseSetElem :: (Parser a) => MParser a
parseSetElem = do
  txt <- parseTextNonEmpty
  case (T.find (\c -> c == '{' || c == '}') txt) of
    Just c ->
      fail
        $ mconcat
          [ "Unexpected char '",
            [c],
            "'. Expected exactly one element, not set syntax."
          ]
    Nothing -> failErr $ P.parseAll txt

parseSet :: (Ord a, Parser a) => String -> MParser (Set a)
parseSet name = do
  set <- parseEmptySet <|> parseSetTxt
  MPC.space
  pure set
  where
    parseEmptySet = MPC.char '∅' $> Set.empty

    parseSetTxt = do
      MPC.char '{'
      txt <- MP.takeWhileP (Just name) (/= '}')
      set <- parseCommaSep txt
      MPC.char '}'
      pure set

    parseCommaSep txt = do
      let stripped = T.strip txt
      if T.null stripped
        -- 1. If stripped text is empty that means we only received
        --    whitespace i.e. the empty set, OK.
        then pure Set.empty
        -- 2. Stripped text is non-empty. Parse comma-sep text and
        --    strip each entry. If any of the entries are empty that
        --    means we either had a leading/trailing comma or
        --    "consecutive" ones e.g. {a,  ,b}, BAD.
        else do
          let xs =
                fmap T.strip
                  . T.split (== ',')
                  $ txt

          when (any T.null xs)
            $ fail
            $ mconcat
              [ "Unexpected empty text. Possibly there are leading/",
                "trailing/consecutive commas."
              ]

          Set.fromList <$> (failErr $ traverse P.parseAll xs)

-------------------------------------------------------------------------------
--                                  Operators                                --
-------------------------------------------------------------------------------

-- | Operator for LHS element and RHS set.
data FilterElemOpSet
  = -- | Membership.
    FilterElemInSet
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterElemOpSet where
  displayBuilder FilterElemInSet = "∈"

instance Parser FilterElemOpSet where
  parser =
    asum
      [ P.char '∈' $> FilterElemInSet,
        P.string "in" $> FilterElemInSet
      ]

-------------------------------------------------------------------------------
--                                  Functions                                --
-------------------------------------------------------------------------------

memberFun :: (Ord a) => FilterElemOpSet -> a -> Set a -> Bool
memberFun FilterElemInSet = Set.member

applyFilterElem :: (Ord b) => b -> FilterElem p b -> Bool
applyFilterElem actVal = \case
  FilterElemEq op t -> Eq.toFun op actVal t
  FilterElemExists op set -> memberFun op actVal set

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
  displayBuilder = \case
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
          displaySet set
        ]
    where
      symStr = displayBuilder $ symbolVal (Proxy :: Proxy s)
      symsStrSpc = symStr <> "s "

-------------------------------------------------------------------------------
--                                   Parsing                                 --
-------------------------------------------------------------------------------

instance (KnownSymbol s, Ord a, Parser a) => Parser (FilterSet s a) where
  parser = parseFilterSet symsStr <?> symsStr
    where
      symStr = symbolVal (Proxy :: Proxy s)
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
      txt <- parseSetElem
      pure $ FilterSetHasElem op txt

    parseMany = do
      op <- parser
      MPC.space
      set <- parseSet symsStr
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

-------------------------------------------------------------------------------
--                                   Misc                                    --
-------------------------------------------------------------------------------

parseTextNonEmpty :: MParser Text
parseTextNonEmpty = do
  txt <- MP.takeWhile1P (Just "text") (const True)
  let stripped = T.strip txt
  -- For reasons I do not currently understand, the below guard is
  -- apparently unnecessary. That is, we do not want to parse empty
  -- (whitespace only) text here, hence the check. But it actually does
  -- not matter, as all users (label = ..., labels ∋) end up failing
  -- with white-space only text, which is what we want.
  --
  -- As to _why_ we have this behavior, I am not sure, since whitespace
  -- _does_ satisfy takeWhile1P. But for some reason it is failing here,
  -- so good? It would be nice to understand this.
  when (T.null stripped) $ fail "Unexpected empty text"
  pure stripped

displaySet :: (Display a, Eq a) => Set a -> Builder
displaySet s
  | s == Set.empty = "∅"
  | otherwise = showFn s
  where
    showFn xs =
      Utils.Show.showListLike
        . Utils.Show.ShowListInline (Utils.Show.ShowListMap displayBuilder xs)
        $ mempty {Utils.Show.brackets = Utils.Show.ShowListBracketsCurly}
