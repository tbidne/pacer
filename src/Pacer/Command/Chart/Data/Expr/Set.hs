-- | Provides filters over generic set.
module Pacer.Command.Chart.Data.Expr.Set
  ( -- * FilterElem
    FilterElem (..),
    FilterElemOpSet (..),
    memberFun,

    -- * FilterSet
    FilterSet (..),
    FilterSetOpElem (..),
    FilterSetOpSet (..),
    existsElemFun,
    existsSetFun,
    hasElemFun,
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
import Pacer.Command.Chart.Data.Expr.Eq
  ( FilterOpEq (FilterOpEqEq, FilterOpEqNEq),
  )
import Pacer.Command.Chart.Data.Expr.Eq qualified as Eq
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-------------------------------------------------------------------------------
--                                 FilterElem                                --
-------------------------------------------------------------------------------

-- | @FilterSet X@ tests set operations on the set @X@ e.g. X = "labels".
-- Note that, wrt the operators, the LHS is always a set (e.g. labels set),
-- so operators where the LHS is expected to be an element --
-- FilterOpEq used by FilterSetExistsElem -- the LHS is considered to be
-- an arbitrary element in the set.
--
-- For instance, @label = foo@ means
-- "there exists an l in labels s.t. l == foo".
type FilterElem :: Symbol -> Type
data FilterElem p
  = -- | Tests an element e for membership in X e.g.
    --
    -- 1. @label = foo@ <=> exists label l s.t. l == foo.
    -- 2. @label /= foo@ <=> forall labels l, l /= foo.
    --
    -- Notice that negation negates the entire expression, not the inner
    -- symbol. That is, 2 is equivalent to:
    --
    --   not (exists label l s.t. l == foo)
    --
    -- NOT
    --
    --   exists label l s.t. l /= foo
    FilterElemEq FilterOpEq Text
  | -- | Tests some x in X for membership in Y e.g.
    --
    -- 1. @label ∈ {a, b}@ <=> exists label l s.t. l == a or l == b.
    -- 2. @label ∉ {a, b}@ <=> forall labels l, l /= a and l /= b.
    --
    -- This can be used to encode "OR". Once again, negation is out the
    -- "outside", not the inside.
    FilterElemExists FilterElemOpSet (Set Text)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance (KnownSymbol s) => Display (FilterElem s) where
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

instance (KnownSymbol s) => Parser (FilterElem s) where
  parser = parseFilterElem symStr <?> symStr
    where
      symStr = symbolVal (Proxy :: Proxy s)

parseFilterElem :: String -> MParser (FilterElem s)
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

parseSetElem :: MParser Text
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
    Nothing -> pure txt

parseSet :: String -> MParser (Set Text)
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
          let set =
                Set.fromList
                  . fmap T.strip
                  . T.split (== ',')
                  $ txt

          when (any T.null set)
            $ fail
            $ mconcat
              [ "Unexpected empty text. Possibly there are leading/",
                "trailing/consecutive commas."
              ]

          pure set

-------------------------------------------------------------------------------
--                                  Operators                                --
-------------------------------------------------------------------------------

-- | Operator for LHS element and RHS set.
data FilterElemOpSet
  = -- | Membership.
    FilterElemInSet
  | -- | Not membership.
    FilterElemNotInSet
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterElemOpSet where
  displayBuilder = \case
    FilterElemInSet -> "∈"
    FilterElemNotInSet -> "∉"

instance Parser FilterElemOpSet where
  parser =
    asum
      [ P.char '∈' $> FilterElemInSet,
        P.string "in" $> FilterElemInSet,
        P.char '∉' $> FilterElemNotInSet,
        P.string "not_in" $> FilterElemNotInSet
      ]

-------------------------------------------------------------------------------
--                                  Functions                                --
-------------------------------------------------------------------------------

memberFun :: (Ord a) => FilterElemOpSet -> a -> Set a -> Bool
memberFun FilterElemInSet = Set.member
memberFun FilterElemNotInSet = Set.notMember

-------------------------------------------------------------------------------
--                                 FilterSet                                 --
-------------------------------------------------------------------------------

-- | @FilterSet X@ tests set operations on the set @X@ e.g. X = "labels".
-- Note that, wrt the operators, the LHS is always a set (e.g. labels set),
-- so operators where the LHS is expected to be an element --
-- FilterOpEq used by FilterSetExistsElem -- the LHS is considered to be
-- an arbitrary element in the set.
--
-- For instance, @label = foo@ means
-- "there exists an l in labels s.t. l == foo".
type FilterSet :: Symbol -> Type
data FilterSet p
  = -- | Operations on an arbitrary element in X. Not only does this allow
    -- alternative syntax for @X ∋ a@ -- i.e. @x = a@, it also allows
    -- encoding "OR" e.g. @x ∈ {a, b}@ for any x in X.
    FilterSetElem (FilterElem p)
  | -- | Set membership.
    FilterSetHasElem FilterSetOpElem Text
  | -- | Set comparisons e.g. @labels ⊇ {label_1, label_2}@. This tests that
    -- all labels exist within labels ("AND").
    FilterSetComp FilterSetOpSet (Set Text)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance (KnownSymbol s) => Display (FilterSet s) where
  displayBuilder = \case
    FilterSetElem e -> displayBuilder e
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

instance (KnownSymbol s) => Parser (FilterSet s) where
  parser =
    asum
      [ -- plural needs to precede singular.
        parseFilterSet symsStr <?> symsStr,
        FilterSetElem <$> parser <?> symStr
      ]
    where
      symStr = symbolVal (Proxy :: Proxy s)
      symsStr = symStr <> "s"

parseFilterSet :: String -> MParser (FilterSet s)
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
  | -- | Not membership.
    FilterSetNContainsElem
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterSetOpElem where
  displayBuilder = \case
    FilterSetContainsElem -> "∋"
    FilterSetNContainsElem -> "∌"

instance Parser FilterSetOpElem where
  parser =
    asum
      [ P.char '∋' $> FilterSetContainsElem,
        P.string "contains" $> FilterSetContainsElem,
        P.char '∌' $> FilterSetNContainsElem,
        P.string "not_contains" $> FilterSetNContainsElem
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
        P.char '⊂' $> FilterSetOpSetPSub
      ]

-------------------------------------------------------------------------------
--                                 Functions                                 --
-------------------------------------------------------------------------------

-- LHS always set i.e. FilterSet functions.

existsElemFun :: (Ord a) => FilterOpEq -> Set a -> a -> Bool
existsElemFun FilterOpEqEq = flip Set.member
existsElemFun FilterOpEqNEq = flip Set.notMember

existsSetFun :: (Ord a) => FilterElemOpSet -> Set a -> Set a -> Bool
existsSetFun FilterElemInSet ls rs = Set.intersection ls rs /= mempty
existsSetFun FilterElemNotInSet ls rs = Set.intersection ls rs == mempty

hasElemFun :: (Ord a) => FilterSetOpElem -> Set a -> a -> Bool
hasElemFun FilterSetContainsElem = flip Set.member
hasElemFun FilterSetNContainsElem = flip Set.notMember

compFun :: (Ord b) => FilterSetOpSet -> (Set b -> Set b -> Bool)
compFun (FilterSetOpSetEq op) = Eq.toFun op
compFun FilterSetOpSetSuper = isSuperSetOf
compFun FilterSetOpSetPSuper = isProperSuperSetOf
compFun FilterSetOpSetSub = Set.isSubsetOf
compFun FilterSetOpSetPSub = Set.isProperSubsetOf

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
  | otherwise = Utils.showMapSetInline displayBuilder s
