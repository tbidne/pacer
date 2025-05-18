-- | Provides filters over generic set.
module Pacer.Command.Chart.Data.Expr.Set
  ( -- * Types
    FilterSet (..),
    FilterSetElemOp (..),
    FilterSetSetOp (..),
    elemElemToFun,
    setElemToFun,
    setSetToFun,

    -- * Misc
    parseTextNonEmpty,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text qualified as T
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
--                                   Many                                    --
-------------------------------------------------------------------------------

-- | Filters on the X set, hence we can test multiple xs at once.
type FilterSet :: Symbol -> Type
data FilterSet p
  = -- | Tests a single y against an arbitrary element x in X e.g. written as
    -- x = y. For instance, @label = some_label@. This tests for membership
    -- of some_label in labels.
    FilterElemElem FilterOpEq Text
  | -- | Same as 'FilterElemElem' except it is written in set syntax e.g.
    -- X ∋ y. Thus this is merely alternative ("more proper") syntax.
    FilterSetElem FilterSetElemOp Text
  | -- | Set comparisons e.g. @X ⊇ Y@.
    FilterSetSet FilterSetSetOp (Set Text)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance (KnownSymbol s) => Display (FilterSet s) where
  displayBuilder = \case
    FilterElemElem op t ->
      mconcat
        [ fromString symStr,
          " ",
          displayBuilder op,
          " ",
          displayBuilder t
        ]
    FilterSetElem op t ->
      mconcat
        [ fromString symStr,
          "s ",
          displayBuilder op,
          " ",
          displayBuilder t
        ]
    FilterSetSet op set ->
      mconcat
        [ fromString symStr,
          "s ",
          displayBuilder op,
          " ",
          displaySet set
        ]
      where
        displaySet s
          | s == Set.empty = "∅"
          | otherwise = Utils.showMapSetInline displayBuilder set
    where
      symStr = symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s) => Parser (FilterSet s) where
  parser =
    asum
      [ parseFilterSet symsStr <?> symStr,
        parseFilterElemElem symStr <?> symsStr
      ]
    where
      symStr = symbolVal (Proxy :: Proxy s)
      symsStr = symStr <> "s"

parseFilterElemElem :: String -> MParser (FilterSet s)
parseFilterElemElem symStr = do
  void $ MPC.string (T.pack symStr)
  MPC.space
  op <- parser
  MPC.space
  lbl <- parseTextNonEmpty
  pure $ FilterElemElem op lbl

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
      txt <- parseTextNonEmpty
      case (T.find (\c -> c == '{' || c == '}') txt) of
        Just c ->
          fail
            $ mconcat
              [ "Unexpected char '",
                [c],
                "'. ∋ and ∌ require exactly one element, not set syntax."
              ]
        Nothing -> pure ()

      pure $ FilterSetElem op txt

    parseMany = do
      op <- parser
      MPC.space
      set <- parseEmptySet <|> parseSet
      MPC.space
      pure $ FilterSetSet op set

    parseEmptySet = MPC.char '∅' $> Set.empty

    parseSet = do
      MPC.char '{'
      txt <- MP.takeWhileP (Just symsStr) (/= '}')
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

-- | Set operator for a single element.
data FilterSetElemOp
  = -- | Membership.
    FilterSetElemOpMember
  | -- | Not membership.
    FilterSetElemOpNMember
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterSetElemOp where
  displayBuilder = \case
    FilterSetElemOpMember -> "∋"
    FilterSetElemOpNMember -> "∌"

-- See NOTE: [Operators]
--
-- This ostensibly violate principle 1 (ascii versions of all
-- operators/expressions), but arguably it's fine, since e.g.
-- 'A ∋ b' is equivalent to 'A = b'.
instance Parser FilterSetElemOp where
  parser =
    asum
      [ P.char '∋' $> FilterSetElemOpMember,
        P.char '∌' $> FilterSetElemOpNMember
      ]

-- | Operator for set comparisons.
data FilterSetSetOp
  = -- | Activity set X must equal the given set.
    FilterSetSetOpEq FilterOpEq
  | -- | Activity set X must be a proper superset of the given set.
    FilterSetSetOpPSuper
  | -- | Activity set X must be a superset of the given set.
    FilterSetSetOpSuper
  | -- | Activity set X must be a proper subset of the given set.
    FilterSetSetOpPSub
  | -- | Activity set X must be a subset of given set.
    FilterSetSetOpSub
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

instance Display FilterSetSetOp where
  displayBuilder = \case
    FilterSetSetOpEq op -> displayBuilder op
    FilterSetSetOpPSuper -> "⊃"
    FilterSetSetOpSuper -> "⊇"
    FilterSetSetOpPSub -> "⊂"
    FilterSetSetOpSub -> "⊆"

instance Parser FilterSetSetOp where
  parser =
    asum
      [ parser <&> FilterSetSetOpEq,
        P.string ">=" $> FilterSetSetOpSuper,
        P.char '⊇' $> FilterSetSetOpSuper,
        P.char '>' $> FilterSetSetOpPSuper,
        P.char '⊃' $> FilterSetSetOpPSuper,
        P.string "<=" $> FilterSetSetOpSub,
        P.char '⊆' $> FilterSetSetOpSub,
        P.char '<' $> FilterSetSetOpPSub,
        P.char '⊂' $> FilterSetSetOpPSub
      ]

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

elemElemToFun :: (Ord a) => FilterOpEq -> Set a -> a -> Bool
elemElemToFun FilterOpEqEq = flip Set.member
elemElemToFun FilterOpEqNEq = flip Set.notMember

setElemToFun :: (Ord a) => FilterSetElemOp -> Set a -> a -> Bool
setElemToFun FilterSetElemOpMember = flip Set.member
setElemToFun FilterSetElemOpNMember = flip Set.notMember

setSetToFun :: (Ord b) => FilterSetSetOp -> (Set b -> Set b -> Bool)
setSetToFun (FilterSetSetOpEq op) = Eq.toFun op
setSetToFun FilterSetSetOpSuper = isSuperSetOf
setSetToFun FilterSetSetOpPSuper = isProperSuperSetOf
setSetToFun FilterSetSetOpSub = Set.isSubsetOf
setSetToFun FilterSetSetOpPSub = Set.isProperSubsetOf

isSuperSetOf :: forall a. (Ord a) => Set a -> Set a -> Bool
isSuperSetOf = flip Set.isSubsetOf

isProperSuperSetOf :: forall a. (Ord a) => Set a -> Set a -> Bool
isProperSuperSetOf = flip Set.isProperSubsetOf
