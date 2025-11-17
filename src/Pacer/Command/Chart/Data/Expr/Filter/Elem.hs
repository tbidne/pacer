-- | Provides filters over generic set.
module Pacer.Command.Chart.Data.Expr.Filter.Elem
  ( -- * FilterElem
    FilterElem (..),
    FilterElemOpSet (..),
    memberFun,
    applyFilterElem,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Pacer.Class.Parser (MParser, Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Expr.Eq (FilterOpEq)
import Pacer.Command.Chart.Data.Expr.Eq qualified as Eq
import Pacer.Command.Chart.Data.Expr.Filter.Utils qualified as Filter.Utils
import Pacer.Prelude
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- NOTE: [Filter negative operators]
--
-- While we could (and did, at one point) include negation for some
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

-- | @FilterElem X@ compares a LHS element against RHS element or set e.g.
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
  displayBuilder :: forall t. (KnownSymbol t) => FilterElem t a -> TextBuilderLinear
  displayBuilder @t = \case
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
          Filter.Utils.displaySet set
        ]
    where
      symStr = displayBuilder $ symbolVal (Proxy :: Proxy t)

-------------------------------------------------------------------------------
--                                  Parsing                                  --
-------------------------------------------------------------------------------

instance (KnownSymbol s, Ord a, Parser a) => Parser (FilterElem s a) where
  parser :: forall t. (KnownSymbol t) => MParser (FilterElem t a)
  parser @t = parseFilterElem symStr <?> symStr
    where
      symStr = symbolVal (Proxy :: Proxy t)

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
  txt <- Filter.Utils.parseTextNonEmpty
  case T.find (\c -> c == '{' || c == '}') txt of
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

          Set.fromList <$> failErr (traverse P.parseAll xs)

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
