-- | Provides filter labeling.
module Pacer.Command.Chart.Data.Expr.Labels
  ( -- * Labels

    -- ** Single
    FilterLabelOp (..),

    -- ** Many
    FilterLabelSet (..),
    FilterLabelSetOpOne (..),
    FilterLabelSetOpMany (..),

    -- * Misc
    parseTextNonEmpty,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Pacer.Class.Parser (MParser, Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-------------------------------------------------------------------------------
--                                    One                                    --
-------------------------------------------------------------------------------

-- | Operator for filtering on a single label. These are based on simple
-- inclusion/exclusion.
data FilterLabelOp
  = -- | Activity labels must contain the given label.
    FilterLabelOpEq
  | -- | Activity labels must not contain the given label.
    FilterLabelOpNeq
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterLabelOp where
  displayBuilder = \case
    FilterLabelOpEq -> "="
    FilterLabelOpNeq -> "≠"

instance Parser FilterLabelOp where
  parser =
    asum
      [ P.char '=' $> FilterLabelOpEq,
        P.string "/=" $> FilterLabelOpNeq,
        P.char '≠' $> FilterLabelOpNeq
      ]

-------------------------------------------------------------------------------
--                                   Many                                    --
-------------------------------------------------------------------------------

-- | Filters on the label set, hence we can test multiple labels at once.
data FilterLabelSet
  = -- | Tests a single label against the activity's set. This is thus
    -- equivalent to FilterLabelOp, hence merely provides an alternative
    -- "set syntax".
    FilterLabelSetOne FilterLabelSetOpOne Text
  | -- | Set comparisons.
    FilterLabelSetMany FilterLabelSetOpMany (Set Text)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterLabelSet where
  displayBuilder = \case
    FilterLabelSetOne op t ->
      mconcat
        [ "labels ",
          displayBuilder op,
          " ",
          displayBuilder t
        ]
    FilterLabelSetMany op set ->
      mconcat
        [ "labels ",
          displayBuilder op,
          " ",
          displaySet set
        ]
      where
        displaySet s
          | s == Set.empty = "∅"
          | otherwise = Utils.showMapSetInline displayBuilder set

instance Parser FilterLabelSet where
  parser = do
    void $ MPC.string "labels"
    MPC.space
    asum
      [ parseOne,
        parseMany
      ]
    where
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
                  "'. ∋ and ∌ require exactly one label, not set syntax."
                ]
          Nothing -> pure ()

        pure $ FilterLabelSetOne op txt

      parseMany = do
        op <- parser
        MPC.space
        labels <- parseEmptySet <|> parseSet
        MPC.space
        pure $ FilterLabelSetMany op labels

      parseEmptySet = MPC.char '∅' $> Set.empty

      parseSet = do
        MPC.char '{'
        txt <- MP.takeWhileP (Just "labels") (/= '}')
        labels <- parseCommaSep txt
        MPC.char '}'
        pure labels

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
            let lblSet =
                  Set.fromList
                    . fmap T.strip
                    . T.split (== ',')
                    $ txt

            when (any T.null lblSet)
              $ fail
              $ mconcat
                [ "Unexpected empty text. Possibly there are leading/",
                  "trailing/consecutive commas."
                ]

            pure lblSet

-- | Set operator for a single label.
data FilterLabelSetOpOne
  = -- | Membership.
    FilterLabelSetOpOneMember
  | -- | Not membership.
    FilterLabelSetOpOneNMember
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterLabelSetOpOne where
  displayBuilder = \case
    FilterLabelSetOpOneMember -> "∋"
    FilterLabelSetOpOneNMember -> "∌"

-- See NOTE: [Operators]
--
-- This ostensibly violate principle 1 (ascii versions of all
-- operators/expressions), but arguably it's fine, since e.g.
-- 'A ∋ b' is equivalent to 'A = b'.
instance Parser FilterLabelSetOpOne where
  parser =
    asum
      [ P.char '∋' $> FilterLabelSetOpOneMember,
        P.char '∌' $> FilterLabelSetOpOneNMember
      ]

-- | Operator for set comparisons.
data FilterLabelSetOpMany
  = -- | Activity labels must equal the given set.
    FilterLabelSetOpManyEq
  | -- | Activity labels must not equal the given set.
    FilterLabelSetOpManyNEq
  | -- | Activity labels must be a proper superset of the given set.
    FilterLabelSetOpManyPSuper
  | -- | Activity labels must be a superset of the given set.
    FilterLabelSetOpManySuper
  | -- | Activity labels must be a proper subset of the given set.
    FilterLabelSetOpManyPSub
  | -- | Activity labels must be a subset of given set.
    FilterLabelSetOpManySub
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

instance Display FilterLabelSetOpMany where
  displayBuilder = \case
    FilterLabelSetOpManyEq -> "="
    FilterLabelSetOpManyNEq -> "≠"
    FilterLabelSetOpManyPSuper -> "⊃"
    FilterLabelSetOpManySuper -> "⊇"
    FilterLabelSetOpManyPSub -> "⊂"
    FilterLabelSetOpManySub -> "⊆"

instance Parser FilterLabelSetOpMany where
  parser =
    asum
      [ P.char '=' $> FilterLabelSetOpManyEq,
        P.string "/=" $> FilterLabelSetOpManyNEq,
        P.char '≠' $> FilterLabelSetOpManyNEq,
        P.string ">=" $> FilterLabelSetOpManySuper,
        P.char '⊇' $> FilterLabelSetOpManySuper,
        P.char '>' $> FilterLabelSetOpManyPSuper,
        P.char '⊃' $> FilterLabelSetOpManyPSuper,
        P.string "<=" $> FilterLabelSetOpManySub,
        P.char '⊆' $> FilterLabelSetOpManySub,
        P.char '<' $> FilterLabelSetOpManyPSub,
        P.char '⊂' $> FilterLabelSetOpManyPSub
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
