-- | Provides filter.
module Pacer.Command.Chart.Data.Expr.Filter
  ( FilterType (..),
    FilterOp (..),
  )
where

import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Expr.Labels
import Pacer.Command.Chart.Data.Expr.Labels qualified as Labels
import Pacer.Command.Chart.Data.Time.Moment (Moment)
import Pacer.Data.Distance (SomeDistance)
import Pacer.Data.Duration (Duration)
import Pacer.Data.Pace (SomePace)
import Pacer.Prelude
import Text.Megaparsec ((<?>))
import Text.Megaparsec.Char qualified as MPC

-------------------------------------------------------------------------------
--                                  FilterOp                                 --
-------------------------------------------------------------------------------

-- | Operator for filter comparisons.
data FilterOp
  = FilterOpEq
  | FilterOpNEq
  | FilterOpLte
  | FilterOpLt
  | FilterOpGte
  | FilterOpGt
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterOp where
  displayBuilder = \case
    FilterOpEq -> "="
    FilterOpNEq -> "≠"
    FilterOpLte -> "≤"
    FilterOpLt -> "<"
    FilterOpGte -> "≥"
    FilterOpGt -> ">"

-- NOTE: [Operators]
--
-- In general, we have a few principles when choosing operators.
--
-- 1. Every operator/expression should always be expressible in plain ascii.
--
-- 2. For any "composite" ascii operator, if a unicode (non-ascii) variant
--    exists, allow it. Hence adding ≥, ≤, ≠, ∅.
--
-- 3. There should be at most one operator for each of ascii, unicode.
--
-- 4. Displayed output should prioritize unicode operators, when they
--    exist.
--
-- 5. Displayed output should always be a valid input i.e. no
--    "prettifying" some operator to invalid syntax.

instance Parser FilterOp where
  parser =
    asum
      [ P.string "<=" $> FilterOpLte,
        P.char '≤' $> FilterOpLte,
        P.char '<' $> FilterOpLt,
        P.char '=' $> FilterOpEq,
        P.string "/=" $> FilterOpNEq,
        P.char '≠' $> FilterOpNEq,
        P.string ">=" $> FilterOpGte,
        P.char '≥' $> FilterOpGte,
        P.char '>' $> FilterOpGt
      ]

-------------------------------------------------------------------------------
--                                 FilterType                                --
-------------------------------------------------------------------------------

-- | Ways in which we can filter activities.
data FilterType a
  = FilterDistance FilterOp (SomeDistance a)
  | FilterDuration FilterOp (Duration a)
  | -- | Filters by presence of a single label.
    FilterLabel FilterLabelOp Text
  | -- | Filters by presence of multiple labels.
    FilterLabels FilterLabelSet
  | FilterDate FilterOp Moment
  | FilterPace FilterOp (SomePace a)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance
  ( AMonoid a,
    Display a,
    Fromℤ a,
    MSemigroup a,
    Ord a,
    Show a,
    Toℚ a
  ) =>
  Display (FilterType a)
  where
  displayBuilder = \case
    FilterDistance op d ->
      mconcat
        [ "distance ",
          displayBuilder op,
          " ",
          displayBuilder d
        ]
    FilterDuration op d ->
      mconcat
        [ "duration ",
          displayBuilder op,
          " ",
          displayBuilder d
        ]
    FilterLabel op t ->
      mconcat
        [ "label ",
          displayBuilder op,
          " ",
          displayBuilder t
        ]
    FilterLabels labelsSet -> displayBuilder labelsSet
    FilterDate op m ->
      mconcat
        [ "datetime ",
          displayBuilder op,
          " ",
          displayBuilder m
        ]
    FilterPace op d ->
      mconcat
        [ "pace ",
          displayBuilder op,
          " ",
          displayBuilder d
        ]

instance
  ( Fromℚ a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (FilterType a)
  where
  parser =
    asum
      [ FilterLabels <$> parser <?> "labels",
        parseLabel <?> "label",
        parseDist <?> "distance",
        parseDuration <?> "duration",
        parsePace <?> "pace",
        parseDate <?> "datetime"
      ]
    where
      parseLabel = do
        void $ MPC.string "label"
        MPC.space
        op <- parser
        MPC.space
        lbl <- Labels.parseTextNonEmpty
        pure $ FilterLabel op lbl

      parseDate = parsePred "datetime" FilterDate
      parseDist = parsePred "distance" FilterDistance
      parseDuration = parsePred "duration" FilterDuration
      parsePace = parsePred "pace" FilterPace

      parsePred s cons = do
        MPC.string s
        MPC.space
        op <- parser
        MPC.space
        d <- parser
        pure $ cons op d
