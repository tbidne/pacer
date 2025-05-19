-- | Provides filter.
module Pacer.Command.Chart.Data.Expr.Filter
  ( FilterType (..),
  )
where

import Pacer.Class.Parser (Parser (parser))
import Pacer.Command.Chart.Data.Activity (ActivityType, Label)
import Pacer.Command.Chart.Data.Expr.Ord (FilterOpOrd)
import Pacer.Command.Chart.Data.Expr.Set qualified as Set
import Pacer.Command.Chart.Data.Time.Moment (Moment)
import Pacer.Data.Distance (SomeDistance)
import Pacer.Data.Duration (Duration)
import Pacer.Data.Pace (SomePace)
import Pacer.Prelude
import Text.Megaparsec ((<?>))
import Text.Megaparsec.Char qualified as MPC

-------------------------------------------------------------------------------
--                                 FilterType                                --
-------------------------------------------------------------------------------

-- | Ways in which we can filter activities.
data FilterType a
  = -- | Filter based on distance.
    FilterDistance FilterOpOrd (SomeDistance a)
  | -- | Filter based on duration.
    FilterDuration FilterOpOrd (Duration a)
  | -- | Filter based on label set.
    FilterLabel (Set.FilterSet "label" Label)
  | -- | Filter based on date.
    FilterDate FilterOpOrd Moment
  | -- | Filter based on pace.
    FilterPace FilterOpOrd (SomePace a)
  | -- | Filter based on type.
    FilterType (Set.FilterElem "type" ActivityType)
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
    FilterLabel labelsSet -> displayBuilder labelsSet
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
    FilterType typeElem -> displayBuilder typeElem

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
      [ FilterLabel <$> parser,
        parseDist <?> "distance",
        parseDuration <?> "duration",
        parsePace <?> "pace",
        parseDate <?> "datetime",
        parseDate <?> "datetime",
        FilterType <$> parser
      ]
    where
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
