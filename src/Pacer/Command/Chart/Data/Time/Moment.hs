module Pacer.Command.Chart.Data.Time.Moment
  ( -- * Moment
    Moment (..),
  )
where

import Pacer.Class.IOrd (IEq ((~~)), IOrd ((<~)))
import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time.Month (Month)
import Pacer.Command.Chart.Data.Time.Timestamp
  ( Timestamp,
    timestampToYear,
    timestampToYearMonth,
  )
import Pacer.Command.Chart.Data.Time.Year (Year)
import Pacer.Prelude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Generalized 'Timestamp', for activity filtering.
data Moment
  = -- | A year like 2013.
    MomentYear Year
  | -- | A year and month like 2013-08.
    MomentYearMonth Year Month
  | -- A Timestamp.
    --
    -- NOTE: These timestamps should be in between 1950 and 2099. Should we
    -- enforce this? E.g. do not export the bare constructor, and make sure
    -- parsing enforces this.
    --
    -- Probably the most robust course of action would be to make Timestamp
    -- enforce the constraint, then we'd inherit it here. We'd probably
    -- want Year and Timestamp defined in the same module so we could share
    -- the validation.
    MomentTimestamp Timestamp
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

-- NOTE: We do not implement the usual Eq/Ord here like timestamp, since we
-- have a different (non-lawful) instance in mind. The derived Eq instance
-- is purely so "upstream" types (e.g. Expr, ChartRequests) can have Eq.
--
-- That notwithstanding, we only care about IEq/IOrd.
--
-- These _do_ allow different constructors to be equal, provided they overlap.

-- Our general interest in Moment Ord comparisons is when filtering Activities
-- e.g. we want to filter activities w/ Activity.datetime >= Moment. In
-- particular, we need to compare "low precision" moments like years w/ higher
-- precision timestamps. For example, we may want to takes activities with a
-- timestamp >= 2013.
--
-- To do this, we introduce the below IEq/IOrd functions that compare
-- based on the "common data" e.g. 2013-08-10 and 2013 are compared
-- via the year, 2013.
--
-- Note that this is not lawful (breaks transitivity, specifically), hence
-- are not part of Eq/Ord instances.

instance IEq Moment where
  MomentYear y1 ~~ MomentYear y2 = y1 == y2
  MomentYear y1 ~~ MomentYearMonth y2 _ = y1 == y2
  MomentYear y1 ~~ MomentTimestamp t2 = y1 == errorErr (timestampToYear t2)
  MomentYearMonth y1 _ ~~ MomentYear y2 = y1 == y2
  MomentYearMonth y1 m1 ~~ MomentYearMonth y2 m2 = y1 == y2 && m1 == m2
  MomentYearMonth y1 m1 ~~ MomentTimestamp t2 = (y1, m1) == errorErr (timestampToYearMonth t2)
  MomentTimestamp t1 ~~ MomentYear y2 = errorErr (timestampToYear t1) == y2
  MomentTimestamp t1 ~~ MomentYearMonth y2 m2 = errorErr (timestampToYearMonth t1) == (y2, m2)
  MomentTimestamp t1 ~~ MomentTimestamp t2 = t1 ~~ t2

instance IOrd Moment where
  MomentYear y1 <~ MomentYear y2 = y1 <= y2
  MomentYear y1 <~ MomentYearMonth y2 _ = y1 <= y2
  MomentYear y1 <~ MomentTimestamp t2 = y1 <= errorErr (timestampToYear t2)
  -- Notice that, unlike Timestamp's Ord (and what we would do if we had Ord
  -- for Moment), we use (<=) even when the LHS has _more_ precision than the
  -- RHS. Timestamp uses (<), since it is the only way to be lawful:
  -- data w/ less precision is arbitrarily chosen to be "lesser", so the only
  -- way it can be (<=) is if it satisfies the stronger (<).
  --
  -- Here, OTOH, we explicitly do _not_ promise transitivity. We consistently
  -- compare "up to common data", which allows us to sensibly make comparisons
  -- like. For a concrete example, consider:
  --
  --     2013-08-12T14:30:00 >  2013-08-12 -- True (Timestamp)
  --     2013-08-12T14:30:00 >. 2013-08-12 -- False (Moment)
  --
  -- Explanation: Timestamp's rule about less precise data being "lesser"
  -- means that 2013-08-12 < 2013-08-12T14:30:00.
  --
  -- On the other hand, Moment's IOrd only compares whatever data they have
  -- in common. In this case that is 2013-08-12, which is equal. Hence
  -- <~ is true, therefore >. cannot be true (because laws).
  MomentYearMonth y1 _ <~ MomentYear y2 = y1 <= y2
  MomentYearMonth y1 m1 <~ MomentYearMonth y2 m2
    | y1 < y2 = True
    | y1 == y2 = m1 <= m2
    | otherwise = False
  MomentYearMonth y1 m1 <~ MomentTimestamp t2 =
    let (y2, m2) = errorErr $ timestampToYearMonth t2
     in if
          | y1 < y2 -> True
          | y1 == y2 -> m1 <= m2
          | otherwise -> False
  MomentTimestamp t1 <~ MomentYear y2 = errorErr (timestampToYear t1) <= y2
  MomentTimestamp t1 <~ MomentYearMonth y2 m2 =
    let (y1, m1) = errorErr $ timestampToYearMonth t1
     in if
          | y1 < y2 -> True
          | y1 == y2 -> m1 <= m2
          | otherwise -> False
  MomentTimestamp t1 <~ MomentTimestamp t2 = t1 ~~ t2 || t1 < t2

instance Display Moment where
  displayBuilder = \case
    MomentYear x -> displayBuilder x
    MomentYearMonth x y -> displayBuilder x <> "-" <> displayBuilder y
    MomentTimestamp x -> displayBuilder x

instance FromJSON Moment where
  parseJSON = asnWithText "Moment" (failErr . P.parseAll)

instance Parser Moment where
  parser = do
    asum
      [ MomentTimestamp <$> MP.try parser,
        MP.try parseYearMonth,
        parseYear
      ]
    where
      parseYearMonth = do
        year <- parser
        MPC.char '-'
        month <- parser
        pure $ MomentYearMonth year month

      parseYear = MomentYear <$> parser
