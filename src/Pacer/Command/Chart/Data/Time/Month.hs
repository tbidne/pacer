module Pacer.Command.Chart.Data.Time.Month
  ( Month (..),
    mkMonth,
    monthToTimeMoY,
  )
where

import Data.Char qualified as Ch
import Pacer.Class.Parser (Parser (parser))
import Pacer.Prelude
import Text.Megaparsec qualified as MP

-- | Represents a calendar month.
data Month
  = Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                               Serialization                               --
-------------------------------------------------------------------------------

instance Parser Month where
  parser = do
    txt <- MP.takeWhile1P (Just "01-12") (\c -> Ch.isDigit c)
    case txt of
      "01" -> pure Jan
      "02" -> pure Feb
      "03" -> pure Mar
      "04" -> pure Apr
      "05" -> pure May
      "06" -> pure Jun
      "07" -> pure Jul
      "08" -> pure Aug
      "09" -> pure Sep
      "10" -> pure Oct
      "11" -> pure Nov
      "12" -> pure Dec
      other ->
        fail $ "Expected a month in 01 - 12, received: " ++ unpackText other

instance Display Month where
  displayBuilder = \case
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"

monthToTimeMoY :: Month -> Int
monthToTimeMoY Jan = 1
monthToTimeMoY Feb = 2
monthToTimeMoY Mar = 3
monthToTimeMoY Apr = 4
monthToTimeMoY May = 5
monthToTimeMoY Jun = 6
monthToTimeMoY Jul = 7
monthToTimeMoY Aug = 8
monthToTimeMoY Sep = 9
monthToTimeMoY Oct = 10
monthToTimeMoY Nov = 11
monthToTimeMoY Dec = 12

mkMonth :: (Toℤ a) => a -> ResultDefault Month
mkMonth =
  toℤ >>> \case
    1 -> pure Jan
    2 -> pure Feb
    3 -> pure Mar
    4 -> pure Apr
    5 -> pure May
    6 -> pure Jun
    7 -> pure Jul
    8 -> pure Aug
    9 -> pure Sep
    10 -> pure Oct
    11 -> pure Nov
    12 -> pure Dec
    other ->
      fail
        $ "Expected month in 1-12: "
        ++ show other
