{-# OPTIONS_GHC -Wwarn #-}

module Running.Config.Args () where

import Options.Applicative
  ( Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Running
import Running.Data.Distance (SomeDistance)
import Running.Data.Duration (SomeDuration)
import Running.Data.Pace (Pace)
import Running.Prelude

-- 1. Given pace (min sec), calculate percentages (percent optional, have defaults)
-- 2. given marathon time(s), calculate pace
-- 3. given pace(s), calculate marathon time

-- pace: time-str km
-- distance: time-str 42 km

-- 2. --distance marathon --duration 3h30m
-- distToPace :: Distance d1 Natural -> Duration t n -> Pace t d2 n
-- distToPace = todo

-- 3. --pace "5m30s km" --distance marathon
--    Pace Second KM Natural -> Distance Meter Natural -> Duration Second Natural
-- paceToTime :: Pace Second d1 Natural -> Distance d2 Natural -> Duration Second Natural
-- paceToTime = todo

-- TODO:
--
-- - units? ugh

newtype Args = MkArgs
  { cmd :: Command
  }
  deriving stock (Eq, Show)

data Command
  = Convert
  | Scale
  deriving stock (Eq, Show)

data ConvertArgs
  = ConvertToPace (SomeDistance PDouble) (SomeDuration Double)
  | ConvertToDuration (SomeDistance PDouble) (forall d. Pace d Double)
