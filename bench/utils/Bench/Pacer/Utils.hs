{-# LANGUAGE QuasiQuotes #-}

module Bench.Pacer.Utils
  ( genAndDecodeRuns,
    genRunsJson,
    decodeRuns,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.List qualified as L
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.Calendar qualified as Cal
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Run (SomeRuns)
import Pacer.Command.Chart.Data.Time
  ( Timestamp
      ( TimestampDate,
        TimestampTime,
        TimestampZoned
      ),
  )
import Pacer.Command.Chart.Data.Time qualified as Time
import Pacer.Data.Result (onErr)
import Pacer.Prelude hiding (Double)
import Pacer.Utils qualified as Utils
import Prelude (Double)

-- | 'genRunsJson' and 'decodeRuns'.
genAndDecodeRuns :: Word -> SomeRuns Double
genAndDecodeRuns = decodeRuns . genRunsJson

-- | Decodes runs json.
decodeRuns :: ByteString -> SomeRuns Double
decodeRuns = onErr (error . displayException) . Utils.decodeJson

-- | Generate runs json sized by the parameter.
genRunsJson :: Word -> ByteString
genRunsJson maxAll = mkBS allRuns
  where
    -- NOTE: [Avoiding overlaps]
    --
    -- Our runs creation is fairly simple. For each of Date, Time, Zoned
    -- types, given a start date, create a sequence of runs by where each
    -- run is the previous run + 1 day.
    --
    -- Because we interlace all of Date, Time, Zoned, the easiest way to
    -- avoid overlaps is to choose a start date for each s.t. the ranges will
    -- not overlap at all.
    --
    -- The current max runs is 10_000, so each range is 10_000 / 3 = 3,333
    -- days ~ 9 years.
    --
    -- Thus if we space out each start date by 10 years, we should be fine.
    maxEach = maxAll .%. 3

    allRuns :: List ByteString
    allRuns =
      join
        -- zipWith3 so that we can interleave the different times, just so the
        -- sorting is non-trivial.
        $ L.zipWith3
          (\d t z -> mkRun d : mkRun t : [mkRun z])
          dates
          times
          zoneds

    mkRun :: Timestamp -> ByteString
    mkRun t =
      -- intercalate over unlines so we don't have a trailing newline
      BS.intercalate
        "\n"
        [ "    {",
          "      \"datetime\": \"" <> encodeTimestamp t <> "\",",
          "      \"distance\": \"marathon\",",
          "      \"duration\": \"3h20m\"",
          "    }"
        ]

    mkBS :: List ByteString -> ByteString
    mkBS rs =
      C8.unlines
        [ "{",
          "  \"runs\": [",
          BS.intercalate ",\n" rs,
          "  ]",
          "}"
        ]

    -- See NOTE: [Avoiding overlaps] for choosing good start dates for each
    -- 3 types.
    startDate :: Day
    startDate = unsafeParse "1990-04-08"

    dates :: List Timestamp
    dates = (TimestampDate . incDate startDate) <$> [0 .. maxEach]

    startTime :: LocalTime
    startTime = unsafeParse "2000-04-08T12:00:00"

    times :: List Timestamp
    times = (TimestampTime . incTime startTime) <$> [0 .. maxEach]

    startZoned :: ZonedTime
    startZoned = unsafeParse "2010-04-08T12:00:00-0800"

    zoneds :: List Timestamp
    zoneds = (TimestampZoned . incZoned startZoned) <$> [0 .. maxEach]

    encodeTimestamp :: Timestamp -> ByteString
    encodeTimestamp = encodeUtf8 . Time.fmtTimestamp

    incDate :: Day -> Word -> Day
    incDate d i = Cal.addDays (fromIntegral @Word @Integer i) d

    incTime :: LocalTime -> Word -> LocalTime
    incTime (LocalTime d t) i = LocalTime (incDate d i) t

    incZoned :: ZonedTime -> Word -> ZonedTime
    incZoned (ZonedTime (LocalTime d t) z) i =
      ZonedTime (LocalTime (incDate d i) t) z

    unsafeParse :: (Parser a) => Text -> a
    unsafeParse = errorErr . P.parseAll
