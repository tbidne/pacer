{-# LANGUAGE QuasiQuotes #-}

module Bench.Pacer.Utils
  ( -- * High level
    genAndDecodeRuns,
    genAndDecodeOverlappedRuns,

    -- * Low level
    genRunsJson,
    genOverlappedRunsJson,
    decodeRuns,
    decodeErrorRuns,

    -- * Low level
    concatRunsJson,
    mkRunJsonList,
    tsStrToRunJson,
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
import Pacer.Data.Result (onErr, onOk)
import Pacer.Prelude hiding (Double)
import Pacer.Utils (AesonE)
import Pacer.Utils qualified as Utils
import Prelude (Double)

-- | 'genRunsJson' and 'decodeRuns'.
genAndDecodeRuns :: Word -> SomeRuns Double
genAndDecodeRuns = decodeRuns . genRunsJson

-- | 'genOverlappedRunsJson' and 'decodeErrorRuns'.
genAndDecodeOverlappedRuns :: Word -> AesonE
genAndDecodeOverlappedRuns = decodeErrorRuns . genOverlappedRunsJson

-- | Decodes runs json w/ expected error.
decodeErrorRuns :: ByteString -> AesonE
decodeErrorRuns = onOk (error "") . Utils.decodeJson @(SomeRuns Double)

-- | Decodes runs json.
decodeRuns :: ByteString -> SomeRuns Double
decodeRuns = onErr (error . displayException) . Utils.decodeJson

-- | Generate runs json with immediate overlap.
genOverlappedRunsJson :: Word -> ByteString
genOverlappedRunsJson n =
  concatRunsJson
    $ tsStrToRunJson "1980-04-08"
    : tsStrToRunJson "1980-04-08"
    : mkRunJsonList n

-- | Generate runs json sized by the parameter.
genRunsJson :: Word -> ByteString
genRunsJson = concatRunsJson . mkRunJsonList

mkRunJsonList :: Word -> List ByteString
mkRunJsonList maxAll = allRuns
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
          (\d t z -> [tsToRunJson d, tsToRunJson t, tsToRunJson z])
          (dates maxEach)
          (times maxEach)
          (zoneds maxEach)

tsStrToRunJson :: Text -> ByteString
tsStrToRunJson = tsToRunJson . unsafeParse

tsToRunJson :: Timestamp -> ByteString
tsToRunJson t =
  -- intercalate over unlines so we don't have a trailing newline
  BS.intercalate
    "\n"
    [ "    {",
      "      \"datetime\": \"" <> encodeTimestamp t <> "\",",
      "      \"distance\": \"marathon\",",
      "      \"duration\": \"3h20m\"",
      "    }"
    ]

concatRunsJson :: List ByteString -> ByteString
concatRunsJson rs =
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

dates :: Word -> List Timestamp
dates mx = (TimestampDate . incDate startDate) <$> [0 .. mx]

startTime :: LocalTime
startTime = unsafeParse "2000-04-08T12:00:00"

times :: Word -> List Timestamp
times mx = (TimestampTime . incTime startTime) <$> [0 .. mx]

startZoned :: ZonedTime
startZoned = unsafeParse "2010-04-08T12:00:00-0800"

zoneds :: Word -> List Timestamp
zoneds mx = (TimestampZoned . incZoned startZoned) <$> [0 .. mx]

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
