module Bench.Pacer.Utils
  ( -- * High level
    genAndDecodeActivities,
    genAndDecodeOverlappedActivities,

    -- * Low level
    genActivitiesJson,
    genOverlappedActivitiesJson,
    decodeActivities,
    decodeErrorActivities,

    -- * Low level
    concatActivitiesJson,
    mkActivityJsonList,
    tsStrToActivityJson,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.List qualified as L
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.Calendar qualified as Cal
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Activity
  ( SomeActivities,
    SomeActivitiesParse (unSomeActivitiesParse),
  )
import Pacer.Command.Chart.Data.Activity qualified as Activity
import Pacer.Command.Chart.Data.Time.Timestamp qualified as TS
import Pacer.Command.Chart.Data.Time.Timestamp.Internal
  ( Timestamp
      ( TimestampDate,
        TimestampTime,
        TimestampZoned
      ),
  )
import Pacer.Data.Result (onErr, onOk)
import Pacer.Prelude hiding (Double)
import Pacer.Utils (AesonE (MkAesonE))
import Pacer.Utils qualified as Utils
import Prelude (Double)

-- | 'genActivitiesJson' and 'decodeActivities'.
genAndDecodeActivities :: Word -> SomeActivities Double
genAndDecodeActivities = decodeActivities . genActivitiesJson

-- | 'genOverlappedActivitiesJson' and 'decodeErrorActivities'.
genAndDecodeOverlappedActivities :: (HasCallStack) => Word -> AesonE
genAndDecodeOverlappedActivities = decodeErrorActivities . genOverlappedActivitiesJson

-- | Decodes activities json w/ expected error.
decodeErrorActivities :: (HasCallStack) => ByteString -> AesonE
decodeErrorActivities = onOk (error "Parse succeeded") . parseSomeActivities

-- | Decodes activities json.
decodeActivities :: (HasCallStack) => ByteString -> SomeActivities Double
decodeActivities = onErr (error . displayException) . parseSomeActivities

parseSomeActivities :: ByteString -> Result AesonE (SomeActivities Double)
parseSomeActivities bs = do
  -- Have to do this manually because the overlap error is no longer in the
  -- FromJSON instance. It is in the mkSomeActivitiesFail function,
  -- hence we need to explicitly use it to trigger the expected error.
  parseResult <- Utils.decodeJsonP (Activity.parseSomeActivitiesParse []) bs
  results <- first mkAesonE $ sequenceA parseResult.unSomeActivitiesParse
  first mkAesonE $ Activity.mkSomeActivitiesFail results
  where
    mkAesonE = MkAesonE Nothing

-- | Generate activities json with immediate overlap.
genOverlappedActivitiesJson :: Word -> ByteString
genOverlappedActivitiesJson n =
  concatActivitiesJson
    $ tsStrToActivityJson "1980-04-08"
    : tsStrToActivityJson "1980-04-08"
    : mkActivityJsonList n

-- | Generate activities json sized by the parameter.
genActivitiesJson :: Word -> ByteString
genActivitiesJson = concatActivitiesJson . mkActivityJsonList

mkActivityJsonList :: Word -> List ByteString
mkActivityJsonList maxAll = allActivities
  where
    -- NOTE: [Avoiding overlaps]
    --
    -- Our activities creation is fairly simple. For each of Date, Time, Zoned
    -- types, given a start date, create a sequence of activities by where each
    -- activity is the previous activity + 1 day.
    --
    -- Because we interlace all of Date, Time, Zoned, the easiest way to
    -- avoid overlaps is to choose a start date for each s.t. the ranges will
    -- not overlap at all.
    --
    -- The current max activities is 10_000, so each range is 10_000 / 3 = 3,333
    -- days ~ 9 years.
    --
    -- Thus if we space out each start date by 10 years, we should be fine.
    maxEach = maxAll .%. 3

    allActivities :: List ByteString
    allActivities =
      join
        -- zipWith3 so that we can interleave the different times, just so the
        -- sorting is non-trivial.
        $ L.zipWith3
          (\d t z -> [tsToActivityJson d, tsToActivityJson t, tsToActivityJson z])
          (dates maxEach)
          (times maxEach)
          (zoneds maxEach)

tsStrToActivityJson :: Text -> ByteString
tsStrToActivityJson = tsToActivityJson . unsafeParse

tsToActivityJson :: Timestamp -> ByteString
tsToActivityJson t =
  -- intercalate over unlines so we don't have a trailing newline
  BS.intercalate
    "\n"
    [ "    {",
      "      \"datetime\": \"" <> encodeTimestamp t <> "\",",
      "      \"distance\": \"marathon\",",
      "      \"duration\": \"3h20m\"",
      "    }"
    ]

concatActivitiesJson :: List ByteString -> ByteString
concatActivitiesJson rs =
  C8.unlines
    [ "{",
      "  \"activities\": [",
      BS.intercalate ",\n" rs,
      "  ]",
      "}"
    ]

-- See NOTE: [Avoiding overlaps] for choosing good start dates for each
-- 3 types.
startDate :: Day
startDate = unsafeParse "1990-04-08"

dates :: Word -> List Timestamp
dates mx = TimestampDate . incDate startDate <$> [0 .. mx]

startTime :: LocalTime
startTime = unsafeParse "2000-04-08T12:00:00"

times :: Word -> List Timestamp
times mx = TimestampTime . incTime startTime <$> [0 .. mx]

startZoned :: ZonedTime
startZoned = unsafeParse "2010-04-08T12:00:00-0800"

zoneds :: Word -> List Timestamp
zoneds mx = TimestampZoned . incZoned startZoned <$> [0 .. mx]

encodeTimestamp :: Timestamp -> ByteString
encodeTimestamp = encodeUtf8 . TS.fmtTimestamp

incDate :: Day -> Word -> Day
incDate d i = Cal.addDays (fromIntegral @Word @Integer i) d

incTime :: LocalTime -> Word -> LocalTime
incTime (LocalTime d t) i = LocalTime (incDate d i) t

incZoned :: ZonedTime -> Word -> ZonedTime
incZoned (ZonedTime (LocalTime d t) z) i =
  ZonedTime (LocalTime (incDate d i) t) z

unsafeParse :: (Parser a) => Text -> a
unsafeParse = errorErr . P.parseAll
