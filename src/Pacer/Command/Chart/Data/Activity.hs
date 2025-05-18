{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.Activity
  ( -- * Activity
    Activity (..),
    ActivityType (..),
    Label (..),

    -- ** Functions
    derivePace,

    -- * SomeActivity
    SomeActivity (..),
    someActivityIso,
    someActivityApplyActivity,

    -- ** Functions
    deriveSomePace,

    -- * SomeActivities
    SomeActivitiesKey (..),
    someActivitiesKeyIso,
    SomeActivities (..),

    -- ** Construction
    mkSomeActivitiesFail,
    mkSomeActivities,
    ActivityDatetimeOverlapE (..),

    -- ** Elimination
    someActivitiesToNE,
    someActivitiesToList,

    -- ** Functions
    mapSomeActivities,
    mapAccumSomeActivities,
    unionSomeActivities,
  )
where

import Data.Aeson qualified as Asn
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Time.Relative qualified as RelTime
import Data.Tuple (uncurry)
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Command.Chart.Data.Time.Timestamp qualified as TS
import Pacer.Command.Derive qualified as Derive
import Pacer.Data.Distance
  ( Distance,
    DistanceUnit,
    HasDistance
      ( DistanceVal,
        HideDistance,
        distanceOf,
        distanceUnitOf,
        hideDistance
      ),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance.Units
  ( ConvertDistance (ConvertedDistance, convertDistance_),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration (unDuration))
import Pacer.Data.Pace (Pace, PaceDistF, SomePace)
import Pacer.Prelude
import Pacer.Utils ((.:?:))
import Pacer.Utils qualified as Utils

-------------------------------------------------------------------------------
--                                 Activity                                  --
-------------------------------------------------------------------------------

-- | Activity type.
newtype ActivityType = MkActivityType {unActivityType :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving newtype (Eq, FromJSON, IsString, Ord, ToJSON)

-- | Activity label.
newtype Label = MkLabel {unLabel :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving newtype (Eq, FromJSON, IsString, Ord, ToJSON)

-- | Type for activities.
type Activity :: DistanceUnit -> Type -> Type
data Activity dist a = MkActivity
  { -- | The type of the activity. This is only used in filtering, hence it
    -- is arguably redundant due to the existence of 'labels'. Its only
    -- justification is for interfacing with garmin (which has exactly one
    -- type), and though we could just parse that into labels, it is probably
    -- simpler to separate them.
    atype :: Maybe ActivityType,
    -- | The start time of the activity.
    datetime :: Timestamp,
    -- | The activity's total distance.
    distance :: Distance dist a,
    -- | The activity's total duration.
    duration :: Duration a,
    -- | Optional labels.
    labels :: Set Label,
    -- | Optional title.
    title :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

-- NOTE: ConvertDistance needs a Semifield instance presumably because
-- Activity contains Positive, which needs it for construction.

instance
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a,
    SingI dist
  ) =>
  ConvertDistance (Activity dist a)
  where
  type ConvertedDistance (Activity dist a) e = Activity e a
  type ToConstraints (Activity dist a) _ = ()

  convertDistance_ r =
    MkActivity
      { atype = r.atype,
        datetime = r.datetime,
        distance = convertDistance_ r.distance,
        duration = r.duration,
        labels = r.labels,
        title = r.title
      }

instance (SingI dist) => HasDistance (Activity dist a) where
  type DistanceVal (Activity dist a) = Distance dist a
  type HideDistance (Activity dist a) = SomeActivity a

  distanceUnitOf _ = fromSingI @_ @dist

  distanceOf = (.distance)

  hideDistance = MkSomeActivity (sing @dist)

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Derives the pace from a activity.
derivePace ::
  ( MGroup a,
    PaceDistF d
  ) =>
  Activity d a ->
  Pace d a
derivePace r =
  Derive.derivePace
    r.distance
    r.duration

-------------------------------------------------------------------------------
--                                SomeActivity                               --
-------------------------------------------------------------------------------

-- | Existential quantifies an Activity's distance.
type SomeActivity :: Type -> Type
data SomeActivity a where
  MkSomeActivity :: Sing d -> Activity d a -> SomeActivity a

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance (NFData a) => NFData (SomeActivity a) where
  rnf (MkSomeActivity s r) = s `deepseq` r `deepseq` ()

instance
  ( Fromℤ a,
    MetricSpace a,
    Semifield a,
    Ord a,
    Show a
  ) =>
  Eq (SomeActivity a)
  where
  (==) = applySomeActivity2 (==)

instance (Show a) => Show (SomeActivity a) where
  showsPrec i (MkSomeActivity s r) =
    showParen
      (i >= 11)
      ( showString "MkSomeActivity "
          . showsPrec 11 s
          . showSpace
          . withSingI s showsPrec 11 r
      )

-------------------------------------------------------------------------------
--                               Serialization                               --
-------------------------------------------------------------------------------

-- NOTE: The ToJSON test here is used entirely for logging. Consequently,
-- we do not have to care _too_ much about display e.g. the duration is
-- display as a "time string" (e.g. "5m30s") rather than our normal display,
-- (5'30"). This is a round trip, though if we ever want to display it, we
-- should change it.

instance
  ( Display a,
    Fromℤ a,
    MSemigroup a,
    Toℝ a
  ) =>
  ToJSON (SomeActivity a)
  where
  toJSON (MkSomeActivity sz r) =
    Asn.object
      $ [ "datetime" .= r.datetime,
          "distance" .= withSingI sz display r.distance,
          "duration" .= durationTimeString,
          "labels" .= r.labels
        ]
      ++ Utils.encodeMaybes
        [ ("title", r.title),
          ("type", fmap (.unActivityType) r.atype)
        ]
    where
      durationTimeString =
        RelTime.toString
          . RelTime.fromSeconds
          . floor
          . toℝ
          $ r.duration.unDuration.unPositive

instance
  ( Fromℚ a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  FromJSON (SomeActivity a)
  where
  parseJSON = asnWithObject "SomeActivity" $ \v -> do
    datetime <- v .: "datetime"

    someDistanceTxt <- v .: "distance"
    someDistance <- failErr . P.parseAll $ someDistanceTxt

    durationTxt <- v .: "duration"
    duration <- failErr . P.parseAll $ durationTxt

    labels <- v .:?: "labels"
    title <- v .:? "title"

    atype <- v .:? "type"

    Utils.failUnknownFields
      "SomeActivity"
      [ "datetime",
        "distance",
        "duration",
        "labels",
        "title",
        "type"
      ]
      v

    pure $ case someDistance of
      MkSomeDistance s distance ->
        MkSomeActivity s
          $ MkActivity
            { atype,
              datetime,
              distance,
              duration,
              labels,
              title
            }

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  ConvertDistance (SomeActivity a)
  where
  type ConvertedDistance (SomeActivity a) e = Activity e a
  type ToConstraints (SomeActivity a) _ = ()

  convertDistance_ (MkSomeActivity s x) = withSingI s convertDistance_ x

instance HasDistance (SomeActivity a) where
  type DistanceVal (SomeActivity a) = SomeDistance a
  type HideDistance (SomeActivity a) = SomeActivity a

  distanceUnitOf (MkSomeActivity s _) = fromSing s

  distanceOf (MkSomeActivity s r) = withSingI s $ hideDistance $ distanceOf r

  hideDistance = id

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Applies the function to the underlying activity.
someActivityApplyActivity :: (forall d. Activity d a -> b) -> SomeActivity a -> b
someActivityApplyActivity f (MkSomeActivity _ r) = f r

-- | 'Iso' between 'SomeActivity' and 'Activity'. Note that this converts the
-- underlying distance to the requested unit, hence the actual value _can_
-- change. Thus the isomorphism must be understood in terms of the
-- hypothetical equivalence class on distance units.
someActivityIso ::
  forall d a.
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a,
    SingI d
  ) =>
  Iso' (SomeActivity a) (Activity d a)
someActivityIso =
  iso
    (\(MkSomeActivity s r) -> withSingI s convertDistance_ r)
    hideDistance

-------------------------------------------------------------------------------
--                             SomeActivitiesKey                             --
-------------------------------------------------------------------------------

-- | Key for 'SomeActivities'. Eq/Ord use an equivalence class on the timestamp,
-- used to enforce that timestamps are unique.
newtype SomeActivitiesKey a
  = MkSomeActivitiesKey {unSomeActivitiesKey :: SomeActivity a}
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance HasField "atype" (SomeActivitiesKey a) (Maybe ActivityType) where
  getField (MkSomeActivitiesKey (MkSomeActivity _ r)) = r.atype

instance HasField "datetime" (SomeActivitiesKey a) Timestamp where
  getField (MkSomeActivitiesKey (MkSomeActivity _ r)) = r.datetime

instance HasField "duration" (SomeActivitiesKey a) (Duration a) where
  getField (MkSomeActivitiesKey (MkSomeActivity _ r)) = r.duration

instance HasField "labels" (SomeActivitiesKey a) (Set Label) where
  getField (MkSomeActivitiesKey (MkSomeActivity _ r)) = r.labels

instance HasField "title" (SomeActivitiesKey a) (Maybe Text) where
  getField (MkSomeActivitiesKey (MkSomeActivity _ r)) = r.title

instance Eq (SomeActivitiesKey a) where
  MkSomeActivitiesKey (MkSomeActivity _ r1)
    == MkSomeActivitiesKey (MkSomeActivity _ r2) =
      r1.datetime == r2.datetime

instance Ord (SomeActivitiesKey a) where
  MkSomeActivitiesKey (MkSomeActivity _ r1)
    <= MkSomeActivitiesKey (MkSomeActivity _ r2) =
      r1.datetime <= r2.datetime

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance HasDistance (SomeActivitiesKey a) where
  type DistanceVal (SomeActivitiesKey a) = SomeDistance a
  type HideDistance (SomeActivitiesKey a) = SomeActivitiesKey a

  distanceUnitOf :: SomeActivitiesKey a -> DistanceUnit
  distanceUnitOf (MkSomeActivitiesKey sr) = distanceUnitOf sr

  distanceOf :: SomeActivitiesKey a -> SomeDistance a
  distanceOf (MkSomeActivitiesKey sr) = distanceOf sr

  hideDistance = id

-------------------------------------------------------------------------------
--                                   Misc                                    --
-------------------------------------------------------------------------------

-- | 'Iso' between 'SomeActivitiesKey' and 'SomeActivity'.
someActivitiesKeyIso :: Iso' (SomeActivitiesKey a) (SomeActivity a)
someActivitiesKeyIso = iso (\(MkSomeActivitiesKey x) -> x) MkSomeActivitiesKey

-------------------------------------------------------------------------------
--                               SomeActivities                              --
-------------------------------------------------------------------------------

-- | Holds multiple activities.
newtype SomeActivities a
  = MkSomeActivities {unSomeActivities :: NESet (SomeActivitiesKey a)}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance HasDistance (SomeActivities a) where
  type DistanceVal (SomeActivities a) = SomeDistance a
  type HideDistance (SomeActivities a) = SomeActivities a

  distanceUnitOf :: SomeActivities a -> DistanceUnit
  distanceUnitOf (MkSomeActivities (SetToSeqNE (srk :<|| _))) =
    distanceUnitOf srk

  distanceOf :: SomeActivities a -> SomeDistance a
  distanceOf (MkSomeActivities (SetToSeqNE (srk :<|| _))) = distanceOf srk

  hideDistance = id

-------------------------------------------------------------------------------
--                               Serialization                               --
-------------------------------------------------------------------------------

mkSomeActivitiesFail :: (MonadFail m) => List (SomeActivity a) -> m (SomeActivities a)
mkSomeActivitiesFail xs = case xs of
  [] -> fail "Received empty list"
  (y : ys) -> case mkSomeActivities (y :| ys) of
    Err err -> fail $ displayException err
    Ok x -> pure x

instance
  ( Fromℚ a,
    Ord a,
    Semifield a,
    Show a,
    Parser a
  ) =>
  FromJSON (SomeActivities a)
  where
  parseJSON = asnWithObject "SomeActivities" $ \v -> do
    activities <- v .: "activities"
    Utils.failUnknownFields "SomeActivities" ["activities"] v
    mkSomeActivitiesFail activities

data ActivityDatetimeOverlapE = MkActivityDatetimeOverlapE
  { r1 :: Tuple2 (Maybe Text) Timestamp,
    r2 :: Tuple2 (Maybe Text) Timestamp
  }
  deriving stock (Show)

instance Exception ActivityDatetimeOverlapE where
  displayException (MkActivityDatetimeOverlapE x1 x2) =
    unpackText
      $ mconcat
        [ "Found overlapping timestamps\n - ",
          fmtTitle mTitle1,
          ": ",
          TS.fmtTimestamp ts1,
          "\n - ",
          fmtTitle mTitle2,
          ": ",
          TS.fmtTimestamp ts2
        ]
    where
      fmtTitle Nothing = "<no title>"
      fmtTitle (Just t) = t

      ((mTitle1, ts1), (mTitle2, ts2)) =
        if x1 <= x2
          then (x1, x2)
          else (x2, x1)

mkSomeActivities :: NonEmpty (SomeActivity a) -> Result ActivityDatetimeOverlapE (SomeActivities a)
mkSomeActivities (y@(MkSomeActivity _ r) :| ys) =
  bimap
    (uncurry MkActivityDatetimeOverlapE)
    (MkSomeActivities . fst)
    eDuplicate
  where
    -- The logic here is:
    --
    -- 1. Transform parsed @List -> NESet (SomeActivitiesKey a)@
    -- 2. Simultaneously, accumulate a @Map Timestamp OverlapData@ to check
    --    for overlaps.
    --
    -- Why don't we just use the NESet to check for overlaps?
    --
    -- Because Timestamp's Ord will not detect the overlaps we want.
    -- For instance, the timestamps 2013-10-08 and 2013-10-08T12:14:30
    -- will compare non-equal (which we need for Eq/Ord to be lawful),
    -- but we want these to compare Eq for purposes of detecting
    -- overlaps. Thus we cannot rely solely on Timestamp's Ord for this, hence
    -- naive Set/Map etc. are out.
    --
    -- Instead, when given a timestamp @ts@, we @ts@ and all of it's
    -- "overlaps" to the Map. So for 2013-10-08T12:14:30, we'd have key/vals:
    --
    --     2013-10-08T12:14:30 => OverlapData1
    --     2013-10-08 => OverlapData2
    --
    -- OverlapData1 is considered "primary", whereas OverlapData2 is
    -- "secondary". When inserting @ts@, we verify two conditions:
    --
    -- See NOTE: [Timestamp Overlaps] for details.
    eDuplicate = foldr go initVal ys

    initVal =
      Ok
        ( NESet.singleton (MkSomeActivitiesKey y),
          initOverlapData
        )

    initOverlapData :: HashMap Timestamp OverlapData
    initOverlapData = activityToOverlapMap r

    -- NOTE: [SomeActivities error short-circuit]
    --
    -- Notice anything unfortunate about this function? It is not lazy in its
    -- right argument, despite being called with foldr! This is a shame as it
    -- means it will not short-circuit in the event of an error. While we need
    -- the accumulating map to actually check for errors (hence it is unclear
    -- how to achieve short-cicuiting w/ foldr), surely we could rewrite this
    -- to short-circuit with ordinary recursion?
    --
    -- Alas, there is a complication. At this point, we have already decoded
    -- the bytestring into @List (SomeActivity a)@, so we are stuck with linear
    -- time anyway. What we'd want is a way to stream the decoding and check
    -- for errors right from the get go.
    --
    -- Manual testing with both foldl' and direct recursion possibly showed
    -- a (very) minor improvement on the larger benchmarks (10,000 activities),
    -- but it was so modest that it is hard to distinguish from noise. For now,
    -- we will stick with the status quo, with an eye towards real streaming.
    go :: SomeActivity a -> SomeActivitiesAcc a -> SomeActivitiesAcc a
    go _ (Err overlap) = Err overlap
    go someActivity@(MkSomeActivity _ q) (Ok (acc, foundKeys)) =
      case checkOverlap q foundKeys of
        Ok foundKeys' ->
          let acc' = NESet.insert (MkSomeActivitiesKey someActivity) acc
           in Ok (acc', foundKeys')
        Err overlapped -> Err ((q.title, q.datetime), overlapped)

-- REVIEW: Now that I think about it, this elaborate overlap detection might
-- not be worth it. First, note that Timestamp's Eq/Ord is lawful exactly
-- because it does not detect overlaps: Timestamps w/ less precision are
-- always considered "lesser" than those with more.
--
-- The entire point of overlap detection is for preventing ambiguities when
-- creating charts e.g. if we have activities with datetimes:
--
--     2024-10-08
--     2024-10-08T12:30:00
--
-- How do we sort these? Well, our Eq/Ord always does just fine by arbtirarily
-- deciding less precision means less. And if the user supplies an actual
-- duplicate e.g. another 2024-10-08, well that is always an error.
--
-- So what does overlap detection buy us? Arguably not much. If we just resort
-- to our Eq/Ord then duplicate detect is much simple, and we do not really
-- lose much. Consider removing it.

-- | NOTE: [Timestamp Overlaps]
--
-- Timestamp data we use when checking for overlaps. First, we partition
-- timestamps into "primary" and "secondary" categories. Primary timestamps
-- are those that correspond to an actual Activity i.e. activity.datetime.
-- Secondary timestamps are all of the potential "overlaps" for primary
-- timestamps. For instance, given an activity with timestamp
--
-- @
--    ts := "2024-08-10T13:15:30-0700"
-- @
--
-- Then we will have:
--
-- @
--   "2024-08-10T13:15:30-0700" -- primary
--   "2024-08-10T13:15:30"      -- secondary
--   "2024-08-10"               -- secondary
-- @
--
-- An "overlap error" is defined as having a primary timestamp duplicate some
-- other (primary or secondary) timestamp.
--
-- Then when checking a new timestamp @ts@ for uniqueness, we verify:
--
-- 1. @ts@ does not exist in the map as a _primary_ or _secondary_ key.
--
--     Both of these are errors. The former means we have an ordinary duplicate,
--     whereas the latter is an overlap.
--
-- 2. None of @ts@'s overlaps (hence, secondary keys) exist in the map as
--    a primary key.
--
-- Notice that encountering duplicate secondary keys is fine e.g.
--
-- @
--   "2024-08-10T13:15:30"
--   "2024-08-10T12:15:30"
-- @
--
-- will have the same secondary key @2024-08-10@, but that is not a conflict.
data OverlapData
  = -- | The value is a _primary_ key i.e. the timestamp corresponds
    -- to an actual Activity's. The only caveat here is that it might not be
    -- "exact" equality in the case of zoned time e.g.
    --
    --   2024-08-10T13:15:30-0700
    --   2024-08-10T12:15:30-0800
    --
    -- will compare Eq because of UTC conversions, though they are not
    -- strictly the same data.
    OverlapPrimary Timestamp (Maybe Text)
  | -- | The value is a _secondary_ key i.e. the timestamp is merely an
    -- overlap for some activity's actual timestamp.
    OverlapSecondary Timestamp (Maybe Text)

-- | Verifies that the given timestamp does not overlap with any map entries.
-- An overlap is defined as a primary timestamp equaling some other
-- (primary or secondary) timestamp.
checkOverlap ::
  -- | Timestamp to insert.
  Activity d a ->
  -- | Map.
  HashMap Timestamp OverlapData ->
  -- | Error or new map, with the timestamp (and overlaps) inserted.
  Result TitleAndTime (HashMap Timestamp OverlapData)
checkOverlap activity map = case HMap.lookup activity.datetime map of
  -- 1. The timestamp exists in the map, error.
  Just (OverlapPrimary origTs mTitle) -> Err (mTitle, origTs)
  Just (OverlapSecondary origTs mTitle) -> Err (mTitle, origTs)
  -- 2. Timestamp does not exist in the map, check its overlaps for a
  --    match.
  Nothing ->
    let overlaps = TS.strictOverlaps activity.datetime
        init = activityToOverlapMap' activity.datetime activity.title overlaps
     in HMap.union map <$> foldr go (Ok init) overlaps
  where
    go ::
      Timestamp ->
      Result TitleAndTime (HashMap Timestamp OverlapData) ->
      Result TitleAndTime (HashMap Timestamp OverlapData)
    go t acc = case HMap.lookup t map of
      -- 2.1 The timestamp has an overlap that exists as a _primary_
      --     key in the map, error.
      Just (OverlapPrimary origTs mTitle) -> Err (mTitle, origTs)
      -- 2.2 The timestamp either does not exist in the map or it only exists
      --     as a _secondary_ key. That is fine.
      _ -> acc

-- | Creates a map based on the activity. Adds the activity's timestamp as a
-- Primary entry and all potential overlaps as secondary entries.
activityToOverlapMap :: Activity d a -> HashMap Timestamp OverlapData
activityToOverlapMap activity =
  activityToOverlapMap'
    activity.datetime
    activity.title
    (TS.strictOverlaps activity.datetime)

-- | Helper for 'activityToOverlapMap'. This exists entirely so we do not have
-- to calculate the overlaps twice in some places. It is intended to always
-- be called like:
--
-- @
--   activityToOverlapMap' r.datetime r.title (TS.strictOverlaps r.datetime)
-- @
--
-- 'activityToOverlapMap' should be preferred as it is safer to use correctly.
activityToOverlapMap' ::
  Timestamp ->
  Maybe Text ->
  List Timestamp ->
  HashMap Timestamp OverlapData
activityToOverlapMap' ts mTitle overlaps =
  HMap.fromList
    $ (ts, OverlapPrimary ts mTitle)
    : fmap
      (,OverlapSecondary ts mTitle)
      overlaps

type TitleAndTime = Tuple2 (Maybe Text) Timestamp

type SomeActivitiesAcc a =
  Result
    (Tuple2 TitleAndTime TitleAndTime)
    (Tuple2 (NESet (SomeActivitiesKey a)) (HashMap Timestamp OverlapData))

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Derives a pace from some activity.
deriveSomePace ::
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  SomeActivity a ->
  SomePace a
deriveSomePace (MkSomeActivity sr r) = case sr of
  SMeter -> hideDistance $ derivePace (DistU.convertToKilometers r)
  SKilometer -> hideDistance $ derivePace r
  SMile -> hideDistance $ derivePace r

applySomeActivity2 ::
  (Fromℤ a, Ord a, Semifield a, Show a) =>
  (forall d. (SingI d) => Activity d a -> Activity d a -> r) ->
  SomeActivity a ->
  SomeActivity a ->
  r
applySomeActivity2 f p1 p2 =
  DistU.convertToMeters p1 `f` DistU.convertToMeters p2

mapSomeActivities ::
  (forall d. Activity d a -> Activity d b) ->
  SomeActivities a ->
  SomeActivities b
mapSomeActivities f (MkSomeActivities s) = MkSomeActivities $ NESet.map g s
  where
    g (MkSomeActivitiesKey (MkSomeActivity d r)) =
      MkSomeActivitiesKey (MkSomeActivity d (f r))

mapAccumSomeActivities ::
  forall a1 a2 b.
  (Semigroup b) =>
  (forall d. Activity d a1 -> Tuple2 (Activity d a2) b) ->
  SomeActivities a1 ->
  Tuple2 (SomeActivities a2) b
mapAccumSomeActivities f (MkSomeActivities s) = (MkSomeActivities newActivities, result)
  where
    (newActivities, result) = Set.foldl' g init rest

    g ::
      Tuple2 (NESet (SomeActivitiesKey a2)) b ->
      SomeActivitiesKey a1 ->
      Tuple2 (NESet (SomeActivitiesKey a2)) b
    g (rs, bs) (MkSomeActivitiesKey (MkSomeActivity d r)) =
      let (newActivity, b) = f r
       in ( NESet.insert (MkSomeActivitiesKey (MkSomeActivity d newActivity)) rs,
            b <> bs
          )

    root :: SomeActivitiesKey a1
    rest :: Set (SomeActivitiesKey a1)
    (root, rest) = NESet.deleteFindMin s

    initSet :: NESet (SomeActivitiesKey a2)
    (initSet, initB) = case root of
      MkSomeActivitiesKey (MkSomeActivity d r) ->
        let (activity1, b1) = f r
         in ( NESet.singleton (MkSomeActivitiesKey (MkSomeActivity d activity1)),
              b1
            )

    init :: Tuple2 (NESet (SomeActivitiesKey a2)) b
    init = (initSet, initB)

unionSomeActivities ::
  SomeActivities a ->
  SomeActivities a ->
  Result ActivityDatetimeOverlapE (SomeActivities a)
unionSomeActivities xs ys = mkSomeActivities (toListSR xs <> toListSR ys)
  where
    toListSR :: SomeActivities a -> NonEmpty (SomeActivity a)
    toListSR (MkSomeActivities rs) =
      fmap (\(MkSomeActivitiesKey sr) -> sr)
        . NESet.toList
        $ rs

someActivitiesToNE :: SomeActivities a -> NonEmpty (SomeActivity a)
someActivitiesToNE =
  fmap (.unSomeActivitiesKey)
    . NESet.toList
    . (.unSomeActivities)

someActivitiesToList :: SomeActivities a -> List (SomeActivity a)
someActivitiesToList = NE.toList . someActivitiesToNE

makeFieldLabelsNoPrefix ''Activity
