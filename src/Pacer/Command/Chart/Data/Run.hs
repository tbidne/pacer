{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.Run
  ( -- * Run
    Run (..),

    -- ** Functions
    derivePace,

    -- * SomeRun
    SomeRun (..),
    someRunIso,

    -- ** Functions
    deriveSomePace,

    -- * SomeRuns
    SomeRunsKey (..),
    someRunsKeyIso,
    SomeRuns (..),

    -- ** Construction
    mkSomeRunsFail,
    mkSomeRuns,
    RunDatetimeOverlapE (..),

    -- ** Elimination
    someRunsToNE,
    someRunsToList,

    -- ** Functions
    mapSomeRuns,
    unionSomeRuns,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set.NonEmpty qualified as NESet
import Data.Tuple (uncurry)
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time (Timestamp)
import Pacer.Command.Chart.Data.Time qualified as Time
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
import Pacer.Data.Duration (Duration)
import Pacer.Data.Pace (Pace, PaceDistF, SomePace)
import Pacer.Prelude
import Pacer.Utils ((.:?:))
import Pacer.Utils qualified as Utils

-------------------------------------------------------------------------------
--                                    Run                                    --
-------------------------------------------------------------------------------

-- | Type for runs.
type Run :: DistanceUnit -> Type -> Type
data Run dist a = MkRun
  { -- | The start time of the run.
    datetime :: Timestamp,
    -- | The run's total distance.
    distance :: Distance dist a,
    -- | The run's total duration.
    duration :: Duration a,
    -- | Optional labels.
    labels :: Set Text,
    -- | Optional title.
    title :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

-- NOTE: ConvertDistance needs a Semifield instance presumably because
-- Run contains Positive, which needs it for construction.

instance
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a,
    SingI dist
  ) =>
  ConvertDistance (Run dist a)
  where
  type ConvertedDistance (Run dist a) e = Run e a
  type ToConstraints (Run dist a) _ = ()

  convertDistance_ r =
    MkRun
      { datetime = r.datetime,
        distance = convertDistance_ r.distance,
        duration = r.duration,
        labels = r.labels,
        title = r.title
      }

instance (SingI dist) => HasDistance (Run dist a) where
  type DistanceVal (Run dist a) = Distance dist a
  type HideDistance (Run dist a) = SomeRun a

  distanceUnitOf _ = fromSingI @_ @dist

  distanceOf = (.distance)

  hideDistance = MkSomeRun (sing @dist)

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Derives the pace from a run.
derivePace ::
  ( MGroup a,
    PaceDistF d
  ) =>
  Run d a ->
  Pace d a
derivePace r =
  Derive.derivePace
    r.distance
    r.duration

-------------------------------------------------------------------------------
--                                  SomeRun                                  --
-------------------------------------------------------------------------------

-- | Existential quantifies a Run's distance.
type SomeRun :: Type -> Type
data SomeRun a where
  MkSomeRun :: Sing d -> Run d a -> SomeRun a

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance (NFData a) => NFData (SomeRun a) where
  rnf (MkSomeRun s r) = s `deepseq` r `deepseq` ()

instance
  ( Fromℤ a,
    MetricSpace a,
    Semifield a,
    Ord a,
    Show a
  ) =>
  Eq (SomeRun a)
  where
  (==) = applySomeRun2 (==)

instance (Show a) => Show (SomeRun a) where
  showsPrec i (MkSomeRun s r) =
    showParen
      (i >= 11)
      ( showString "MkSomeRun "
          . showsPrec 11 s
          . showSpace
          . withSingI s showsPrec 11 r
      )

-------------------------------------------------------------------------------
--                               Serialization                               --
-------------------------------------------------------------------------------

instance
  ( Fromℚ a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  FromJSON (SomeRun a)
  where
  parseJSON = asnWithObject "SomeRun" $ \v -> do
    datetime <- v .: "datetime"

    someDistanceTxt <- v .: "distance"
    someDistance <- (failErr . P.parse $ someDistanceTxt)

    durationTxt <- v .: "duration"
    duration <- (failErr . P.parse $ durationTxt)

    labels <- v .:?: "labels"
    title <- v .:? "title"

    Utils.failUnknownFields
      "SomeRun"
      [ "datetime",
        "distance",
        "duration",
        "labels",
        "title"
      ]
      v

    pure $ case someDistance of
      MkSomeDistance s distance ->
        MkSomeRun s
          $ MkRun
            { datetime,
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
  ConvertDistance (SomeRun a)
  where
  type ConvertedDistance (SomeRun a) e = Run e a
  type ToConstraints (SomeRun a) _ = ()

  convertDistance_ (MkSomeRun s x) = withSingI s convertDistance_ x

instance HasDistance (SomeRun a) where
  type DistanceVal (SomeRun a) = SomeDistance a
  type HideDistance (SomeRun a) = SomeRun a

  distanceUnitOf (MkSomeRun s _) = fromSing s

  distanceOf (MkSomeRun s r) = withSingI s $ hideDistance $ distanceOf r

  hideDistance = id

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | 'Iso' between 'SomeRun' and 'Run'. Note that this converts the underlying
-- distance to the requested unit, hence the actual value _can_ change.
-- Thus the isomorphism must be understood in terms of the hypothetical
-- equivalence class on distance units.
someRunIso ::
  forall d a.
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a,
    SingI d
  ) =>
  Iso' (SomeRun a) (Run d a)
someRunIso =
  iso
    (\(MkSomeRun s r) -> (withSingI s convertDistance_ r))
    hideDistance

-------------------------------------------------------------------------------
--                                SomeRunsKey                                --
-------------------------------------------------------------------------------

-- | Key for 'SomeRuns'. Eq/Ord use an equivalence class on the timestamp,
-- used to enforce that timestamps are unique.
newtype SomeRunsKey a = MkSomeRunsKey {unSomeRunsKey :: SomeRun a}
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance Eq (SomeRunsKey a) where
  MkSomeRunsKey (MkSomeRun _ r1) == MkSomeRunsKey (MkSomeRun _ r2) =
    r1.datetime == r2.datetime

instance Ord (SomeRunsKey a) where
  MkSomeRunsKey (MkSomeRun _ r1) <= MkSomeRunsKey (MkSomeRun _ r2) =
    r1.datetime <= r2.datetime

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance HasDistance (SomeRunsKey a) where
  type DistanceVal (SomeRunsKey a) = SomeDistance a
  type HideDistance (SomeRunsKey a) = SomeRunsKey a

  distanceUnitOf :: SomeRunsKey a -> DistanceUnit
  distanceUnitOf (MkSomeRunsKey sr) = distanceUnitOf sr

  distanceOf :: SomeRunsKey a -> SomeDistance a
  distanceOf (MkSomeRunsKey sr) = distanceOf sr

  hideDistance = id

-------------------------------------------------------------------------------
--                                   Misc                                    --
-------------------------------------------------------------------------------

-- | 'Iso' between 'SomeRunsKey' and 'SomeRun'.
someRunsKeyIso :: Iso' (SomeRunsKey a) (SomeRun a)
someRunsKeyIso = iso (\(MkSomeRunsKey x) -> x) MkSomeRunsKey

-------------------------------------------------------------------------------
--                                  SomeRuns                                 --
-------------------------------------------------------------------------------

-- | Holds multiple runs.
newtype SomeRuns a = MkSomeRuns {unSomeRuns :: (NESet (SomeRunsKey a))}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance HasDistance (SomeRuns a) where
  type DistanceVal (SomeRuns a) = SomeDistance a
  type HideDistance (SomeRuns a) = SomeRuns a

  distanceUnitOf :: SomeRuns a -> DistanceUnit
  distanceUnitOf (MkSomeRuns (SetToSeqNE (srk :<|| _))) = distanceUnitOf srk

  distanceOf :: SomeRuns a -> SomeDistance a
  distanceOf (MkSomeRuns (SetToSeqNE (srk :<|| _))) = distanceOf srk

  hideDistance = id

-------------------------------------------------------------------------------
--                               Serialization                               --
-------------------------------------------------------------------------------

mkSomeRunsFail :: (MonadFail m) => List (SomeRun a) -> m (SomeRuns a)
mkSomeRunsFail xs = case xs of
  [] -> fail "Received empty list"
  (y : ys) -> case mkSomeRuns (y :| ys) of
    Err err -> fail $ displayException err
    Ok x -> pure x

instance
  ( Fromℚ a,
    Ord a,
    Semifield a,
    Show a,
    Parser a
  ) =>
  FromJSON (SomeRuns a)
  where
  parseJSON = asnWithObject "SomeRuns" $ \v -> do
    runs <- v .: "runs"
    Utils.failUnknownFields "SomeRuns" ["runs"] v
    mkSomeRunsFail runs

data RunDatetimeOverlapE = MkRunDatetimeOverlapE
  { r1 :: Tuple2 (Maybe Text) Timestamp,
    r2 :: Tuple2 (Maybe Text) Timestamp
  }
  deriving stock (Show)

instance Exception RunDatetimeOverlapE where
  displayException (MkRunDatetimeOverlapE x1 x2) =
    unpackText
      $ mconcat
        [ "Found overlapping timestamps\n - ",
          fmtTitle mTitle1,
          ": ",
          Time.fmtTimestamp ts1,
          "\n - ",
          fmtTitle mTitle2,
          ": ",
          Time.fmtTimestamp ts2
        ]
    where
      fmtTitle Nothing = "<no title>"
      fmtTitle (Just t) = t

      ((mTitle1, ts1), (mTitle2, ts2)) =
        if x1 <= x2
          then (x1, x2)
          else (x2, x1)

mkSomeRuns :: NonEmpty (SomeRun a) -> Result RunDatetimeOverlapE (SomeRuns a)
mkSomeRuns (y@(MkSomeRun _ r) :| ys) =
  bimap
    (uncurry MkRunDatetimeOverlapE)
    (MkSomeRuns . fst)
    eDuplicate
  where
    -- The logic here is:
    --
    -- 1. Transform parsed @List -> NESet (SomeRunsKey a)@
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
        ( NESet.singleton (MkSomeRunsKey y),
          initOverlapData
        )

    initOverlapData :: Map Timestamp OverlapData
    initOverlapData = runToOverlapMap r

    go :: SomeRun a -> SomeRunsAcc a -> SomeRunsAcc a
    go _ (Err overlap) = Err overlap
    go someRun@(MkSomeRun _ q) (Ok (acc, foundKeys)) =
      case checkOverlap q foundKeys of
        Ok foundKeys' ->
          let acc' = NESet.insert (MkSomeRunsKey someRun) acc
           in Ok (acc', foundKeys')
        Err overlapped -> Err ((q.title, q.datetime), overlapped)

-- | NOTE: [Timestamp Overlaps]
--
-- Timestamp data we use when checking for overlaps. First, we partition
-- timestamps into "primary" and "secondary" categories. Primary timestamps
-- are those that correspond to an actual Run i.e. run.datetime. Secondary
-- timestamps are all of the potential "overlaps" for primary timestamps. For
-- instance, given a run with timestamp
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
    -- to an actual Run's. The only caveat here is that it might not be
    -- "exact" equality in the case of zoned time e.g.
    --
    --   2024-08-10T13:15:30-0700
    --   2024-08-10T12:15:30-0800
    --
    -- will compare Eq because of UTC conversions, though they are not
    -- strictly the same data.
    OverlapPrimary Timestamp (Maybe Text)
  | -- | The value is a _secondary_ key i.e. the timestamp is merely an
    -- overlap for some run's actual timestamp.
    OverlapSecondary Timestamp (Maybe Text)

-- | Verifies that the given timestamp does not overlap with any map entries.
-- An overlap is defined as a primary timestamp equaling some other
-- (primary or secondary) timestamp.
checkOverlap ::
  -- | Timestamp to insert.
  Run d a ->
  -- | Map.
  Map Timestamp OverlapData ->
  -- | Error or new map, with the timestamp (and overlaps) inserted.
  Result TitleAndTime (Map Timestamp OverlapData)
checkOverlap run map = case Map.lookup run.datetime map of
  -- 1. The timestamp exists in the map, error.
  Just (OverlapPrimary origTs mTitle) -> Err (mTitle, origTs)
  Just (OverlapSecondary origTs mTitle) -> Err (mTitle, origTs)
  -- 2. Timestamp does not exist in the map, check its overlaps for a
  --    match.
  Nothing ->
    let overlaps = Time.strictOverlaps run.datetime
        init = runToOverlapMap' run.datetime run.title overlaps
     in (Map.union map) <$> foldr go (Ok init) overlaps
  where
    go ::
      Timestamp ->
      Result TitleAndTime (Map Timestamp OverlapData) ->
      Result TitleAndTime (Map Timestamp OverlapData)
    go t acc = case Map.lookup t map of
      -- 2.1 The timestamp has an overlap that exists as a _primary_
      --     key in the map, error.
      Just (OverlapPrimary origTs mTitle) -> Err (mTitle, origTs)
      -- 2.2 The timestamp either does not exist in the map or it only exists
      --     as a _secondary_ key. That is fine.
      _ -> acc

-- | Creates a map based on the run. Adds the run's timestamp as a Primary
-- entry and all potential overlaps as secondary entries.
runToOverlapMap :: Run d a -> Map Timestamp OverlapData
runToOverlapMap run =
  runToOverlapMap'
    run.datetime
    run.title
    (Time.strictOverlaps run.datetime)

-- | Helper for 'runToOverlapMap'. This exists entirely so we do not have to
-- calculate the overlaps twice in some places. It is intended to always
-- be called like:
--
-- @
--   runToOverlapMap' r.datetime r.title (Time.strictOverlaps r.datetime)
-- @
--
-- 'runToOverlapMap' should be preferred as it is safer to use correctly.
runToOverlapMap' ::
  Timestamp ->
  Maybe Text ->
  List Timestamp ->
  Map Timestamp OverlapData
runToOverlapMap' ts mTitle overlaps =
  Map.fromList
    $ (ts, OverlapPrimary ts mTitle)
    : fmap
      (\t -> (t, OverlapSecondary ts mTitle))
      overlaps

type TitleAndTime = Tuple2 (Maybe Text) Timestamp

type SomeRunsAcc a =
  Result
    (Tuple2 TitleAndTime TitleAndTime)
    (Tuple2 (NESet (SomeRunsKey a)) (Map Timestamp OverlapData))

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Derives a pace from some run.
deriveSomePace ::
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  SomeRun a ->
  SomePace a
deriveSomePace (MkSomeRun sr r) = case sr of
  SMeter -> hideDistance $ derivePace (DistU.convertToKilometers r)
  SKilometer -> hideDistance $ derivePace r
  SMile -> hideDistance $ derivePace r

applySomeRun2 ::
  (Fromℤ a, Ord a, Semifield a, Show a) =>
  (forall d. (SingI d) => Run d a -> Run d a -> r) ->
  SomeRun a ->
  SomeRun a ->
  r
applySomeRun2 f p1 p2 =
  DistU.convertToMeters p1 `f` DistU.convertToMeters p2

mapSomeRuns ::
  (forall d. Run d a -> Run d a) ->
  SomeRuns a ->
  SomeRuns a
mapSomeRuns f (MkSomeRuns s) = MkSomeRuns $ NESet.map g s
  where
    g (MkSomeRunsKey (MkSomeRun d r)) =
      MkSomeRunsKey (MkSomeRun d (f r))

unionSomeRuns ::
  SomeRuns a ->
  SomeRuns a ->
  Result RunDatetimeOverlapE (SomeRuns a)
unionSomeRuns xs ys = mkSomeRuns (toListSR xs <> toListSR ys)
  where
    toListSR :: SomeRuns a -> NonEmpty (SomeRun a)
    toListSR (MkSomeRuns rs) =
      fmap (\(MkSomeRunsKey sr) -> sr)
        . NESet.toList
        $ rs

someRunsToNE :: SomeRuns a -> NonEmpty (SomeRun a)
someRunsToNE = fmap (.unSomeRunsKey) . NESet.toList . (.unSomeRuns)

someRunsToList :: SomeRuns a -> List (SomeRun a)
someRunsToList = NE.toList . someRunsToNE

makeFieldLabelsNoPrefix ''Run
