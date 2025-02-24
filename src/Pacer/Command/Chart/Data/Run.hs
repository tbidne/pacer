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
    mkSomeRunsFail,
    mkSomeRuns,
    RunDatetimeOverlapE (..),
    mapSomeRuns,
    unionSomeRuns,
  )
where

import Data.Foldable qualified as F
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set.NonEmpty qualified as NESet
import Data.Tuple (uncurry)
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time (Timestamp)
import Pacer.Command.Chart.Data.Time qualified as T
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
mkSomeRuns (y@(MkSomeRun _ _) :| ys) =
  bimap
    (uncurry MkRunDatetimeOverlapE)
    (MkSomeRuns . NESet.fromList . toNonEmpty)
    eDuplicate
  where
    -- The logic here is:
    --
    -- 1. Transform parsed List -> NonEmpty (SomeRunsKey a)
    -- 2. If we don't receive any duplicates, transform
    --      NonEmpty (SomeRunsKey a) -> Set (SomeRunsKey a)
    --
    -- Why don't we just immediately used a set, rather than the
    -- intermediate NonEmpty?
    --
    -- Because Timestamp's Ord will not detect the duplicates we want.
    -- For instance, the timestamps 2013-10-08 and 2013-10-08T12:14:30
    -- will compare non-equal (which we need for Eq/Ord to be lawful),
    -- but we want these to compare Eq for purposes of detecting
    -- overlaps. Thus we cannot use Timestamp's Ord for this, hence
    -- Set/Map etc. are out.
    --
    -- Instead, we iterate manually, looking for overlaps using the
    -- bespoke 'T.overlaps' function. Unfortuanately this is O(n^2),
    -- and it is difficult to see a way around this, as the overlap
    -- function is inherently intransitive e.g. consider
    --
    --   x = 2013-10-08T12:14:30
    --   y = 2013-10-08
    --   z = 2013-10-08T12:14:40
    --
    -- For overlaps, we want x == y == z and x /= z. It is unclear how
    -- to preserve this and achieve O(1) (or O(lg n)) lookup. Hopefully
    -- this does not impact performance too much.
    eDuplicate = foldr go (Ok $ NonEmpty.singleton (MkSomeRunsKey y)) ys

    go :: SomeRun a -> SomeRunsAcc a -> SomeRunsAcc a
    go _ (Err collision) = Err collision
    go someRun@(MkSomeRun _ q) (Ok someRuns) =
      case findOverlap someRun someRuns of
        Nothing -> Ok $ (MkSomeRunsKey someRun) <| someRuns
        Just (MkSomeRunsKey (MkSomeRun _ collision)) ->
          Err ((q.title, q.datetime), (collision.title, collision.datetime))

type TitleAndTime = Tuple2 (Maybe Text) Timestamp

type SomeRunsAcc a =
  Result
    (Tuple2 TitleAndTime TitleAndTime)
    (NonEmpty (SomeRunsKey a))

-- | Look for overlaps, O(n).
findOverlap :: SomeRun a -> NonEmpty (SomeRunsKey a) -> Maybe (SomeRunsKey a)
findOverlap (MkSomeRun _ r1) = F.find p
  where
    -- TODO: [Timestamp overlap lookup]
    --
    -- It would be great if we found a decent way to account for overlaps that
    -- doesn't require O(n) lookup e.g. at least O(lg n). For instance, maybe we
    -- can store all possible "least upper bounds" for a given timestamp, and
    -- lookup on that.

    p (MkSomeRunsKey (MkSomeRun _ r2)) = T.overlaps r1.datetime r2.datetime

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

makeFieldLabelsNoPrefix ''Run
