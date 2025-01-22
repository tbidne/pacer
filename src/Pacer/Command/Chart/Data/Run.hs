{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.Run
  ( -- * Run
    Run (..),

    -- ** Functions
    derivePace,

    -- * SomeRun
    SomeRun (..),

    -- ** Functions
    deriveSomePace,

    -- * SomeRuns
    SomeRunsKey (..),
    SomeRuns (..),
  )
where

import Data.Foldable qualified as F
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set.NonEmpty qualified as NESet
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
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Pace (Pace, PaceDistF, SomePace)
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import TOML
  ( DecodeTOML (tomlDecoder),
    Decoder,
  )
import TOML qualified

-------------------------------------------------------------------------------
--                                    Run                                    --
-------------------------------------------------------------------------------

-- | Type for runs.
type Run :: DistanceUnit -> Type -> Type
data Run dist a = MkRun
  { -- | The start time of the run.
    datetime :: Timestamp,
    -- | The run's total distance.
    distance :: Distance dist (Positive a),
    -- | The run's total duration.
    duration :: Seconds (Positive a),
    -- | Optional labels.
    labels :: List Text,
    -- | Optional title.
    title :: Maybe Text
  }
  deriving stock (Eq, Show)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

-- NOTE: ConvertDistance needs a Semifield instance presumably because
-- Run contains Positive, which needs it for construction.

instance
  ( FromInteger a,
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
  type DistanceVal (Run dist a) = Distance dist (Positive a)
  type HideDistance (Run dist a) = SomeRun a

  distanceUnitOf _ = fromSingI @_ @dist

  distanceOf = (.distance)

  hideDistance = MkSomeRun (sing @dist)

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Derives the pace from a run.
derivePace ::
  ( FromInteger a,
    MGroup a,
    PaceDistF d
  ) =>
  Run d a ->
  Pace d a
derivePace r =
  Derive.derivePace
    r.distance
    ((.unPositive) <$> r.duration)

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

instance
  ( FromInteger a,
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
  ( FromRational a,
    Ord a,
    Parser (Positive a),
    Semifield a,
    Show a
  ) =>
  DecodeTOML (SomeRun a)
  where
  tomlDecoder = do
    datetime <- TOML.getFieldWith tomlDecoder "datetime"
    someDistance <- TOML.getFieldWith decodeDistance "distance"
    duration <- TOML.getFieldWith decodeDuration "duration"
    labels <- Utils.getFieldOptArrayOf "labels"
    title <- TOML.getFieldOptWith tomlDecoder "title"

    case someDistance of
      MkSomeDistance s distance ->
        pure
          $ MkSomeRun s
          $ MkRun
            { datetime,
              distance,
              duration,
              labels,
              title
            }

decodeDistance :: (FromRational a, Parser a) => Decoder (SomeDistance a)
decodeDistance = tomlDecoder >>= P.parseFail

decodeDuration :: forall a. (Parser (Seconds a)) => Decoder (Seconds a)
decodeDuration = tomlDecoder >>= P.parseFail

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance
  ( FromInteger a,
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
  type DistanceVal (SomeRun a) = SomeDistance (Positive a)
  type HideDistance (SomeRun a) = SomeRun a

  distanceUnitOf (MkSomeRun s _) = fromSing s

  distanceOf (MkSomeRun s r) = withSingI s $ hideDistance $ distanceOf r

  hideDistance = id

-------------------------------------------------------------------------------
--                                SomeRunsKey                                --
-------------------------------------------------------------------------------

-- | Key for 'SomeRuns'. Eq/Ord use an equivalence class on the timestamp,
-- used to enforce that timestamps are unique.
newtype SomeRunsKey a = MkSomeRunsKey {unSomeRunsKey :: SomeRun a}

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

deriving stock instance (Show a) => Show (SomeRunsKey a)

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
  type DistanceVal (SomeRunsKey a) = SomeDistance (Positive a)
  type HideDistance (SomeRunsKey a) = SomeRunsKey a

  distanceUnitOf :: SomeRunsKey a -> DistanceUnit
  distanceUnitOf (MkSomeRunsKey sr) = distanceUnitOf sr

  distanceOf :: SomeRunsKey a -> SomeDistance (Positive a)
  distanceOf (MkSomeRunsKey sr) = distanceOf sr

  hideDistance = id

-------------------------------------------------------------------------------
--                                  SomeRuns                                 --
-------------------------------------------------------------------------------

-- | Holds multiple runs.
newtype SomeRuns a = MkSomeRuns {unSomeRuns :: (NESet (SomeRunsKey a))}
  deriving stock (Eq, Show)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance HasDistance (SomeRuns a) where
  type DistanceVal (SomeRuns a) = SomeDistance (Positive a)
  type HideDistance (SomeRuns a) = SomeRuns a

  distanceUnitOf :: SomeRuns a -> DistanceUnit
  distanceUnitOf (MkSomeRuns (SetToSeqNE (srk :<|| _))) = distanceUnitOf srk

  distanceOf :: SomeRuns a -> SomeDistance (Positive a)
  distanceOf (MkSomeRuns (SetToSeqNE (srk :<|| _))) = distanceOf srk

  hideDistance = id

-------------------------------------------------------------------------------
--                               Serialization                               --
-------------------------------------------------------------------------------

instance
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a,
    Parser (Positive a)
  ) =>
  DecodeTOML (SomeRuns a)
  where
  tomlDecoder = do
    xs <- TOML.getFieldWith (TOML.getArrayOf tomlDecoder) "runs"
    case xs of
      [] -> fail "Received empty list"
      (y@(MkSomeRun _ _) : ys) -> case eDuplicate of
        Left ((ts1, mTitle1), (ts2, mTitle2)) ->
          fail
            $ unpackText
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
        Right mp -> pure $ MkSomeRuns $ NESet.fromList (toNonEmpty mp)
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
          eDuplicate =
            foldr go (Right $ NonEmpty.singleton (MkSomeRunsKey y)) ys

          go :: SomeRun a -> SomeRunsAcc a -> SomeRunsAcc a
          go _ (Left collision) = Left collision
          go someRun@(MkSomeRun _ q) (Right someRuns) =
            case findOverlap someRun someRuns of
              Nothing -> Right $ (MkSomeRunsKey someRun) <| someRuns
              Just (MkSomeRunsKey (MkSomeRun _ collision)) ->
                Left ((q.datetime, q.title), (collision.datetime, collision.title))

          fmtTitle Nothing = "<no title>"
          fmtTitle (Just t) = t

type TitleAndTime = Tuple2 Timestamp (Maybe Text)

type SomeRunsAcc a =
  Either
    (Tuple2 TitleAndTime TitleAndTime)
    (NonEmpty (SomeRunsKey a))

-- | Look for overlaps, O(n).
findOverlap :: SomeRun a -> NonEmpty (SomeRunsKey a) -> Maybe (SomeRunsKey a)
findOverlap (MkSomeRun _ r1) = F.find p
  where
    p (MkSomeRunsKey (MkSomeRun _ r2)) = T.overlaps r1.datetime r2.datetime

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Derives a pace from some run.
deriveSomePace ::
  ( FromInteger a,
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
  (FromInteger a, Ord a, Semifield a, Show a) =>
  (forall d. (SingI d) => Run d a -> Run d a -> r) ->
  SomeRun a ->
  SomeRun a ->
  r
applySomeRun2 f p1 p2 =
  DistU.convertToMeters p1 `f` DistU.convertToMeters p2
