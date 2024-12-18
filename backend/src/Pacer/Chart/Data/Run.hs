{-# LANGUAGE UndecidableInstances #-}

module Pacer.Chart.Data.Run
  ( -- * Run
    Run (..),
    RunTimestamp (..),

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

import Data.Aeson (ToJSON (toJSON))
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Set.NonEmpty qualified as NESet
import Data.Time qualified as Time
import Data.Time.Format qualified as Format
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    ZonedTime (ZonedTime),
  )
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Data.Distance
  ( Distance,
    DistanceUnit (Kilometer),
    HasDistance (HideDistance, distanceUnitOf, hideDistance),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance.Units
  ( ConvertDistance (ConvertedDistance, convertDistance_),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Pace (Pace, PaceDistF, SomePace)
import Pacer.Derive qualified as Derive
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import TOML
  ( DecodeTOML (tomlDecoder),
    Decoder,
  )
import TOML qualified

-- | Timestamp for runs.
data RunTimestamp
  = RunDay Day
  | RunLocalTime LocalTime
  | RunZonedTime ZonedTime
  deriving stock (Show)

instance Eq RunTimestamp where
  RunDay d1 == t2 = d1 == toDay t2
  RunLocalTime (LocalTime d1 _) == RunDay d2 = d1 == d2
  RunLocalTime l1 == RunLocalTime l2 = l1 == l2
  RunLocalTime l1 == RunZonedTime (ZonedTime l2 _) = l1 == l2
  RunZonedTime (ZonedTime (LocalTime d1 _) _) == RunDay d2 = d1 == d2
  RunZonedTime (ZonedTime l1 _) == RunLocalTime l2 = l1 == l2
  RunZonedTime z1 == RunZonedTime z2 =
    Time.zonedTimeToUTC z1 == Time.zonedTimeToUTC z2

instance Ord RunTimestamp where
  RunDay d1 <= t2 = d1 <= toDay t2
  RunLocalTime (LocalTime d1 _) <= RunDay d2 = d1 <= d2
  RunLocalTime l1 <= RunLocalTime l2 = l1 <= l2
  RunLocalTime l1 <= RunZonedTime (ZonedTime l2 _) = l1 <= l2
  RunZonedTime (ZonedTime (LocalTime d1 _) _) <= RunDay d2 = d1 <= d2
  RunZonedTime (ZonedTime l1 _) <= RunLocalTime l2 = l1 <= l2
  RunZonedTime z1 <= RunZonedTime z2 =
    Time.zonedTimeToUTC z1 <= Time.zonedTimeToUTC z2

toDay :: RunTimestamp -> Day
toDay (RunDay d) = d
toDay (RunLocalTime (LocalTime d _)) = d
toDay (RunZonedTime (ZonedTime (LocalTime d _) _)) = d

instance DecodeTOML RunTimestamp where
  tomlDecoder =
    RunDay
      <$> tomlDecoder
      <|> RunLocalTime
      <$> tomlDecoder
      <|> RunZonedTime
      <$> tomlDecoder

instance ToJSON RunTimestamp where
  toJSON (RunDay d) = toJSON d
  toJSON (RunLocalTime lt) = toJSON lt
  toJSON (RunZonedTime zt) = toJSON zt

fmtRunTimestamp :: RunTimestamp -> Text
fmtRunTimestamp =
  packText <<< \case
    RunDay d -> Format.formatTime l dfmt d
    RunLocalTime lt -> Format.formatTime l (dfmt ++ tfmt) lt
    RunZonedTime zt -> Format.formatTime l (dfmt ++ tfmt ++ zfmt) zt
  where
    dfmt = "%Y-%m-%d"
    tfmt = "T%H:%M:%S"
    zfmt = "%z"
    l = Format.defaultTimeLocale

-- | Type for runs.
type Run :: DistanceUnit -> Type -> Type
data Run dist a = MkRun
  { -- | The start time of the run.
    datetime :: RunTimestamp,
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

-- NOTE: ConvertDistance needs a Semifield instance presumably because
-- Run contains Positive, which needs it for construction.

instance
  ( FromInteger a,
    Ord a,
    Semifield a,
    Show a,
    SingI dist,
    SingI e
  ) =>
  ConvertDistance (Run dist a) e
  where
  type ConvertedDistance (Run dist a) e = Run e a

  convertDistance_ r =
    MkRun
      { datetime = r.datetime,
        distance = convertDistance_ r.distance,
        duration = r.duration,
        labels = r.labels,
        title = r.title
      }

instance (SingI dist) => HasDistance (Run dist a) where
  type HideDistance (Run dist a) = SomeRun a

  distanceUnitOf _ = fromSingI @_ @dist

  hideDistance = MkSomeRun (sing @dist)

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

-- | Existential quantifies a Run's distance.
type SomeRun :: Type -> Type
data SomeRun a where
  MkSomeRun :: Sing d -> Run d a -> SomeRun a

instance
  ( FromInteger a,
    Ord a,
    Semifield a,
    Show a,
    SingI e
  ) =>
  ConvertDistance (SomeRun a) e
  where
  type ConvertedDistance (SomeRun a) e = Run e a

  convertDistance_ (MkSomeRun s x) = withSingI s convertDistance_ x

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

instance HasDistance (SomeRun a) where
  type HideDistance (SomeRun a) = SomeRun a

  distanceUnitOf (MkSomeRun s _) = fromSing s

  hideDistance = id

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

-- | Key for 'SomeRuns'. Used to enforce that timestamps are unique.
newtype SomeRunsKey a = MkSomeRunsKey {unSomeRunsKey :: SomeRun a}

deriving stock instance (Show a) => Show (SomeRunsKey a)

instance Eq (SomeRunsKey a) where
  MkSomeRunsKey (MkSomeRun _ r1) == MkSomeRunsKey (MkSomeRun _ r2) =
    r1.datetime == r2.datetime

instance Ord (SomeRunsKey a) where
  MkSomeRunsKey (MkSomeRun _ r1) <= MkSomeRunsKey (MkSomeRun _ r2) =
    r1.datetime <= r2.datetime

-- | Holds multiple runs.
newtype SomeRuns a = MkSomeRuns (NESet (SomeRunsKey a))
  deriving stock (Eq, Show)

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
      (y@(MkSomeRun _ r) : ys) -> case eDuplicate of
        Left ((ts1, mTitle1), (ts2, mTitle2)) ->
          fail
            $ unpackText
            $ mconcat
              [ "Found overlapping timestamps\n - ",
                fmtTitle mTitle1,
                ": ",
                fmtRunTimestamp ts1,
                "\n - ",
                fmtTitle mTitle2,
                ": ",
                fmtRunTimestamp ts2
              ]
        Right mp ->
          pure $ MkSomeRuns $ NESet.fromList (toNonEmpty mp)
        where
          -- The logic here is:
          --
          -- 1. Transform parsed List -> Map Timestamp SomeRunsKey
          -- 2. If we don't receive any duplicates, transform
          --      Map Timestamp SomeRunsKey -> Set SomeRunsKey
          --
          -- Why don't we just immediately used a set, rather than the
          -- intermediate map? Because given some collision
          --
          --   (ts1, r1), (ts2, r2)
          --
          -- we want to report the timestamps ts1, ts2, and the titles if they
          -- exist, r1.title and r2.title. But if we just used a
          --
          --   Set (SomeRunsKey a)
          --
          -- all we can do is check for a collision on the timestamp
          -- (using its Eq/Ord). We would have no way to retrieve the original
          -- ts2 and r2 because there is no `lookup :: a -> Set a -> Maybe a`.
          --
          -- Hence we use a Map to store the entire original, and if all goes
          -- well, pass it to a Set.
          eDuplicate =
            foldr go (Right $ NEMap.singleton r.datetime (MkSomeRunsKey y)) ys

          go :: SomeRun a -> SomeRunsAcc a -> SomeRunsAcc a
          go _ (Left collision) = Left collision
          go someRun@(MkSomeRun _ q) (Right mp) =
            case NEMap.lookup q.datetime mp of
              Nothing -> Right $ NEMap.insert q.datetime (MkSomeRunsKey someRun) mp
              Just (MkSomeRunsKey (MkSomeRun _ collision)) ->
                Left ((q.datetime, q.title), (collision.datetime, collision.title))

          fmtTitle Nothing = "<no title>"
          fmtTitle (Just t) = t

type TitleAndTime = Tuple2 RunTimestamp (Maybe Text)

type SomeRunsAcc a =
  Either
    (Tuple2 TitleAndTime TitleAndTime)
    (NEMap RunTimestamp (SomeRunsKey a))

deriveSomePace ::
  ( FromInteger a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  SomeRun a ->
  SomePace a
deriveSomePace (MkSomeRun sr r) = case sr of
  SMeter -> hideDistance $ derivePace (convertDistance_ @_ @Kilometer r)
  SKilometer -> hideDistance $ derivePace r
  SMile -> hideDistance $ derivePace r

applySomeRun2 ::
  (FromInteger a, Ord a, Semifield a, Show a) =>
  (forall d. (SingI d) => Run d a -> Run d a -> r) ->
  SomeRun a ->
  SomeRun a ->
  r
applySomeRun2 f p1 p2 =
  DistU.convertToMeters_ p1 `f` DistU.convertToMeters_ p2
