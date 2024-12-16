{-# LANGUAGE UndecidableInstances #-}

module Pacer.Chart.Data.Run
  ( -- * Run
    Run (..),
    RunTimestamp (..),
    toMeters,
    convertDistance,

    -- * SomeRun
    SomeRun (..),
    convertSomeDistance,

    -- * SomeRuns
    SomeRuns (..),
    convertSomeDistances,
  )
where

import Data.Aeson (ToJSON (toJSON))
import Data.Sequence.NonEmpty (fromList)
import Data.Time qualified as Time
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Data.Distance
  ( Distance,
    DistanceUnit (Meter),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance qualified as Dist
import Pacer.Data.Duration (Seconds)
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
  RunDay x == RunDay y = x == y
  RunLocalTime x == RunLocalTime y = x == y
  RunZonedTime x == RunZonedTime y =
    Time.zonedTimeToUTC x == Time.zonedTimeToUTC y
  _ == _ = False

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

-- | Converts arbitrary run to meters.
toMeters ::
  ( AMonoid a,
    FromInteger a,
    MSemigroup a,
    Ord a,
    Show a,
    SingI dist
  ) =>
  Run dist a ->
  Run Meter a
toMeters r =
  MkRun
    { datetime = r.datetime,
      distance = Dist.toMeters r.distance,
      duration = r.duration,
      labels = r.labels,
      title = r.title
    }

-- | Converts arbitrary run to some distance unit.
convertDistance ::
  forall d2 d1 a.
  ( AMonoid a,
    FromInteger a,
    MGroup a,
    Ord a,
    Show a,
    SingI d1,
    SingI d2
  ) =>
  Run d1 a ->
  Run d2 a
convertDistance r =
  MkRun
    { datetime = r.datetime,
      distance = Dist.convertDistance @d2 r.distance,
      duration = r.duration,
      labels = r.labels,
      title = r.title
    }

-- | Existential quantifies a Run's distance.
type SomeRun :: Type -> Type
data SomeRun a where
  MkSomeRun :: Sing d -> Run d a -> SomeRun a

instance
  ( AMonoid a,
    FromInteger a,
    MSemigroup a,
    MetricSpace a,
    Ord a,
    Show a
  ) =>
  Eq (SomeRun a)
  where
  MkSomeRun sx x == MkSomeRun sy y =
    withSingI sx
      $ withSingI sy
      $ toMeters x
      == toMeters y

instance (Show a) => Show (SomeRun a) where
  showsPrec i (MkSomeRun s r) =
    showParen
      (i >= 11)
      ( showString "MkDuration "
          . showsPrec 11 s
          . showSpace
          . withSingI s showsPrec 11 r
      )

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

-- | Converts arbitrary run to some distance unit.
convertSomeDistance ::
  forall d a.
  ( AMonoid a,
    FromInteger a,
    MGroup a,
    Ord a,
    Show a,
    SingI d
  ) =>
  SomeRun a ->
  Run d a
convertSomeDistance (MkSomeRun s r) =
  MkRun
    { datetime = r.datetime,
      distance = withSingI s $ Dist.convertDistance @d r.distance,
      duration = r.duration,
      labels = r.labels,
      title = r.title
    }

-- | Holds multiple runs.
newtype SomeRuns a = MkSomeRuns (NESeq (SomeRun a))
  deriving stock (Eq, Show)

-- TODO: Decoder should probably do validation e.g. ensure all datetimes are
-- unique.

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
      (y : ys) -> pure $ MkSomeRuns $ fromList (y :| ys)

-- | Converts arbitrary run to some distance unit.
convertSomeDistances ::
  forall d a.
  ( AMonoid a,
    FromInteger a,
    MGroup a,
    Ord a,
    Show a,
    SingI d
  ) =>
  SomeRuns a ->
  NESeq (Run d a)
convertSomeDistances (MkSomeRuns rs) = convertSomeDistance <$> rs
