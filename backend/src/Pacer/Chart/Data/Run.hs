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
import Data.Map.Strict qualified as Map
import Data.Sequence.NonEmpty (fromList)
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
      ( showString "MkSomeRun "
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
  ( FromInteger a,
    Ord a,
    Semifield a,
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

-- TODO: SomeRuns should maintain sorted order.
--
-- Could change this to an NESet. The main difficulty is that in
-- Pacer.Chart.Data.Chart, we have
--
--   mkChart (MkSomeRuns someRuns@((MkSomeRun @distUnit sd _) :<|| _)) request =
--
-- i.e. we destruct the SomeRuns in the function defn, and use distUnit in the
-- where clause. distUnit is an existential so we can only access this because
-- of the pattern match. Thus if we want to switch toe NESet w/ the least
-- amount of hassle, we need to create a PatternSynonym that allows peeking
-- the head element. This will require ViewPatterns.
--
-- Or maybe this is too hard, and we can rewrite mkChart w/o the top-level
-- pattern match (maybe we split the logic into a separate function).

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
        Right _ -> pure $ MkSomeRuns $ fromList (y :| ys)
        where
          eDuplicate = foldr go (Right $ Map.singleton r.datetime (r.datetime, r.title)) ys

          go :: SomeRun a -> SomeRunsAcc -> SomeRunsAcc
          go _ (Left collision) = Left collision
          go (MkSomeRun _ q) (Right mp) =
            case Map.lookup q.datetime mp of
              Nothing -> Right $ Map.insert q.datetime (q.datetime, q.title) mp
              Just collision -> Left ((q.datetime, q.title), collision)

          fmtTitle Nothing = "<no title>"
          fmtTitle (Just t) = t

type TitleAndTime = Tuple2 RunTimestamp (Maybe Text)

type SomeRunsAcc =
  Either
    (Tuple2 TitleAndTime TitleAndTime)
    (Map.Map RunTimestamp TitleAndTime)

-- | Converts arbitrary run to some distance unit.
convertSomeDistances ::
  forall d a.
  ( FromInteger a,
    Ord a,
    Semifield a,
    Show a,
    SingI d
  ) =>
  SomeRuns a ->
  NESeq (Run d a)
convertSomeDistances (MkSomeRuns rs) = convertSomeDistance <$> rs
