{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Chart
  ( -- * Params
    ChartParams (..),
    ChartPhase (..),
    ChartParamsArgs,
    ChartParamsFinal,
    advancePhase,

    -- * Functions
    createChartsJsonFile,
    createChartsJsonBS,

    -- * Default paths
    defChartRequestsPath,
    defRunsPath,
    defOutJsonPath,
  )
where

import Data.Aeson.Encode.Pretty
  ( Config (confIndent, confTrailingNewline),
    Indent (Spaces),
  )
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Pacer.Chart.Data.Chart (Chart)
import Pacer.Chart.Data.Chart qualified as Chart
import Pacer.Chart.Data.ChartRequest (ChartRequests)
import Pacer.Chart.Data.Run (SomeRuns)
import Pacer.Prelude
import System.Directory.OsPath qualified as Dir
import System.OsPath qualified as OsPath
import TOML (DecodeTOML, decode)

-- | Phase for 'ChartParams'.
data ChartPhase
  = ChartArgs
  | ChartFinal
  deriving stock (Eq, Show)

-- | Type familiy for evolving optional params.
type MaybePhaseF :: ChartPhase -> Type -> Type
type family MaybePhaseF p a where
  MaybePhaseF ChartArgs a = Maybe a
  MaybePhaseF ChartFinal a = a

-- | Params for making charts. The parameter is so we can re-use this type
-- for when providing paths are optional (CLI) and when they are mandatory
-- ('createChartsJsonFile', after filling in missing values with defaults).
type ChartParams :: ChartPhase -> Type
data ChartParams p = MkChartParams
  { -- | Path to input chart-requests.toml file.
    chartRequestsPath :: MaybePhaseF p OsPath,
    -- | Path to output charts.json file.
    outJsonPath :: MaybePhaseF p OsPath,
    -- | Path to input runs.toml file.
    runsPath :: MaybePhaseF p OsPath
  }

deriving stock instance (Eq (MaybePhaseF p OsPath)) => Eq (ChartParams p)

deriving stock instance (Show (MaybePhaseF p OsPath)) => Show (ChartParams p)

type ChartParamsArgs = ChartParams ChartArgs

type ChartParamsFinal = ChartParams ChartFinal

instance Semigroup ChartParamsArgs where
  MkChartParams x1 x2 x3 <> MkChartParams y1 y2 y3 =
    MkChartParams (x1 <|> y1) (x2 <|> y2) (x3 <|> y3)

instance Monoid ChartParamsArgs where
  mempty = MkChartParams mempty mempty mempty

-- | Given 'ChartParamsFinal', generates a json-encoded array of charts, and
-- writes the file to the given location.
createChartsJsonFile :: ChartParamsFinal -> IO ()
createChartsJsonFile params = do
  bs <- createChartsJsonBS (Just params.runsPath) (Just params.chartRequestsPath)

  let outFile = params.outJsonPath
      (dir, _) = OsPath.splitFileName outFile

  Dir.createDirectoryIfMissing True dir

  writeBinaryFileIO params.outJsonPath (toStrictByteString bs)

-- | Given file paths to runs and chart requests, returns a lazy
-- json-encoded bytestring of a chart array.
createChartsJsonBS ::
  -- | Path to runs.toml. Defaults to 'defRunsPath'.
  Maybe OsPath ->
  -- | Path to chart-requests.toml. Defaults to 'defChartRequestsPath'.
  Maybe OsPath ->
  IO LazyByteString
createChartsJsonBS mRunsTomlPath mChartRequestsPath =
  AsnPretty.encodePretty' cfg <$> createChartSeq runsTomlPath chartRequestsPath
  where
    chartRequestsPath = fromMaybe defChartRequestsPath mChartRequestsPath
    runsTomlPath = fromMaybe defRunsPath mRunsTomlPath

    cfg =
      AsnPretty.defConfig
        { confIndent = Spaces 2,
          confTrailingNewline = True
        }

-- | Given file paths to runs and chart requests, generates a sequence of
-- charts.
createChartSeq ::
  -- | Path to runs.toml
  OsPath ->
  -- | Path to chart-requests.toml
  OsPath ->
  IO (Seq Chart)
createChartSeq runsPath chartRequestsPath = do
  runs <- readDecodeToml @(SomeRuns Double) runsPath
  chartRequests <- readDecodeToml @ChartRequests chartRequestsPath

  let titlesOrCharts = Chart.mkCharts runs chartRequests

  for titlesOrCharts $ \case
    Right c -> pure c
    Left t ->
      throwText
        $ mconcat
          [ "Chart with title '",
            t,
            "' is empty due to all runs being filtered out."
          ]
  where
    readDecodeToml :: forall a. (DecodeTOML a) => OsPath -> IO a
    readDecodeToml = failMapLeft displayException . decode <=< readFileUtf8

-- | Advances the ChartParams phase, filling in missing values with defaults.
advancePhase :: ChartParamsArgs -> ChartParamsFinal
advancePhase paramsArgs =
  MkChartParams
    { chartRequestsPath = fromMaybe defChartRequestsPath paramsArgs.chartRequestsPath,
      outJsonPath = fromMaybe defOutJsonPath paramsArgs.outJsonPath,
      runsPath = fromMaybe defRunsPath paramsArgs.runsPath
    }

defChartRequestsPath :: OsPath
defChartRequestsPath = [ospPathSep|backend/data/input/chart-requests.toml|]

defOutJsonPath :: OsPath
defOutJsonPath = [ospPathSep|web/data/input/charts.json|]

defRunsPath :: OsPath
defRunsPath = [ospPathSep|backend/data/input/runs.toml|]
