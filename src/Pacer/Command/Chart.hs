{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Chart functionality.
module Pacer.Command.Chart
  ( -- * Top level command
    handle,

    -- * Low level functions
    Create.ChartPaths,

    -- ** Misc
    Create.createChartSeq,
    Create.updateLabels,
  )
where

import Effectful.Logger.Dynamic qualified as Logger
import Pacer.Command.Chart.Create qualified as Create
import Pacer.Command.Chart.Data.Chart (Charts)
import Pacer.Command.Chart.Params (ChartParamsFinal)
import Pacer.Command.Chart.Server (Server)
import Pacer.Command.Chart.Server qualified as Server
import Pacer.Configuration.Config (ChartConfig)
import Pacer.Configuration.Env.Types (LogEnv)
import Pacer.Prelude
import Pacer.Utils.Json qualified as Json
import Pacer.Utils.Show qualified as Utils.Show
import Pacer.Web qualified as Web
import Pacer.Web.Paths qualified as WPaths
import System.OsPath qualified as OsPath

-- | Handles chart command.
handle ::
  forall env k es.
  ( HasCallStack,
    FileReader :> es,
    FileWriter :> es,
    LoggerNS env k es,
    PathReader :> es,
    PathWriter :> es,
    Reader LogEnv :> es,
    Server :> es
  ) =>
  Maybe ChartConfig ->
  ChartParamsFinal ->
  Eff es Unit
handle @env mChartConfig params = do
  if params ^. #json
    then do
      charts <- Create.createCharts @env mChartConfig params

      $(Logger.logInfo)
        $ "Using build-dir: "
        <> Utils.Show.showtPath (params ^. #buildDir)

      -- params.json is active, so stop after json generation
      let jsonPath = params ^. #buildDir <</>> jsonName
      createChartsJsonFile @env charts jsonPath
    else do
      webDir <- WPaths.getWebPath
      Web.ensureWebDirExists @env webDir (params ^. #cleanInstall)

      Server.launchServer
        @env
        mChartConfig
        params
        (params ^. #port)
        (webDir <</>> [reldir|dist|])

-- | Given 'ChartParamsFinal', generates a json-encoded array of charts, and
-- writes the file to the given location.
createChartsJsonFile ::
  forall env k es.
  ( HasCallStack,
    FileWriter :> es,
    LoggerNS env k es,
    PathWriter :> es
  ) =>
  Charts ->
  Path Abs File ->
  Eff es Unit
createChartsJsonFile @env charts outJson =
  addNamespace @env "createChartsJsonFile" $ do
    let bs = Json.encodePretty charts
        (dir, _) = OsPath.splitFileName outJsonOsPath

    createDirectoryIfMissing True dir

    writeBinaryFile outJsonOsPath (toStrictBS bs)

    let msg = "Wrote json file: " <> Utils.Show.showtOsPath outJsonOsPath
    $(Logger.logInfo) msg
  where
    outJsonOsPath = toOsPath outJson

jsonName :: Path Rel File
jsonName = [relfile|charts.json|]
