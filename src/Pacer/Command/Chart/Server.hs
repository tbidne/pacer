{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoScopedTypeVariables #-}

-- | Chart functionality.
module Pacer.Command.Chart.Server
  ( launchServer,

    -- * Effect
    ServerEff (..),
    run,

    -- ** Handlers
    runServerEff,
    runServerEffMock,
  )
where

import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.OsPath (decodeThrowM)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Pacer.Command.Chart.Data.Chart (Charts)
import Pacer.Prelude
import Servant
  ( Application,
    Get,
    JSON,
    Raw,
    (:<|>) ((:<|>)),
  )
import Servant qualified
import System.IO (FilePath)

-- To avoid effectful clash.
type (::>) :: forall k. k -> Type -> Type
type k ::> a = k Servant.:> a

-- | Endpoints used by the frontend.
type Api =
  "health" ::> Get [JSON] Text
    :<|> "charts" ::> Get [JSON] Charts

-- | Total API, including the raw static assets.
type WebApi = "api" ::> Api :<|> Raw

-- | staticDir should be an absolute path to the xdg_cache/web/dist directory.
-- Morally it is @Path Abs Dir@, though we resort to FilePath since that's
-- what serveDirectoryFileServer uses, and we don't want to deserialize here.
mkApp :: FilePath -> Charts -> Application
mkApp staticDir charts = Servant.serve webApi server
  where
    server = serveApi :<|> Servant.serveDirectoryFileServer staticDir

    serveApi = pure "Pacer is up!" :<|> pure charts

    webApi :: Proxy WebApi
    webApi = Proxy

launchServer ::
  forall env k es.
  ( HasCallStack,
    LoggerNS env k es,
    ServerEff :> es
  ) =>
  Word16 ->
  Path Abs Dir ->
  Charts ->
  Eff es Unit
launchServer
  @env
  port
  webDistPath
  charts = addNamespace @env "launchServer" $ do
    $(Logger.logInfo) msg
    webDistStr <- decodeThrowM (toOsPath webDistPath)

    run portInt (mkApp webDistStr charts)
    where
      msg =
        mconcat
          [ "Pacer is running in your browser on http://localhost:",
            showt portInt,
            " :-)"
          ]
      portInt = fromIntegral @Word16 @Int port

data ServerEff :: Effect where
  Run :: Port -> Application -> ServerEff m Unit

type instance DispatchOf ServerEff = Dynamic

runServerEff :: (IOE :> es) => Eff (ServerEff : es) a -> Eff es a
runServerEff = interpret_ $ \case
  Run p a -> liftIO $ Warp.run p a

runServerEffMock :: Eff (ServerEff : es) a -> Eff es a
runServerEffMock = interpret_ $ \case
  Run _ _ -> pure ()

run :: (HasCallStack, ServerEff :> es) => Port -> Application -> Eff es Unit
run p = send . Run p
