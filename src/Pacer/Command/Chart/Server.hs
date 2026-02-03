{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Chart functionality.
module Pacer.Command.Chart.Server
  ( -- * Entry point
    launchServer,

    -- * Effect
    Server (..),
    run,
    serve,

    -- ** Handlers
    runServer,
    runServerMock,
  )
where

import Effectful
  ( Limit (Unlimited),
    Persistence (Ephemeral),
    UnliftStrategy (ConcUnlift),
  )
import Effectful.Dispatch.Dynamic (localSeqUnlift, localSeqUnliftIO, localUnliftIO)
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.OsPath (decodeThrowM)
import Network.HTTP.Types.Status qualified as HTTP.Status
import Network.Wai (Request, Response, ResponseReceived)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Pacer.Command.Chart.Create qualified as Create
import Pacer.Command.Chart.Data.Chart (Charts)
import Pacer.Command.Chart.Params (ChartParamsFinal)
import Pacer.Configuration.Config (ChartConfig)
import Pacer.Configuration.Env.Types (LogEnv)
import Pacer.Exception (ShowException (MkShowException))
import Pacer.Prelude
import Servant
  ( Get,
    Handler,
    HasServer,
    JSON,
    Raw,
    ServerT,
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
mkApp ::
  forall env k es.
  ( FileReader :> es,
    HasCallStack,
    LoggerNS env k es,
    Reader LogEnv :> es,
    Server :> es
  ) =>
  Maybe ChartConfig ->
  ChartParamsFinal ->
  FilePath ->
  Request ->
  (Response -> Eff es ResponseReceived) ->
  Eff es ResponseReceived
mkApp @env @_ @es mChartConfig params staticDir = serve webApi serveApi
  where
    serveApi :: ServerT WebApi (Eff es)
    serveApi =
      (pure "Pacer is up!" :<|> Create.createCharts @env mChartConfig params)
        :<|> Servant.serveDirectoryFileServer staticDir

    webApi :: Proxy WebApi
    webApi = Proxy

launchServer ::
  forall env k es.
  ( FileReader :> es,
    HasCallStack,
    LoggerNS env k es,
    Reader LogEnv :> es,
    Server :> es
  ) =>
  Maybe ChartConfig ->
  ChartParamsFinal ->
  Word16 ->
  Path Abs Dir ->
  Eff es Unit
launchServer
  @env
  mChartConfig
  params
  port
  webDistPath = addNamespace @env "launchServer" $ do
    $(Logger.logInfo) msg
    webDistStr <- decodeThrowM (toOsPath webDistPath)

    run portInt (mkApp @env mChartConfig params webDistStr)
    where
      msg =
        mconcat
          [ "Pacer is running in your browser on http://localhost:",
            showt portInt,
            " :-)"
          ]
      portInt = fromIntegral @Word16 @Int port

-- | Effect for Servant. The type:
--
-- @
--   Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived)
-- @
--
-- is known as 'Application' in Servant, but we expand the alias here
-- for clarify, and lift to Eff.
data Server :: Effect where
  Run ::
    Port ->
    ( Request ->
      (Response -> m ResponseReceived) ->
      m ResponseReceived
    ) ->
    Server m Unit
  Serve ::
    (HasServer api []) =>
    Proxy api ->
    ServerT api m ->
    Request ->
    (Response -> m ResponseReceived) ->
    Server m ResponseReceived

type instance DispatchOf Server = Dynamic

-- | Runs the 'Server' effect in IO.
runServer :: (IOE :> es) => Eff (Server : es) a -> Eff es a
runServer = interpret $ \env -> \case
  Run p app -> localUnliftIO env strategy $ \unlift ->
    Warp.run p $ \req onResp -> unlift $ do
      -- Rethrow exceptions as ShowExceptions since servant uses show instead
      -- of displayException. We have a custom type rather than use
      -- StringException so the constructor does not show up.
      trySync (app req (unsafeEff_ . onResp)) >>= \case
        Left ex -> throwM $ MkShowException (displayException ex)
        Right x -> pure x
  Serve p api req onResp -> localSeqUnliftIO env $ \unlift -> do
    let handlerApi = Servant.hoistServer p (liftIO @Handler . unlift) api
    Servant.serve p handlerApi req (unlift . onResp)
  where
    -- We need ConcUnlift because Warp.run spawns thread(s). Otherwise we
    -- receive a runtime error:
    --
    --   If you want to use the unlifting function to run Eff computations in
    --   multiple threads, have a look at UnliftStrategy (ConcUnlift).
    --
    -- Servant.serve seems fine to use the synchronous strategy.
    --
    -- It is less clear what ConcUnlift args to use.
    --
    -- With the Ephemeral strategy, each page refesh counts as an unlift
    -- usage, so once our refreshes exceedes the limit, the server will die.
    -- Hence we would need Unlimited.
    --
    -- With Persistent, Limited 1 appears to work.
    --
    -- The docs explain that Persistent "Persists the environment between calls
    -- to the unlifting function within a particular thread.", whereas
    -- Ephemeral does not.
    --
    -- Further, the Ephemeral Limit limits the number of times the unlift
    -- function can be called in "threads distinct from its creator" to N.
    -- It is not entirely clear whether this N is shared between all threads,
    -- or each thread can call unlift N times.
    --
    -- The Persistent Limit, OTOH, limits the number of threads that can call
    -- unlift, but each thread can call it N times.
    --
    -- I _think_ this means we only need the Persistent strategy when our
    -- unlifted actions actually modify the environment, and we want that
    -- persisted (e.g. State effect). But since we do not, Ephemeral with
    -- Unlimited should be okay.
    --
    -- https://hackage-content.haskell.org/package/effectful-core-2.6.1.0/docs/Effectful.html#t:UnliftStrategy
    strategy = ConcUnlift Ephemeral Unlimited

-- | Mock 'Server' effect.
runServerMock :: Eff (Server : es) a -> Eff es a
runServerMock = interpret $ \env -> \case
  Run {} -> pure ()
  Serve _ _ _ onResp -> localSeqUnlift env $ \unlift -> do
    let req = Wai.responseBuilder HTTP.Status.status200 [] mempty
    unlift $ onResp req

run ::
  ( HasCallStack,
    Server :> es
  ) =>
  Port ->
  (Request -> (Response -> Eff es ResponseReceived) -> Eff es ResponseReceived) ->
  Eff es Unit
run p = send . Run p

serve ::
  ( HasCallStack,
    HasServer api [],
    Server :> es
  ) =>
  Proxy api ->
  ServerT api (Eff es) ->
  Request ->
  (Response -> Eff es ResponseReceived) ->
  Eff es ResponseReceived
serve p api req = send . Serve p api req
