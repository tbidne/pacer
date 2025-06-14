{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effectful.Concurrent qualified as CC
import Effectful.Concurrent.Async qualified as Async
import Effectful.Environment qualified as Env
import Network.HTTP.Client qualified as HttpClient
import Pacer.Command.Chart.Server qualified as Server
import Pacer.Driver qualified as Driver
import Pacer.Prelude
import System.Environment.Guard
  ( ExpectEnv (ExpectEnvSet),
    guardOrElse',
  )
import System.IO qualified as IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))

main :: IO ()
main =
  guardOrElse' "RUN_E2E" ExpectEnvSet runTests dontRun
  where
    runTests =
      defaultMain
        $ testGroup
          "End-to-end"
          [ testChartHealth,
            testChart
          ]

    dontRun =
      IO.putStrLn
        "*** End-to-end tests disabled. Enabled with RUN_E2E=1 ***"

testChart :: TestTree
testChart = testCase desc $ do
  run $ Async.race_ testServer runServer
  where
    desc = "Tests chart server"
    run = runEff . runConcurrent

testChartHealth :: TestTree
testChartHealth = testCase desc $ do
  result <- run $ Async.race testServerHealthCheck runServer

  case result of
    Left msg -> "\"Pacer is up!\"" @=? msg
    Right _ -> assertFailure "Server finished, impossible!"
  where
    desc = "Tests chart server healthcheck"
    run = runEff . runConcurrent

testServer :: (Concurrent :> es) => Eff es ()
testServer = CC.threadDelay 5_000_000

testServerHealthCheck :: (Concurrent :> es, IOE :> es) => Eff es ByteString
testServerHealthCheck = do
  -- The server comes up surprisingly fast i.e. I have had success with
  -- no delay. Still, probably it's better to include _some_ delay.
  CC.threadDelay 1_000_000

  (manager, request) <- liftIO $ do
    manager <- HttpClient.newManager HttpClient.defaultManagerSettings
    request <- mkRequest
    pure (manager, request)

  eResponse <- trySync $ getResponse manager request
  case eResponse of
    -- If it fails the first time, try again with a delay.
    Left _ -> do
      CC.threadDelay 5_000_000
      toBS <$> getResponse manager request
    Right r -> pure $ toBS r
  where
    url = "http://localhost:3000/api/health"
    mkRequest = HttpClient.parseRequest url

    getResponse m r = liftIO $ HttpClient.httpLbs r m

    toBS = toStrictBS . HttpClient.responseBody

-- For simplicity, eliminate all effects except for IO and concurrency.
runServer ::
  ( Concurrent :> es,
    HasCallStack,
    IOE :> es
  ) =>
  Eff es ()
runServer = run $ Env.withArgs args $ Driver.runApp
  where
    args =
      [ "--log-level",
        "none",
        "chart",
        "-d",
        "examples"
      ]

    run =
      Env.runEnvironment
        . runFileReader
        . runFileWriter
        . runOptparse
        . runPathReader
        . runPathWriter
        . Server.runServerEff
        . runTerminal
        . runTime
