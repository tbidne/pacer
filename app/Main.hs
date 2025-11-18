-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils
  ( setIgnoreKnownCallStackHandler,
  )
import Pacer.Command.Chart.Server (runServer)
import Pacer.Driver qualified as Driver
import Pacer.Prelude hiding (IO, putStrLn)
import System.IO (IO)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO Unit
main = do
  setIgnoreKnownCallStackHandler Driver.knownExceptions

  runner Driver.runApp
  where
    runner =
      runEff
        . runConcurrent
        . runFileReader
        . runFileWriter
        . runOptparse
        . runPathReader
        . runPathWriter
        . runServer
        . runTerminal
        . runTime
