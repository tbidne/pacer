-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils
  ( setIgnoreKnownCallStackHandler,
  )
import Pacer.Command.Chart.Server (runServerEff)
import Pacer.Driver qualified as Driver
import Pacer.Prelude hiding (IO, putStrLn)
import System.IO (IO)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
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
        . runServerEff
        . runTerminal
        . runTime
