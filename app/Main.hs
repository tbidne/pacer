-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils
  ( setIgnoreKnownCallStackHandler,
  )
import Pacer.Driver (runApp)
import Pacer.Exception qualified as PEx
import Pacer.Prelude hiding (IO, putStrLn)
import System.IO (IO)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setIgnoreKnownCallStackHandler PEx.knownExceptions

  runner runApp
  where
    runner =
      runEff
        . runConcurrent
        . runFileReader
        . runFileWriter
        . runOptparse
        . runPathReader
        . runPathWriter
        . runTerminal
        . runTime
        . runTypedProcess
