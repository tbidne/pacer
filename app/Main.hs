-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils
  ( setUncaughtExceptionDisplayInnerMatch,
  )
import Pacer.Driver (runApp)
import Pacer.Exception qualified as PEx
import System.IO (IO, putStrLn)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionDisplayInnerMatch
    PEx.knownExceptions
    putStrLn

  runApp
