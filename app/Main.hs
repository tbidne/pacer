-- | Main module.
--
-- @since 0.1
module Main (main) where

import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Pacer.Driver (runApp)
import Pacer.Prelude hiding (IO)
import System.IO (IO)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)
  runApp
