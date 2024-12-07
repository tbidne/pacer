-- | Main module.
--
-- @since 0.1
module Main (main) where

import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Running.Driver (runApp)
import Running.Prelude hiding (IO)
import System.IO (IO)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)
  runApp
