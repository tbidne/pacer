-- | Main module.
--
-- @since 0.1
module Main (main) where

import Running.Config.Args (runApp)
import System.IO (IO)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = runApp
