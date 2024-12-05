-- | Main module.
--
-- @since 0.1
module Main (main) where

import Running.Prelude hiding (IO)
import System.IO (IO)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = putStrLn "hi"
