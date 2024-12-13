{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Functional.Derive qualified
import Functional.Prelude
import Functional.Scale qualified
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Functional"
      [ Functional.Derive.tests,
        Functional.Scale.tests
      ]
