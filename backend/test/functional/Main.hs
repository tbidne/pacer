{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Functional.Chart qualified
import Functional.Derive qualified
import Functional.Prelude
import Functional.Scale qualified
import Test.Tasty (defaultMain, localOption)
import Test.Tasty.Golden (DeleteOutputFile (OnPass))

main :: IO ()
main =
  defaultMain
    $ localOption OnPass
    $ testGroup
      "Functional"
      [ Functional.Chart.tests,
        Functional.Derive.tests,
        Functional.Scale.tests
      ]
