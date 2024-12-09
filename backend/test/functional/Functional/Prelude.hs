module Functional.Prelude
  ( module X,

    -- * HUnit
    (<@=?>),
  )
where

import Hedgehog as X
  ( Gen,
    Property,
    PropertyName,
    PropertyT,
    Range,
    annotate,
    annotateShow,
    assert,
    failure,
    forAll,
    property,
    withTests,
    (/==),
    (===),
  )
import Pacer.Prelude as X hiding (IO)
import System.IO as X (IO)
import Test.Tasty as X (TestName, TestTree, testGroup)
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)

(<@=?>) :: (Eq a, Show a) => a -> IO a -> Assertion
x <@=?> my = do
  y <- my
  x @=? y

infix 1 <@=?>
