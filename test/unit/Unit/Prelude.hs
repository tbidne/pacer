module Unit.Prelude
  ( module X,

    -- * Hedgehog
    annotateUnpack,
    hdiff,

    -- * HUnit
    (@/=?),

    -- * Misc
    mkDistanceD,
    mkDurationD,
    parseOrDie,
    parseOrDieM,
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
import Hedgehog qualified as H
import Running.Class.Parser (Parser)
import Running.Class.Parser qualified as Parser
import Running.Data.Distance (Distance (MkDistance))
import Running.Data.Duration (Duration (MkDuration))
import Running.Prelude as X hiding (IO)
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

annotateUnpack :: Text -> PropertyT IO ()
annotateUnpack = annotate . unpackText

(@/=?) :: (Eq a, Show a) => a -> a -> Assertion
x @/=? y = assertBool msg (x /= y)
  where
    msg =
      mconcat
        [ show x,
          " == ",
          show y
        ]

infix 1 @/=?

parseOrDie :: (HasCallStack, Parser a) => Text -> a
parseOrDie = errorMapLeft unpackText . Parser.parse

parseOrDieM :: forall a m. (MonadFail m, Parser a) => Text -> m a
parseOrDieM t = do
  case Parser.parse t of
    Right r -> pure r
    Left err -> fail (unpackText err)

hdiff :: (Show a, Show b) => a -> (a -> b -> Bool) -> b -> PropertyT IO ()
hdiff = H.diff

mkDistanceD :: forall t. Double -> Distance t Double
mkDistanceD = MkDistance

mkDurationD :: forall t. Double -> Duration t Double
mkDurationD = MkDuration
