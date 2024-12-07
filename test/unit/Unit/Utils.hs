module Unit.Utils
  ( -- * Numeric
    genℕ,
    genℕ1,
    genℕMax,
    genDoubleNN,
    genDoublePos,
    genDoubleMax,
    genTextDouble,
    genTextDoublePos,
    genTextℕ,
    genTextℕ1,
    genAffineSpace,
  )
where

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Text.Read qualified as TR
import Unit.Prelude

genℕ :: Gen Natural
genℕ = G.integral (R.exponentialFrom 0 0 1_000_000)

genℕMax :: Natural -> Gen Natural
genℕMax n = G.integral (R.exponentialFrom 0 0 n)

genℕ1 :: Gen Natural
genℕ1 = G.integral (R.exponentialFrom 1 1 1_000_000)

-- | Generate double. We depend on 'genDoubleText' rather than generating
-- the double directly, to prevent generating doubles w/ many decimals e.g.
-- N.12345678. These may end up requiring exponential notation e.g.
-- 1234.5678e-300, which we do _not_ support.
genDoubleNN :: Gen Double
genDoubleNN = do
  t <- genTextDouble

  let t' = unpackText t

  case TR.readMaybe t' of
    Just x -> pure x
    Nothing -> error $ "Could not read double: " <> t'

genDoublePos :: Gen PDouble
genDoublePos = do
  t <- genTextDoublePos

  let t' = unpackText t

  case TR.readMaybe t' of
    Just x -> pure $ unsafePositive x
    Nothing -> error $ "Could not read double: " <> t'

genDoubleMax :: Int -> Gen Double
genDoubleMax max = do
  t <- genTextDoubleMax max

  let t' = unpackText t

  case TR.readMaybe t' of
    Just x -> pure x
    Nothing -> error $ "Could not read double: " <> t'

genTextℕ :: Gen Text
genTextℕ = showt <$> genℕ

genTextℕ1 :: Gen Text
genTextℕ1 = showt <$> genℕ1

-- | Generate double text like "25.349".
genTextDouble :: Gen Text
genTextDouble = do
  G.choice
    [ showt <$> genInt,
      do
        n <- genInt
        d <- G.integral @_ @Int (R.exponentialFrom 0 0 1_000)
        pure
          $ mconcat
            [ showt n,
              ".",
              showt d
            ]
    ]
  where
    genInt = G.integral @_ @Int (R.exponentialFrom 0 0 1_000_000)

genTextDoublePos :: Gen Text
genTextDoublePos = do
  G.choice
    [ showt <$> genInt,
      do
        n <- genInt
        d <- G.integral @_ @Int (R.exponentialFrom 1 1 1_000)
        pure
          $ mconcat
            [ showt n,
              ".",
              showt d
            ]
    ]
  where
    genInt = G.integral @_ @Int (R.exponentialFrom 1 1 1_000_000)

-- | Generate double text like "25.349".
genTextDoubleMax :: Int -> Gen Text
genTextDoubleMax max = do
  G.choice
    [ showt <$> genInt,
      do
        n <- genInt
        d <- genInt
        pure
          $ mconcat
            [ showt n,
              ".",
              showt d
            ]
    ]
  where
    genInt = G.integral @_ @Int (R.exponentialFrom 0 0 max)

genAffineSpace :: (IsString a) => Gen a
genAffineSpace = G.element [" ", ""]
