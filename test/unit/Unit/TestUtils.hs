module Unit.TestUtils
  ( -- * Numeric
    genℕ,
    genℕ1,
    genℕMax,
    genDoubleNN,
    genDouble,
    genDoublePos,
    genDoubleMax,
    genTextDouble,
    genTextDoublePos,
    genTextDoublePrecision,
    genTextℕ,
    genTextℕ1,
    genAffineSpace,

    -- * ByteStrings
    genWithLineComments,
    genWithBlockComments,
    genWithAllComments,
    genLineComment,
    genBlockComment,
    genNonComment,
    genNonCommentTxt,
  )
where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
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
  t <- genTextDoublePrecision True

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

genDouble :: Gen Double
genDouble = do
  t <- genTextDoublePos

  let t' = unpackText t

  case TR.readMaybe t' of
    Just x -> pure x
    Nothing -> error $ "Could not read double: " <> t'

genDoubleMax :: Int -> Gen Double
genDoubleMax maxInt = do
  t <- genTextDoubleMax maxInt

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

genTextDoublePrecision :: Bool -> Gen Text
genTextDoublePrecision shouldGenFrac = do
  -- If shouldGenFrac is true, then we might generate a text w/ some
  -- fractional part i.e. non-zero decimal part. If false we generate a
  -- whole number
  if shouldGenFrac
    then
      G.choice
        [ showt <$> genInt,
          do
            n <- genInt
            d <- genFrac
            pure
              $ mconcat
                [ showt n,
                  ".",
                  showt d
                ]
        ]
    else showt <$> genInt
  where
    genInt = G.integral @_ @Int (R.exponentialFrom 0 0 1_000_000)

    -- gen 2 decimal places max
    genFrac = G.integral @_ @Int (R.exponentialFrom 0 0 99)

genTextDoublePos :: Gen Text
genTextDoublePos = do
  G.choice
    [ showt <$> genInt,
      do
        n <- genInt
        d <- genFrac
        pure
          $ mconcat
            [ showt n,
              ".",
              showt d
            ]
    ]
  where
    genInt = G.integral @_ @Int (R.exponentialFrom 1 1 1_000_000)

    -- gen 2 decimal places max
    genFrac = G.integral @_ @Int (R.exponentialFrom 0 0 99)

-- | Generate double text like "25.349".
genTextDoubleMax :: Int -> Gen Text
genTextDoubleMax maxInt = do
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
    genInt = G.integral @_ @Int (R.exponentialFrom 0 0 maxInt)

genAffineSpace :: (IsString a) => Gen a
genAffineSpace = G.element [" ", ""]

genWithLineComments :: Gen ByteString
genWithLineComments = do
  xs <- G.list r genNonComment
  cs <- G.list r genLineComment
  mconcat <$> G.shuffle (xs ++ cs)
  where
    r = R.linearFrom 0 0 100

genWithBlockComments :: Gen ByteString
genWithBlockComments = do
  -- Strip out forward slashes. Why? Consider the following scenario:
  -- xs := ["foo/", "bar"]
  -- cs := ["/* some comment */"]
  -- shuffled := ["foo/", "/* some comment */", "bar"]
  -- concat := "foo//* some comment */bar"
  --
  -- In other words, the concat turned the block comment into a line comment!
  -- That's bad because we didn't generate the corresponding line end, i.e.
  -- it is invalid.
  xs <- G.list r $ (encodeUtf8 . T.replace "/" "") <$> genNonCommentTxt
  cs <- G.list r genBlockComment
  mconcat <$> G.shuffle (xs ++ cs)
  where
    r = R.linearFrom 0 0 100

genWithAllComments :: Gen ByteString
genWithAllComments = do
  xs <- G.list r genNonComment
  ls <- G.list r genLineComment
  bs <- G.list r genBlockComment
  mconcat <$> G.shuffle (xs ++ ls ++ bs)
  where
    r = R.linearFrom 0 0 100

genNonComment :: Gen ByteString
genNonComment = fmap encodeUtf8 genNonCommentTxt

genNonCommentTxt :: Gen Text
genNonCommentTxt =
  removeSlashStar
    . removeDoubleSlash
    -- Gen.text rather than Gen.utf8 (ByteString) as it is easier to remove
    -- unwanted substrings from Text.
    <$> G.text r G.unicode
  where
    r = R.linearFrom 0 0 100

genLineComment :: Gen ByteString
genLineComment = do
  txt <- genNonCommentTxt
  let txt' = removeNewlines txt
  pure $ "//" <> encodeUtf8 txt' <> "\n"

genBlockComment :: Gen ByteString
genBlockComment = do
  txt <- genNonCommentTxt
  let txt' = removeStarSlash txt
  pure $ "/*" <> encodeUtf8 txt' <> "*/"

removeNewlines :: Text -> Text
removeNewlines = T.replace "\n" ""

removeDoubleSlash :: Text -> Text
removeDoubleSlash = removePat2 '/' '/'

removeSlashStar :: Text -> Text
removeSlashStar = removePat2 '/' '*'

removeStarSlash :: Text -> Text
removeStarSlash = removePat2 '*' '/'

-- | Removes all occurrences of a two-sequence pattern. Whenever we encounter
-- the two-sequence pattern, we take the first char, and drop the second.
removePat2 :: Char -> Char -> Text -> Text
removePat2 startChar endChar =
  toStrictText
    . builderToLazyText
    . snd
    . TL.foldl' go (False, "")
    . fromStrictText
  where
    -- For instance, suppose we want to remove all "start block comments" i.e.
    -- the sequence "/*". Then startChar is / and endChar is *.
    go :: Tuple2 Bool TextBuilder -> Char -> Tuple2 Bool TextBuilder
    go (inPat, acc) currChar
      -- 1. If we encounter the start char (/) then we are potentially in a
      --    pattern. This is true regardless of the preceeding characters
      --    (e.g. duplicate /'s).
      | currChar == startChar = (True, acc <> TLB.singleton currChar)
      -- 2. If we encounter the end char (*) _and_ we are in a pattern, then
      --    this is exactly what we want to strip. Skip the end char, and
      --    continue, noting that we are still in the pattern.
      | currChar == endChar && inPat = (True, acc)
      -- 3. Otherwise, we are not in a pattern. Proceed normally.
      | otherwise = (False, acc <> TLB.singleton currChar)
