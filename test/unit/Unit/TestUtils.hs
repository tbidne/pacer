module Unit.TestUtils
  ( -- * Laws
    testEqLaws,
    testOrdLaws,
    testIEqLaws,
    testIOrdLaws,

    -- * Generators

    -- * Text
    genText,

    -- ** Numeric
    genℕ,
    genℕ1,
    genℕMax,
    genDoubleNN,
    genDouble,
    genDoublePos,
    genDoubleMinPos,
    genDoubleMax,
    genDoubleMin,
    genTextDouble,
    genTextDoublePos,
    genTextDoublePrecision,
    genTextℕ,
    genTextℕ1,
    genAffineSpace,

    -- ** ByteStrings
    genWithLineComments,
    genWithBlockComments,
    genWithAllComments,
    genLineComment,
    genBlockComment,
    genNonComment,
    genNonCommentTxt,

    -- ** Time
    genYear,
    genMonth,
    genTimestamp,
    genZonedTime,
    genTimeZone,
    genLocalTime,
    genDay,
    genTimeOfDay,
    genMomentTxt,
    genTimestampTxt,
    genLocalTimeTxt,
    genTzOffsetTxt,
    genTimeTxt,
    genDateTxt,
    genYearTxt,
    genMonthTxt,
    genDayTxt,
  )
where

import Data.Enum (Enum (toEnum))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Time
  ( Day (ModifiedJulianDay),
    LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    TimeZone,
    ZonedTime (ZonedTime),
  )
import Data.Time.Zones qualified as TZ
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.All qualified as TZ.All
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Pacer.Class.IOrd
  ( IEq ((~~)),
    IOrd ((<~)),
    imax,
    imin,
    (/~),
    (<.),
    (>.),
    (>~),
  )
import Pacer.Command.Chart.Data.Time.Month (Month)
import Pacer.Command.Chart.Data.Time.Timestamp.Internal
  ( Timestamp (TimestampDate, TimestampTime, TimestampZoned),
  )
import Pacer.Command.Chart.Data.Time.Year (Year)
import Text.Read qualified as TR
import Unit.Prelude

testEqLaws :: (Eq a, Show a) => a -> a -> a -> PropertyT IO ()
testEqLaws x y z = do
  -- reflexivity
  x === x

  -- symmetry
  (x == y) === (y == x)

  -- transitivity
  when (x == y && y == z) (x === z)

  -- negation
  (x /= y) === not (x == y)

testOrdLaws :: (Ord a) => a -> a -> a -> PropertyT IO ()
testOrdLaws x y z = do
  -- comparability
  let xLteY = x <= y
      yLteX = y <= x

  assert (xLteY || yLteX)

  -- transitivity
  let yLteZ = y <= z
  when (xLteY && yLteZ) $ assert (x <= z)

  -- reflexivity
  assert (x <= x)

  -- antisymmetry
  when (xLteY && yLteX) $ assert (x == y)

  -- other operators
  (x <= y) === not (x > y)
  (x < y) === (x <= y && not (x == y))
  (x >= y) === (x > y || x == y)
  (x > y) === (x >= y && not (x == y))

  -- prelude:
  -- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Ord.html#t:Ord
  (x >= y) === (y <= x)
  (x < y) === (x <= y && x /= y)
  (x > y) === (y < x)
  assert $ min x y == if x <= y then x else y
  assert $ max x y == if x >= y then x else y

testIEqLaws :: (IEq a, Show a) => a -> a -> PropertyT IO ()
testIEqLaws x y = do
  -- reflexivity
  x ~~~ x

  -- symmetry
  (x ~~ y) === (y ~~ x)

  -- negation
  (x /~ y) === not (x ~~ y)

testIOrdLaws :: (IOrd a) => a -> a -> PropertyT IO ()
testIOrdLaws x y = do
  -- comparability
  let xLteY = x <~ y
      yLteX = y <~ x

  assert (xLteY || yLteX)

  -- reflexivity
  assert (x <~ x)

  -- antisymmetry
  when (xLteY && yLteX) $ assert (x ~~ y)

  -- other operators
  (x <~ y) === not (x >. y)
  (x <~ y) === (x <. y || x ~~ y)
  (x >~ y) === (x >. y || x ~~ y)
  (x >. y) === (x >~ y && not (x ~~ y))

  -- prelude:
  -- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Ord.html#t:Ord
  (x >~ y) === (y <~ x)
  (x <. y) === (x <~ y && x /~ y)
  (x >. y) === (y <. x)
  assert $ imin x y ~~ if x <~ y then x else y
  assert $ imax x y ~~ if x >~ y then x else y

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

genDoubleMinPos :: Int -> Gen PDouble
genDoubleMinPos minInt = do
  t <- genTextDoubleMin minInt

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

genDoubleMin :: Int -> Gen Double
genDoubleMin minInt = do
  t <- genTextDoubleMin minInt

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

-- | Generate double text like "25.349".
genTextDoubleMin :: Int -> Gen Text
genTextDoubleMin minInt = do
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
    genInt = G.integral @_ @Int (R.exponentialFrom minInt minInt 99)

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
  xs <- G.list r $ encodeUtf8 . T.replace "/" "" <$> genNonCommentTxt
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
  -- Do not generate any slashes. Consider o/w e.g.
  --
  --   - genNonCommentTxt: "/"
  --   - genBlockComment: "/**/"
  --
  -- Then we have
  --
  --   "//**/"
  --
  -- Which is invalid text. This string was generated on CI and causes the
  -- tests to fail; presumably something like the above happened.
  removeSlash
    -- G.text rather than G.utf8 (ByteString) as it is easier to remove
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

removeSlash :: Text -> Text
removeSlash = T.replace "/" ""

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

genYear :: Gen Year
genYear = G.enumBounded

genMonth :: Gen Month
genMonth = G.enumBounded

genMomentTxt :: Gen Text
genMomentTxt = do
  G.choice
    [ genYearTxt,
      genYearMonth,
      genTimestampTxt
    ]
  where
    genYearMonth =
      (\y m -> y <> "-" <> m) <$> genYearTxt <*> genMonthTxt

genTimestamp :: Gen Timestamp
genTimestamp =
  G.choice
    [ TimestampDate <$> genDay,
      TimestampTime <$> genLocalTime,
      TimestampZoned <$> genZonedTime
    ]

genZonedTime :: Gen ZonedTime
genZonedTime = do
  lt <- genLocalTime
  tz <- genTimeZone
  pure $ ZonedTime lt tz

genTimeZone :: Gen TimeZone
genTimeZone =
  (\l -> TZ.timeZoneForPOSIX (TZ.All.tzByLabel l) 0)
    <$> tzLabels
  where
    tzLabels :: Gen TZLabel
    tzLabels = G.enumBounded

genLocalTime :: Gen LocalTime
genLocalTime = do
  d <- genDay
  tod <- genTimeOfDay
  pure $ LocalTime d tod

genDay :: Gen Day
genDay =
  G.element
    [ ModifiedJulianDay 33282, -- 1950-01-01
      ModifiedJulianDay 88068 -- 2099-12-31
    ]

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = do
  h <- G.integral (R.linear 0 23)
  m <- G.integral (R.linear 0 59)
  s <- G.integral (R.linear 0 60) -- 60 == leap second
  -- REVIEW: Is toEnum legit here?
  pure $ TimeOfDay h m (toEnum s)

genTimestampTxt :: Gen Text
genTimestampTxt = do
  G.choice
    [ genDateTxt,
      genLocalTimeTxt,
      genZonedTxt
    ]

genZonedTxt :: Gen Text
genZonedTxt = do
  lt <- genLocalTimeTxt
  tz <- genTzOffsetTxt
  pure $ lt <> tz

genLocalTimeTxt :: Gen Text
genLocalTimeTxt = do
  dateTxt <- genDateTxt
  timeTxt <- genTimeTxt
  sep <- G.element ['T', ' ']
  pure (dateTxt <> T.singleton sep <> timeTxt)

genTzOffsetTxt :: Gen Text
genTzOffsetTxt = do
  h <- G.integral @_ @Word8 (R.linear 0 23)
  m <- G.integral @_ @Word8 (R.linear 0 59)
  s <- G.element ["", " "]
  d <- G.element ['+', '-']
  pure
    $ mconcat
      [ s,
        T.singleton d,
        showtPad2 h,
        showtPad2 m
      ]

genTimeTxt :: Gen Text
genTimeTxt = do
  h <- G.integral @_ @Word8 (R.linear 0 23)
  m <- G.integral @_ @Word8 (R.linear 0 59)
  s <- G.integral @_ @Word8 (R.linear 0 59)
  pure $ T.intercalate ":" (showtPad2 <$> [h, m, s])

genDateTxt :: Gen Text
genDateTxt = do
  y <- genYearTxt
  m <- genMonthTxt
  d <- genDayTxt m
  pure $ T.intercalate "-" [y, m, d]

genYearTxt :: Gen Text
genYearTxt = do
  y <- G.integral @_ @Word16 (R.linear 1950 2099)
  pure $ showt y

genMonthTxt :: Gen Text
genMonthTxt = do
  y <- G.integral @_ @Word8 (R.linear 1 12)
  pure $ showtPad2 y

genDayTxt :: Text -> Gen Text
genDayTxt monthTxt = do
  let maxN = maxDay monthTxt
  y <- G.integral @_ @Word8 (R.linear 1 maxN)
  pure $ showtPad2 y
  where
    maxDay "01" = 31
    maxDay "02" = 28
    maxDay "03" = 31
    maxDay "04" = 30
    maxDay "05" = 31
    maxDay "06" = 30
    maxDay "07" = 31
    maxDay "08" = 31
    maxDay "09" = 30
    maxDay "10" = 31
    maxDay "11" = 30
    maxDay "12" = 31
    maxDay d = error $ "Unexpected day: " ++ unpackText d

showtPad2 :: (Show a) => a -> Text
showtPad2 = pad2 . showt

pad2 :: Text -> Text
pad2 t =
  if T.length t == 1
    then T.singleton '0' <> t
    else t

genText :: Gen Text
genText = G.text (R.linearFrom 0 0 20) G.unicode
