{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Utils.Show
  ( -- * Paths
    showOsPath,
    showtOsPath,
    showPath,
    showtPath,

    -- * List-like
    showMapListInline,
    showMapListNewlines,
    showListLike,
    ShowListStyle (..),
    ShowListTarget (..),
    ShowListInlineConfig (..),
    ShowListBracketStyle (..),

    -- ** Sequences
    mkNonPosErrMsg,
    displaySummarizedSequences,
    summarizeSequences,
  )
where

import Data.List qualified as L
import FileSystem.OsPath qualified as OsPath
import Pacer.Prelude

showOsPath :: OsPath -> String
showOsPath = OsPath.decodeLenient

showtOsPath :: OsPath -> Text
showtOsPath = packText . showOsPath

showPath :: Path b t -> String
showPath = OsPath.decodeLenient . toOsPath

showtPath :: Path b t -> Text
showtPath = showtOsPath . toOsPath

-- | Bracket style.
data ShowListBracketStyle
  = ShowListBracketsSquare
  | ShowListBracketsCurly
  | ShowListBracketsNone

instance Semigroup ShowListBracketStyle where
  ShowListBracketsNone <> _ = ShowListBracketsNone
  _ <> ShowListBracketsNone = ShowListBracketsNone
  ShowListBracketsCurly <> _ = ShowListBracketsCurly
  _ <> ShowListBracketsCurly = ShowListBracketsCurly
  _ <> _ = ShowListBracketsSquare

instance Monoid ShowListBracketStyle where
  mempty = ShowListBracketsSquare

-- | Inline config.
data ShowListInlineConfig = MkShowListInlineConfig
  { -- | Whether comma separator should have a space.
    spaces :: Bool,
    brackets :: ShowListBracketStyle
  }

makeFieldLabelsNoPrefix ''ShowListInlineConfig

instance Semigroup ShowListInlineConfig where
  l <> r =
    MkShowListInlineConfig
      { spaces = l ^. #spaces && r ^. #spaces,
        brackets = l ^. #brackets <> r ^. #brackets
      }

instance Monoid ShowListInlineConfig where
  mempty = MkShowListInlineConfig True mempty

-- | Wrapper for showing a list-like value.
type ShowListTarget :: (Type -> Type) -> Type -> Type
data ShowListTarget f r where
  -- | The structure is the processed type.
  ShowListActual :: f r -> ShowListTarget f r
  -- | The structure needs its contents mapped to a suitable type.
  ShowListMap :: (a -> r) -> f a -> ShowListTarget f r

-- | Base show list type.
type ShowListStyle :: (Type -> Type) -> Type -> Type
data ShowListStyle f r
  = -- | Inline style.
    ShowListInline (ShowListTarget f r) ShowListInlineConfig
  | -- | Newline style.
    ShowListNewlines (ShowListTarget f r)

showMapListInline :: (Foldable f, IsString c, Semigroup c) => (a -> c) -> f a -> c
showMapListInline f xs =
  showListLike
    . ShowListInline (ShowListMap f xs)
    $ mempty

showMapListNewlines :: (Foldable f, IsString c, Semigroup c) => (a -> c) -> f a -> c
showMapListNewlines f =
  showListLike
    . ShowListNewlines
    . ShowListMap f

-- | Shows a list-like value.
showListLike :: (Foldable f, IsString r, Semigroup r) => ShowListStyle f r -> r
showListLike = \case
  ShowListNewlines sl -> go ys
    where
      go :: forall x. (IsString x, Semigroup x) => List x -> x
      go [] = ""
      go (z : zs) = "\n  - " <> z <> go zs

      ys = case sl of
        ShowListActual xs -> toList xs
        ShowListMap f xs -> f <$> toList xs
  ShowListInline sl cfg -> case ys of
    [] -> l <> r
    _ -> l <> go ys
    where
      go [] = r
      go [z] = z <> r
      go (z : zs) = z <> sep <> go zs

      ys = case sl of
        ShowListActual xs -> toList xs
        ShowListMap f xs -> f <$> toList xs

      (l, r) = case cfg ^. #brackets of
        ShowListBracketsSquare -> ("[", "]")
        ShowListBracketsCurly -> ("{", "}")
        ShowListBracketsNone -> ("", "")

      sep
        | cfg ^. #spaces = ", "
        | otherwise = ","

mkNonPosErrMsg :: (Eq a, Num a, Show a) => Text -> List a -> Text
mkNonPosErrMsg suffix posErrs =
  mconcat
    [ "Skipping non-positive values found ",
      suffix,
      ": ",
      -- The order is backwards, due to foldM being a left fold,
      -- hence the reverse. The indexes appear correct, however.
      displaySummarizedSequences (L.reverse posErrs)
    ]

-- Summarizes list of natural e.g.
--
-- λ. displaySummarizedSequences [1,2,3,5,8,10,11,14,15,16]
-- "[1-3,5,8,10-11,14-16]"
displaySummarizedSequences :: (Eq a, Num a, Show a) => List a -> Text
displaySummarizedSequences = showFn . summarizeSequences
  where
    showFn xs =
      showListLike
        . ShowListInline (ShowListActual xs)
        $ mempty {spaces = False}

-- Summarizes list of natural e.g.
--
-- λ. summarizeSequences [1,2,3,5,8,10,11,14,15,16]
-- ["1-3","5","8","10-11","14-16"]
summarizeSequences :: forall a. (Eq a, Num a, Show a) => List a -> List Text
summarizeSequences = fmap showGroup . groupSequential
  where
    showGroup :: List a -> Text
    showGroup [] = ""
    showGroup (start : es) = case L.unsnoc es of
      Nothing -> showt start
      Just (_, end) -> showt start <> "-" <> showt end

    groupSequential :: List a -> List (List a)
    groupSequential [] = []
    groupSequential (x : xs) = case go 1 [x] xs of
      (grp, []) -> [L.reverse grp]
      (grp, rest) -> L.reverse grp : groupSequential rest
      where
        go _ acc [] = (acc, [])
        go !cnt acc (y : ys)
          | y - x == cnt = go (cnt + 1) (y : acc) ys
          | otherwise = (acc, y : ys)
