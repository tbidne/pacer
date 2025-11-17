-- | Provides filters over generic set.
module Pacer.Command.Chart.Data.Expr.Filter.Utils
  ( displaySet,
    parseTextNonEmpty,
    parseSetElem,
    parseSet,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder)
import Pacer.Class.Parser (MParser, Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Prelude
import Pacer.Utils.Show qualified as Utils.Show
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

parseSetElem :: (Parser a) => MParser a
parseSetElem = do
  txt <- parseTextNonEmpty
  case T.find (\c -> c == '{' || c == '}') txt of
    Just c ->
      fail
        $ mconcat
          [ "Unexpected char '",
            [c],
            "'. Expected exactly one element, not set syntax."
          ]
    Nothing -> failErr $ P.parseAll txt

parseSet :: (Ord a, Parser a) => String -> MParser (Set a)
parseSet name = do
  set <- parseEmptySet <|> parseSetTxt
  MPC.space
  pure set
  where
    parseEmptySet = MPC.char '∅' $> Set.empty

    parseSetTxt = do
      MPC.char '{'
      txt <- MP.takeWhileP (Just name) (/= '}')
      set <- parseCommaSep txt
      MPC.char '}'
      pure set

    parseCommaSep txt = do
      let stripped = T.strip txt
      if T.null stripped
        -- 1. If stripped text is empty that means we only received
        --    whitespace i.e. the empty set, OK.
        then pure Set.empty
        -- 2. Stripped text is non-empty. Parse comma-sep text and
        --    strip each entry. If any of the entries are empty that
        --    means we either had a leading/trailing comma or
        --    "consecutive" ones e.g. {a,  ,b}, BAD.
        else do
          let xs =
                fmap T.strip
                  . T.split (== ',')
                  $ txt

          when (any T.null xs)
            $ fail
            $ mconcat
              [ "Unexpected empty text. Possibly there are leading/",
                "trailing/consecutive commas."
              ]

          Set.fromList <$> failErr (traverse P.parseAll xs)

parseTextNonEmpty :: MParser Text
parseTextNonEmpty = do
  txt <- MP.takeWhile1P (Just "text") (const True)
  let stripped = T.strip txt
  -- For reasons I do not currently understand, the below guard is
  -- apparently unnecessary. That is, we do not want to parse empty
  -- (whitespace only) text here, hence the check. But it actually does
  -- not matter, as all users (label = ..., labels ∋) end up failing
  -- with white-space only text, which is what we want.
  --
  -- As to _why_ we have this behavior, I am not sure, since whitespace
  -- _does_ satisfy takeWhile1P. But for some reason it is failing here,
  -- so good? It would be nice to understand this.
  when (T.null stripped) $ fail "Unexpected empty text"
  pure stripped

displaySet :: (Display a, Eq a) => Set a -> Builder
displaySet s
  | s == Set.empty = "∅"
  | otherwise = showFn s
  where
    showFn xs =
      Utils.Show.showListLike
        . Utils.Show.ShowListInline (Utils.Show.ShowListMap displayBuilder xs)
        $ mempty {Utils.Show.brackets = Utils.Show.ShowListBracketsCurly}
