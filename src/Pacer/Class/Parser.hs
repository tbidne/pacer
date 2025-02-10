-- | Provides parsing functionality.
module Pacer.Class.Parser
  ( -- * Parsing

    -- ** Class
    Parser (..),
    MParser,

    -- ** Parse functions
    parse,
    parseAll,
    parseWith,
    parseAllWith,

    -- ** Digits
    parseDigits,
    parseDigitText,
    readDigits,

    -- ** TimeString
    parsePosTimeString,

    -- * Lexing,
    lexeme,
    allExcept,
    char,
    string,
    symbol,
    symbols,

    -- * Expressions
    prefix,
    binary,
  )
where

import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix))
import Data.Char qualified as Ch
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Format qualified as Format
import Data.Time.Relative qualified as Rel
import Pacer.Data.Result (Result (Err, Ok), ResultDefault)
import Pacer.Prelude
import Text.Megaparsec
  ( MonadParsec,
    Parsec,
    ShowErrorComponent,
    Stream (Tokens),
    TraversableStream,
    VisualStream,
  )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as Lex
import Text.Read qualified as TR

-- | Main parsing type.
type MParser a = Parsec Void Text a

-- REVIEW: It might be nice to generalize this class to accept input types
-- other than Text i.e. consider that Expr parses List ExprToken.

-- | Class for parsing.
class Parser a where
  -- | Megaparsec parser for the given type.
  parser :: MParser a

instance Parser Word16 where
  parser = parseIntegral

instance Parser Double where
  parser = parseDigits

instance (AMonoid a, Ord a, Parser a, Show a) => Parser (Positive a) where
  parser = do
    d <- parser
    case mkPositive d of
      Nothing -> fail $ "Parsed non-positive: " ++ show d
      Just x -> pure x

instance Parser LocalTime where
  parser = do
    str <- unpackText <$> MP.takeWhile1P (Just "local-time") p
    case Format.parseTimeM False Format.defaultTimeLocale fmt str of
      Ok d -> pure d
      Err err ->
        fail
          $ mconcat
            [ "Failed parsing localtime from string '",
              str,
              "': ",
              err
            ]
    where
      p c =
        Ch.isDigit c
          || c
          == '-'
          || c
          == 'T'
          || c
          == ':'

      fmt = "%Y-%m-%dT%H:%M:%S"

instance Parser ZonedTime where
  parser = do
    str <- unpackText <$> MP.takeWhile1P (Just "zoned-time") p
    case Format.parseTimeM False Format.defaultTimeLocale fmt str of
      Ok d -> pure d
      Err err ->
        fail
          $ mconcat
            [ "Failed parsing zoned time from string '",
              str,
              "': ",
              err
            ]
    where
      p c =
        Ch.isDigit c
          || c
          == '-'
          || c
          == 'T'
          || c
          == ':'
          || c
          == '+'
          || c
          == '-'

      fmt = "%Y-%m-%dT%H:%M:%S%z"

instance Parser Day where
  parser = do
    str <- unpackText <$> MP.takeWhile1P (Just "date") (\c -> Ch.isDigit c || c == '-')
    case Format.parseTimeM False Format.defaultTimeLocale fmt str of
      Ok d -> pure d
      Err err ->
        fail
          $ mconcat
            [ "Failed parsing day from string '",
              str,
              "': ",
              err
            ]
    where
      fmt = "%Y-%m-%d"

-- | Parser combinator for digits with a 'Read' instance.
parseDigits :: (Read n) => Parsec Void Text n
parseDigits = parseDigitText >>= readDigits

-- | Parser combinator for digits with a 'Read' instance.
parseIntegral :: (Read n) => Parsec Void Text n
parseIntegral = parseIntegralText >>= readDigits

-- | Parser combinator for digits.
parseDigitText :: Parsec Void Text Text
parseDigitText =
  MP.takeWhile1P (Just "digits") (\c -> Ch.isDigit c || c == '.')

-- | Parser combinator for digits.
parseIntegralText :: Parsec Void Text Text
parseIntegralText =
  MP.takeWhile1P (Just "digits") Ch.isDigit

-- | Read text like "1d2h3m4s", parse w/ relative time into positive seconds.
parsePosTimeString ::
  forall a.
  (AMonoid a, Fromℤ a, Ord a, Show a) =>
  MParser (Positive a)
parsePosTimeString = do
  t <-
    MP.takeWhile1P
      (Just "time-string")
      (\c -> Ch.isDigit c || c `T.elem` chars)
  case Rel.fromString (unpackText t) of
    Left err -> fail $ "Could not read duration: " ++ err
    Right rt -> do
      let secondsℕ = Rel.toSeconds rt
          secondsℤ = toℤ secondsℕ
          -- NOTE: This is potentially unsafe e.g. if @a@ happens to be
          -- @Positive b@. For this reason, we include the positive check here
          -- as it is clearer that a should be a primitive, not Positive.
          secondsA = fromℤ secondsℤ

      mkPositiveFail secondsA
  where
    chars = "hmds"

readDigits :: (Read n) => Text -> Parsec Void Text n
readDigits b =
  case TR.readMaybe (T.unpack b) of
    Nothing -> fail $ "Could not read digits: " <> T.unpack b
    Just b' -> pure b'

-- | Combines 'parseWith' with 'Parser'.
parse :: (Parser a) => Text -> ResultDefault a
parse = parseWith parser

-- | Combines 'parseAllWith' with 'Parser'.
parseAll :: (Parser a) => Text -> ResultDefault a
parseAll = parseAllWith parser

-- | Runs the given parser. Allows trailing whitespace, but other unconsumed
-- tokens are an error.
parseAllWith :: MParser a -> Text -> ResultDefault a
parseAllWith p = parseWith (lexeme p <* MP.eof)

-- | Runs the given parser. Unconsumed tokens are not an error.
parseWith ::
  ( ShowErrorComponent e,
    TraversableStream s,
    VisualStream s
  ) =>
  Parsec e s a ->
  s ->
  ResultDefault a
parseWith p t = case MP.runParser p "Pacer.Class.Parser.parseWith" t of
  Left err -> Err . MP.errorBundlePretty $ err
  Right v -> Ok v

lexeme :: MParser a -> MParser a
lexeme = Lex.lexeme MPC.space

allExcept :: List Char -> MParser Text
allExcept ts = lexeme $ MP.takeWhile1P (Just "expr") (not . badChar)
  where
    badChar c = Ch.isSpace c || Set.member c badChars
    badChars = Set.fromList ts

char :: Char -> MParser Char
char = lexeme . MPC.char

-- | Takes a text literal, and trailing whitespace.
string :: Text -> MParser Text
string = lexeme . MPC.string

symbols :: List Text -> MParser Text
symbols = asum . fmap symbol

symbol :: Text -> MParser Text
symbol = Lex.symbol MPC.space

binary :: (MonadParsec e s m) => Tokens s -> (a -> a -> a) -> Operator m a
binary t f = InfixL (f <$ MPC.string t)

prefix :: (MonadParsec e s m) => Tokens s -> (a -> a) -> Operator m a
prefix t f = Prefix (f <$ MPC.string t)
