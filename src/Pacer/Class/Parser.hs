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
    parseTokWith,

    -- ** Digits
    parseDigits,
    parseDigitText,
    readDigits,

    -- ** TimeString
    parseTimeString,

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
import Pacer.Data.Result (Result (Err, Ok))
import Pacer.Prelude
import Text.Megaparsec (MonadParsec, Parsec, Stream (Tokens))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as Lex
import Text.Read qualified as TR

-- | Main parsing type.
type MParser a = Parsec Void Text a

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

-- | Read text like "1d2h3m4s", parse w/ relative time into Fractional
-- seconds.
parseTimeString :: forall a. (FromInteger a) => MParser a
parseTimeString = do
  t <-
    MP.takeWhile1P
      (Just "time-string")
      (\c -> Ch.isDigit c || c `T.elem` chars)
  case Rel.fromString (unpackText t) of
    Left err -> fail $ "Could not read duration: " ++ err
    Right rt -> do
      let secondsℕ = Rel.toSeconds rt
          secondsℤ = toℤ secondsℕ
          secondsA = fromℤ secondsℤ

      pure secondsA
  where
    chars = "hmds"

readDigits :: (Read n) => Text -> Parsec Void Text n
readDigits b =
  case TR.readMaybe (T.unpack b) of
    Nothing -> fail $ "Could not read digits: " <> T.unpack b
    Just b' -> pure b'

-- | Combines 'parseWith' with 'Parser'.
parse :: (Parser a) => Text -> Result a
parse = parseWith parser

-- | Combines 'parseAllWith' with 'Parser'.
parseAll :: (Parser a) => Text -> Result a
parseAll = parseAllWith parser

-- | Runs the given parser. Allows trailing whitespace, but other unconsumed
-- tokens are an error.
parseAllWith :: MParser a -> Text -> Result a
parseAllWith p = parseWith (lexeme p <* MP.eof)

-- | Runs the given parser. Unconsumed tokens are not an error.
parseWith :: MParser a -> Text -> Result a
parseWith p t = case MP.runParser p "Pacer.Class.Parser.parseWith" t of
  Left err -> Err . MP.errorBundlePretty $ err
  Right v -> Ok v

-- | Like 'parseWith', except over an arbitrary token. It would be nice to
-- consolidate these, though we'd have to ensure our consumers implement the
-- correct classes to use errorBundlePretty.
parseTokWith :: (Show e, Show s, Show (MP.Token s)) => Parsec e s a -> s -> Result a
parseTokWith p t = case MP.runParser p "Pacer.Class.Parser.parseWith" t of
  Left err -> Err . show $ err
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

binary :: (MonadParsec e s m) => List (Tokens s) -> (a -> a -> a) -> Operator m a
binary ts f = InfixL (f <$ p)
  where
    p = asum $ fmap MPC.string ts

prefix :: (MonadParsec e s m) => List (Tokens s) -> (a -> a) -> Operator m a
prefix ts f = Prefix (f <$ p)
  where
    p = asum $ fmap MPC.string ts
