-- | Provides parsing functionality.
module Pacer.Class.Parser
  ( Parser (..),
    MParser,
    parse,
    parseFail,
    parseWith,
    parseWithFail,

    -- * Digits
    parseDigits,
    parseDigitText,
    readDigits,

    -- * TimeString
    parseTimeString,

    -- * Misc
    failNonSpace,
    nonSpace,
    optionalTry,
  )
where

import Data.Char qualified as Ch
import Data.Text qualified as T
import Data.Time.Relative qualified as Rel
import Pacer.Prelude
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Read qualified as TR

-- | Main parsing type.
type MParser a = Parsec Void Text a

-- | Class for parsing.
class Parser a where
  -- | Megaparsec parser for the given type.
  parser :: MParser a

instance Parser Double where
  parser = parseDigits

instance Parser PDouble where
  parser = do
    d <- parser
    case mkPositive d of
      Nothing -> fail $ "Parsed non-positive: " ++ show d
      Just x -> pure x

-- | Parser combinator for digits with a 'Read' instance.
parseDigits :: (Read n) => Parsec Void Text n
parseDigits = parseDigitText >>= readDigits

-- | Parser combinator for digits.
parseDigitText :: Parsec Void Text Text
parseDigitText =
  MP.takeWhile1P Nothing (\c -> Ch.isDigit c || c == '.')

-- | Read text like "1d2h3m4s", parse w/ relative time into Fractional
-- seconds.
parseTimeString :: forall a. (FromInteger a) => MParser a
parseTimeString = do
  t <- MP.takeWhile1P Nothing (\c -> Ch.isDigit c || c `T.elem` chars)
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

parse :: (Parser a) => Text -> Either Text a
parse = parseWith parser

parseFail :: forall a m. (MonadFail m, Parser a) => Text -> m a
parseFail t = case parse t of
  Left err -> fail $ unpackText err
  Right x -> pure x

parseWith :: MParser a -> Text -> Either Text a
parseWith p t = case MP.runParser (p <* MP.eof) "Pacer.Class.Parser.parseWith" t of
  Left err -> Left . T.pack . MP.errorBundlePretty $ err
  Right v -> Right v

parseWithFail :: forall a m. (MonadFail m) => MParser a -> Text -> m a
parseWithFail p t = case parseWith p t of
  Left err -> fail $ unpackText err
  Right x -> pure x

failNonSpace :: MParser ()
failNonSpace = MP.notFollowedBy nonSpace

nonSpace :: MParser Char
nonSpace = MP.satisfy (not . Ch.isSpace) <?> "non white space"

optionalTry :: MParser a -> MParser (Maybe a)
optionalTry = MP.optional . MP.try
