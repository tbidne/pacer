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
import Data.Time.Format qualified as Format
import Data.Time.Relative qualified as Rel
import Data.Word (Word16)
import Numeric.Data.Fraction.Algebra (mkFraction)
import Pacer.Prelude
import Pacer.Utils (EitherString (EitherLeft, EitherRight))
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as MP
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

instance Parser ℚNonNeg where
  parser = do
    -- Parse to double since we want to allow decimal notation, rather than
    -- Rational's weird fraction notation.
    x <- parseDigits @Double
    let n :% d = fromℝ @Rational x

    if n < 0 || d < 0
      then fail $ "Parsed non-negative fraction from: " ++ show x
      else do
        let n' = fromℤ n
            d' = fromℤ d

        -- Somewhat redundant because of above checks. Alternatively, we could
        -- create an UnsafeFraction and reduce.
        case mkFraction n' d' of
          Just f -> pure f
          Nothing ->
            fail $ "Failed making fraction from: " ++ show n ++ " / " ++ show d

instance (AMonoid a, Ord a, Parser a, Show a) => Parser (Positive a) where
  parser = do
    d <- parser
    case mkPositive d of
      Nothing -> fail $ "Parsed non-positive: " ++ show d
      Just x -> pure x

instance Parser LocalTime where
  parser = do
    str <- unpackText <$> MP.takeWhile1P Nothing (\c -> Ch.isDigit c || c == '-')
    case Format.parseTimeM False Format.defaultTimeLocale fmt str of
      EitherRight d -> pure d
      EitherLeft err ->
        fail
          $ mconcat
            [ "Failed parsing localtime from string '",
              str,
              "': ",
              err
            ]
    where
      fmt = "%Y-%m-%dT%H:%M:%S"

instance Parser ZonedTime where
  parser = do
    str <- unpackText <$> MP.takeWhile1P Nothing (\c -> Ch.isDigit c || c == '-')
    case Format.parseTimeM False Format.defaultTimeLocale fmt str of
      EitherRight d -> pure d
      EitherLeft err ->
        fail
          $ mconcat
            [ "Failed parsing zoned time from string '",
              str,
              "': ",
              err
            ]
    where
      fmt = "%Y-%m-%dT%H:%M:%S%z"

instance Parser Day where
  parser = do
    str <- unpackText <$> MP.takeWhile1P Nothing (\c -> Ch.isDigit c || c == '-')
    case Format.parseTimeM False Format.defaultTimeLocale fmt str of
      EitherRight d -> pure d
      EitherLeft err ->
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
  MP.takeWhile1P (Just "parseDigitText") (\c -> Ch.isDigit c || c == '.')

-- | Parser combinator for digits.
parseIntegralText :: Parsec Void Text Text
parseIntegralText = MP.takeWhile1P (Just "parseIntegralText") Ch.isDigit

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
