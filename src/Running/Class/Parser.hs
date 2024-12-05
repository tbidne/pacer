-- | Provides parsing functionality.
module Running.Class.Parser
  ( Parser (..),
    MParser,
    parse,

    -- * Digits
    parseDigits,
    parseDigitText,
    readDigits,
  )
where

import Data.Char qualified as Ch
import Data.Text qualified as T
import Running.Prelude
import Text.Megaparsec (Parsec)
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
--
-- @since 0.1
parseDigits :: (Read n) => Parsec Void Text n
parseDigits = parseDigitText >>= readDigits

-- | Parser combinator for digits.
--
-- @since 0.1
parseDigitText :: Parsec Void Text Text
parseDigitText =
  MP.takeWhile1P Nothing (\c -> Ch.isDigit c || c == '.')

readDigits :: (Read n) => Text -> Parsec Void Text n
readDigits b =
  case TR.readMaybe (T.unpack b) of
    Nothing -> fail $ "Could not read digits: " <> T.unpack b
    Just b' -> pure b'

parse :: (Parser a) => Text -> Either Text a
parse t = case MP.runParser parser "Running.Class.Parser.parse" t of
  Left err -> Left . T.pack . MP.errorBundlePretty $ err
  Right v -> Right v
