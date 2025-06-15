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

    -- * Comment stripping
    stripComments,

    -- ** Low-level
    stripCommentsBS,
    stripCommentsMpAuto,
    stripCommentsMpManual,
  )
where

import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix))
import Data.ByteString qualified as BS
import Data.Char qualified as Ch
import Data.Coerce (Coercible, coerce)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Format (ParseTime)
import Data.Time.Format qualified as Format
import Data.Time.Relative qualified as Rel
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
import Text.Megaparsec.Byte qualified as MPB
import Text.Megaparsec.Byte.Lexer qualified as MPBL
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
  default parser :: (Coercible Text a) => MParser a
  parser = do
    str <-
      MP.takeWhile1P
        (Just "nonempty-string")
        (const True)
    pure $ coerce str

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

instance Parser Day where
  parser = do
    str <- unpackText <$> dateParser
    case parseTime fmt str of
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

instance Parser LocalTime where
  parser = do
    dayTxt <- dateParser

    _ <- dateTimeSepParser

    timeTxt <- timeParser

    let str = unpackText (dayTxt <> "T" <> timeTxt)

    case parseTime fmt str of
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
      fmt = "%Y-%m-%dT%H:%M:%S"

instance Parser ZonedTime where
  parser = do
    dayTxt <- dateParser

    _ <- dateTimeSepParser

    timeTxt <- timeParser

    _ <- MP.optional (MPC.char ' ')

    zoneTxt <- zonedParser

    let str = unpackText (dayTxt <> "T" <> timeTxt <> zoneTxt)

    case parseTime fmt str of
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
      fmt = "%Y-%m-%dT%H:%M:%S%z"

dateParser :: MParser Text
dateParser = MP.takeWhile1P (Just "date") (\c -> Ch.isDigit c || c == '-')

dateTimeSepParser :: MParser Char
dateTimeSepParser = MPC.char 'T' <|> MPC.char ' '

timeParser :: MParser Text
timeParser = MP.takeWhile1P (Just "time") (\c -> Ch.isDigit c || c == ':')

zonedParser :: MParser Text
zonedParser =
  MP.takeWhile1P (Just "tz") (\c -> Ch.isDigit c || c == '+' || c == '-')

parseTime :: (ParseTime a) => String -> String -> ResultDefault a
parseTime = Format.parseTimeM False Format.defaultTimeLocale

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

blexeme :: Parsec Void ByteString a -> Parsec Void ByteString a
blexeme = Lex.lexeme MPB.space

-- | Strips a bytestring of line (//) and block (/* */) comments.
stripComments :: ByteString -> ResultDefault ByteString
-- Benchmarks on CI show stripCommentsBS to be the fastest and use the least
-- memory.
stripComments = stripCommentsBS

-- | Strips comment using ByteString's API.
stripCommentsBS :: ByteString -> ResultDefault ByteString
stripCommentsBS bs =
  let (preComment, mWithFSlash) = BS.break (== fslashW8) bs
   in case uncons2 mWithFSlash of
        -- 1. Remaining text is either 1 char or a single fslash: It cannot
        -- possibly start a comment, so return it.
        Nothing -> Ok $ preComment <> mWithFSlash
        -- 2. Some remaining text. Need to check for a comment start.
        --
        -- - fslashChar: a single fslash
        -- - fslashNext: next char, need to check
        -- - mPostCommentStart: The part after potential comment start.
        Just (fslashChar, fslashNext, mPostCommentStart)
          | fslashNext == fslashW8 ->
              -- 2.1. Another fslash, we have started a line comment. Skip until
              -- the next newline.
              second
                (preComment <>)
                (skipLineComment mPostCommentStart >>= stripCommentsBS)
          | fslashNext == starW8 ->
              -- 2.2. A star, we have started a block comment. Skip until the
              -- next '*/'.
              second
                (preComment <>)
                (skipBlockComment mPostCommentStart >>= stripCommentsBS)
          -- 2.3. No fslash or star i.e. not a comment start. Concat it back in,
          -- and proceed with the rest of the string.
          | otherwise ->
              let start = preComment <> BS.pack [fslashChar, fslashNext]
               in second (start <>) (stripCommentsBS mPostCommentStart)
  where
    -- es is the start of a line comment, w/o the opening '//'.
    skipLineComment es =
      let (_lineComment, mCommentEnd) = BS.break (== newlineW8) es
       in case BS.uncons mCommentEnd of
            -- Did not find a closing newline: error!
            Nothing -> Err "Found line comment (//) without ending newline."
            Just (_nl, rest) -> Ok rest

    -- es is the start of a block comment, w/o the opening '/*'.
    skipBlockComment es =
      let (_blockComment, mCommentEnd) = BS.break (== starW8) es
       in case uncons2 mCommentEnd of
            -- es had fewer than 2 chars, so it could not possibly end the
            -- comment, error.
            Nothing -> Err "Found block comment (/*) without ending (*/)."
            -- If we get here we know we have found a star char and at least one
            -- other char (starNext).
            Just (_starChar, starNext, mPostCommentStart)
              -- Found an ending slash, we have successfully ended the comment.
              | starNext == fslashW8 -> Ok mPostCommentStart
              -- starNext is something else. Search the rest of the string.
              | otherwise -> skipBlockComment mPostCommentStart

uncons2 :: ByteString -> Maybe (Word8, Word8, ByteString)
uncons2 bs = case BS.uncons bs of
  Nothing -> Nothing
  Just (c1, rest1) -> case BS.uncons rest1 of
    Nothing -> Nothing
    Just (c2, rest2) -> Just (c1, c2, rest2)

-- | Strips comment using megaparsec's built-in comment utilities.
stripCommentsMpAuto :: ByteString -> ResultDefault ByteString
stripCommentsMpAuto = fmap mconcat . parseWith (blexeme bsParser <* MP.eof)
  where
    bsParser :: Parsec Void ByteString (List ByteString)
    bsParser =
      many
        $ asum
          [ parseLineComment,
            MPBL.skipBlockComment "/*" "*/" $> "",
            parseNonComment,
            parseOneFSlash
          ]
      where
        parseLineComment :: Parsec Void ByteString ByteString
        parseLineComment = do
          _ <- MPBL.skipLineComment "//"
          -- skipLineComment does not consume the newline, so we have to
          _ <- MPB.char newlineW8
          pure ""

-- | Strips comment using hand-rolled megaparsec.
stripCommentsMpManual :: ByteString -> ResultDefault ByteString
stripCommentsMpManual = fmap mconcat . parseWith (blexeme bsParser <* MP.eof)
  where
    bsParser :: Parsec Void ByteString (List ByteString)
    bsParser =
      many
        $ asum
          [ parseLineComment,
            parseBlockComment,
            parseNonComment,
            parseOneFSlash
          ]
      where
        parseLineComment :: Parsec Void ByteString ByteString
        parseLineComment = do
          parseLineCommentStart
          _ <- MP.takeWhileP (Just "text") (/= newlineW8)
          MPB.char newlineW8
          pure ""

        parseLineCommentStart :: Parsec Void ByteString ()
        parseLineCommentStart = void $ MPC.string "//"

        parseBlockComment :: Parsec Void ByteString ByteString
        parseBlockComment = do
          parseStartBlock
          takeUntilEndBlock
          parseEndBlock
          pure ""

        takeUntilEndBlock :: Parsec Void ByteString ()
        takeUntilEndBlock = void $ do
          many
            $ asum
              [ parseNonStar,
                parseOneStar
              ]

        parseNonStar :: Parsec Void ByteString ()
        parseNonStar = void $ MP.takeWhile1P (Just "text") (/= starW8)

        -- We need to backtrack (hence try) in case the first star
        -- succeeds but the followedBy fails.
        parseOneStar :: Parsec Void ByteString ()
        parseOneStar = MP.try $ do
          _ <- MPB.char starW8
          MP.notFollowedBy $ MPB.char fslashW8
          pure ()

        parseStartBlock :: Parsec Void ByteString ()
        parseStartBlock = void $ MPC.string "/*"

        parseEndBlock :: Parsec Void ByteString ()
        parseEndBlock = void $ MPC.string "*/"

parseOneFSlash :: Parsec Void ByteString ByteString
parseOneFSlash = do
  c <- MPB.char fslashW8
  MP.notFollowedBy $ MPB.char fslashW8 <|> MPB.char starW8
  pure $ BS.singleton c

parseNonComment :: Parsec Void ByteString ByteString
parseNonComment = MP.takeWhile1P (Just "text") (/= fslashW8)

newlineW8 :: Word8
newlineW8 = i2w8 $ Ch.ord '\n'

starW8 :: Word8
starW8 = i2w8 $ Ch.ord '*'

fslashW8 :: Word8
fslashW8 = i2w8 $ Ch.ord '/'

i2w8 :: Int -> Word8
i2w8 = fromIntegral
