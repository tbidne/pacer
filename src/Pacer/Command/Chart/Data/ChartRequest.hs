{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.ChartRequest
  ( -- * ChartRequest
    ChartRequest (..),
    YAxisType (..),

    -- ** Filtering
    FilterType (..),
    FilterOp (..),
    Expr (..),
    eval,
    FilterExpr,

    -- * ChartRequests
    ChartRequests (..),

    -- * Misc
    debugLex,
  )
where

import Control.Monad.Combinators.Expr (Operator)
import Control.Monad.Combinators.Expr qualified as Combs
import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Text qualified as T
import Pacer.Class.Parser (MParser, Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time (Moment)
import Pacer.Data.Distance (DistanceUnit, SomeDistance)
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Pace (SomePace)
import Pacer.Data.Result (Result (Err, Ok))
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import System.IO qualified as IO
import TOML
  ( DecodeTOML (tomlDecoder),
  )
import TOML qualified
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Possibly y-axes.
data YAxisType
  = YAxisDistance
  | YAxisDuration
  | YAxisPace
  deriving stock (Eq, Show)

instance DecodeTOML YAxisType where
  tomlDecoder =
    tomlDecoder @Text >>= \case
      "distance" -> pure YAxisDistance
      "duration" -> pure YAxisDuration
      "pace" -> pure YAxisPace
      other -> fail $ unpackText err
        where
          err =
            mconcat
              [ "Unexpected y-axis, '",
                other,
                "', expected one of (distance|duration|pace)."
              ]

instance ToJSON YAxisType where
  toJSON YAxisDistance = "distance"
  toJSON YAxisDuration = "duration"
  toJSON YAxisPace = "pace"

-- | Operator for filter comparisons. The text field is just so we can have
-- Eq and Show instances, though they are of arguably little value.
-- data FilterOp = MkFilterOp Text (forall a. (Ord a) => a -> a -> Bool)
data FilterOp
  = FilterOpEq
  | FilterOpNeq
  | FilterOpLte
  | FilterOpLt
  | FilterOpGte
  | FilterOpGt
  deriving stock (Eq, Show)

instance Parser FilterOp where
  parser =
    asum
      -- NOTE: These use megaparsec's variants rather than ours defined in
      -- Pacer.Class.Parser which parse all trailing whitespace too.
      -- We currently need this since it is used in (FilterType a)'s Parser
      -- instance, and we require space1 there. It would be nice to figure
      -- out a more consistent implementation here.
      [ MPC.string "<=" $> FilterOpLte,
        MPC.char '<' $> FilterOpLt,
        MPC.char '=' $> FilterOpEq,
        MPC.string ">=" $> FilterOpGte,
        MPC.char '>' $> FilterOpGt
      ]

-- | Ways in which we can filter runs.
data FilterType a
  = FilterDistance FilterOp (SomeDistance (Positive a))
  | FilterDuration FilterOp (Seconds (Positive a))
  | -- | Filters by label equality.
    FilterLabel Text
  | FilterDate FilterOp Moment
  | FilterPace FilterOp (SomePace (Positive a))
  deriving stock (Eq, Show)

instance
  ( FromRational a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (FilterType a)
  where
  parser =
    asum
      [ parseLabel <?> "label",
        parseDist <?> "distance",
        parseDuration <?> "duration",
        parsePace <?> "pace",
        parseDate <?> "datetime"
      ]
    where
      parseLabel = do
        void $ MPC.string "label"
        MPC.space1
        FilterLabel <$> MP.takeWhile1P (Just "string") (const True)

      parseDate = do
        MPC.string "datetime"
        MPC.space1
        op <- parser
        MPC.space1
        m <- parser
        pure $ FilterDate op m

      parseDist = parsePred "distance" FilterDistance
      parseDuration = parsePred "duration" FilterDuration
      parsePace = parsePred "pace" FilterPace

      parsePred s cons = do
        MPC.string s
        MPC.space1
        op <- parser
        MPC.space1
        d <- parser
        pure $ cons op d

data ExprToken
  = -- Values
    TokenString Text
  | -- Exprs
    TokenExprAnd
  | TokenExprNot
  | TokenExprOr
  | TokenExprXor
  | -- Parens
    TokenParenL
  | TokenParenR
  deriving stock (Eq, Ord, Show)

exprLexer :: MParser (List ExprToken)
exprLexer =
  some
    $ asum
      [ TokenExprAnd <$ P.string "and",
        TokenExprNot <$ P.string "not",
        TokenExprOr <$ P.string "or",
        TokenExprXor <$ P.string "xor",
        TokenParenL <$ P.symbol "(",
        TokenParenR <$ P.symbol ")",
        TokenString <$> P.allExcept "()"
      ]

-- | Expressions upon which we can filter
data Expr a
  = -- | "expr"
    Atom a
  | -- | "not (expr)"
    Not (Expr a)
  | -- | "or (expr) (expr)"
    Or (Expr a) (Expr a)
  | -- | "and (expr) (expr)"
    And (Expr a) (Expr a)
  deriving stock (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Alias for a filter expression.
type FilterExpr a = Expr (FilterType a)

instance (Parser a) => DecodeTOML (Expr a) where
  tomlDecoder = do
    txt <- tomlDecoder @Text

    case P.parseAllWith exprLexer txt of
      Err err -> fail $ "Token failure: " ++ err
      Ok ts -> case P.parseTokWith exprParser ts of
        Err err2 ->
          fail
            $ mconcat
              [ "Tokens: ",
                show ts,
                "\n\nExpr Text err: " ++ err2
              ]
        Ok expr -> case traverse P.parseAll expr of
          Err err3 ->
            fail
              $ mconcat
                [ "Tokens: ",
                  show ts,
                  "\n\nExpr Text: ",
                  show expr,
                  "\n\nExpr a err: " ++ err3
                ]
          Ok x -> pure x

eval :: (a -> Bool) -> Expr a -> Bool
eval p = runIdentity . evalA (\x -> Identity (p x))

evalA :: (Applicative f) => (a -> f Bool) -> Expr a -> f Bool
evalA p = go
  where
    go (Atom x) = p x
    go (Not e) = not <$> go e
    go (Or e1 e2) = liftA2 (||) (go e1) (go e2)
    go (And e1 e2) = liftA2 (&&) (go e1) (go e2)

exprParser :: Parsec Void (List ExprToken) (Expr Text)
exprParser = Combs.makeExprParser term table <?> "expression"

term :: Parsec Void (List ExprToken) (Expr Text)
term = parens <|> p
  where
    parens = MP.single TokenParenL *> exprParser <* MP.single TokenParenR

    p :: Parsec Void (List ExprToken) (Expr Text)
    p = do
      ts <-
        MP.takeWhile1P
          (Just "atom")
          (\case TokenString _ -> True; _ -> False)
      pure $ Atom $ T.intercalate " " $ fmap concatStrs ts
      where
        concatStrs (TokenString t) = t
        concatStrs _ = ""

table :: List (List (Operator (Parsec Void (List ExprToken)) (Expr Text)))
table =
  [ [ P.prefix [[TokenExprNot]] Not
    ],
    [ P.binary [[TokenExprAnd]] And,
      P.binary [[TokenExprOr]] Or,
      P.binary [[TokenExprXor]] (\x y -> Or (And x (Not y)) (And (Not x) y))
    ]
  ]

-- | Chart request type.
data ChartRequest a = MkChartRequest
  { -- | Optional text description.
    description :: Maybe Text,
    -- | Optional list of filters to apply. The filters are "AND'd" together.
    filters :: List (FilterExpr a),
    -- | Title for this chart.
    title :: Text,
    -- | Optional output unit.
    unit :: Maybe DistanceUnit,
    -- | Y-axis value.
    yAxis :: YAxisType,
    -- | Optional second y-axis.
    y1Axis :: Maybe YAxisType
  }
  deriving stock (Eq, Show)

instance
  ( FromRational a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  DecodeTOML (ChartRequest a)
  where
  tomlDecoder = do
    description <- TOML.getFieldOptWith tomlDecoder "description"
    filters <- Utils.getFieldOptArrayOf "filters"
    title <- TOML.getFieldWith tomlDecoder "title"
    unit <- TOML.getFieldOptWith tomlDecoder "unit"
    yAxis <- TOML.getFieldWith tomlDecoder "y-axis"
    y1Axis <- TOML.getFieldOptWith tomlDecoder "y1-axis"
    pure
      $ MkChartRequest
        { description,
          filters,
          title,
          unit,
          yAxis,
          y1Axis
        }

-- | List of chart requests.
newtype ChartRequests a = MkChartRequests {unChartRequests :: Seq (ChartRequest a)}
  deriving stock (Eq, Show)

instance
  ( FromRational a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  DecodeTOML (ChartRequests a)
  where
  tomlDecoder = MkChartRequests <$> TOML.getFieldWith tomlDecoder "charts"

debugLex :: Text -> IO ()
debugLex txt =
  case P.parseAllWith exprLexer txt of
    Err err -> IO.putStrLn err
    Ok ts -> do
      IO.putStrLn $ "Tokens: " ++ show ts
      case P.parseTokWith exprParser ts of
        Err err2 -> IO.putStrLn err2
        Ok exprTxt -> do
          IO.putStrLn $ "Expr Text: " ++ show exprTxt
          case traverse (P.parseAll @(FilterType Double)) exprTxt of
            Err err3 -> IO.putStrLn err3
            Ok result -> IO.putStrLn $ "Expr F: " ++ show result
