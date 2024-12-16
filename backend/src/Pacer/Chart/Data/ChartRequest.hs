module Pacer.Chart.Data.ChartRequest
  ( -- * ChartRequest
    ChartRequest (..),
    FilterType (..),
    Expr (..),
    eval,
    FilterExpr,
    YAxisType (..),

    -- * ChartRequests
    ChartRequests (..),
  )
where

import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import TOML
  ( DecodeTOML (tomlDecoder),
  )
import TOML qualified
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

-- TODO: More filters e.g. inequalities w/ distance, duration, pace

-- | Ways in which we can filter runs.
newtype FilterType
  = -- | Filters by label equality.
    FilterLabel Text
  deriving stock (Eq, Show)

instance Parser FilterType where
  parser = parseLabel
    where
      parseLabel = FilterLabel <$> MP.takeWhile1P Nothing (/= ')')

-- | Expressions upon which we can filter
data Expr a
  = -- | "expr"
    Atom a
  | -- | "not (expr)"
    Not (Expr a) -- "not (expr)"
  | -- | "or (expr) (expr)""
    Or (Expr a) (Expr a)
  | -- | To keep the expression simple, we don't actually parse AND
    --  (AND is represented by multiple filters). Its inclusion here is purely
    -- to represent Xor.
    And (Expr a) (Expr a)
  deriving stock (Eq, Show)

eval :: (a -> Bool) -> Expr a -> Bool
eval p = go
  where
    go (Atom x) = p x
    go (Not e) = not (go e)
    go (Or e1 e2) = go e1 || go e2
    go (And e1 e2) = go e1 && go e2

-- | Alias for a filter expression.
type FilterExpr = Expr FilterType

instance (Parser a) => Parser (Expr a) where
  parser = parseFilter
    where
      parseFilter = do
        MP.choice
          [ parseNot,
            parseOr,
            parseXor,
            parseAtom
          ]

      -- REVIEW: Can we eliminate unnecessary parens? E.g. not (label),
      -- or (label1) (label2)?
      parseNot = do
        MPC.string "not ("
        f <- parseFilter
        MPC.char ')'
        pure $ Not f

      parseOr = do
        -- Can't do infix due to lack of left-recusion in megaparsec.
        -- Maybe we can swap for something fancy like earley.
        MPC.string "or ("
        f1 <- parseFilter
        MPC.string ") ("
        f2 <- parseFilter
        MPC.char ')'
        pure $ Or f1 f2

      parseXor = do
        MPC.string "xor ("
        f1 <- parseFilter
        MPC.string ") ("
        f2 <- parseFilter
        MPC.char ')'
        pure $ Or (And f1 (Not f2)) (And (Not f1) f2)

      parseAtom = Atom <$> P.parser

instance (Parser a) => DecodeTOML (Expr a) where
  tomlDecoder = tomlDecoder >>= P.parseFail

-- TODO: Chart request could have optional units

-- | Chart request type.
data ChartRequest = MkChartRequest
  { -- | Optional list of filters to apply. The filters are "AND'd" together.
    filters :: List FilterExpr,
    -- | Title for this chart.
    title :: Text,
    -- | Y-axis value.
    yAxis :: YAxisType,
    -- | Optional second y-axis.
    yAxis1 :: Maybe YAxisType
  }
  deriving stock (Eq, Show)

instance DecodeTOML ChartRequest where
  tomlDecoder = do
    filters <- Utils.getFieldOptArrayOf "filters"
    title <- TOML.getFieldWith tomlDecoder "title"
    yAxis <- TOML.getFieldWith tomlDecoder "y-axis"
    yAxis1 <- TOML.getFieldOptWith tomlDecoder "y-axis1"
    pure
      $ MkChartRequest
        { filters,
          title,
          yAxis,
          yAxis1
        }

-- | List of chart requests.
newtype ChartRequests = MkChartRequests {unChartRequests :: Seq ChartRequest}
  deriving stock (Eq, Show)

instance DecodeTOML ChartRequests where
  tomlDecoder = MkChartRequests <$> TOML.getFieldWith tomlDecoder "charts"
