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
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Data.Distance (DistanceUnit, SomeDistance)
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Pace (SomePace)
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

instance ToJSON YAxisType where
  toJSON YAxisDistance = "distance"
  toJSON YAxisDuration = "duration"
  toJSON YAxisPace = "pace"

-- | Operator for filter comparisons. The text field is just so we can have
-- Eq and Show instances, though they are of arguably little value.
data FilterOp = MkFilterOp Text (forall a. (Ord a) => a -> a -> Bool)

instance Show FilterOp where
  show (MkFilterOp txt _) = "MkFilterOp " ++ unpackText txt ++ " _"

instance Eq FilterOp where
  MkFilterOp t1 _ == MkFilterOp t2 _ = t1 == t2

instance Parser FilterOp where
  parser =
    MP.choice
      [ MPC.string "<=" $> MkFilterOp "<=" (<=),
        MPC.char '<' $> MkFilterOp "<" (<),
        MPC.char '=' $> MkFilterOp "==" (==),
        MPC.string ">=" $> MkFilterOp ">=" (>=),
        MPC.char '>' $> MkFilterOp ">" (>)
      ]

-- | Ways in which we can filter runs.
data FilterType a
  = -- | Filters by label equality.
    FilterLabel Text
  | FilterDistance FilterOp (SomeDistance (Positive a))
  | FilterDuration FilterOp (Seconds (Positive a))
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
    MP.choice
      [ parseLabel,
        parseDist,
        parseDuration,
        parsePace
      ]
    where
      parseLabel = do
        void $ MPC.string "label "
        FilterLabel <$> MP.takeWhile1P Nothing (/= ')')

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
  deriving stock (Eq, Show)

eval :: (a -> Bool) -> Expr a -> Bool
eval p = go
  where
    go (Atom x) = p x
    go (Not e) = not (go e)
    go (Or e1 e2) = go e1 || go e2
    go (And e1 e2) = go e1 && go e2

-- | Alias for a filter expression.
type FilterExpr a = Expr (FilterType a)

instance (Parser a) => Parser (Expr a) where
  parser = parseFilter
    where
      parseFilter = do
        MP.choice
          [ parseNot,
            parseOr,
            parseAnd,
            parseXor,
            parseAtom
          ]

      -- NOTE: In theory, we could make parens optional, and simply try
      -- parsing an atom e.g.
      --
      --     not distance > 5 km
      --
      -- However, as all of our atoms are multiple words -- thus
      -- require/support whitespace -- parens unambiguously improve
      -- readability, so we require them. The only change that maybe makes
      -- sense here is making parens part of the atom parser.
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

      parseAnd = do
        MPC.string "and ("
        f1 <- parseFilter
        MPC.string ") ("
        f2 <- parseFilter
        MPC.char ')'
        pure $ And f1 f2

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
