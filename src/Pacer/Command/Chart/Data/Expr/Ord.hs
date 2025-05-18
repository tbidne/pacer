-- | Provides filters over generic equals.
module Pacer.Command.Chart.Data.Expr.Ord
  ( FilterOpOrd (..),
    toFun,
    toIFun,
  )
where

import Pacer.Class.IOrd (IOrd ((<~)), (<.), (>.), (>~))
import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Expr.Eq (FilterOpEq)
import Pacer.Command.Chart.Data.Expr.Eq qualified as Eq
import Pacer.Prelude

-- | Operator for ord comparisons.
data FilterOpOrd
  = FilterOpOrdEq FilterOpEq
  | FilterOpOrdLte
  | FilterOpOrdLt
  | FilterOpOrdGte
  | FilterOpOrdGt
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterOpOrd where
  displayBuilder = \case
    FilterOpOrdEq op -> displayBuilder op
    FilterOpOrdLte -> "≤"
    FilterOpOrdLt -> "<"
    FilterOpOrdGte -> "≥"
    FilterOpOrdGt -> ">"

instance Parser FilterOpOrd where
  parser =
    asum
      [ parser <&> FilterOpOrdEq,
        P.char '≤' $> FilterOpOrdLte,
        P.string "<=" $> FilterOpOrdLte,
        P.char '<' $> FilterOpOrdLt,
        P.char '≥' $> FilterOpOrdGte,
        P.string ">=" $> FilterOpOrdGte,
        P.char '>' $> FilterOpOrdGt
      ]

toFun :: forall b. (Ord b) => FilterOpOrd -> (b -> b -> Bool)
toFun (FilterOpOrdEq op) = Eq.toFun op
toFun FilterOpOrdLte = (<=)
toFun FilterOpOrdLt = (<)
toFun FilterOpOrdGte = (>=)
toFun FilterOpOrdGt = (>)

toIFun :: (IOrd b) => FilterOpOrd -> (b -> b -> Bool)
toIFun (FilterOpOrdEq op) = Eq.toIFun op
toIFun FilterOpOrdLte = (<~)
toIFun FilterOpOrdLt = (<.)
toIFun FilterOpOrdGte = (>~)
toIFun FilterOpOrdGt = (>.)
