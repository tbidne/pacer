-- | Provides filters over generic equals.
module Pacer.Command.Chart.Data.Expr.Eq
  ( FilterOpEq (..),
    toFun,
    toIFun,
  )
where

import Pacer.Class.IOrd (IEq ((~~)), (/~))
import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Prelude

-- | Operator for equality comparisons.
data FilterOpEq
  = FilterOpEqEq
  | FilterOpEqNEq
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterOpEq where
  displayBuilder = \case
    FilterOpEqEq -> "="
    FilterOpEqNEq -> "≠"

instance Parser FilterOpEq where
  parser =
    asum
      [ P.char '=' $> FilterOpEqEq,
        P.string "/=" $> FilterOpEqNEq,
        P.char '≠' $> FilterOpEqNEq
      ]

toFun :: forall b. (Eq b) => FilterOpEq -> (b -> b -> Bool)
toFun FilterOpEqEq = (==)
toFun FilterOpEqNEq = (/=)

toIFun :: forall b. (IEq b) => FilterOpEq -> (b -> b -> Bool)
toIFun FilterOpEqEq = (~~)
toIFun FilterOpEqNEq = (/~)
