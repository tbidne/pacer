-- | Derive CLI parsing.
module Pacer.Command.Derive.Args
  ( parser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Pacer.Class.Parser qualified as P
import Pacer.Command.Derive.Params
  ( DeriveParams (MkDeriveParams, quantity, unit),
    DeriveParamsArgs,
  )
import Pacer.Config.Utils qualified as Utils
import Pacer.Prelude

-- | Parses derive params.
parser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (DeriveParamsArgs a)
parser = do
  quantity <- Utils.ddpArgsParser helpTxt
  unit <- OA.optional Utils.distanceUnitParser
  pure
    $ MkDeriveParams
      { quantity,
        unit
      }
  where
    helpTxt =
      mconcat
        [ "A pace e.g. '4m30s /km', '1h5m /mi', '4m30'. If the units are not ",
          "given, we use the distance's units. Only kilometers and miles are ",
          "allowed."
        ]
