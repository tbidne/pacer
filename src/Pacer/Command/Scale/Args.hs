-- | Scale CLI parsing.
module Pacer.Command.Scale.Args
  ( parser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Pacer.Class.Parser qualified as P
import Pacer.Command.Scale.Params
import Pacer.Config.Utils qualified as Utils
import Pacer.Prelude

-- | Parses scale params.
parser ::
  forall a.
  ( Fromℚ a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (ScaleParamsArgs a)
parser = do
  quantity <- Utils.ddpArgsParser "A pace e.g. '4m30s /km', '1h5m /mi', '4m30'."
  factor <- factorParser
  unit <- OA.optional Utils.distanceUnitParser
  pure
    $ MkScaleParams
      { quantity,
        factor,
        unit
      }

factorParser ::
  forall a.
  ( AMonoid a,
    Ord a,
    P.Parser a,
    Show a
  ) =>
  Parser (Positive a)
factorParser =
  OA.option
    Utils.readParseable
    ( mconcat
        [ OA.short 'k',
          OA.long "factor",
          OA.metavar "POS_INT",
          Utils.mkHelp "The scaling factor."
        ]
    )
