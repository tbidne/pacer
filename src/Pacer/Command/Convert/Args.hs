-- | Convert CLI parsing.
module Pacer.Command.Convert.Args
  ( parser,
  )
where

import Options.Applicative (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Convert.Params
  ( ConvertParams (MkConvertParams, quantity, unit),
    ConvertParamsArgs,
  )
import Pacer.Config.Utils qualified as Utils
import Pacer.Prelude

-- | Parse convert args.
parser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (ConvertParamsArgs a)
parser = do
  quantity <- Utils.dpArgsParser
  unit <- Utils.distanceUnitParser

  pure
    $ MkConvertParams
      { quantity,
        unit
      }
