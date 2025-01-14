{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command
  ( -- * Type
    Command (..),
    CommandPhaseArgs,
    CommandPhaseFinal,

    -- * Evolving
    evolvePhase,

    -- * Parsing
    parser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Args qualified as Chart
import Pacer.Command.Chart.Params qualified as Chart
import Pacer.Command.Convert.Args qualified as Convert
import Pacer.Command.Convert.Params qualified as Convert
import Pacer.Command.Derive.Args qualified as Derive
import Pacer.Command.Derive.Params qualified as Derive
import Pacer.Command.Scale.Args qualified as Scale
import Pacer.Command.Scale.Params qualified as Scale
import Pacer.Config.Env.Types (CachedPaths)
import Pacer.Config.Phase (ConfigPhase (ConfigPhaseArgs, ConfigPhaseFinal))
import Pacer.Config.Toml (Toml)
import Pacer.Config.Utils qualified as Utils
import Pacer.Prelude

-- | Possible commands
data Command p a
  = -- | Generate charts
    Chart (Chart.ChartParams p)
  | -- | Converts a quantity.
    Convert (Convert.ConvertParams p a)
  | -- | Given 2 of distance, duration, and pace, derives the 3rd.
    Derive (Derive.DeriveParams p a)
  | -- | Scales a value.
    Scale (Scale.ScaleParams p a)

type CommandPhaseArgs = Command ConfigPhaseArgs

type CommandPhaseFinal = Command ConfigPhaseFinal

deriving stock instance
  ( Eq a,
    Eq (Convert.ConvertParamsF p a),
    Eq (Derive.DeriveQuantityF p a),
    Eq (Chart.PathF p Dir),
    Eq (Chart.PathF p File),
    Eq (Scale.ScaleQuantityF p a)
  ) =>
  Eq (Command p a)

deriving stock instance
  ( Show a,
    Show (Convert.ConvertParamsF p a),
    Show (Derive.DeriveQuantityF p a),
    Show (Chart.PathF p Dir),
    Show (Chart.PathF p File),
    Show (Scale.ScaleQuantityF p a)
  ) =>
  Show (Command p a)

-- | Evolve phase.
evolvePhase ::
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  CommandPhaseArgs a ->
  Maybe Toml ->
  Eff es (CommandPhaseFinal a)
evolvePhase cmd mToml = case cmd of
  Chart params -> Chart <$> Chart.evolvePhase params mToml
  Convert params -> Convert <$> Convert.evolvePhase params
  Derive params -> Derive <$> Derive.evolvePhase params
  Scale params -> Scale <$> Scale.evolvePhase params

-- | Parse command.
parser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (Command ConfigPhaseArgs a)
parser =
  OA.hsubparser
    ( mconcat
        [ Utils.mkCommand "chart" chartParser chartTxt,
          Utils.mkCommand "convert" convertParser convertTxt,
          Utils.mkCommand "derive" deriveParser deriveTxt,
          Utils.mkCommand "scale" scaleParser scaleTxt
        ]
    )
  where
    chartTxt =
      Utils.mkCommandDesc
        $ mconcat
          [ "Generates charts based on a chart-requests file and a runs ",
            "file. Requires npm to be installed."
          ]
    convertTxt =
      Utils.mkCommandDesc
        "Converts a quantity. Requires exactly one quantity and the unit."
    deriveTxt =
      Utils.mkCommandDesc
        $ mconcat
          [ "Given two quantities, derives the third. For instance, given a ",
            "distance and a duration, derives the pace."
          ]

    scaleTxt =
      Utils.mkCommandDesc
        "Scales a quantity. Requires exactly one quantity and the scale factor."

    chartParser = Chart <$> Chart.parser
    convertParser = Convert <$> Convert.parser
    deriveParser = Derive <$> Derive.parser
    scaleParser = Scale <$> Scale.parser
