{-# LANGUAGE UndecidableInstances #-}

-- | Convert parameters.
module Pacer.Command.Convert.Params
  ( ConvertParams (..),
    ConvertParamsArgs,
    ConvertParamsFinal,

    -- * Functions
    evolvePhase,

    -- * Type families
    ConvertParamsF,
  )
where

import Pacer.Config.Phase (ConfigPhase (ConfigPhaseArgs, ConfigPhaseFinal))
import Pacer.Config.Utils
  ( DistancePaceArgs
      ( mSomeDistance,
        mSomePace
      ),
  )
import Pacer.Data.Distance (DistanceUnit, SomeDistance)
import Pacer.Data.Pace (SomePace)
import Pacer.Exception
  ( CommandConvertE
      ( CommandConvertArgs0,
        CommandConvertArgs2
      ),
  )
import Pacer.Prelude

type family ConvertParamsF p a where
  ConvertParamsF ConfigPhaseArgs a = DistancePaceArgs a
  ConvertParamsF ConfigPhaseFinal a =
    Either (SomeDistance (Positive a)) (SomePace (Positive a))

-- | Convert params.
type ConvertParams :: ConfigPhase -> Type -> Type
data ConvertParams p a = MkConvertParams
  { -- | Quantity to convert.
    quantity :: ConvertParamsF p a,
    -- | Output unit.
    unit :: DistanceUnit
  }

type ConvertParamsArgs a = ConvertParams ConfigPhaseArgs a

type ConvertParamsFinal a = ConvertParams ConfigPhaseFinal a

deriving stock instance (Eq (ConvertParamsF p a)) => Eq (ConvertParams p a)

deriving stock instance (Show (ConvertParamsF p a)) => Show (ConvertParams p a)

-- | Evolve convert params' phase.
evolvePhase ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  ConvertParamsArgs a ->
  m (ConvertParamsFinal a)
evolvePhase args =
  case ( dpArgs.mSomeDistance,
         dpArgs.mSomePace
       ) of
    (Just _, Just _) -> throwM CommandConvertArgs2
    (Nothing, Nothing) -> throwM CommandConvertArgs0
    (Just a, Nothing) -> pure $ MkConvertParams (Left a) args.unit
    (Nothing, Just b) -> pure $ MkConvertParams (Right b) args.unit
  where
    dpArgs = args.quantity
