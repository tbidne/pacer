{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Scale parameters.
module Pacer.Command.Scale.Params
  ( -- * Types
    ScaleParams (..),
    ScaleParamsArgs,
    ScaleParamsFinal,
    ScaleQuantity (..),

    -- * Functions
    evolvePhase,

    -- * Type families
    ScaleQuantityF,
  )
where

import Pacer.Configuration.Phase (ConfigPhase (ConfigPhaseArgs, ConfigPhaseFinal))
import Pacer.Configuration.Utils (DistanceDurationPaceArgs, PaceOptUnits)
import Pacer.Data.Distance (DistanceUnit, SomeDistance)
import Pacer.Data.Duration (Duration)
import Pacer.Exception
  ( CommandScaleE
      ( CommandScaleArgs0,
        CommandScaleArgs2,
        CommandScaleArgs3
      ),
  )
import Pacer.Prelude

-- | Quantity to scale.
data ScaleQuantity a
  = ScaleDistance (SomeDistance a)
  | ScaleDuration (Duration a)
  | ScalePace (PaceOptUnits a)

type family ScaleQuantityF p a where
  ScaleQuantityF ConfigPhaseArgs a = DistanceDurationPaceArgs a
  ScaleQuantityF ConfigPhaseFinal a = ScaleQuantity a

-- | Scale parameters.
type ScaleParams :: ConfigPhase -> Type -> Type
data ScaleParams p a = MkScaleParams
  { -- | Scale factor.
    factor :: Positive a,
    -- | Quantity to scale.
    quantity :: ScaleQuantityF p a,
    -- | Possible output unit.
    unit :: Maybe DistanceUnit
  }

makeFieldLabelsNoPrefix ''ScaleParams

type ScaleParamsArgs a = ScaleParams ConfigPhaseArgs a

type ScaleParamsFinal a = ScaleParams ConfigPhaseFinal a

deriving stock instance
  (Eq a, Eq (ScaleQuantityF p a)) => Eq (ScaleParams p a)

deriving stock instance
  (Show a, Show (ScaleQuantityF p a)) => Show (ScaleParams p a)

-- | Evolve scale params' phase.
evolvePhase ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  ScaleParamsArgs a ->
  m (ScaleParamsFinal a)
evolvePhase args =
  case ( ddpArgs ^. #mSomeDistance,
         ddpArgs ^. #mDuration,
         ddpArgs ^. #mPaceOptUnits
       ) of
    (Just _, Just _, Just _) -> throwM CommandScaleArgs3
    (Nothing, Nothing, Nothing) -> throwM CommandScaleArgs0
    (Just a, Nothing, Nothing) ->
      pure $ MkScaleParams factor (ScaleDistance a) unit
    (Nothing, Just b, Nothing) ->
      pure $ MkScaleParams factor (ScaleDuration b) unit
    (Nothing, Nothing, Just c) ->
      pure $ MkScaleParams factor (ScalePace c) unit
    _ -> throwM CommandScaleArgs2
  where
    ddpArgs = args ^. #quantity
    factor = args ^. #factor
    unit = args ^. #unit
