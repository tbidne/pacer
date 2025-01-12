{-# LANGUAGE UndecidableInstances #-}

-- | Derive parameters.
module Pacer.Command.Derive.Params
  ( -- * Types
    DeriveParams (..),
    DeriveParamsArgs,
    DeriveParamsFinal,
    DeriveQuantity (..),

    -- * Functions
    evolvePhase,

    -- * Type families
    DeriveQuantityF,
  )
where

import Pacer.Config.Phase (ConfigPhase (ConfigPhaseArgs, ConfigPhaseFinal))
import Pacer.Config.Utils
  ( DistanceDurationPaceArgs
      ( mDuration,
        mPaceOptUnits,
        mSomeDistance
      ),
    PaceOptUnits,
  )
import Pacer.Data.Distance (DistanceUnit, SomeDistance)
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Pace (SomePace)
import Pacer.Exception
  ( CommandDeriveE
      ( CommandDeriveArgs0,
        CommandDeriveArgs1,
        CommandDeriveArgs3,
        CommandDeriveNoPaceUnit
      ),
  )
import Pacer.Prelude

-- | Quantity to derive.
data DeriveQuantity a
  = -- | Derives distance from duration and pace.
    DeriveDistance (Seconds (Positive a)) (SomePace (Positive a))
  | -- | Derives duration from pace and distance.
    DeriveDuration (PaceOptUnits a) (SomeDistance (Positive a))
  | -- | Derives pace from duration and distance.
    DerivePace (Seconds (Positive a)) (SomeDistance (Positive a))

type family DeriveQuantityF p a where
  DeriveQuantityF ConfigPhaseArgs a = DistanceDurationPaceArgs a
  DeriveQuantityF ConfigPhaseFinal a = DeriveQuantity a

-- | Derive params.
type DeriveParams :: ConfigPhase -> Type -> Type
data DeriveParams p a = MkDeriveParams
  { -- | Quantity to derive.
    quantity :: DeriveQuantityF p a,
    -- | Possible output unit.
    unit :: Maybe DistanceUnit
  }

type DeriveParamsArgs a = DeriveParams ConfigPhaseArgs a

type DeriveParamsFinal a = DeriveParams ConfigPhaseFinal a

deriving stock instance
  (Eq a, Eq (DeriveQuantityF p a)) => Eq (DeriveParams p a)

deriving stock instance
  (Show a, Show (DeriveQuantityF p a)) => Show (DeriveParams p a)

-- | Evolve derive params' phase.
evolvePhase ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  DeriveParamsArgs a ->
  m (DeriveParamsFinal a)
evolvePhase args =
  case ( ddpArgs.mDuration,
         ddpArgs.mPaceOptUnits,
         ddpArgs.mSomeDistance
       ) of
    (Just _, Just _, Just _) -> throwM CommandDeriveArgs3
    (Nothing, Nothing, Nothing) -> throwM CommandDeriveArgs0
    -- Duration x Pace -> Distance
    (Just a, Just b, Nothing) -> case b of
      Left pace -> pure $ MkDeriveParams (DeriveDistance a pace) args.unit
      Right _ -> throwM CommandDeriveNoPaceUnit
    -- PaceOptUnits x Distance -> Duration
    (Nothing, Just b, Just c) ->
      pure $ MkDeriveParams (DeriveDuration b c) args.unit
    -- Duration x Distance -> Pace
    (Just a, Nothing, Just c) ->
      pure $ MkDeriveParams (DerivePace a c) args.unit
    _ -> throwM CommandDeriveArgs1
  where
    ddpArgs = args.quantity
