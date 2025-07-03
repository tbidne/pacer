module Pacer.Configuration.Phase
  ( -- * Phases
    ConfigPhase (..),

    -- * Type families
    ConfigPhaseF,
    ConfigOnlyF,
  )
where

import Pacer.Prelude

-- | Phases for data evolution.
data ConfigPhase
  = -- | CLI args phase
    ConfigPhaseArgs
  | -- | Final phase (i.e. ready for use)
    ConfigPhaseFinal

type ConfigPhaseF :: ConfigPhase -> Type -> Type
type family ConfigPhaseF p a where
  ConfigPhaseF ConfigPhaseArgs a = Maybe a
  ConfigPhaseF ConfigPhaseFinal a = a

type ConfigOnlyF :: ConfigPhase -> Type -> Type
type family ConfigOnlyF p a where
  ConfigOnlyF ConfigPhaseArgs a = Maybe a
  ConfigOnlyF ConfigPhaseFinal _ = Unit
