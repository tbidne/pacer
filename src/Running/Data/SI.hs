module Running.Data.SI
  ( SI (..),
    baseFactor,

    -- * Singletons
    SSI (..),
    withSingSI,
    ssiToSI,
    SingSI (..),
  )
where

import Data.Kind (Constraint)
import Data.Text.Display (Display (displayBuilder))

-- | SI Units.
data SI
  = Micro -- 10^-6
  | Milli -- 1,000
  | Centi -- 100
  | Deci -- 10
  | Base -- 1
  | Deca -- 10
  | Hecto -- 100
  | Kilo -- 1,000
  | Mega -- 10^6
  deriving stock (Eq, Show)

instance Display SI where
  displayBuilder Micro = "μ"
  displayBuilder Milli = "m"
  displayBuilder Centi = "c"
  displayBuilder Deci = "d"
  displayBuilder Base = ""
  displayBuilder Deca = "da"
  displayBuilder Hecto = "h"
  displayBuilder Kilo = "k"
  displayBuilder Mega = "M"

-- | Ratio of unit/meter.
baseFactor :: (Fractional a) => SI -> a
baseFactor Micro = 0.000_001
baseFactor Milli = 0.001
baseFactor Centi = 0.01
baseFactor Deci = 0.1
baseFactor Base = 1
baseFactor Deca = 10
baseFactor Hecto = 100
baseFactor Kilo = 1_000
baseFactor Mega = 1_000_000

data SSI (s :: SI) where
  SMilli :: SSI Milli
  SCenti :: SSI Centi
  SDeci :: SSI Deci
  SBase :: SSI Base
  SDeca :: SSI Deca
  SHecto :: SSI Hecto
  SKilo :: SSI Kilo
  SMega :: SSI Mega

ssiToSI :: SSI s -> SI
ssiToSI SMilli = Milli
ssiToSI SCenti = Centi
ssiToSI SDeci = Deci
ssiToSI SBase = Base
ssiToSI SDeca = Deca
ssiToSI SHecto = Hecto
ssiToSI SKilo = Kilo
ssiToSI SMega = Mega

type SingSI :: SI -> Constraint
class SingSI (s :: SI) where
  singSI :: SSI s

instance SingSI Milli where singSI = SMilli

instance SingSI Centi where singSI = SCenti

instance SingSI Deci where singSI = SDeci

instance SingSI Base where singSI = SBase

instance SingSI Deca where singSI = SDeca

instance SingSI Hecto where singSI = SHecto

instance SingSI Kilo where singSI = SKilo

instance SingSI Mega where singSI = SMega

withSingSI :: SSI s -> ((SingSI s) => r) -> r
withSingSI s x = case s of
  SMilli -> x
  SCenti -> x
  SDeci -> x
  SBase -> x
  SDeca -> x
  SHecto -> x
  SKilo -> x
  SMega -> x
