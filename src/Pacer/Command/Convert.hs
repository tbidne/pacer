-- | Convert functionality.
module Pacer.Command.Convert
  ( handle,
  )
where

import Pacer.Command.Convert.Params
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit,
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Exception qualified as PEx
import Pacer.Prelude

-- | Handles convert command.
handle ::
  forall es a.
  ( Display a,
    Fromℤ a,
    HasCallStack,
    Ord a,
    Semifield a,
    Show a,
    Terminal :> es,
    Toℚ a
  ) =>
  ConvertParamsFinal a ->
  Eff es ()
handle params = case params ^. #quantity of
  Left dist ->
    case toSing unit of
      SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
        let dist' = DistU.convertDistance e dist
        putTextLn $ display dist'
  Right pace -> case unit of
    Meter -> throwM PEx.CommandConvertPaceMeters
    Kilometer -> putTextLn $ display $ DistU.convertDistance Kilometer pace
    Mile -> putTextLn $ display $ DistU.convertDistance Mile pace
  where
    unit = params ^. #unit
