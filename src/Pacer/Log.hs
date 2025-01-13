module Pacer.Log
  ( debug,
    info,
    warning,
  )
where

import Pacer.Prelude

debug :: (HasCallStack, Terminal :> es) => Text -> Eff es ()
debug = logHelper "[Debug] "

info :: (HasCallStack, Terminal :> es) => Text -> Eff es ()
info = logHelper "[Info] "

warning :: (HasCallStack, Terminal :> es) => Text -> Eff es ()
warning = logHelper "[Warning] "

logHelper :: (HasCallStack, Terminal :> es) => Text -> Text -> Eff es ()
logHelper lvl = putTextLn . (lvl <>)
