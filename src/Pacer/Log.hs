module Pacer.Log
  ( debug,
    info,
    warning,
  )
where

import Pacer.Prelude

debug :: (HasCallStack, MonadTerminal m) => Text -> m ()
debug = logHelper "[Debug] "

info :: (HasCallStack, MonadTerminal m) => Text -> m ()
info = logHelper "[Info] "

warning :: (HasCallStack, MonadTerminal m) => Text -> m ()
warning = logHelper "[Warning] "

logHelper :: (HasCallStack, MonadTerminal m) => Text -> Text -> m ()
logHelper lvl = putTextLn . (lvl <>)
