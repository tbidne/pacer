{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Pacer.Web.Utils
  ( -- * TH
    bindIOToTH,
    liftIOToTH,
  )
where

import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Pacer.Prelude

-- | Binds an IO action to TH.
bindIOToTH :: (HasCallStack, Lift b) => ((HasCallStack) => a -> IO b) -> a -> Code Q b
bindIOToTH f x = TH.bindCode (TH.runIO (f x)) liftTyped

-- | Lifts an IO action to TH.
liftIOToTH :: (HasCallStack, Lift a) => IO a -> Code Q a
liftIOToTH m = bindIOToTH (const m) ()
