module Pacer.Utils
  ( -- * Seq
    seqGroupBy,
    neSeqGroupBy,

    -- * Optics
    SomeSetter (..),
    setMany',

    -- * Misc
    PaceMetersErrMsg,
    isNonPosError,
  )
where

import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Pacer.Prelude

type PaceMetersErrMsg = "Meters are disallowed in Pace; use km or mi."

seqGroupBy :: forall a. (a -> a -> Bool) -> Seq a -> Seq (NESeq a)
seqGroupBy @a p = go
  where
    go :: Seq a -> Seq (NESeq a)
    go Seq.Empty = Seq.Empty
    go (x :<| xs) = (x :<|| ys) :<| go zs
      where
        (ys, zs) = Seq.spanl (p x) xs

neSeqGroupBy :: forall a. (a -> a -> Bool) -> NESeq a -> NESeq (NESeq a)
neSeqGroupBy @a p = go
  where
    go :: NESeq a -> NESeq (NESeq a)
    go (x :<|| xs) = fstGrp :<|| rest
      where
        (ys, zs) = Seq.spanl (p x) xs
        fstGrp = x :<|| ys
        rest = case zs of
          Seq.Empty -> Seq.Empty
          (w :<| ws) -> NESeq.toSeq (go (w :<|| ws))

-- | Existential wrapper for an optic setter with new object.
data SomeSetter p where
  MkSomeSetter ::
    forall p x k ix.
    (Is k A_Setter) =>
    Optic k ix p p x x ->
    x ->
    SomeSetter p

-- | Targets some object using a list of optics and the new values.
-- Equivalent to calling set' multiple times.
setMany' ::
  forall f p.
  (Foldable f) =>
  f (SomeSetter p) ->
  p ->
  p
setMany' ls s = foldl' (\s' (MkSomeSetter l x) -> set' l x s') s ls

-- | Detects if an error concerns non-positivity. Crude, but it works.
-- Note that this message comes from smart-math.
isNonPosError :: Text -> Bool
isNonPosError = T.isInfixOf "Received value <= zero"
