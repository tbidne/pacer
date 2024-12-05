{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}

{- ORMOLU_DISABLE -}

module Running.Prelude
  ( -- * ByteString
    LazyByteString,
    toStrictByteString,
    fromStrictByteString,

    -- * Text
    showt,
    packText,
    unpackText,

    -- ** Lazy
    LazyText,
    toStrictText,
    fromStrictText,

    -- ** Builder
    TextBuilder,
    builderToLazyText,

#if !MIN_VERSION_base(4, 20, 0)

    -- * Anti-punning aliases
    List,
    Tuple2,
    Tuple3,
    Tuple4,

#endif

    -- * Numeric
    Ratioℕ,

    -- ** Positive
    PDouble,
    pone,

    -- ** Floating
    ɛEq,

    -- * Dev / Debug
    todo,

    -- * Misc
    errorLeft,
    errorMapLeft,

    -- Prelude re-exports
    module X
  )
where

{- ORMOLU_ENABLE -}

import Control.Applicative as X
  ( Alternative (empty, (<|>)),
    Applicative (liftA2, pure, (*>), (<*>)),
    (<**>),
  )
import Control.Category as X (Category ((.)), (<<<), (>>>))
import Control.Monad as X
  ( Monad ((>>=)),
    forever,
    join,
    unless,
    void,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Data.Bifunctor as X (Bifunctor (bimap, first, second))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Char as X (Char)
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X
  ( Foldable (fold, foldl', foldr, toList),
    any,
    for_,
    length,
    traverse_,
  )
import Data.Function as X (const, flip, id, ($), (&))
import Data.Functor as X
  ( Functor (fmap),
    ($>),
    (<$),
    (<$>),
    (<&>),
  )
import Data.Int as X (Int)
#if MIN_VERSION_base(4, 20, 0)
import Data.List as X (List, elem, filter, replicate, zip, (++))
#else
import Data.List as X (elem, filter, replicate, zip, (++))
#endif
import Data.Kind as X (Constraint, Type)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (Just, Nothing), catMaybes, fromMaybe, maybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X
  ( Ord (compare, (<), (<=), (>), (>=)),
    Ordering (EQ, GT, LT),
  )
import Data.Proxy as X (Proxy (Proxy))
import Data.Ratio as X (Ratio, Rational, denominator, numerator, (%))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.String as X (IsString, String)
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.Display as X (Display (displayBuilder), display)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Traversable as X (Traversable (sequenceA, traverse))
#if MIN_VERSION_base(4, 20, 0)
import Data.Tuple.Experimental as X (Tuple2, Tuple3, Tuple4)
#endif
import Data.Type.Equality as X (type (~))
import Data.Void as X (Void, absurd)
import Data.Word as X (Word32)
import GHC.Enum as X (Bounded (maxBound, minBound), Enum)
import GHC.Err as X (error, undefined)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#)
import GHC.Float as X (Double, Float)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num (abs, fromInteger, (*), (+), (-)))
import GHC.Read as X (Read (readPrec))
import GHC.Real as X
  ( Fractional (fromRational, (/)),
    Integral (quotRem),
    Ratio ((:%)),
    Real,
    RealFrac (floor, properFraction, round),
    fromIntegral,
    realToFrac,
    toInteger,
    toRational,
    truncate,
  )
import GHC.Records as X (HasField (getField))
import GHC.Show as X (Show (show, showsPrec), showParen, showSpace, showString)
import GHC.Stack as X (HasCallStack)
import Numeric.Algebra as X
  ( AGroup ((.-.)),
    AMonoid (zero),
    ASemigroup ((.+.)),
    Field,
    MEuclidean (mdivMod),
    MGroup ((.%.)),
    MMonoid (one),
    MSemiSpace ((.*)),
    MSemigroup ((.*.)),
    MSpace ((.%)),
    Module,
    Ring,
    Semifield,
    Semimodule,
    Semiring,
    SemivectorSpace,
    VectorSpace,
  )
import Numeric.Algebra.MetricSpace as X (MetricSpace (diff))
import Numeric.Class.Division as X (Division)
import Numeric.Data.Positive as X (mkPositive, unsafePositive, (+!))
import Numeric.Data.Positive.Internal as X (Positive (MkPositive, UnsafePositive))
import Numeric.Data.Positive.Internal qualified as PosI
import Numeric.Literal.Integer as X (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Literal.Rational as X (FromRational (fromQ), ToRational (toQ))
import System.IO as X (IO, putStrLn)

errorMapLeft :: (HasCallStack) => (a -> String) -> Either a c -> c
errorMapLeft f = errorLeft . first f

errorLeft :: (HasCallStack) => Either String a -> a
errorLeft (Left str) = error str
errorLeft (Right x) = x

showt :: (Show a) => a -> Text
showt = packText . show

packText :: String -> Text
packText = T.pack

unpackText :: Text -> String
unpackText = T.unpack

type LazyText = TL.Text

toStrictText :: LazyText -> Text
toStrictText = TL.toStrict

fromStrictText :: Text -> LazyText
fromStrictText = TL.fromStrict

type LazyByteString = BSL.ByteString

toStrictByteString :: LazyByteString -> ByteString
toStrictByteString = BSL.toStrict

fromStrictByteString :: ByteString -> LazyByteString
fromStrictByteString = BSL.fromStrict

type TextBuilder = TLB.Builder

builderToLazyText :: TextBuilder -> TL.Text
builderToLazyText = TLB.toLazyText

#if !MIN_VERSION_base(4, 20, 0)

-- | Alias for [].
type List = []

-- | Alias for (,).
type Tuple2 = (,)

-- | Alias for (,,).
type Tuple3 = (,,)

-- | Alias for (,,,).
type Tuple4 = (,,,)

#endif

todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING todo "todo remains in code" #-}

type Ratioℕ = Ratio Natural

type PDouble = Positive Double

pone :: PDouble
pone = PosI.UnsafePositive 1

ɛEq :: (MetricSpace a) => Double -> a -> a -> Bool
ɛEq e x y = diff x y < e
