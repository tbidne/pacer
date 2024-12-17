{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}

{- ORMOLU_DISABLE -}

module Pacer.Prelude
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

    -- * File IO
    readFileUtf8,
    writeFileUtf8,

    -- * Singletons
    fromSingI,

#if !MIN_VERSION_base(4, 20, 0)

    -- * Anti-punning aliases
    List,
    Tuple2,
    Tuple3,
    Tuple4,

#endif

    -- * Exceptions
    displayExceptiont,
    displayInnerMatchKnown,
    knownExceptions,

    -- * Numeric

    -- ** Positive
    PDouble,
    mkPositiveFail,

    -- ** Floating
    ɛEq,

    -- * Foldable
    listToNESeq,

    -- * Dev / Debug
    todo,

    -- * Misc
    pattern SetToSeqNE,
    errorLeft,
    errorMapLeft,
    failLeft,
    failMapLeft,
    readFail,

    -- Prelude re-exports
    module X
  )
where

{- ORMOLU_ENABLE -}

import Control.Applicative as X
  ( Alternative (empty, (<|>)),
    Applicative (liftA2, pure, (*>), (<*), (<*>)),
    (<**>),
  )
import Control.Category as X (Category ((.)), (<<<), (>>>))
import Control.Exception as X
  ( Exception (displayException),
    SomeException,
    catch,
    throwIO,
    try,
  )
import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as Ex.Ann.Utils
import Control.Exception.Utils as X (TextException, throwText, trySync)
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
  ( Foldable (fold, foldMap, foldl', foldr, toList),
    any,
    for_,
    length,
    traverse_,
  )
import Data.Foldable1 as X (Foldable1 (foldMap1, toNonEmpty))
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
import Data.List as X
  ( List,
    elem,
    filter,
    replicate,
    sortOn,
    zip,
    zipWith,
    (++)
  )
#else
import Data.List as X (elem, filter, replicate, sortOn, zip, zipWith, (++))
#endif
import Data.Kind as X (Constraint, Type)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe as X (Maybe (Just, Nothing), catMaybes, fromMaybe, maybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X
  ( Ord (compare, (<), (<=), (>), (>=)),
    Ordering (EQ, GT, LT),
  )
import Data.Proxy as X (Proxy (Proxy))
import Data.Ratio as X (Ratio, Rational, denominator, numerator, (%))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.Sequence as X (Seq ((:<|), (:|>)))
import Data.Sequence.NonEmpty as X (NESeq ((:<||), (:||>)))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set.NonEmpty as X (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Singletons as X
  ( Sing,
    SingI (sing),
    SingKind (Demote, fromSing, toSing),
    SomeSing (SomeSing),
    withSingI,
  )
import Data.String as X (IsString, String)
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.Display as X (Display (displayBuilder), display)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Time.Calendar as X (Day)
import Data.Time.LocalTime as X (LocalTime, ZonedTime)
import Data.Traversable as X (Traversable (sequenceA, traverse), for)
import Data.Tuple as X (fst, snd)
#if MIN_VERSION_base(4, 20, 0)
import Data.Tuple.Experimental as X (Tuple2, Tuple3, Tuple4)
#endif
import Data.Text.Encoding (encodeUtf8)
import Data.Type.Equality as X (type (~))
import Data.Void as X (Void, absurd)
import Data.Word as X (Word32)
import FileSystem.IO as X (readBinaryFileIO, writeBinaryFileIO)
import FileSystem.OsPath as X (OsPath, osp, ospPathSep, (</>))
import FileSystem.UTF8 as X (decodeUtf8ThrowM, encodeUtf8)
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
import GHC.Show as X
  ( Show (show, showsPrec),
    showParen,
    showSpace,
    showString,
  )
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
    MetricSpace (diffR),
    Module,
    Ring,
    Semifield,
    Semimodule,
    Semiring,
    SemivectorSpace,
    VectorSpace,
    diffℝ,
  )
import Numeric.Class.Division as X (Division)
import Numeric.Convert.Integer as X
  ( FromInteger (fromZ),
    ToInteger (toZ),
    fromℤ,
    toℤ,
  )
import Numeric.Convert.Rational as X
  ( FromRational (fromQ),
    ToRational (toQ),
    fromℚ,
    toℚ,
  )
import Numeric.Convert.Real as X
  ( FromReal (fromR),
    ToReal (toR),
    fromℝ,
    toℝ,
  )
import Numeric.Data.Positive.Algebra as X (mkPositive, unsafePositive, (+!))
import Numeric.Data.Positive.Algebra.Internal as X
  ( Positive
      ( MkPositive,
        UnsafePositive
      ),
  )
import System.IO as X (IO, putStrLn)
import Text.Read as X (readMaybe)
import Text.Read qualified as TR

errorMapLeft :: (HasCallStack) => (a -> String) -> Either a b -> b
errorMapLeft f = errorLeft . first f

errorLeft :: (HasCallStack) => Either String a -> a
errorLeft (Left str) = error str
errorLeft (Right x) = x

failMapLeft :: (MonadFail m) => (a -> String) -> Either a b -> m b
failMapLeft f = failLeft . first f

failLeft :: (MonadFail m) => Either String a -> m a
failLeft (Left str) = fail str
failLeft (Right x) = pure x

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

-- | Positive Double.
type PDouble = Positive Double

-- | Equality with epsilon check for floating points.
ɛEq :: (MetricSpace a) => Double -> a -> a -> Bool
ɛEq e x y = diffℝ x y < e

mkPositiveFail ::
  forall m a.
  ( AMonoid a,
    MonadFail m,
    Ord a,
    Show a
  ) =>
  a ->
  m (Positive a)
mkPositiveFail x = case mkPositive x of
  Just y -> pure y
  Nothing -> fail $ "Received non-positive: " ++ show x

-- | Convenience function for retrieving the demoted value from a type
-- parameter.
fromSingI :: forall k (a :: k). (SingI a, SingKind k) => Demote k
fromSingI = fromSing @k (sing @a)

displayExceptiont :: (Exception e) => e -> Text
displayExceptiont = packText . displayException

readFail :: forall a m. (MonadFail m, Read a) => String -> String -> m a
readFail tyStr s = case TR.readMaybe s of
  Nothing -> fail $ "Could not read " ++ tyStr ++ ": " ++ s
  Just x -> pure x

listToNESeq :: (HasCallStack) => List a -> NESeq a
listToNESeq = NESeq.fromList . NE.fromList

readFileUtf8 :: OsPath -> IO Text
readFileUtf8 = decodeUtf8ThrowM <=< readBinaryFileIO

writeFileUtf8 :: OsPath -> Text -> IO ()
writeFileUtf8 p = writeBinaryFileIO p . encodeUtf8

-- | Bidirectional pattern synonym for NESet <-> NESeq
pattern SetToSeqNE :: (Ord a) => NESeq a -> NESet a
pattern SetToSeqNE x <- (NESeq.fromList . NESet.toList -> x)
  where
    SetToSeqNE x = NESet.fromList (toNonEmpty x)

{-# COMPLETE SetToSeqNE #-}

-- | This and knownExceptions will probably need to be moved to e.g.
-- Pacer.Exception, when we actually make our own.
displayInnerMatchKnown :: (Exception e) => e -> String
displayInnerMatchKnown = Ex.Ann.Utils.displayInnerMatch knownExceptions

knownExceptions :: List ExceptionProxy
knownExceptions =
  [ MkExceptionProxy @TextException Proxy
  ]
