{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{- ORMOLU_DISABLE -}

module Pacer.Prelude
  ( -- * Functor
    (<<$>>),

    -- * ByteString
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

#if !MIN_VERSION_base(4, 21, 0)

    -- * Bitraversable
    firstA,
    secondA,

#endif

    -- * XDG
    getXdgCachePath,
    getXdgConfigPath,

    -- * Singletons
    fromSingI,

#if !MIN_VERSION_base(4, 20, 0)

    -- * Anti-punning aliases
    List,
    Tuple2,
    Tuple3,
    Tuple4,

#endif

    -- * Errors
    -- ** Either
    failLeft,
    failMapLeft,
    readFail,
    throwLeft,
    throwMapLeft,
    errorLeft,
    errorMapLeft,

    -- ** Misc
    displayExceptiont,

    -- * Numeric

    -- ** Positive
    mkPositiveFail,

    -- ** Fraction
    ℚNonNeg,
    ℚPos,

    -- ** Floating
    PDouble,
    ɛEq,

    -- * Foldable
    listToNESeq,

    -- * Dev / Debug
    todo,
    traceFile,
    traceFileA,
    traceFileLine,
    traceFileLineA,

    -- * OS
    Os (..),
    currentOs,
    currentOsStr,
    isPosix,
    posixWindowsStr,

    -- * Misc
    pattern SetToSeqNE,

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
  ( Exception (displayException, fromException, toException),
    SomeException,
  )
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
import Control.Monad.Catch as X
  ( MonadCatch (catch),
    MonadMask,
    MonadThrow (throwM),
    bracket,
    try,
  )
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Data.Bifunctor as X (Bifunctor (bimap, first, second))
#if MIN_VERSION_base(4, 21, 0)
import Data.Bitraversable as X (Bitraversable (bitraverse), firstA, secondA)
#else
import Data.Bitraversable as X (Bitraversable (bitraverse))
#endif
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Char as X (Char)
import Data.Char qualified as Ch
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
import Numeric.Data.Fraction.Algebra as X (Fraction)
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
import Data.Maybe as X
  ( Maybe (Just, Nothing),
    catMaybes,
    fromMaybe,
    isJust,
    maybe,
  )
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
import Effects.FileSystem.FileReader as X
  ( MonadFileReader (readBinaryFile),
    readFileUtf8ThrowM,
  )
import Effects.FileSystem.FileWriter as X
  ( MonadFileWriter (writeBinaryFile),
    appendFileUtf8,
    writeFileUtf8,
  )
import Effects.FileSystem.PathReader as X
  ( MonadPathReader
      ( getXdgDirectory
      ),
    XdgDirectory (XdgCache, XdgConfig),
  )
import Effects.FileSystem.PathWriter as X
  ( MonadPathWriter
      ( createDirectoryIfMissing
      ),
  )
import Effects.IORef as X
  ( IORef,
    MonadIORef
      ( modifyIORef',
        newIORef,
        readIORef,
        writeIORef
      ),
  )
import Effects.Optparse as X (MonadOptparse (execParser))
import Effects.Process.Typed as X (MonadTypedProcess)
import Effects.System.Terminal as X (MonadTerminal (putStrLn), putTextLn)
import FileSystem.IO as X (throwPathIOError)
import FileSystem.OsPath as X (OsPath, osp, ospPathSep, (</>))
import FileSystem.OsPath qualified as OsPath
import FileSystem.Path as X
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    absdirPathSep,
    absfilePathSep,
    pathToOsPath,
    reldirPathSep,
    relfilePathSep,
    (<</>>),
  )
import FileSystem.Path qualified as Path
import FileSystem.UTF8 as X (decodeUtf8ThrowM, encodeUtf8)
import GHC.Enum as X (Bounded (maxBound, minBound), Enum)
import GHC.Err as X (error, undefined)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#, seq)
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
import OsPath as X
  ( absdir,
    absfile,
    reldir,
    relfile,
  )
import System.FilePath (FilePath)
import System.IO as X (IO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read as X (readMaybe)
import Text.Read qualified as TR

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

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

throwMapLeft ::
  ( HasCallStack,
    Exception e,
    MonadThrow m
  ) =>
  (a -> e) ->
  Either a b ->
  m b
throwMapLeft f = throwLeft . first f

throwLeft ::
  ( HasCallStack,
    Exception e,
    MonadThrow m
  ) =>
  Either e a ->
  m a
throwLeft (Left ex) = throwM ex
throwLeft (Right x) = pure x

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

-- | Placeholder for unwritten code.
todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING todo "todo remains in code" #-}

-- | Traces to a file.
traceFile :: FilePath -> Text -> a -> a
traceFile path txt x = writeFn `seq` x
  where
    io = appendFileUtf8 (OsPath.unsafeEncode path) txt
    writeFn = unsafePerformIO io

-- | Traces to a file in an Applicative.
traceFileA :: (Applicative f) => FilePath -> Text -> f ()
traceFileA f t = traceFile f t (pure ())

-- | Traces to a file with newline.
traceFileLine :: FilePath -> Text -> a -> a
traceFileLine path txt = traceFile path (txt <> "\n")

-- | Traces to a file with newline in an Applicative.
traceFileLineA :: (Applicative f) => FilePath -> Text -> f ()
traceFileLineA p t = traceFileLine p t (pure ())

-- | Positive Double.
type PDouble = Positive Double

type ℚNonNeg = Fraction Natural

type ℚPos = Positive (Fraction Natural)

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

-- | Bidirectional pattern synonym for NESet <-> NESeq
pattern SetToSeqNE :: (Ord a) => NESeq a -> NESet a
pattern SetToSeqNE x <- (NESeq.fromList . NESet.toList -> x)
  where
    SetToSeqNE x = NESet.fromList (toNonEmpty x)

{-# COMPLETE SetToSeqNE #-}

#if !MIN_VERSION_base(4, 21, 0)
firstA :: (Bitraversable t, Applicative f) => (a -> f c) -> t a d -> f (t c d)
firstA f = bitraverse f pure

secondA :: (Bitraversable t, Applicative f) => (b -> f d) -> t c b -> f (t c d)
secondA = bitraverse pure
#endif
-- | Xdg cache dir.
getXdgCachePath ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  m (Path Abs Dir)
getXdgCachePath =
  getXdgDirectory XdgCache [osp|pacer|] >>= Path.parseAbsDir

-- | Xdg config dir.
getXdgConfigPath ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  m (Path Abs Dir)
getXdgConfigPath =
  getXdgDirectory XdgConfig [osp|pacer|] >>= Path.parseAbsDir

data Os
  = Linux
  | Osx
  | Windows
  deriving stock (Eq, Show)

currentOs :: Os
#if WINDOWS
currentOs = Windows
#elif OSX
currentOs = Osx
#else
currentOs = Linux
#endif

currentOsStr :: String
currentOsStr = Ch.toLower <$> show currentOs

isPosix :: Bool
isPosix = case currentOs of
  Windows -> False
  _ -> True

posixWindowsStr :: String
posixWindowsStr =
  if isPosix
    then "posix"
    else "windows"
