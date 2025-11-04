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
    (<<&>>),

    -- * Functions
    identity,

    -- * Tuples
    uncurry3,

    -- * ByteString
    -- ** Lazy
    LazyByteString,
    toStrictBS,
    fromStrictBS,

    -- ** Builder
    ByteStringBuilder,
    builderToLazyBS,

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

    -- * Errors
    readFail,

    -- ** Misc
    displayExceptiont,

    -- * Numeric

    PInt,
    PWord8,
    PWord16,

    -- ** Positive
    mkPositiveFail,

    -- ** Floating
    PDouble,
    ɛEq,

    -- * Foldable
    listToNESeq,

    -- * File IO
    getCurrentDirectory,
    parseCanonicalAbsDir,
    parseCanonicalAbsFile,

    -- * Dev / Debug
    todo,
    traceFile,
    traceFileBS,
    traceFileA,
    traceFileLine,
    traceFileLineA,

    -- * OS
    Os (..),
    currentOs,
    currentOsStr,
    isPosix,
    posixWindowsStr,
    posixWindowsOsPath,

    -- * Misc
    pattern SetToSeqNE,

    -- Prelude re-exports
    module X
  )
where

{- ORMOLU_ENABLE -}

import Control.Applicative as X
  ( Alternative (empty, many, some, (<|>)),
    Applicative (liftA2, pure, (*>), (<*), (<*>)),
    asum,
    (<**>),
  )
import Control.Category as X (Category ((.)), (<<<), (>>>))
import Control.DeepSeq as X (NFData (rnf), deepseq)
import Control.Exception as X
  ( Exception (displayException, fromException, toException),
    SomeException,
  )
import Control.Exception.Utils as X
  ( StringException,
    throwString,
    throwText,
    trySync,
  )
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
import Data.ByteString.Builder qualified as BSBuilder
import Data.ByteString.Lazy qualified as BSL
import Data.Char as X (Char)
import Data.Char qualified as Ch
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X
  ( Foldable (fold, foldMap, foldl', foldr, toList),
    any,
    foldlM,
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
import Data.Kind as X (Constraint, Type)
import Data.List as X
  ( List,
    elem,
    filter,
    replicate,
    sortOn,
    zip,
    zipWith,
    (++),
  )
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
    max,
    min,
  )
import Data.Proxy as X (Proxy (Proxy))
import Data.Ratio as X (Ratio, Rational, denominator, numerator)
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
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Time.Calendar as X (Day)
import Data.Time.LocalTime as X (LocalTime, ZonedTime)
import Data.Traversable as X (Traversable (sequenceA, traverse), for)
import Data.Tuple as X (fst, snd, uncurry)
import Data.Tuple.Experimental as X (CUnit, Tuple2, Tuple3, Tuple4, Unit)
import Data.Type.Equality as X (type (~))
import Data.Void as X (Void, absurd)
import Data.Word as X (Word, Word16, Word32, Word8)
import Effectful as X
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    raise,
    runEff,
    runPureEff,
    type (:>),
  )
import Effectful.Concurrent as X (Concurrent, runConcurrent)
import Effectful.Dispatch.Dynamic as X
  ( interpose,
    interpret,
    interpret_,
    passthrough,
    reinterpret,
    reinterpret_,
    send,
  )
import Effectful.Dynamic.Utils as X (ShowEffect (showEffectCons))
import Effectful.FileSystem.FileReader.Dynamic as X
  ( FileReader,
    readBinaryFile,
    readFileUtf8ThrowM,
    runFileReader,
  )
import Effectful.FileSystem.FileWriter.Dynamic as X
  ( FileWriter,
    appendFileUtf8,
    runFileWriter,
    writeBinaryFile,
    writeFileUtf8,
  )
import Effectful.FileSystem.PathReader.Dynamic as X
  ( PathReader,
    XdgDirectory (XdgCache, XdgConfig),
    getXdgDirectory,
    runPathReader,
  )
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic as X
  ( PathWriter,
    createDirectoryIfMissing,
    runPathWriter,
  )
import Effectful.IORef.Static as X
  ( IORef,
    IORefE,
    modifyIORef',
    newIORef,
    readIORef,
    runIORef,
    writeIORef,
  )
import Effectful.Logger.Dynamic as X (Logger)
import Effectful.Logger.Namespace as X
  ( HasNamespace,
    LoggerNS,
    Namespace,
    addNamespace,
  )
import Effectful.Optparse.Static as X (Optparse, execParser, runOptparse)
import Effectful.Reader.Static as X (Reader, ask, asks, local, runReader)
import Effectful.State.Static.Local as X
  ( State,
    evalState,
    get,
    gets,
    modify,
    put,
    runState,
  )
import Effectful.Terminal.Dynamic as X
  ( Terminal,
    putStrLn,
    putText,
    putTextLn,
    runTerminal,
  )
import Effectful.Time.Dynamic as X (Time, runTime)
import FileSystem.IO as X (appendBinaryFileIO, throwPathIOError)
import FileSystem.OsPath as X (OsPath, osp, ospPathSep, (</>))
import FileSystem.OsPath qualified as OsPath
import FileSystem.OsString as X (OsString, osstr)
import FileSystem.Path as X
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    absdirPathSep,
    absfilePathSep,
    reldirPathSep,
    relfilePathSep,
    toOsPath,
    (<</>>),
  )
import FileSystem.Path qualified as Path
import FileSystem.UTF8 as X (decodeUtf8ThrowM, encodeUtf8)
import FileSystem.UTF8 qualified as UTF8
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
    Fromℤ,
    ToInteger (toZ),
    Toℤ,
    fromℤ,
    toℤ,
    ℤ,
  )
import Numeric.Convert.Rational as X
  ( FromRational (fromQ),
    Fromℚ,
    ToRational (toQ),
    Toℚ,
    fromℚ,
    toℚ,
    ℚ,
  )
import Numeric.Convert.Real as X
  ( FromReal (fromR),
    Fromℝ,
    ToReal (toR),
    Toℝ,
    fromℝ,
    toℝ,
    ℝ,
  )
import Numeric.Data.Fraction as X (Fraction)
import Numeric.Data.Positive as X (mkPositive, unsafePositive, (+!))
import Numeric.Data.Positive.Internal as X
  ( Positive
      ( MkPositive,
        UnsafePositive
      ),
  )
import Optics.Core as X
  ( A_Getter,
    A_Lens,
    A_Setter,
    AffineTraversal',
    An_AffineTraversal,
    An_Iso,
    Getter,
    Is,
    Iso,
    Iso',
    LabelOptic (labelOptic),
    Lens,
    Lens',
    NoIx,
    Optic,
    Optic',
    Prism,
    Prism',
    iso,
    lens,
    lensVL,
    over',
    preview,
    review,
    set',
    to,
    view,
    (%),
    (%?),
    (^.),
    (^?),
    _Just,
    _Nothing,
  )
import Optics.Core.Extras as X (is)
import Optics.TH as X
  ( makeFieldLabelsNoPrefix,
    makePrisms,
  )
import OsPath as X
  ( absdir,
    absfile,
    reldir,
    relfile,
  )
import Pacer.Data.Result as X
  ( Result (Err, Ok),
    ResultDefault,
    errorErr,
    failErr,
    throwErr,
  )
import System.FilePath (FilePath)
import System.IO as X (IO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read as X (readMaybe)
import Text.Read qualified as TR

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (<<$>>)

infixl 1 <<&>>

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

toStrictBS :: LazyByteString -> ByteString
toStrictBS = BSL.toStrict

fromStrictBS :: ByteString -> LazyByteString
fromStrictBS = BSL.fromStrict

type ByteStringBuilder = BSBuilder.Builder

builderToLazyBS :: ByteStringBuilder -> LazyByteString
builderToLazyBS = BSBuilder.toLazyByteString

type TextBuilder = TLB.Builder

builderToLazyText :: TextBuilder -> TL.Text
builderToLazyText = TLB.toLazyText

-- | Placeholder for unwritten code.
todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING todo "todo remains in code" #-}

-- | Traces to a file.
traceFile :: FilePath -> Text -> a -> a
traceFile path = traceFileBS path . UTF8.encodeUtf8

-- | Traces to a file.
traceFileBS :: FilePath -> ByteString -> a -> a
traceFileBS path bs x = writeFn `seq` x
  where
    io = appendBinaryFileIO (OsPath.unsafeEncode path) bs
    writeFn = unsafePerformIO io

-- | Traces to a file in an Applicative.
traceFileA :: (Applicative f) => FilePath -> Text -> f Unit
traceFileA f t = traceFile f t (pure ())

-- | Traces to a file with newline.
traceFileLine :: FilePath -> Text -> a -> a
traceFileLine path txt = traceFile path (txt <> "\n")

-- | Traces to a file with newline in an Applicative.
traceFileLineA :: (Applicative f) => FilePath -> Text -> f Unit
traceFileLineA p t = traceFileLine p t (pure ())

-- | Positive Double.
type PDouble = Positive Double

-- | Equality with epsilon check for floating points.
ɛEq :: (MetricSpace a) => Double -> a -> a -> Bool
ɛEq e x y = diffℝ x y < e

type PInt = Positive Int

type PWord8 = Positive Word8

type PWord16 = Positive Word16

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
  Right y -> pure y
  Left err -> fail err

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

-- | Current directory.
getCurrentDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es (Path Abs Dir)
getCurrentDirectory = PR.getCurrentDirectory >>= parseCanonicalAbsDir

-- | Xdg cache dir.
getXdgCachePath ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es (Path Abs Dir)
getXdgCachePath =
  getXdgDirectory XdgCache [osp|pacer|] >>= parseCanonicalAbsDir

-- | Xdg config dir.
getXdgConfigPath ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es (Path Abs Dir)
getXdgConfigPath =
  getXdgDirectory XdgConfig [osp|pacer|] >>= parseCanonicalAbsDir

parseCanonicalAbsDir ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es (Path Abs Dir)
parseCanonicalAbsDir = PR.canonicalizePath >=> Path.parseAbsDir

parseCanonicalAbsFile ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es (Path Abs File)
parseCanonicalAbsFile = PR.canonicalizePath >=> Path.parseAbsFile

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

posixWindowsOsPath :: OsPath
posixWindowsOsPath =
  if isPosix
    then [osp|posix|]
    else [osp|windows|]

posixWindowsStr :: String
posixWindowsStr =
  if isPosix
    then "posix"
    else "windows"

identity :: a -> a
identity x = x

uncurry3 :: (a -> b -> c -> d) -> Tuple3 a b c -> d
uncurry3 f (a, b, c) = f a b c
