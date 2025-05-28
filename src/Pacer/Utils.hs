{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pacer.Utils
  ( -- * JSON

    -- ** Encoding
    encodeMaybe,
    encodeMonoid,
    encodeMaybes,
    encodePretty,

    -- ** Decoding
    (.:?:),
    failUnknownFields,
    decodeJson,
    AesonE (..),
    readDecodeJson,

    -- * Show
    showtOsPath,
    showPath,
    showtPath,

    -- ** List-like
    showMapListInline,
    showMapListNewlines,
    showListLike,
    ShowListStyle (..),
    ShowListTarget (..),
    ShowListInlineConfig (..),
    ShowListBracketStyle (..),

    -- * File discovery
    DirNotExistsHandler (..),
    SearchFiles (..),
    FileAliases (..),
    searchFiles,
    DirExistsCheck (..),
    searchFileAliases,
    searchFilesToList,

    -- ** Errors
    DirNotFoundE (..),

    -- * Seq
    seqGroupBy,
    neSeqGroupBy,

    -- * Misc
    PaceMetersErrMsg,
  )
where

import Data.Aeson (Key, (<?>))
import Data.Aeson qualified as Asn
import Data.Aeson.Encode.Pretty
  ( Config (confIndent, confTrailingNewline),
    Indent (Spaces),
  )
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (JSONPathElement (Key), Pair)
import Data.Aeson.Types qualified as AsnT
import Data.Foldable qualified as F
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.OsPath (decodeLenient, encodeLenient)
import FileSystem.OsPath qualified as OsPath
import FileSystem.Path qualified as Path
import GHC.IsList (IsList (Item))
import GHC.IsList qualified as IL
import Pacer.Class.FromAlt (FromAlt, isNonEmpty)
import Pacer.Class.Parser qualified as P
import Pacer.Prelude

-- | Encodes 'Maybe's to a possibly empty list.
encodeMaybes :: (ToJSON v) => List (Tuple2 Key (Maybe v)) -> List Pair
encodeMaybes = (>>= encodeMaybe)

-- | Encodes a 'Monoid' to a possibly empty list.
encodeMonoid :: (Eq v, Monoid v, ToJSON v) => Tuple2 Key v -> List Pair
encodeMonoid = encodeMaybe . second toMaybe
  where
    toMaybe x
      | x == mempty = Nothing
      | otherwise = Just x

-- | Encodes a 'Maybe' to a possibly empty list.
encodeMaybe :: (ToJSON v) => Tuple2 Key (Maybe v) -> List Pair
encodeMaybe (_, Nothing) = []
encodeMaybe (k, Just v) = [k .= v]

type PaceMetersErrMsg = "Meters are disallowed in Pace; use km or mi."

showtOsPath :: OsPath -> Text
showtOsPath = packText . OsPath.decodeLenient

showPath :: Path b t -> String
showPath = OsPath.decodeLenient . toOsPath

showtPath :: Path b t -> Text
showtPath = showtOsPath . toOsPath

-- | Bracket style.
data ShowListBracketStyle
  = ShowListBracketsSquare
  | ShowListBracketsCurly
  | ShowListBracketsNone

instance Semigroup ShowListBracketStyle where
  ShowListBracketsNone <> _ = ShowListBracketsNone
  _ <> ShowListBracketsNone = ShowListBracketsNone
  ShowListBracketsCurly <> _ = ShowListBracketsCurly
  _ <> ShowListBracketsCurly = ShowListBracketsCurly
  _ <> _ = ShowListBracketsSquare

instance Monoid ShowListBracketStyle where
  mempty = ShowListBracketsSquare

-- | Inline config.
data ShowListInlineConfig = MkShowListInlineConfig
  { -- | Whether comma separator should have a space.
    spaces :: Bool,
    brackets :: ShowListBracketStyle
  }

instance Semigroup ShowListInlineConfig where
  l <> r =
    MkShowListInlineConfig
      { spaces = l.spaces && r.spaces,
        brackets = l.brackets <> r.brackets
      }

instance Monoid ShowListInlineConfig where
  mempty = MkShowListInlineConfig True mempty

-- | Wrapper for showing a list-like value.
type ShowListTarget :: (Type -> Type) -> Type -> Type
data ShowListTarget f r where
  -- | The structure is the processed type.
  ShowListActual :: f r -> ShowListTarget f r
  -- | The structure needs its contents mapped to a suitable type.
  ShowListMap :: (a -> r) -> f a -> ShowListTarget f r

-- | Base show list type.
type ShowListStyle :: (Type -> Type) -> Type -> Type
data ShowListStyle f r
  = -- | Inline style.
    ShowListInline (ShowListTarget f r) ShowListInlineConfig
  | -- | Newline style.
    ShowListNewlines (ShowListTarget f r)

showMapListInline :: (Foldable f, IsString c, Semigroup c) => (a -> c) -> f a -> c
showMapListInline f xs =
  showListLike
    . ShowListInline (ShowListMap f xs)
    $ mempty

showMapListNewlines :: (Foldable f, IsString c, Semigroup c) => (a -> c) -> f a -> c
showMapListNewlines f =
  showListLike
    . ShowListNewlines
    . ShowListMap f

-- | Shows a list-like value.
showListLike :: (Foldable f, IsString r, Semigroup r) => ShowListStyle f r -> r
showListLike = \case
  ShowListNewlines sl -> go ys
    where
      go :: forall x. (IsString x, Semigroup x) => List x -> x
      go [] = ""
      go (z : zs) = "\n  - " <> z <> go zs

      ys = case sl of
        ShowListActual xs -> toList xs
        ShowListMap f xs -> f <$> toList xs
  ShowListInline sl cfg -> case ys of
    [] -> l <> r
    _ -> l <> go ys
    where
      go [] = r
      go [z] = z <> r
      go (z : zs) = z <> sep <> go zs

      ys = case sl of
        ShowListActual xs -> toList xs
        ShowListMap f xs -> f <$> toList xs

      (l, r) = case cfg.brackets of
        ShowListBracketsSquare -> ("[", "]")
        ShowListBracketsCurly -> ("{", "}")
        ShowListBracketsNone -> ("", "")

      sep
        | cfg.spaces = ", "
        | otherwise = ","

seqGroupBy :: forall a. (a -> a -> Bool) -> Seq a -> Seq (NESeq a)
seqGroupBy p = go
  where
    go :: Seq a -> Seq (NESeq a)
    go Seq.Empty = Seq.Empty
    go (x :<| xs) = (x :<|| ys) :<| go zs
      where
        (ys, zs) = Seq.spanl (p x) xs

neSeqGroupBy :: forall a. (a -> a -> Bool) -> NESeq a -> NESeq (NESeq a)
neSeqGroupBy p = go
  where
    go :: NESeq a -> NESeq (NESeq a)
    go (x :<|| xs) = fstGrp :<|| rest
      where
        (ys, zs) = Seq.spanl (p x) xs
        fstGrp = x :<|| ys
        rest = case zs of
          Seq.Empty -> Seq.Empty
          (w :<| ws) -> NESeq.toSeq (go (w :<|| ws))

-- | We use this rather than aeson's AesonException for two reasons:
--
-- 1. AesonException does not have an NFData instance, which we require for
--    the benchmarks.
--
-- 2. Optional path improves the error message.
data AesonE = MkAesonE (Maybe OsPath) String
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

instance Exception AesonE where
  displayException (MkAesonE mPath s) =
    case mPath of
      Just p ->
        mconcat
          [ "Error decoding json path '",
            decodeLenient p,
            "': ",
            s
          ]
      Nothing -> "Error decoding json: " ++ s

-- | Decodes a json(c) file.
readDecodeJson ::
  forall a es.
  ( FileReader :> es,
    FromJSON a,
    HasCallStack
  ) =>
  Path Abs File ->
  Eff es a
readDecodeJson path = do
  contents <- readBinaryFile osPath
  throwErr
    $ first toAesonPathE
    $ decodeJson @a contents
  where
    osPath = toOsPath path
    toAesonPathE (MkAesonE _ s) = MkAesonE (Just osPath) s

-- | Fails if there are any unknown fields in the object.
failUnknownFields ::
  -- | Key label to improve error message.
  Key ->
  -- | Known keys.
  List Key ->
  -- | Actual keys.
  KeyMap Asn.Value ->
  AsnT.Parser ()
failUnknownFields name knownKeys kmap = do
  case HSet.toList unknownKeys of
    [] -> pure ()
    unknownKeyList@(_ : _) ->
      fail
        ( mconcat
            [ "Encountered unknown keys: ",
              showKeys unknownKeyList
            ]
        )
        <?> Key name
  where
    actualKeyMap = KM.toHashMap kmap
    actualKeySet = HMap.keysSet actualKeyMap

    knownKeySet = HSet.fromList knownKeys
    unknownKeys = HSet.difference actualKeySet knownKeySet

    showKeys :: List Key -> String
    showKeys = show . L.sort . fmap Key.toString

-- | Decodes json(c).
decodeJson :: (FromJSON a) => ByteString -> Result AesonE a
decodeJson =
  first (MkAesonE Nothing)
    <<< review #eitherIso
    . Asn.eitherDecodeStrict
    <=< P.stripComments

-- | Decodes a json list into an 'IsList'. Like '(.:?)', omitted keys are fine
-- and decode to an empty list.
(.:?:) ::
  forall l.
  (FromJSON (Item l), IsList l) =>
  Asn.Object ->
  Key ->
  AsnT.Parser l
(.:?:) o k = do
  o .:? k <&> \case
    Nothing -> IL.fromList []
    Just xs -> IL.fromList xs

infixl 9 .:?:

-- | Exception for a directory not existing.
newtype DirNotFoundE = MkDirNotFoundE OsPath
  deriving stock (Show)

instance Exception DirNotFoundE where
  displayException (MkDirNotFoundE p) =
    mconcat
      [ "Directory not found: ",
        decodeLenient p
      ]

-- | How to handle a directory not existing.
data DirNotExistsHandler
  = -- | Directory not required; inexistence OK.
    DirNotExistsOk
  | -- | Directory required; inexistence should error.
    DirNotExistsFail

-- | Attempts to discover files based on 'SearchFiles' i.e.
-- "expected" filenames. For each search file @s@, produces a search result
-- @r@ per 'searchFile', and combines the result via '(<|>)'.
--
-- In other words, depending on choice of type variable @f@, can return
-- multiple results ('List') or a single result ('Maybe').
searchFiles ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    PathReader :> es
  ) =>
  -- | File names to search.
  SearchFiles ->
  -- | How to handle the directory not existing.
  DirNotExistsHandler ->
  -- | Data dir to search.
  Path Abs Dir ->
  Eff es (f (Path Abs File))
searchFiles fileNames dner dataDir = do
  $(Logger.logDebug) msg

  dExists <- PR.doesDirectoryExist dataDirOsPath

  if dExists
    then go . NE.toList $ fileNames.unSearchFiles
    else handleEmptyDir dner dataDir
  where
    dataDirOsPath = toOsPath dataDir

    go :: (HasCallStack) => List FileAliases -> Eff es (f (Path Abs File))
    go [] = pure empty
    go (f : fs) = do
      result <- searchFileAliases DirExistsCheckOff dataDir f
      (result <|>) <$> go fs

    msg =
      mconcat
        [ "Searching for path(s) ",
          showMapListInline showtPath fileNamesList,
          " in: ",
          showtPath dataDir
        ]

    fileNamesList = searchFilesToList fileNames

-- | Handles empty directory according to the parameter i.e. either throws
-- an exception or logs and returns empty.
handleEmptyDir ::
  ( Alternative f,
    Logger :> es
  ) =>
  DirNotExistsHandler ->
  Path b Dir ->
  Eff es (f a)
handleEmptyDir DirNotExistsFail dataDir = throwM $ MkDirNotFoundE (toOsPath dataDir)
handleEmptyDir DirNotExistsOk dataDir = do
  $(Logger.logDebug) $ mkDirNotExistMsg dataDir
  pure empty

mkDirNotExistMsg :: Path b Dir -> Text
mkDirNotExistMsg d =
  mconcat
    [ "Directory '",
      packText $ showPath d,
      "' does not exist."
    ]

-- | Determines whether to check a directory for existence prior to usage.
-- Generally used for the purposes of better error messages / not repeating
-- checks.
data DirExistsCheck
  = -- | Check existence prior to usage.
    DirExistsCheckOn
  | -- | Do not check existence prior to usage.
    DirExistsCheckOff

-- | Searches for a single file with potentially multiple aliases. Returns
-- at most one result.
--
-- The search is case-insensitive.
searchFileAliases ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    PathReader :> es
  ) =>
  -- | Determines whether we check the directory for existence.
  -- If we do not check, we still use it i.e. we assume the check has
  -- already taken place. If the check is off, then failures are allowed
  -- i.e. return empty.
  DirExistsCheck ->
  Path Abs Dir ->
  FileAliases ->
  Eff es (f (Path Abs File))
searchFileAliases checkExists dataDir aliases = do
  -- Cases:
  --
  -- 1. searchFiles: dir must exist but already checked
  -- 2. config: dir might not exist
  case checkExists of
    DirExistsCheckOn -> do
      -- NOTE: [Data dir existence]
      --
      -- Check dir existence first, since otherwise none of this matters.
      dExists <- PR.doesDirectoryExist dataDirOsPath

      if dExists
        then runSearch
        -- Lack of existence OK, hence log and return empty.
        else handleEmptyDir DirNotExistsOk dataDir
    DirExistsCheckOff -> runSearch
  where
    runSearch = go . NE.toList $ aliases.unAliases
    dataDirOsPath = toOsPath dataDir

    go :: (HasCallStack) => List (Path Rel File) -> Eff es (f (Path Abs File))
    go [] = pure empty
    go (f : fs) = do
      let path = dataDir <</>> f
      exists <- PR.doesFileExist (toOsPath path)
      if exists
        -- 1. File exists, return single result
        then pure $ pure path
        else do
          -- 2. File does not exist, try case insensitive search.
          caseInsensResult <- searchCaseInsens f
          if isNonEmpty caseInsensResult
            -- 2.1. Some case-insensitive match, return.
            then pure caseInsensResult
            -- 2.2. No matches, try remaining aliases.
            else go fs

    searchCaseInsens :: (HasCallStack) => Path Rel File -> Eff es (f (Path Abs File))
    searchCaseInsens f = do
      let p = toOsPath f
          pLower = toLower p

      -- NOTE: This will _fail_ if the data directory does not exist. Hence
      -- we should check existence first. See NOTE: [Data dir existence].
      allFiles <- PR.listDirectory dataDirOsPath

      case F.find ((==) pLower . toLower) allFiles of
        Just osPath -> do
          relFile <- Path.parseRelFile osPath
          let absFile = dataDir <</>> relFile
              msg =
                mconcat
                  [ "Did not find exact match for '",
                    packText $ showPath f,
                    "' but found '",
                    packText $ showPath absFile,
                    "', using it."
                  ]
          -- NOTE: As we only perform a case-insens search during file
          -- discovery, this really shouldn't be a warning, since it's
          -- "best-effort" anyway. For instance, if someone names their garmin
          -- file 'activites.csv' -- cf. 'Activites'csv' -- then we should
          -- find it without any fanfare.
          --
          -- The info level already logs the exact filepath being used,
          -- so this slightly more granular message (searched vs. found)
          -- makes more sense for debug.
          $(Logger.logDebug) msg
          pure $ pure absFile
        Nothing -> pure empty

    toLower :: OsPath -> OsPath
    toLower =
      encodeLenient
        . unpackText
        . T.toCaseFold
        . packText
        . decodeLenient

-- | Represents multiple files for which we search. Unlike 'FileAliases',
-- 'SearchFiles' represents _different_ files (not mere alias differences),
-- hence a 'SearchFiles' search can return multiple results.
newtype SearchFiles = MkSearchFiles
  { unSearchFiles :: NonEmpty FileAliases
  }

-- | Represents a single file for which we want to search. Holds multiple
-- aliases for the case where the file can have multiple names.
-- For instance, both chart-requests.json and chart-requests.jsonc are valid
-- expected names for the chart-requests file, so we search for both, even
-- though we want at most one.
newtype FileAliases = MkFileAliases
  { unAliases :: NonEmpty (Path Rel File)
  }

searchFilesToList :: SearchFiles -> List (Path Rel File)
searchFilesToList =
  NE.toList
    <=< ( NE.toList
            . fmap (.unAliases)
            . (.unSearchFiles)
        )

encodePretty :: (ToJSON a) => a -> LazyByteString
encodePretty = AsnPretty.encodePretty' cfg
  where
    cfg =
      AsnPretty.defConfig
        { confIndent = Spaces 2,
          confTrailingNewline = True
        }
