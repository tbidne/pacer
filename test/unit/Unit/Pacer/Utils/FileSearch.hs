{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Unit.Pacer.Utils.FileSearch (tests) where

import Data.Char qualified as Ch
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import FileSystem.OsPath (unsafeEncode)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Pacer.Class.FromAlt (FromAlt)
import Pacer.Configuration.Env.Types (LogEnv (MkLogEnv), runReaderLogEnvMock)
import Pacer.Utils.FileSearch
  ( FileAliases (MkFileAliases),
    FileSearch (SearchFileAliases, SearchFileInfix),
    MatchResult,
  )
import Pacer.Utils.FileSearch qualified as FileSearch
import System.OsPath qualified as OsP
import System.OsPath qualified as OsPath
import Unit.Prelude

data PathData = MkPathData
  { dirs :: List OsPath,
    name :: OsPath,
    exts :: List OsPath
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''PathData

toPath :: PathData -> OsPath
toPath pd =
  dirPath
    </> (pd ^. #name)
    <> exts
  where
    dirPath = OsP.joinPath (pd ^. #dirs)

    exts = concatExts (pd ^. #exts)

tests :: TestTree
tests =
  testGroup
    "Pacer.Utils.FileSearch"
    [ findDirectoryPathTests,
      miscTests
    ]

findDirectoryPathTests :: TestTree
findDirectoryPathTests =
  testGroup
    "findDirectoryPath"
    [ aliasSearchTests,
      infixSearchTests
    ]

aliasSearchTests :: TestTree
aliasSearchTests =
  testGroup
    "Aliases"
    [ aliasSearchOneTests,
      aliasSearchManyTests
    ]

-- Tests for a single alias result i.e. FromAlt f ~ Maybe.
aliasSearchOneTests :: TestTree
aliasSearchOneTests =
  testGroup
    "One"
    [ testAliasOneNone,
      testAliasOneMatch,
      testAliasOneSecondMatch,
      testAliasOneMultiMatch,
      testAliasOneExts
    ]

testAliasOneNone :: TestTree
testAliasOneNone = testCase "Finds no matches" $ do
  for_ fileSearch $ \sf -> do
    result <- runFileSearch @Maybe sf
    Nothing @=? result
  where
    fileSearch :: List FileSearch
    fileSearch =
      [ SearchFileAliases (MkFileAliases [f] Set.empty),
        SearchFileAliases (MkFileAliases [g] Set.empty),
        SearchFileAliases (MkFileAliases [f, g] Set.empty),
        SearchFileAliases (MkFileAliases [g, f] Set.empty)
      ]
    f = [relfile|four|]
    g = [relfile|five|]

testAliasOneMatch :: TestTree
testAliasOneMatch = testCase "Finds match" $ do
  result <- runFileSearch @Maybe fileSearch
  Just [osp|fooone|] @=? result
  where
    fileSearch = SearchFileAliases aliases
    aliases = MkFileAliases [[relfile|fooone|]] Set.empty

testAliasOneSecondMatch :: TestTree
testAliasOneSecondMatch = testCase "Finds second match" $ do
  result <- runFileSearch @Maybe fileSearch
  Just [osp|footwo|] @=? result
  where
    fileSearch = SearchFileAliases aliases
    aliases = MkFileAliases [[relfile|four|], [relfile|footwo|]] Set.empty

testAliasOneMultiMatch :: TestTree
testAliasOneMultiMatch = testCase "Finds first of multi search" $ do
  -- finds the first.
  result1 <-
    runFileSearch
      @Maybe
      (SearchFileAliases (MkFileAliases [f, g] Set.empty))
  Just [osp|fooone|] @=? result1

  -- finds the swapped first.
  result2 <-
    runFileSearch
      @Maybe
      (SearchFileAliases (MkFileAliases [g, f] Set.empty))
  Just [osp|footwo|] @=? result2
  where
    f = [relfile|fooone|]
    g = [relfile|footwo|]

testAliasOneExts :: TestTree
testAliasOneExts = testCase "Finds matching extension" $ do
  result <-
    runFileSearch
      @Maybe
      (SearchFileInfix f [[osp|.csv|], [osp|.tar.gz|]])

  case result of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults
  where
    f = [relfile|bazquux|]

    possibleResults =
      Set.fromList
        [ [osp|bazquux.csv|],
          [osp|bazquux.tar.gz|]
        ]

aliasSearchManyTests :: TestTree
aliasSearchManyTests =
  testGroup
    "Many"
    [ testAliasManyNone,
      testAliasManyMatch,
      testAliasManySecondMatch,
      testAliasManyMultiMatch,
      testAliasManyExts
    ]

testAliasManyNone :: TestTree
testAliasManyNone = testCase "Finds no matches" $ do
  for_ fileSearch $ \sf -> do
    result <-
      runFileSearch @List sf
    [] @=? result
  where
    fileSearch :: List FileSearch
    fileSearch =
      [ SearchFileAliases (MkFileAliases [f] Set.empty),
        SearchFileAliases (MkFileAliases [g] Set.empty),
        SearchFileAliases (MkFileAliases [f, g] Set.empty),
        SearchFileAliases (MkFileAliases [g, f] Set.empty)
      ]
    f = [relfile|four|]
    g = [relfile|five|]

testAliasManyMatch :: TestTree
testAliasManyMatch = testCase "Finds match" $ do
  result <-
    runFileSearch @List fileSearch
  [[osp|fooone|]] @=? result
  where
    fileSearch = SearchFileAliases aliases
    aliases = MkFileAliases [[relfile|fooone|]] Set.empty

testAliasManySecondMatch :: TestTree
testAliasManySecondMatch = testCase "Finds second match" $ do
  result <-
    runFileSearch @List fileSearch
  [[osp|footwo|]] @=? result
  where
    fileSearch = SearchFileAliases aliases
    aliases = MkFileAliases [[relfile|four|], [relfile|footwo|]] Set.empty

testAliasManyMultiMatch :: TestTree
testAliasManyMultiMatch = testCase "Finds all for multi search" $ do
  -- finds all matches.
  result1 <-
    runFileSearch
      @List
      (SearchFileAliases (MkFileAliases [f, g] Set.empty))
  [[osp|fooone|], [osp|footwo|]] @=? result1

  -- finds all matches, different order.
  result2 <-
    runFileSearch
      @List
      (SearchFileAliases (MkFileAliases [g, f] Set.empty))
  [[osp|footwo|], [osp|fooone|]] @=? result2
  where
    f = [relfile|fooone|]
    g = [relfile|footwo|]

testAliasManyExts :: TestTree
testAliasManyExts = testCase "Finds all matching extension" $ do
  result1 <-
    L.sort
      <$> runFileSearch
        @List
        (SearchFileInfix f [[osp|.csv|], [osp|.tar.gz|]])
  for_ result1 $ \r ->
    assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1
  where
    f = [relfile|bazquux|]

    possibleResults1 =
      Set.fromList
        [ [osp|bazquux.csv|],
          [osp|bazquux.tar.gz|]
        ]

infixSearchTests :: TestTree
infixSearchTests =
  testGroup
    "Infix"
    [ infixSearchOneTests,
      infixSearchManyTests
    ]

infixSearchOneTests :: TestTree
infixSearchOneTests =
  testGroup
    "One"
    [ testInfixOneNone,
      testInfixOneMatch,
      testInfixOneMultiMatch,
      testInfixOneExts
    ]

testInfixOneNone :: TestTree
testInfixOneNone = testCase "Finds no matches" $ do
  for_ fileSearch $ \sf -> do
    result <- runFileSearch @Maybe sf
    Nothing @=? result
  where
    fileSearch :: List FileSearch
    fileSearch =
      [ SearchFileInfix f [],
        SearchFileInfix g [],
        SearchFileAliases (MkFileAliases [f, g] Set.empty)
      ]
    f = [relfile|threebar|]
    g = [relfile|barx|]

testInfixOneMatch :: TestTree
testInfixOneMatch = testCase "Finds match" $ do
  result <- runFileSearch @Maybe fileSearch
  Just [osp|foothree|] @=? result
  where
    fileSearch = SearchFileInfix [relfile|three|] []

testInfixOneMultiMatch :: TestTree
testInfixOneMultiMatch = testCase "Finds first of multi search" $ do
  result1 <- runFileSearch @Maybe (SearchFileInfix f [])

  -- These are non-deterministically determined by the order of
  -- listDirectory.
  case result1 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1

  result2 <- runFileSearch @Maybe (SearchFileInfix g [])

  case result2 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults2
  where
    f = [relfile|one|]
    g = [relfile|two|]

    possibleResults1 =
      Set.fromList
        [ [osp|fooone|],
          [osp|fooonebar|],
          [osp|onebar|]
        ]

    possibleResults2 =
      Set.fromList
        [ [osp|footwo|],
          [osp|twobar|]
        ]

testInfixOneExts :: TestTree
testInfixOneExts = testCase "Finds some matching extension" $ do
  result1 <-
    runFileSearch
      @Maybe
      (SearchFileInfix f [[osp|.csv|], [osp|.tar.gz|]])

  case result1 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1

  result2 <-
    runFileSearch
      @Maybe
      (SearchFileInfix g [[osp|.json|], [osp|.yaml|]])

  case result2 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults2

  result3 <-
    runFileSearch
      @Maybe
      (SearchFileInfix h [[osp|.gz|]])

  case result3 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> [osp|mer.tar.gz|] @=? r
  where
    f = [relfile|baz|]
    g = [relfile|quux|]
    h = [relfile|mer|]

    possibleResults1 =
      Set.fromList
        [ [osp|bazquux.csv|],
          [osp|bazquux.tar.gz|]
        ]

    possibleResults2 =
      Set.fromList
        [ [osp|bazquux.json|],
          [osp|bazquux.yaml|]
        ]

infixSearchManyTests :: TestTree
infixSearchManyTests =
  testGroup
    "Many"
    [ testInfixManyNone,
      testInfixManyMatch,
      testInfixManyMultiMatch,
      testInfixManyExts
    ]

testInfixManyNone :: TestTree
testInfixManyNone = testCase "Finds no matches" $ do
  for_ fileSearch $ \sf -> do
    result <- runFileSearch @List sf
    [] @=? result
  where
    fileSearch :: List FileSearch
    fileSearch =
      [ SearchFileInfix f [],
        SearchFileInfix g [],
        SearchFileAliases (MkFileAliases [f, g] Set.empty)
      ]
    f = [relfile|threebar|]
    g = [relfile|barx|]

testInfixManyMatch :: TestTree
testInfixManyMatch = testCase "Finds match" $ do
  result <- runFileSearch @List fileSearch
  [[osp|foothree|]] @=? result
  where
    fileSearch = SearchFileInfix [relfile|three|] []

testInfixManyMultiMatch :: TestTree
testInfixManyMultiMatch = testCase "Finds all for multi search" $ do
  -- Sorting for non-determinism.
  result1 <-
    L.sort
      <$> runFileSearch
        @List
        (SearchFileInfix f [])

  for_ result1 $ \r ->
    assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1

  -- finds the swapped first.
  result2 <-
    L.sort
      <$> runFileSearch
        @List
        (SearchFileInfix g [])

  for_ result2 $ \r ->
    assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults2
  where
    f = [relfile|one|]
    g = [relfile|two|]

    possibleResults1 =
      Set.fromList
        [ [osp|fooone|],
          [osp|fooonebar|],
          [osp|onebar|]
        ]

    possibleResults2 =
      Set.fromList
        [ [osp|footwo|],
          [osp|twobar|]
        ]

testInfixManyExts :: TestTree
testInfixManyExts = testCase "Finds all matching extension" $ do
  result1 <-
    L.sort
      <$> runFileSearch
        @List
        (SearchFileInfix f [[osp|.csv|], [osp|.tar.gz|]])
  for_ result1 $ \r ->
    assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1

  result2 <-
    L.sort
      <$> runFileSearch
        @List
        (SearchFileInfix g [[osp|.json|], [osp|.yaml|]])

  for_ result2 $ \r ->
    assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults2

  result3 <-
    L.sort
      <$> runFileSearch
        @List
        (SearchFileInfix h [[osp|.gz|]])

  [[osp|mer.tar.gz|]] @=? result3
  where
    f = [relfile|baz|]
    g = [relfile|quux|]
    h = [relfile|mer|]

    possibleResults1 =
      Set.fromList
        [ [osp|bazquux.csv|],
          [osp|bazquux.tar.gz|]
        ]

    possibleResults2 =
      Set.fromList
        [ [osp|bazquux.json|],
          [osp|bazquux.yaml|]
        ]

runFileSearch ::
  (FromAlt f) =>
  FileSearch ->
  IO (f OsPath)
runFileSearch fileSearch = do
  results <-
    run
      $ FileSearch.resolveFilePath
        @_
        @LogEnv
        "runFileSearch_test"
        fileSearch
        [FileSearch.findDirectoryPath @_ @LogEnv $ Just fileSearchDir]

  pure $ k <$> results
  where
    run =
      runEff
        . runLoggerMock
        . runReaderLogEnvMock
        . runPathReader
        . runReader logEnv

    logEnv = MkLogEnv Nothing mempty mempty

    k = OsPath.takeFileName . toOsPath

runLoggerMock :: Eff (Logger : es) a -> Eff es a
runLoggerMock = interpret_ $ \case
  LoggerLog {} -> pure ()

fileSearchDir :: OsPath
fileSearchDir = toOsPath [reldirPathSep|test/unit/data/file-search|]

miscTests :: TestTree
miscTests =
  testGroup
    "Misc"
    [ testAliasSatisfiesPattern,
      testInfixSatisfiesPattern,
      testOsPathToNameExtsSpecs,
      testOsPathToNameExtsProps
    ]

testAliasSatisfiesPattern :: TestTree
testAliasSatisfiesPattern = testCase desc $ do
  -- simple equality
  let simple1 = satisfies mempty [relfile|foo|] [osp|foo|]
      simple2 = satisfies mempty [relfile|foo|] [osp|foobar|]

  assertPatSuccessNoExt "simple_1" simple1
  assertPatFailNoExt "simple_2" simple2

  -- ignoring extensions
  let ignoreExt1 = satisfies mempty [relfile|foo|] [osp|foo.json|]
      ignoreExt2 = satisfies mempty [relfile|foo|] [osp|bar.json|]

  assertPatSuccessNoExt "ignore_ext_1" ignoreExt1
  assertPatFailNoExt "ignore_ext_2" ignoreExt2

  -- extensions
  let ext1 = satisfies exts [relfile|foo|] [osp|foo.json|]
      ext2 = satisfies exts [relfile|foo|] [osp|foo.jsonc|]
      ext3 = satisfies exts [relfile|foo|] [osp|bar.jsonc|]
      ext4 = satisfies exts [relfile|foo|] [osp|bar.json|]

  assertPatSuccessExtSuccess "ext_1" ext1
  assertPatSuccessExtFail "ext_2" ext2
  assertPatFailExtFail "ext_3" ext3
  assertPatFailExtSuccess "ext_4" ext4

  -- multi-extensions
  let multiExt1 = satisfies exts2 [relfile|foo|] [osp|foo.tar.gz|]
      multiExt2 = satisfies exts2 [relfile|foo|] [osp|foo.gz|]
      multiExt3 = satisfies exts2 [relfile|foo|] [osp|foo.tar.gz|]
      multiExt4 = satisfies exts3 [relfile|foo|] [osp|foo.tar.gz|]

  assertPatSuccessExtSuccess "multi_ext_1" multiExt1
  assertPatSuccessExtSuccess "multi_ext_2" multiExt2
  assertPatSuccessExtSuccess "multi_ext_3" multiExt3
  assertPatSuccessExtFail "multi_ext_4" multiExt4
  where
    desc = "Satisfies alias patterns"
    satisfies = FileSearch.satisfiesPattern (==)

    exts =
      Set.fromList
        [ [osstr|.json|],
          [osstr|.yaml|]
        ]

    exts2 = Set.fromList [[osstr|.gz|]]

    exts3 = Set.fromList [[osstr|.tar|]]

testInfixSatisfiesPattern :: TestTree
testInfixSatisfiesPattern = testCase desc $ do
  -- simple equality
  let simple1 = satisfies mempty [relfile|foo|] [osp|foo|]
      simple2 = satisfies mempty [relfile|foo|] [osp|foobar|]
      simple3 = satisfies mempty [relfile|foo|] [osp|barfoo|]
      simple4 = satisfies mempty [relfile|foo|] [osp|barfoobaz|]
      simple5 = satisfies mempty [relfile|foo|] [osp|foxo|]

  assertPatSuccessNoExt "simple_1" simple1
  assertPatSuccessNoExt "simple_2" simple2
  assertPatSuccessNoExt "simple_3" simple3
  assertPatSuccessNoExt "simple_4" simple4
  assertPatFailNoExt "simple_5" simple5

  -- ignoring extensions
  let ignoreExt1 = satisfies mempty [relfile|foo|] [osp|foo.json|]
      ignoreExt2 = satisfies mempty [relfile|foo|] [osp|foobar.json|]
      ignoreExt3 = satisfies mempty [relfile|foo|] [osp|barfoo.json|]
      ignoreExt4 = satisfies mempty [relfile|foo|] [osp|barfoobaz.json|]
      ignoreExt5 = satisfies mempty [relfile|foo|] [osp|foxo.json|]

  assertPatSuccessNoExt "ignore_ext_1" ignoreExt1
  assertPatSuccessNoExt "ignore_ext_2" ignoreExt2
  assertPatSuccessNoExt "ignore_ext_3" ignoreExt3
  assertPatSuccessNoExt "ignore_ext_4" ignoreExt4
  assertPatFailNoExt "ignore_ext_5" ignoreExt5

  -- extensions
  let ext1 = satisfies exts [relfile|foo|] [osp|barfoobaz.json|]
      ext2 = satisfies exts [relfile|foo|] [osp|barfoobaz.jsonc|]
      ext3 = satisfies exts [relfile|foo|] [osp|bar.jsonc|]
      ext4 = satisfies exts [relfile|foo|] [osp|bar.json|]

  assertPatSuccessExtSuccess "ext_1" ext1
  assertPatSuccessExtFail "ext_2" ext2
  assertPatFailExtFail "ext_3" ext3
  assertPatFailExtSuccess "ext_4" ext4

  -- multi-extensions
  let multiExt1 = satisfies exts2 [relfile|foo|] [osp|foobarbaz.tar.gz|]
      multiExt2 = satisfies exts2 [relfile|foo|] [osp|foobarbaz.gz|]
      multiExt3 = satisfies exts2 [relfile|foo|] [osp|foobarbaz.tar.gz|]
      multiExt4 = satisfies exts3 [relfile|foo|] [osp|foobarbaz.tar.gz|]

  assertPatSuccessExtSuccess "multi_ext_1" multiExt1
  assertPatSuccessExtSuccess "multi_ext_2" multiExt2
  assertPatSuccessExtSuccess "multi_ext_3" multiExt3
  assertPatSuccessExtFail "multi_ext_4" multiExt4
  where
    desc = "Satisfies infix patterns"
    satisfies = FileSearch.satisfiesPattern T.isInfixOf

    exts =
      Set.fromList
        [ [osstr|.json|],
          [osstr|.yaml|]
        ]

    exts2 = Set.fromList [[osstr|.gz|]]

    exts3 = Set.fromList [[osstr|.tar|]]

assertPatSuccessNoExt :: String -> MatchResult -> Assertion
assertPatSuccessNoExt str r = do
  assertBoolMsg str True $ r ^. #patternResult
  case r ^. #extensionResult of
    Nothing -> pure ()
    other -> assertFailure $ str ++ ": Expected Nothing, received: " ++ show other

assertPatSuccessExtSuccess :: String -> MatchResult -> Assertion
assertPatSuccessExtSuccess str r = do
  assertBoolMsg str True $ r ^. #patternResult
  case r ^. #extensionResult of
    Just True -> pure ()
    other -> assertFailure $ str ++ ": Expected True, received: " ++ show other

assertPatSuccessExtFail :: String -> MatchResult -> Assertion
assertPatSuccessExtFail str r = do
  assertBoolMsg str True $ r ^. #patternResult
  case r ^. #extensionResult of
    Just False -> pure ()
    other -> assertFailure $ str ++ ": Expected False, received: " ++ show other

assertPatFailNoExt :: String -> MatchResult -> Assertion
assertPatFailNoExt str r = do
  assertBoolMsg str False $ r ^. #patternResult
  case r ^. #extensionResult of
    Nothing -> pure ()
    other -> assertFailure $ str ++ ": Expected Nothing, received: " ++ show other

assertPatFailExtSuccess :: String -> MatchResult -> Assertion
assertPatFailExtSuccess str r = do
  assertBoolMsg str False $ r ^. #patternResult
  case r ^. #extensionResult of
    Just True -> pure ()
    other -> assertFailure $ str ++ ": Expected True, received: " ++ show other

assertPatFailExtFail :: String -> MatchResult -> Assertion
assertPatFailExtFail str r = do
  assertBoolMsg str False $ r ^. #patternResult
  case r ^. #extensionResult of
    Just False -> pure ()
    other -> assertFailure $ str ++ ": Expected False, received: " ++ show other

testOsPathToNameExtsSpecs :: TestTree
testOsPathToNameExtsSpecs = testCase desc $ do
  ([osstr|foo|], e, e) @=? FileSearch.osPathToNameExts [osp|foo|]

  ( [osstr|foo|],
    [osstr|.tar|],
    [osstr|.tar|]
    )
    @=? FileSearch.osPathToNameExts [osp|foo.tar|]

  ( [osstr|foo|],
    [osstr|.gz|],
    [osstr|.tar.gz|]
    )
    @=? FileSearch.osPathToNameExts [osp|foo.tar.gz|]

  ( [osstr|foo|],
    [osstr|.gz|],
    [osstr|.tar.gz|]
    )
    @=? FileSearch.osPathToNameExts
      ([osp|bar|] </> [osp|foo.tar.gz|])
  where
    desc = "osPathToNameExts specs"
    e = mempty

testOsPathToNameExtsProps :: TestTree
testOsPathToNameExtsProps = testProp "testOsPathToNameExtsProps" desc $ do
  pd <- forAll genPathData
  let fullPath = toPath pd
      result@(name, ext, exts) = FileSearch.osPathToNameExts fullPath

  annotateShow fullPath
  annotateShow result

  -- assert name
  pd ^. #name === name

  -- assert extension
  case L.unsnoc (pd ^. #exts) of
    Nothing -> do
      ext === mempty
      exts === mempty
    Just (_, last) -> do
      concatExts (pd ^. #exts) === exts
      dot <> last === ext

  -- assert full name (no dirs)
  pd ^. #name <> concatExts (pd ^. #exts) === name <> exts
  where
    desc = "Parses filepath to specified parts properties"

concatExts :: List OsString -> OsString
concatExts [] = mempty
concatExts xs@(_ : _) =
  mconcat
    . (dot :)
    . L.intersperse dot
    $ xs

dot :: OsString
dot = [osstr|.|]

genPathData :: Gen PathData
genPathData = do
  dirs <- G.list (R.linearFrom 0 0 5) genPathElem
  name <- genFileName
  exts <- G.list (R.linearFrom 0 0 3) genExt
  pure
    $ MkPathData
      { dirs,
        name,
        exts
      }

genPathElem :: Gen OsPath
genPathElem = unsafeEncode . unpackText <$> G.text r g
  where
    r = R.linearFrom 1 1 5
    g = G.filterT goodChar G.ascii

genFileName :: Gen OsPath
genFileName = unsafeEncode . unpackText <$> G.text r g
  where
    r = R.linearFrom 1 1 5
    g = G.filterT goodChar' G.ascii

    goodChar' c = goodChar c && c /= '.'

genExt :: Gen OsPath
genExt = unsafeEncode . unpackText <$> G.text r g
  where
    r = R.linearFrom 1 1 5
    g = G.filterT goodChar' G.ascii

    goodChar' c = goodChar c && c /= '.'

goodChar :: Char -> Bool
goodChar c = not (Ch.isControl c) && Set.notMember c badChars
  where
    badChars =
      Set.fromList
        [ '/',
          ':',
          '\\'
        ]

assertBoolMsg :: String -> Bool -> Bool -> Assertion
assertBoolMsg _ True True = pure ()
assertBoolMsg _ False False = pure ()
assertBoolMsg prefix expected result = assertFailure str
  where
    str =
      mconcat
        [ prefix,
          ": Expected ",
          show expected,
          ", received ",
          show result
        ]
