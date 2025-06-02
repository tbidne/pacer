{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Utils.FileSearch (tests) where

import Data.List qualified as L
import Data.Set qualified as Set
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import Pacer.Class.FromAlt (FromAlt)
import Pacer.Configuration.Env.Types (LogEnv (MkLogEnv))
import Pacer.Utils.FileSearch
  ( FileAliases (MkFileAliases),
    FileSearch (SearchFileAliases, SearchFileInfix),
  )
import Pacer.Utils.FileSearch qualified as FileSearch
import System.OsPath qualified as OsPath
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Utils.FileSearch"
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
        "runFileSearch_test"
        fileSearch
        [FileSearch.findDirectoryPath $ Just fileSearchDir]

  pure $ k <$> results
  where
    run =
      runEff
        . runLoggerMock
        . runLoggerNS ""
        . runPathReader
        . runReader logEnv

    logEnv = MkLogEnv Nothing mempty mempty

    k = OsPath.takeFileName . toOsPath

runLoggerMock :: Eff (Logger : es) a -> Eff es a
runLoggerMock = interpret_ $ \case
  LoggerLog {} -> pure ()

fileSearchDir :: OsPath
fileSearchDir = toOsPath $ [reldirPathSep|test/unit/data/file-search|]
