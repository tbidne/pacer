{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Utils.FileSearch (tests) where

import Data.List qualified as L
import Data.Set qualified as Set
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import Pacer.Class.FromAlt (FromAlt)
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
      testAliasOneMultiMatch
    ]

testAliasOneNone :: TestTree
testAliasOneNone = testCase "Finds no matches" $ do
  for_ fileSearch $ \sf -> do
    result <-
      runFileSearch @Maybe [reldir|testAlias|] sf
    Nothing @=? result
  where
    fileSearch :: List FileSearch
    fileSearch =
      [ SearchFileAliases (MkFileAliases [f]),
        SearchFileAliases (MkFileAliases [g]),
        SearchFileAliases (MkFileAliases [f, g]),
        SearchFileAliases (MkFileAliases [g, f])
      ]
    f = [relfile|four|]
    g = [relfile|five|]

testAliasOneMatch :: TestTree
testAliasOneMatch = testCase "Finds match" $ do
  result <-
    runFileSearch @Maybe [reldir|testAlias|] fileSearch
  Just [osp|one|] @=? result
  where
    fileSearch = SearchFileAliases aliases
    aliases = MkFileAliases [[relfile|one|]]

testAliasOneSecondMatch :: TestTree
testAliasOneSecondMatch = testCase "Finds second match" $ do
  result <-
    runFileSearch @Maybe [reldir|testAlias|] fileSearch
  Just [osp|two|] @=? result
  where
    fileSearch = SearchFileAliases aliases
    aliases = MkFileAliases [[relfile|four|], [relfile|two|]]

testAliasOneMultiMatch :: TestTree
testAliasOneMultiMatch = testCase "Finds first of multi search" $ do
  -- finds the first.
  result1 <-
    runFileSearch
      @Maybe
      [reldir|testAlias|]
      (SearchFileAliases (MkFileAliases [f, g]))
  Just [osp|one|] @=? result1

  -- finds the swapped first.
  result2 <-
    runFileSearch
      @Maybe
      [reldir|testAlias|]
      (SearchFileAliases (MkFileAliases [g, f]))
  Just [osp|two|] @=? result2
  where
    f = [relfile|one|]
    g = [relfile|two|]

aliasSearchManyTests :: TestTree
aliasSearchManyTests =
  testGroup
    "Many"
    [ testAliasManyNone,
      testAliasManyMatch,
      testAliasManySecondMatch,
      testAliasManyMultiMatch
    ]

testAliasManyNone :: TestTree
testAliasManyNone = testCase "Finds no matches" $ do
  for_ fileSearch $ \sf -> do
    result <-
      runFileSearch @List [reldir|testAlias|] sf
    [] @=? result
  where
    fileSearch :: List FileSearch
    fileSearch =
      [ SearchFileAliases (MkFileAliases [f]),
        SearchFileAliases (MkFileAliases [g]),
        SearchFileAliases (MkFileAliases [f, g]),
        SearchFileAliases (MkFileAliases [g, f])
      ]
    f = [relfile|four|]
    g = [relfile|five|]

testAliasManyMatch :: TestTree
testAliasManyMatch = testCase "Finds match" $ do
  result <-
    runFileSearch @List [reldir|testAlias|] fileSearch
  [[osp|one|]] @=? result
  where
    fileSearch = SearchFileAliases aliases
    aliases = MkFileAliases [[relfile|one|]]

testAliasManySecondMatch :: TestTree
testAliasManySecondMatch = testCase "Finds second match" $ do
  result <-
    runFileSearch @List [reldir|testAlias|] fileSearch
  [[osp|two|]] @=? result
  where
    fileSearch = SearchFileAliases aliases
    aliases = MkFileAliases [[relfile|four|], [relfile|two|]]

testAliasManyMultiMatch :: TestTree
testAliasManyMultiMatch = testCase "Finds all for multi search" $ do
  -- finds all matches.
  result1 <-
    runFileSearch
      @List
      [reldir|testAlias|]
      (SearchFileAliases (MkFileAliases [f, g]))
  [[osp|one|], [osp|two|]] @=? result1

  -- finds all matches, different order.
  result2 <-
    runFileSearch
      @List
      [reldir|testAlias|]
      (SearchFileAliases (MkFileAliases [g, f]))
  [[osp|two|], [osp|one|]] @=? result2
  where
    f = [relfile|one|]
    g = [relfile|two|]

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
    result <-
      runFileSearch @Maybe [reldir|testInfix|] sf
    Nothing @=? result
  where
    fileSearch :: List FileSearch
    fileSearch =
      [ SearchFileInfix f [],
        SearchFileInfix g [],
        SearchFileAliases (MkFileAliases [f, g])
      ]
    f = [relfile|threebar|]
    g = [relfile|barx|]

testInfixOneMatch :: TestTree
testInfixOneMatch = testCase "Finds match" $ do
  result <-
    runFileSearch @Maybe [reldir|testInfix|] fileSearch
  Just [osp|foothree|] @=? result
  where
    fileSearch = SearchFileInfix [relfile|three|] []

testInfixOneMultiMatch :: TestTree
testInfixOneMultiMatch = testCase "Finds first of multi search" $ do
  result1 <-
    runFileSearch
      @Maybe
      [reldir|testInfix|]
      (SearchFileInfix f [])

  -- These are non-deterministically determined by the order of
  -- listDirectory.
  case result1 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1

  result2 <-
    runFileSearch
      @Maybe
      [reldir|testInfix|]
      (SearchFileInfix g [])

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
      [reldir|testInfix|]
      (SearchFileInfix f [[osp|.csv|], [osp|.tar.gz|]])

  case result1 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1

  result2 <-
    runFileSearch
      @Maybe
      [reldir|testInfix|]
      (SearchFileInfix g [[osp|.json|], [osp|.yaml|]])

  case result2 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults2

  result3 <-
    runFileSearch
      @Maybe
      [reldir|testInfix|]
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
    result <-
      runFileSearch @List [reldir|testInfix|] sf
    [] @=? result
  where
    fileSearch :: List FileSearch
    fileSearch =
      [ SearchFileInfix f [],
        SearchFileInfix g [],
        SearchFileAliases (MkFileAliases [f, g])
      ]
    f = [relfile|threebar|]
    g = [relfile|barx|]

testInfixManyMatch :: TestTree
testInfixManyMatch = testCase "Finds match" $ do
  result <-
    runFileSearch @List [reldir|testInfix|] fileSearch
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
        [reldir|testInfix|]
        (SearchFileInfix f [])

  for_ result1 $ \r ->
    assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1

  -- finds the swapped first.
  result2 <-
    L.sort
      <$> runFileSearch
        @List
        [reldir|testInfix|]
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
        [reldir|testInfix|]
        (SearchFileInfix f [[osp|.csv|], [osp|.tar.gz|]])
  for_ result1 $ \r ->
    assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults1

  result2 <-
    L.sort
      <$> runFileSearch
        @List
        [reldir|testInfix|]
        (SearchFileInfix g [[osp|.json|], [osp|.yaml|]])

  for_ result2 $ \r ->
    assertBool ("Received: " ++ show r) $ r `Set.member` possibleResults2

  result3 <-
    L.sort
      <$> runFileSearch
        @List
        [reldir|testInfix|]
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
  Path Rel Dir ->
  FileSearch ->
  IO (f OsPath)
runFileSearch dir fileSearch = do
  results <-
    run
      $ FileSearch.resolveFilePath
        "runFileSearch_test"
        fileSearch
        [FileSearch.findDirectoryPath $ Just $ nameToDir dir]

  pure $ k <$> results
  where
    run =
      runEff
        . runLoggerMock
        . runLoggerNS ""
        . runPathReader

    k = OsPath.takeFileName . toOsPath

runLoggerMock :: Eff (Logger : es) a -> Eff es a
runLoggerMock = interpret_ $ \case
  LoggerLog {} -> pure ()

nameToDir :: Path Rel Dir -> OsPath
nameToDir p = toOsPath $ [reldirPathSep|test/unit/data/file-search|] <</>> p
