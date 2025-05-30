{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Utils.FileSearch (tests) where

import Data.List qualified as L
import Data.Set qualified as Set
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import Pacer.Class.FromAlt (FromAlt)
import Pacer.Utils.FileSearch
  ( FileAliases (MkFileAliases),
    SearchFileType (SearchFileAliases, SearchFileInfix),
    SearchFiles (MkSearchFiles),
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
  result <-
    runFileSearch @Maybe [reldir|testAlias|] searchFiles
  Nothing @=? result
  where
    searchFiles =
      MkSearchFiles
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
    runFileSearch @Maybe [reldir|testAlias|] searchFiles
  Just [osp|OnE|] @=? result
  where
    searchFiles = MkSearchFiles [SearchFileAliases aliases]
    aliases = MkFileAliases [[relfile|oNe|]]

testAliasOneSecondMatch :: TestTree
testAliasOneSecondMatch = testCase "Finds second match" $ do
  result <-
    runFileSearch @Maybe [reldir|testAlias|] searchFiles
  Just [osp|TwO|] @=? result
  where
    searchFiles = MkSearchFiles [SearchFileAliases aliases]
    aliases = MkFileAliases [[relfile|four|], [relfile|tWo|]]

-- Note that:
--
-- - SearchFiles [Aliases a1, Aliases a2]
-- - SearchFiles [Aliases [a1, a2]]
--
-- Are equivalent for Maybe.
testAliasOneMultiMatch :: TestTree
testAliasOneMultiMatch = testCase "Finds first of multi search" $ do
  -- finds the first.
  result1 <-
    runFileSearch
      @Maybe
      [reldir|testAlias|]
      (MkSearchFiles [SearchFileAliases (MkFileAliases [f]), SearchFileAliases (MkFileAliases [g])])
  Just [osp|OnE|] @=? result1

  -- finds the swapped first.
  result2 <-
    runFileSearch
      @Maybe
      [reldir|testAlias|]
      (MkSearchFiles [SearchFileAliases (MkFileAliases [g]), SearchFileAliases (MkFileAliases [f])])
  Just [osp|TwO|] @=? result2

  -- should be the same as the first.
  result3 <-
    runFileSearch
      @Maybe
      [reldir|testAlias|]
      (MkSearchFiles [SearchFileAliases (MkFileAliases [f, g])])
  Just [osp|OnE|] @=? result3
  where
    f = [relfile|oNe|]
    g = [relfile|tWo|]

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
  result <-
    runFileSearch @List [reldir|testAlias|] searchFiles
  [] @=? result
  where
    searchFiles =
      MkSearchFiles
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
    runFileSearch @List [reldir|testAlias|] searchFiles
  [[osp|OnE|]] @=? result
  where
    searchFiles = MkSearchFiles [SearchFileAliases aliases]
    aliases = MkFileAliases [[relfile|oNe|]]

testAliasManySecondMatch :: TestTree
testAliasManySecondMatch = testCase "Finds second match" $ do
  result <-
    runFileSearch @List [reldir|testAlias|] searchFiles
  [[osp|TwO|]] @=? result
  where
    searchFiles = MkSearchFiles [SearchFileAliases aliases]
    aliases = MkFileAliases [[relfile|four|], [relfile|tWo|]]

testAliasManyMultiMatch :: TestTree
testAliasManyMultiMatch = testCase "Finds all for multi search" $ do
  -- finds all matches.
  result1 <-
    runFileSearch
      @List
      [reldir|testAlias|]
      ( MkSearchFiles
          [ SearchFileAliases (MkFileAliases [f]),
            SearchFileAliases (MkFileAliases [g])
          ]
      )
  [[osp|OnE|], [osp|TwO|]] @=? result1

  -- finds all matches, different order.
  result2 <-
    runFileSearch
      @List
      [reldir|testAlias|]
      ( MkSearchFiles
          [ SearchFileAliases (MkFileAliases [g]),
            SearchFileAliases (MkFileAliases [f])
          ]
      )
  [[osp|TwO|], [osp|OnE|]] @=? result2

  -- Tests that SearchAliases is distributive i.e.
  --
  -- - SearchFiles [Aliases a1, Aliases a2]
  -- - SearchFiles [Aliases [a1, a2]]
  result3 <-
    runFileSearch
      @List
      [reldir|testAlias|]
      (MkSearchFiles [SearchFileAliases (MkFileAliases [f, g])])
  [[osp|OnE|], [osp|TwO|]] @=? result3
  where
    f = [relfile|oNe|]
    g = [relfile|tWo|]

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
  result <-
    runFileSearch @Maybe [reldir|testInfix|] searchFiles
  Nothing @=? result
  where
    searchFiles =
      MkSearchFiles
        [ SearchFileInfix f [],
          SearchFileInfix g [],
          SearchFileAliases (MkFileAliases [f, g])
        ]
    f = [relfile|threebar|]
    g = [relfile|barx|]

testInfixOneMatch :: TestTree
testInfixOneMatch = testCase "Finds match" $ do
  result <-
    runFileSearch @Maybe [reldir|testInfix|] searchFiles
  Just [osp|FoOtHrEe|] @=? result
  where
    searchFiles = MkSearchFiles [SearchFileInfix [relfile|ThReE|] []]

testInfixOneMultiMatch :: TestTree
testInfixOneMultiMatch = testCase "Finds first of multi search" $ do
  result1 <-
    runFileSearch
      @Maybe
      [reldir|testInfix|]
      (MkSearchFiles [SearchFileInfix f [], SearchFileInfix g []])
  Just [osp|FoOoNe|] @=? result1

  -- These are non-deterministically determined by the order of
  -- listDirectory.
  case result1 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received :" ++ show r) $ r `Set.member` possibleResults

  result2 <-
    runFileSearch
      @Maybe
      [reldir|testInfix|]
      (MkSearchFiles [SearchFileInfix g [], SearchFileInfix f []])

  case result2 of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received :" ++ show r) $ r `Set.member` possibleResults
  where
    f = [relfile|oNe|]
    g = [relfile|tWo|]

    possibleResults =
      Set.fromList
        [ [osp|FoOoNe|],
          [osp|FoOoNeBaR|],
          [osp|FoOtWo|],
          [osp|OnEbAr|],
          [osp|TwObAr|]
        ]

testInfixOneExts :: TestTree
testInfixOneExts = testCase "Finds some matching extension" $ do
  result <-
    runFileSearch
      @Maybe
      [reldir|testInfix|]
      ( MkSearchFiles
          [ SearchFileInfix f [[osp|.csv|], [osp|.tar.gz|]],
            SearchFileInfix g [[osp|.json|], [osp|.yaml|]],
            SearchFileInfix h [[osp|.gz|]]
          ]
      )

  case result of
    Nothing -> assertFailure "Expected result, received nothing"
    Just r -> assertBool ("Received :" ++ show r) $ r `Set.member` possibleResults
  where
    f = [relfile|baz|]
    g = [relfile|quux|]
    h = [relfile|mer|]

    possibleResults =
      Set.fromList
        [ [osp|bazquux.csv|],
          [osp|bazquux.json|],
          [osp|bazquux.tar.gz|],
          [osp|bazquux.yaml|],
          [osp|mer.tar.gz|]
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
  result <-
    runFileSearch @List [reldir|testInfix|] searchFiles
  [] @=? result
  where
    searchFiles =
      MkSearchFiles
        [ SearchFileInfix f [],
          SearchFileInfix g [],
          SearchFileAliases (MkFileAliases [f, g])
        ]
    f = [relfile|threebar|]
    g = [relfile|barx|]

testInfixManyMatch :: TestTree
testInfixManyMatch = testCase "Finds match" $ do
  result <-
    runFileSearch @List [reldir|testInfix|] searchFiles
  [[osp|FoOtHrEe|]] @=? result
  where
    searchFiles = MkSearchFiles [SearchFileInfix [relfile|ThReE|] []]

testInfixManyMultiMatch :: TestTree
testInfixManyMultiMatch = testCase "Finds all for multi search" $ do
  -- Sorting for non-determinism.
  result1 <-
    L.sort
      <$> runFileSearch
        @List
        [reldir|testInfix|]
        (MkSearchFiles [SearchFileInfix f [], SearchFileInfix g []])
  [ [osp|FoOoNe|],
    [osp|FoOoNeBaR|],
    [osp|FoOtWo|],
    [osp|OnEbAr|],
    [osp|TwObAr|]
    ]
    @=? result1

  -- finds the swapped first.
  result2 <-
    L.sort
      <$> runFileSearch
        @List
        [reldir|testInfix|]
        (MkSearchFiles [SearchFileInfix g [], SearchFileInfix f []])
  [ [osp|FoOoNe|],
    [osp|FoOoNeBaR|],
    [osp|FoOtWo|],
    [osp|OnEbAr|],
    [osp|TwObAr|]
    ]
    @=? result2
  where
    f = [relfile|oNe|]
    g = [relfile|tWo|]

testInfixManyExts :: TestTree
testInfixManyExts = testCase "Finds all matching extension" $ do
  result <-
    L.sort
      <$> runFileSearch
        @List
        [reldir|testInfix|]
        ( MkSearchFiles
            [ SearchFileInfix f [[osp|.csv|], [osp|.tar.gz|]],
              SearchFileInfix g [[osp|.json|], [osp|.yaml|]],
              SearchFileInfix h [[osp|.gz|]]
            ]
        )
  [ [osp|bazquux.csv|],
    [osp|bazquux.json|],
    [osp|bazquux.tar.gz|],
    [osp|bazquux.yaml|],
    [osp|mer.tar.gz|]
    ]
    @=? result
  where
    f = [relfile|baz|]
    g = [relfile|quux|]
    h = [relfile|mer|]

runFileSearch ::
  (FromAlt f) =>
  Path Rel Dir ->
  SearchFiles ->
  IO (f OsPath)
runFileSearch dir searchFiles = do
  results <-
    run
      $ FileSearch.resolveFilePath
        "runFileSearch_test"
        searchFiles
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
