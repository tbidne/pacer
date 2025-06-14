{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.ChartData (tests) where

import Data.Char qualified as Ch
import Data.IORef qualified as IORef
import Data.List.NonEmpty qualified as NE
import Data.Sequence.NonEmpty qualified as NESeq
import FileSystem.OsPath (unsafeEncode)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Pacer.Command.Chart.Data.Activity
  ( Activity
      ( MkActivity,
        atype,
        datetime,
        distance,
        duration,
        labels,
        title
      ),
    SomeActivity (MkSomeActivity),
  )
import Pacer.Command.Chart.Data.ChartData qualified as ChartData
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartSmooth (MkChartSmooth, smoothPeriod, smoothType),
    ChartSmoothType (ChartSmoothRolling, ChartSmoothWindow),
    ChartSumPeriod (ChartSumDays),
    ChartType (ChartTypeSum),
  )
import Pacer.Configuration.Env.Types (runLoggerMock, runReaderLogEnvMock)
import Pacer.Data.Distance (Distance (MkDistance))
import Pacer.Data.Distance.Units (SDistanceUnit (SKilometer))
import Pacer.Data.Duration (Duration (MkDuration))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.ChartData"
    [ smoothTests,
      allGroupsTests
    ]

smoothTests :: TestTree
smoothTests =
  testGroup
    "Smooth"
    (testSmoothFaqEx ++ ts)
  where
    ts =
      (uncurry3 (mkSmoothTest defActivities Nothing))
        <$> [ (x, y, z)
            | x <- [2, 4],
              y <- [minBound .. maxBound],
              z <- [3, 5]
            ]

    uncurry3 f (a, b, c) = f a b c

testSmoothFaqEx :: List TestTree
testSmoothFaqEx =
  [ mkTest ChartSmoothRolling ("FAQ rolling example", [osp|testSmoothFaqExRolling|]),
    mkTest ChartSmoothWindow ("FAQ rolling window", [osp|testSmoothFaqExWindow|])
  ]
  where
    mkTest ty ns = mkSmoothTest acts (Just ns) 2 ty 3

    acts =
      toNESeq
        [ mkAct "2024-07-01" 5 1200,
          mkAct "2024-07-02" 10 2400,
          mkAct "2024-07-03" 15 3600,
          mkAct "2024-07-04" 20 4800,
          mkAct "2024-07-05" 15 3600,
          mkAct "2024-07-06" 10 2400,
          mkAct "2024-07-07" 25 6000,
          mkAct "2024-07-08" 5 1200,
          mkAct "2024-07-09" 10 2400,
          mkAct "2024-07-10" 5 1200
        ]

mkSmoothTest ::
  NESeq (SomeActivity Double) ->
  Maybe (Tuple2 TestName OsPath) ->
  Word16 ->
  ChartSmoothType ->
  Word8 ->
  TestTree
mkSmoothTest activities mNames sumPeriod smoothType smoothPeriod =
  testGoldenParams
    $ MkGoldenParams
      { testDesc,
        testName,
        runner =
          smoothGoldenRunner
            activities
            sumPeriod
            smoothType
            smoothPeriod
      }
  where
    (testDesc, testName) =
      fromMaybe
        (derivedTestDesc, derivedTestName)
        mNames

    derivedTestDesc =
      mconcat
        [ "Sum ",
          show sumPeriod,
          ", smooth ",
          Ch.toLower <$> smoothTypeStr,
          " ",
          show smoothPeriod
        ]

    derivedTestName =
      mconcat
        [ [osp|testSum|],
          unsafeEncode $ show sumPeriod,
          [osp|Smooth|],
          unsafeEncode smoothTypeStr,
          unsafeEncode $ show smoothPeriod
        ]

    smoothTypeStr = case smoothType of
      ChartSmoothRolling -> "Rolling"
      ChartSmoothWindow -> "Window"

smoothGoldenRunner ::
  -- | Activities
  NESeq (SomeActivity Double) ->
  -- | Sum period.
  Word16 ->
  -- | Smooth type.
  ChartSmoothType ->
  -- | Smooth period.
  Word8 ->
  IO ByteString
smoothGoldenRunner activities sumPeriod smoothType smoothPeriod = do
  result <-
    runner
      $ ChartData.handleChartType
        (Just chartType)
        activities

  pure $ pShowBS result
  where
    chartType =
      ChartTypeSum
        (ChartSumDays (unsafePositive sumPeriod))
        (Just chartSmooth)

    chartSmooth =
      MkChartSmooth
        { smoothPeriod = unsafePositive smoothPeriod,
          smoothType
        }

    runner =
      runEff
        . runLoggerMock
        . runReaderLogEnvMock

allGroupsTests :: TestTree
allGroupsTests =
  testGroup
    "allGroups"
    [ testAllGroupsSpecs,
      testAllGroupsProps
    ]

testAllGroupsSpecs :: TestTree
testAllGroupsSpecs = testCase desc $ do
  expected @=? ChartData.allGroups (unsafePositive 3) (toNESeq [1 .. 6])
  where
    desc = "Splits the list into all groups of size k"

    expected :: NESeq (NESeq Int)
    expected =
      toNESeq
        . fmap toNESeq
        $ [ [1, 2, 3],
            [2, 3, 4],
            [3, 4, 5],
            [4, 5, 6]
          ]

testAllGroupsProps :: TestTree
testAllGroupsProps = testProp "testAllGroupsProps" desc $ do
  seq <- forAll genNESeq
  k <- forAll genPWord8

  let result = ChartData.allGroups k seq
      k' = toInt $ k ^. #unPositive

  annotateShow result

  if k' > length seq
    then
      NESeq.singleton seq === result
    else do
      prevGroupRef <- liftIO $ IORef.newIORef Nothing
      for_ result $ \g -> do
        annotateShow g
        toInt (k ^. #unPositive) === length g

        -- This prop relies on the fact that the generated list is a sequence
        -- e.g. [1,2,3,4,5,6,7,8].
        (liftIO $ IORef.readIORef prevGroupRef) >>= \case
          Nothing -> pure ()
          Just prevGroup -> do
            fmap (+ 1) prevGroup === g

        liftIO $ IORef.writeIORef prevGroupRef (Just g)
  where
    desc = "allGroups properties"

    toInt = fromIntegral @Word8 @Int

defActivities :: NESeq (SomeActivity Double)
defActivities =
  toNESeq
    [ mkAct "2024-07-01" 5 1200,
      mkAct "2024-07-03" 10 2400,
      mkAct "2024-07-04" 3 600,
      mkAct "2024-07-10" 20 4800,
      mkAct "2024-07-12" 15 3000,
      mkAct "2024-07-17" 12 2600,
      mkAct "2024-07-19" 8 1500,
      mkAct "2024-07-20" 6 1400,
      mkAct "2024-07-22" 2 400,
      mkAct "2024-07-25" 5 1100,
      mkAct "2024-07-28" 8 1300,
      mkAct "2024-07-31" 42 9000,
      mkAct "2024-08-01" 30 6000,
      mkAct "2024-08-02" 17 3000,
      mkAct "2024-08-06" 5 1000,
      mkAct "2024-08-08" 8 1700
    ]

mkAct :: Text -> Double -> Double -> SomeActivity Double
mkAct ts dist duration =
  MkSomeActivity
    SKilometer
    $ MkActivity
      { atype = Nothing,
        datetime = parseOrDie ts,
        distance = MkDistance $ unsafePositive dist,
        duration = MkDuration $ unsafePositive duration,
        labels = mempty,
        title = Nothing
      }

genNESeq :: Gen (NESeq Int)
genNESeq = do
  len <- G.integral (R.linearFrom 1 1 100)
  let xs = [1 .. len]
  case NE.nonEmpty xs of
    Nothing ->
      error
        $ mconcat
          [ "Failed generating non-empty list for len ",
            show len,
            ": ",
            show xs
          ]
    Just ys -> pure $ NESeq.fromList ys

genPWord8 :: Gen PWord8
genPWord8 = mkPositiveFail =<< G.integral (R.linearFrom 1 1 10)

toNESeq :: (HasCallStack) => List a -> NESeq a
toNESeq = NESeq.fromList . NE.fromList
