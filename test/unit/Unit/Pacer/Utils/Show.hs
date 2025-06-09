{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Utils.Show (tests) where

import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Pacer.Utils.Show qualified as Show
import Text.Read qualified as TR
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Utils.Show"
    [ displaySummarizedSequencesTests
    ]

displaySummarizedSequencesTests :: TestTree
displaySummarizedSequencesTests =
  testGroup
    "displaySummarizedSequences"
    [ displaySummarizedSequencesSpec,
      displaySummarizedSequencesProps
    ]

displaySummarizedSequencesSpec :: TestTree
displaySummarizedSequencesSpec = testCase "Summarizes list" $ do
  let result = Show.displaySummarizedSequences @Int xs
  expected @=? result
  where
    expected =
      mconcat
        [ "[4,",
          "56,",
          "70-71,",
          "73-74,",
          "76,",
          "78,",
          "80-81,",
          "83,",
          "86,",
          "88,",
          "91-92,",
          "94-95,",
          "97-98,",
          "100-105,",
          "108-109,",
          "111,",
          "114,",
          "117,",
          "119,",
          "121]"
        ]
    xs =
      [ 4,
        56,
        70,
        71,
        73,
        74,
        76,
        78,
        80,
        81,
        83,
        86,
        88,
        91,
        92,
        94,
        95,
        97,
        98,
        100,
        101,
        102,
        103,
        104,
        105,
        108,
        109,
        111,
        114,
        117,
        119,
        121
      ]

displaySummarizedSequencesProps :: TestTree
displaySummarizedSequencesProps = testProp "displaySummarizedSequences" desc $ do
  nats <- forAll genList

  let resultTxt = Show.displaySummarizedSequences nats

  annotateShow (unpackText resultTxt)

  resultNats <- case parseResult resultTxt of
    Ok x -> pure x
    Err e -> annotate e *> failure

  annotateShow resultNats

  -- check:
  --   - monotonically increasing: start > prevEnd + 1
  --   - each range well-formed: start <= end
  prevEndRef <- liftIO $ IORef.newIORef Nothing
  for_ resultNats $ \(s, e) -> do
    mPrevEnd <- liftIO $ IORef.readIORef prevEndRef

    -- special case for first element w/ no previous end.
    case mPrevEnd of
      Nothing -> pure ()
      Just prevEnd ->
        if not (s > prevEnd + 1)
          then do
            annotate
              $ mconcat
                [ "Start '",
                  show s,
                  "' and previous end '",
                  show prevEnd,
                  "' failed invariant 'start > prevEnd + 1'."
                ]
            failure
          else pure ()

    if not (s <= e)
      then do
        annotate
          $ mconcat
            [ "Start '",
              show s,
              "' and end '",
              show e,
              "' failed invariant 'start <= end'."
            ]
        failure
      else pure ()

    liftIO $ IORef.writeIORef prevEndRef (Just e)

  -- check original values in range
  let rangeMap = Map.fromList resultNats
  for_ nats $ \n -> do
    -- Each original n should either be a key (range start) or "within"
    -- some range i.e. there should range (s, e) with s <= n <= e.
    case Map.lookupLE n rangeMap of
      Nothing -> annotate (show n ++ " not found in results") *> failure
      Just (s, e) ->
        if
          | s <= n && e >= n -> pure ()
          | otherwise -> annotate ("bad range for: " ++ show n) *> failure
  where
    desc = "Satisfies properties"

parseResult :: Text -> Result String (List (Tuple2 Natural Natural))
parseResult "[]" = Ok []
parseResult xs =
  traverse parseElem
    . T.split (== ',')
    -- drop start '[' and end ']'
    . T.drop 1
    . T.dropEnd 1
    $ xs

parseElem :: Text -> Result String (Tuple2 Natural Natural)
parseElem t = case T.break (== '-') t of
  (start, mEnd) -> case T.uncons mEnd of
    Nothing -> (,) <$> parseNum start <*> parseNum start
    Just ('-', end) -> (,) <$> parseNum start <*> parseNum end
    Just (_, _) -> Err $ "Unexpected format: " ++ unpackText t

parseNum :: Text -> Result String Natural
parseNum t = case TR.readMaybe (unpackText t) of
  Just n -> Ok n
  Nothing -> Err $ "Could not read ℕ: " ++ (unpackText t)

genList :: Gen (List Natural)
genList = gnats <&> Set.toList . Set.fromList
  where
    gnats = G.list (R.linearFrom 0 0 20) gnat
    gnat = G.integral (R.linearFrom 0 0 20)
