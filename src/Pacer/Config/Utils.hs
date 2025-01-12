module Pacer.Config.Utils
  ( -- * Custom types and parsers
    DistancePaceArgs (..),
    dpArgsParser,
    DistanceDurationPaceArgs (..),
    ddpArgsParser,
    PaceOptUnits,
    paceOptUnitsParserHelp,

    -- * Other Parsers
    distanceUnitParser,
    someDistanceParser,
    somePaceParser,

    -- * Readers
    readOsPath,
    readPath,
    readParseable,

    -- * Commands
    mkCommand,
    mkCommandDesc,
    mkCommandDescChunk,

    -- * Misc
    mkHelp,
  )
where

import FileSystem.OsPath qualified as OsPath
import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    Parser,
    ReadM,
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help (Chunk, Doc)
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Pacer.Class.Parser qualified as P
import Pacer.Data.Distance (DistanceUnit, SomeDistance)
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Pace (SomePace)
import Pacer.Prelude

-- | Represents the pace. For some conversions, the units are optional,
-- hence we only require the underlying Duration (a Pace is just a duration
-- with a unit, after all).
--
-- It is arguably simpler to require a unit for pace, and make the user
-- use --duration for when they don't want a unit (e.g. convert or scale).
-- So why do we allow no units? Consider deriving duration:
--
--     derive --distance '10 km' --pace 5m30s
--
-- Requring the user to type '/km' on the pace is redundant (hence annoying).
-- Futhermore, we cannot replace this with '--duration 5m30s', since that has
-- an entirely different meaning. Therefore it makes sense to have the units
-- optional here, so we might as well make the units optional wherever
-- sensible.
type PaceOptUnits a = Either (SomePace (Positive a)) (Seconds (Positive a))

paceOptUnitsParserHelp ::
  forall a.
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  String ->
  Parser (PaceOptUnits a)
paceOptUnitsParserHelp helpTxt =
  OA.option
    read
    ( mconcat
        [ OA.long "pace",
          OA.metavar "TIME_STR [/UNIT]",
          mkHelp helpTxt
        ]
    )
  where
    read :: ReadM (PaceOptUnits a)
    read = do
      s <- OA.str
      case P.parse s of
        Right duration -> pure $ Right duration
        Left err1 -> do
          case P.parse s of
            Right pace -> pure $ Left pace
            Left err2 ->
              fail
                $ mconcat
                  [ "Could not parse pace.\n* Error 1 (without unit):\n",
                    unpackText err1,
                    "\n* Error 2 (with unit):\n",
                    unpackText err2
                  ]

data DistancePaceArgs a = MkDistancePaceArgs
  { -- | Possible distance.
    mSomeDistance :: Maybe (SomeDistance (Positive a)),
    -- | Possible pace.
    mSomePace :: Maybe (SomePace (Positive a))
  }
  deriving stock (Eq, Show)

dpArgsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (DistancePaceArgs a)
dpArgsParser = do
  mSomeDistance <- OA.optional someDistanceParser
  mSomePace <- OA.optional somePaceParser

  pure
    $ MkDistancePaceArgs
      { mSomeDistance,
        mSomePace
      }

-- | Args for derive and scale. All args are optional, though we require
-- exactly 2 and 1, respectively.
data DistanceDurationPaceArgs a = MkDistanceDurationPaceArgs
  { -- | Possible duration.
    mDuration :: Maybe (Seconds (Positive a)),
    -- | Possible pace.
    mPaceOptUnits :: Maybe (PaceOptUnits a),
    -- | Possible distance.
    mSomeDistance :: Maybe (SomeDistance (Positive a))
  }
  deriving stock (Eq, Show)

ddpArgsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  String ->
  Parser (DistanceDurationPaceArgs a)
ddpArgsParser paceOptUnitsHelp = do
  mDuration <- OA.optional durationParser
  mPaceOptUnits <- OA.optional $ paceOptUnitsParserHelp paceOptUnitsHelp
  mSomeDistance <- OA.optional someDistanceParser

  pure
    $ MkDistanceDurationPaceArgs
      { mDuration,
        mPaceOptUnits,
        mSomeDistance
      }

distanceUnitParser :: Parser DistanceUnit
distanceUnitParser =
  OA.option
    readParseable
    ( mconcat
        [ OA.short 'u',
          OA.long "unit",
          OA.metavar "UNIT",
          mkHelp helpTxt
        ]
    )
  where
    helpTxt = "Output unit e.g. 'km', 'miles'."

durationParser ::
  forall a.
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Parser (Seconds (Positive a))
durationParser =
  OA.option
    readParseable
    ( mconcat
        [ OA.long "duration",
          OA.metavar "TIME_STR",
          mkHelp "A length of time e.g. '1h2m3s'."
        ]
    )

someDistanceParser ::
  forall a.
  ( AMonoid a,
    FromRational a,
    Ord a,
    P.Parser a,
    Show a
  ) =>
  Parser (SomeDistance (Positive a))
someDistanceParser =
  OA.option
    readParseable
    ( mconcat
        [ OA.long "distance",
          OA.metavar "DIST_STR",
          mkHelp "A distance with units e.g. '4 km'."
        ]
    )

somePaceParser ::
  forall a.
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Parser (SomePace (Positive a))
somePaceParser =
  OA.option
    readParseable
    ( mconcat
        [ OA.long "pace",
          OA.metavar "TIME_STR (/UNIT)",
          mkHelp helpTxt
        ]
    )
  where
    helpTxt = "A pace e.g. '4m30s /km', '1h5m /mi'."

readOsPath :: ReadM OsPath
readOsPath = OA.str >>= OsPath.encodeFail

readPath :: (Exception e) => (OsPath -> Either e (Path b t)) -> ReadM (Path b t)
readPath pathParser = do
  osPath <- readOsPath
  case pathParser osPath of
    Left ex ->
      fail
        $ mconcat
          [ "Failed making path '",
            OsPath.decodeLenient osPath,
            "' absolute: ",
            displayException ex
          ]
    Right x -> pure x

readParseable :: (P.Parser a) => ReadM a
readParseable =
  OA.str
    >>= ( P.parse >>> \case
            Right y -> pure y
            Left err -> fail $ unpackText err
        )

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

mkHelp :: String -> Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkCommandDesc :: String -> InfoMod a
mkCommandDesc = mkCommandDescChunk . Chunk.paragraph

mkCommandDescChunk :: Chunk Doc -> InfoMod a
mkCommandDescChunk =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
