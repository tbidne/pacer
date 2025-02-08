module Pacer.Exception
  ( -- * Exceptions
    ChartFileMissingE (..),
    CreateChartE (..),
    FileNotFoundE (..),
    GarminE (..),
    NpmE (..),
    TomlE (..),

    -- ** Commands
    CommandConvertE (..),
    CommandDeriveE (..),
    CommandScaleE (..),

    -- * Misc
    displayInnerMatchKnown,
    knownExceptions,
  )
where

import Control.Exception (SomeException (SomeException))
import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as Ex.Ann.Utils
import Data.List qualified as L
import FileSystem.OsPath (decodeLenient)
import FileSystem.UTF8 (decodeUtf8Lenient)
import GHC.TypeLits (symbolVal)
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import TOML (TOMLError)

-- | Exception for CLI scale command.
data CommandConvertE
  = -- | Wanted 1 quantity, received 0.
    CommandConvertArgs0
  | -- | Wanted 1 quantity, received 2.
    CommandConvertArgs2
  | -- | Error for converting pace with out unit in meters, not allowed.
    CommandConvertPaceMeters
  deriving stock (Show)

instance Exception CommandConvertE where
  displayException = \case
    CommandConvertArgs0 -> argsErr "0"
    CommandConvertArgs2 -> argsErr "2"
    CommandConvertPaceMeters -> symbolVal (Proxy @Utils.PaceMetersErrMsg)
    where
      argsErr i =
        "Convert requires exactly 1 quantity, received "
          ++ i
          ++ "."

-- | Exception for CLI derive command.
data CommandDeriveE
  = -- | Wanted 2 quantities, received 0.
    CommandDeriveArgs0
  | -- | Wanted 2 quantities, received 1.
    CommandDeriveArgs1
  | -- | Wanted 2 quantities, received 3.
    CommandDeriveArgs3
  | -- | Output units are not used with derive duration.
    CommandDeriveDurationUnit
  | -- | Deriving distance requires pace with units.
    CommandDeriveNoPaceUnit
  | -- | Error for deriving pace with out unit in meters, not allowed.
    CommandDerivePaceMeters
  deriving stock (Show)

instance Exception CommandDeriveE where
  displayException = \case
    CommandDeriveArgs0 -> argsErr "0"
    CommandDeriveArgs1 -> argsErr "1"
    CommandDeriveArgs3 -> argsErr "3"
    CommandDeriveDurationUnit -> "--unit is not used when deriving duration."
    CommandDeriveNoPaceUnit -> "Deriving distance requires that pace has units."
    CommandDerivePaceMeters -> symbolVal (Proxy @Utils.PaceMetersErrMsg)
    where
      argsErr i =
        "Derive requires exactly 2 quantities, received "
          ++ i
          ++ "."

-- | Exception for CLI scale command.
data CommandScaleE
  = -- | Wanted 1 quantity, received 0.
    CommandScaleArgs0
  | -- | Wanted 1 quantity, received 2.
    CommandScaleArgs2
  | -- | Wanted 1 quantity, received 3.
    CommandScaleArgs3
  | -- | Output units are not used with scale duration.
    CommandScaleDurationUnit
  | -- | Error for scaling pace with out unit in meters, not allowed.
    CommandScalePaceMeters
  | -- | Error for scaling pace with out unit when original unit not given,
    -- nonsensical. The text arg is the original pace without units, so we
    -- can produce an example of correct usage.
    CommandScalePaceUnitNoUnit Text
  deriving stock (Show)

instance Exception CommandScaleE where
  displayException = \case
    CommandScaleArgs0 -> argsErr "0"
    CommandScaleArgs2 -> argsErr "2"
    CommandScaleArgs3 -> argsErr "3"
    CommandScaleDurationUnit -> "--unit is not used when scaling duration."
    CommandScalePaceMeters -> symbolVal (Proxy @Utils.PaceMetersErrMsg)
    CommandScalePaceUnitNoUnit example ->
      mconcat
        [ "Scaling pace with --unit requires that the original units are ",
          "given e.g. --pace '",
          unpackText example,
          "'."
        ]
    where
      argsErr i =
        "Scale requires exactly 1 quantity, received "
          ++ i
          ++ "."

-- | Exception during chart creation.
newtype CreateChartE = CreateChartFilterEmpty Text
  deriving stock (Show)

instance Exception CreateChartE where
  displayException (CreateChartFilterEmpty t) =
    mconcat
      [ "Chart with title '",
        unpackText t,
        "' is empty due to all runs being filtered out."
      ]

-- | General exception for when a file at an expected path does not exist.
-- We would normally use IOException for this, except we want a custom type
-- so that we can ignore callstacks. We do not want to ignore IOException
-- since other errors may throw them, and those we want to know about.
newtype FileNotFoundE = MkFileNotFoundE OsPath
  deriving stock (Show)

instance Exception FileNotFoundE where
  displayException (MkFileNotFoundE p) =
    mconcat
      [ "File not found: ",
        decodeLenient p
      ]

-- | Exception for when we are expecting a required chart file to exist, but
-- none was found. This differs from 'FileNotFoundE' in that the latter is
-- caused by a failure to find an extant file at an exact location. This
-- exception, on the other hand, is caused by looking for a desired file in
-- several locations, but none was found.
data ChartFileMissingE = MkChartFileMissingE
  { cliDataDir :: Maybe OsPath,
    expectedFiles :: List (Path Rel File),
    tomlDataDir :: Maybe OsPath,
    xdgDir :: Path Abs Dir
  }
  deriving stock (Show)

instance Exception ChartFileMissingE where
  displayException e =
    mconcat
      [ "Required chart file not found. Searched for paths(s) ",
        Utils.showListF Utils.showPath e.expectedFiles,
        " in directories:",
        dirsStr
      ]
    where
      dirsStr =
        mconcat
          $ filter
            (not . L.null)
            [ displayDir e.cliDataDir,
              displayDir e.tomlDataDir,
              displayDir (Just $ pathToOsPath e.xdgDir)
            ]

      displayDir Nothing = ""
      displayDir (Just d) = "\n - " <> decodeLenient d

-- | Exception for toml errors that includes the path.
data TomlE = MkTomlE OsPath TOMLError
  deriving stock (Show)

instance Exception TomlE where
  displayException (MkTomlE p err) =
    mconcat
      [ "Error decoding toml file '",
        decodeLenient p,
        "': ",
        displayException err
      ]

data NpmE = MkNpmE OsPath (List String) Int LazyByteString
  deriving stock (Show)

instance Exception NpmE where
  displayException (MkNpmE exeName args i t) =
    mconcat
      [ "Command '",
        L.unwords (decodeLenient exeName : args),
        "' exited with error code ",
        show i,
        ": ",
        unpackText $ decodeUtf8Lenient $ toStrictBS t
      ]

-- | Garmin exceptions.
data GarminE
  = -- | Error decoding a garmin file.
    GarminDecode String
  | -- | Error for specifing garmin unit as meters.
    GarminMeters
  | -- | Generic garmin error.
    GarminOther String
  | -- | Error for unspecified garmin distance unit.
    GarminUnitRequired
  deriving stock (Eq, Show)

instance Exception GarminE where
  displayException = \case
    GarminDecode s -> "Error decoding garmin file: " ++ s
    GarminMeters -> "Meters are invalid for Garmin; use km or mi."
    GarminOther s -> "Garmin error: " ++ s
    GarminUnitRequired ->
      mconcat
        [ "The 'garmin.unit' settings is required in chart-requests.toml ",
          "when used with a garmin runs file."
        ]

displayInnerMatchKnown :: (Exception e) => e -> String
displayInnerMatchKnown e =
  if Ex.Ann.Utils.matchesException knownExceptions e
    then case toException e of
      SomeException innerEx -> displayException innerEx
    else displayException e

knownExceptions :: List ExceptionProxy
knownExceptions =
  [ MkExceptionProxy @ChartFileMissingE,
    MkExceptionProxy @CommandConvertE,
    MkExceptionProxy @CommandDeriveE,
    MkExceptionProxy @CommandScaleE,
    MkExceptionProxy @CreateChartE,
    MkExceptionProxy @FileNotFoundE,
    MkExceptionProxy @NpmE,
    MkExceptionProxy @TomlE
  ]
