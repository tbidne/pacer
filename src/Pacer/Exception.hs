{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Exception
  ( -- * Exceptions
    ChartFileMissingE (..),
    CreateChartE (..),
    GarminE (..),

    -- ** Commands
    CommandConvertE (..),
    CommandDeriveE (..),
    CommandScaleE (..),

    -- * Misc
    ShowException (..),
  )
where

import Data.Foldable qualified as F
import Data.List qualified as L
import Data.Set (Set)
import FileSystem.OsPath (decodeLenient)
import GHC.TypeLits (symbolVal)
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import Pacer.Utils.Show qualified as Utils.Show

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
        "' is empty due to all activities being filtered out."
      ]

-- | Exception for when we are expecting a required chart file to exist, but
-- none was found. This differs from 'FileNotFoundE' in that the latter is
-- caused by a failure to find an extant file at an exact location. This
-- exception, on the other hand, is caused by looking for a desired file in
-- several locations, but none was found.
data ChartFileMissingE = MkChartFileMissingE
  { cliDataDir :: Maybe OsPath,
    configDataDir :: Maybe OsPath,
    currentDir :: Path Abs Dir,
    expectedFiles :: List (Path Rel File),
    expectedExts :: Set OsPath,
    xdgDir :: Path Abs Dir
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''ChartFileMissingE

instance Exception ChartFileMissingE where
  displayException e =
    mconcat
      [ "Required chart file not found. Searched for paths(s) ",
        Utils.Show.showMapListInline Utils.Show.showPath expectedFiles,
        extsStr,
        " in directories:",
        dirsStr
      ]
    where
      expectedExts = e ^. #expectedExts
      expectedFiles = e ^. #expectedFiles

      dirsStr =
        mconcat
          $ filter
            (not . L.null)
            [ displayDir $ e ^. #cliDataDir,
              displayDir $ e ^. #configDataDir,
              displayDir (Just $ toOsPath $ e ^. #currentDir),
              displayDir (Just $ toOsPath $ e ^. #xdgDir)
            ]

      extsStr
        | F.null expectedExts = ""
        | otherwise =
            mconcat
              [ " with extension(s) ",
                Utils.Show.showMapListInline Utils.Show.showOsPath expectedExts
              ]

      displayDir Nothing = ""
      displayDir (Just d) = "\n - " <> decodeLenient d

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
        [ "The 'garmin.unit' setting is required in chart-requests json ",
          "when used with a garmin activities file."
        ]

-- | Morally equivalent to StringException, except its Show instance
-- ignores the constructor, which is then derived for displayException.
--
-- Used for when some library unfortunately renders exceptions via
-- show instance of displayException (servant).
newtype ShowException = MkShowException String
  deriving anyclass (Exception)

instance Show ShowException where
  show (MkShowException s) = s
