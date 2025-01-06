module Pacer.Exception
  ( -- * Exceptions
    CreateChartE (..),
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
        L.intercalate " " (decodeLenient exeName : args),
        "' exited with error code ",
        show i,
        ": ",
        unpackText $ decodeUtf8Lenient $ toStrictByteString t
      ]

displayInnerMatchKnown :: (Exception e) => e -> String
displayInnerMatchKnown = Ex.Ann.Utils.displayInnerMatch knownExceptions

knownExceptions :: List ExceptionProxy
knownExceptions =
  [ MkExceptionProxy @CreateChartE Proxy,
    MkExceptionProxy @CommandConvertE Proxy,
    MkExceptionProxy @CommandDeriveE Proxy,
    MkExceptionProxy @CommandScaleE Proxy,
    MkExceptionProxy @NpmE Proxy,
    MkExceptionProxy @TomlE Proxy
  ]
