module Pacer.Exception
  ( -- * Exceptions
    CreateChartE (..),
    TomlE (..),

    -- ** Commands
    CommandDeriveE (..),
    CommandScaleE (..),

    -- * Misc
    displayInnerMatchKnown,
    knownExceptions,
  )
where

import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as Ex.Ann.Utils
import FileSystem.OsPath (decodeLenient)
import Pacer.Prelude
import TOML (TOMLError)

-- | Exception for CLI derive command.
data CommandDeriveE
  = CommandDeriveArgs0
  | CommandDeriveArgs1
  | CommandDeriveArgs3
  | CommandDeriveNoPaceUnit
  deriving stock (Show)

instance Exception CommandDeriveE where
  displayException = \case
    CommandDeriveArgs0 -> argsErr "0"
    CommandDeriveArgs1 -> argsErr "1"
    CommandDeriveArgs3 -> argsErr "3"
    CommandDeriveNoPaceUnit -> "Deriving distance requires that pace has units."
    where
      argsErr i =
        "Derive requires exactly 2 quantities, received "
          ++ i
          ++ "."

-- | Exception for CLI scale command.
data CommandScaleE
  = CommandScaleArgs0
  | CommandScaleArgs2
  | CommandScaleArgs3
  deriving stock (Show)

instance Exception CommandScaleE where
  displayException = \case
    CommandScaleArgs0 -> argsErr "0"
    CommandScaleArgs2 -> argsErr "2"
    CommandScaleArgs3 -> argsErr "3"
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

displayInnerMatchKnown :: (Exception e) => e -> String
displayInnerMatchKnown = Ex.Ann.Utils.displayInnerMatch knownExceptions

knownExceptions :: List ExceptionProxy
knownExceptions =
  [ MkExceptionProxy @CreateChartE Proxy,
    MkExceptionProxy @CommandDeriveE Proxy,
    MkExceptionProxy @CommandScaleE Proxy,
    MkExceptionProxy @TomlE Proxy
  ]
