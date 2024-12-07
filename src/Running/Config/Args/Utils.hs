module Running.Config.Args.Utils
  ( mkCommand,
    mkCommandDesc,
    mkCommandDescChunk,
    mkHelp,
  )
where

import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    Parser,
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help (Chunk, Doc)
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Running.Prelude

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
