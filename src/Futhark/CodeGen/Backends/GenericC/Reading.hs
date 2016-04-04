{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
-- | C code for reading Futhark values from standard input.  Put here in
-- order not to clutter the main code generation module with a huge
-- block of C.
module Futhark.CodeGen.Backends.GenericC.Reading
  ( readerFunctions
  ) where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C
import Data.FileEmbed

-- | An inclusion of the @rts/c/reading.h@ file.
readerFunctions :: [C.Definition]
readerFunctions =
  [C.cunit|$esc:src|]
  where src = $(embedStringFile "rts/c/reader.h")
