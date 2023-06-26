{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Futhark Prelude Library embedded embedded as strings read .
-- during compilation of the Futhark compiler.  The advantage is that
-- the prelude can be accessed without reading it from disk, thus
-- saving users from include path headaches.
module Language.Futhark.Prelude (prelude) where

import Data.FileEmbed
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Futhark.Util (toPOSIX)
import System.FilePath.Posix qualified as Posix

-- | Prelude embedded as 'T.Text' values, one for every file.
prelude :: [(Posix.FilePath, T.Text)]
prelude = map fixup prelude_bs
  where
    prelude_bs = $(embedDir "prelude")
    fixup (path, s) = ("/prelude" Posix.</> toPOSIX path, T.decodeUtf8 s)
