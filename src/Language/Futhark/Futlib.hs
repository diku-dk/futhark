{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | The Futhark basis library embedded embedded as strings read during
-- compilation of the Futhark compiler.  The advantage is that the
-- standard library can be accessed without reading it from disk, thus
-- saving users from include path headaches.
module Language.Futhark.Futlib (futlib, prelude) where

import Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.FilePath.Posix as Posix

import Futhark.Util (toPOSIX)

-- | Futlib embedded as 'T.Text' values, one for every file.
futlib :: [(Posix.FilePath, T.Text)]
futlib = map fixup futlib_bs
  where futlib_bs = $(embedDir "futlib")
        fixup (path, s) = ("/futlib" Posix.</> toPOSIX path, T.decodeUtf8 s)

-- The files intended to be implicitly imported into every Futhark
-- program.  Make sure it does not depend on anything too big to be
-- serialised efficiently.
prelude :: [String]
prelude = map ("/futlib/"++) ["prelude"]
