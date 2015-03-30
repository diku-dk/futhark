-- | This module exports version information about the Futhark
-- compiler.
module Futhark.Version
       (
         commit
       , version
       , versionString
       )
       where

import Data.Version

import qualified Paths_futhark
import qualified Build_futhark (gitCommit)

-- | The state of Git @HEAD@ when Futhark was built.
commit :: String
commit = Build_futhark.gitCommit

-- | The version of Futhark that we are using.  This is equivalent to
-- the version defined in the .cabal file.
version :: Version
version = Paths_futhark.version

-- | The version of Futhark that we are using.  This is equivalent to
-- the version defined in the .cabal file, but with the Git commit
-- hash appended.
versionString :: String
versionString = showVersion version ++ "-" ++ commit
