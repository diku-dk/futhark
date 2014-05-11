-- | This module exports version information about the Futhark
-- compiler.
module Futhark.Version
       (
         commit
       , version
       )
       where

import Data.Version

import qualified Paths_futhark
import qualified Build_futhark (gitCommit)

-- | The state of Git @HEAD@ when Futhark was built.
commit :: String
commit = Build_futhark.gitCommit

-- | The version of Futhark that we are using.  This is equivalent to
-- the version defined in the .cabal file, except that the specific
-- Git revision ('commit') is added as a version tag.
version :: Version
version = Paths_futhark.version
          { versionTags = versionTags Paths_futhark.version ++ [commit]
          }
