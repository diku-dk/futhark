{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- | This module exports version information about the Futhark
-- compiler.
module Futhark.Version
  ( version,
    versionString,
  )
where

import Data.Version
import Development.GitRev
import qualified Paths_futhark

-- | The version of Futhark that we are using.  This is equivalent to
-- the version defined in the .cabal file.
version :: Version
version = Paths_futhark.version

-- | The version of Futhark that we are using, as a 'String'
versionString :: String
versionString =
  showVersion version
    ++ if used_hash /= "UNKNOWN"
      then "\n" ++ gitversion
      else ""
  where
    used_hash = take 7 $(gitHash)

    gitversion =
      concat
        [ "git: ",
          branch,
          used_hash,
          " (",
          $(gitCommitDate),
          ")",
          dirty
        ]
    branch
      | $(gitBranch) == "master" = ""
      | otherwise = $(gitBranch) ++ " @ "
    dirty = if $(gitDirtyTracked) then " [modified]" else ""
