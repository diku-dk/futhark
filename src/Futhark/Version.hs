{-# LANGUAGE TemplateHaskell #-}

-- | This module exports version information about the Futhark
-- compiler.
module Futhark.Version
  ( version,
    versionString,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.FileEmbed
import Data.Text qualified as T
import Data.Version
import Futhark.Util (trim)
import GitHash
import Paths_futhark qualified

{-# NOINLINE version #-}

-- | The version of Futhark that we are using.  This is equivalent to
-- the version defined in the .cabal file.
version :: Version
version = Paths_futhark.version

{-# NOINLINE versionString #-}

-- | The version of Futhark that we are using, in human-readable form.
versionString :: T.Text
versionString =
  T.pack (showVersion version) <> unreleased <> gitversion $$tGitInfoCwdTry
  where
    unreleased =
      if last (versionBranch version) == 0
        then " (prerelease - include info below when reporting bugs)"
        else mempty
    gitversion (Left _) =
      case commitIdFromFile of
        Nothing -> ""
        Just commit -> "\ngit: " <> T.pack commit
    gitversion (Right gi) =
      mconcat
        [ "\n",
          "git: ",
          branch,
          T.pack (take 7 $ giHash gi),
          " (",
          T.pack (giCommitDate gi),
          ")",
          dirty
        ]
      where
        branch
          | giBranch gi == "master" = ""
          | otherwise = T.pack (giBranch gi) <> " @ "
        dirty = if giDirty gi then " [modified]" else ""

commitIdFromFile :: Maybe String
commitIdFromFile = trim . BS.unpack <$> $(embedFileIfExists "./commit-id")
