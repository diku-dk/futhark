{-# LANGUAGE CPP #-}
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
import Futhark.Util (showText, trim)
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
  T.pack (showVersion version)
    <> unreleased
    <> "\n"
    <> gitversion $$tGitInfoCwdTry
    <> ghcversion
  where
    unreleased =
      if last (versionBranch version) == 0
        then " (prerelease - include info below when reporting bugs)"
        else mempty
    gitversion (Left _) =
      case commitIdFromFile of
        Nothing -> ""
        Just commit -> "git: " <> T.pack commit
    gitversion (Right gi) =
      mconcat
        [ "git: ",
          branch,
          T.pack (take 7 $ giHash gi),
          " (",
          T.pack (giCommitDate gi),
          ")",
          dirty,
          "\n"
        ]
      where
        branch
          | giBranch gi == "master" = ""
          | otherwise = T.pack (giBranch gi) <> " @ "
        dirty = if giDirty gi then " [modified]" else ""

    ghcversion = "Compiled with GHC " <> showText a <> "." <> showText b <> "." <> showText c <> ".\n"
      where
        a, b, c :: Int
        a = __GLASGOW_HASKELL__ `div` 100
        b = __GLASGOW_HASKELL__ `mod` 100
        c = __GLASGOW_HASKELL_PATCHLEVEL1__

commitIdFromFile :: Maybe String
commitIdFromFile = trim . BS.unpack <$> $(embedFileIfExists "./commit-id")
