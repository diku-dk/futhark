-- | Configuration of compiler behaviour that is universal to all backends.
module Futhark.Compiler.Config
  ( FutharkConfig (..),
    newFutharkConfig,
    Verbosity (..),
  )
where

import Futhark.IR
import Futhark.Pipeline

-- | The compiler configuration.  This only contains options related
-- to core compiler functionality, such as reading the initial program
-- and running passes.  Options related to code generation are handled
-- elsewhere.
data FutharkConfig = FutharkConfig
  { futharkVerbose :: (Verbosity, Maybe FilePath),
    -- | Warn if True.
    futharkWarn :: Bool,
    -- | If true, error on any warnings.
    futharkWerror :: Bool,
    -- | If True, ignore @unsafe@.
    futharkSafe :: Bool,
    -- | Additional functions that should be exposed as entry points.
    futharkEntryPoints :: [Name],
    -- | If false, disable type-checking
    futharkTypeCheck :: Bool
  }

-- | The default compiler configuration.
newFutharkConfig :: FutharkConfig
newFutharkConfig =
  FutharkConfig
    { futharkVerbose = (NotVerbose, Nothing),
      futharkWarn = True,
      futharkWerror = False,
      futharkSafe = False,
      futharkEntryPoints = [],
      futharkTypeCheck = True
    }
