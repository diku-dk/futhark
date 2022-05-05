-- | Configuration of compiler behaviour that is universal to all backends.
module Futhark.Compiler.Config
  ( FutharkConfig (..),
    newFutharkConfig,
    Verbosity (..),
    CompilerMode (..),
  )
where

import Futhark.IR.Syntax.Core (Name)

-- | Are we compiling a library or an executable?
data CompilerMode
  = ToLibrary
  | ToExecutable
  | ToServer
  deriving (Eq, Ord, Show)

-- | How much information to print to stderr while the compiler is running.
data Verbosity
  = -- | Silence is golden.
    NotVerbose
  | -- | Print messages about which pass is running.
    Verbose
  | -- | Also print logs from individual passes.
    VeryVerbose
  deriving (Eq, Ord)

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
