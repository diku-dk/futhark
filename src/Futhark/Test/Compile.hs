-- | Compiling Futhark programs and running them as servers, as needed for
-- testing and benchmarking, but also by anything else that runs compiled
-- programs.
module Futhark.Test.Compile
  ( FutharkExe (..),
    binaryName,
    compileProgram,
    futharkServerCfg,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as SBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Futhark.Server
import Futhark.Util (isEnvVarAtLeast)
import System.Exit
import System.FilePath
import System.Process.ByteString (readProcessWithExitCode)

-- | The @futhark@ executable we are using.  This is merely a wrapper
-- around the underlying file path, because we will be using a lot of
-- different file paths here, and it is easy to mix them up.
newtype FutharkExe = FutharkExe FilePath
  deriving (Eq, Ord, Show)

-- | The name we use for compiled programs.
binaryName :: FilePath -> FilePath
binaryName = dropExtension

-- | @compileProgram extra_options futhark backend program@ compiles
-- @program@ with the command @futhark backend extra-options...@, and
-- returns stdout and stderr of the compiler.  Throws an IO exception
-- containing stderr if compilation fails.
compileProgram ::
  (MonadIO m, MonadError T.Text m) =>
  [String] ->
  FutharkExe ->
  String ->
  FilePath ->
  m (SBS.ByteString, SBS.ByteString)
compileProgram extra_options (FutharkExe futhark) backend program = do
  (futcode, stdout, stderr) <- liftIO $ readProcessWithExitCode futhark (backend : options) ""
  case futcode of
    ExitFailure 127 -> throwError $ progNotFound $ T.pack futhark
    ExitFailure _ -> throwError $ T.decodeUtf8 stderr
    ExitSuccess -> pure ()
  pure (stdout, stderr)
  where
    binOutputf = binaryName program
    options = [program, "-o", binOutputf] ++ extra_options
    progNotFound s = s <> ": command not found"

-- | Create a Futhark server configuration suitable for use when
-- testing/benchmarking Futhark programs.
futharkServerCfg :: FilePath -> [String] -> ServerCfg
futharkServerCfg prog opts =
  (newServerCfg prog opts)
    { cfgDebug = isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1
    }
