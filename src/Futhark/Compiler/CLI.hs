-- | Convenient common interface for command line Futhark compilers.
-- Using this module ensures that all compilers take the same options.
-- A small amount of flexibility is provided for backend-specific
-- options.
module Futhark.Compiler.CLI
       ( compilerMain
       , CompilerOption
       , CompilerMode(..)
       )
where

import Data.Maybe
import System.FilePath
import System.Console.GetOpt

import Futhark.Pipeline
import Futhark.Compiler
import Futhark.Representation.AST (Prog)
import Futhark.Representation.SOACS (SOACS)
import Futhark.Util.Options
import Language.Futhark.Futlib.Prelude

-- | Run a parameterised Futhark compiler, where @cfg@ is a user-given
-- configuration type.  Call this from @main@.
compilerMain :: cfg -- ^ Initial configuration.
             -> [CompilerOption cfg] -- ^ Options that affect the configuration.
             -> String -- ^ The short action name (e.g. "compile to C").
             -> String -- ^ The longer action description.
             -> Pipeline SOACS lore -- ^ The pipeline to use.
             -> (cfg -> CompilerMode -> FilePath -> Prog lore -> FutharkM ())
             -- ^ The action to take on the result of the pipeline.
             -> IO ()
compilerMain cfg cfg_opts name desc pipeline doIt =
  reportingIOErrors $
  mainWithOptions (newCompilerConfig cfg) (commandLineOptions ++ map wrapOption cfg_opts)
  inspectNonOptions
  where inspectNonOptions [file] config = Just $ compile config file
        inspectNonOptions _      _      = Nothing

        compile config filepath =
          runCompilerOnProgram (futharkConfig config) preludeBasis
          pipeline (action config filepath) filepath

        action config filepath =
          Action { actionName = name
                 , actionDescription = desc
                 , actionProcedure =
                   doIt (compilerConfig config) (compilerMode config) $
                   outputFilePath filepath config
                 }

-- | An option that modifies the configuration of type @cfg@.
type CompilerOption cfg = OptDescr (Either (IO ()) (cfg -> cfg))

type CoreCompilerOption cfg = OptDescr (Either
                                        (IO ())
                                        (CompilerConfig cfg -> CompilerConfig cfg))

commandLineOptions :: [CoreCompilerOption cfg]
commandLineOptions =
  [ Option "o" []
    (ReqArg (\filename -> Right $ \config -> config { compilerOutput = Just filename })
     "FILE")
    "Name of the compiled binary."
  , Option "v" ["verbose"]
    (OptArg (\file -> Right $ \config -> config { compilerVerbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["library"]
    (NoArg $ Right $ \config -> config { compilerMode = ToLibrary })
    "Generate a library instead of an executable."
  , Option [] ["executable"]
    (NoArg $ Right $ \config -> config { compilerMode = ToExecutable })
    "Generate an executable instead of a library (set by default)."
  ]

wrapOption :: CompilerOption cfg -> CoreCompilerOption cfg
wrapOption = fmap wrap
  where wrap f = do
          g <- f
          return $ \cfg -> cfg { compilerConfig = g (compilerConfig cfg) }

data CompilerConfig cfg =
  CompilerConfig { compilerOutput :: Maybe FilePath
                 , compilerVerbose :: Maybe (Maybe FilePath)
                 , compilerMode :: CompilerMode
                 , compilerConfig :: cfg
                 }

-- | Are we compiling a library or an executable?
data CompilerMode = ToLibrary | ToExecutable deriving (Eq, Ord, Show)

-- | The configuration of the compiler.
newCompilerConfig :: cfg -> CompilerConfig cfg
newCompilerConfig x = CompilerConfig { compilerOutput = Nothing
                                     , compilerVerbose = Nothing
                                     , compilerMode = ToExecutable
                                     , compilerConfig = x
                                     }

outputFilePath :: FilePath -> CompilerConfig cfg -> FilePath
outputFilePath srcfile =
  fromMaybe (srcfile `replaceExtension` "") . compilerOutput

futharkConfig :: CompilerConfig cfg -> FutharkConfig
futharkConfig config =
  newFutharkConfig { futharkVerbose = compilerVerbose config }
