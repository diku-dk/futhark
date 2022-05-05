-- | Convenient common interface for command line Futhark compilers.
-- Using this module ensures that all compilers take the same options.
-- A small amount of flexibility is provided for backend-specific
-- options.
module Futhark.Compiler.CLI
  ( compilerMain,
    CompilerOption,
    CompilerMode (..),
    module Futhark.Pipeline,
    module Futhark.Compiler,
  )
where

import Control.Monad
import Data.Maybe
import Futhark.Compiler
import Futhark.IR (Name, Prog, nameFromString)
import Futhark.IR.SOACS (SOACS)
import Futhark.Pipeline
import Futhark.Util.Options
import System.FilePath

-- | Run a parameterised Futhark compiler, where @cfg@ is a user-given
-- configuration type.  Call this from @main@.
compilerMain ::
  -- | Initial configuration.
  cfg ->
  -- | Options that affect the configuration.
  [CompilerOption cfg] ->
  -- | The short action name (e.g. "compile to C").
  String ->
  -- | The longer action description.
  String ->
  -- | The pipeline to use.
  Pipeline SOACS rep ->
  -- | The action to take on the result of the pipeline.
  ( FutharkConfig ->
    cfg ->
    CompilerMode ->
    FilePath ->
    Prog rep ->
    FutharkM ()
  ) ->
  -- | Program name
  String ->
  -- | Command line arguments.
  [String] ->
  IO ()
compilerMain cfg cfg_opts name desc pipeline doIt =
  mainWithOptions
    (newCompilerConfig cfg)
    (commandLineOptions ++ map wrapOption cfg_opts)
    "options... <program.fut>"
    inspectNonOptions
  where
    inspectNonOptions [file] config = Just $ compile config file
    inspectNonOptions _ _ = Nothing

    compile config filepath =
      runCompilerOnProgram
        (futharkConfig config)
        pipeline
        (action config filepath)
        filepath

    action config filepath =
      Action
        { actionName = name,
          actionDescription = desc,
          actionProcedure =
            doIt
              (futharkConfig config)
              (compilerConfig config)
              (compilerMode config)
              (outputFilePath filepath config)
        }

-- | An option that modifies the configuration of type @cfg@.
type CompilerOption cfg = OptDescr (Either (IO ()) (cfg -> cfg))

type CoreCompilerOption cfg =
  OptDescr
    ( Either
        (IO ())
        (CompilerConfig cfg -> CompilerConfig cfg)
    )

commandLineOptions :: [CoreCompilerOption cfg]
commandLineOptions =
  [ Option
      "o"
      []
      ( ReqArg
          (\filename -> Right $ \config -> config {compilerOutput = Just filename})
          "FILE"
      )
      "Name of the compiled binary.",
    Option
      "v"
      ["verbose"]
      (OptArg (Right . incVerbosity) "FILE")
      "Print verbose output on standard error; wrong program to FILE.",
    Option
      []
      ["library"]
      (NoArg $ Right $ \config -> config {compilerMode = ToLibrary})
      "Generate a library instead of an executable.",
    Option
      []
      ["executable"]
      (NoArg $ Right $ \config -> config {compilerMode = ToExecutable})
      "Generate an executable instead of a library (set by default).",
    Option
      []
      ["server"]
      (NoArg $ Right $ \config -> config {compilerMode = ToServer})
      "Generate a server executable instead of a library.",
    Option
      "w"
      []
      (NoArg $ Right $ \config -> config {compilerWarn = False})
      "Disable all warnings.",
    Option
      []
      ["Werror"]
      (NoArg $ Right $ \config -> config {compilerWerror = True})
      "Treat warnings as errors.",
    Option
      []
      ["safe"]
      (NoArg $ Right $ \config -> config {compilerSafe = True})
      "Ignore 'unsafe' in code.",
    Option
      []
      ["entry-point"]
      ( ReqArg
          ( \arg -> Right $ \config ->
              config
                { compilerEntryPoints =
                    nameFromString arg : compilerEntryPoints config
                }
          )
          "NAME"
      )
      "Treat this function as an additional entry point."
  ]

wrapOption :: CompilerOption cfg -> CoreCompilerOption cfg
wrapOption = fmap wrap
  where
    wrap f = do
      g <- f
      pure $ \cfg -> cfg {compilerConfig = g (compilerConfig cfg)}

incVerbosity :: Maybe FilePath -> CompilerConfig cfg -> CompilerConfig cfg
incVerbosity file cfg =
  cfg {compilerVerbose = (v, file `mplus` snd (compilerVerbose cfg))}
  where
    v = case fst $ compilerVerbose cfg of
      NotVerbose -> Verbose
      Verbose -> VeryVerbose
      VeryVerbose -> VeryVerbose

data CompilerConfig cfg = CompilerConfig
  { compilerOutput :: Maybe FilePath,
    compilerVerbose :: (Verbosity, Maybe FilePath),
    compilerMode :: CompilerMode,
    compilerWerror :: Bool,
    compilerSafe :: Bool,
    compilerWarn :: Bool,
    compilerConfig :: cfg,
    compilerEntryPoints :: [Name]
  }

-- | The configuration of the compiler.
newCompilerConfig :: cfg -> CompilerConfig cfg
newCompilerConfig x =
  CompilerConfig
    { compilerOutput = Nothing,
      compilerVerbose = (NotVerbose, Nothing),
      compilerMode = ToExecutable,
      compilerWerror = False,
      compilerSafe = False,
      compilerWarn = True,
      compilerConfig = x,
      compilerEntryPoints = mempty
    }

outputFilePath :: FilePath -> CompilerConfig cfg -> FilePath
outputFilePath srcfile =
  fromMaybe (srcfile `replaceExtension` "") . compilerOutput

futharkConfig :: CompilerConfig cfg -> FutharkConfig
futharkConfig config =
  newFutharkConfig
    { futharkVerbose = compilerVerbose config,
      futharkWerror = compilerWerror config,
      futharkSafe = compilerSafe config,
      futharkWarn = compilerWarn config,
      futharkEntryPoints = compilerEntryPoints config
    }
