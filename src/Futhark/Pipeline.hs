{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- | Definition of the core compiler driver building blocks.  The
-- spine of the compiler is the 'FutharkM' monad, although note that
-- individual passes are pure functions, and do not use the 'FutharkM'
-- monad (see "Futhark.Pass").
--
-- Running the compiler involves producing an initial IR program (see
-- "Futhark.Compiler.Program"), running a 'Pipeline' to produce a
-- final program (still in IR), then running an 'Action', which is
-- usually a code generator.
module Futhark.Pipeline
  ( Pipeline,
    PipelineConfig (..),
    Action (..),
    FutharkM,
    runFutharkM,
    Verbosity (..),
    module Futhark.Error,
    onePass,
    passes,
    condPipeline,
    runPipeline,
  )
where

import Control.Category
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Strict hiding (pass)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Compiler.Config (Verbosity (..))
import Futhark.Error
import Futhark.IR (PrettyRep, Prog)
import Futhark.IR.TypeCheck
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Util.Log
import Futhark.Util.Pretty (prettyText)
import System.IO
import Text.Printf
import Prelude hiding (id, (.))

newtype FutharkEnv = FutharkEnv {futharkVerbose :: Verbosity}

data FutharkState = FutharkState
  { futharkPrevLog :: UTCTime,
    futharkNameSource :: VNameSource
  }

-- | The main Futhark compiler driver monad - basically some state
-- tracking on top if 'IO'.
newtype FutharkM a = FutharkM (ExceptT CompilerError (StateT FutharkState (ReaderT FutharkEnv IO)) a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadError CompilerError,
      MonadState FutharkState,
      MonadReader FutharkEnv,
      MonadIO
    )

instance MonadFreshNames FutharkM where
  getNameSource = gets futharkNameSource
  putNameSource src = modify $ \s -> s {futharkNameSource = src}

instance MonadLogger FutharkM where
  addLog = mapM_ perLine . T.lines . toText
    where
      perLine msg = do
        verb <- asks $ (>= Verbose) . futharkVerbose
        prev <- gets futharkPrevLog
        now <- liftIO getCurrentTime
        let delta :: Double
            delta = fromRational $ toRational (now `diffUTCTime` prev)
            prefix = printf "[  +%.6f] " delta
        modify $ \s -> s {futharkPrevLog = now}
        when verb $ liftIO $ T.hPutStrLn stderr $ T.pack prefix <> msg

-- | Run a 'FutharkM' action.
runFutharkM :: FutharkM a -> Verbosity -> IO (Either CompilerError a)
runFutharkM (FutharkM m) verbose = do
  s <- FutharkState <$> getCurrentTime <*> pure blankNameSource
  runReaderT (evalStateT (runExceptT m) s) newEnv
  where
    newEnv = FutharkEnv verbose

-- | A compilation always ends with some kind of action.
data Action rep = Action
  { actionName :: String,
    actionDescription :: String,
    actionProcedure :: Prog rep -> FutharkM ()
  }

-- | Configuration object for running a compiler pipeline.
data PipelineConfig = PipelineConfig
  { pipelineVerbose :: Bool,
    pipelineValidate :: Bool
  }

-- | A compiler pipeline is conceptually a function from programs to
-- programs, where the actual representation may change.  Pipelines
-- can be composed using their 'Category' instance.
newtype Pipeline fromrep torep = Pipeline {unPipeline :: PipelineConfig -> Prog fromrep -> FutharkM (Prog torep)}

instance Category Pipeline where
  id = Pipeline $ const pure
  p2 . p1 = Pipeline perform
    where
      perform cfg prog =
        runPipeline p2 cfg =<< runPipeline p1 cfg prog

-- | Run the pipeline on the given program.
runPipeline ::
  Pipeline fromrep torep ->
  PipelineConfig ->
  Prog fromrep ->
  FutharkM (Prog torep)
runPipeline = unPipeline

-- | Construct a pipeline from a single compiler pass.
onePass ::
  Checkable torep =>
  Pass fromrep torep ->
  Pipeline fromrep torep
onePass pass = Pipeline perform
  where
    perform cfg prog = do
      when (pipelineVerbose cfg) $
        logMsg $
          "Running pass " <> T.pack (passName pass)
      prog' <- runPass pass prog
      let prog'' = Alias.aliasAnalysis prog'
      when (pipelineValidate cfg) $
        case checkProg prog'' of
          Left err -> validationError pass prog'' $ show err
          Right () -> pure ()
      pure prog'

-- | Conditionally run pipeline if predicate is true.
condPipeline ::
  (Prog rep -> Bool) -> Pipeline rep rep -> Pipeline rep rep
condPipeline cond (Pipeline f) =
  Pipeline $ \cfg prog ->
    if cond prog
      then f cfg prog
      else pure prog

-- | Create a pipeline from a list of passes.
passes ::
  Checkable rep =>
  [Pass rep rep] ->
  Pipeline rep rep
passes = foldl (>>>) id . map onePass

validationError ::
  PrettyRep rep =>
  Pass fromrep torep ->
  Prog rep ->
  String ->
  FutharkM a
validationError pass prog err =
  throwError $ InternalError msg (prettyText prog) CompilerBug
  where
    msg = "Type error after pass '" <> T.pack (passName pass) <> "':\n" <> T.pack err

runPass ::
  Pass fromrep torep ->
  Prog fromrep ->
  FutharkM (Prog torep)
runPass pass prog = do
  (prog', logged) <- runPassM (passFunction pass prog)
  verb <- asks $ (>= VeryVerbose) . futharkVerbose
  when verb $ addLog logged
  pure prog'
