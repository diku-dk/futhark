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
    FutharkState (..),
    module Futhark.Error,
    onePass,
    passes,
    condPipeline,
    runPipeline,
  )
where

import Control.Category
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Parallel
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Compiler.Config (Verbosity (..))
import Futhark.Error
import Futhark.IR (PrettyRep, Prog)
import Futhark.IR.TypeCheck
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Util.Log
import Futhark.Util.Pretty
import System.IO
import Text.Printf
import Prelude hiding (id, (.))

newtype FutharkEnv = FutharkEnv {futharkVerbose :: Verbosity}

-- | The overall compiler state.
data FutharkState = FutharkState
  { futharkPrevLog :: UTCTime,
    futharkNameSource :: VNameSource
  }

instance Pretty FutharkState where
  pretty (FutharkState _ (VNameSource counter)) =
    "name_source" <+> braces (pretty counter)

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
            prefix = printf "[  +%7.3f] " delta
        modify $ \s -> s {futharkPrevLog = now}
        when verb $ liftIO $ T.hPutStrLn stderr $ T.pack prefix <> msg

runFutharkM' ::
  FutharkM a ->
  FutharkState ->
  FutharkEnv ->
  IO (Either CompilerError a, FutharkState)
runFutharkM' (FutharkM m) s =
  runReaderT (runStateT (runExceptT m) s)

-- | Run a 'FutharkM' action.
runFutharkM :: FutharkM a -> Verbosity -> IO (Either CompilerError a)
runFutharkM m verbose = do
  s <- FutharkState <$> getCurrentTime <*> pure blankNameSource
  fst <$> runFutharkM' m s (FutharkEnv verbose)

catchIO :: FutharkM a -> (SomeException -> FutharkM a) -> FutharkM a
catchIO m f = FutharkM $ do
  s <- get
  env <- ask
  (x, s') <-
    liftIO $
      runFutharkM' m s env `catch` \e ->
        runFutharkM' (f e) s env
  put s'
  case x of
    Left e -> throwError e
    Right x' -> pure x'

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
newtype Pipeline fromrep torep = Pipeline
  { unPipeline ::
      forall a.
      PipelineConfig ->
      Prog fromrep ->
      FutharkM ((Prog torep -> FutharkM a) -> FutharkM a)
  }

instance Category Pipeline where
  id = Pipeline $ \_ prog -> pure $ \c -> c prog
  p2 . p1 = Pipeline perform
    where
      perform cfg prog = do
        rc <- unPipeline p1 cfg prog
        rc $ unPipeline p2 cfg

-- | Run the pipeline on the given program.
runPipeline ::
  Pipeline fromrep torep ->
  PipelineConfig ->
  Prog fromrep ->
  FutharkM (Prog torep)
runPipeline p cfg prog = do
  rc <- unPipeline p cfg prog
  rc pure

-- | Construct a pipeline from a single compiler pass.
onePass ::
  (Checkable torep) =>
  Pass fromrep torep ->
  Pipeline fromrep torep
onePass pass = Pipeline perform
  where
    perform cfg prog = do
      when (pipelineVerbose cfg) . logMsg $
        "Running pass: " <> T.pack (passName pass)
      prog' <- runPass pass prog
      -- Spark validation in a separate task and speculatively execute
      -- next pass.  If the next pass throws an exception, we better
      -- be ready to catch it and check if it might be because the
      -- program was actually ill-typed.
      let check =
            if pipelineValidate cfg
              then validate prog'
              else Right ()
      par check $ pure $ \c ->
        (errorOnError check pure =<< c prog')
          `catchIO` errorOnError check (liftIO . throwIO)
    validate prog =
      let prog' = Alias.aliasAnalysis prog
       in case checkProg prog' of
            Left err -> Left (prog', err)
            Right () -> Right ()
    errorOnError (Left (prog, err)) _ _ =
      validationError pass prog $ show err
    errorOnError _ c x = c x

-- | Conditionally run pipeline if predicate is true.
condPipeline ::
  (Prog rep -> Bool) -> Pipeline rep rep -> Pipeline rep rep
condPipeline cond (Pipeline f) =
  Pipeline $ \cfg prog ->
    if cond prog
      then f cfg prog
      else pure $ \c -> c prog

-- | Create a pipeline from a list of passes.
passes ::
  (Checkable rep) =>
  [Pass rep rep] ->
  Pipeline rep rep
passes = foldl (>>>) id . map onePass

validationError ::
  (PrettyRep rep) =>
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
