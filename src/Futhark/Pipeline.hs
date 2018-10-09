{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, OverloadedStrings #-}
module Futhark.Pipeline
       ( Pipeline
       , PipelineConfig (..)
       , Action (..)

       , FutharkM
       , runFutharkM
       , Verbosity(..)

       , internalErrorS

       , module Futhark.Error

       , onePass
       , passes
       , runPasses
       , runPipeline
       )
       where

import Control.Category
import Control.Monad
import Control.Monad.Writer.Strict hiding (pass)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import System.IO
import Text.Printf

import Prelude hiding (id, (.))

import Futhark.Error
import Futhark.Representation.AST (Prog, PrettyLore)
import Futhark.TypeCheck
import Futhark.Pass
import Futhark.Util.Log
import Futhark.Util.Pretty (Pretty, prettyText)
import Futhark.MonadFreshNames

-- | If Verbose, print log messages to standard error.  If
-- VeryVerbose, also print logs from individual passes.
data Verbosity = NotVerbose | Verbose | VeryVerbose deriving (Eq, Ord)

newtype FutharkEnv = FutharkEnv { futharkVerbose :: Verbosity }

data FutharkState = FutharkState { futharkPrevLog :: UTCTime
                                 , futharkNameSource :: VNameSource }

newtype FutharkM a = FutharkM (ExceptT CompilerError (StateT FutharkState (ReaderT FutharkEnv IO)) a)
                     deriving (Applicative, Functor, Monad,
                               MonadError CompilerError,
                               MonadState FutharkState,
                               MonadReader FutharkEnv,
                               MonadIO)

instance MonadFreshNames FutharkM where
  getNameSource = gets futharkNameSource
  putNameSource src = modify $ \s -> s { futharkNameSource = src }

instance MonadLogger FutharkM where
  addLog = mapM_ perLine . T.lines . toText
    where perLine msg = do
            verb <- asks $ (>=Verbose) . futharkVerbose
            prev <- gets futharkPrevLog
            now <- liftIO getCurrentTime
            let delta :: Double
                delta = fromRational $ toRational (now `diffUTCTime` prev)
                prefix = printf "[  +%.6f] " delta
            modify $ \s -> s { futharkPrevLog = now }
            when verb $ liftIO $ T.hPutStrLn stderr $ T.pack prefix <> msg

runFutharkM :: FutharkM a -> Verbosity -> IO (Either CompilerError a)
runFutharkM (FutharkM m) verbose = do
  s <- FutharkState <$> getCurrentTime <*> pure blankNameSource
  runReaderT (evalStateT (runExceptT m) s) newEnv
  where newEnv = FutharkEnv verbose

internalErrorS :: Pretty t => String -> t -> FutharkM a
internalErrorS s p = throwError $ InternalError (T.pack s) (prettyText p) CompilerBug

data Action lore =
  Action { actionName :: String
         , actionDescription :: String
         , actionProcedure :: Prog lore -> FutharkM ()
         }

data PipelineConfig =
  PipelineConfig { pipelineVerbose :: Bool
                 , pipelineValidate :: Bool
                 }

newtype Pipeline fromlore tolore =
  Pipeline { unPipeline :: PipelineConfig -> Prog fromlore -> FutharkM (Prog tolore) }

instance Category Pipeline where
  id = Pipeline $ const return
  p2 . p1 = Pipeline perform
    where perform cfg prog =
            runPasses p2 cfg =<< runPasses p1 cfg prog

runPasses :: Pipeline fromlore tolore
          -> PipelineConfig
          -> Prog fromlore
          -> FutharkM (Prog tolore)
runPasses = unPipeline

runPipeline :: Pipeline fromlore tolore
            -> PipelineConfig
            -> Prog fromlore
            -> Action tolore
            -> FutharkM ()
runPipeline p cfg prog a = do
  prog' <- runPasses p cfg prog
  when (pipelineVerbose cfg) $ logMsg $
    "Running action " <> T.pack (actionName a)
  actionProcedure a prog'

onePass :: (Checkable fromlore, Checkable tolore) =>
           Pass fromlore tolore -> Pipeline fromlore tolore
onePass pass = Pipeline perform
  where perform cfg prog = do
          when (pipelineVerbose cfg) $ logMsg $
            "Running pass " <> T.pack (passName pass)
          prog' <- runPass pass prog
          when (pipelineValidate cfg) $
            case checkProg prog' of
              Left err -> validationError pass prog' $ show err
              Right () -> return ()
          return prog'

passes :: Checkable lore =>
          [Pass lore lore] -> Pipeline lore lore
passes = foldl (>>>) id . map onePass

validationError :: PrettyLore tolore =>
                   Pass fromlore tolore -> Prog tolore -> String -> FutharkM a
validationError pass prog err =
  throwError $ InternalError msg (prettyText prog) CompilerBug
  where msg = "Type error after pass '" <> T.pack (passName pass) <> "':\n" <> T.pack err

runPass :: PrettyLore fromlore =>
           Pass fromlore tolore
        -> Prog fromlore
        -> FutharkM (Prog tolore)
runPass pass prog = do
  (res, logged) <- runPassM (passFunction pass prog)
  verb <- asks $ (>=VeryVerbose) . futharkVerbose
  when verb $ addLog logged
  case res of Left err -> internalError err $ prettyText prog
              Right x  -> return x
