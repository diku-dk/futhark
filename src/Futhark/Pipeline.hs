{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, OverloadedStrings #-}
module Futhark.Pipeline
       ( CompileError (..)
       , Pipeline
       , PipelineConfig (..)
       , Action (..)

       , FutharkM
       , runFutharkM
       , compileError
       , compileErrorS
       , compileFail

       , onePass
       , passes
       , runPasses
       , runPipeline
       )
       where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Writer.Strict hiding (pass)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

import Prelude hiding (id, (.))

import Futhark.Representation.AST (Prog, pretty, PrettyLore)
import Futhark.TypeCheck
import Futhark.Pass
import Futhark.Util.Log
import Futhark.MonadFreshNames
import qualified Futhark.Util.Pretty as PP

data CompileError = CompileError {
    errorDesc :: T.Text
  , errorData :: T.Text
  }

newtype FutharkEnv = FutharkEnv {
  futharkVerbose :: Bool
  -- ^ If true, print log messages to standard error.
  }

newtype FutharkM a = FutharkM (ExceptT CompileError (StateT VNameSource (ReaderT FutharkEnv IO)) a)
                     deriving (Applicative, Functor, Monad,
                               MonadError CompileError,
                               MonadState VNameSource,
                               MonadReader FutharkEnv,
                               MonadIO)

instance MonadFreshNames FutharkM where
  getNameSource = get
  putNameSource = put

instance MonadLogger FutharkM where
  addLog msg = do verb <- asks futharkVerbose
                  when verb $ liftIO $ T.hPutStr stderr $ toText msg

runFutharkM :: FutharkM a -> Bool -> IO (Either CompileError a)
runFutharkM (FutharkM m) verbose =
  runReaderT (evalStateT (runExceptT m) blankNameSource) newEnv
  where newEnv = FutharkEnv verbose

compileError :: (MonadError CompileError m, PP.Pretty err) =>
                T.Text -> err -> m a
compileError s e = throwError $ CompileError s $ T.pack $ pretty e

compileErrorS :: (MonadError CompileError m) =>
                 T.Text -> String -> m a
compileErrorS s e = throwError $ CompileError s $ T.pack e

compileFail :: String -> FutharkM a
compileFail s = compileErrorS (T.pack s) "<nothing>"

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

onePass :: Checkable tolore =>
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
  compileError msg prog
  where msg = "Type error after pass '" <> T.pack (passName pass) <> "':\n" <> T.pack err

runPass :: Pass fromlore tolore
        -> Prog fromlore
        -> FutharkM (Prog tolore)
runPass pass prog = do
  (res, logged) <- runPassM (passFunction pass prog)
  addLog logged
  case res of Left err -> compileError err ()
              Right x  -> return x
