{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
-- | Futhark compiler internal pipeline.
module Futhark.Pipeline
  (
    runPasses
  , FutharkM
  , runFutharkM
  , PipelineState (..)
  , Pass (..)
  , canFail
  , polyPass
  , basicPass
  , unfailableBasicPass
  , basicProg
  , explicitMemoryPass
  , unfailableExplicitMemoryPass
  , explicitMemoryProg
  , CompileError(..)
  , compileError
  , Action
  , actionDescription
  , applyAction
  , polyAction
  , basicAction
  , explicitMemoryAction
  , FutharkConfig(..)
  , module Language.Futhark.Parser.RealConfiguration
  , verbose
  )

where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Writer.Strict hiding (pass)
import Control.Monad.Except
import Data.Maybe (isJust)

import Prelude

import qualified Text.PrettyPrint.Mainland as PP

import Futhark.Representation.AST (Prog)
import qualified Futhark.Representation.Basic as Basic
import qualified Futhark.Representation.ExplicitMemory as ExplicitMemory
import Futhark.TypeCheck
import Futhark.Analysis.Alias
import Language.Futhark.Parser.RealConfiguration

runPasses :: FutharkConfig -> PipelineState -> FutharkM PipelineState
runPasses config startprog =
  foldM (applyPass config) startprog $ futharkPipeline config

applyPass :: FutharkConfig -> PipelineState -> Pass
          -> FutharkM PipelineState
applyPass config prog pass = do
  when (verbose config) $
    tell $ "Running " ++ passName pass ++ ".\n"
  runPass config pass prog

validationError :: Pass -> PipelineState -> String -> FutharkM a
validationError pass s err =
  compileError msg $ Just s
  where msg = "Type error after pass '" ++ passName pass ++ "':\n" ++ err

validateState :: FutharkConfig -> Pass -> PipelineState -> FutharkM PipelineState
validateState config pass (Basic prog) =
  case typeCheck config prog of
    Left err -> validationError pass (Basic prog) $ show err
    Right {} -> return $ Basic prog
validateState config pass (ExplicitMemory prog) =
  case typeCheck config prog of
    Left err -> validationError pass (ExplicitMemory prog) $ show err
    Right {} -> return $ ExplicitMemory prog

data CompileError = CompileError {
    errorDesc :: String
  , errorState :: Maybe PipelineState
  }

type PassM = ExceptT CompileError (Writer String)

runPassM :: PassM a -> (String, Either CompileError a)
runPassM m = case runWriter $ runExceptT m of
  (res, w) -> (w, res)

newtype FutharkM a = FutharkM (ExceptT CompileError (WriterT String IO) a)
                     deriving (Applicative, Functor, Monad,
                               MonadWriter String,
                               MonadError CompileError,
                               MonadIO)

runFutharkM :: FutharkM a -> IO (Either CompileError a, String)
runFutharkM (FutharkM m) = runWriterT $ runExceptT m

runPass :: FutharkConfig -> Pass -> PipelineState -> FutharkM PipelineState
runPass config pass state = do
  (w,res) <- liftIO $ handle onPassException $
             let res = runPassM $ passOp pass state
             in forcePassRes res `seq` case res of
               (w, Left err) ->
                 return (w, Left ("Error during pass '" ++
                                  passName pass ++
                                  "':\n" ++ errorDesc err))
               (w, Right state') ->
                 return (w, Right state')
  tell w
  case res of Left err    -> compileError err (Just state)
              Right prog' -> validateState config pass prog'

  where onPassException err =
          return ("", Left $ "IO exception during pass '" ++
                      passName pass ++ "':\n" ++
                      show (err :: SomeException))

        -- XXX: For lack of a deepseq instance for Futhark
        -- programs, we prettyprint the AST to force out IO
        -- exceptions.
        forcePassRes
          | isJust $ futharkVerbose config = either (const "") Basic.pretty . snd
          | otherwise                      = const ""


data PipelineState = Basic Basic.Prog
                   | ExplicitMemory ExplicitMemory.Prog

instance PP.Pretty PipelineState where
  ppr (Basic prog)          = PP.ppr $ aliasAnalysis prog
  ppr (ExplicitMemory prog) = PP.ppr $ aliasAnalysis prog

data Pass = Pass {
    passName :: String
  , passOp :: PipelineState -> PassM PipelineState
  }

data Action = Action String (PipelineState -> IO ())

actionDescription :: Action -> String
actionDescription (Action name _) = name

applyAction :: Action -> PipelineState -> IO ()
applyAction (Action _ f) = f

data FutharkConfig = FutharkConfig {
    futharkPipeline :: [Pass]
  , futharkCheckAliases :: Bool
  , futharkVerbose :: Maybe (Maybe FilePath)
  , futharkBoundsCheck :: Bool
  , futharkRealConfiguration :: RealConfiguration
}

verbose :: FutharkConfig -> Bool
verbose = isJust . futharkVerbose

compileError :: MonadError CompileError m =>
                String -> Maybe PipelineState -> m a
compileError s p = throwError $ CompileError s p

typeCheck :: Checkable lore => FutharkConfig -> Prog lore
          -> Either (TypeError lore) (Prog lore)
typeCheck config prog = either Left (const $ Right prog) $ check prog
  where check | futharkCheckAliases config = checkProg
              | otherwise                  = checkProgNoUniqueness

canFail :: Show err =>
           String -> Maybe PipelineState -> Either err a -> PassM a
canFail d p (Left err) = compileError (d ++ show err) p
canFail _ _ (Right v)  = return v

basicProg :: PipelineState -> PassM Basic.Prog
basicProg (Basic p) = return p
basicProg s         = compileError "Expected basic representation." $ Just s

basicPass :: Show err =>
             String -> (Basic.Prog -> Either err Basic.Prog)
          -> Pass
basicPass name f = polyPass name op
  where op s = Basic <$> (canFail "" (Just s) =<< (f <$> basicProg s))

unfailableBasicPass :: String -> (Basic.Prog -> Basic.Prog)
                    -> Pass
unfailableBasicPass name f = basicPass name f'
  where f' :: Basic.Prog -> Either () Basic.Prog
        f' = Right . f

explicitMemoryProg :: PipelineState -> PassM ExplicitMemory.Prog
explicitMemoryProg (ExplicitMemory p) = return p
explicitMemoryProg s                  = compileError "Expected explicit memory representation." $ Just s

explicitMemoryPass :: Show err =>
                      String -> (ExplicitMemory.Prog -> Either err ExplicitMemory.Prog)
                   -> Pass
explicitMemoryPass name f = polyPass name op
  where op s = ExplicitMemory <$> (canFail "" (Just s) =<< (f <$> explicitMemoryProg s))

unfailableExplicitMemoryPass :: String -> (ExplicitMemory.Prog -> ExplicitMemory.Prog)
                    -> Pass
unfailableExplicitMemoryPass name f = explicitMemoryPass name f'
  where f' :: ExplicitMemory.Prog -> Either () ExplicitMemory.Prog
        f' = Right . f

polyPass :: String -> (PipelineState -> PassM PipelineState)
         -> Pass
polyPass name f = Pass { passName = name
                       , passOp = f
                       }

basicAction :: String -> (Basic.Prog -> IO ())
            -> Action
basicAction name f = polyAction name op
  where op (Basic prog) = f prog
        op _            = error msg
        msg = "Action '" ++ name ++ "' expects basic representation."

explicitMemoryAction :: String -> (ExplicitMemory.Prog -> IO ())
                     -> Action
explicitMemoryAction name f = polyAction name op
  where op (ExplicitMemory prog) = f prog
        op _                     = error msg
        msg = "Action '" ++ name ++ "' expects explicit memory representation."

polyAction :: String -> (PipelineState -> IO ())
           -> Action
polyAction = Action
