-- | Futhark compiler internal pipeline.
module Futhark.Pipeline
  (
    runPasses
  , FutharkM
  , PipelineState (..)
  , Pass (..)
  , canFail
  , polyPass
  , basicPass
  , unfailableBasicPass
  , basicProg
  , CompileError(..)
  , compileError
  , Action
  , actionDescription
  , applyAction
  , polyAction
  , basicAction
  , explicitMemoryAction
  , FutharkConfig(..)
  , verbose
  )

where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer.Strict (Writer, tell)
import Control.Monad.Except
import Data.Maybe (isJust)

import qualified Text.PrettyPrint.Mainland as PP

import Futhark.Representation.AST (Prog)
import qualified Futhark.Representation.Basic as Basic
import qualified Futhark.Representation.ExplicitMemory as ExplicitMemory
import Futhark.TypeCheck

runPasses :: FutharkConfig -> PipelineState -> FutharkM PipelineState
runPasses config = foldl comb return $ futharkpipeline config
  where comb prev pass prog = do
          prog' <- prev prog
          when (verbose config) $ tell $ "Running " ++ passName pass ++ ".\n"
          res <- lift $ runExceptT $ passOp pass prog'
          case res of
            Left err ->
              compileError ("Error during pass '" ++ passName pass ++ "':\n" ++ errorDesc err)
                           (Just prog')
            Right prog'' ->
              validateState config pass prog''

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

type FutharkM = ExceptT CompileError (Writer String)

data PipelineState = Basic Basic.Prog
                   | ExplicitMemory ExplicitMemory.Prog

instance PP.Pretty PipelineState where
  ppr (Basic prog)          = PP.ppr prog
  ppr (ExplicitMemory prog) = PP.ppr prog

data Pass = Pass {
    passName :: String
  , passOp :: PipelineState -> FutharkM PipelineState
  }

data Action = Action String (PipelineState -> IO ())

actionDescription :: Action -> String
actionDescription (Action name _) = name

applyAction :: Action -> PipelineState -> IO ()
applyAction (Action _ f) = f

data FutharkConfig = FutharkConfig {
    futharkpipeline :: [Pass]
  , futharkaction :: Action
  , futharkcheckAliases :: Bool
  , futharkverbose :: Maybe (Maybe FilePath)
  , futharkboundsCheck :: Bool
}

verbose :: FutharkConfig -> Bool
verbose = isJust . futharkverbose

compileError :: String -> Maybe PipelineState -> FutharkM a
compileError s p = throwError $ CompileError s p

typeCheck :: Checkable lore => FutharkConfig -> Prog lore
          -> Either (TypeError lore) (Prog lore)
typeCheck config prog = either Left (const $ Right prog) $ check prog
  where check | futharkcheckAliases config = checkProg
              | otherwise                  = checkProgNoUniqueness

canFail :: Show err =>
           String -> Maybe PipelineState -> Either err a -> FutharkM a
canFail d p (Left err) = compileError (d ++ show err) p
canFail _ _ (Right v)  = return v

basicProg :: PipelineState -> FutharkM Basic.Prog
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

polyPass :: String -> (PipelineState -> FutharkM PipelineState)
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
