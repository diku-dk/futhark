-- | Futhark compiler internal pipeline.
module Futhark.Pipeline
  (
    runPasses
  , FutharkM
  , Pass(..)
  , CompileError(..)
  , compileError
  , Action
  , Futharkonfig(..)
  , verbose
  )

where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer.Strict (Writer, tell)
import Control.Monad.Error
import Data.Maybe (isJust)

import Futhark.InternalRep
import Futhark.InternalRep.TypeChecker

runPasses :: Futharkonfig -> Prog -> FutharkM Prog
runPasses config = foldl comb return $ futharkpipeline config
  where comb prev pass prog = do
          prog' <- prev prog
          when (verbose config) $ tell $ "Running " ++ passName pass ++ ".\n"
          res <- lift $ runErrorT $ passOp pass prog'
          case res of
            Left err ->
              compileError ("Error during pass '" ++ passName pass ++ "':\n" ++ errorDesc err)
                           (Just prog')
            Right prog'' ->
              case typeCheck config prog'' of
                Left err ->
                  compileError ("Type error after pass '" ++ passName pass ++
                                "':\n" ++ show err)
                               (Just prog'')
                Right prog''' -> return prog'''

data CompileError = CompileError {
    errorDesc :: String
  , errorProg :: Maybe Prog
  }

instance Error CompileError where
  strMsg s = CompileError s Nothing

type FutharkM = ErrorT CompileError (Writer String)

data Pass = Pass {
    passName :: String
  , passOp :: Prog -> FutharkM Prog
  }

type Action = (String, Prog -> IO ())

data Futharkonfig = Futharkonfig {
    futharkpipeline :: [Pass]
  , futharkaction :: Action
  , futharkcheckAliases :: Bool
  , futharkverbose :: Maybe (Maybe FilePath)
  , futharkboundsCheck :: Bool
}

verbose :: Futharkonfig -> Bool
verbose = isJust . futharkverbose

compileError :: String -> Maybe Prog -> FutharkM a
compileError s p = throwError $ CompileError s p

typeCheck :: Futharkonfig -> Prog -> Either TypeError Prog
typeCheck config
  | futharkcheckAliases config = checkProg
  | otherwise             = checkProgNoUniqueness
