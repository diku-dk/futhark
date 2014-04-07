-- | L0 compiler internal pipeline.
module L0C.Pipeline
  (
    runPasses
  , L0CM
  , Pass(..)
  , CompileError(..)
  , compileError
  , Action
  , L0Config(..)
  , verbose
  )

where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer.Strict (Writer, tell)
import Control.Monad.Error
import Data.Maybe (isJust)

import L0C.InternalRep
import L0C.InternalRep.TypeChecker

runPasses :: L0Config -> Prog -> L0CM Prog
runPasses config = foldl comb return $ l0pipeline config
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

type L0CM = ErrorT CompileError (Writer String)

data Pass = Pass {
    passName :: String
  , passOp :: Prog -> L0CM Prog
  }

type Action = (String, Prog -> IO ())

data L0Config = L0Config {
    l0pipeline :: [Pass]
  , l0action :: Action
  , l0checkAliases :: Bool
  , l0verbose :: Maybe (Maybe FilePath)
  , l0boundsCheck :: Bool
}

verbose :: L0Config -> Bool
verbose = isJust . l0verbose

compileError :: String -> Maybe Prog -> L0CM a
compileError s p = throwError $ CompileError s p

typeCheck :: L0Config -> Prog -> Either TypeError Prog
typeCheck config
  | l0checkAliases config = checkProg
  | otherwise             = checkProgNoUniqueness
