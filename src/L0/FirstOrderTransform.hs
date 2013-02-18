module L0.FirstOrderTransform
  ( transformProg )
  where

import Control.Monad.State
import Control.Monad.Identity

import L0.AbSyn

data TrState = TrState {
    stCounter :: Int
  }

newtype TransformM a = TransformM (State TrState a)

runTransformM :: TransformM a -> a
runTransformM (TransformM m) = evalState m (TrState 0)

transformProg :: Prog Identity -> Prog Identity
transformProg = runTransformM . mapM transformFunDec

transformFunDec :: FunDec Identity -> FunDec Identity
transformFunDec (fname, rettype, params, body, loc) = do
  body' <- transformExp body
  return (fname, rettype, params, body', loc)

transformExp :: Exp Identity -> Exp Identity
transformExp (Literal v) = return $ Literal v
