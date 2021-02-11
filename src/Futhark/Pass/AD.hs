{-# LANGUAGE TypeFamilies #-}

-- | Apply all AD operators in the program, leaving AD-free code.
module Futhark.Pass.AD (algebraicDifferentiation) where

import Control.Monad
import Futhark.AD.Fwd (fwdJVP)
import Futhark.AD.Rev (revVJP)
import Futhark.Binder
import Futhark.IR.SOACS
import Futhark.Pass

bindLambda ::
  (MonadBinder m, Lore m ~ SOACS) =>
  Pattern ->
  StmAux (ExpDec SOACS) ->
  Lambda ->
  [SubExp] ->
  m ()
bindLambda pat aux (Lambda params body _) args = do
  auxing aux . forM_ (zip params args) $ \(param, arg) ->
    letBindNames [paramName param] $ BasicOp $ SubExp arg
  res <- bodyBind body
  forM_ (zip (patternNames pat) res) $ \(v, se) ->
    letBindNames [v] $ BasicOp $ SubExp se

onStm :: Scope SOACS -> Stm -> PassM (Stms SOACS)
onStm scope (Let pat aux (Op (VJP lam args vec))) = do
  lam' <- revVJP scope lam
  runBinderT_ (bindLambda pat aux lam' $ args ++ vec) scope
onStm scope (Let pat aux (Op (JVP lam args vec))) = do
  lam' <- fwdJVP scope lam
  runBinderT_ (bindLambda pat aux lam' $ args ++ vec) scope
onStm scope (Let pat aux e) = oneStm . Let pat aux <$> mapExpM mapper e
  where
    mapper =
      identityMapper
        { mapOnBody = \bscope -> onBody (bscope <> scope),
          mapOnOp = mapSOACM soac_mapper
        }
    soac_mapper = identitySOACMapper {mapOnSOACLambda = onLambda scope}

onStms :: Scope SOACS -> Stms SOACS -> PassM (Stms SOACS)
onStms scope stms = mconcat <$> mapM (onStm scope') (stmsToList stms)
  where
    scope' = scopeOf stms <> scope

onBody :: Scope SOACS -> Body -> PassM Body
onBody scope body = do
  stms <- onStms scope $ bodyStms body
  pure $ body {bodyStms = stms}

onLambda :: Scope SOACS -> Lambda -> PassM Lambda
onLambda scope lam = do
  body <- onBody (scopeOfLParams (lambdaParams lam) <> scope) $ lambdaBody lam
  pure $ lam {lambdaBody = body}

onFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
onFun consts fd = do
  body <- onBody (scopeOf consts <> scopeOf fd) $ funDefBody fd
  pure $ fd {funDefBody = body}

algebraicDifferentiation :: Pass SOACS SOACS
algebraicDifferentiation =
  Pass
    { passName = "ad",
      passDescription = "Apply AD operators",
      passFunction = intraproceduralTransformationWithConsts (onStms mempty) onFun
    }
