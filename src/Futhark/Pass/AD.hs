{-# LANGUAGE TypeFamilies #-}

-- | Apply all AD operators in the program, leaving AD-free code.
module Futhark.Pass.AD (algebraicDifferentiation) where

import Control.Monad
import Futhark.AD.Fwd (fwdJVP)
import Futhark.AD.Rev (revVJP)
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Pass

bindLambda ::
  (MonadBuilder m, Rep m ~ SOACS) =>
  Pat Type ->
  StmAux (ExpDec SOACS) ->
  Lambda SOACS ->
  [SubExp] ->
  m ()
bindLambda pat aux (Lambda params body _) args = do
  auxing aux . forM_ (zip params args) $ \(param, arg) ->
    letBindNames [paramName param] $
      BasicOp $ case (paramType param, arg) of
        (Array {}, Var v) -> Copy v
        _ -> SubExp arg
  res <- bodyBind body
  forM_ (zip (patNames pat) res) $ \(v, SubExpRes cs se) ->
    certifying cs $ letBindNames [v] $ BasicOp $ SubExp se

onStm :: Scope SOACS -> Stm SOACS -> PassM (Stms SOACS)
onStm scope (Let pat aux (Op (VJP lam args vec))) = do
  lam' <- revVJP scope =<< onLambda scope lam
  runBuilderT_ (bindLambda pat aux lam' $ args ++ vec) scope
onStm scope (Let pat aux (Op (JVP lam args vec))) = do
  lam' <- fwdJVP scope =<< onLambda scope lam
  runBuilderT_ (bindLambda pat aux lam' $ args ++ vec) scope
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

onBody :: Scope SOACS -> Body SOACS -> PassM (Body SOACS)
onBody scope body = do
  stms <- onStms scope $ bodyStms body
  pure $ body {bodyStms = stms}

onLambda :: Scope SOACS -> Lambda SOACS -> PassM (Lambda SOACS)
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
