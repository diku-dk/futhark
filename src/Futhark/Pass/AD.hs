{-# LANGUAGE TypeFamilies #-}

-- | Apply all AD operators in the program, leaving AD-free code.
module Futhark.Pass.AD (applyAD, applyADInnermost) where

import Control.Monad
import Control.Monad.Reader
import Futhark.AD.Fwd (fwdJVP)
import Futhark.AD.Rev (revVJP)
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify (simplifyLambda)
import Futhark.Pass

-- | Whether we apply only the innermost AD operators, or all of them.
-- The former is very useful for debugging, but probably not useful
-- for actual compilation.
data Mode = Innermost | All
  deriving (Eq)

bindLambda ::
  (MonadBuilder m, Rep m ~ SOACS) =>
  Pat Type ->
  StmAux (ExpDec SOACS) ->
  Lambda SOACS ->
  [SubExp] ->
  m ()
bindLambda pat aux (Lambda params _ body) args = do
  auxing aux . forM_ (zip params args) $ \(param, arg) ->
    letBindNames [paramName param] $
      BasicOp $ case paramType param of
        Array {} -> Replicate mempty arg
        _ -> SubExp arg
  res <- bodyBind body
  forM_ (zip (patNames pat) res) $ \(v, SubExpRes cs se) ->
    certifying cs $ letBindNames [v] $ BasicOp $ SubExp se

onStm :: Bool -> Mode -> Scope SOACS -> Stm SOACS -> PassM (Stms SOACS)
onStm _ mode scope (Let pat aux (Op (VJP args vec lam))) = do
  lam' <- onLambda True mode scope lam
  if mode == All || lam == lam'
    then do
      lam'' <- (`runReaderT` scope) . simplifyLambda =<< revVJP scope lam'
      runBuilderT_ (bindLambda pat aux lam'' $ args ++ vec) scope
    else pure $ oneStm $ Let pat aux $ Op $ VJP args vec lam'
onStm _ mode scope (Let pat aux (Op (JVP args vec lam))) = do
  lam' <- onLambda True mode scope lam
  if mode == All || lam == lam'
    then do
      lam'' <- fwdJVP scope lam'
      runBuilderT_ (bindLambda pat aux lam'' $ args ++ vec) scope
    else pure $ oneStm $ Let pat aux $ Op $ JVP args vec lam'
--
-- This corresponds to a WithVJP that is not inside of a differential operator.
-- FIXME: this assumption will go bad when we don't inline so much.
onStm False _ scope (Let pat aux (Op (WithVJP args lam _))) =
  runBuilderT_ (bindLambda pat aux lam args) scope
onStm ad mode scope (Let pat aux e) = oneStm . Let pat aux <$> mapExpM mapper e
  where
    mapper =
      (identityMapper @SOACS)
        { mapOnBody = \bscope -> onBody ad mode (bscope <> scope),
          mapOnOp = mapSOACM soac_mapper
        }
    soac_mapper = identitySOACMapper {mapOnSOACLambda = onLambda ad mode scope}

onStms :: Bool -> Mode -> Scope SOACS -> Stms SOACS -> PassM (Stms SOACS)
onStms ad mode scope stms = mconcat <$> mapM (onStm ad mode scope') (stmsToList stms)
  where
    scope' = scopeOf stms <> scope

onBody :: Bool -> Mode -> Scope SOACS -> Body SOACS -> PassM (Body SOACS)
onBody ad mode scope body = do
  stms <- onStms ad mode scope $ bodyStms body
  pure $ body {bodyStms = stms}

onLambda :: Bool -> Mode -> Scope SOACS -> Lambda SOACS -> PassM (Lambda SOACS)
onLambda ad mode scope lam = do
  body <- onBody ad mode (scopeOfLParams (lambdaParams lam) <> scope) $ lambdaBody lam
  pure $ lam {lambdaBody = body}

onFun :: Mode -> Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
onFun mode consts fd = do
  body <- onBody False mode (scopeOf consts <> scopeOf fd) $ funDefBody fd
  pure $ fd {funDefBody = body}

applyAD :: Pass SOACS SOACS
applyAD =
  Pass
    { passName = "ad",
      passDescription = "Apply AD operators",
      passFunction =
        intraproceduralTransformationWithConsts
          (onStms False All mempty)
          (onFun All)
    }

applyADInnermost :: Pass SOACS SOACS
applyADInnermost =
  Pass
    { passName = "ad innermost",
      passDescription = "Apply innermost AD operators",
      passFunction =
        intraproceduralTransformationWithConsts
          (onStms False Innermost mempty)
          (onFun Innermost)
    }
