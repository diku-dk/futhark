{-# LANGUAGE TypeFamilies #-}

-- | Preprocess the program before flattening. For now, this includes removing
-- 'Stream' SOACs.
module Futhark.Pass.Flatten.PreProcess (preprocessProg) where

import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Pass
import Futhark.Tools

onStm :: Scope SOACS -> Stm SOACS -> PassM (Stms SOACS)
onStm scope (Let pat aux (Op (Stream w arrs nes lam))) = do
  lam' <- onLambda scope lam
  stms <- runBuilderT_ (auxing aux $ sequentialStreamWholeArray pat w nes lam' arrs) scope
  onStms scope stms
onStm scope (Let pat aux e) =
  oneStm . Let pat aux <$> mapExpM mapper e
  where
    mapper =
      (identityMapper @SOACS)
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

preprocessProg :: Prog SOACS -> PassM (Prog SOACS)
preprocessProg =
  intraproceduralTransformationWithConsts
    (onStms mempty)
    onFun