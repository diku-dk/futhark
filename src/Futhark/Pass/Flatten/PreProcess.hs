{-# LANGUAGE TypeFamilies #-}

-- | Preprocess the program before flattening.  This rewrites SOAC forms
-- that flatten does not want to see directly, while leaving the result in
-- SOACS form so the normal flattening pipeline can continue afterwards.
module Futhark.Pass.Flatten.PreProcess (preprocessProg) where

import Data.Maybe (isNothing)
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Pass
import Futhark.Tools

shouldDissectForm :: ScremaForm SOACS -> Bool
shouldDissectForm form =
  isNothing (isMapSOAC form)
    && isNothing (isReduceSOAC form)
    && isNothing (isScanSOAC form)
    && isNothing (isRedomapSOAC form)
    && isNothing (isScanomapSOAC form)

soacMapper :: Scope SOACS -> SOACMapper SOACS SOACS PassM
soacMapper scope =
  identitySOACMapper {mapOnSOACLambda = onLambda scope}

-- TODO: maybe it is better to seperate these as they are doing different things.
onStm :: Scope SOACS -> Stm SOACS -> PassM (Stms SOACS)
onStm scope (Let pat aux (Op (Stream w arrs nes lam))) = do
  lam' <- onLambda scope lam
  runBuilderT_ (auxing aux $ sequentialStreamWholeArray pat w nes lam' arrs) scope
onStm scope (Let pat aux (Op (Screma w arrs form))) = do
  soac' <- mapSOACM (soacMapper scope) (Screma w arrs form)
  case soac' of
    Screma w' arrs' form' ->
      if shouldDissectForm form'
        then
          runBuilderT_ (auxing aux $ dissectScrema pat w' form' arrs') scope
        else
          pure $ oneStm $ Let pat aux $ Op $ Screma w' arrs' form'
    _ -> undefined
onStm scope (Let pat aux e) =
  oneStm . Let pat aux <$> mapExpM mapper e
  where
    mapper =
      (identityMapper @SOACS)
        { mapOnBody = \bscope -> onBody (bscope <> scope),
          mapOnOp = mapSOACM (soacMapper scope)
        }

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