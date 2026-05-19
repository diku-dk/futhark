{-# LANGUAGE TypeFamilies #-}

-- | Preprocess the program before flattening.  This rewrites SOAC forms
-- that flatten does not want to see directly, while leaving the result in
-- SOACS form so the normal flattening pipeline can continue afterwards.
module Futhark.Pass.Flatten.PreProcess (preprocessProg, preprocessBody, preprocessStms, preprocessStm, preprocessLambda) where

import Data.Maybe (isNothing)
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify 
import Futhark.IR.SOACS.Simplify qualified as SOACS
import Futhark.Pass
import Futhark.Pass.Flatten.Distribute (isParallelStm)
import Futhark.Pass.Flatten.ISRWIM (irwim, iswim)
import Futhark.Tools

shouldDissectForm :: ScremaForm SOACS -> Bool
shouldDissectForm form =
  isNothing (isMapSOAC form)
    && isNothing (isReduceSOAC form)
    && isNothing (isScanSOAC form)
    && isNothing (isRedomapSOAC form)
    && isNothing (isScanomapSOAC form)
    && isNothing (isMaposcanomapSOAC form)

runSimplifiedBuilder ::
  MonadFreshNames m =>
  Scope SOACS ->
  BuilderT SOACS m a ->
  m (Stms SOACS)
runSimplifiedBuilder scope m =
  fst <$> runBuilderT (simplifyStms =<< collectStms_ m) scope

-- TODO: maybe it is better to seperate these as they are doing different things.
preprocessStm ::
  MonadFreshNames m =>
  Scope SOACS ->
  Stm SOACS ->
  m (Stms SOACS)
preprocessStm _ stm 
 | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) = pure $ oneStm stm
preprocessStm scope (Let pat aux (Op (Stream w arrs nes lam))) = do
  stms <- runSimplifiedBuilder scope (auxing aux $ sequentialStreamWholeArray pat w nes lam arrs)
  preprocessStms scope stms
preprocessStm scope (Let pat aux (Op (Screma w' arrs' form')))
  | Just scans <- isScanSOAC form',
    Scan scan_lam nes <- singleScan scans,
    Just do_iswim <- iswim pat w' scan_lam (zip nes arrs') = do
      stms <- runSimplifiedBuilder scope $ auxing aux do_iswim
      preprocessStms scope stms
  | Just [Reduce comm red_fun nes] <- isReduceSOAC form',
    let comm'
          | commutativeLambda red_fun = Commutative
          | otherwise = comm,
    Just do_irwim <- irwim pat w' comm' red_fun (zip nes arrs') = do
      stms <- runSimplifiedBuilder scope $ auxing aux do_irwim
      preprocessStms scope stms
  | shouldDissectForm form' = do
      stms <- runSimplifiedBuilder scope (auxing aux $ dissectScrema pat w' form' arrs')
      preprocessStms scope stms
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form',
    any isParallelStm (bodyStms $ lambdaBody map_lam),
    any isParallelStm (bodyStms $ lambdaBody post_lam) = do
      (mapstm, scanstm, poststm) <-
        maposcanomapToMapScanAndMap
          pat
          (w', post_lam, scans, map_lam, arrs')
      let stms = stmsFromList  [mapstm, scanstm, poststm]
      stms' <-  fst <$> runBuilderT (simplifyStms stms) scope
      preprocessStms scope stms'
  | Just (post_lam, _, _) <- isMaposcanomapSOAC form',
    any isParallelStm (bodyStms $ lambdaBody post_lam) = do
      stms <-
        runSimplifiedBuilder scope $
          auxing aux $
            extractPostLambda pat w' arrs' form'
      preprocessStms scope stms
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form',
    any isParallelStm (bodyStms $ lambdaBody map_lam) = do
      (mapstm, scanstm) <-
        maposcanomapToMaposcanAndMap
          pat
          (w', post_lam, scans, map_lam, arrs')
      let stms = stmsFromList [mapstm, scanstm]
      stms' <- fst <$> runBuilderT (simplifyStms stms) scope
      preprocessStms scope stms'
  | Just (reds, map_lam) <- isRedomapSOAC form',
    any isParallelStm (bodyStms $ lambdaBody map_lam) = do
      (mapstm, redstm) <-
        redomapToMapAndReduce
          pat
          (w', reds, map_lam, arrs')
      let stms = stmsFromList [mapstm, redstm]
      stms' <- fst <$> runBuilderT (simplifyStms stms) scope
      preprocessStms scope stms'
preprocessStm _ stm = pure $ oneStm stm

preprocessStms ::
  MonadFreshNames m =>
  Scope SOACS ->
  Stms SOACS ->
  m (Stms SOACS)
preprocessStms scope stms = mconcat <$> mapM (preprocessStm scope') (stmsToList stms)
  where
    scope' = scopeOf stms <> scope

preprocessBody ::
  MonadFreshNames m =>
  Scope SOACS ->
  Body SOACS ->
  m (Body SOACS)
preprocessBody scope body = do
  stms <- preprocessStms scope $ bodyStms body
  pure $ body {bodyStms = stms}

preprocessLambda ::
  MonadFreshNames m =>
  Scope SOACS ->
  Lambda SOACS ->
  m (Lambda SOACS)
preprocessLambda scope lam = do
  body <- preprocessBody (scopeOfLParams (lambdaParams lam) <> scope) $ lambdaBody lam
  let lam' = lam {lambdaBody = body}
  fst <$> runBuilderT (simplifyLambda lam') scope


preprocessFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
preprocessFun consts fd = do
  body <- preprocessBody (scopeOf consts <> scopeOf fd) $ funDefBody fd
  pure $ fd {funDefBody = body}

preprocessProg :: Prog SOACS -> PassM (Prog SOACS)
preprocessProg =
  intraproceduralTransformationWithConsts
    (preprocessStms mempty)
    preprocessFun
