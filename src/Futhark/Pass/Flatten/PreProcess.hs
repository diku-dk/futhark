{-# LANGUAGE TypeFamilies #-}

-- | Preprocess the program before flattening.  This rewrites SOAC forms
-- that flatten does not want to see directly, while leaving the result in
-- SOACS form so the normal flattening pipeline can continue afterwards.
module Futhark.Pass.Flatten.PreProcess (preprocessProg) where

import Data.Maybe (isNothing)
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify (simplifyStms)
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

soacMapper :: Scope SOACS -> SOACMapper SOACS SOACS PassM
soacMapper scope =
  identitySOACMapper {mapOnSOACLambda = preprocessLambda scope}

runSimplifiedBuilder ::
  Scope SOACS ->
  BuilderT SOACS PassM a ->
  PassM (Stms SOACS)
runSimplifiedBuilder scope m =
  fst <$> runBuilderT (simplifyStms =<< collectStms_ m) scope

-- TODO: maybe it is better to seperate these as they are doing different things.
preprocessStm :: Scope SOACS -> Stm SOACS -> PassM (Stms SOACS)
preprocessStm scope (Let pat aux (Op (Stream w arrs nes lam))) = do
  lam' <- preprocessLambda scope lam
  runBuilderT_ (auxing aux $ sequentialStreamWholeArray pat w nes lam' arrs) scope
preprocessStm scope (Let pat aux (Op (Screma w arrs form))) = do
  soac' <- mapSOACM (soacMapper scope) (Screma w arrs form)
  case soac' of
    Screma w' arrs' form'
      | Just scans <- isScanSOAC form',
        Scan scan_lam nes <- singleScan scans,
        Just do_iswim <- iswim pat w' scan_lam (zip nes arrs') ->
          runSimplifiedBuilder scope $ auxing aux do_iswim
      | Just [Reduce comm red_fun nes] <- isReduceSOAC form',
        let comm'
              | commutativeLambda red_fun = Commutative
              | otherwise = comm,
        Just do_irwim <- irwim pat w' comm' red_fun (zip nes arrs') ->
          runSimplifiedBuilder scope $ auxing aux do_irwim
      | shouldDissectForm form' ->
          runBuilderT_ (auxing aux $ dissectScrema pat w' form' arrs') scope
      | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form',
        any isParallelStm (bodyStms $ lambdaBody map_lam),
        any isParallelStm (bodyStms $ lambdaBody post_lam) -> do
          (mapstm, scanstm, poststm) <-
            maposcanomapToMapScanAndMap
              pat
              (w', post_lam, scans, map_lam, arrs')
          preprocessStms scope $ stmsFromList [mapstm, scanstm, poststm]
      | Just (post_lam, _, _) <- isMaposcanomapSOAC form',
        any isParallelStm (bodyStms $ lambdaBody post_lam) -> do
          stms <-
            runSimplifiedBuilder scope $
              auxing aux $
                extractPostLambda pat w' arrs' form'
          preprocessStms scope stms

      | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form',
        any isParallelStm (bodyStms $ lambdaBody map_lam) -> do
          (mapstm, scanstm) <-
            maposcanomapToMaposcanAndMap
              pat
              (w', post_lam, scans, map_lam, arrs')
          preprocessStms scope $ stmsFromList [mapstm, scanstm]   
      | Just (reds, map_lam) <- isRedomapSOAC form',
        any isParallelStm (bodyStms $ lambdaBody map_lam) -> do
          (mapstm, redstm) <-
            redomapToMapAndReduce
              pat
              (w', reds, map_lam, arrs')
          preprocessStms scope $ stmsFromList [mapstm, redstm]
      | otherwise ->
          pure $ oneStm $ Let pat aux $ Op $ Screma w' arrs' form'
    _ ->
      error "preprocessStm: impossible non-Screma"
preprocessStm scope (Let pat aux e) =
  oneStm . Let pat aux <$> mapExpM mapper e
  where
    mapper =
      (identityMapper @SOACS)
        { mapOnBody = \bscope -> preprocessBody (bscope <> scope),
          mapOnOp = mapSOACM (soacMapper scope)
        }

preprocessStms :: Scope SOACS -> Stms SOACS -> PassM (Stms SOACS)
preprocessStms scope stms = mconcat <$> mapM (preprocessStm scope') (stmsToList stms)
  where
    scope' = scopeOf stms <> scope

preprocessBody :: Scope SOACS -> Body SOACS -> PassM (Body SOACS)
preprocessBody scope body = do
  stms <- preprocessStms scope $ bodyStms body
  pure $ body {bodyStms = stms}

preprocessLambda :: Scope SOACS -> Lambda SOACS -> PassM (Lambda SOACS)
preprocessLambda scope lam = do
  body <- preprocessBody (scopeOfLParams (lambdaParams lam) <> scope) $ lambdaBody lam
  pure $ lam {lambdaBody = body}

preprocessFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
preprocessFun consts fd = do
  body <- preprocessBody (scopeOf consts <> scopeOf fd) $ funDefBody fd
  pure $ fd {funDefBody = body}

preprocessProg :: Prog SOACS -> PassM (Prog SOACS)
preprocessProg prog = do
  prog' <-
    intraproceduralTransformationWithConsts
      (preprocessStms mempty)
      preprocessFun
      prog
  SOACS.simplifySOACS prog' -- Is this a good idea?