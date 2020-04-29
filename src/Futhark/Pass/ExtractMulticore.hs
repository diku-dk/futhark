module Futhark.Pass.ExtractMulticore (extractMulticore) where

import Control.Monad.Reader
import Control.Monad.State

import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.AST
import Futhark.Representation.MC
import qualified Futhark.Representation.MC as MC
import Futhark.Representation.SOACS
  hiding (Body, Exp, Lambda, LParam, Pattern, Stm)
import qualified Futhark.Representation.SOACS as SOACS
import Futhark.Util (chunks, takeLast)

type ExtractM = ReaderT (Scope MC) (State VNameSource)

indexArray :: VName -> LParam SOACS -> VName -> Stm MC
indexArray i (Param p t) arr =
  Let (Pattern [] [PatElem p t]) (defAux ()) $
  BasicOp $ Index arr $ DimFix (Var i) : map sliceDim (arrayDims t)

mapLambdaToBody :: VName -> Lambda SOACS -> [VName] -> ExtractM (Body MC)
mapLambdaToBody i lam arrs = do
  let indexings = zipWith (indexArray i) (lambdaParams lam) arrs
  Body () stms res <- inScopeOf indexings $ transformBody $ lambdaBody lam
  return $ Body () (stmsFromList indexings <> stms) res

mapLambdaToKernelBody :: VName -> Lambda SOACS -> [VName]
                      -> ExtractM (KernelBody MC)
mapLambdaToKernelBody i lam arrs = do
  Body () stms res <- mapLambdaToBody i lam arrs
  return $ KernelBody () stms $ map (Returns ResultMaySimplify) res

reduceToSegRedOp :: Reduce SOACS -> ExtractM (SegRedOp MC)
reduceToSegRedOp (Reduce comm lam nes) =
  SegRedOp comm <$> transformLambda lam <*> pure nes <*> pure mempty

mkSegSpace :: MonadFreshNames m => SubExp -> m (VName, SegSpace)
mkSegSpace w = do
  flat <- newVName "flat_tid"
  gtid <- newVName "gtid"
  let space = SegSpace flat [(gtid, w)]
  return (gtid, space)

transformLoopForm :: LoopForm SOACS -> LoopForm MC
transformLoopForm (WhileLoop cond) = WhileLoop cond
transformLoopForm (ForLoop i it bound params) = ForLoop i it bound params

transformStm :: Stm SOACS -> ExtractM (Stms MC)
transformStm (Let pat aux (BasicOp op)) =
  pure $ oneStm $ Let pat aux $ BasicOp op
transformStm (Let pat aux (Apply f args ret info)) =
  pure $ oneStm $ Let pat aux $ Apply f args ret info
transformStm (Let pat aux (DoLoop ctx val form body)) = do
  let form' = transformLoopForm form
  body' <- localScope (scopeOfFParams (map fst ctx) <>
                       scopeOfFParams (map fst val) <>
                       scopeOf form') $
           transformBody body
  return $ oneStm $ Let pat aux $ DoLoop ctx val form' body'
transformStm (Let pat aux (If cond tbranch fbranch ret)) =
  oneStm . Let pat aux <$>
  (If cond <$> transformBody tbranch <*> transformBody fbranch <*> pure ret)
transformStm (Let pat aux (Op op)) =
  fmap (certify (stmAuxCerts aux)) <$> transformSOAC pat op

transformLambda :: Lambda SOACS -> ExtractM (Lambda MC)
transformLambda (Lambda params body ret) =
  Lambda params
  <$> localScope (scopeOfLParams params) (transformBody body)
  <*> pure ret

transformStms :: Stms SOACS -> ExtractM (Stms MC)
transformStms stms =
  case stmsHead stms of
    Nothing -> return mempty
    Just (stm, stms') -> do
      stm_stms <- transformStm stm
      inScopeOf stm_stms $ (stm_stms<>) <$> transformStms stms'

transformBody :: Body SOACS -> ExtractM (Body MC)
transformBody (Body () stms res) =
  Body () <$> transformStms stms <*> pure res

transformFunDef :: FunDef SOACS -> ExtractM (FunDef MC)
transformFunDef (FunDef entry name rettype params body) = do
  body' <- localScope (scopeOfFParams params) $ transformBody body
  return $ FunDef entry name rettype params body'

transformProg :: Prog SOACS -> PassM (Prog MC)
transformProg (Prog consts funs) =
  modifyNameSource $ runState (runReaderT m mempty)
  where m = do
          consts' <- transformStms consts
          funs' <- inScopeOf consts' $ mapM transformFunDef funs
          return $ Prog consts' funs'

transformSOAC :: Pattern SOACS -> SOAC SOACS -> ExtractM (Stms MC)

transformSOAC pat (Screma w form arrs)
  | Just map_lam <- isMapSOAC form = do
      (gtid, space) <- mkSegSpace w
      kbody <- mapLambdaToKernelBody gtid map_lam arrs
      return $ oneStm $ Let pat (defAux ()) $ Op $
        SegMap () space (lambdaReturnType map_lam) kbody

  | Just (reds, map_lam) <- isRedomapSOAC form = do
      (gtid, space) <- mkSegSpace w
      kbody <- mapLambdaToKernelBody gtid map_lam arrs
      reds' <- mapM reduceToSegRedOp reds
      return $ oneStm $ Let pat (defAux ()) $ Op $
        SegRed () space reds' (lambdaReturnType map_lam) kbody

  | Just (lam, nes, map_lam) <- isScanomapSOAC form = do
      (gtid, space) <- mkSegSpace w
      kbody <- mapLambdaToKernelBody gtid map_lam arrs
      lam' <- transformLambda lam
      return $ oneStm $ Let pat (defAux ()) $ Op $
        SegScan () space lam' nes (lambdaReturnType map_lam) kbody

  | otherwise = do
      -- This screma is too complicated for us to immediately do
      -- anything, so split it up and try again.
      scope <- castScope <$> askScope
      transformStms =<< runBinderT_ (dissectScrema pat w form arrs) scope

transformSOAC pat (Scatter w lam ivs dests) = do
  (gtid, space) <- mkSegSpace w

  Body () kstms res <- mapLambdaToBody gtid lam ivs

  let (dests_ws, dests_ns, dests_vs) = unzip3 dests
      (i_res, v_res) = splitAt (sum dests_ns) res
      rets = takeLast (length dests) $ lambdaReturnType lam
      kres = do (a_w, a, is_vs) <- zip3 dests_ws dests_vs $
                                   chunks dests_ns $ zip i_res v_res
                return $ WriteReturns [a_w] a [ ([i],v) | (i,v) <- is_vs ]
      kbody = KernelBody () kstms kres
  return $ oneStm $ Let pat (defAux ()) $ Op $ SegMap () space rets kbody

transformSOAC pat (Hist w ops lam arrs) = do
  ops' <- forM ops $ \(SOACS.HistOp num_bins rf dests nes op) -> do
    op' <- transformLambda op
    return $ MC.HistOp num_bins rf dests nes mempty op'

  (gtid, space) <- mkSegSpace w

  Body () kstms res <- mapLambdaToBody gtid lam arrs
  let kbody = KernelBody () kstms $ map (Returns ResultMaySimplify) res

  return $ oneStm $ Let pat (defAux ()) $ Op $
    SegHist () space ops' (lambdaReturnType lam) kbody

transformSOAC pat (Stream w form lam arrs) = do
  -- Just remove the stream and transform the resulting stms.
  soacs_scope <- castScope <$> askScope
  stream_stms <-
    flip runBinderT_ soacs_scope $
    sequentialStreamWholeArray pat w (getStreamAccums form) lam arrs
  transformStms stream_stms

extractMulticore :: Pass SOACS MC
extractMulticore =
  Pass { passName = "extract multicore parallelism"
       , passDescription = "Extract multicore parallelism"
       , passFunction = transformProg
       }
