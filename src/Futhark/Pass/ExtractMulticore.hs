{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Futhark.Pass.ExtractMulticore (extractMulticore) where

import Control.Monad.Reader
import Control.Monad.State

import Futhark.Tools
import Futhark.Pass
import Futhark.IR
import Futhark.IR.MC
import qualified Futhark.IR.MC as MC
import Futhark.IR.SOACS
  hiding (Body, Exp, Lambda, LParam, Pattern, Stm)
import qualified Futhark.IR.SOACS as SOACS
import qualified Futhark.IR.SOACS.Simplify as SOACS
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.DistributeNests
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Util (chunks, takeLast)
import Futhark.Util.Log

newtype ExtractM a = ExtractM (ReaderT (Scope MC) (State VNameSource) a)
  deriving (Functor, Applicative, Monad,
            HasScope MC, LocalScope MC,
            MonadFreshNames)

-- XXX: throwing away the log here...
instance MonadLogger ExtractM where
  addLog _ = pure ()

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

reduceToSegBinOp :: Reduce SOACS -> ExtractM (Stms MC, SegBinOp MC)
reduceToSegBinOp (Reduce comm lam nes) = do
  ((lam', nes', shape), stms) <- runBinder $ determineReduceOp lam nes
  lam'' <- transformLambda lam'
  return (stms, SegBinOp comm lam'' nes' shape)

scanToSegBinOp :: Scan SOACS -> ExtractM (Stms MC, SegBinOp MC)
scanToSegBinOp (Scan lam nes) = do
  ((lam', nes', shape), stms) <- runBinder $ determineReduceOp lam nes
  lam'' <- transformLambda lam'
  return (stms, SegBinOp Noncommutative lam'' nes' shape)

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
  fmap (certify (stmAuxCerts aux)) <$> transformSOAC pat (stmAuxAttrs aux) op

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
transformFunDef (FunDef entry attrs name rettype params body) = do
  body' <- localScope (scopeOfFParams params) $ transformBody body
  return $ FunDef entry attrs name rettype params body'

transformMap :: MapLoop -> ExtractM (Stms MC)
transformMap (MapLoop pat cs w lam arrs) = do
  scope <- askScope
  let loopnest =
        MapNesting pat cs w $ zip (lambdaParams lam) arrs
      runExtract (ExtractM m) = do
        local_scope <- castScope <$> askScope
        modifyNameSource $ runState (runReaderT m local_scope)
      env = DistEnv
            { distNest = singleNesting (Nesting mempty loopnest)
            , distScope = scopeOfPattern pat <> castScope (scopeOf lam) <> scope
            , distOnInnerMap = distributeMap
            , distOnTopLevelStms = \m -> do
                local_scope <- askScope
                lift $ localScope local_scope $ transformStms m
            , distSegLevel = \_ _ _ -> pure ()
            , distOnSOACSStms = runExtract . transformStm
            , distOnSOACSLambda = runExtract . transformLambda
            }
      acc = DistAcc { distTargets =
                        singleTarget (pat, bodyResult $ lambdaBody lam)
                    , distStms =
                        mempty
                    }

  -- XXX: we are throwing away the Log here.
  runDistNestT env $
    distributeMapBodyStms acc $ bodyStms $ lambdaBody lam

-- Sets the chunk size to one.
unstreamLambda :: Attrs -> [SubExp] -> Lambda SOACS -> ExtractM (Lambda SOACS)
unstreamLambda attrs nes lam = do
  let (chunk_param, acc_params, slice_params) =
        partitionChunkedFoldParameters (length nes) (lambdaParams lam)

  inp_params <- forM slice_params $ \(Param p t) ->
    newParam (baseString p) (rowType t)

  body <- runBodyBinder $ localScope (scopeOfLParams inp_params) $ do
    letBindNames [paramName chunk_param] $
      BasicOp $ SubExp $ intConst Int32 1

    forM_ (zip acc_params nes) $ \(p, ne) ->
      letBindNames [paramName p] $ BasicOp $ SubExp ne

    forM_ (zip slice_params inp_params) $ \(slice, v) ->
      letBindNames [paramName slice] $
      BasicOp $ ArrayLit [Var $ paramName v] (paramType v)

    (red_res, map_res) <- splitAt (length nes) <$> bodyBind (lambdaBody lam)

    map_res' <- forM map_res $ \se -> do
      v <- letExp "map_res" $ BasicOp $ SubExp se
      v_t <- lookupType v
      letSubExp "chunk" $ BasicOp $ Index v $
        fullSlice v_t [DimFix $ intConst Int32 0]

    pure $ resultBody $ red_res <> map_res'

  let (red_ts, map_ts) = splitAt (length nes) $ lambdaReturnType lam
      map_lam = Lambda { lambdaReturnType = red_ts ++ map rowType map_ts
                       , lambdaParams = inp_params
                       , lambdaBody = body
                       }

  soacs_scope <- castScope <$> askScope
  map_lam' <- runReaderT (SOACS.simplifyLambda map_lam) soacs_scope

  if "sequential_inner" `inAttrs` attrs
    then FOT.transformLambda map_lam'
    else return map_lam'

transformSOAC :: Pattern SOACS -> Attrs -> SOAC SOACS -> ExtractM (Stms MC)

transformSOAC pat _ (Screma w form arrs)
  | Just lam <- isMapSOAC form =
      transformMap $ MapLoop pat (defAux ()) w lam arrs

  | Just (reds, map_lam) <- isRedomapSOAC form = do
      (gtid, space) <- mkSegSpace w
      kbody <- mapLambdaToKernelBody gtid map_lam arrs
      (reds_stms, reds') <- unzip <$> mapM reduceToSegBinOp reds
      return $ mconcat reds_stms <>
        oneStm (Let pat (defAux ()) $ Op $
                SegRed () space reds' (lambdaReturnType map_lam) kbody)

  | Just (scans, map_lam) <- isScanomapSOAC form = do
      (gtid, space) <- mkSegSpace w
      kbody <- mapLambdaToKernelBody gtid map_lam arrs
      (scans_stms, scans') <- unzip <$> mapM scanToSegBinOp scans
      return $ mconcat scans_stms <>
        oneStm (Let pat (defAux ()) $ Op $
                SegScan () space scans' (lambdaReturnType map_lam) kbody)

  | otherwise = do
      -- This screma is too complicated for us to immediately do
      -- anything, so split it up and try again.
      scope <- castScope <$> askScope
      transformStms =<< runBinderT_ (dissectScrema pat w form arrs) scope

transformSOAC pat _ (Scatter w lam ivs dests) = do
  (gtid, space) <- mkSegSpace w

  Body () kstms res <- mapLambdaToBody gtid lam ivs

  let (dests_ws, dests_ns, dests_vs) = unzip3 dests
      (i_res, v_res) = splitAt (sum dests_ns) res
      rets = takeLast (length dests) $ lambdaReturnType lam
      kres = do (a_w, a, is_vs) <- zip3 dests_ws dests_vs $
                                   chunks dests_ns $ zip i_res v_res
                return $ WriteReturns [a_w] a [ ([DimFix i],v) | (i,v) <- is_vs ]
      kbody = KernelBody () kstms kres
  return $ oneStm $ Let pat (defAux ()) $ Op $ SegMap () space rets kbody

transformSOAC pat _ (Hist w ops lam arrs) = do
  ops' <- forM ops $ \(SOACS.HistOp num_bins rf dests nes op) -> do
    op' <- transformLambda op
    return $ MC.HistOp num_bins rf dests nes mempty op'

  (gtid, space) <- mkSegSpace w

  Body () kstms res <- mapLambdaToBody gtid lam arrs
  let kbody = KernelBody () kstms $ map (Returns ResultMaySimplify) res

  return $ oneStm $ Let pat (defAux ()) $ Op $
    SegHist () space ops' (lambdaReturnType lam) kbody

transformSOAC pat attrs (Stream w (Parallel _ comm red_lam red_nes) fold_lam arrs)
  | not $ null red_nes = do
  (gtid, space) <- mkSegSpace w
  map_lam <- unstreamLambda attrs red_nes fold_lam
  kbody <- mapLambdaToKernelBody gtid map_lam arrs
  (red_stms, red) <- reduceToSegBinOp $ Reduce comm red_lam red_nes
  return $ red_stms <>
    oneStm (Let pat (defAux ()) $ Op $
            SegRed () space [red] (lambdaReturnType map_lam) kbody)

transformSOAC pat _ (Stream w form lam arrs) = do
  -- Just remove the stream and transform the resulting stms.
  soacs_scope <- castScope <$> askScope
  stream_stms <-
    flip runBinderT_ soacs_scope $
    sequentialStreamWholeArray pat w (getStreamAccums form) lam arrs
  transformStms stream_stms

transformProg :: Prog SOACS -> PassM (Prog MC)
transformProg (Prog consts funs) =
  modifyNameSource $ runState (runReaderT m mempty)
  where ExtractM m = do
          consts' <- transformStms consts
          funs' <- inScopeOf consts' $ mapM transformFunDef funs
          return $ Prog consts' funs'

extractMulticore :: Pass SOACS MC
extractMulticore =
  Pass { passName = "extract multicore parallelism"
       , passDescription = "Extract multicore parallelism"
       , passFunction = transformProg
       }
