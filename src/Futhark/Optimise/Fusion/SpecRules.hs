{-# LANGUAGE Strict #-}

-- | Implements a specialized rule for fusing a pattern
--   formed by a map o flatten o scatter, i.e., 
--      let (inds,   vals) = map-nest f inps
--          (finds, fvals) = (flatten inds, flatten vals)
--          let res = scatter res0 finds fvals
--   where inds & vals have higher rank than finds & fvals.
--   
module Futhark.Optimise.Fusion.SpecRules (ruleMFScat) where

-- import Control.Applicative
-- import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.State
import Data.Graph.Inductive.Graph qualified as G
-- import Data.Graph.Inductive.Query.DFS qualified as Q
import Data.List qualified as L
-- import Data.Map.Strict qualified as M
import qualified Data.Set as S
import Data.Maybe
-- import Futhark.Analysis.Alias qualified as Alias
import Futhark.Analysis.HORep.SOAC qualified as H
-- import Futhark.Analysis.HORep.MapNest qualified as MapNest
import Futhark.Construct
-- import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as F
-- import Futhark.IR.SOACS.Simplify (simplifyLambda)
import Futhark.Optimise.Fusion.GraphRep
-- import Futhark.Optimise.Fusion.TryFusion qualified as TF
-- import Futhark.Pass
-- import Futhark.Transform.Rename
-- import Futhark.Transform.Substitute
import Futhark.Tools

import Debug.Trace

se0 :: SubExp
se0 = intConst Int64 0

se1 :: SubExp
se1 = intConst Int64 1

empty_aux :: StmAux ()
empty_aux = StmAux mempty mempty mempty

type IotaInp = ((VName, LParam SOACS), (SubExp, SubExp, SubExp, IntType))
-- ^           ((array-name, lambda param), (len, start, stride, Int64))
type RshpInp = ((VName, LParam SOACS), (Shape, Shape, Type))
-- ^           ((array-name, lambda param), (flat-shape, unflat-shape, elem-type))

-- | Wrapper function for constructing the body of the withAcc
--   translation of the scatter
mkWithAccBdy :: (HasScope SOACS m, MonadFreshNames m) =>
                Shape -> [IotaInp] -> [RshpInp] ->
                [(LParam SOACS, (Shape, Int, VName))] ->
                Lambda SOACS           -> 
                m (Body SOACS)
mkWithAccBdy shp iota_inps rshp_inps cons_params_outs scat_lam = do
  let cons_ps = map fst cons_params_outs
      scat_res_info = map snd cons_params_outs
      static_arg = (iota_inps, rshp_inps, scat_res_info, scat_lam)
      mkParam ((nm,_),(_,s,t)) = Param mempty nm (arrayOfShape t s)
      rshp_ps = map mkParam rshp_inps
  mkWithAccBdy' static_arg (shapeDims shp) [] [] rshp_ps cons_ps

-- | builds a body that essentially consists of a map-nest with accumulators,
--   i.e., one level for each level of the unflatten shape of scatter's reshaped
--   input arrays
mkWithAccBdy':: (HasScope SOACS m, MonadFreshNames m) =>
                ([IotaInp], [RshpInp], [(Shape, Int, VName)], Lambda SOACS) ->
                [SubExp] -> [SubExp] -> [VName]  ->
                [LParam SOACS] -> [LParam SOACS] ->
                m (Body SOACS)
-- | the base case below addapts the scatter's lambda
mkWithAccBdy' static_arg [] dims_rev iot_par_nms rshp_ps cons_ps = do
  let (iota_inps, rshp_inps, scat_res_info, scat_lam) = static_arg
      tp_int = Prim $ IntType Int64
  scope <- askScope
  runBodyBuilder $ do
    buildBody_ . localScope (scope <> scopeOfLParams (rshp_ps ++ cons_ps)) $ do
      -- handle iota args
      let strides_rev = scanl (*) (pe64 se1) $ map pe64 dims_rev
          strides = tail $ reverse strides_rev
          prods   = zipWith (*) (map le64 iot_par_nms) strides
          i_pe    = foldl (+) (pe64 se0) prods
      i_norm <- letExp "iota_norm_arg" =<< toExp i_pe
      _ <- forM iota_inps $ \ arg -> do
            let ((_, i_par), (_,b,s,_)) = arg
            i_new <- letExp "tmp" =<< toExp (pe64 b + le64 i_norm * pe64 s)
            addStm $ Let (Pat [PatElem (paramName i_par) tp_int]) empty_aux $ BasicOp $ SubExp $ Var i_new
      -- handle rshp args
      let rshp_lam_args = map (snd . fst) rshp_inps
      _ <- forM (zip rshp_lam_args rshp_ps) $ \ (old_par, new_par) -> do
            let pat = Pat [PatElem (paramName old_par) (paramDec old_par)]
            addStm $ Let pat empty_aux $ BasicOp $ SubExp $ Var $ paramName new_par
      -- add the body of the scatter's lambda
      mapM_ addStm $ bodyStms $ lambdaBody scat_lam
      -- add the withAcc update statements
      let iv_ses = groupScatterResults' scat_res_info $ bodyResult $ lambdaBody scat_lam
      res_nms <-
        forM (zip cons_ps iv_ses) $ \ (cons_p, (i_ses, v_se)) -> do -- i_ses is a list
          let f nm_in i_se =
               letExp (baseString nm_in) $ BasicOp $ UpdateAcc Unsafe nm_in [resSubExp i_se] [resSubExp v_se]
          foldM f (paramName cons_p) i_ses
      let lam_certs = foldl (<>) mempty $ map resCerts $ bodyResult $ lambdaBody scat_lam
      pure $ map (\nm -> SubExpRes lam_certs (Var nm)) res_nms
-- | the recursive case builds a call to a map soac.
mkWithAccBdy' static_arg (dim:dims) dims_rev iot_par_nms rshp_ps cons_ps = do
  scope <- askScope
  runBodyBuilder $ do
    buildBody_ . localScope (scope <> scopeOfLParams (rshp_ps ++ cons_ps)) $ do
      iota_arr <- letExp "iota_arr" $ BasicOp $ Iota dim se0 se1 Int64
      iota_p   <- newParam "iota_arg" $ Prim $ IntType Int64
      rshp_ps' <- forM (zip [0..length rshp_ps-1] (map paramDec rshp_ps)) $
        \ (i, arr_tp) ->
          newParam ("rshp_arg_" ++ show i) $ stripArray 1 arr_tp
      cons_ps' <- forM (zip [0..length cons_ps-1] (map paramDec cons_ps)) $
        \ (i, arr_tp) ->
          newParam ("acc_arg_" ++ show i) arr_tp
      map_lam_bdy <- 
          mkWithAccBdy' static_arg dims (dim:dims_rev) (iot_par_nms ++ [paramName iota_p]) rshp_ps' cons_ps'
      let map_lam  = Lambda (rshp_ps' ++ [iota_p] ++ cons_ps') (map paramDec cons_ps') map_lam_bdy
          map_inps = map paramName rshp_ps ++ [iota_arr] ++ map paramName cons_ps
          map_soac = F.Screma dim map_inps $ ScremaForm [] [] map_lam
      res_nms <- letTupExp "acc_res" $ Op map_soac
      pure $ map (subExpRes . Var) res_nms

-- | produces the withAcc statement that constitutes the translation of
--   the scater o flatten o map composition in which the map inputs are
--   reshaped in the same way
mkWithAccStm :: (HasScope SOACS m, MonadFreshNames m) =>
                [IotaInp] -> [RshpInp] ->
                [(PatElem (LetDec SOACS), (Shape, Int, VName))] ->
                StmAux (ExpDec SOACS)  ->
                Lambda SOACS           ->
                m (Stm SOACS)
mkWithAccStm iota_inps rshp_inps cons_patels_outs scatter_aux scatter_lam
  -- iotas are assumed to operate on Int64 values
  -- ToDo: maybe simplify rshp_inps
  --       check that the unflat shape is the same across reshapes
  --       check that the rank of the unflatten shape is higher than the flatten 
  | rshp_inp : _ <- rshp_inps,
    (_,(_,s_unflat,_)) <- rshp_inp,
    (_: _) <- shapeDims s_unflat = do
  --
  (cert_params, acc_params) <- fmap unzip $
    forM cons_patels_outs $ \(patel, (shp, _, nm)) -> do
      cert_param <- newParam "acc_cert_p" $ Prim Unit
      let arr_tp = patElemType patel
          acc_tp = stripArray (shapeRank shp) arr_tp
      acc_param <- newParam (baseString nm) $
                    Acc (paramName cert_param) shp [acc_tp] NoUniqueness
      pure(cert_param, acc_param)
  let cons_params_outs = zip acc_params $ map snd cons_patels_outs
  acc_bdy <- mkWithAccBdy s_unflat iota_inps rshp_inps cons_params_outs scatter_lam
  let withacc_lam  = Lambda (cert_params ++ acc_params) (map paramDec acc_params) acc_bdy
      withacc_inps = map (\ (_,(shp,_,nm)) -> (shp,[nm],Nothing)) cons_patels_outs
      withacc_pat  = Pat $ map fst cons_patels_outs
      stm = Let withacc_pat scatter_aux $
            WithAcc withacc_inps withacc_lam
  return stm
mkWithAccStm _ _ _ _ _ =
  error "Unreachable case reached!"

checkSafeAndProfitable :: DepGraph -> G.Node -> [DepContext] -> [DepContext] -> Bool
checkSafeAndProfitable dg scat_node_id ctxs_rshp ctxs_cons =
  let all_deps = concatMap (\(x,_,_,_) -> x) $ ctxs_rshp ++ ctxs_cons
      prof1  = all (\(_,dep_id) -> dep_id == scat_node_id) all_deps
      -- ^ scatter is the sole target to all consume & unflatten-reshape deps
      (_, map_node_id, map_nT,_) = head ctxs_rshp
      prof2 = all (\(_,nid,_,_) -> nid == map_node_id) ctxs_rshp
      prof3 = isMap map_nT
      -- ^ all reshapes come from the same node, which is a map
      safe  = vFusionFeasability dg map_node_id scat_node_id
  in  safe && prof1 && prof2 && prof3
  where
    isMap nT
      | SoacNode out_trsfs _pat soac _ <- nT,
        H.Screma _ form _  <- soac,
        ScremaForm [] [] _ <- form =
      H.nullTransforms out_trsfs
    isMap _ = False
      
ruleMFScat :: (HasScope SOACS m, MonadFreshNames m) =>
              DepNode -> DepGraph -> m (Maybe DepGraph)
ruleMFScat node_to_fuse dg@DepGraph {dgGraph = g}
  | (_, soac_nodeT) <- node_to_fuse,
    scat_node_id <- nodeFromLNode node_to_fuse,
    SoacNode node_out_trsfs scat_pat scat_soac scat_aux <- soac_nodeT,
    H.nullTransforms node_out_trsfs,
    -- ^ for simplicity we do not allow transforms on scatter's result.
    H.Scatter _len scat_lam scat_inp scat_out <- scat_soac,
    -- ^ get the scatter
    scat_trsfs <- map H.inputTransforms (H.inputs scat_soac),
    -- ^ get the transforms on the input
--    [pat_el_nm] <- map patElemName (patElems scat_pat),
--    "scatter_res_9806" == prettyString pat_el_nm,
    any (/= mempty) scat_trsfs,
    scat_ctx <- G.context g scat_node_id,
    (out_deps, _, _, inp_deps) <- scat_ctx,
    cons_deps <- filter (isCons . fst) inp_deps,
    drct_deps <- filter (isDep  . fst) inp_deps,
    cons_ctxs <- map (G.context g . snd) cons_deps,
    drct_ctxs <- map (G.context g . snd) drct_deps,
    -- ^ ToDo check profitability, i.e., fusion opportunities with at least one context
    cons_nTs  <- map getNodeTfromCtx cons_ctxs,  -- not used!!
--    drct_nTs  <- map getNodeTfromCtx drct_ctxs,
    drct_tups0<- mapMaybe (pairUp (zip drct_ctxs (map fst drct_deps))) scat_inp,
    length drct_tups0 == length scat_inp,
    -- ^ checks that all direct dependencies are also array
    --   inputs to scatter
    (t1s,t2s) <- unzip drct_tups0,
    drct_tups <- zip t1s $ zip t2s (lambdaParams scat_lam),
    (ctxs_iots,drct_iots)<- unzip $ filter (isIota . snd . fst . snd) drct_tups,
    (ctxs_rshp,drct_rshp) <- unzip $ filter (not . isIota . snd . fst . snd) drct_tups,
    length drct_iots + length drct_rshp == length scat_inp,
    -- ^ direct dependencies are either flatten reshapes or iotas.
    rep_iotas <- mapMaybe getRepIota drct_iots,
    length rep_iotas == length drct_iots,
    rep_rshps_certs <- mapMaybe getRepRshpArr drct_rshp,
    (rep_rshps, certs_rshps) <- unzip rep_rshps_certs,
    not (null rep_rshps),
    -- ^ at least a flatten-reshape array
    length rep_rshps == length drct_rshp,
    (_,(s1,s2,_)) : _ <- rep_rshps,
    all (\(_,(s1',s2',_)) -> s1 == s1' && s2 == s2') rep_rshps,
    -- ^ Check that all unflatten shape diffs are the same,
    --   so that we can construct a map nest;
    --   in principle, we can also shorted the representation.
    checkSafeAndProfitable dg scat_node_id ctxs_rshp cons_ctxs
  = do
  -- generate the withAcc statement
  let cons_patels_outs = zip (patElems scat_pat) scat_out
  wacc_stm <- mkWithAccStm rep_iotas rep_rshps cons_patels_outs scat_aux scat_lam
  let all_cert_rshp = foldl (<>) mempty certs_rshps
      aux = stmAux wacc_stm
      aux' = aux { stmAuxCerts = all_cert_rshp <> stmAuxCerts aux }
      wacc_stm' = wacc_stm { stmAux = aux' }
  -- get the input deps of iotas
      fiot acc (_,_,_,inp_deps_iot) =
        acc <> inp_deps_iot
      deps_of_iotas = foldl fiot mempty ctxs_iots
      --
      iota_nms = S.fromList $ map (fst . fst) rep_iotas
      notIota (dep, _) = not $ S.member (getName dep) iota_nms
      inp_deps_wo_iotas = filter notIota inp_deps
  -- generate a new node for the with-acc-stmt and its associated context
  -- add the inpdeps of iotas but remove the iota themselves from deps.
      new_withacc_nT  = StmNode wacc_stm'
      inp_deps' = inp_deps_wo_iotas <> deps_of_iotas
      new_withacc_ctx = (out_deps, scat_node_id, new_withacc_nT, inp_deps')
      
      -- do we need to use `fusedSomething` ??
      new_node = G.node' new_withacc_ctx
      dg' = dg {dgGraph = new_withacc_ctx G.& G.delNodes [new_node] g}
      
  trace ("\nCOSMIN scatter:\n" ++ prettyString scat_soac ++
           "\n scat_out: " ++ prettyString scat_out ++ "\n" ++
           "\n scat inps: " ++ show scat_inp ++
           "\nInput-Transforms: " ++ show scat_trsfs ++
           "\nScat-Out-Transforms: " ++ show node_out_trsfs ++
           "\nPattern: " ++ prettyString scat_pat ++
           "\nAux: " ++ show scat_aux ++
           "\nScatter-Context: " ++ show scat_ctx ++
           "\nCons NodeT: " ++ L.intercalate "\n\t" (map printNodeT cons_nTs) ++
        -- "\nDDep NodeT: " ++ L.intercalate "\n\t" (map printNodeT drct_nTs) ++
           "\nDirect-Tuples: " ++ show drct_tups ++
           "\nDirect-Rshps: " ++ show drct_rshp ++
           "\nIota-Rep: " ++ show rep_iotas ++
           "\nRshp-Rep: " ++ show rep_rshps ++
           "\n\n\nWithAccStmt:\n" ++ prettyString wacc_stm' ++
           "\n\nDrct ctxts: " ++ show drct_ctxs ++
           "\n\nInp-Deps-Result: " ++ show inp_deps' ++
           "\nIota-Ctx: " ++ show ctxs_iots ++
           "\n\n\nNew-DG: " ++ pprg dg'
        ) $
    pure $ Just dg'
  where
    --
    getNodeTfromCtx (_, _, nT, _) = nT
    printNodeT (StmNode stm) = prettyString stm
    printNodeT (SoacNode trsf pat soac aux) =
      prettyString pat  ++ " = " ++
      prettyString soac ++ "\n"  ++
      "\n\t\tAux: " ++ show aux ++
      "\n\t\tTransforms: " ++ show trsf
    printNodeT _ = "Unimplemented Print NodeT"
    findCtxOf ctxes nm
      | [ctxe] <- filter (\x -> nm == getName (snd x)) ctxes =
        Just ctxe
    findCtxOf _ _ = Nothing
    pairUp :: [(DepContext,EdgeT)] -> H.Input -> Maybe (DepContext,(H.Input, NodeT))
    pairUp ctxes inp@(H.Input _arrtrsfs nm _tp)
      | Just (ctx@(_,_,nT,_),_) <- findCtxOf ctxes nm =
          Just (ctx, (inp,nT))
    pairUp _ _ = Nothing
    isIota :: NodeT -> Bool
    isIota (StmNode (Let _ _ (BasicOp (Iota _ _ _ _)))) = True
    isIota _ = False
    getRepIota :: ((H.Input, NodeT), LParam SOACS) -> 
                  Maybe ((VName, LParam SOACS), (SubExp, SubExp, SubExp, IntType))
    getRepIota ((H.Input iottrsf arr_nm _arr_tp, nt), farg)
      | mempty == iottrsf,
        StmNode (Let _ _ (BasicOp (Iota n x s Int64))) <- nt =
      Just ((arr_nm, farg), (n, x, s, Int64))
    getRepIota _ = Nothing
    getRepRshpArr :: ((H.Input, NodeT), LParam SOACS) -> 
                  Maybe (((VName, LParam SOACS), (Shape, Shape, Type)), Certs)
    getRepRshpArr ((H.Input outtrsf arr_nm arr_tp, _nt), farg)
      | rshp_trsfm H.:< other_trsfms <- H.viewf outtrsf,
        (H.Reshape c ReshapeArbitrary shp_flat) <- rshp_trsfm,
        other_trsfms == mempty,
        eltp <- paramDec farg,
        Just shp_flat' <- checkShp eltp shp_flat,
        Array _ptp shp_unflat _ <- arr_tp,
        Just shp_unflat' <- checkShp eltp shp_unflat,
        shapeRank shp_flat' == 1,
        shapeRank shp_flat' < shapeRank shp_unflat' =
      Just (((arr_nm, farg), (shp_flat', shp_unflat', eltp)), c)
    -- ^ representation is ((arr_name, farg of lam), (flat_shp_diff, unflat_shp_diff, elem-type))
    getRepRshpArr _ = Nothing
    checkShp (Prim _) shp_arr = Just shp_arr
    checkShp (Array _ptp shp_elm _) shp_arr =
      let dims_elm = shapeDims shp_elm
          dims_arr = shapeDims shp_arr
          (m, n) = (length dims_elm, length dims_arr)
          shp' = Shape $ take (n-m) dims_arr
          dims_com = drop (n-m) dims_arr
      in  if all (\(x,y) -> x==y) (zip dims_com dims_elm)
          then Just shp'
          else Nothing
    checkShp _ _ = Nothing
ruleMFScat _ _ = return Nothing
    
{--
    node_to_fuse_id <- nodeFromLNode node_to_fuse,
    (Just (dep_on_scat, scat_node, scat_lab, scat_dep_on), g') <- G.match node_to_fuse_id g,
    [(scat_v_e, scat_v_n), (_cons_e,_cons_n), (_dcons_e, _dcons_n), (_scat_i_e,_scat_i_n), (_asrt_e,_asrt_n), (_scat_l_e,_scat_len_n) ] <- scat_dep_on,
    node_to_fuse_id == scat_node,
    (Just ctx_rshp@(dep_on_val, val_node, val_lab, val_dep_on), g'') <- G.match scat_v_n g',
    null dep_on_val,
    (_, rshp_nodeT) <- G.labNode' ctx_rshp,
    TransNode rshp_res trsf rshp_inp <- rshp_nodeT,
    _ : (_map_e, map_n) : _ <- val_dep_on,
    (Just ctx_map@(dep_on_map, map_node, map_lab, map_dep_on), _g''') <- G.match map_n g'',
    (_, map_nodeT) <- G.labNode' ctx_map,
    SoacNode _soac_trsf map_pat map_soac _aux <- map_nodeT,
    --
    trace ("\nCOSMIN scatter:\n" ++ prettyString scat_soac ++
           "\nG.lpre: " ++ show (G.lpre g node_to_fuse_id) ++
           "\ntransforms:\n" ++ show scat_trsfs ++
           "\nPattern: " ++ prettyString scat_pat ++
           "\nAux: " ++ show scat_aux ++
           "\nScat-node-id: " ++ show node_to_fuse_id ++
           "\nContext: " ++ show dep_on_scat ++ " | " ++
               show scat_node ++ " | " ++ show scat_lab ++
               show scat_dep_on ++
           "\nMatching node: " ++ show scat_v_e ++
             " deps_on_val: " ++ show dep_on_val ++
             " node: " ++ show val_node ++ " " ++ show val_lab ++
             " val_dep_on: " ++ show val_dep_on ++
           "\nReshape-Stmt: " ++ prettyString rshp_res ++ " " ++ show trsf ++ " " ++ prettyString rshp_inp ++
           "\nMatching map node: " ++ show map_lab ++ " of id: " ++ show map_node ++
             " deps_on_map: " ++ show dep_on_map ++
             -- " node: " ++ show map_node ++ " " ++ show map_lab ++
             " map_dep_on: " ++ show map_dep_on ++
           "\nMap-Stmt:\n" ++ prettyString map_pat ++ " = " ++
              "\n" ++ prettyString map_soac ++ 
           "\nDG: " ++ pprg dg
          ) $ False = do
--}

------------------------------
---  clones from Fusion.hs ---
------------------------------

vFusionFeasability :: DepGraph -> G.Node -> G.Node -> Bool
vFusionFeasability dg@DepGraph {dgGraph = g} n1 n2 =
  not (any isInf (edgesBetween dg n1 n2))
    && not (any (reachable dg n2) (filter (/= n2) (G.pre g n1)))

