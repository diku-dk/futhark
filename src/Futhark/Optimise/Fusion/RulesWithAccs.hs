{-# LANGUAGE Strict #-}

-- | This module consists of rules for fusion
--     that involves WithAcc constructs.
--   Currently, we support two non-trivial
--   transformations:
--     I. map-flatten-scatter: a map nest produces
--        multi-dimensional index and values arrays
--        that are then flattened and used in a
--        scatter consumer. Such pattern can be fused
--        by re-writing the scatter by means of a WithAcc
--        containing a map-nest, thus eliminating the flatten
--        operations. The obtained WithAcc can then be fused
--        with the producer map nest, e.g., benefiting intra-group
--        kernels. The eloquent target for this rule is
--        an efficient implementation of radix-sort.
--
--    II. WithAcc-WithAcc fusion: two withaccs can be
--        fused as long as the common accumulators use
--        the same operator, and as long as the non-accumulator
--        input of an WithAcc is not used as an accumulator in
--        the other. This fusion opens the door for fusing
--        the SOACs appearing inside the WithAccs. This is
--        also intended to demonstrate that it is not so
--        important where exactly the WithAccs were originally
--        introduced in the code, it is more important that
--        they can be transformed by various optimizations passes.
module Futhark.Optimise.Fusion.RulesWithAccs
  ( ruleMFScat,
    tryFuseWithAccs,
  )
where

import Control.Monad
import Data.Graph.Inductive.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as F
import Futhark.Optimise.Fusion.GraphRep
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute

se0 :: SubExp
se0 = intConst Int64 0

se1 :: SubExp
se1 = intConst Int64 1

emptyAux :: StmAux ()
emptyAux = StmAux mempty mempty mempty

-------------------------------------
--- I. Map-Flatten-Scatter Fusion ---
-------------------------------------

-- helper data structures
type IotaInp = ((VName, LParam SOACS), (SubExp, SubExp, SubExp, IntType))
-- ^           ((array-name, lambda param), (len, start, stride, Int64))

type RshpInp = ((VName, LParam SOACS), (Shape, Shape, Type))
-- ^           ((array-name, lambda param), (flat-shape, unflat-shape, elem-type))

-- | Implements a specialized rule for fusing a pattern
--   formed by a map o flatten o scatter, i.e.,
--      let (inds,   vals) = map-nest f inps
--          (finds, fvals) = (flatten inds, flatten vals)
--          let res = scatter res0 finds fvals
--   where inds & vals have higher rank than finds & fvals.
ruleMFScat ::
  (HasScope SOACS m, MonadFreshNames m) =>
  DepNode ->
  DepGraph ->
  m (Maybe DepGraph)
ruleMFScat node_to_fuse dg@DepGraph {dgGraph = g}
  | soac_nodeT <- snd node_to_fuse,
    scat_node_id <- nodeFromLNode node_to_fuse,
    SoacNode node_out_trsfs scat_pat scat_soac scat_aux <- soac_nodeT,
    H.nullTransforms node_out_trsfs,
    -- \^ for simplicity we do not allow transforms on scatter's result.
    H.Scatter _len scat_inp scat_out scat_lam <- scat_soac,
    -- \^ get the scatter
    scat_trsfs <- map H.inputTransforms (H.inputs scat_soac),
    -- \^ get the transforms on the input
    -- trace ("\n\n!!!!!!!!!! Fusion-Map-Flat-Scatter !!!!!!!!!!!!\n\n") $
    any (/= mempty) scat_trsfs,
    scat_ctx <- G.context g scat_node_id,
    (out_deps, _, _, inp_deps) <- scat_ctx,
    cons_deps <- filter (isCons . fst) inp_deps,
    drct_deps <- filter (isDep . fst) inp_deps,
    cons_ctxs <- map (G.context g . snd) cons_deps,
    drct_ctxs <- map (G.context g . snd) drct_deps,
    _cons_nTs <- map getNodeTfromCtx cons_ctxs, -- not used!!
    drct_tups0 <- mapMaybe (pairUp (zip drct_ctxs (map fst drct_deps))) scat_inp,
    length drct_tups0 == length scat_inp,
    -- \^ checks that all direct dependencies are also array
    --   inputs to scatter
    (t1s, t2s) <- unzip drct_tups0,
    drct_tups <- zip t1s $ zip t2s (lambdaParams scat_lam),
    (ctxs_iots, drct_iots) <- unzip $ filter (isIota . snd . fst . snd) drct_tups,
    (ctxs_rshp, drct_rshp) <- unzip $ filter (not . isIota . snd . fst . snd) drct_tups,
    length drct_iots + length drct_rshp == length scat_inp,
    -- \^ direct dependencies are either flatten reshapes or iotas.
    rep_iotas <- mapMaybe getRepIota drct_iots,
    length rep_iotas == length drct_iots,
    rep_rshps_certs <- mapMaybe getRepRshpArr drct_rshp,
    (rep_rshps, certs_rshps) <- unzip rep_rshps_certs,
    -- \^ gather the representations for the iotas and reshapes, that use
    --   the helper types `IotaInp` and `RshpInp`
    not (null rep_rshps),
    -- \^ at least one flatten-reshaped array
    length rep_rshps == length drct_rshp,
    (_, (s1, s2, _)) : _ <- rep_rshps,
    all (\(_, (s1', s2', _)) -> s1 == s1' && s2 == s2') rep_rshps,
    -- \^ Check that all unflatten shape dimensions are the same,
    --   so that we can construct a map nest;
    checkSafeAndProfitable dg scat_node_id ctxs_rshp cons_ctxs =
      -- \^ check profitability, which is conservatively defined as
      --   all the reshaped and consumer arrays are used solely
      --   by the scatter AND all reshape dependencies originate
      --   in the same map.
      do
        -- generate the withAcc statement
        let cons_patels_outs = zip (patElems scat_pat) scat_out
        wacc_stm <- mkWithAccStm rep_iotas rep_rshps cons_patels_outs scat_aux scat_lam
        let all_cert_rshp = foldl (<>) mempty certs_rshps
            aux = stmAux wacc_stm
            aux' = aux {stmAuxCerts = all_cert_rshp <> stmAuxCerts aux}
            wacc_stm' = wacc_stm {stmAux = aux'}
            -- get the input deps of iotas
            fiot acc (_, _, _, inp_deps_iot) =
              acc <> inp_deps_iot
            deps_of_iotas = foldl fiot mempty ctxs_iots
            --
            iota_nms = namesFromList $ map (fst . fst) rep_iotas
            inp_deps_wo_iotas = filter ((`notNameIn` iota_nms) . getName . fst) inp_deps
            -- generate a new node for the with-acc-stmt and its associated context:
            --   add the inp-deps of iotas but remove the iota themselves from deps.
            new_withacc_nT = StmNode wacc_stm'
            inp_deps' = inp_deps_wo_iotas <> deps_of_iotas
            new_withacc_ctx = (out_deps, scat_node_id, new_withacc_nT, inp_deps')
            -- construct the new WithAcc node/graph; do we need to use `fusedSomething` ??
            new_node = G.node' new_withacc_ctx
            dg' = dg {dgGraph = new_withacc_ctx G.& G.delNodes [new_node] g}
        -- result
        pure $ Just dg'
  where
    {--
      trace ("\nCOSMIN debug\n input scatter:\n" ++ prettyString scat_soac ++
               "\n scat_out: " ++ prettyString scat_out ++ "\n" ++
               "\n scat inps: " ++ show scat_inp ++
               "\nInput-Transforms: " ++ show scat_trsfs ++
               "\nScat-Out-Transforms: " ++ show node_out_trsfs ++
               "\nPattern: " ++ prettyString scat_pat ++
               "\nAux: " ++ show scat_aux ++
               "\n\n\nResult WithAccStmt:\n" ++ prettyString wacc_stm'
            ) $
    --}

    --
    getNodeTfromCtx (_, _, nT, _) = nT
    findCtxOf ctxes nm
      | [ctxe] <- filter (\x -> nm == getName (snd x)) ctxes =
          Just ctxe
    findCtxOf _ _ = Nothing
    pairUp :: [(DepContext, EdgeT)] -> H.Input -> Maybe (DepContext, (H.Input, NodeT))
    pairUp ctxes inp@(H.Input _arrtrsfs nm _tp)
      | Just (ctx@(_, _, nT, _), _) <- findCtxOf ctxes nm =
          Just (ctx, (inp, nT))
    pairUp _ _ = Nothing
    --
    isIota :: NodeT -> Bool
    isIota (StmNode (Let _ _ (BasicOp (Iota {})))) = True
    isIota _ = False
    --
    getRepIota :: ((H.Input, NodeT), LParam SOACS) -> Maybe IotaInp
    getRepIota ((H.Input iottrsf arr_nm _arr_tp, nt), farg)
      | mempty == iottrsf,
        StmNode (Let _ _ (BasicOp (Iota n x s Int64))) <- nt =
          Just ((arr_nm, farg), (n, x, s, Int64))
    getRepIota _ = Nothing
    --
    getRepRshpArr :: ((H.Input, NodeT), LParam SOACS) -> Maybe (RshpInp, Certs)
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
    getRepRshpArr _ = Nothing
    --
    checkShp (Prim _) shp_arr = Just shp_arr
    checkShp (Array _ptp shp_elm _) shp_arr =
      let dims_elm = shapeDims shp_elm
          dims_arr = shapeDims shp_arr
          (m, n) = (length dims_elm, length dims_arr)
          shp' = Shape $ take (n - m) dims_arr
          dims_com = drop (n - m) dims_arr
       in if all (uncurry (==)) (zip dims_com dims_elm)
            then Just shp'
            else Nothing
    checkShp _ _ = Nothing
-- default fails:
ruleMFScat _ _ = pure Nothing

checkSafeAndProfitable :: DepGraph -> G.Node -> [DepContext] -> [DepContext] -> Bool
checkSafeAndProfitable dg scat_node_id ctxs_rshp@(_ : _) ctxs_cons =
  let all_deps = concatMap (\(x, _, _, _) -> x) $ ctxs_rshp ++ ctxs_cons
      prof1 = all (\(_, dep_id) -> dep_id == scat_node_id) all_deps
      -- \^ scatter is the sole target to all consume & unflatten-reshape deps
      (_, map_node_id, map_nT, _) = head ctxs_rshp
      prof2 = all (\(_, nid, _, _) -> nid == map_node_id) ctxs_rshp
      prof3 = isMap map_nT
      -- \^ all reshapes come from the same node, which is a map
      safe = vFusionFeasability dg map_node_id scat_node_id
   in safe && prof1 && prof2 && prof3
  where
    isMap nT
      | SoacNode out_trsfs _pat soac _ <- nT,
        H.Screma _ _ form <- soac,
        ScremaForm [] [] _ <- form =
          H.nullTransforms out_trsfs
    isMap _ = False
checkSafeAndProfitable _ _ _ _ = False

-- | produces the withAcc statement that constitutes the translation of
--   the scater o flatten o map composition in which the map inputs are
--   reshaped in the same way
mkWithAccStm ::
  (HasScope SOACS m, MonadFreshNames m) =>
  [IotaInp] ->
  [RshpInp] ->
  [(PatElem (LetDec SOACS), (Shape, Int, VName))] ->
  StmAux (ExpDec SOACS) ->
  Lambda SOACS ->
  m (Stm SOACS)
mkWithAccStm iota_inps rshp_inps cons_patels_outs scatter_aux scatter_lam
  -- iotas are assumed to operate on Int64 values
  -- ToDo: maybe simplify rshp_inps
  --       check that the unflat shape is the same across reshapes
  --       check that the rank of the unflatten shape is higher than the flatten
  | rshp_inp : _ <- rshp_inps,
    (_, (_, s_unflat, _)) <- rshp_inp,
    (_ : _) <- shapeDims s_unflat = do
      --
      (cert_params, acc_params) <- fmap unzip $
        forM cons_patels_outs $ \(patel, (shp, _, nm)) -> do
          cert_param <- newParam "acc_cert_p" $ Prim Unit
          let arr_tp = patElemType patel
              acc_tp = stripArray (shapeRank shp) arr_tp
          acc_param <-
            newParam (baseString nm) $
              Acc (paramName cert_param) shp [acc_tp] NoUniqueness
          pure (cert_param, acc_param)
      let cons_params_outs = zip acc_params $ map snd cons_patels_outs
      acc_bdy <- mkWithAccBdy s_unflat iota_inps rshp_inps cons_params_outs scatter_lam
      let withacc_lam =
            Lambda
              { lambdaParams = cert_params ++ acc_params,
                lambdaReturnType = map paramDec acc_params,
                lambdaBody = acc_bdy
              }
          withacc_inps = map (\(_, (shp, _, nm)) -> (shp, [nm], Nothing)) cons_patels_outs
          withacc_pat = Pat $ map fst cons_patels_outs
          stm =
            Let withacc_pat scatter_aux $
              WithAcc withacc_inps withacc_lam
      pure stm
mkWithAccStm _ _ _ _ _ =
  error "Unreachable case reached!"

-- | Wrapper function for constructing the body of the withAcc
--   translation of the scatter
mkWithAccBdy ::
  (HasScope SOACS m, MonadFreshNames m) =>
  Shape ->
  [IotaInp] ->
  [RshpInp] ->
  [(LParam SOACS, (Shape, Int, VName))] ->
  Lambda SOACS ->
  m (Body SOACS)
mkWithAccBdy shp iota_inps rshp_inps cons_params_outs scat_lam = do
  let cons_ps = map fst cons_params_outs
      scat_res_info = map snd cons_params_outs
      static_arg = (iota_inps, rshp_inps, scat_res_info, scat_lam)
      mkParam ((nm, _), (_, s, t)) = Param mempty nm (arrayOfShape t s)
      rshp_ps = map mkParam rshp_inps
  mkWithAccBdy' static_arg (shapeDims shp) [] [] rshp_ps cons_ps

-- | builds a body that essentially consists of a map-nest with accumulators,
--   i.e., one level for each level of the unflatten shape of scatter's reshaped
--   input arrays
mkWithAccBdy' ::
  (HasScope SOACS m, MonadFreshNames m) =>
  ([IotaInp], [RshpInp], [(Shape, Int, VName)], Lambda SOACS) ->
  [SubExp] ->
  [SubExp] ->
  [VName] ->
  [LParam SOACS] ->
  [LParam SOACS] ->
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
          prods = zipWith (*) (map le64 iot_par_nms) strides
          i_pe = foldl (+) (pe64 se0) prods
      i_norm <- letExp "iota_norm_arg" =<< toExp i_pe
      forM_ iota_inps $ \arg -> do
        let ((_, i_par), (_, b, s, _)) = arg
        i_new <- letExp "tmp" =<< toExp (pe64 b + le64 i_norm * pe64 s)
        addStm $ Let (Pat [PatElem (paramName i_par) tp_int]) emptyAux $ BasicOp $ SubExp $ Var i_new
      -- handle rshp args
      let rshp_lam_args = map (snd . fst) rshp_inps
      forM_ (zip rshp_lam_args rshp_ps) $ \(old_par, new_par) -> do
        let pat = Pat [PatElem (paramName old_par) (paramDec old_par)]
        addStm $ Let pat emptyAux $ BasicOp $ SubExp $ Var $ paramName new_par
      -- add the body of the scatter's lambda
      mapM_ addStm $ bodyStms $ lambdaBody scat_lam
      -- add the withAcc update statements
      let iv_ses = groupScatterResults' scat_res_info $ bodyResult $ lambdaBody scat_lam
      res_nms <-
        forM (zip cons_ps iv_ses) $ \(cons_p, (i_ses, v_se)) -> do
          -- i_ses is a list
          let f nm_in i_se =
                letExp (baseString nm_in) $ BasicOp $ UpdateAcc Safe nm_in [resSubExp i_se] [resSubExp v_se]
          foldM f (paramName cons_p) i_ses
      let lam_certs = foldl (<>) mempty $ map resCerts $ bodyResult $ lambdaBody scat_lam
      pure $ map (SubExpRes lam_certs . Var) res_nms
-- \| the recursive case builds a call to a map soac.
mkWithAccBdy' static_arg (dim : dims) dims_rev iot_par_nms rshp_ps cons_ps = do
  scope <- askScope
  runBodyBuilder $ do
    buildBody_ . localScope (scope <> scopeOfLParams (rshp_ps ++ cons_ps)) $ do
      iota_arr <- letExp "iota_arr" $ BasicOp $ Iota dim se0 se1 Int64
      iota_p <- newParam "iota_arg" $ Prim $ IntType Int64
      rshp_ps' <- forM (zip [0 .. length rshp_ps - 1] (map paramDec rshp_ps)) $
        \(i, arr_tp) ->
          newParam ("rshp_arg_" ++ show i) $ stripArray 1 arr_tp
      cons_ps' <- forM (zip [0 .. length cons_ps - 1] (map paramDec cons_ps)) $
        \(i, arr_tp) ->
          newParam ("acc_arg_" ++ show i) arr_tp
      map_lam_bdy <-
        mkWithAccBdy' static_arg dims (dim : dims_rev) (iot_par_nms ++ [paramName iota_p]) rshp_ps' cons_ps'
      let map_lam = Lambda (rshp_ps' ++ [iota_p] ++ cons_ps') (map paramDec cons_ps') map_lam_bdy
          map_inps = map paramName rshp_ps ++ [iota_arr] ++ map paramName cons_ps
          map_soac = F.Screma dim map_inps $ ScremaForm [] [] map_lam
      res_nms <- letTupExp "acc_res" $ Op map_soac
      pure $ map (subExpRes . Var) res_nms

---------------------------------------------------
--- II. WithAcc-WithAcc Fusion
---------------------------------------------------

-- | Local helper type that tuples together:
--   1.   the pattern element corresponding to one withacc input
--   2.   the withacc input
--   3-5  withacc's lambda corresponding acc-certificate param,
--           argument param and result name
type AccTup =
  ( [PatElem (LetDec SOACS)],
    WithAccInput SOACS,
    LParam SOACS,
    LParam SOACS,
    (VName, Certs)
  )

accTup1 :: AccTup -> [PatElem (LetDec SOACS)]
accTup1 (a, _, _, _, _) = a

accTup2 :: AccTup -> WithAccInput SOACS
accTup2 (_, a, _, _, _) = a

accTup3 :: AccTup -> LParam SOACS
accTup3 (_, _, a, _, _) = a

accTup4 :: AccTup -> LParam SOACS
accTup4 (_, _, _, a, _) = a

accTup5 :: AccTup -> (VName, Certs)
accTup5 (_, _, _, _, a) = a

-- | Simple case for fusing two withAccs (can be extended):
--    let (b1, ..., bm, x1, ..., xq) = withAcc a1 ... am lam1
--    let (d1, ..., dn, y1, ..., yp) = withAcc c1 ... cn lam2
-- Notation: `b1 ... bm` are the accumulator results of the
--     first withAcc and `d1, ..., dn` of the second withAcc.
--     `x1 ... xq` and `y1, ..., yp` are non-accumulator results.
-- Conservative conditions:
--   1. for any bi (i=1..m) either `bi IN {c1, ..., cm}` OR
--        `bi NOT-IN FV(lam2)`, i.e., perfect producer-consumer
--        relation on accums. Of course the binary-op should
--        be the same.
--   2. The `bs` that are also accumulated upon in lam2
--        do NOT belong to the `infusible` set (they are destroyed)
--   3. x1 ... xq do not overlap with c1 ... cn
-- Fusion will create one withacc that accumulates on the
--   union of `a1 ... am` and `c1 ... cn` and returns, in addition
--   to the accumulator arrays the union of regular variables
--   `x1 ... xq` and `y1, ..., yp`
tryFuseWithAccs ::
  (HasScope SOACS m, MonadFreshNames m) =>
  [VName] ->
  Stm SOACS ->
  Stm SOACS ->
  m (Maybe (Stm SOACS))
tryFuseWithAccs
  infusible
  (Let pat1 aux1 (WithAcc w_inps1 lam1))
  (Let pat2 aux2 (WithAcc w_inps2 lam2))
    | (pat1_els, pat2_els) <- (patElems pat1, patElems pat2),
      (acc_tup1, other_pr1) <- groupAccs pat1_els w_inps1 lam1,
      (acc_tup2, other_pr2) <- groupAccs pat2_els w_inps2 lam2,
      (tup_common, acc_tup1', acc_tup2') <-
        groupCommonAccs acc_tup1 acc_tup2,
      -- safety 0: make sure that the accs from acc_tup1' and
      --           acc_tup2' do not overlap
      pnms_1' <- map patElemName $ concatMap (\(nms, _, _, _, _) -> nms) acc_tup1',
      winp_2' <- concatMap (\(_, (_, nms, _), _, _, _) -> nms) acc_tup2',
      not $ namesIntersect (namesFromList pnms_1') (namesFromList winp_2'),
      -- safety 1: we have already determined the commons;
      --           now we also need to check NOT-IN FV(lam2)
      not $ namesIntersect (namesFromList pnms_1') (freeIn lam2),
      -- safety 2:
      -- bs <- map patElemName $ concatMap accTup1 acc_tup1,
      bs <- map patElemName $ concatMap (accTup1 . fst) tup_common,
      all (`notElem` infusible) bs,
      -- safety 3:
      cs <- namesFromList $ concatMap ((\(_, xs, _) -> xs) . accTup2) acc_tup2,
      all ((`notNameIn` cs) . patElemName . fst) other_pr1 =
        do
          -- rest of implementation
          let getCertPairs (t1, t2) = (paramName (accTup3 t2), paramName (accTup3 t1))
              tab_certs = M.fromList $ map getCertPairs tup_common
              lam2_bdy' = substituteNames tab_certs (lambdaBody lam2)
              rcrt_params = map (accTup3 . fst) tup_common ++ map accTup3 acc_tup1' ++ map accTup3 acc_tup2'
              racc_params = map (accTup4 . fst) tup_common ++ map accTup4 acc_tup1' ++ map accTup4 acc_tup2'
              (comm_res_nms, comm_res_certs2) = unzip $ map (accTup5 . snd) tup_common
              (_, comm_res_certs1) = unzip $ map (accTup5 . fst) tup_common
              com_res_certs = zipWith (\x y -> Certs (unCerts x ++ unCerts y)) comm_res_certs1 comm_res_certs2
              bdyres_certs = com_res_certs ++ map (snd . accTup5) (acc_tup1' ++ acc_tup2')
              bdyres_accse = map Var comm_res_nms ++ map (Var . fst . accTup5) (acc_tup1' ++ acc_tup2')
              bdy_res_accs = zipWith SubExpRes bdyres_certs bdyres_accse
              bdy_res_others = map snd $ other_pr1 ++ other_pr2
          scope <- askScope
          lam_bdy <-
            runBodyBuilder $ do
              buildBody_ . localScope (scope <> scopeOfLParams (rcrt_params ++ racc_params)) $ do
                -- add the stms of lam1
                mapM_ addStm $ stmsToList $ bodyStms $ lambdaBody lam1
                -- add the copy stms for the common accumulator
                forM_ tup_common $ \(tup1, tup2) -> do
                  let (lpar1, lpar2) = (accTup4 tup1, accTup4 tup2)
                      ((nm1, _), nm2, tp_acc) = (accTup5 tup1, paramName lpar2, paramDec lpar1)
                  addStm $ Let (Pat [PatElem nm2 tp_acc]) emptyAux $ BasicOp $ SubExp $ Var nm1
                -- add copy stms to bring in scope x1 ... xq
                forM_ other_pr1 $ \(pat_elm, bdy_res) -> do
                  let (nm, se, tp) = (patElemName pat_elm, resSubExp bdy_res, patElemType pat_elm)
                      aux = emptyAux {stmAuxCerts = resCerts bdy_res}
                  addStm $ Let (Pat [PatElem nm tp]) aux $ BasicOp $ SubExp se
                -- add the statements of lam2 (in which the acc-certificates have been substituted)
                mapM_ addStm $ stmsToList $ bodyStms lam2_bdy'
                -- build the result of body
                pure $ bdy_res_accs ++ bdy_res_others
          let tp_res_other = map (patElemType . fst) (other_pr1 ++ other_pr2)
              res_lam =
                Lambda
                  { lambdaParams = rcrt_params ++ racc_params,
                    lambdaBody = lam_bdy,
                    lambdaReturnType = map paramDec racc_params ++ tp_res_other
                  }
          res_lam' <- renameLambda res_lam
          let res_pat =
                concatMap (accTup1 . snd) tup_common
                  ++ concatMap accTup1 (acc_tup1' ++ acc_tup2')
                  ++ map fst (other_pr1 ++ other_pr2)
              res_w_inps = map (accTup2 . fst) tup_common ++ map accTup2 (acc_tup1' ++ acc_tup2')
          res_w_inps' <- mapM renameLamInWAccInp res_w_inps
          let stm_res = Let (Pat res_pat) (aux1 <> aux2) $ WithAcc res_w_inps' res_lam'
          -- trace ("WithAcc-WithAcc: stm1:\n" ++ prettyString stm1 ++ "\nstm2:\n" ++ prettyString stm2 ++ "\nFusedRes:\n" ++ prettyString stm_res) $
          pure $ Just stm_res
    where
      -- local helpers:

      groupAccs ::
        [PatElem (LetDec SOACS)] ->
        [WithAccInput SOACS] ->
        Lambda SOACS ->
        ([AccTup], [(PatElem (LetDec SOACS), SubExpRes)])
      groupAccs pat_els wacc_inps wlam =
        let lam_params = lambdaParams wlam
            n = length lam_params
            (lam_par_crts, lam_par_accs) = splitAt (n `div` 2) lam_params
            lab_res_ses = bodyResult $ lambdaBody wlam
         in groupAccsHlp pat_els wacc_inps lam_par_crts lam_par_accs lab_res_ses
      groupAccsHlp ::
        [PatElem (LetDec SOACS)] ->
        [WithAccInput SOACS] ->
        [LParam SOACS] ->
        [LParam SOACS] ->
        [SubExpRes] ->
        ([AccTup], [(PatElem (LetDec SOACS), SubExpRes)])
      groupAccsHlp pat_els [] [] [] lam_res_ses
        | length pat_els == length lam_res_ses =
            ([], zip pat_els lam_res_ses)
      groupAccsHlp
        pat_els
        (winp@(_, inp, _) : wacc_inps)
        (par_crt : lam_par_crts)
        (par_acc : lam_par_accs)
        (res_se : lam_res_ses)
          | n <- length inp,
            (n <= length pat_els) && (n <= (1 + length lam_res_ses)),
            Var res_nm <- resSubExp res_se =
              let (pat_els_cur, pat_els') = splitAt n pat_els
                  (rec1, rec2) = groupAccsHlp pat_els' wacc_inps lam_par_crts lam_par_accs lam_res_ses
               in ((pat_els_cur, winp, par_crt, par_acc, (res_nm, resCerts res_se)) : rec1, rec2)
      groupAccsHlp _ _ _ _ _ =
        error "Unreachable case reached in groupAccsHlp!"
      --
      groupCommonAccs :: [AccTup] -> [AccTup] -> ([(AccTup, AccTup)], [AccTup], [AccTup])
      groupCommonAccs [] tup_accs2 =
        ([], [], tup_accs2)
      groupCommonAccs (tup_acc1 : tup_accs1) tup_accs2
        | commons2 <- filter (matchingAccTup tup_acc1) tup_accs2,
          length commons2 <= 1 =
            let (rec1, rec2, rec3) =
                  groupCommonAccs tup_accs1 $
                    if null commons2
                      then tup_accs2
                      else filter (not . matchingAccTup tup_acc1) tup_accs2
             in if null commons2
                  then (rec1, tup_acc1 : rec2, rec3)
                  else ((tup_acc1, head commons2) : rec1, tup_accs1, rec3)
      groupCommonAccs _ _ =
        error "Unreachable case reached in groupCommonAccs!"
      renameLamInWAccInp (shp, inps, Just (lam, se)) = do
        lam' <- renameLambda lam
        pure (shp, inps, Just (lam', se))
      renameLamInWAccInp winp = pure winp
--
tryFuseWithAccs _ _ _ =
  pure Nothing

-------------------------------
--- simple helper functions ---
-------------------------------

equivLambda ::
  M.Map VName VName ->
  Lambda SOACS ->
  Lambda SOACS ->
  Bool
equivLambda stab lam1 lam2
  | (ps1, ps2) <- (lambdaParams lam1, lambdaParams lam2),
    (nms1, nms2) <- (map paramName ps1, map paramName ps2),
    allEq (map paramDec ps1) (map paramDec ps2),
    allEq (map paramAttrs ps1) (map paramAttrs ps2),
    allEq (lambdaReturnType lam1) (lambdaReturnType lam2),
    (bdy1, bdy2) <- (lambdaBody lam1, lambdaBody lam2),
    bodyDec bdy1 == bodyDec bdy2 =
      let insert tab (x, k) = M.insert k x tab
          stab' = foldl insert stab $ zip nms1 nms2
          fStm (vtab, False) _ = (vtab, False)
          fStm (vtab, True) (s1, s2) = equivStm vtab s1 s2
          (stab'', success) =
            foldl fStm (stab', True) $
              zip (stmsToList (bodyStms bdy1)) $
                stmsToList (bodyStms bdy2)
          sres2 = substInSEs stab'' $ map resSubExp $ bodyResult bdy2
       in success && allEq (map resSubExp (bodyResult bdy1)) sres2
equivLambda _ _ _ =
  False

equivStm ::
  M.Map VName VName ->
  Stm SOACS ->
  Stm SOACS ->
  (M.Map VName VName, Bool)
equivStm
  stab
  (Let pat1 aux1 (BasicOp (BinOp bop1 se11 se12)))
  (Let pat2 aux2 (BasicOp (BinOp bop2 se21 se22)))
    | allEq [se11, se12] (substInSEs stab [se21, se22]),
      (pels1, pels2) <- (patElems pat1, patElems pat2),
      allEq (map patElemDec pels1) (map patElemDec pels2),
      bop1 == bop2 && aux1 == aux2 =
        let stab_new =
              M.fromList $
                zip (map patElemName pels2) (map patElemName pels1)
         in (M.union stab_new stab, True)
-- To Be Continued ...
equivStm vtab _ _ = (vtab, False)

matchingAccTup :: AccTup -> AccTup -> Bool
matchingAccTup
  (pat_els1, (shp1, _winp_arrs1, mlam1), _, _, _)
  (_, (shp2, winp_arrs2, mlam2), _, _, _) =
    allEq (shapeDims shp1) (shapeDims shp2)
      && allEq (map patElemName pat_els1) winp_arrs2
      && case (mlam1, mlam2) of
        (Nothing, Nothing) -> True
        (Just (lam1, see1), Just (lam2, see2)) ->
          (see1 == see2) && equivLambda M.empty lam1 lam2
        _ -> False

substInSEs :: M.Map VName VName -> [SubExp] -> [SubExp]
substInSEs vtab = map substInSE
  where
    substInSE (Var x)
      | Just y <- M.lookup x vtab = Var y
    substInSE z = z

allEq :: (Eq a) => [a] -> [a] -> Bool
allEq = (==)

{--
printNodeT :: NodeT -> String
printNodeT (StmNode stm) = prettyString stm
printNodeT (SoacNode trsf pat soac aux) =
  prettyString pat  ++ " = " ++
  prettyString soac ++ "\n"  ++
  "\n\t\tAux: " ++ show aux ++
  "\n\t\tTransforms: " ++ show trsf
printNodeT (TransNode nm1 _trsf nm2) =
  prettyString nm1 ++ " H.transform " ++ prettyString nm2
printNodeT (ResNode nm) =
  "Result " ++ prettyString nm
printNodeT (FreeNode nm) =
  "FreeNode " ++ prettyString nm
printNodeT (MatchNode stm _) =
  "MatchNode of stm: " ++ prettyString stm
printNodeT (DoNode stm _) =
  "DoNode of stm: " ++ prettyString stm
--}
