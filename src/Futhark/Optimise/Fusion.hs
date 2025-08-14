{-# LANGUAGE Strict #-}

-- | Perform horizontal and vertical fusion of SOACs.  See the paper
-- /A T2 Graph-Reduction Approach To Fusion/ for the basic idea (some
-- extensions discussed in /Design and GPGPU Performance of Futhark’s
-- Redomap Construct/).
module Futhark.Optimise.Fusion (fuseSOACs) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.Query.DFS qualified as Q
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as Futhark
import Futhark.IR.SOACS.Simplify (simplifyLambda)
import Futhark.Optimise.Fusion.GraphRep
import Futhark.Optimise.Fusion.RulesWithAccs qualified as SF
import Futhark.Optimise.Fusion.TryFusion qualified as TF
import Futhark.Pass
import Futhark.Transform.Rename
import Futhark.Transform.Substitute

data FusionEnv = FusionEnv
  { vNameSource :: VNameSource,
    fusionCount :: Int,
    fuseScans :: Bool
  }

freshFusionEnv :: FusionEnv
freshFusionEnv =
  FusionEnv
    { vNameSource = blankNameSource,
      fusionCount = 0,
      fuseScans = True
    }

newtype FusionM a = FusionM (ReaderT (Scope SOACS) (State FusionEnv) a)
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadState FusionEnv,
      HasScope SOACS,
      LocalScope SOACS
    )

instance MonadFreshNames FusionM where
  getNameSource = gets vNameSource
  putNameSource source =
    modify (\env -> env {vNameSource = source})

runFusionM :: (MonadFreshNames m) => Scope SOACS -> FusionEnv -> FusionM a -> m a
runFusionM scope fenv (FusionM a) = modifyNameSource $ \src ->
  let x = runReaderT a scope
      (y, z) = runState x (fenv {vNameSource = src})
   in (y, vNameSource z)

doFuseScans :: FusionM a -> FusionM a
doFuseScans m = do
  fs <- gets fuseScans
  modify (\s -> s {fuseScans = True})
  r <- m
  modify (\s -> s {fuseScans = fs})
  pure r

dontFuseScans :: FusionM a -> FusionM a
dontFuseScans m = do
  fs <- gets fuseScans
  modify (\s -> s {fuseScans = False})
  r <- m
  modify (\s -> s {fuseScans = fs})
  pure r

isNotVarInput :: [H.Input] -> [H.Input]
isNotVarInput = filter (isNothing . H.isVarInput)

finalizeNode :: (HasScope SOACS m, MonadFreshNames m) => NodeT -> m (Stms SOACS)
finalizeNode nt = case nt of
  StmNode stm -> pure $ oneStm stm
  SoacNode ots outputs soac aux -> runBuilder_ $ do
    untransformed_outputs <- mapM newName $ patNames outputs
    auxing aux $ letBindNames untransformed_outputs . Op =<< H.toSOAC soac
    forM_ (zip (patNames outputs) untransformed_outputs) $ \(output, v) ->
      letBindNames [output] . BasicOp . SubExp . Var =<< H.applyTransforms ots v
  ResNode _ -> pure mempty
  TransNode output tr ia -> do
    (aux, e) <- H.transformToExp tr ia
    runBuilder_ $ auxing aux $ letBindNames [output] e
  FreeNode _ -> pure mempty
  DoNode stm lst -> do
    lst' <- mapM (finalizeNode . fst) lst
    pure $ mconcat lst' <> oneStm stm
  MatchNode stm lst -> do
    lst' <- mapM (finalizeNode . fst) lst
    pure $ mconcat lst' <> oneStm stm

linearizeGraph :: (HasScope SOACS m, MonadFreshNames m) => DepGraph -> m (Stms SOACS)
linearizeGraph dg =
  fmap mconcat $ mapM finalizeNode $ reverse $ Q.topsort' (dgGraph dg)

fusedSomething :: NodeT -> FusionM (Maybe NodeT)
fusedSomething x = do
  modify $ \s -> s {fusionCount = 1 + fusionCount s}
  pure $ Just x

vTryFuseNodesInGraph :: G.Node -> G.Node -> DepGraphAug FusionM
-- find the neighbors -> verify that fusion causes no cycles -> fuse
vTryFuseNodesInGraph node_1 node_2 dg@DepGraph {dgGraph = g}
  | not (G.gelem node_1 g && G.gelem node_2 g) = pure dg
  | vFusionFeasability dg node_1 node_2 = do
      let (ctx1, ctx2) = (G.context g node_1, G.context g node_2)
      fres <- vFuseContexts edgs infusable_nodes ctx1 ctx2
      case fres of
        Just (inputs, _, nodeT, outputs) -> do
          nodeT' <-
            if null fusedC
              then pure nodeT
              else do
                let (_, _, _, deps_1) = ctx1
                    (_, _, _, deps_2) = ctx2
                    -- make copies of everything that was not
                    -- previously consumed
                    old_cons = map (getName . fst) $ filter (isCons . fst) (deps_1 <> deps_2)
                makeCopiesOfFusedExcept old_cons nodeT
          contractEdge node_2 (inputs, node_1, nodeT', outputs) dg
        Nothing -> pure dg
  | otherwise = pure dg
  where
    edgs = map G.edgeLabel $ edgesBetween dg node_1 node_2
    fusedC = map getName $ filter isCons edgs
    infusable_nodes =
      map
        depsFromEdge
        (concatMap (edgesBetween dg node_1) (filter (/= node_2) $ G.pre g node_1))

hTryFuseNodesInGraph :: G.Node -> G.Node -> DepGraphAug FusionM
hTryFuseNodesInGraph node_1 node_2 dg@DepGraph {dgGraph = g}
  | not (G.gelem node_1 g && G.gelem node_2 g) = pure dg
  | hFusionFeasability dg node_1 node_2 = do
      fres <- hFuseContexts (G.context g node_1) (G.context g node_2)
      case fres of
        Just ctx -> contractEdge node_2 ctx dg
        Nothing -> pure dg
  | otherwise = pure dg

hFuseContexts :: DepContext -> DepContext -> FusionM (Maybe DepContext)
hFuseContexts c1 c2 = do
  let (_, _, nodeT1, _) = c1
      (_, _, nodeT2, _) = c2
  fres <- hFuseNodeT nodeT1 nodeT2
  case fres of
    Just nodeT -> pure $ Just (mergedContext nodeT c1 c2)
    Nothing -> pure Nothing

vFuseContexts :: [EdgeT] -> [VName] -> DepContext -> DepContext -> FusionM (Maybe DepContext)
vFuseContexts edgs infusable c1 c2 = do
  let (i1, n1, nodeT1, o1) = c1
      (_i2, n2, nodeT2, o2) = c2
  fres <-
    vFuseNodeT
      edgs
      infusable
      (nodeT1, map fst $ filter ((/=) n2 . snd) i1, map fst o1)
      (nodeT2, map fst $ filter ((/=) n1 . snd) o2)
  case fres of
    Just nodeT -> pure $ Just (mergedContext nodeT c1 c2)
    Nothing -> pure Nothing

makeCopiesOfFusedExcept ::
  (LocalScope SOACS m, MonadFreshNames m) =>
  [VName] ->
  NodeT ->
  m NodeT
makeCopiesOfFusedExcept noCopy (SoacNode ots pats soac aux) = do
  let lam = H.lambda soac
  localScope (scopeOf lam) $ do
    fused_inner <-
      filterM (fmap (not . isAcc) . lookupType) . namesToList . consumedByLambda $
        Alias.analyseLambda mempty lam
    lam' <- makeCopiesInLambda (fused_inner L.\\ noCopy) lam
    pure $ SoacNode ots pats (H.setLambda lam' soac) aux
makeCopiesOfFusedExcept _ nodeT = pure nodeT

makeCopiesInLambda ::
  (LocalScope SOACS m, MonadFreshNames m) =>
  [VName] ->
  Lambda SOACS ->
  m (Lambda SOACS)
makeCopiesInLambda toCopy lam = do
  (copies, nameMap) <- makeCopyStms toCopy
  let l_body = lambdaBody lam
      newBody = insertStms copies (substituteNames nameMap l_body)
      newLambda = lam {lambdaBody = newBody}
  pure newLambda

makeCopyStms ::
  (LocalScope SOACS m, MonadFreshNames m) =>
  [VName] ->
  m (Stms SOACS, M.Map VName VName)
makeCopyStms vs = do
  vs' <- mapM makeNewName vs
  copies <- forM (zip vs vs') $ \(name, name') ->
    mkLetNames [name'] $ BasicOp $ Replicate mempty $ Var name
  pure (stmsFromList copies, M.fromList $ zip vs vs')
  where
    makeNewName name = newVName $ baseString name <> "_copy"

okToFuseProducer :: H.SOAC SOACS -> FusionM Bool
okToFuseProducer (H.Screma _ _ form) = do
  let is_scan = isJust $ Futhark.isScanomapSOAC form
  gets $ (not is_scan ||) . fuseScans
okToFuseProducer _ = pure True

-- First node is producer, second is consumer.
vFuseNodeT ::
  [EdgeT] ->
  [VName] ->
  (NodeT, [EdgeT], [EdgeT]) ->
  (NodeT, [EdgeT]) ->
  FusionM (Maybe NodeT)
vFuseNodeT _ infusible (s1, _, e1s) (MatchNode stm2 dfused, _)
  | isRealNode s1,
    null infusible =
      pure $ Just $ MatchNode stm2 $ (s1, e1s) : dfused
vFuseNodeT _ infusible (TransNode stm1_out tr stm1_in, _, _) (SoacNode ots2 pats2 soac2 aux2, _)
  | null infusible = do
      stm1_in_t <- lookupType stm1_in
      let onInput inp
            | H.inputArray inp == stm1_out =
                H.Input (tr H.<| H.inputTransforms inp) stm1_in stm1_in_t
            | otherwise =
                inp
          soac2' = map onInput (H.inputs soac2) `H.setInputs` soac2
      pure $ Just $ SoacNode ots2 pats2 soac2' aux2
vFuseNodeT
  _
  _
  (SoacNode ots1 pats1 soac1 aux1, i1s, _e1s)
  (SoacNode ots2 pats2 soac2 aux2, _e2s) = do
    let ker =
          TF.FusedSOAC
            { TF.fsSOAC = soac2,
              TF.fsOutputTransform = ots2,
              TF.fsOutNames = patNames pats2
            }
        preserveEdge InfDep {} = True
        preserveEdge e = isDep e
        preserve = namesFromList $ map getName $ filter preserveEdge i1s
    ok <- okToFuseProducer soac1
    -- It is not safe to fuse if any accumulators are updated by both, as the
    -- semantics require that any updates done by the consumer take precedence
    -- over those in the producer. This is implemented with a manual check here
    -- for convenience, but it could be argued that this should really be a Fake
    -- edge in the graph.
    let isProducedAcc (H.Input _ v Acc {}) = v `elem` patNames pats1
        isProducedAcc _ = False
    r <-
      if ok && ots1 == mempty && not (any isProducedAcc (H.inputs soac2))
        then TF.attemptFusion TF.Vertical preserve (patNames pats1) soac1 ker
        else pure Nothing
    case r of
      Just ker' -> do
        let pats2' =
              zipWith PatElem (TF.fsOutNames ker') (H.typeOf (TF.fsSOAC ker'))
        fusedSomething $
          SoacNode
            (TF.fsOutputTransform ker')
            (Pat pats2')
            (TF.fsSOAC ker')
            (aux1 <> aux2)
      Nothing -> pure Nothing
vFuseNodeT
  _
  infusible
  (SoacNode ots1 pat1 (H.Screma w inps form) aux1, _, _)
  (TransNode stm2_out (H.Index cs slice@(Slice (ds@(DimSlice _ w' _) : ds_rest))) _, _)
    | null infusible,
      w /= w',
      ots1 == mempty,
      Just _ <- isMapSOAC form,
      [pe] <- patElems pat1 = do
        let out_t = patElemType pe `setArrayShape` sliceShape slice
            inps' = map sliceInput inps
            -- Even if we move the slice of the outermost dimension, there
            -- might still be some slicing of the inner ones.
            ots1' = ots1 H.|> H.Index cs (Slice (sliceDim w' : ds_rest))
        fusedSomething $
          SoacNode
            ots1'
            (Pat [PatElem stm2_out out_t])
            (H.Screma w' inps' form)
            aux1
    where
      sliceInput inp =
        H.addTransform
          (H.Index cs (fullSlice (H.inputType inp) [ds]))
          inp
-- Case of fusing a screma with an WithAcc such as to (hopefully) perform
--   more fusion within the WithAcc. This would allow the withAcc to move in
--   the code (since up to now they mostly remain where they were introduced.)
-- We conservatively allow the fusion to fire---i.e., to move the soac inside
--   the withAcc---when the following are not part of withAcc's accumulators:
--    1. the in-dependencies of the soac and
--    2. the result of the soac
--  Note that the soac result is allowed to be part of the `infusible`
--    for as long as it is returned by the withAcc. If `infusible` is empty
--    then the extranous result will be simplified away.
vFuseNodeT
  edges
  _infusible
  (SoacNode ots1 pat1 soac@(H.Screma _w _form _s_inps) aux1, _is1, os1)
  (StmNode (Let pat2 aux2 (WithAcc w_inps lam0)), _os2)
    | ots1 == mempty,
      not $ any isFake edges,
      wacc_cons_nms <- namesFromList $ concatMap (\(_, nms, _) -> nms) w_inps,
      soac_prod_nms <- map patElemName $ patElems pat1,
      soac_indep_nms <- map getName os1,
      all (`notNameIn` wacc_cons_nms) (soac_indep_nms ++ soac_prod_nms) = do
        lam <- fst <$> doFusionInLambda lam0
        bdy' <-
          runBodyBuilder $ inScopeOf lam $ do
            soac' <- H.toExp soac
            addStm $ Let pat1 aux1 soac'
            lam_res <- bodyBind $ lambdaBody lam
            let pat1_res = map (SubExpRes (Certs []) . Var) soac_prod_nms
            pure $ lam_res ++ pat1_res
        let lam_ret_tp = lambdaReturnType lam ++ map patElemType (patElems pat1)
            pat = Pat $ patElems pat2 ++ patElems pat1
        lam' <- renameLambda $ lam {lambdaBody = bdy', lambdaReturnType = lam_ret_tp}
        -- see if bringing the map inside the scatter has actually benefitted fusion
        (lam'', success) <- doFusionInLambda lam'
        if not success
          then pure Nothing
          else do
            -- `aux1` already appear in the moved SOAC stm; is there
            -- any need to add it to the enclosing withAcc stm as well?
            fusedSomething $ StmNode $ Let pat aux2 $ WithAcc w_inps lam''

--
-- The reverse of the case above, i.e., fusing a screma at the back of an
--   WithAcc such as to (hopefully) enable more fusion there.
-- This should be safe as long as the SOAC does not uses any of the
--   accumulator arrays produced by the withAcc.
-- We could not provide a test for this case, due to the very restrictive
--   way in which accumulators can be used at source level.
--
--
vFuseNodeT
  edges
  _infusible
  (StmNode (Let pat1 aux1 (WithAcc w_inps wlam0)), _is1, _os1)
  (SoacNode ots2 pat2 soac@(H.Screma _w _form _s_inps) aux2, _os2)
    | ots2 == mempty,
      n <- length (lambdaParams wlam0) `div` 2,
      pat1_acc_nms <- namesFromList $ take n $ map patElemName $ patElems pat1,
      -- not $ namesIntersect (freeIn soac) pat1_acc_nms
      all ((`notNameIn` pat1_acc_nms) . getName) edges = do
        wlam <- fst <$> doFusionInLambda wlam0
        bdy' <-
          runBodyBuilder $ inScopeOf wlam $ do
            -- adding stms of withacc's lambda
            wlam_res <- bodyBind $ lambdaBody wlam
            -- add copies of the non-accumulator results of withacc
            let other_pr1 = drop n $ zip (patElems pat1) wlam_res
            forM_ other_pr1 $ \(pat_elm, bdy_res) -> do
              let (nm, se, tp) = (patElemName pat_elm, resSubExp bdy_res, patElemType pat_elm)
                  aux = (defAux ()) {stmAuxCerts = resCerts bdy_res}
              addStm $ Let (Pat [PatElem nm tp]) aux $ BasicOp $ SubExp se
            -- add the soac stmt
            soac' <- H.toExp soac
            addStm $ Let pat2 aux2 soac'
            -- build the body result
            let pat2_res = map (SubExpRes (Certs []) . Var . patElemName) $ patElems pat2
            pure $ wlam_res ++ pat2_res
        let lam_ret_tp = lambdaReturnType wlam ++ map patElemType (patElems pat2)
            pat = Pat $ patElems pat1 ++ patElems pat2
        wlam' <- renameLambda $ wlam {lambdaBody = bdy', lambdaReturnType = lam_ret_tp}
        -- see if bringing the map inside the scatter has actually benefitted fusion
        (wlam'', success) <- doFusionInLambda wlam'
        if not success
          then pure Nothing
          else -- `aux2` already appear in the enclosed SOAC stm; is there
          -- any need to add it to the enclosing withAcc stm as well?
            fusedSomething $ StmNode $ Let pat aux1 $ WithAcc w_inps wlam''
-- the case of fusing two withaccs
vFuseNodeT
  edges
  infusible
  (StmNode (Let pat1 aux1 (WithAcc w1_inps lam1)), is1, _os1)
  (StmNode (Let pat2 aux2 (WithAcc w2_inps lam2)), _os2)
    | not $ any isFake edges,
      wacc2_cons_nms <- namesFromList $ concatMap (\(_, nms, _) -> nms) w2_inps,
      wacc1_indep_nms <- map getName is1,
      all (`notNameIn` wacc2_cons_nms) wacc1_indep_nms = do
        -- the other safety checks are done inside `tryFuseWithAccs`
        lam1' <- fst <$> doFusionInLambda lam1
        lam2' <- fst <$> doFusionInLambda lam2
        let stm1 = Let pat1 aux1 (WithAcc w1_inps lam1')
            stm2 = Let pat2 aux2 (WithAcc w2_inps lam2')
        mstm <- sequence $ SF.tryFuseWithAccs infusible stm1 stm2
        case mstm of
          Nothing -> pure Nothing
          Just (Let pat aux (WithAcc w_inps wlam)) -> do
            (wlam', success) <- doFusionInLambda wlam
            let new_stm = Let pat aux (WithAcc w_inps wlam')
            if success then fusedSomething (StmNode new_stm) else pure Nothing
          Just _ -> error "Illegal result of tryFuseWithAccs called from vFuseNodeT."
--
vFuseNodeT _ _ _ _ = pure Nothing

resFromLambda :: Lambda rep -> Result
resFromLambda = bodyResult . lambdaBody

hasNoDifferingInputs :: [H.Input] -> [H.Input] -> Bool
hasNoDifferingInputs is1 is2 =
  let (vs1, vs2) = (isNotVarInput is1, isNotVarInput $ is2 L.\\ is1)
   in null $ vs1 `L.intersect` vs2

hFuseNodeT :: NodeT -> NodeT -> FusionM (Maybe NodeT)
hFuseNodeT (SoacNode ots1 pats1 soac1 aux1) (SoacNode ots2 pats2 soac2 aux2)
  | ots1 == mempty,
    ots2 == mempty,
    hasNoDifferingInputs (H.inputs soac1) (H.inputs soac2) = do
      let ker =
            TF.FusedSOAC
              { TF.fsSOAC = soac2,
                TF.fsOutputTransform = mempty,
                TF.fsOutNames = patNames pats2
              }
          preserve = namesFromList $ patNames pats1
      r <- TF.attemptFusion TF.Horizontal preserve (patNames pats1) soac1 ker
      case r of
        Just ker' -> do
          let pats2' =
                zipWith PatElem (TF.fsOutNames ker') (H.typeOf (TF.fsSOAC ker'))
          fusedSomething $ SoacNode mempty (Pat pats2') (TF.fsSOAC ker') (aux1 <> aux2)
        Nothing -> pure Nothing
hFuseNodeT
  (StmNode (Let pat1 aux1 (WithAcc w1_inps lam1)))
  (StmNode (Let pat2 aux2 (WithAcc w2_inps lam2))) = do
    -- The only tricky thing here is that we have to put all the
    -- accumulator-based results first.
    let num_inputs1 = length w1_inps
        num_inputs2 = length w2_inps
        num_arrs1 = sum $ map (\(_, as, _) -> length as) w1_inps
        num_arrs2 = sum $ map (\(_, as, _) -> length as) w2_inps
        w3_inps = w1_inps <> w2_inps
        reorder f n a m b =
          let (a_xs, a_ys) = splitAt n $ f a
              (b_xs, b_ys) = splitAt m $ f b
           in a_xs <> b_xs <> a_ys <> b_ys
        lam3 =
          Lambda
            (reorder lambdaParams num_inputs1 lam1 num_inputs2 lam2)
            (reorder lambdaReturnType num_inputs1 lam1 num_inputs2 lam2)
            $ mkBody
              (bodyStms (lambdaBody lam1) <> bodyStms (lambdaBody lam2))
              (reorder (bodyResult . lambdaBody) num_inputs1 lam1 num_inputs2 lam2)
    fusedSomething $
      StmNode $
        Let (Pat $ reorder patElems num_arrs1 pat1 num_arrs2 pat2) (aux1 <> aux2) $
          WithAcc w3_inps lam3
hFuseNodeT _ _ = pure Nothing

removeOutputsExcept :: [VName] -> NodeT -> NodeT
removeOutputsExcept toKeep s = case s of
  SoacNode ots (Pat pats1) (H.Screma w inp (ScremaForm lam_1 scan_1 red_1 post_lam)) aux1 ->
    SoacNode
      ots
      (Pat $ pats_unchanged <> pats_new)
      (H.Screma w inp (ScremaForm lam_1 scan_1 red_1 lam_new))
      aux1
    where
      red_output_size = Futhark.redResults red_1

      (pats_unchanged, pats_toChange) = splitAt red_output_size pats1
      res_toChange = zip (resFromLambda post_lam) (lambdaReturnType post_lam)

      (pats_new, res_new) = unzip $ filter (\(x, _) -> patElemName x `elem` toKeep) (zip pats_toChange res_toChange)
      (results, types) = unzip res_new
      lam_new =
        post_lam
          { lambdaReturnType = types,
            lambdaBody = (lambdaBody post_lam) {bodyResult = results}
          }
  node -> node

vNameFromAdj :: G.Node -> (EdgeT, G.Node) -> VName
vNameFromAdj n1 (edge, n2) = depsFromEdge (n2, n1, edge)

removeUnusedOutputsFromContext :: DepContext -> FusionM DepContext
removeUnusedOutputsFromContext (incoming, n1, nodeT, outgoing) =
  pure (incoming, n1, nodeT', outgoing)
  where
    toKeep = map (vNameFromAdj n1) incoming
    nodeT' = removeOutputsExcept toKeep nodeT

removeUnusedOutputs :: DepGraphAug FusionM
removeUnusedOutputs = mapAcross removeUnusedOutputsFromContext

tryFuseNodeInGraph :: DepNode -> DepGraphAug FusionM
tryFuseNodeInGraph node_to_fuse dg@DepGraph {dgGraph = g}
  | not (G.gelem (nodeFromLNode node_to_fuse) g) = pure dg
-- \^ Node might have been fused away since.
tryFuseNodeInGraph node_to_fuse dg@DepGraph {dgGraph = g} =
  applyAugs (map (vTryFuseNodesInGraph node_to_fuse_id) fuses_with) dg
  where
    node_to_fuse_id = nodeFromLNode node_to_fuse
    relevant (n, InfDep _) = isWithAccNodeId n dg
    relevant (_, e) = isDep e
    fuses_with = map fst $ filter relevant $ G.lpre g node_to_fuse_id

doVerticalFusion :: DepGraphAug FusionM
doVerticalFusion dg = applyAugs (map tryFuseNodeInGraph $ reverse $ filter relevant $ G.labNodes (dgGraph dg)) dg
  where
    relevant (_, n@(StmNode {})) = isWithAccNodeT n
    relevant (_, ResNode {}) = False
    relevant _ = True

-- | For each pair of SOAC nodes that share an input, or any WithAcc nodes,
-- attempt to fuse them horizontally.
doHorizontalFusion :: DepGraphAug FusionM
doHorizontalFusion dg = applyAugs (soac_pairs <> withacc_pairs) dg
  where
    soac_pairs, withacc_pairs :: [DepGraphAug FusionM]
    soac_pairs = do
      (x, SoacNode _ _ soac_x _) <- G.labNodes $ dgGraph dg
      (y, SoacNode _ _ soac_y _) <- G.labNodes $ dgGraph dg
      guard $ x < y
      -- Must share an input.
      guard $
        any
          ((`elem` map H.inputArray (H.inputs soac_x)) . H.inputArray)
          (H.inputs soac_y)
      pure $ \dg' -> do
        -- Nodes might have been fused away by now.
        if G.gelem x (dgGraph dg') && G.gelem y (dgGraph dg')
          then hTryFuseNodesInGraph x y dg'
          else pure dg'

    withacc_pairs = do
      (x, StmNode (Let _ _ (WithAcc {}))) <- G.labNodes $ dgGraph dg
      (y, StmNode (Let _ _ (WithAcc {}))) <- G.labNodes $ dgGraph dg
      guard $ x < y
      pure $ \dg' -> do
        -- Nodes might have been fused away by now.
        if G.gelem x (dgGraph dg') && G.gelem y (dgGraph dg')
          then hTryFuseNodesInGraph x y dg'
          else pure dg'

doInnerFusion :: DepGraphAug FusionM
doInnerFusion = mapAcross runInnerFusionOnContext

-- Fixed-point iteration.
keepTrying :: DepGraphAug FusionM -> DepGraphAug FusionM
keepTrying f g = do
  prev_fused <- gets fusionCount
  g' <- f g
  aft_fused <- gets fusionCount
  if prev_fused /= aft_fused then keepTrying f g' else pure g'

doAllFusion :: DepGraphAug FusionM
doAllFusion =
  applyAugs
    [ keepTrying . applyAugs $
        [ doVerticalFusion,
          doHorizontalFusion,
          doInnerFusion
        ],
      removeUnusedOutputs
    ]

runInnerFusionOnContext :: DepContext -> FusionM DepContext
runInnerFusionOnContext c@(incoming, node, nodeT, outgoing) = case nodeT of
  DoNode (Let pat aux (Loop params form body)) to_fuse ->
    doFuseScans . localScope (scopeOfFParams (map fst params) <> scopeOfLoopForm form) $ do
      b <- doFusionWithDelayed body to_fuse
      pure (incoming, node, DoNode (Let pat aux (Loop params form b)) [], outgoing)
  MatchNode (Let pat aux (Match cond cases defbody dec)) to_fuse -> doFuseScans $ do
    cases' <- mapM (traverse $ renameBody <=< (`doFusionWithDelayed` to_fuse)) cases
    defbody' <- doFusionWithDelayed defbody to_fuse
    pure (incoming, node, MatchNode (Let pat aux (Match cond cases' defbody' dec)) [], outgoing)
  StmNode (Let pat aux (Op (Futhark.VJP args vec lam))) -> doFuseScans $ do
    lam' <- fst <$> doFusionInLambda lam
    pure (incoming, node, StmNode (Let pat aux (Op (Futhark.VJP args vec lam'))), outgoing)
  StmNode (Let pat aux (Op (Futhark.JVP args vec lam))) -> doFuseScans $ do
    lam' <- fst <$> doFusionInLambda lam
    pure (incoming, node, StmNode (Let pat aux (Op (Futhark.JVP args vec lam'))), outgoing)
  StmNode (Let pat aux (WithAcc inputs lam)) -> doFuseScans $ do
    lam' <- fst <$> doFusionInLambda lam
    pure (incoming, node, StmNode (Let pat aux (WithAcc inputs lam')), outgoing)
  SoacNode ots pat soac aux -> do
    soac' <- case soac of
      H.Stream w inputs accs lam ->
        H.Stream w inputs accs <$> dontFuseScans (onLambda lam)
      H.Screma w inputs (ScremaForm lam scans reds post_lam) ->
        H.Screma w inputs
          <$> ( ScremaForm
                  <$> doFuseScans (onLambda lam)
                  <*> mapM onScan scans
                  <*> mapM onRed reds
                  <*> doFuseScans (onLambda post_lam)
              )
      _ ->
        H.setLambda <$> doFuseScans (onLambda (H.lambda soac)) <*> pure soac
    let nodeT' = SoacNode ots pat soac' aux
    pure (incoming, node, nodeT', outgoing)
  _ -> pure c
  where
    onLambda lam = inScopeOf lam . fmap fst $ doFusionInLambda lam
    onScan (Scan lam nes) = Scan <$> onLambda lam <*> pure nes
    onRed (Reduce comm lam nes) = Reduce comm <$> onLambda lam <*> pure nes

    doFusionWithDelayed :: Body SOACS -> [(NodeT, [EdgeT])] -> FusionM (Body SOACS)
    doFusionWithDelayed (Body () stms res) extraNodes = inScopeOf stms $ do
      stm_node <- mapM (finalizeNode . fst) extraNodes
      stms' <- fuseGraph (mkBody (mconcat stm_node <> stms) res)
      pure $ Body () stms' res

doFusionInLambda :: Lambda SOACS -> FusionM (Lambda SOACS, Bool)
doFusionInLambda lam = do
  -- To clean up previous instances of fusion.
  lam' <- simplifyLambda lam
  prev_count <- gets fusionCount
  newbody <- inScopeOf lam' $ doFusionBody $ lambdaBody lam'
  aft_count <- gets fusionCount
  -- To clean up any inner fusion.
  lam'' <-
    (if prev_count /= aft_count then simplifyLambda else pure)
      lam' {lambdaBody = newbody}
  pure (lam'', prev_count /= aft_count)
  where
    doFusionBody :: Body SOACS -> FusionM (Body SOACS)
    doFusionBody body = do
      stms' <- fuseGraph body
      pure $ body {bodyStms = stms'}

-- main fusion function.
fuseGraph :: Body SOACS -> FusionM (Stms SOACS)
fuseGraph body = inScopeOf (bodyStms body) $ do
  graph_not_fused <- mkDepGraph body
  graph_fused <- doAllFusion graph_not_fused
  linearizeGraph graph_fused

fuseConsts :: [VName] -> Stms SOACS -> PassM (Stms SOACS)
fuseConsts outputs stms =
  runFusionM
    (scopeOf stms)
    freshFusionEnv
    (fuseGraph (mkBody stms (varsRes outputs)))

fuseFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
fuseFun consts fun = do
  fun_stms' <-
    runFusionM
      (scopeOf fun <> scopeOf consts)
      freshFusionEnv
      (fuseGraph (funDefBody fun))
  pure fun {funDefBody = (funDefBody fun) {bodyStms = fun_stms'}}

-- | The pass definition.
{-# NOINLINE fuseSOACs #-}
fuseSOACs :: Pass SOACS SOACS
fuseSOACs =
  Pass
    { passName = "Fuse SOACs",
      passDescription = "Perform higher-order optimisation, i.e., fusion.",
      passFunction = \p ->
        intraproceduralTransformationWithConsts
          (fuseConsts (namesToList $ freeIn (progFuns p)))
          fuseFun
          p
    }
