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
vFuseNodeT :: [EdgeT] -> [VName] -> (NodeT, [EdgeT], [EdgeT]) -> (NodeT, [EdgeT]) -> FusionM (Maybe NodeT)
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
    r <-
      if ok && ots1 == mempty
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
  _edges
  _infusible
  (SoacNode ots1 pat1 soac@(H.Screma _w _form _s_inps) aux1, is1, _os1)
  (StmNode (Let pat2 aux2 (WithAcc w_inps lam0)), _os2)
    | ots1 == mempty,
      wacc_cons_nms <- namesFromList $ concatMap (\(_, nms, _) -> nms) w_inps,
      soac_prod_nms <- map patElemName $ patElems pat1,
      soac_indep_nms <- map getName is1,
      all (`notNameIn` wacc_cons_nms) (soac_indep_nms ++ soac_prod_nms) =
        do
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
  _edges
  infusible
  (StmNode (Let pat1 aux1 (WithAcc w1_inps lam1)), is1, _os1)
  (StmNode (Let pat2 aux2 (WithAcc w2_inps lam2)), _os2)
    | wacc2_cons_nms <- namesFromList $ concatMap (\(_, nms, _) -> nms) w2_inps,
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
hFuseNodeT _ _ = pure Nothing

removeOutputsExcept :: [VName] -> NodeT -> NodeT
removeOutputsExcept toKeep s = case s of
  SoacNode ots (Pat pats1) soac@(H.Screma _ _ (ScremaForm lam_1 scans_1 red_1)) aux1 ->
    SoacNode ots (Pat $ pats_unchanged <> pats_new) (H.setLambda lam_new soac) aux1
    where
      scan_output_size = Futhark.scanResults scans_1
      red_output_size = Futhark.redResults red_1

      (pats_unchanged, pats_toChange) = splitAt (scan_output_size + red_output_size) pats1
      (res_unchanged, res_toChange) = splitAt (scan_output_size + red_output_size) (zip (resFromLambda lam_1) (lambdaReturnType lam_1))

      (pats_new, other) = unzip $ filter (\(x, _) -> patElemName x `elem` toKeep) (zip pats_toChange res_toChange)
      (results, types) = unzip (res_unchanged ++ other)
      lam_new =
        lam_1
          { lambdaReturnType = types,
            lambdaBody = (lambdaBody lam_1) {bodyResult = results}
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
tryFuseNodeInGraph node_to_fuse dg@DepGraph {dgGraph = g} = do
  spec_rule_res <- SF.ruleMFScat node_to_fuse dg
  -- \^ specialized fusion rules such as the one
  --   enabling map-flatten-scatter fusion
  case spec_rule_res of
    Just dg' -> pure dg'
    Nothing -> applyAugs (map (vTryFuseNodesInGraph node_to_fuse_id) fuses_with) dg
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

-- | For each pair of SOAC nodes that share an input, attempt to fuse
-- them horizontally.
doHorizontalFusion :: DepGraphAug FusionM
doHorizontalFusion dg = applyAugs pairs dg
  where
    pairs :: [DepGraphAug FusionM]
    pairs = do
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
    let lam = H.lambda soac
    lam' <- inScopeOf lam $ case soac of
      H.Stream {} ->
        dontFuseScans $ fst <$> doFusionInLambda lam
      _ ->
        doFuseScans $ fst <$> doFusionInLambda lam
    let nodeT' = SoacNode ots pat (H.setLambda lam' soac) aux
    pure (incoming, node, nodeT', outgoing)
  _ -> pure c
  where
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
