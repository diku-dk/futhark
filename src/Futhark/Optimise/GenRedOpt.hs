{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tries to turn a generalized reduction kernel into
--     a more specialized construct, for example:
--       (a) a map nest with a sequential redomap ripe for tiling
--       (b) a SegRed kernel followed by a smallish accumulation kernel.
--       (c) a histogram (for this we need to track the withAccs)
--   The idea is to identify the first accumulation and
--     to separate the initial kernels into two:
--     1. the code up to and including the accumulation,
--        which is optimized to turn the accumulation either
--        into a map-reduce composition or a histogram, and
--     2. the remaining code, which is recursively optimized.
--   Since this is mostly prototyping, when the accumulation
--     can be rewritten as a map-reduce, we sequentialize the
--     map-reduce, as to potentially enable tiling oportunities.
module Futhark.Optimise.GenRedOpt (optimiseGenRed) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Builder
import Futhark.IR.GPU
import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.Rename

--import Debug.Trace

type GenRedM = ReaderT (Scope GPU) (State VNameSource)

-- | The pass definition.
optimiseGenRed :: Pass GPU GPU
optimiseGenRed =
  Pass "optimise generalized reductions" "Specializes generalized reductions into map-reductions or histograms" $
    intraproceduralTransformation onStms
  where
    onStms scope stms =
      modifyNameSource $
        runState $
          runReaderT (optimiseStms (M.empty, M.empty) stms) scope

optimiseBody :: Env -> Body GPU -> GenRedM (Body GPU)
optimiseBody env (Body () stms res) =
  Body () <$> optimiseStms env stms <*> pure res

optimiseStms :: Env -> Stms GPU -> GenRedM (Stms GPU)
optimiseStms env stms =
  localScope (scopeOf stms) $ do
    (_, stms') <- foldM foldfun (env, mempty) $ stmsToList stms
    return stms'
  where
    foldfun :: (Env, Stms GPU) -> Stm GPU -> GenRedM (Env, Stms GPU)
    foldfun (e, ss) s = do
      (e', s') <- optimiseStm e s
      return (e', ss <> s')

optimiseStm :: Env -> Stm GPU -> GenRedM (Env, Stms GPU)
optimiseStm env stm@(Let _ _ (Op (SegOp (SegMap SegThread {} _ _ _)))) = do
  res_genred_opt <- genRedOpts env stm
  let stms' =
        case res_genred_opt of
          Just stms -> stms
          Nothing -> oneStm stm
  return (env, stms')
optimiseStm env (Let pat aux e) = do
  env' <- changeEnv env (head $ patNames pat) e
  e' <- mapExpM (optimise env') e
  return (env', oneStm $ Let pat aux e')
  where
    optimise env' = identityMapper {mapOnBody = \scope -> localScope scope . optimiseBody env'}

------------------------

genRedOpts :: Env -> Stm GPU -> GenRedM (Maybe (Stms GPU))
genRedOpts env ker = do
  res_tile <- genRed2Tile2d env ker
  case res_tile of
    Nothing -> do
      res_sgrd <- genRed2SegRed env ker
      helperGenRed res_sgrd
    _ -> helperGenRed res_tile
  where
    helperGenRed Nothing = return Nothing
    helperGenRed (Just (stms_before, ker_snd)) = do
      mb_stms_after <- genRedOpts env ker_snd
      case mb_stms_after of
        Just stms_after -> return $ Just $ stms_before <> stms_after
        Nothing -> return $ Just $ stms_before <> oneStm ker_snd

se1 :: SubExp
se1 = intConst Int64 1

genRed2Tile2d :: Env -> Stm GPU -> GenRedM (Maybe (Stms GPU, Stm GPU))
genRed2Tile2d env kerstm@(Let pat_ker aux (Op (SegOp (SegMap seg_thd seg_space kres_tps old_kbody))))
  | (SegThread _ seg_group_size _novirt) <- seg_thd,
    --novirt == SegNoVirtFull || novirt == SegNoVirt,
    KernelBody () kstms kres <- old_kbody,
    Just (css, r_ses) <- allGoodReturns kres,
    null css,
    -- build the variance table, that records, for
    -- each variable name, the variables it depends on
    initial_variance <- M.map mempty $ scopeOfSegSpace seg_space,
    variance <- varianceInStms initial_variance kstms,
    -- check that the code fits the pattern having:
    -- some `code1`, followed by one accumulation, followed by some `code2`
    -- UpdateAcc VName [SubExp] [SubExp]
    (code1, Just accum_stmt, code2) <- matchCodeAccumCode kstms,
    Let pat_accum _aux_acc (BasicOp (UpdateAcc acc_nm acc_inds acc_vals)) <- accum_stmt,
    [pat_acc_nm] <- patNames pat_accum,
    -- check that the `acc_inds` are invariant to at least one
    -- parallel kernel dimensions, and return the innermost such one:
    Just (invar_gid, gid_ind) <- isInvarToParDim mempty seg_space variance acc_inds,
    gid_dims_new_0 <- filter (\x -> invar_gid /= fst x) (unSegSpace seg_space),
    -- reorder the variant dimensions such that inner(most) accum-indices
    -- correspond to inner(most) parallel dimensions, so that the babysitter
    -- does not introduce transpositions
    -- gid_dims_new <- gid_dims_new_0,
    gid_dims_new <- reorderParDims variance acc_inds gid_dims_new_0,
    -- check that all global-memory accesses in `code1` on which
    --   `accum_stmt` depends on are invariant to at least one of
    --   the remaining parallel dimensions (i.e., excluding `invar_gid`)
    all (isTileable invar_gid gid_dims_new variance pat_acc_nm) (stmsToList code1),
    -- need to establish a cost model for the stms that would now
    --   be redundantly executed by the two kernels. If any recurence
    --   is redundant than it is a no go. Otherwise we need to look at
    --   memory accesses: if more than two are re-executed, then we
    --   should abort.
    cost <- costRedundantExecution variance pat_acc_nm r_ses kstms,
    --trace ("Redundant cost is: "++printCost cost) $
    maxCost cost (Small 2) == Small 2 = do
    -- 1. create the first kernel
    acc_tp <- lookupType acc_nm
    let inv_dim_len = segSpaceDims seg_space !! gid_ind
        -- 1.1. get the accumulation operator
        ((redop0, neutral), el_tps) = getAccLambda acc_tp
    redop <- renameLambda redop0
    let red =
          Reduce
            { redComm = Commutative,
              redLambda = redop,
              redNeutral = neutral
            }
        -- 1.2. build the sequential map-reduce screma
        code1' =
          stmsFromList $
            filter (dependsOnAcc pat_acc_nm variance) $
              stmsToList code1
    (code1'', code1_tr_host) <- transposeFVs (freeIn kerstm) variance invar_gid code1'
    let map_lam_body = mkBody code1'' $ map (SubExpRes (Certs [])) acc_vals
        map_lam0 = Lambda [Param mempty invar_gid (Prim int64)] map_lam_body el_tps
    map_lam <- renameLambda map_lam0
    (k1_res, ker1_stms) <- runBuilderT' $ do
      iota <- letExp "iota" $ BasicOp $ Iota inv_dim_len (intConst Int64 0) (intConst Int64 1) Int64
      let op_exp = Op (OtherOp (Screma inv_dim_len [iota] (ScremaForm [] [red] map_lam)))
      res_redmap <- letTupExp "res_mapred" op_exp
      letSubExp (baseString pat_acc_nm ++ "_big_update") $
        BasicOp (UpdateAcc acc_nm acc_inds $ map Var res_redmap)
    -- Let pat_accum aux (BasicOp (UpdateAcc acc_nm acc_inds res_redmap))

    -- 1.3. build the kernel expression and rename it!
    gid_flat_1 <- newVName "gid_flat"
    let space1 = SegSpace gid_flat_1 gid_dims_new

    (grid_size, host_stms1) <- runBuilder $ do
      let grid_pexp = foldl (\x d -> x * pe64 d) (pe64 se1) $ map snd gid_dims_new
      dim_prod <- letSubExp "dim_prod" =<< toExp grid_pexp
      letSubExp "grid_size" =<< ceilDiv dim_prod (unCount seg_group_size)
    let level1 = SegThread (Count grid_size) seg_group_size (SegNoVirtFull (SegSeqDims [])) -- novirt ?
        kbody1 = KernelBody () ker1_stms [Returns ResultMaySimplify (Certs []) k1_res]

    -- is it OK here to use the "aux" from the parrent kernel?
    ker_exp <- renameExp $ Op (SegOp (SegMap level1 space1 [acc_tp] kbody1))
    let ker1 = Let pat_accum aux ker_exp

    -- 2 build the second kernel
    -- Let pat_ker aux  (Op (SegOp (SegMap seg_thd seg_space kres_tps old_kbody)))
    let ker2_body = old_kbody {kernelBodyStms = code1 <> code2}
    ker2_exp <- renameExp $ Op (SegOp (SegMap seg_thd seg_space kres_tps ker2_body))
    let ker2 = Let pat_ker aux ker2_exp
    return $
      --trace ("\nIdentified Potential for Optimizing Gen Red, acc-stm:\n"++
      --       pretty accum_stmt++"\n invar to:"++pretty (invar_gid, gid_ind)++
      --       -- "\nkernel code:\n"++pretty kerstm++
      --       "\n first redomap kernel:\n"++pretty ker1++"\n transpositions: "++pretty code1_tr_host
      --      ) $
      Just (code1_tr_host <> host_stms1 <> oneStm ker1, ker2)
  where
    isIndVarToParDim _ (Constant _) _ = False
    isIndVarToParDim variance (Var acc_ind) par_dim =
      acc_ind == fst par_dim
        || nameIn (fst par_dim) (M.findWithDefault mempty acc_ind variance)
    foldfunReorder variance (unused_dims, inner_dims) acc_ind =
      case L.findIndex (isIndVarToParDim variance acc_ind) unused_dims of
        Nothing -> (unused_dims, inner_dims)
        Just i ->
          ( take i unused_dims ++ drop (i + 1) unused_dims,
            (unused_dims !! i) : inner_dims
          )
    reorderParDims variance acc_inds gid_dims_new_0 =
      let (invar_dims, inner_dims) =
            foldl
              (foldfunReorder variance)
              (gid_dims_new_0, [])
              (reverse acc_inds)
       in invar_dims ++ inner_dims
    --
    ceilDiv x y = pure $ BasicOp $ BinOp (SDivUp Int64 Unsafe) x y
    getAccLambda acc_tp =
      case acc_tp of
        (Acc tp_id _shp el_tps _) ->
          case M.lookup tp_id (fst env) of
            Just lam -> (lam, el_tps)
            _ -> error $ "Lookup in environment failed! " ++ pretty tp_id ++ " env: " ++ pretty (fst env)
        _ -> error "Illegal accumulator type!"
    -- is a subexp invariant to a gid of a parallel dimension?
    isSeInvar2 variance gid (Var x) =
      let x_deps = M.findWithDefault mempty x variance
       in gid /= x && not (nameIn gid x_deps)
    isSeInvar2 _ _ _ = True
    -- is a DimIndex invar to a gid of a parallel dimension?
    isDimIdxInvar2 variance gid (DimFix d) =
      isSeInvar2 variance gid d
    isDimIdxInvar2 variance gid (DimSlice d1 d2 d3) =
      all (isSeInvar2 variance gid) [d1, d2, d3]
    -- is an entire slice invariant to at least one gid of a parallel dimension
    isSliceInvar2 variance slc gids =
      any (\gid -> all (isDimIdxInvar2 variance gid) (unSlice slc)) gids
    -- are all statements that touch memory invariant to at least one parallel dimension?
    isTileable :: VName -> [(VName, SubExp)] -> VarianceTable -> VName -> Stm GPU -> Bool
    isTileable seq_gid gid_dims variance acc_nm (Let (Pat [pel]) _ (BasicOp (Index _ slc)))
      | acc_deps <- M.findWithDefault mempty acc_nm variance,
        nameIn (patElemName pel) acc_deps =
        let invar_par = isSliceInvar2 variance slc (map fst gid_dims)
            invar_seq = isSliceInvar2 variance slc [seq_gid]
         in invar_par || invar_seq
    --in  trace ("!!!!!!!!!!!!!!Slice "++pretty slc ++" is invariant to: "++pretty gid_dims++" "++
    --             pretty seq_gid++" answer: "++pretty invar_par++" "++pretty invar_seq) $
    --              invar_par || invar_seq
    -- this relies on the cost model, that currently accepts only
    -- global-memory reads, and for example rejects in-place updates
    -- or loops inside the code that is transformed in a redomap.
    isTileable _ _ _ _ _ = True
    -- does the to-be-reduced accumulator depends on this statement?
    dependsOnAcc pat_acc_nm variance (Let pat _ _) =
      let acc_deps = M.findWithDefault mempty pat_acc_nm variance
       in any (`nameIn` acc_deps) $ patNames pat
genRed2Tile2d _ _ =
  return Nothing

genRed2SegRed :: Env -> Stm GPU -> GenRedM (Maybe (Stms GPU, Stm GPU))
genRed2SegRed _ _ =
  return Nothing

-- M.Map VName ([Int], VName, Stms GPU)
transposeFVs ::
  Names ->
  VarianceTable ->
  VName ->
  Stms GPU ->
  GenRedM (Stms GPU, Stms GPU)
transposeFVs fvs variance gid stms = do
  (tab, stms') <- foldM foldfun (M.empty, mempty) $ stmsToList stms
  let stms_host = M.foldr (\(_, _, s) ss -> ss <> s) mempty tab
  return (stms', stms_host)
  where
    foldfun (tab, all_stms) stm = do
      (tab', stm') <- transposeFV (tab, stm)
      return (tab', all_stms <> oneStm stm')
    -- ToDo: currently handles only 2-dim arrays, please generalize
    transposeFV (tab, Let pat aux (BasicOp (Index arr slc)))
      | dims <- unSlice slc,
        all isFixDim dims,
        nameIn arr fvs,
        iis <- L.findIndices depOnGid dims,
        [ii] <- iis,
        -- generalize below: treat any rearange and add to tab if not there.
        Nothing <- M.lookup arr tab,
        ii /= length dims - 1,
        perm <- [0 .. ii -1] ++ [ii + 1 .. length dims - 1] ++ [ii] = do
        --        length dims == 2,
        --        ii == 0 = do
        (arr_tr, stms_tr) <- runBuilderT' $ do
          arr' <- letExp (baseString arr ++ "_trsp") $ BasicOp $ Rearrange perm arr --Manifest [1,0] arr
          letExp (baseString arr' ++ "_opaque") $ BasicOp $ Opaque OpaqueNil $ Var arr'
        let tab' = M.insert arr (perm, arr_tr, stms_tr) tab
            slc' = Slice $ map (dims !!) perm
            stm' = Let pat aux $ BasicOp $ Index arr_tr slc'
        return (tab', stm')
      where
        isFixDim DimFix {} = True
        isFixDim _ = False
        depOnGid (DimFix (Var nm)) =
          gid == nm || nameIn gid (M.findWithDefault mempty nm variance)
        depOnGid _ = False
    transposeFV r = return r

-- | Tries to identify the following pattern:
--   code followed by some UpdateAcc-statement
--   followed by more code.
matchCodeAccumCode ::
  Stms GPU ->
  (Stms GPU, Maybe (Stm GPU), Stms GPU)
matchCodeAccumCode kstms =
  let (code1, screma, code2) =
        foldl
          ( \acc stmt ->
              case (acc, stmt) of
                ((cd1, Nothing, cd2), Let _ _ (BasicOp UpdateAcc {})) ->
                  (cd1, Just stmt, cd2)
                ((cd1, Nothing, cd2), _) ->
                  (cd1 ++ [stmt], Nothing, cd2)
                ((cd1, Just strm, cd2), _) ->
                  (cd1, Just strm, cd2 ++ [stmt])
          )
          ([], Nothing, [])
          (stmsToList kstms)
   in (stmsFromList code1, screma, stmsFromList code2)

-- | Checks that there exist a parallel dimension (among `kids`),
--     to which all the indices (`acc_inds`) are invariant to.
--   It returns the innermost such parallel dimension, as a tuple
--     of the pardim gid (`VName`) and its index (`Int`) in the
--     parallel space.
isInvarToParDim ::
  Names ->
  SegSpace ->
  VarianceTable ->
  [SubExp] ->
  Maybe (VName, Int)
isInvarToParDim branch_variant kspace variance acc_inds =
  let ker_gids = map fst $ unSegSpace kspace
      branch_invariant = not $ any (`nameIn` branch_variant) ker_gids
      allvar2 = allvariant2 acc_inds ker_gids
      last_invar_dim =
        foldl (lastNotIn allvar2) Nothing $
          zip ker_gids [0 .. length ker_gids -1]
   in if branch_invariant
        then last_invar_dim
        else Nothing
  where
    variant2 (Var ind) kids =
      let variant_to =
            M.findWithDefault mempty ind variance
              <> (if ind `elem` kids then oneName ind else mempty)
       in filter (`nameIn` variant_to) kids
    variant2 _ _ = []
    allvariant2 ind_ses kids =
      namesFromList $ concatMap (`variant2` kids) ind_ses
    lastNotIn allvar2 acc (kid, k) =
      if nameIn kid allvar2 then acc else Just (kid, k)

allGoodReturns :: [KernelResult] -> Maybe ([VName], [SubExp])
allGoodReturns kres
  | all goodReturn kres = do
    Just $ foldl addCertAndRes ([], []) kres
  where
    goodReturn (Returns ResultMaySimplify _ _) = True
    goodReturn _ = False
    addCertAndRes (cs, rs) (Returns ResultMaySimplify c r_se) =
      (cs ++ unCerts c, rs ++ [r_se])
    addCertAndRes _ _ =
      error "Impossible case reached in GenRedOpt.hs, function allGoodReturns!"
allGoodReturns _ = Nothing

--------------------------
--- Cost Model Helpers ---
--------------------------

costRedundantExecution ::
  VarianceTable ->
  VName ->
  [SubExp] ->
  Stms GPU ->
  Cost
costRedundantExecution variance pat_acc_nm r_ses kstms =
  let acc_deps = M.findWithDefault mempty pat_acc_nm variance
      vartab_cut_acc = varianceInStmsWithout (oneName pat_acc_nm) mempty kstms
      res_deps = mconcat $ map (findDeps vartab_cut_acc) $ mapMaybe se2nm r_ses
      common_deps = namesIntersection res_deps acc_deps
   in --trace ("Common deps: "++pretty common_deps) $
      foldl (addCostOfStmt common_deps) (Small 0) kstms
  where
    se2nm (Var nm) = Just nm
    se2nm _ = Nothing
    findDeps vartab nm = M.findWithDefault mempty nm vartab
    addCostOfStmt common_deps cur_cost stm =
      let pat_nms = patNames $ stmPat stm
       in if namesIntersect (namesFromList pat_nms) common_deps
            then addCosts cur_cost $ costRedundantStmt stm
            else cur_cost
    varianceInStmsWithout :: Names -> VarianceTable -> Stms GPU -> VarianceTable
    varianceInStmsWithout nms = L.foldl' (varianceInStmWithout nms)
    varianceInStmWithout cuts vartab stm =
      let pat_nms = patNames $ stmPat stm
       in if namesIntersect (namesFromList pat_nms) cuts
            then vartab
            else L.foldl' add vartab pat_nms
      where
        add variance' v = M.insert v binding_variance variance'
        look variance' v = oneName v <> M.findWithDefault mempty v variance'
        binding_variance = mconcat $ map (look vartab) $ namesToList (freeIn stm)

data Cost = Small Int | Big | Break
  deriving (Eq)

{--
printCost :: Cost -> String
printCost Big   = "BIG"
printCost Break = "BREAK"
printCost (Small i) = "(Small "++show i++")"
--}
addCosts :: Cost -> Cost -> Cost
addCosts Break _ = Break
addCosts _ Break = Break
addCosts Big _ = Big
addCosts _ Big = Big
addCosts (Small c1) (Small c2) = Small (c1 + c2)

maxCost :: Cost -> Cost -> Cost
maxCost (Small c1) (Small c2) = Small (max c1 c2)
maxCost c1 c2 = addCosts c1 c2

costBody :: Body GPU -> Cost
costBody bdy =
  foldl addCosts (Small 0) $
    map costRedundantStmt $ stmsToList $ bodyStms bdy

costRedundantStmt :: Stm GPU -> Cost
costRedundantStmt (Let _ _ (Op _)) = Big
costRedundantStmt (Let _ _ DoLoop {}) = Big
costRedundantStmt (Let _ _ Apply {}) = Big
costRedundantStmt (Let _ _ WithAcc {}) = Big
costRedundantStmt (Let _ _ (If _cond b_then b_else _)) =
  maxCost (costBody b_then) (costBody b_else)
costRedundantStmt (Let _ _ (BasicOp (ArrayLit _ Array {}))) = Big
costRedundantStmt (Let _ _ (BasicOp (ArrayLit _ _))) = Small 1
costRedundantStmt (Let _ _ (BasicOp (Index _ slc))) =
  if all isFixDim (unSlice slc) then Small 1 else Small 0
  where
    isFixDim DimFix {} = True
    isFixDim _ = False
costRedundantStmt (Let _ _ (BasicOp FlatIndex {})) = Small 0
costRedundantStmt (Let _ _ (BasicOp Update {})) = Break
costRedundantStmt (Let _ _ (BasicOp FlatUpdate {})) = Break
costRedundantStmt (Let _ _ (BasicOp Concat {})) = Big
costRedundantStmt (Let _ _ (BasicOp Copy {})) = Big
costRedundantStmt (Let _ _ (BasicOp Manifest {})) = Big
costRedundantStmt (Let _ _ (BasicOp Replicate {})) = Big
costRedundantStmt (Let _ _ (BasicOp UpdateAcc {})) = Break
costRedundantStmt (Let _ _ (BasicOp _)) = Small 0

-------------------------
--- Diffs
-------------------------
-- 1. In src/Language/Futhark/Prop.hs
--   -                   $ tupleRecord [Scalar t_b, Scalar t_b]
--   +                   $ RetType [] . Scalar $ tupleRecord [Scalar t_b, Scalar t_b]
-- and another like that
--
-- 2. In src/Futhark/Passes.hs
--              tileLoops,
--     +        simplifyGPU,
--     +        --tileLoops,
-- 3. src/Futhark/IR/GPU/Simplify.hs
--   kernelRules =
--     standardRules <> segOpRules
--       <> ruleBook
--  -      [ RuleOp redomapIotaToLoop,
--  +      [ --RuleOp redomapIotaToLoop,
--           RuleOp SOAC.simplifyKnownIterationSOAC,
