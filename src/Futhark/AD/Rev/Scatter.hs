{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev.Scatter (vjpScatter) where

import Control.Monad
import Futhark.AD.Rev.Monad
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Util (chunk)

withinBounds :: [(SubExp, VName)] -> TPrimExp Bool VName
withinBounds [] = TPrimExp $ ValueExp (BoolValue True)
withinBounds [(q, i)] = (le64 i .<. pe64 q) .&&. (pe64 (intConst Int64 (-1)) .<. le64 i)
withinBounds (qi : qis) = withinBounds [qi] .&&. withinBounds qis

-- Generates a potential tower-of-maps lambda body for an indexing operation.
-- Assuming parameters:
--   `arr`   the array that is indexed
--   `[(w_1, i_1), (w_2, i_2), ..., (w_k, i_k)]` outer lambda formal parameters and their bounds
--   `[n_1,n_2,...]ptp` the type of the index expression `arr[i_1,i_2,...,i_k]`
-- Generates something like:
-- (\ i_1 i_2 ->
--    map (\j_1 -> ... if (i_1 >= 0 && i_1 < w_1) &&
--                        (i_2 >= 0 && i_2 < w_2) && ...
--                     then arr[i_1, i_2, ... j_1, ...]
--                     else 0
--        ) (iota n_1)
-- )
-- The idea is that you do not want to put under the `if` something
--     that is an array because it would not flatten well!
genIdxLamBody :: VName -> [(SubExp, Param Type)] -> Type -> ADM (Body SOACS)
genIdxLamBody as wpis = genRecLamBody as wpis []
  where
    genRecLamBody :: VName -> [(SubExp, Param Type)] -> [Param Type] -> Type -> ADM (Body SOACS)
    genRecLamBody arr w_pis nest_pis (Array t (Shape []) _) =
      genRecLamBody arr w_pis nest_pis (Prim t)
    genRecLamBody arr w_pis nest_pis (Array t (Shape (s : ss)) _) = do
      new_ip <- newParam "i" (Prim int64)
      let t' = Prim t `arrayOfShape` Shape ss
      inner_lam <-
        mkLambda [new_ip] $
          bodyBind =<< genRecLamBody arr w_pis (nest_pis ++ [new_ip]) t'
      let (_, orig_pis) = unzip w_pis
      buildBody_ . localScope (scopeOfLParams (orig_pis ++ nest_pis)) $ do
        iota_v <- letExp "iota" $ BasicOp $ Iota s (intConst Int64 0) (intConst Int64 1) Int64
        r <- letSubExp (baseString arr ++ "_elem") $ Op $ Screma s [iota_v] (mapSOAC inner_lam)
        pure [subExpRes r]
    genRecLamBody arr w_pis nest_pis (Prim ptp) = do
      let (ws, orig_pis) = unzip w_pis
      let inds = map paramName (orig_pis ++ nest_pis)
      localScope (scopeOfLParams (orig_pis ++ nest_pis)) $
        eBody
          [ eIf
              (toExp $ withinBounds $ zip ws $ map paramName orig_pis)
              ( do
                  r <- letSubExp "r" $ BasicOp $ Index arr $ Slice $ map (DimFix . Var) inds
                  resultBodyM [r]
              )
              (resultBodyM [Constant $ blankPrimValue ptp])
          ]
    genRecLamBody _ _ _ _ = error "In Rev.hs, helper function genRecLamBody, unreachable case reached!"

--
-- Original:
--   let ys = scatter xs is vs
-- Assumes no duplicate indices in `is`
-- Forward Sweep:
--   let xs_save = gather xs is
--   let ys = scatter xs is vs
-- Return Sweep:
--   let vs_ctrbs = gather is ys_adj
--   let vs_adj \overline{+}= vs_ctrbs -- by map or generalized reduction
--   let xs_adj = scatter ys_adj is \overline{0}
--   let xs = scatter ys is xs_save
vjpScatter1 ::
  PatElem Type ->
  StmAux () ->
  (SubExp, [VName], (ShapeBase SubExp, Int, VName)) ->
  ADM () ->
  ADM ()
vjpScatter1 pys aux (w, ass, (shp, num_vals, xs)) m = do
  let rank = length $ shapeDims shp
      (all_inds, val_as) = splitAt (rank * num_vals) ass
      inds_as = chunk rank all_inds
  xs_t <- lookupType xs
  let val_t = stripArray (shapeRank shp) xs_t
  -- computing xs_save
  xs_saves <- mkGather inds_as xs xs_t
  -- performing the scatter
  id_lam <-
    mkIdentityLambda $
      replicate (shapeRank shp) (Prim int64) ++ replicate (shapeRank shp) val_t
  addStm $ Let (Pat [pys]) aux $ Op $ Scatter w ass [(shp, num_vals, xs)] id_lam
  m
  let ys = patElemName pys
  -- XXX: Since our restoration of xs will consume ys, we have to
  -- make a copy of ys in the chance that it is actually the result
  -- of the program.  In that case the asymptotics will not be
  -- (locally) preserved, but since ys must necessarily have been
  -- constructed somewhere close, they are probably globally OK.
  ys_copy <-
    letExp (baseString ys <> "_copy") . BasicOp $
      Replicate mempty (Var ys)
  returnSweepCode $ do
    ys_adj <- lookupAdjVal ys
    -- computing vs_ctrbs and updating vs_adj
    vs_ctrbs <- mkGather inds_as ys_adj xs_t
    zipWithM_ updateAdj val_as vs_ctrbs -- use Slice?
    -- creating xs_adj
    zeros <-
      replicateM (length val_as) . letExp "zeros" $
        zeroExp $
          xs_t `setOuterSize` w
    let f_tps = replicate (rank * num_vals) (Prim int64) ++ replicate num_vals val_t
    f <- mkIdentityLambda f_tps
    xs_adj <-
      letExp (baseString xs ++ "_adj") . Op $
        Scatter w (all_inds ++ zeros) [(shp, num_vals, ys_adj)] f
    insAdj xs xs_adj -- reusing the ys_adj for xs_adj!
    f' <- mkIdentityLambda f_tps
    xs_rc <-
      auxing aux . letExp (baseString xs <> "_rc") . Op $
        Scatter w (all_inds ++ xs_saves) [(shp, num_vals, ys)] f'
    addSubstitution xs xs_rc
    addSubstitution ys ys_copy
  where
    -- Creates a potential map-nest that indexes in full the array,
    --   and applies the condition of indices within bounds at the
    --   deepest level in the nest so that everything can be parallel.
    mkGather :: [[VName]] -> VName -> Type -> ADM [VName]
    mkGather inds_as arr arr_t = do
      ips <- forM inds_as $ \idxs ->
        mapM (\idx -> newParam (baseString idx ++ "_elem") (Prim int64)) idxs

      gather_lam <- mkLambda (concat ips) . fmap mconcat . forM ips $ \idxs -> do
        let q = length idxs
            (ws, eltp) = (take q $ arrayDims arr_t, stripArray q arr_t)
        bodyBind =<< genIdxLamBody arr (zip ws idxs) eltp
      let soac = Screma w (concat inds_as) (mapSOAC gather_lam)
      letTupExp (baseString arr ++ "_gather") $ Op soac

vjpScatter ::
  VjpOps ->
  Pat Type ->
  StmAux () ->
  (SubExp, [VName], Lambda SOACS, [(Shape, Int, VName)]) ->
  ADM () ->
  ADM ()
vjpScatter ops (Pat pes) aux (w, ass, lam, written_info) m
  | isIdentityLambda lam,
    [(shp, num_vals, xs)] <- written_info,
    [pys] <- pes =
    vjpScatter1 pys aux (w, ass, (shp, num_vals, xs)) m
  | isIdentityLambda lam = do
    let sind = splitInd written_info
        (inds, vals) = splitAt sind ass
    lst_stms <- chunkScatterInps (inds, vals) (zip pes written_info)
    diffScatters (stmsFromList lst_stms)
  | otherwise =
    error "vjpScatter: cannot handle"
  where
    splitInd [] = 0
    splitInd ((shp, num_res, _) : rest) =
      num_res * length (shapeDims shp) + splitInd rest
    chunkScatterInps (acc_inds, acc_vals) [] =
      case (acc_inds, acc_vals) of
        ([], []) -> pure []
        _ -> error "chunkScatterInps: cannot handle"
    chunkScatterInps
      (acc_inds, acc_vals)
      ((pe, info@(shp, num_vals, _)) : rest) = do
        let num_inds = num_vals * length (shapeDims shp)
            (curr_inds, other_inds) = splitAt num_inds acc_inds
            (curr_vals, other_vals) = splitAt num_vals acc_vals
        vtps <- mapM lookupType curr_vals
        f <- mkIdentityLambda (replicate num_inds (Prim int64) ++ vtps)
        let stm =
              Let (Pat [pe]) aux . Op $
                Scatter w (curr_inds ++ curr_vals) [info] f
        stms_rest <- chunkScatterInps (other_inds, other_vals) rest
        pure $ stm : stms_rest
    diffScatters all_stms
      | Just (stm, stms) <- stmsHead all_stms =
        vjpStm ops stm $ diffScatters stms
      | otherwise = m
