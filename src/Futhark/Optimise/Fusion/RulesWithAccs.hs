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
  ( tryFuseWithAccs,
  )
where

import Control.Monad
import Data.Map.Strict qualified as M
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.Transform.Rename
import Futhark.Transform.Substitute

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
  Maybe (m (Stm SOACS))
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
      all ((`notNameIn` cs) . patElemName . fst) other_pr1 = Just $ do
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
            localScope (scope <> scopeOfLParams (rcrt_params ++ racc_params)) $ do
              -- add the stms of lam1
              mapM_ addStm $ stmsToList $ bodyStms $ lambdaBody lam1
              -- add the copy stms for the common accumulator
              forM_ tup_common $ \(tup1, tup2) -> do
                let (lpar1, lpar2) = (accTup4 tup1, accTup4 tup2)
                    ((nm1, _), nm2, tp_acc) = (accTup5 tup1, paramName lpar2, paramDec lpar1)
                letBind (Pat [PatElem nm2 tp_acc]) $ BasicOp $ SubExp $ Var nm1
              -- add copy stms to bring in scope x1 ... xq
              forM_ other_pr1 $ \(pat_elm, bdy_res) -> do
                let (nm, se, tp) = (patElemName pat_elm, resSubExp bdy_res, patElemType pat_elm)
                certifying (resCerts bdy_res) $
                  letBind (Pat [PatElem nm tp]) $
                    BasicOp (SubExp se)
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
        pure $ Let (Pat res_pat) (aux1 <> aux2) $ WithAcc res_w_inps' res_lam'
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
  Nothing

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
    map paramDec ps1 == map paramDec ps2,
    map paramAttrs ps1 == map paramAttrs ps2,
    lambdaReturnType lam1 == lambdaReturnType lam2,
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
       in success && map resSubExp (bodyResult bdy1) == sres2
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
    | [se11, se12] == substInSEs stab [se21, se22],
      (pels1, pels2) <- (patElems pat1, patElems pat2),
      map patElemDec pels1 == map patElemDec pels2,
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
    shapeDims shp1 == shapeDims shp2
      && map patElemName pat_els1 == winp_arrs2
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
