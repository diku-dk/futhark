{-# LANGUAGE Strict #-}

-- | This module implements sanity checks for
--     a schedule, i.e., does the schedule conforms
--     with the target code ? 
module Futhark.Optimise.Schedules.Safety
  ( checkValidSched
  )
where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Traversable
import Futhark.IR.SOACS
import Futhark.Optimise.Schedules.EnvUtils
import Futhark.Optimise.Schedules.SchedUtils
import Futhark.SoP.SoP

import Debug.Trace

-- | Checks the validity of the schedule. If the schedule is
--   wrong or ambiguous, then compilation terminates with an error.
--   Arguments:
--     1. the schedule
--     2. the expression target to rescheduling, typically a map
--   Results:
--     1. a boolean denoting whether the schedule is ready to fire
--     2. a list of per-dimension bools; True indicates redundant computation
--     3. in case the schedule dictates the manifestation of result,
--        the associative and commutatuive binary-operator of the
--        accumulator and its neutral element, otherwise Nothing.
--   It checks that:
--     (i)   dimensions and strides match (of the schedule and target nest),
--     (ii)  only maps can be subject to padding, including with accumulators
--     (iii) only maps are allowed to introduce redundant computation,
--           i.e., maps with accumulator are not.
--  ToDo:
--    1. check that the target code does not contain in-place updates on the critical path
--    2. add the change-of-index function and the redundant computation info to schedule
--    3. check safety for stripmining redomaps with Macc: all such strips
--         should be on top, because we do not change the operator.
checkValidSched :: TopEnv -> HLSched -> Stm SOACS -> 
                   (Bool, [Bool], Maybe (Lambda SOACS, [SubExp], Commutativity))
checkValidSched tenv sched stm
  | n <- last (origids sched),
    n >= 0,
    Just dims_kind_mbop <- stmtMatchesSched tenv stm [0..n],
    (_, _, mbop) : _ <- dims_kind_mbop =
  let colapse_each_dim = saneLmadDims tenv sched
      strides_of_nest = reverse $ snd $ mapAccumL ff (int2SoP 1) $ reverse dims_kind_mbop
      ok_sane = trace ("Strides: " ++ prettyString strides_of_nest) $
                 all sanityCheckDim $ zip3 colapse_each_dim (zip strides_of_nest [0..n-1]) dims_kind_mbop
  in  (ok_sane, map fst colapse_each_dim, mbop)
  where
    ff cur_stride (len_sop, kd, _)
      | kd == RedoMapF || kd == DoLoop = (cur_stride, int2SoP 0)
      | kd == Map || kd == MapAccum = (cur_stride .*. len_sop, cur_stride)
      | True = error ("In checkValidSched: Unsuported Type of SOAC " ++ show kd)
    --
    sanityCheckDim ((True, _), (_, i), (_, kd, _))
      | not (kd == Map) =
      error $ "In checkValidSched: Dimension " ++ show i ++
              " has redundant computation but is not a Map!"
    sanityCheckDim ((_, (sched_len, sched_strd)), (nest_strd, i), (nest_len, _, _))
      | sched_len == nest_len = -- ^ no padding was used
      if equivSoP tenv (nest_strd, sched_strd)
      then True
      else error $ "In checkValidSched: Dimension " ++ show i ++ "stride unmatch: " ++
                   prettyString (sched_strd, nest_strd)      
    sanityCheckDim ((_, (sched_len, sched_strd)), (nest_strd, _i), (nest_len, kd, _))
      | kd == Map || kd == MapAccum,
        equivSoP tenv (nest_len, sched_len),
        equivSoP tenv (nest_strd, sched_strd) =
      True
    sanityCheckDim ((_, (sched_len, sched_strd)), (nest_strd, i), (nest_len, kd, _)) =
      error $ "In checkValidSched: Dimension " ++ show i ++ 
              " of kind " ++ show kd ++ " fails length/stride checking: " ++
              prettyString ((sched_len, nest_len), (sched_strd, nest_strd))
--
checkValidSched _ _ _ =
  trace "\n\n\nFAILS\n\n\n" (False, [], Nothing)

--
-- | Checks that @orig_dim_lens@ are morally equal to the latter result modulo
--     the padding introduce by map: if they are directly equal, then fine.
--   Otherwise the plan is that for each @(m,T,R) = strip2(M_exp)@ statement,
--     we perform the substitution @m*T*R |-> M_exp@ in each SoP and then check
--     equality with the original sizes and strides.
equivSoP :: TopEnv -> (SoP VName, SoP VName) -> Bool
equivSoP _env (sop_orig, sop_trsf)
  | isZero (sop_orig .-. sop_trsf) = True
equivSoP tenv (sop_orig, sop_trsf) =
  let subs = M.fromList $ mapMaybe (mkSubst tenv) $ S.toList $ free sop_trsf
      sop_subst = applySubsToSoP subs sop_trsf
  in  isZero $ sop_orig .-. sop_subst
  where
    mkSubst env vnm =
      case M.lookup vnm (appTilesInv env) of
        Just ((_pe, sop), syms) -> Just (syms, sop)
        Nothing -> Nothing

applySubsToSoP :: M.Map Names (SoP VName) -> SoP VName -> SoP VName
applySubsToSoP subs sop =
  foldl (.+.) (int2SoP 0) $ map (applySubsToTerm subs) $ M.toList $ getTerms sop

applySubsToTerm :: M.Map Names (SoP VName) -> (Term VName, Integer) -> SoP VName
applySubsToTerm subs (term, i)
  | (syms, sub_sop) : _ <- filter (\(syms,_) -> isFactorOf (syms2Term syms) term) $ M.toList subs,
    Just term_div <- divTerm term (syms2Term syms) =
  term2SoP term_div i .*. sub_sop
  where
    syms2Term syms = toTerm $ namesToList syms
applySubsToTerm _ (term, i) = term2SoP term i


-- | Arguments:
--    @env@ the top-down environment
--    @sched@ the scheduled whose sanity is checked
--   The Result is a per-original dimension triplet
--     1. the boolen indicates whether the dimension's split has introduced redundant computation
--     2. a tuple denoting the collapsed number-of-elements and stride of that dimension;
--        this is to be checked w.r.t. original dimensions of the target nest.
saneLmadDims :: TopEnv -> HLSched -> [(Bool, (SoP VName, SoP VName))]
saneLmadDims _env sched =
  let orig_dims_strd = zip (origids sched) $ zip (map snd $ dimlens sched) (map snd $ strides sched) 
      per_dim_split  = splitLmadPerDim 0 [[]] orig_dims_strd
      per_dim_res = map (colapseOneDim False . reverse) per_dim_split
  in  trace ("\n\n\n!!!Orig Sched: " ++ prettyString per_dim_split ++ " Sane Result: " ++ prettyString per_dim_res++"\n\n\n") $
        per_dim_res
  where
    splitLmadPerDim res_k (dim_res:res) ((k, dim_strd):lmad)
      | res_k == k = splitLmadPerDim res_k ((dim_res++[dim_strd]):res) lmad
    splitLmadPerDim km1 res lmad
      | (k, _):_ <- lmad =
        if k == km1 + 1
        then splitLmadPerDim k ([[]] ++ res) lmad
        else error "In splitLmadPerDim: a dimension index was skipped!"
    splitLmadPerDim _ res [] = reverse res
    --
    hasLenOne (n,_) = isZero $ n .-. int2SoP 1
    queryGeq0 _sop = True
    -- ^ ToDo: This should be a query under the Fourier-Motzkin Monad!!!
    --
    colapseOneDim :: Bool -> [(SoP VName, SoP VName)] -> (Bool, (SoP VName, SoP VName)) 
    colapseOneDim _ [] = error "In colapseLMAD: reached illegal empty lmad case!"
    colapseOneDim kd [pt] = (kd, pt)
    colapseOneDim kd ( (n1,s1) : (n2_s2) : lmad ) =
      let (kd', ns) = applyRules kd (n1,s1) (n2_s2) 
      in  colapseOneDim kd' $ ns : lmad
    --
    applyRules kd ns1 ns2
      | isZero (snd ns1) && isZero (snd ns2) = (kd, (fst ns1 .*. fst ns2, snd ns1))
      | isZero (snd ns1) || isZero (snd ns2) =
      error $ "applyRules: cannot combine a zero with a non-zero stride: " ++ prettyString (ns1, ns2)    
    applyRules kd ns1 ns2
      | hasLenOne ns1 = (kd, ns2)
      | hasLenOne ns2 = (kd, ns1)
    applyRules _ (n1, s1) (n2, s2)
      | s1 == s2 = (True, (n1 .+. n2 .-. int2SoP 1, s1))
    applyRules kd (n1, s1) (n2, s2)
      | ordered_syms <- S.toList (S.difference (free s2) (free s1)) ++ S.toList (free s1),
        Just (q, r) <- polyDivRem ordered_syms s2 s1 =
      case (isZero r, queryGeq0 (n1 .-. q)) of
        (False, _) -> error ( "Rule2 fails due to non-zero rem: " ++ prettyString r ++
                              " args " ++ prettyString ((n1, s1), (n2, s2)) )
        (_, False) -> error ( "Rule2 fails due to unknown pos diff: " ++ prettyString (n1 .-. q) ++
                              " args " ++ prettyString ((n1, s1), (n2, s2)) )
        _ -> let kd' = not $ isZero $ n1 .-. q
             in  ( kd || kd', (n2 .*. q .+. n1 .-. q, s1) )
    applyRules _ (n1, s1) (n2, s2) =
      error $ "SoP's poly division with rem failed for input " ++ prettyString ((n1, s1), (n2, s2))

-----------------------------------------------------
--- checking the target nest
-----------------------------------------------------

-- | Performs various sanity checks:
--   1. searches backwards the lattest recurrence nest that possibly matches the schedule
--   2. checks that the code after it does not contains a recurrence
--   3. checks that in-place updates are safely contained at inner levels than the ones
--        uncovered by the target recurrence nest, i.e., guarantees the safety of map fission
--   Arguments:
--     @env@ is the envirnment built during the top-down pass
--     @stm@ the current statement that is part of the recurrence nest.
--     @sched@ the schedule corresponding to the yet-uncovered part of the nest
--   Result is maybe an array recording a triplet for each level of the nest:
--     @SoP VName@ is the length of the corresponding recurrence
--     @SoacKind@ is the kind of the corresponding recurrence,
--       e.g., map, fully-consumed redomap, loop, etc.
--     @Maybe (Lambda SOACS, SubExp)@ is the binarry associative operator and
--       neutral element corresponding to a reduce/scan recurrence.
stmtMatchesSched :: TopEnv -> Stm SOACS -> [Int] ->
    Maybe [(SoP VName, SoacKind, Maybe (Lambda SOACS, [SubExp], Commutativity))]
stmtMatchesSched _ _ [] = Just []
stmtMatchesSched env stm (_:sched) =
  case getMBody stm of
    Nothing   -> Nothing
    Just (body, res) -> 
      case bodyMatchesSched env body sched of
        Just arr_res -> Just $ res : arr_res
        Nothing -> Nothing
  where
    getMBody (Let _ _ (Loop _ (ForLoop _ intp w) loop_body)) =
      let (_, sop) = peFromSe env (IntType intp) w
      in  Just (loop_body, (sop , DoLoop, Nothing))
    getMBody (Let pat _ (Op (Screma w _ (ScremaForm lam [] [])))) =
      let (_, sop) = peFromSe env (IntType Int64) w
      in if any isAccType $ map patElemDec $ patElems pat
         then Just (lambdaBody lam, (sop , MapAccum, Nothing))
         else Just (lambdaBody lam, (sop , Map, Nothing))
      where
        isAccType _tp@Acc{} = True
        isAccType _ = False
    getMBody (Let _ _ (Op (Screma w _ form@(ScremaForm lam _ _)))) =
      let (_, sop) = peFromSe env (IntType Int64) w
      in case oneFullyConsumedMapRed form of
           Nothing -> Just (lambdaBody lam, (sop, GenScrema, Nothing))
           opnec   -> Just (lambdaBody lam, (sop, RedoMapF, opnec))
      -- ^ can be further expanded to give more precise SoacKinds
    getMBody _ = Nothing

bodyMatchesSched :: TopEnv -> Body SOACS -> [Int] ->
    Maybe [(SoP VName, SoacKind, Maybe (Lambda SOACS, [SubExp], Commutativity))]
bodyMatchesSched _ _ sched
  | null sched = Just []
bodyMatchesSched env body sched =
  foldl f Nothing $ reverse $ stmsToList $ bodyStms body
  where
    f (Just r)  _ = Just r
    f _ stm = stmtMatchesSched env stm sched
    -- ^ ToDo: we need to add checks, e.g.,
    --         only scalar code can follow the recurrence &
    --         safety of in-place updates
