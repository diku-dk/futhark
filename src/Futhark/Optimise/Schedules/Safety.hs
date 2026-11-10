{-# LANGUAGE Strict #-}

-- | This module implements sanity checks for a schedule,
--     i.e., does the schedule conforms with the target code ?
--   The following restrictions are assumed and verified:
--   1. the target nest has one result
--   2. the target nest does not exhibit in-place updates or
--        WithAcc statements on the critical path (but are
--        permitted at lower levels on variables that are not
--        aliased with the ones subject to map fission and such)
--   3. fully-consumed redomaps/scanomaps having scalar bops are
--      contained at innermost levels.
--   4. the rescheduled target nest is either
--        (a) perfectly nested to the bottom, i.e., its result at
--            each level is the result of the nest OR
--        (b) the innermost level is a 0=stride recurrence, e.g.,
--            a fully-consumed redomap having scalar result,
--            followed by some scalar code (that is free of recurrences)
--            Necessarily, in this case the redomap cannot be Macc-ed! 
module Futhark.Optimise.Schedules.Safety
  ( checkValidSched
  )
where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Traversable
import Futhark.IR.SOACS
import Futhark.SoP.FourierMotzkin( ($<=$) )
import Futhark.Optimise.Schedules.ScheduleM( ScheduleM )
import Futhark.Optimise.Schedules.EnvUtils
import Futhark.Optimise.Schedules.SchedUtils
import Futhark.SoP.SoP

import Debug.Trace

-- | Checks the validity of the schedule. If the schedule is
--   wrong or ambiguous, then compilation terminates with an error.
--   Arguments:
--     1. the top-down environment
--     2. the schedule
--     3. the statement target to rescheduling, typically a map
--   Results:
--     1. a boolean denoting whether the schedule is ready to fire
--     2. a list of per-dimension bools; True indicates redundant computation
--     3. in case the schedule dictates the manifestation of result,
--        the associative and commutatuive binary-operator of the
--        accumulator and its neutral element, otherwise Nothing.
--   It checks that:
--     (i)   target nest has one result
--     (ii)  dimensions and strides match (of the schedule and target nest),
--     (iii) only maps can be subject to padding, including with accumulators
--     (iii) only maps are allowed to introduce redundant computation,
--           i.e., maps with accumulator are not.
--  Functions @stmtMatchesSched@ and @bodyMatchesSched@ suposably check that:
--    1. the target nest does not contain in-place updates or WithAccs on the critical path
--    2. the target recurrence is either perfectly nested to the bottom OR it is a
--         recurrence of stride zero and the code following it is simple scalar code.
--  ToDo:
--    1. add the change-of-index function and the redundant computation info to schedule
--    2. figure out the algorithm for computing the rescheduled type of the result, and
--       the manner in which it is obtained from the original type and schedule; this is
--       non-trivial when using accumulators (inside)  but it is critical for
--       (i) manifesting the result and for (ii) substituting the result forward.
--    3. check safety for stripmining redomaps with Macc: all such strips
--         should be on top, because we do not change the operator.
checkValidSched :: TopEnv -> HLSched -> Stm SOACS -> 
                   ScheduleM (Bool, [Bool], Maybe (Lambda SOACS, [SubExp], Commutativity))
checkValidSched _ _ stm@(Let pat _ _)
  | length (patElems pat) > 1 =
  error $ "Schedules are currently restricted to one result, stm: " ++ prettyString stm
checkValidSched tenv sched stm
  | n <- last (origids sched),
    n >= 0,
    Just dims_kind_mbop <- stmtMatchesSched (freeIn stm) tenv stm [0..n],
    (_, _, mbop) : _ <- dims_kind_mbop = do
  colapse_each_dim <- saneLmadDims tenv sched
  let strides_of_nest = reverse $ snd $ mapAccumL ff (int2SoP 1) $ reverse dims_kind_mbop
      ok_sane = all sanityCheckDim $ zip3 colapse_each_dim (zip strides_of_nest [0..n-1]) dims_kind_mbop
  pure (ok_sane, map fst colapse_each_dim, mbop)
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
checkValidSched _ sched _ =
  trace ("\n\n\n!!! Schedule FAILS : " ++ prettyString sched ++ "\n\n\n") $
    pure (False, [], Nothing)

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
saneLmadDims :: TopEnv -> HLSched -> ScheduleM [(Bool, (SoP VName, SoP VName))]
saneLmadDims _env sched = do
  let orig_dims_strd = zip (origids sched) $ zip (map snd $ dimlens sched) (map snd $ strides sched) 
      per_dim_split  = splitLmadPerDim 0 [[]] orig_dims_strd
  per_dim_res <- mapM (colapseOneDim False . reverse) per_dim_split
  trace ("\n\n\n!!!Orig Sched: " ++ prettyString per_dim_split ++ " Sane Result: " ++ prettyString per_dim_res++"\n\n\n") $
    pure per_dim_res
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
    queryGeq0 sop = int2SoP 0 $<=$ sop
    --
    colapseOneDim :: Bool -> [(SoP VName, SoP VName)] -> ScheduleM (Bool, (SoP VName, SoP VName)) 
    colapseOneDim _ [] = error "In colapseLMAD: reached illegal empty lmad case!"
    colapseOneDim kd [pt] = pure (kd, pt)
    colapseOneDim kd ( (n1,s1) : (n2_s2) : lmad ) = do
      (kd', ns) <- applyRules kd (n1,s1) (n2_s2) 
      colapseOneDim kd' $ ns : lmad
    --
    applyRules kd ns1 ns2
      | isZero (snd ns1) && isZero (snd ns2) =
      pure (kd, (fst ns1 .*. fst ns2, snd ns1))
      | isZero (snd ns1) || isZero (snd ns2) =
      error $ "applyRules: cannot combine a zero with a non-zero stride: " ++ prettyString (ns1, ns2)    
    applyRules kd ns1 ns2
      | hasLenOne ns1 = pure (kd, ns2)
      | hasLenOne ns2 = pure (kd, ns1)
    applyRules _ (n1, s1) (n2, s2)
      | s1 == s2 =
      pure (True, (n1 .+. n2 .-. int2SoP 1, s1))
    applyRules kd (n1, s1) (n2, s2)
      | ordered_syms <- S.toList (S.difference (free s2) (free s1)) ++ S.toList (free s1),
        Just (q, r) <- polyDivRem ordered_syms s2 s1 = do
      query_success <- queryGeq0 (n1 .-. q) 
      case (isZero r, query_success) of
        (False, _) -> error ( "Rule2 fails due to non-zero rem: " ++ prettyString r ++
                              " args " ++ prettyString ((n1, s1), (n2, s2)) )
        (_, False) -> error ( "Rule2 fails due to unknown pos diff: " ++ prettyString (n1 .-. q) ++
                              " args " ++ prettyString ((n1, s1), (n2, s2)) )
        _ -> do 
          let kd' = not $ isZero $ n1 .-. q
          pure ( kd || kd', (n2 .*. q .+. n1 .-. q, s1) )
    applyRules _ (n1, s1) (n2, s2) =
      error $ "SoP's poly division with rem failed for input " ++ prettyString ((n1, s1), (n2, s2))

-----------------------------------------------------
--- checking the target nest
-----------------------------------------------------

-- | Performs various sanity checks:
--   1. searches backwards the lattest recurrence nest that possibly matches the schedule
--   2. checks that the said recurrence is either perfectly nested to the bottom OR
--        it is a recurrence of zero stride and the code following it is scalar code.
--   3. checks that in-place updates and WithAccs are safely contained at inner levels
--         than the ones uncovered by the target recurrence nest, i.e.,
--         are not performed on variables aliased to those in the scope of the transformation,
--         hence guaranteeing the safety of map fission.
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
stmtMatchesSched :: Names -> TopEnv -> Stm SOACS -> [Int] ->
    Maybe [(SoP VName, SoacKind, Maybe (Lambda SOACS, [SubExp], Commutativity))]
stmtMatchesSched _ _ _ [] = Just []
stmtMatchesSched nms env stm (_:sched) =
  case getMBody stm of
    Nothing   -> Nothing
    Just (body, res) -> 
      case bodyMatchesSched nms env body sched of
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

bodyMatchesSched :: Names -> TopEnv -> Body SOACS -> [Int] ->
    Maybe [(SoP VName, SoacKind, Maybe (Lambda SOACS, [SubExp], Commutativity))]
bodyMatchesSched _ _ _ sched
  | null sched = Just []
bodyMatchesSched nms0 env body sched =
  let bdy_nms = namesFromList $ concat $ map getPatNames $ stmsToList $ bodyStms body
  in  fst $ foldl (ff (nms0 <> bdy_nms)) (Nothing, []) $ reverse $ stmsToList $ bodyStms body
  -- ^ @ff@ performs the safety checks, such as the obsence of in-place updates 
  where
    getPatNames (Let pat _ _) = map patElemName $ patElems pat
    stmGivesBodyResult (Let (Pat [pat_el]) _ _)
      | [(Var bdy_res_nm)] <- map resSubExp (bodyResult body) =
      bdy_res_nm == patElemName pat_el
    stmGivesBodyResult _ = False
    --
    ff nms _ stm
      | (True, _) <- hasExposedInPlaceOrWithaccs (False, nms) stm =
      error $ "In bodyMatchesSched: stm has In-Place Updates or WithAcc," ++
              " but it should have exit earlier " ++ prettyString stm
    ff _ (Just r, stms) _stm = (Just r, stms)
    -- ^ we have already found the target recurrence and have if it reched here
    --   then it has checked that this statement does not exposes In-Place Updates
    --   or WithAccs om the critical path; just return the result. 
    ff nms (Nothing, stms) stm =
    -- ^ we have not found the target recurrence yet
      case stmtMatchesSched nms env stm sched of
        Nothing -> (Nothing, stm : stms)
        -- ^ have not found the target recurrence yet;
        --   add stm to the accumulator list of statements
        Just rs ->
        -- ^ we have found our recurrence: check that the following code @stms@ is
        --   either empty or is scalar code and the current recurrence has stride zero.
          if null stms && stmGivesBodyResult stm
          then (Just rs, stms)
          else if all isScalarStm stms && any stmGivesBodyResult stms && stmHasStrideZero stm
               then (Just rs, stms)
               else error "In bodyMatchesSched: Incompatible Nesting of Target Recurrence!"

hasExposedInPlaceOrWithaccs :: (Bool, Names) -> Stm SOACS -> (Bool, Names)
hasExposedInPlaceOrWithaccs (True, nms) _ =
  (True, nms)
hasExposedInPlaceOrWithaccs (_, nms) stm@(Let pat _aux e)
  | BasicOp (FlatUpdate tgt_nm _slc _upd_nm) <- e,
    nameIn tgt_nm nms =
    error $ "Sanity check fails: In Place Update on " ++
            prettyString tgt_nm ++ " in stm: " ++ prettyString stm
  --
  | BasicOp (Update _ tgt_nm _slc _se) <- e,
    nameIn tgt_nm nms =
    error $ "Sanity check fails: In Place Update on " ++
            prettyString tgt_nm ++ " in stm: " ++ prettyString stm
  --
  | WithAcc inps _lam <- e,
    acc_nms <- concat (map (\(_, xs,_) -> xs) inps),
    any (`nameIn` nms) acc_nms =
    error $ "Sanity check fails: WithAcc on " ++
            prettyString acc_nms ++ " in stm: " ++ prettyString stm
  --
{--
  | Op (Hist _w hist_nms _op _lam) <- e,
    any (`nameIn` nms) hist_nms =
    error $ "Sanity check fails: Histogram on " ++
            prettyString hist_nms ++ " in stm: " ++ prettyString stm
--}
  --
  | Match _inps cases def_body _match_dec <- e =
    let bodies = def_body : map caseBody cases
        (b_ress, nms_ress) = unzip $
          map (bodyHasInPlaceOrWithAcc (False, nms)) bodies
        b_res = foldl (||) False b_ress
        nms_res = foldl (<>) mempty nms_ress
        bdy_res_nms = mapMaybe getSeName $ concat $ map (map resSubExp . bodyResult) bodies
    in  if any (`nameIn` nms_res) bdy_res_nms
        then (b_res, nms <> namesFromList (map patElemName $ patElems pat))
        -- ^ very conservative; we should do this per-result, i.e., transpose bdy_res_nms
        else (b_res, nms)
  | Loop fpar_inis _loop_form body <- e =
    let ini_bdy_nms = namesFromList $ mapMaybe getParamAlias fpar_inis
        (b_res, nms_res) = bodyHasInPlaceOrWithAcc (False, ini_bdy_nms) body
        bdy_res_nms = mapMaybe getSeName $ map resSubExp $ bodyResult body
    in  if any (`nameIn` nms_res) bdy_res_nms
        then (b_res, nms <> namesFromList (map patElemName $ patElems pat))
        -- ^ very conservative; we should do this per-result
        else (b_res, nms)
  | True =
    let alias_nms = getAlias e
    in  if any (`nameIn` nms) (namesToList alias_nms)
        then (False, nms <> namesFromList (map patElemName $ patElems pat))
        else (False, nms)
  where
    getParamAlias (p, Var nm)
      | nameIn nm nms = Just $ paramName p
    getParamAlias _ = Nothing
    bodyHasInPlaceOrWithAcc (True, names) _ = (True, names)
    bodyHasInPlaceOrWithAcc (_, names) body =
      foldl hasExposedInPlaceOrWithaccs (False, names) $ bodyStms body

getAlias :: Exp SOACS -> Names    
getAlias (BasicOp (FlatIndex tgt_nm _slc)) = oneName tgt_nm
getAlias (BasicOp (SubExp (Var tgt_nm)))   = oneName tgt_nm
getAlias (BasicOp (Opaque _ (Var tgt_nm))) = oneName tgt_nm
getAlias (BasicOp (Index tgt_nm _slc))     = oneName tgt_nm
getAlias (BasicOp (Reshape tgt_nm _newshp))= oneName tgt_nm
getAlias (BasicOp (Rearrange tgt_nm _perm))= oneName tgt_nm
getAlias (BasicOp (UpdateAcc _ tgt_nm _idx _v)) = oneName tgt_nm
getAlias (BasicOp (ArrayLit ses _)) =
  namesFromList $ mapMaybe getSeName ses
getAlias (Op (Hist _w hist_nms _op _lam))  =
  namesFromList hist_nms
getAlias _ = mempty

isScalarStm :: Stm SOACS -> Bool
isScalarStm (Let _ _ e) = isScalarExp e

isScalarExp :: Exp SOACS -> Bool
isScalarExp (Match _ cases def_body _) =
  let bodies = def_body : map caseBody cases
      stms = concat $ map (stmsToList . bodyStms) bodies
  in  all isScalarStm stms
isScalarExp (Loop _ _ _) = False
isScalarExp (WithAcc{} ) = False
isScalarExp (Op (Screma{})) = False
isScalarExp (Op (Stream{})) = False
isScalarExp (Op (Hist{})) = False
isScalarExp (Op (JVP{})) = False
isScalarExp (Op (VJP{})) = False
isScalarExp (BasicOp e)
  | ArrayLit{} <- e = False
  | Manifest{} <- e = False
  | Iota{} <- e = False
  | Replicate{} <- e = False
  | Scratch{} <- e = False
  | Reshape{} <- e = False
  | Rearrange{} <- e = False
isScalarExp _ = True

stmHasStrideZero :: Stm SOACS -> Bool
stmHasStrideZero (Let _pat _aux e)
  | Op (Screma _w _res_nms screma_form) <- e,
    Just (lam, _, _) <- oneFullyConsumedMapRed screma_form =
  all primType $ lambdaReturnType lam
stmHasStrideZero _ = False

getSeName :: SubExp -> Maybe VName
getSeName (Var nm) = Just nm
getSeName _ = Nothing

