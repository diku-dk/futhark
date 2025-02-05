module Futhark.Analysis.Proofs.AlgebraPC.Solve
  ( simplify,
    simplifyLevel,
    findSymbolLEq0,
    transClosInRangesSym,
  )
where

import Control.Monad (forM, (<=<))
import Data.MultiSet qualified as MS
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Futhark.Analysis.Proofs.AlgebraPC.All2AllDriver
import Futhark.Analysis.Proofs.AlgebraPC.Symbol
import Futhark.Analysis.Proofs.AlgebraPC.UnaryRules
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), astMap, identityMapper)
import Futhark.SoP.FourierMotzkin qualified as FM
import Futhark.SoP.Monad
import Futhark.SoP.SoP
import Language.Futhark (VName)

import Futhark.Util.Pretty
-- import Debug.Trace

sop0 :: SoP Symbol
sop0 = int2SoP 0

sop0s :: S.Set (SoP Symbol)
sop0s = S.singleton $ int2SoP 0

sop1s :: S.Set (SoP Symbol)
sop1s = S.singleton $ int2SoP 1

-- | Simplifies the input SoP on all levels
simplify ::
  (MonadSoP Symbol e Property m) =>
  SoP Symbol ->
  m (SoP Symbol)
simplify = substEquivs <=< astMap m <=< substEquivs
  where
    m = identityMapper { mapOnSoP = simplifyLevel }

-- | Performs all accurate simplifications on the
--   current level of the input SoP
simplifyLevel ::
  (MonadSoP Symbol e Property m) =>
  SoP Symbol ->
  m (SoP Symbol)
simplifyLevel sop_orig = do
  let fvs = free sop_orig
  -- pow simplifications
  sop1 <-
    if S.null $ S.filter hasPow fvs
      then pure sop_orig
      else simplifyPows simplifyLevel sop_orig
  -- unite sums with known indices from the equivalence table
  (_s2, sop2) <- simplifyOneSumBef sop1
  -- index & sum-expansion & sum-sum simplifications
  (_s3, sop3) <- -- trace ("Before All-To-All" ++ prettyString sop2) $
    if S.null $ S.filter hasIdxOrSum fvs
      then pure (False, sop2)
      else simplifyAll2All sop2
  -- peel off known end indices of sum via equivalence table
  (_s4, sop4) <- simplifyOneSumAft sop3
  -- do we need to run to a fix point?
  if sop4 == sop_orig then pure sop4 else simplifyLevel sop4

-- | Finds the next symbol to eliminate: this is the one
--   corresponding to the maximal set of VNames appearing
--   in the transitive closure of its ranges, i.e., we
--   call it ``the most dependent'' symbol
findSymbolLEq0 :: (MonadSoP Symbol e Property m) =>
  SoP Symbol -> m (SoP Symbol, Maybe (Symbol, Range Symbol))
findSymbolLEq0 sop = do
  -- perform general simplifications
  sop' <- simplify sop
  -- try to extract an equivalent SoP inequlity by "unifying" exponentials.
  sop'' <-
    if S.null $ S.filter hasPow (free sop')
      then pure sop'
      else powEquiv sop'
  -- peel off the end indices of sums if they have more specialized ranges.
  (_, sop''') <- simplifyPeelSumForFM sop''
  -- find me a symbol to eliminate
  -- trace("Begin Sym2Elim for SOP: "++prettyString sop) $
  let (syms_pow, syms) = S.partition hasPow $ free sop'''
      -- is = map (\s -> (length $ transClosInRanges rs $ S.singleton s, s)) $ S.toList syms
  is <- forM (S.toList syms) $ \ s -> do
          nms <- transClosInRangesSym s
          pure (S.size nms, s)
  -- trace("After Sym2Elim for SOP: "++prettyString sop ++ " " ++ prettyString is)
  case (is, S.size syms_pow) of
    ([], 0) -> pure (sop''', Nothing)
    _ -> do
      let sym2elim =
            if null is 
            then S.elemAt 0 syms_pow -- a pow sym
            else snd $ maximum $ is  -- Yayyy, we have a non-Pow symbol
      range <- getRangeOfSym sym2elim
      pure $ (sop''', Just (sym2elim, range))

transClosInRangesSym :: (MonadSoP Symbol e Property m) => Symbol -> m (S.Set VName)
transClosInRangesSym sym = do
  let leaders = leadingNames sym
  range <- getRangeOfSym sym
  let active = S.union (free sym) $ free range
  dep_names <- transClosHelper S.empty S.empty active
  case S.null (S.intersection leaders dep_names) of
    True  -> pure dep_names
    False -> do
      ranges <- getRanges
      equivs <- getEquivs
      error ( "Circular range encountered in sym: " ++ prettyString sym ++
              "\nDependent names: " ++ prettyString dep_names ++
              "\n" ++ prettyString ranges ++
              "\n" ++ prettyString equivs
            )
  where
    transClosHelper :: (MonadSoP Symbol e Property m) =>
        S.Set VName -> S.Set Symbol -> S.Set Symbol -> m (S.Set VName)
    transClosHelper clos_nms seen active
      | S.null active = pure clos_nms
      | (sym', active') <- S.deleteFindMin active,
        leaders  <- leadingNames sym',
        clos_nms'<- S.union clos_nms leaders,
        freesyms <- free sym',
        active'' <- S.union active' freesyms,
        seen' <- S.insert sym' seen = do
      range <- getRangeOfSym $ sym'
      let active''' = S.union active'' $ free range S.\\ seen
      transClosHelper clos_nms' seen' active'''
    --
    leadingNames :: Symbol -> S.Set VName
    leadingNames sym' =
      case sym' of
        Var nm       -> S.singleton nm
        Pow _        -> S.empty
        Idx ix _     -> leadingIdxNames ix
        Mdf _ nm _ _ -> S.singleton nm
        Sum ix _ _   -> leadingIdxNames ix
    --
    leadingIdxNames (One nm ) = S.singleton nm
    leadingIdxNames (POR nms) = nms

powEquiv ::
  (MonadSoP Symbol e Property m) =>
  SoP Symbol ->
  m (SoP Symbol)
powEquiv sop
  -- Single term: a sufficient condition for `t * 2^expo <= 0`
  --              is `t <= 0`, since 2^expo is always >= 1
  | Just (Term mset, k) <- justSingleTerm sop,
    (mset_pows, mset_others) <- MS.partition hasPow mset,
    length (MS.toOccurList mset_pows) >= 1 =
      pure $ term2SoP (Term mset_others) k
  -- Two terms involving same pow with different exponents.
  -- Sufficient conditions for
  --   `t * 2^e1 - t * 2^e2 <= 0`, which is equivalent with:
  --   `t * 2^e1 <= t * 2^e2`
  -- are:
  --    (t >= 0) && (e1 <= e2) || (t <= 0 && (e2 <= e1))
  | [(Term ms1, k1), (Term ms2, k2)] <- sopToList sop,
    (mset1_pow, mset1_others) <- MS.partition hasPow ms1,
    (mset2_pow, mset2_others) <- MS.partition hasPow ms2,
    [(pow1, m1)] <- MS.toOccurList mset1_pow,
    [(pow2, m2)] <- MS.toOccurList mset2_pow,
    (Pow (b1, expo1), Pow (b2, expo2)) <- (pow1, pow2),
    b1 == b2 && k1 == 0 - k2 && m1 == m2 && mset1_others == mset2_others = do
      let (e1, e2, k) = if k1 > 0 then (expo1, expo2, k1) else (expo2, expo1, k2)
          t_sop = term2SoP (Term mset1_others) k
      t_gt0 <- (int2SoP 0) FM.$<=$ t_sop
      if t_gt0
        then simplify $ e1 .-. e2
        else do
          t_lt0 <- (int2SoP 0) FM.$>=$ t_sop
          if t_lt0
            then simplify $ e2 .-. e1
            else pure sop
  -- The case: kfree + k*b^{expo} <= 0,
  --   where kfree and k are constants.
  | [(Term ms1, k1), (Term ms2, k2)] <- sopToList sop,
    MS.null ms1 || MS.null ms2,
    (mset, k, kfree) <- if MS.null ms1 then (ms2, k2, k1) else (ms1, k1, k2),
    k /= 0 && kfree /= 0,
    (mset_pows, mset_others) <- MS.partition hasPow mset,
    [(Pow (b, expo), m)] <- MS.toOccurList mset_pows,
    MS.null mset_others && m == 1 = do
      case (kfree > 0, k > 0) of
        (True, True) -> pure $ int2SoP 1 -- False
        (False, False) -> pure $ int2SoP 0 -- True
        (True, False) -> do
          let q = findSmallestPowGEq kfree b (0, -k)
          simplify $ int2SoP q .-. expo
        (False, True) -> do
          let q = findSmallestPowLEq (0 - kfree) b (0, k)
          if q < 0
            then pure $ int2SoP 1 -- False
            else simplify $ expo .-. int2SoP q
  where
    -- \| when called with q==0, it finds the smallest
    --     int q such that k <= acc*b^q
    --   assumes k > 0 && b > 1 && acc > 0
    findSmallestPowGEq k b (q, acc) =
      if k <= acc
        then q
        else findSmallestPowGEq k b (q + 1, b * acc)
    -- \| when called with q==0, it finds the largest
    --     int q such that k >= acc*b^q
    --   assumes k > 0 && b > 1 && acc > 0
    findSmallestPowLEq k b (q, acc) =
      if k > acc
        then findSmallestPowLEq k b (q + 1, b * acc)
        else if k == acc then q else q - 1 -- k < acc
        --
powEquiv sop = pure sop

getRangeOfSym ::
    (MonadSoP Symbol e p m) =>
    Symbol ->
    m (Range Symbol)
getRangeOfSym sym@Var{} = do
  lookupRange sym -- rs <- getRanges
--
getRangeOfSym (Idx (POR {}) _) =
  pure $ Range sop0s 1 sop1s
--
getRangeOfSym idxsym@(Idx (One arr_nm) _) = do
  ranges <- getRanges
  case M.lookup idxsym ranges of
    Nothing -> getRangeOfSym $ Var arr_nm
    Just rg -> pure rg
--
getRangeOfSym (Pow{}) = do
  pure $ Range sop1s 1 S.empty
--
getRangeOfSym (Sum (POR {}) slc_beg slc_end) = do
  let ub = S.singleton $ slc_end .-. slc_beg .+. int2SoP 1
  pure $ Range sop0s 1 ub
--
getRangeOfSym (Sum (One arr_nm) slc_beg slc_end) = do
  let slc_len = slc_end .-. slc_beg .+. int2SoP 1
  mlegal <- sop0 FM.$<=$ slc_len
  Range elm_lb m elm_ub <- lookupRange $ Var arr_nm
  if mlegal
    then do
      let [sum_lbs, sum_ubs] = map (S.map (slc_len .*.)) [elm_lb, elm_ub]
      pure $ Range sum_lbs m sum_ubs
    else do
      lenis0 <- sop0 FM.$>=$ slc_len
      if not lenis0
        then pure $ Range S.empty 1 S.empty
        else pure $ Range sop0s 1 sop0s
--
getRangeOfSym (Mdf dir arr_nm idx1 idx2) = do
  Range elm_lbs m elm_ubs <- lookupRange $ Var arr_nm
  is_1geq2 <- idx1 FM.$>=$ idx2
  is_1leq2 <- idx1 FM.$<=$ idx2
  --
  let i1_m_i2 = int2SoP m .*. (idx1 .-. idx2)
      i2_m_i1 = int2SoP m .*. (idx2 .-. idx1)
      nontriv_diff = if dir == IncS then i1_m_i2 else i2_m_i1
      -- lower-bound for all cases derived from monotonicity:
      clb = (is_1geq2 && dir == Inc) || (is_1leq2 && dir == Dec)
      clbS = (is_1geq2 && dir == IncS) || (is_1leq2 && dir == DecS)
      lbm = ifThenElif clb sop0s clbS $ S.singleton nontriv_diff
      -- upper-bound for all cases derived from monotonicity:
      cub = (is_1leq2 && dir == Inc) || (is_1geq2 && dir == Dec)
      cubS = (is_1leq2 && dir == IncS) || (is_1geq2 && dir == DecS)
      ubm = ifThenElif cub sop0s cubS $ S.singleton nontriv_diff
  -- Add the range derived from monotonicity to the existing
  --   lower- or upper-bounds of `arr_nm`;
  -- Currently we only consider the case when there are exactly
  -- one lower and one upper bound.
  if 1 /= S.size elm_lbs || 1 /= S.size elm_ubs
    then pure $ Range lbm 1 ubm
    else do
      let (elm_lb, elm_ub) = (S.elemAt 0 elm_lbs, S.elemAt 0 elm_ubs)
          (sub_lb, sub_ub) = (elm_lb .-. elm_ub, elm_ub .-. elm_lb)
          lbs = S.singleton sub_lb <> S.map (int2SoP m .*.) lbm
          ubs = S.singleton sub_ub <> S.map (int2SoP m .*.) ubm
      pure $ Range lbs m ubs
  where
    ifThenElif c1 s1 c2 s2 =
      if c1 then s1 else if c2 then s2 else mempty

-------------------------------
--- Previous Code (Garbage) ---
-------------------------------

{--
findSpecialSymbolToElim ::
  (MonadSoP Symbol e p m) =>
  SoP Symbol ->
  m (Maybe (Symbol, Range Symbol))
findSpecialSymbolToElim sop
  | (special_syms, _) <- S.partition hasIdxOrSum $ free sop,
    not (S.null special_syms) = do
      let (sum_syms, others) = S.partition hasSum special_syms
      if S.size sum_syms > 0
        then getSumRange $ S.elemAt 0 sum_syms
        else do
          let (mon_syms, idx_syms) = S.partition hasMdf others
          if S.size mon_syms > 0
            then getMonRange $ S.elemAt 0 mon_syms
            else getIdxRange $ S.elemAt 0 idx_syms
-- \^ guaranteed to have at least one elem.
findSpecialSymbolToElim _ =
  pure Nothing


getSumRange ::
  (MonadSoP Symbol e p m) =>
  Symbol ->
  m (Maybe (Symbol, Range Symbol))
getSumRange sym@(Sum (POR {}) slc_beg slc_end) = do
  let ub = S.singleton $ slc_end .-. slc_beg .+. int2SoP 1
  pure $ Just (sym, Range sop0s 1 ub)
--
getSumRange sym@(Sum (One arr_nm) slc_beg slc_end) = do
  let slc_len = slc_end .-. slc_beg .+. int2SoP 1
  mlegal <- sop0 FM.$<=$ slc_len
  Range elm_lb m elm_ub <- lookupRange $ Var arr_nm
  if mlegal
    then do
      let [sum_lbs, sum_ubs] = map (S.map (slc_len .*.)) [elm_lb, elm_ub]
      pure $ Just (sym, Range sum_lbs m sum_ubs)
    else do
      lenis0 <- sop0 FM.$>=$ slc_len
      if not lenis0
        then pure Nothing
        else pure $ Just (sym, Range sop0s 1 sop0s)
getSumRange _ = pure Nothing

getMonRange ::
  (MonadSoP Symbol e p m) =>
  Symbol ->
  m (Maybe (Symbol, Range Symbol))
getMonRange sym@(Mdf dir arr_nm idx1 idx2) = do
  Range elm_lbs m elm_ubs <- lookupRange $ Var arr_nm
  is_1geq2 <- idx1 FM.$>=$ idx2
  is_1leq2 <- idx1 FM.$<=$ idx2
  --
  let i1_m_i2 = int2SoP m .*. (idx1 .-. idx2)
      i2_m_i1 = int2SoP m .*. (idx2 .-. idx1)
      nontriv_diff = if dir == IncS then i1_m_i2 else i2_m_i1
      -- lower-bound for all cases derived from monotonicity:
      clb = (is_1geq2 && dir == Inc) || (is_1leq2 && dir == Dec)
      clbS = (is_1geq2 && dir == IncS) || (is_1leq2 && dir == DecS)
      lbm = ifThenElif clb sop0s clbS $ S.singleton nontriv_diff
      -- upper-bound for all cases derived from monotonicity:
      cub = (is_1leq2 && dir == Inc) || (is_1geq2 && dir == Dec)
      cubS = (is_1leq2 && dir == IncS) || (is_1geq2 && dir == DecS)
      ubm = ifThenElif cub sop0s cubS $ S.singleton nontriv_diff
      -- Add the range derived from monotonicity to the existing
      --   lower- or upper-bounds of `arr_nm`;
      -- Currently we only consider the case when there are exactly
      -- one lower and one upper bound.
      range =
        if 1 /= S.size elm_lbs || 1 /= S.size elm_ubs
          then Range lbm 1 ubm
          else
            let (elm_lb, elm_ub) = (S.elemAt 0 elm_lbs, S.elemAt 0 elm_ubs)
                (sub_lb, sub_ub) = (elm_lb .-. elm_ub, elm_ub .-. elm_lb)
                lbs = S.singleton sub_lb <> S.map (int2SoP m .*.) lbm
                ubs = S.singleton sub_ub <> S.map (int2SoP m .*.) ubm
             in Range lbs m ubs
  pure $ Just (sym, range)
  where
    ifThenElif c1 s1 c2 s2 =
      if c1 then s1 else if c2 then s2 else mempty
getMonRange _ = pure Nothing

getIdxRange ::
  (MonadSoP Symbol e p m) =>
  Symbol ->
  m (Maybe (Symbol, Range Symbol))
getIdxRange sym@(Idx (POR {}) _) =
  pure $ Just (sym, Range sop0s 1 $ S.singleton $ int2SoP 1)
getIdxRange sym@(Idx (One arr_nm) _) = do
  range_idx <- lookupRange $ Var arr_nm
  pure $ Just (sym, range_idx)
getIdxRange _ = pure Nothing

getPowRange ::
  (MonadSoP Symbol e p m) =>
  Symbol ->
  m (Maybe (Symbol, Range Symbol))
getPowRange sym@(Pow{}) = do
  pure $ Just (sym, Range sop1s 1 S.empty)
getPowRange _ = pure Nothing
--}
