module Futhark.Analysis.Proofs.AlgebraPC.Solve
  ( simplify,
    simplifyLevel,
    findSymbolLEq0,
  )
where

import Data.MultiSet qualified as MS
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraPC.All2AllDriver
import Futhark.Analysis.Proofs.AlgebraPC.Symbol
import Futhark.Analysis.Proofs.AlgebraPC.UnaryRules
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), astMap)
import Futhark.SoP.FourierMotzkin qualified as FM
import Futhark.SoP.Monad
import Futhark.SoP.SoP
import Control.Monad ((<=<))

simplify ::
  (MonadSoP Symbol e Property m) =>
  SoP Symbol ->
  m (SoP Symbol)
simplify = astMap m <=< substEquivs
  where
    m =
      ASTMapper
        { mapOnSymbol = pure,
          mapOnSoP = simplifyLevel
        }

simplifyLevel ::
  (MonadSoP Symbol e Property m) =>
  SoP Symbol ->
  m (SoP Symbol)
simplifyLevel sop0 = do
  let fvs = free sop0
  -- pow simplifications
  sop1 <-
    if S.null $ S.filter hasPow fvs
      then pure sop0
      else simplifyPows simplifyLevel sop0
  -- index & sum-expansion & sum-sum simplifications
  (s2, sop2) <-
    if S.null $ S.filter hasIdxOrSum fvs
      then pure (False, sop1)
      else simplifyAll2All sop1
  -- peel off known indices by looking in the equality table
  equivs <- getEquivs
  let (s3, sop3) = peelOffSumsFP equivs sop2
  -- do we need to run to a fix point ?
  if s2 || s3 then simplifyLevel sop3 else pure sop3

---------------------------------------------------
--- Some Thinking but the code below is garbage ---
---------------------------------------------------

-- | finds a symbol
findSymbolLEq0 ::
  (MonadSoP Symbol e Property m) =>
  SoP Symbol ->
  m (SoP Symbol, Maybe (Symbol, Range Symbol))
findSymbolLEq0 sop = do
  sop' <- simplify sop
  sop'' <-
    if S.null $ S.filter hasPow (free sop')
      then pure sop'
      else powEquiv sop'
  -- find me a special symbol to eliminate; please refine the ranges for it
  -- if none such symbol was found, default to `FM.findSymLEq0Def`
  msymrg <- findSpecialSymbolToElim sop''
  case msymrg of
    Just {} -> pure (sop'', msymrg)
    Nothing -> findSymLEq0Def sop''

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
    --

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

findSpecialSymbolToElim ::
  (MonadSoP Symbol e p m) =>
  SoP Symbol ->
  m (Maybe (Symbol, Range Symbol))
findSpecialSymbolToElim sop
  | (special_syms, _) <- S.partition hasIdxOrSum $ free sop,
    not (S.null special_syms) = do
      let (sum_syms, others) = S.partition hasSum sum_syms
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

sop0s :: S.Set (SoP Symbol)
sop0s = S.singleton (int2SoP 0)

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
  mlegal <- (int2SoP 0) FM.$<=$ slc_len
  Range elm_lb m elm_ub <- lookupRange $ Var arr_nm
  if mlegal
    then do
      let [sum_lbs, sum_ubs] = map (S.map (slc_len .*.)) [elm_lb, elm_ub]
      pure $ Just (sym, Range sum_lbs m sum_ubs)
    else do
      lenis0 <- (int2SoP 0) FM.$>=$ slc_len
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
        if 1 /= S.size elm_lbs && 1 /= S.size elm_ubs
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

{--
PLAN:

1. find `syms` the symbols appearing in SoP
2. investigate the opportunities for applying the simplification
   rules that do not lose precision:
     -- normalizing sums of slices
     -- pealing the first/last symbol (of known range) from a sum slice
3. invesitgate the opportunities for creating new ranges from properties
     such as sum-slices or monotonicity.
   3.1 For example, if we have a symbol such as `Sum(a[ind_1: ind_2])` then
   	    this can be replaced with a new VName symbol `SA_slc` with bounds:
   	        (a) In case one can prove `ind_1 > ind_2`:
		 		{ LB = 0, UB = 0 }
		 	(b) In case one can prove `ind_1 <= ind_2
		 		{ LB = (ind_2 - ind_1) * lowerBound(a)
		 		, UB = (ind_2 - ind_1) * upperBound(a)
		 		}
   3.2 For example, if `a` is known to be strictly increasing monotonically,
   	    and `ind_1 > ind_2` is provable, then SoP:
     		`t1 * a[ind_1] - t1 * a[ind_2] + r`
     	can be transformed to:
     		`t1 * a_diff + r`
     	where the range of `a_diff` is:
        	{ LB = ind_1 - ind_2, UB = upperBound(a) - lowerBound(a) }
4. choose the most-dependent variable to eliminate according to the range table,
   i.e., here it should be safe to use the generic functionality of SoP
   (`transClosInRanges`)
--}

-- f :: (SoP Symbol >= 0) -> AlgM e Bool
-- f sop = do
--   modifyEnv $ undefined
--   undefined

-- runF :: (SoP Symbol >= 0) -> AlgEnv Symbol e Property -> VNameSource -> (Bool, VEnv e)
-- runF sop env vns= runAlgM (f sop) env vns

-- rules :: RuleBook (SoP Symbol) Symbol (AlgM e)
-- rules = []

-- (&<) ::  SoP Symbol -> SoP Symbol ->  AlgM e Bool
-- sop1 &< sop2 = do
--  prop  <- getProperties
--  sop1 F.$<$ sop2 -- fourier mo
