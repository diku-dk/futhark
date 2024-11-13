-- | Precise simplificaion rules for a pair of patterns.
--   Examples include subtraction of sum-of-slices, addition
--   of a sum-of-slice with a contiguous index, creating
--   index-subtraction pairs operating on monotonic arrays.
module Futhark.Analysis.Proofs.AlgebraPC.BinaryRules
  ( simplifyPair,
  )
where

import Data.Map.Strict qualified as M
import Data.Maybe
import Data.MultiSet qualified as MS
import Data.Set qualified as S
-- import Futhark.Analysis.Proofs.AlgebraPC.Algebra
import Futhark.Analysis.Proofs.AlgebraPC.Symbol
import Futhark.SoP.FourierMotzkin qualified as FM
import Futhark.SoP.Monad
import Futhark.SoP.SoP

-- import Futhark.Util.Pretty
-- import Debug.Trace

sop_one :: SoP Symbol
sop_one = int2SoP 1

simplifyPair ::
  (MonadSoP Symbol e Property m) =>
  (Symbol, (Term Symbol, Integer)) ->
  (Symbol, (Term Symbol, Integer)) ->
  m (Maybe (SoP Symbol, SoP Symbol))
simplifyPair t1 t2 = do
  mr1 <- matchUniteSums t1 t2
  case mr1 of
    Just r1 -> pure $ Just r1
    Nothing -> do
      mr1' <- matchUniteSums t2 t1
      case mr1' of
        Just r1' -> pure $ Just r1'
        Nothing -> matchMonIdxDif t1 t2

-- | Covers simplifications for indices of a monotonic array A:
--     1. t1 * A[i_1] - t1 * A[i_2] => t1 * MonDif A i_1 i_2 mondir
--     2. t1 * MonDif A i_1 i_2 mondir + t1 * A[i_2] => t1 * A[i_1]
--     3. t1 * MonDif A i_1 i_2 mondir - t1 * A[i_1] => - t1 * A[i_2]
--     4. t1 * MonDif A i_1 i_2 + t1 * MonDif A i_2 i_3 =>
--             t1 * MonDif A i_1 i_3
--     5. t1 * MonDif A i_1 i_2 - t1 * MonDif A i_1 i_3 =>
--             t1 * MonDif A i_3 i_2
-- This implements half of the functionality, i.e., call it twice.
matchMonIdxDif ::
  (MonadSoP Symbol e Property m) =>
  (Symbol, (Term Symbol, Integer)) ->
  (Symbol, (Term Symbol, Integer)) ->
  m (Maybe (SoP Symbol, SoP Symbol))
matchMonIdxDif t1@(sym1, (ms1, k1)) t2@(sym2, (ms2, k2))
  -- quick fail:
  | (k1 /= k2 && k1 /= 0 - k2) || (ms1 /= ms2) =
      pure Nothing
  -- case 1:
  | Idx (One anm) aidx <- sym1,
    Idx (One bnm) bidx <- sym2,
    anm == bnm && k1 == 0 - k2 = do
      mono <- askMonotonic (Var anm)
      case mono of
        Nothing -> pure Nothing
        Just dir ->
          pure $
            Just $
              mkEquivSoPs (Mdf dir anm aidx bidx, sym1, sym2) (ms1, k1, k2)
  -- case 2:
  | Mdf _ anm i1 i2 <- sym1,
    Idx (One bnm) i3 <- sym2,
    anm == bnm && k1 == k2 && i3 == i2 =
      pure $
        Just $
          mkEquivSoPs (Idx (One anm) i1, sym1, sym2) (ms1, k1, k2)
  -- case 3:
  | Mdf _ anm i1 i2 <- sym1,
    Idx (One bnm) i3 <- sym2,
    anm == bnm && k1 == 0 - k2 && i1 == i3 =
      pure $
        Just $
          mkEquivSoPs (Idx (One anm) i2, sym2, sym1) (ms1, k2, k1)
  -- reverse of cases 2 and 3:
  | (Idx {}, Mdf {}) <- (sym1, sym2) = matchMonIdxDif t2 t1
  -- case 4:
  | Mdf d1 anm i1 i2 <- sym1,
    Mdf d2 bnm i3 i4 <- sym2,
    anm == bnm && d1 == d2 && k1 == k2 && i2 == i3 =
      pure $
        Just $
          mkEquivSoPs (Mdf d1 anm i1 i4, sym1, sym2) (ms1, k1, k1)
  -- case 5:
  | Mdf d1 anm i1 i2 <- sym1,
    Mdf d2 bnm i3 i4 <- sym2,
    anm == bnm && d1 == d2 && k1 == 0 - k2 && i1 == i3 =
      pure $
        Just $
          mkEquivSoPs (Mdf d1 anm i4 i2, sym1, sym2) (ms1, k1, k2)
-- default conservative case:
matchMonIdxDif _ _ = pure Nothing

-- | This identifies a Sum-of-slice simplification, either
--     1. a sum of a slice that can be extended with an index, or
--     2. two sum of slices that come in continuation of each other:
--            Sum(X[lb: mb]) + Sum(X[mb+1:ub]) => Sum(X[lb:ub])
--     2. an overlapping addition of two sums of boolean slices, or
--     3. a subtraction of two sums of slices of the same array
--   Remember to call this twice: on the current and reversed pair.
matchUniteSums ::
  (MonadSoP Symbol e Property m) =>
  (Symbol, (Term Symbol, Integer)) ->
  (Symbol, (Term Symbol, Integer)) ->
  m (Maybe (SoP Symbol, SoP Symbol))
matchUniteSums (sym1, (ms1, k1)) (sym2, (ms2, k2))
  -- quick fail:
  | (k1 /= k2 && k1 /= 0 - k2) || (ms1 /= ms2) =
      pure Nothing
  -- case 1:
  | Sum anm aidx_beg aidx_end <- sym1,
    Idx bnm bidx <- sym2,
    anm == bnm && ms1 == ms2 && k1 == k2 = do
      -- \^ possible match for extending a sum with an index
      -- Allows x[lb-1] + Sum x[lb:lb-1] -> Sum x[lb-1:lb-1].
      valid_slice <- aidx_beg FM.$<=$ (aidx_end .+. sop_one)
      let bidx_m_1 = bidx .-. (sop_one)
          bidx_p_1 = bidx .+. (sop_one)
      case (valid_slice, bidx_m_1 == aidx_end, bidx_p_1 == aidx_beg) of
        (True, True, _) -> -- extends the upper bound
          pure $ Just $ mkEquivSoPs (Sum anm aidx_beg bidx, sym1, sym2) (ms1, k1, k1)
        (True, _, True) -> -- extends the lower bound
          pure $ Just $ mkEquivSoPs (Sum anm bidx aidx_end, sym1, sym2) (ms1, k1, k1)
        _ -> pure Nothing -- be conservative if slice is not provably non-empty
  -- case 2:
  | Sum anm aidx_beg aidx_end <- sym1,
    Sum bnm bidx_beg bidx_end <- sym2,
    anm == bnm && ms1 == ms2 && k1 == k2,
    aidx_end .+. sop_one == bidx_beg = do
      valid_1 <- aidx_beg FM.$<=$ (aidx_end .+. sop_one)
      valid_2 <- bidx_beg FM.$<=$ (bidx_end .+. sop_one)
      if valid_1 && valid_2
      then pure $ Just $ mkEquivSoPs (Sum anm aidx_beg bidx_end, sym1, sym2) (ms1, k1, k1)
      else pure Nothing       
  -- case 3:
  | Sum (POR anms) aidx_beg aidx_end <- sym1,
    Sum (POR bnms) bidx_beg bidx_end <- sym2,
    S.size anms > 0 && S.size bnms > 0,
    S.disjoint anms bnms,
    anm <- S.elemAt 0 anms,
    ms1 == ms2 && k1 == k2 = do
  disjoint <- askPairwiseDisjoint (Var anm)
  case disjoint of
    Nothing -> pure Nothing
    Just nms_disjoint_with_a -> do
      let abnms = S.union anms bnms
      if S.null $ S.difference bnms nms_disjoint_with_a
      then
        do -- \^ possible match for simplifying: Sum(A[b1:e1]) + Sum(B[b2:e2])
          success <- checkOverlappingSums (aidx_beg, aidx_end) (bidx_beg, bidx_end)
          case success of
            (True, False) -> -- case 1: a_beg <= b_beg <= a_end <= b_end
              pure $ Just $ fPOR (anms, abnms, bnms) aidx_beg bidx_beg aidx_end bidx_end
            (False, True) -> -- case 2: a_beg <= b_beg <= b_end <= a_end
              pure $ Just $ fPOR (anms, abnms, anms) aidx_beg bidx_beg bidx_end aidx_end
            _ -> pure Nothing
      else pure Nothing  
  -- case 4:
  | Sum anm aidx_beg aidx_end <- sym1,
    Sum bnm bidx_beg bidx_end <- sym2,
    anm == bnm && ms1 == ms2 && k1 == 0 - k2 = do
      -- \^ possible match for simplifying: Sum(A[b1:e1]) - Sum(A[b2:e2])
      --   assumes both begin and end index are inclusive
  success <- checkOverlappingSums (aidx_beg, aidx_end) (bidx_beg, bidx_end)
  case success of
    (True, False) -> -- case 1: a_beg <= b_beg <= a_end <= b_end
      pure $ Just $ f (anm, k2) aidx_beg bidx_beg aidx_end bidx_end
    (False, True) -> -- case 2: a_beg <= b_beg <= b_end <= a_end
      pure $ Just $ f (anm, k1) aidx_beg bidx_beg bidx_end aidx_end
    _ -> pure Nothing
  where
    checkOverlappingSums (aidx_beg, aidx_end) (bidx_beg, bidx_end) = do
      succ_1_1 <- aidx_beg FM.$<=$ bidx_beg
      succ_1_2 <- bidx_beg FM.$<=$ aidx_end
      succ_1_3 <- aidx_end FM.$<=$ bidx_end
      if succ_1_1 && succ_1_2 && succ_1_3
        then -- overlap case: a_beg <= b_beg <= a_end <= b_end
             pure (True, False)
        else do
          succ_2_1 <- bidx_end FM.$<=$ aidx_end
          succ_2_2 <- bidx_beg FM.$<=$ (bidx_end .+. sop_one)
          pure (False, succ_1_1 && succ_2_1 && succ_2_2)
          -- \^ overlap case: a_beg <= b_beg <= b_end <= a_end
    --
    fPOR (nm1, nm2, nm3) a_0 a_1 a_2 a_3 =
    -- \^ assumes a0 <= a1 <= a2 <= a3 and constructs a three-sum new sop
      let sum1 = Sum (POR nm1) a_0 (a_1 .-. sop_one)
          sum2 = Sum (POR nm2) a_1 a_2
          sum3 = Sum (POR nm3) (a_2 .+. sop_one) a_3
          trms = map insertInTerm [sum1, sum2, sum3]
          sop_new = SoP $ foldl (\acc trm -> M.insert trm k1 acc) M.empty trms
          sop_old = mkOrigTerms (sym1, sym2) (ms1, k1, k2)
      in  (sop_new, sop_old)
    --
    f (anm, k12) a_0 a_1 a_2 a_3 =
      let sum1 = Sum anm a_0 (a_1 .-. sop_one)
          sum2 = Sum anm (a_2 .+. sop_one) a_3
          (trm1, trm2) = (insertInTerm sum1, insertInTerm sum2)
          sop_new = SoP $ M.insert trm1 k1 $ M.singleton trm2 k12
          sop_old = mkOrigTerms (sym1, sym2) (ms1, k1, k2)
       in (sop_new, sop_old)
    --
    insertInTerm sym = Term $ MS.insert sym $ getTerm ms1
--
matchUniteSums _ _ = pure Nothing

-- | creates a SoP consisting of two terms of the original SoP
mkOrigTerms ::
  (Symbol, Symbol) ->
  (Term Symbol, Integer, Integer) ->
  SoP Symbol
mkOrigTerms (old_sym1, old_sym2) (Term ms, k1, k2) =
  let term1 = Term $ MS.insert old_sym1 ms
      term2 = Term $ MS.insert old_sym2 ms
   in SoP $ M.insert term1 k1 $ M.singleton term2 k2

-- | creates two equivalent SOPs corresponding to a
--   simplification. Denoting by `s` the SoP target
--   to simplification and `(s_new, s_old)` the result
--   then the "simplified" SoP would be `s + s_new - s_old`
mkEquivSoPs ::
  (Symbol, Symbol, Symbol) ->
  (Term Symbol, Integer, Integer) ->
  (SoP Symbol, SoP Symbol)
mkEquivSoPs (new_sym, old_sym1, old_sym2) trm@(Term ms, k1, _) =
  ( SoP $ M.singleton (Term (MS.insert new_sym ms)) k1,
    mkOrigTerms (old_sym1, old_sym2) trm
  )
