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
      tab_props <- getProperties
      case hasMon (fromMaybe S.empty $ M.lookup (Var anm) tab_props) of
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
--     2. a subtraction of two sums of slices of the same array
--   Remember to call this twice: on the current and reversed pair.
matchUniteSums ::
  (MonadSoP Symbol e p m) =>
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
      let bidx_m_1 = bidx .-. (int2SoP 1)
          bidx_p_1 = bidx .+. (int2SoP 1)
      if bidx_m_1 == aidx_end
        then pure $ Just $ mkEquivSoPs (Sum anm aidx_beg bidx, sym1, sym2) (ms1, k1, k1)
        else
          if bidx_p_1 == aidx_beg
            then pure $ Just $ mkEquivSoPs (Sum anm bidx aidx_end, sym1, sym2) (ms1, k1, k1)
            else pure Nothing
  | Sum anm aidx_beg aidx_end <- sym1,
    Sum bnm bidx_beg bidx_end <- sym2,
    anm == bnm && ms1 == ms2 && k1 == 0 - k2 = do
      -- \^ possible match for simplifying: Sum(A[b1:e1]) - Sum(A[b2:e2])
      --   assumes both begin and end index are inclusive
      succ_1_1 <- aidx_beg FM.$<=$ bidx_beg
      succ_1_2 <- bidx_beg FM.$<=$ aidx_end
      succ_1_3 <- aidx_end FM.$<=$ bidx_end
      if succ_1_1 && succ_1_2 && succ_1_3
        then -- case: a_beg <= b_beg <= a_end <= b_end
        -- results in Sum(A[a_beg:b_beg-1] - Sum(A[a_end+1:b_end])

          pure $
            Just $
              f (aidx_beg, bidx_beg .-. int2SoP 1) (aidx_end .+. int2SoP 1, bidx_end) (anm, k1, k2, k2)
        else do
          succ_2_1 <- bidx_end FM.$<=$ aidx_end
          succ_2_2 <- bidx_beg FM.$<=$ bidx_end
          if succ_1_1 && succ_2_1 && succ_2_2
            then -- case: a_beg <= b_beg <= b_end <= a_end
            -- results in: Sum(A[a_beg:b_beg-1]) + Sum(A[b_beg+1,a_end])

              pure $
                Just $
                  f (aidx_beg, bidx_beg .-. int2SoP 1) (bidx_end .+. int2SoP 1, aidx_end) (anm, k1, k1, k2)
            else pure Nothing
  where
    f (beg1, end1) (beg2, end2) (anm, k11, k12, k22) =
      -- only k12 seems to be needed as parameter
      let trm1 = Term $ MS.insert (Sum anm beg1 end1) $ getTerm ms1
          trm2 = Term $ MS.insert (Sum anm beg2 end2) $ getTerm ms1
          sop_new = SoP $ M.insert trm1 k11 $ M.singleton trm2 k12 -- the first is always k1
          sop_old = mkOrigTerms (sym1, sym2) (ms1, k11, k22) -- this is always k1, k2
       in (sop_new, sop_old)
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
