module Futhark.Analysis.Proofs.AlgebraPC.Solve
  ( simplifyOnce,
  )
where

import Data.Maybe
-- import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MultiSet as MS
-- import Futhark.SoP.Monad
import Futhark.SoP.Expression
import Futhark.SoP.SoP
import Futhark.Analysis.Proofs.AlgebraPC.Algebra
-- import Language.Futhark (VName)
import qualified Futhark.SoP.FourierMotzkin as FM


simplifyOnce :: SoP Symbol -> AlgM e (SoP Symbol)
simplifyOnce sop0 = do
  let fvs = free sop0
  sop1 <- ifM hasPow fvs simplifyPow sop0
  (_s2, sop2) <- ifSM hasSum fvs simplifySum sop1
  pure sop2

-----------------------------------------
--- 1. Simplifications related to Pow ---
-----------------------------------------

simplifyPow :: SoP Symbol -> AlgM e (SoP Symbol)
simplifyPow sop = do
  tmp <- mapM simplifyTerm $ M.toList $ getTerms sop
  pure (SoP (M.fromList tmp))
  where
  simplifyTerm :: (Term Symbol, Integer) -> AlgM e (Term Symbol, Integer)
  simplifyTerm (Term mset, k) = do
    let (mset_pows, mset_others) = MS.partition hasPow mset
        mset_tup_pows = MS.mapMaybe mpowAsTup mset_pows
        lst_pows = map normalizePow $ MS.toOccurList mset_tup_pows
        (k', map_pows') = foldl combineSamePow (k, M.empty) lst_pows
        -- ToDo: we might want to recursively simplify the obtained SoPs?
        mset_pows'' =  MS.fromList $ map Pow $ M.toList map_pows'
    pure $ (Term (mset_pows'' <> mset_others), k')
  --
  normalizePow :: ((Integer, SoP Symbol), Int) -> (Integer, SoP Symbol)
  normalizePow ((base, expnt), p) = 
    (base, (ctSoP (fromIntegral p)) .*. expnt)

---------------------------------------------------
--- 2. Simplifications related to Sum of Slices ---
---------------------------------------------------

simplifySum :: SoP Symbol -> AlgM e (Bool, SoP Symbol)
simplifySum sop = do
  let term_ks = M.toList $ getTerms sop
      exp_terms = map expandSumIdxTerm term_ks
      -- ^ this is a list of Maps matching term_ks
  pure (False, sop)

-- expSumIdx :: SoP Symbol -> [M.Map Symbol (Term Symbol, Integer)]

expandSumIdxTerm :: (Term Symbol, Integer) -> M.Map Symbol (Term Symbol, Integer)
expandSumIdxTerm (Term ms, k) =
  M.fromList $ mapMaybe f $ MS.toOccurList ms
  where
    f (sym@Idx{}, 1) = Just (sym, (Term (MS.delete sym ms), k))
    f (sym@Sum{}, 1) = Just (sym, (Term (MS.delete sym ms), k))
    f _ = Nothing

-- | when you use this function, remember to call it twice
--   on the current pair and reversed pair
findMatchHard :: (Expression e, Ord e) =>
                 (Symbol, (Term Symbol, Integer)) ->
                 (Symbol, (Term Symbol, Integer)) ->
                 AlgM e (Maybe (SoP Symbol, SoP Symbol))
findMatchHard (sym1, (ms1,k1)) (sym2, (ms2,k2))
  | Sum anm aidx_beg aidx_end <- sym1,
    Idx bnm bidx <- sym2,
    anm == bnm && ms1 == ms2 && k1 == k2 = do
  -- ^ possible match for extending a sum
  let bidx_m_1 = bidx .-. (ctSoP 1)
      bidx_p_1 = bidx .+. (ctSoP 1)
  if bidx_m_1 == aidx_end
  then pure $ Just $ mkMatchHard1 (Sum anm aidx_beg bidx, sym1, sym2) (ms1, k1)
  else if bidx_p_1 == aidx_beg
       then pure $ Just $ mkMatchHard1 (Sum anm bidx aidx_end, sym1, sym2) (ms1, k1)
       else pure Nothing
  | Sum anm aidx_beg aidx_end <- sym1,
    Sum bnm bidx_beg bidx_end <- sym2,
    anm == bnm && ms1 == ms2 && k1 == 0 - k2 = do
  -- ^ possible match for simplifying two sums Sum(A[b1:e1]) - Sum(A[b2:e2])
  --   assumes both begin and end index are inclusive
  succ_1_1 <- aidx_beg FM.$<=$ bidx_beg
  succ_1_2 <- bidx_beg FM.$<=$ aidx_end
  succ_1_3 <- aidx_end FM.$<=$ bidx_end
  if succ_1_1 && succ_1_2 && succ_1_3
  then pure $ Just $
              f (aidx_beg, bidx_end .-. ctSoP 1, aidx_end .+. ctSoP 1, bidx_end) (anm, k1, k2, k2)

{--
       do
       let (beg1, end2) = (aidx_beg, bidx_end)
           end1 = bidx_end .-. (ctSoP 1)
           beg2 = aidx_end .+. (ctSoP 1)
           trm1 = Term $ MS.insert (Sum anm beg1 end1) $ getTerm ms1
           trm2 = Term $ MS.insert (Sum anm beg2 end2) $ getTerm ms1
           sop_new = SoP $ M.insert trm1 k1 $ M.singleton trm2 k2
           sop_old = mkMatchHard0 (sym1, sym2) (ms1, k1, k2)
       pure $ Just (sop_new, sop_old)
--}
  else do
       succ_2_1 <- bidx_end FM.$<=$ aidx_end
       if succ_1_1 && succ_2_1
       then pure $ Just $
              f (aidx_beg, bidx_beg .-. ctSoP 1, bidx_end .+. ctSoP 1, aidx_end) (anm, k1, k1, k2)

{--
            do
            let (beg1, end2) = (aidx_beg, aidx_end)
                end1 = bidx_beg .-. (ctSoP 1)
                beg2 = bidx_end .+. (ctSoP 1)
                trm1 = Term $ MS.insert (Sum anm beg1 end1) $ getTerm ms1
                trm2 = Term $ MS.insert (Sum anm beg2 end2) $ getTerm ms1
                sop_new = SoP $ M.insert trm1 k1 $ M.singleton trm2 k1
                sop_old = mkMatchHard0 (sym1, sym2) (ms1, k1, k2)
            pure $ Just (sop_new, sop_old)
--}
       else pure Nothing
  where
    f (beg1, end1, beg2, end2) (anm, k11, k12, k22) =
      let trm1 = Term $ MS.insert (Sum anm beg1 end1) $ getTerm ms1
          trm2 = Term $ MS.insert (Sum anm beg2 end2) $ getTerm ms1
          sop_new = SoP $ M.insert trm1 k11 $ M.singleton trm2 k12
          sop_old = mkMatchHard0 (sym1, sym2) (ms1, k1, k22)
      in (sop_new, sop_old)
      
--
findMatchHard _ _ = pure Nothing

mkMatchHard0 :: (Symbol, Symbol)
             -> (Term Symbol, Integer, Integer)
             -> SoP Symbol
mkMatchHard0 (old_sym1, old_sym2) (Term ms, k1, k2) =
  let term1 = Term $ MS.insert old_sym1 ms
      term2 = Term $ MS.insert old_sym2 ms
  in  SoP $ M.insert term1 k1 $ M.singleton term2 k2

mkMatchHard1 :: (Symbol, Symbol, Symbol)
            -> (Term Symbol, Integer)
            -> (SoP Symbol, SoP Symbol)
mkMatchHard1 (new_sym, old_sym1, old_sym2) trm@(Term ms, k) =
  ( SoP $ M.singleton (Term (MS.insert new_sym ms)) k
  , mkMatchHard0 (old_sym1,old_sym2) (Term ms, k, k)
  )


------------------------------------------
--- Various Low-Level Helper Functions ---
------------------------------------------

ifM :: (Symbol -> Bool) -> S.Set Symbol ->
       (a -> AlgM e a) -> a -> AlgM e a
ifM hasSmth fvs f x =
  if S.null (S.filter hasSmth fvs) then pure x else f x

ifSM :: (Symbol -> Bool) -> S.Set Symbol ->
        (a -> AlgM e (Bool,a)) -> a ->
        AlgM e (Bool,a)
ifSM hasSmth fvs f x =
  if S.null (S.filter hasSmth fvs)
  then pure (False, x)
  else f x

-- impif :: Bool -> (a -> a) -> a -> a
-- impif b f x = if b then f x else x

hasPow :: Symbol -> Bool
hasPow (Pow _) = True
hasPow _ = False

hasSum :: Symbol -> Bool
hasSum (Sum {}) = True
hasSum _ = False

combineSamePow :: (Integer, M.Map Integer (SoP Symbol))
               -> (Integer, SoP Symbol)
               -> (Integer, M.Map Integer (SoP Symbol))
combineSamePow (q,tab) (b, sop) =
  let (q', sop') =
        if (q `mod` b) /= 0
         then (q, sop)
         else let (non_pow_b, pow_b_of_q) = getPowOfFactor b q
                  comb_sop = (ctSoP pow_b_of_q) .+. sop
              in  (non_pow_b, comb_sop)
      sop'' = maybe sop' (.+. sop') $ M.lookup b tab 
  in  (q', M.insert b sop'' tab)

mpowAsTup :: Symbol -> Maybe (Integer, SoP Symbol)
mpowAsTup (Pow (base, expnt)) = Just (base, expnt)
mpowAsTup _ = Nothing

getPowOfFactor :: Integer -> Integer -> (Integer, Integer)
getPowOfFactor q b
  | q `mod` b /= 0 = (q, 0)
getPowOfFactor q b =
  let (x,y) = getPowOfFactor (q `div` b) b in (x, y+1)



---------------------------------------------
--- Some Thinking but the code is garbage ---
---------------------------------------------


-- | this function is inteneded to do most of the work:
--   1. it simplifies the input SoP based on high-level rules
--   2. it finds a symbol to eleminate (via Fourier Motzking)
--      and it adjusts the environment.
--   3. it returns the symbol to eliminate, together with its
--      range, new/simplified SoP, and the new environment 
findSymbol :: SoP Symbol -> 
              AlgM e (SoP Symbol, Maybe (Symbol, Range Symbol))
findSymbol sop
  | fvs <- free sop,
    (s:_) <- S.toList fvs = do
  let r = Range { lowerBound = S.singleton zeroSoP
                , rangeMult = 1
                , upperBound = S.singleton (ctSoP 1)
                }
  pure (sop, Just (s, r))
-- the default case conservatively fails
findSymbol sop =
  pure (sop, Nothing)


-- | Meaning of first integral argument:
--     0 -> treat pows and recurse with 1
--     1 -> tream precise simplifications of sum of slices, recurse with 2
--     2 -> treat precise simplifications of pealing, recurse with 3
--     3 -> refine ranges based on monotonicity, i.e.,
--				t1*a[ind_1] - t1*a[ind_2]
--     4 -> refine ranges based on sum of slices
--              t1 * Sum(a[slc1])
--     5 -> defer to the Fourier-Motzkin
plan :: (Expression e, Ord e) => S.Set Symbol -> Integer -> SoP Symbol -> 
              AlgM e (SoP Symbol)
plan fvs 0 sop
  | pows <- mapMaybe mpowAsTup (S.toList fvs),
    not (null pows) =
  simplifyPow sop
plan fvs 0 sop = plan fvs 1 sop
-- default case: nothing to do.
plan _ _ sop =
  pure sop


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

