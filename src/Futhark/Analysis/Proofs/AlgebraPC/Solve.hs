module Futhark.Analysis.Proofs.AlgebraPC.Solve
  ( simplifyLevel,
  )
where

import Data.Maybe
-- import Data.Foldable
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MultiSet as MS
-- import Futhark.SoP.Monad
import Futhark.SoP.Expression
import Futhark.SoP.SoP
import Futhark.Analysis.Proofs.AlgebraPC.Algebra
-- import Language.Futhark (VName)
import qualified Futhark.SoP.FourierMotzkin as FM
import Futhark.SoP.Monad
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), astMap)
-- import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (mkName))

simplify :: (Expression e, Ord e) =>
  SoP Symbol -> AlgM e (SoP Symbol)
simplify = astMap m
  where
    m =
      ASTMapper
        { mapOnSymbol = pure,
          mapOnSoP = simplifyLevel
        }

simplifyLevel :: (Expression e, Ord e) => SoP Symbol -> AlgM e (SoP Symbol)
simplifyLevel sop0 = do
  let fvs = free sop0
  -- pow simplifications
  sop1 <- ifM hasPow fvs simplifyPows sop0
  -- index & sum-expansion & sum-sum simplifications
  (s2, sop2) <- ifSM hasIdxOrSum fvs simplifySumMonIdxFP sop1
  -- peel off known indices by looking in the equality table
  equivs <- getEquivs
  let (s3, sop3) = peelOffSumsFP equivs sop2
  -- do we need to run to a fix point ??
  if s2 || s3 then simplifyLevel sop3 else pure sop3

-----------------------------------------
--- 1. Simplifications related to Pow ---
-----------------------------------------

simplifyPows :: SoP Symbol -> AlgM e (SoP Symbol)
simplifyPows sop = do
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
    (base, (int2SoP (fromIntegral p)) .*. expnt)

---------------------------------------------------
--- 2. Simplifications related to Sum of Slices ---
---------------------------------------------------

simplifySumMonIdxFP :: (Expression e, Ord e) =>
                     SoP Symbol -> AlgM e (Bool, SoP Symbol)
simplifySumMonIdxFP sop = do
  let exp_terms =
        map expandSumIdxTerm $
        M.toList $ getTerms sop
  -- ^ this is a list of Maps matching sop's terms
  mr <- matchLstQuad exp_terms
  -- ^ try all with all matches (quadratic)
  case mr of
    Nothing -> pure (False, sop)
    Just (sop_new, sop_old) -> do
      (_, sop') <- simplifySumMonIdxFP $ (sop .-. sop_old) .+. sop_new
      -- ^ simplify to a fix point.
      pure (True, sop')
  where
    matchLstQuad [] = pure Nothing
    matchLstQuad (el:els) = do
      mr <- foldM (ff el) Nothing els
      case mr of
        Just{} -> pure mr
        Nothing-> matchLstQuad els
    ff _ acc@Just{} _ = pure acc
    ff tab1 Nothing tab2 = matchMapWithMap tab1 tab2
    matchMapWithMap tab1 tab2 = 
      matchAllWithAll simplifyPair (M.toList tab1) (M.toList tab2)

expandSumIdxTerm :: (Term Symbol, Integer) -> M.Map Symbol (Term Symbol, Integer)
expandSumIdxTerm (Term ms, k) =
  M.fromList $ mapMaybe f $ MS.toOccurList ms
  where
    newSymTerm sym = Just (sym, (Term (MS.delete sym ms), k))
    f (sym@Idx{}, 1) = newSymTerm sym
    f (sym@Sum{}, 1) = newSymTerm sym
    f (sym@Mdf{}, 1) = newSymTerm sym
    f _ = Nothing

matchAllWithAll :: (a -> a -> AlgM e (Maybe b))
                -> [a] -> [a] -> AlgM e (Maybe b)
matchAllWithAll fMatch els1 els2 =
  foldM (ff1 els2) Nothing els1
  where
    ff1 _ acc@Just{} _ = pure acc
    ff1 lst Nothing el1  = foldM (ff2 el1) Nothing lst
    ff2 _ acc@Just{} _ = pure acc
    ff2 el1 Nothing el2 = fMatch el1 el2

----------------------------------------------------
--- 3. ReWrite Rules for Precise Simplifications ---
----------------------------------------------------

simplifyPair :: (Expression e, Ord e) =>
                (Symbol, (Term Symbol, Integer)) ->
                (Symbol, (Term Symbol, Integer)) ->
                AlgM e (Maybe (SoP Symbol, SoP Symbol))
simplifyPair t1 t2 = do
  mr1 <- matchUniteSums t1 t2
  case mr1 of
    Just r1 -> pure $ Just r1
    Nothing -> do
      mr1' <- matchUniteSums t2 t1
      case mr1' of
        Just r1'-> pure $ Just r1'
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
matchMonIdxDif :: (Expression e, Ord e) =>
                  (Symbol, (Term Symbol, Integer)) ->
                  (Symbol, (Term Symbol, Integer)) ->
                  AlgM e (Maybe (SoP Symbol, SoP Symbol))
matchMonIdxDif t1@(sym1, (ms1,k1)) t2@(sym2, (ms2,k2))
  -- quick fail:
  | (k1 /= k2 && k1 /= 0-k2) || (ms1 /= ms2) =
  pure Nothing
  -- case 1:
  | Idx anm aidx <- sym1,
    Idx bnm bidx <- sym2,
    anm == bnm && k1 == 0-k2 = do
  tab_props  <- getProperties
  case hasMon (fromMaybe S.empty $ M.lookup (Var anm) tab_props) of
    Nothing -> pure Nothing
    Just dir-> pure $ Just $
                 mkEquivSoPs (Mdf dir anm aidx bidx, sym1, sym2) (ms1, k1, k2)
  -- case 2:
  | Mdf _ anm i1 i2 <- sym1,
    Idx bnm i3 <- sym2,
    anm == bnm && k1 == k2 && i3 == i2 =
  pure $ Just $
    mkEquivSoPs (Idx anm i1, sym1, sym2) (ms1, k1, k2)
  -- case 3:
  | Mdf _ anm i1 i2 <- sym1,
    Idx bnm i3 <- sym2,
    anm == bnm && k1 == 0 - k2 && i1 == i3 =
  pure $ Just $
    mkEquivSoPs (Idx anm i2, sym2, sym1) (ms1, k2, k1)
  -- reverse of cases 2 and 3:
  | (Idx{}, Mdf{}) <- (sym1, sym2) = matchMonIdxDif t2 t1
  -- case 4:
  | Mdf d1 anm i1 i2 <- sym1,
    Mdf d2 bnm i3 i4 <- sym2,
    anm == bnm && d1 == d2 && k1 == k2 && i2 == i3 =
  pure $ Just $
    mkEquivSoPs (Mdf d1 anm i1 i4, sym1, sym2) (ms1, k1, k1)
  -- case 5:
  | Mdf d1 anm i1 i2 <- sym1,
    Mdf d2 bnm i3 i4 <- sym2,
    anm == bnm && d1 == d2 && k1 == 0-k2 && i1 == i3 =
  pure $ Just $
    mkEquivSoPs (Mdf d1 anm i4 i2, sym1, sym2) (ms1, k1, k2)
-- default conservative case:
matchMonIdxDif _ _ = pure Nothing

-- | This identifies a Sum-of-slice simplification, either
--     1. a sum of a slice that can be extended with an index, or
--     2. a subtraction of two sums of slices of the same array
--   Remember to call this twice: on the current and reversed pair.
matchUniteSums :: (Expression e, Ord e) =>
                  (Symbol, (Term Symbol, Integer)) ->
                  (Symbol, (Term Symbol, Integer)) ->
                  AlgM e (Maybe (SoP Symbol, SoP Symbol))
matchUniteSums (sym1, (ms1,k1)) (sym2, (ms2,k2))
  -- quick fail:
  | (k1 /= k2 && k1 /= 0-k2) || (ms1 /= ms2) =
  pure Nothing
  -- case 1:
  | Sum anm aidx_beg aidx_end <- sym1,
    Idx bnm bidx <- sym2,
    anm == bnm && ms1 == ms2 && k1 == k2 = do
  -- ^ possible match for extending a sum with an index
  let bidx_m_1 = bidx .-. (int2SoP 1)
      bidx_p_1 = bidx .+. (int2SoP 1)
  if bidx_m_1 == aidx_end
  then pure $ Just $ mkEquivSoPs (Sum anm aidx_beg bidx, sym1, sym2) (ms1, k1, k1)
  else if bidx_p_1 == aidx_beg
       then pure $ Just $ mkEquivSoPs (Sum anm bidx aidx_end, sym1, sym2) (ms1, k1, k1)
       else pure Nothing
  | Sum anm aidx_beg aidx_end <- sym1,
    Sum bnm bidx_beg bidx_end <- sym2,
    anm == bnm && ms1 == ms2 && k1 == 0-k2 = do
  -- ^ possible match for simplifying: Sum(A[b1:e1]) - Sum(A[b2:e2])
  --   assumes both begin and end index are inclusive
  succ_1_1 <- aidx_beg FM.$<=$ bidx_beg
  succ_1_2 <- bidx_beg FM.$<=$ aidx_end
  succ_1_3 <- aidx_end FM.$<=$ bidx_end
  if succ_1_1 && succ_1_2 && succ_1_3
  then -- case: a_beg <= b_beg <= a_end <= b_end
       -- results in Sum(A[a_beg:b_beg-1] - Sum(A[a_end+1:b_end])
       pure $ Just $
              f (aidx_beg, bidx_beg .-. int2SoP 1) (aidx_end .+. int2SoP 1, bidx_end) (anm, k1, k2, k2)
  else do
       succ_2_1 <- bidx_end FM.$<=$ aidx_end
       if succ_1_1 && succ_2_1
       then -- case: a_beg <= b_beg <= b_end <= a_end
            -- results in: Sum(A[a_beg:b_beg-1]) + Sum(A[b_beg+1,a_end])
            pure $ Just $
              f (aidx_beg, bidx_beg .-. int2SoP 1) (bidx_end .+. int2SoP 1, aidx_end) (anm, k1, k1, k2)
       else pure Nothing
  where
    f (beg1, end1) (beg2, end2) (anm, k11, k12, k22) = -- only k12 seems to be needed as parameter
      let trm1 = Term $ MS.insert (Sum anm beg1 end1) $ getTerm ms1
          trm2 = Term $ MS.insert (Sum anm beg2 end2) $ getTerm ms1
          sop_new = SoP $ M.insert trm1 k11 $ M.singleton trm2 k12  -- the first is always k1
          sop_old = mkOrigTerms (sym1, sym2) (ms1, k11, k22) -- this is always k1, k2
      in (sop_new, sop_old)
--
matchUniteSums _ _ = pure Nothing

-- | creates a SoP consisting of two terms of the original SoP
mkOrigTerms :: (Symbol, Symbol)
            -> (Term Symbol, Integer, Integer)
            -> SoP Symbol
mkOrigTerms (old_sym1, old_sym2) (Term ms, k1, k2) =
  let term1 = Term $ MS.insert old_sym1 ms
      term2 = Term $ MS.insert old_sym2 ms
  in  SoP $ M.insert term1 k1 $ M.singleton term2 k2

-- | creates two equivalent SOPs corresponding to a
--   simplification. Denoting by `s` the SoP target
--   to simplification and `(s_new, s_old)` the result
--   then the "simplified" SoP would be `s + s_new - s_old`
mkEquivSoPs :: (Symbol, Symbol, Symbol)
            -> (Term Symbol, Integer, Integer)
            -> (SoP Symbol, SoP Symbol)
mkEquivSoPs (new_sym, old_sym1, old_sym2) trm@(Term ms, k1, _) =
  ( SoP $ M.singleton (Term (MS.insert new_sym ms)) k1
  , mkOrigTerms (old_sym1,old_sym2) trm
  )

---------------------------------------------------------------
--- 4. Peeling off first/last known elements of a slice-sum ---
---------------------------------------------------------------

peelOffSumsFP :: M.Map Symbol (SoP Symbol) -> SoP Symbol -> (Bool, SoP Symbol)
peelOffSumsFP equivs sop
  | hasPeelableSums sop =
  case peelOffSums equivs sop of
    (False, _)   -> (False, sop)
    (True, sop') -> -- fix point
      let (_, sop'') = peelOffSumsFP equivs sop'
      in  (True, sop'')
  where
    hasPeelableSums = any hasPeelableSumSym . S.toList . free
    hasPeelableSumSym (Sum nm beg end) =
      isJust (M.lookup (Idx nm beg) equivs) ||
      isJust (M.lookup (Idx nm end) equivs)
    hasPeelableSumSym _ = False
--
peelOffSumsFP _ sop = (False, sop)


peelOffSums :: M.Map Symbol (SoP Symbol) -> SoP Symbol -> (Bool, SoP Symbol)
peelOffSums equivs sop = do
  case foldl peelTerm Nothing (M.toList (getTerms sop)) of
    Nothing -> (False, sop)
    Just (old_term_sop, new_sop) ->
      (True, (sop .-. old_term_sop) .+. new_sop)
  where
    peelTerm acc@(Just{}) _ = acc
    peelTerm Nothing (t,k)  =
      let mres = foldl peelSymb Nothing $ MS.toOccurList $ getTerm t
      in  case mres of
            Nothing -> Nothing
            Just (sop_sym, sum_sym) ->
              let ms'= MS.delete sum_sym $ getTerm t
                  sop' = sop_sym .*. term2SoP (Term ms') k
              in  Just (term2SoP t k, sop')
    peelSymb acc@(Just{}) _ = acc
    peelSymb Nothing (sym@(Sum nm beg end), 1) =
    -- ^ ToDo: extend for any multiplicity >= 1
      let mfst_el = M.lookup (Idx nm beg) equivs
          mlst_el = M.lookup (Idx nm end) equivs
      in case (mfst_el, mlst_el) of
           (Just fst_el, Nothing) ->
             let new_sum = Sum nm (beg .+. int2SoP 1) end
             in  Just (fst_el .+. sym2SoP new_sum, sym)
           (Nothing, Just lst_el) ->
             let new_sum = Sum nm beg (end .-. int2SoP 1)
             in  Just (lst_el .+. sym2SoP new_sum, sym)
           (Just fst_el, Just lst_el) ->
             let new_sum = Sum nm (beg .+. int2SoP 1) (end .-. int2SoP 1)
             in  Just (fst_el .+. lst_el .+. sym2SoP new_sum, sym)
           (Nothing, Nothing) -> Nothing
    peelSymb Nothing _ = Nothing
        
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
hasSum (Sum{}) = True
hasSum _ = False

hasIdx :: Symbol -> Bool
hasIdx (Idx {}) = True
hasIdx (Mdf {}) = True
hasIdx _ = False

hasIdxOrSum :: Symbol -> Bool
hasIdxOrSum x = hasIdx x || hasSum x

hasMon :: S.Set Property -> Maybe MonDir
hasMon props
  | S.null props = Nothing
  | Monotonic dir:_ <- filter f (S.toList props) =
    Just dir
  where f (Monotonic _) = True
        f _ = False
hasMon _ = Nothing

combineSamePow :: (Integer, M.Map Integer (SoP Symbol))
               -> (Integer, SoP Symbol)
               -> (Integer, M.Map Integer (SoP Symbol))
combineSamePow (q,tab) (b, sop) =
  let (q', sop') =
        if (q `mod` b) /= 0
         then (q, sop)
         else let (non_pow_b, pow_b_of_q) = getPowOfFactor b q
                  comb_sop = (int2SoP pow_b_of_q) .+. sop
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



---------------------------------------------------
--- Some Thinking but the code below is garbage ---
---------------------------------------------------


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
                , upperBound = S.singleton (int2SoP 1)
                }
  pure (sop, Just (s, r))
-- the default case conservatively fails
findSymbol sop =
  pure (sop, Nothing)



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

