-- | Precise simplificaion rules for one pattern.
--   Examples include multiplication of Pow Symbols
--   and peeling off known Indexes from the beginning
--   or end of sum-of-slices.
module Futhark.Analysis.Properties.AlgebraPC.UnaryRules
  ( simplifyPows,
    simplifyOneSumBef,
    simplifyOneSumAft,
    simplifyPeelSumForFM,
  )
where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.MultiSet qualified as MS
import Data.Set qualified as S
import Futhark.Analysis.Properties.AlgebraPC.Symbol
-- lookupRange

import Futhark.Analysis.Properties.Property (hasDisjoint, Property (Rng, For), askRng, askForRng, Predicate (Predicate), mapProperty)
import Futhark.SoP.FourierMotzkin qualified as FM
import Futhark.SoP.Monad (MonadSoP, getEquivs, getProperties, getRanges)
import Futhark.SoP.SoP
import Language.Futhark (VName)

-- import Futhark.Util.Pretty
-- import Debug.Trace

-----------------------------------------
--- 1. Simplifications related to Pow ---
-----------------------------------------

simplifyPows ::
  (MonadSoP Symbol e p m) =>
  (SoP Symbol -> m (SoP Symbol)) ->
  SoP Symbol ->
  m (SoP Symbol)
simplifyPows simplifyLevel sop = do
  lst <- mapM simplifyTerm $ M.toList $ getTerms sop
  let sop' = normalize $ SoP $ foldl ff M.empty lst
  pure $ reducePairPows sop'
  where
    -- pure $ SoP $ M.fromList lst   -- BIG BUG!!!
    -- pure $ foldl (.+.) sop_zero $ map (\ (t,i) -> term2SoP t i) lst

    ff acc (t, i) =
      case M.lookup t acc of
        Nothing -> M.insert t i acc
        Just j -> M.insert t (i + j) acc
    -- simplifyTerm :: (Term Symbol, Integer) -> AlgM e (Term Symbol, Integer)
    simplifyTerm (Term mset, k) = do
      let (mset_pows, mset_others) = MS.partition hasPow mset
          mset_tup_pows = MS.mapMaybe mpowAsTup mset_pows
          lst_pows = map normalizePow $ MS.toOccurList mset_tup_pows
          (k', map_pows') = foldl combineSamePow (k, M.empty) lst_pows
      mset_pows'' <-
        forM (M.toList map_pows') $ \(b, p_sop) -> do
          p_sop' <- simplifyLevel p_sop
          -- \^ we simplify the exponents
          pure $ Pow (b, p_sop')
      pure $ (Term (MS.fromList mset_pows'' <> mset_others), k')
    --
    normalizePow :: ((Integer, SoP Symbol), Int) -> (Integer, SoP Symbol)
    normalizePow ((base, expnt), p) =
      (base, (int2SoP (fromIntegral p)) .*. expnt)

mpowAsTup :: Symbol -> Maybe (Integer, SoP Symbol)
mpowAsTup (Pow (base, expnt)) = Just (base, expnt)
mpowAsTup _ = Nothing

combineSamePow ::
  (Integer, M.Map Integer (SoP Symbol)) ->
  (Integer, SoP Symbol) ->
  (Integer, M.Map Integer (SoP Symbol))
combineSamePow (q, tab) (b, sop) =
  let (q', sop') =
        case getPowOfFactor q b of
          (_, 0) -> (q, sop)
          (r, p) -> (r, int2SoP p .+. sop)
      sop'' = maybe sop' (.+. sop') $ M.lookup b tab
   in (q', M.insert b sop'' tab)
  where
    getPowOfFactor :: Integer -> Integer -> (Integer, Integer)
    getPowOfFactor qq bb = getPowOfFactorTR qq bb 0
    getPowOfFactorTR qq bb pr
      | qq `mod` bb /= 0 = (qq, pr)
    getPowOfFactorTR qq bb pr =
      getPowOfFactorTR (qq `div` bb) bb (pr + 1)

reducePairPows :: SoP Symbol -> SoP Symbol
reducePairPows sop =
  let lst = M.toList $ getTerms sop
      lst' = filter (any hasPow . MS.elems . getTerm . fst) lst
   in case findSatisfyingPair hasPattern lst' of
        Nothing -> sop
        Just (t1, t2, r) ->
          reducePairPows $ foldl (.+.) (int2SoP 0) $ map (\(t, i) -> term2SoP t i) (t1 : t2 : r : lst)
  where
    findSatisfyingPair _ [] = Nothing
    findSatisfyingPair p (a : as) =
      case findPairHelper p a as of
        Just r -> Just r
        Nothing -> findSatisfyingPair p as
    --
    findPairHelper _ _ [] = Nothing
    findPairHelper p x (y : ys) =
      case p x y of
        Just r -> Just r
        Nothing -> findPairHelper p x ys
    --
    hasPattern (t1, i1) (t2, i2)
      | i1 == -i2,
        (msetpows1, others1) <- MS.partition hasPow (getTerm t1),
        (msetpows2, others2) <- MS.partition hasPow (getTerm t2),
        others1 == others2,
        [(base1, expnt1)] <- MS.toList (MS.mapMaybe mpowAsTup msetpows1),
        [(base2, expnt2)] <- MS.toList (MS.mapMaybe mpowAsTup msetpows2),
        base1 == base2 && base1 == 2 =
          let diff = expnt1 .-. expnt2
              mr =
                if diff == int2SoP 1
                  then Just (Pow (base1, expnt2), i1)
                  else
                    if diff == int2SoP (0 - 1)
                      then Just (Pow (base1, expnt1), i2)
                      else Nothing
           in case mr of
                Nothing -> Nothing
                Just (r, i) -> Just ((t1, -i1), (t2, -i2), (Term (MS.insert r others1), i))
    hasPattern _ _ = Nothing

---------------------------------------------------------------
--- 2. Pre Simplification of each (individual) slice sum:   ---
---      i.e., before applying binary simplifications       ---
---    2.1. sum x[lb .. ub] => 0     whenever lb  > ub      ---
---    2.2. uniting a "potentially nicely-empty slice" with ---
---           a first/last known element. This requires FM  ---
---           to check nicety:  ub - lb + 1 >= 0            ---
---------------------------------------------------------------

sop_one :: SoP Symbol
sop_one = int2SoP 1

type FoldFunTp m =
  Maybe (SoP Symbol, Symbol) ->
  (Symbol, Int) ->
  m (Maybe (SoP Symbol, Symbol))

simplifyOneSumBef ::
  (MonadSoP Symbol e p m) => SoP Symbol -> m (Bool, SoP Symbol)
simplifyOneSumBef sop = do
  equivs <- getEquivs
  sop' <- elimEmptySums sop
  (success, sop'') <- unaryOpOnSumFP (hasUnitingSums equivs) uniteSumSym sop'
  pure (success, sop'')

hasUnitingSums :: M.Map Symbol (SoP Symbol) -> (SoP Symbol) -> Bool
hasUnitingSums equivs =
  any hasUnitingSumSym . S.toList . free
  where
    hasUnitingSumSym (Sum (POR nms) beg end)
      | S.size nms > 1 =
          any hasUnitingSumSym $
            map (\nm -> Sum (POR (S.singleton nm)) beg end) $
              S.toList nms
    hasUnitingSumSym (Sum nm beg end) =
      isJust (M.lookup (Idx nm (beg .-. sop_one)) equivs)
        || isJust (M.lookup (Idx nm (end .+. sop_one)) equivs)
    hasUnitingSumSym _ = False

uniteSumSym :: (MonadSoP Symbol e p m) => FoldFunTp m
uniteSumSym acc@(Just {}) _ = pure acc
uniteSumSym Nothing (sym@(Sum nm beg end), 1) = do
  -- \^ ToDo: extend for any multiplicity >= 1
  equivs <- getEquivs
  valid_slice <- beg FM.$<=$ (end .+. sop_one)
  let beg_m_1 = beg .-. sop_one
      end_p_1 = end .+. sop_one
  -- \^ do we need to further simplify these?
  mfst_el <- getEquivSoP equivs $ Idx nm beg_m_1
  mlst_el <- getEquivSoP equivs $ Idx nm end_p_1
  case (valid_slice, mfst_el, mlst_el) of
    (False, _, _) ->
      pure Nothing
    (True, Just fst_el, Nothing) -> do
      let new_sum = Sum nm beg_m_1 end
      pure $ Just (sym2SoP new_sum .-. fst_el, sym)
    (True, Nothing, Just lst_el) -> do
      let new_sum = Sum nm beg end_p_1
      pure $ Just (sym2SoP new_sum .-. lst_el, sym)
    (True, Just fst_el, Just lst_el) -> do
      let new_sum = Sum nm beg_m_1 end_p_1
      pure $ Just (sym2SoP new_sum .-. (fst_el .+. lst_el), sym)
    (True, Nothing, Nothing) -> pure Nothing
uniteSumSym Nothing _ = pure Nothing

-- | Transforms a "known" array index to its value,
--   by looking it up in the table of equivalences.
getEquivSoP ::
  (MonadSoP Symbol e p m) =>
  M.Map Symbol (SoP Symbol) ->
  Symbol ->
  m (Maybe (SoP Symbol))
getEquivSoP equivs symb@(Idx (POR nms) ind_sop)
  | S.size nms > 1,
    syms <- map (nm2PORsym ind_sop) $ S.toList nms,
    eq_vs <- mapMaybe (`M.lookup` equivs) syms =
      if not $ null $ filter (== sop_one) eq_vs
        then pure $ Just sop_one
        else -- \^ we found a True term in an OR node => True

          if length eq_vs == length syms
            && all isZero eq_vs
            then pure $ Just $ int2SoP 0
            else -- \^ all are zero
              pure $ M.lookup symb equivs
  where
    nm2PORsym ind arr_nm = Idx (POR (S.singleton arr_nm)) ind
getEquivSoP equivs symb@Idx {} =
  pure $ M.lookup symb equivs
getEquivSoP _ _ =
  pure Nothing

-- getForRangeSoP :: (MonadSoP Symbol Symbol (Property Symbol) m) =>
--   Symbol -> m (Maybe (SoP Symbol))
-- getForRangeSoP (Idx (One vn) idx) = do
--   p <- askForRng (Var vn)
--   case p of
--     Just (For vn' (Predicate i (Rng vn'' bounds)))
--       | vn == vn',
--         vn' == vn'' -> do
--         -- idx has been bounds checked, so Rng holds for idx.
--         let (a,b) = mapProperty (sop2Symbol . rep (mkRep i (sym2SoP (Var k)))) bounds
--         undefined
--     _ -> undefined
-- getForRangeSoP _ =
--   pure Nothing

---------------------------------------------------------------
--- 2. Post Simplification of each (individual) slice sum:  ---
---    2.1. sum x[lb .. ub] => 0     whenever lb  > ub      ---
---    2.2. sum x[lb .. ub] => x[lb] whenever lb == ub      ---
---    2.3. peeling off first/last known elements of a sum  ---
---         ToDo: this case requires checking that slice is ---
---               not empty by FM.                          ---
---------------------------------------------------------------

-- | This is the last stage of SoP simplification, hence
--   it is also used by ther FM solver.
simplifyOneSumAft ::
  (MonadSoP Symbol e Prop m) => SoP Symbol -> m (Bool, SoP Symbol)
simplifyOneSumAft sop = do
  equivs <- getEquivs
  sop' <- elimEmptySums sop
  let (succ1, sop'') = transfSum2Idx sop'
  (succ2, sop''') <- unaryOpOnSumFP (hasPeelableSums equivs) peelSumSymb sop''
  pure (succ1 || succ2, sop''')

-- | This is intended to only by used by the FM solver, i.e.,
--   not for just simplification. It only splits the start/emd
--   indices of the sum, if either array index has a more
--   specialized range (in the range symbol table).
simplifyPeelSumForFM ::
  (MonadSoP Symbol e Prop m) => SoP Symbol -> m (Bool, SoP Symbol)
simplifyPeelSumForFM sop =
  unaryOpOnSumFP (\_ -> True) peelSumOnRanges sop

transfSum2Idx :: SoP Symbol -> (Bool, SoP Symbol)
transfSum2Idx sop
  | tgt_sums <- filter isOneElmSum $ S.toList $ free sop,
    not (null tgt_sums) =
      let subs = M.fromList $ zip tgt_sums $ map sum2Idx tgt_sums
       in (True, substitute subs sop)
  where
    isOneElmSum (Sum _ lb ub) = lb == ub
    isOneElmSum _ = False
    sum2Idx (Sum idxsym lb _) = Idx idxsym lb
    sum2Idx _ = error "Unreachable case reached in transfSum2Idx."
transfSum2Idx sop = (False, sop)

hasPeelableSums :: M.Map Symbol (SoP Symbol) -> (SoP Symbol) -> Bool
hasPeelableSums equivs = (\_ -> True)
  where
    -- any hasPeelableSumSym . S.toList . free
    -- \^ has to make it look inside POR nodes

    hasPeelableSumSym (Sum nm beg end) =
      isJust (M.lookup (Idx nm beg) equivs)
        || isJust (M.lookup (Idx nm end) equivs)
    hasPeelableSumSym _ = False

disjointAllTheWay :: M.Map Symbol (S.Set Prop) -> S.Set VName -> Bool
disjointAllTheWay tab_props nms
  | S.size nms > 1,
    nm <- S.elemAt 0 nms,
    mtmp <- M.lookup (Var nm) tab_props,
    Just nms_disjoint <- hasDisjoint (fromMaybe S.empty mtmp) =
      nms == S.insert nm nms_disjoint
disjointAllTheWay _ _ =
  False

-- | Several cases:
--   Case 1 (SUM):
--     If x DISJOINT (y,z) holds and also
--        ub - lb + 1 >= 0 holds
--     then Sum(x||y||z)[lb,ub] == ub - lb + 1
--   Case 2 (IDX): similar with Case 1 but for an index.
--   Case 3 (SUM):
--     peeling off first/last known elements of a sum
--     (requires non-empty slice)
--   Case 4 (IDX): replaces an index symbol that is found in
--     equiv symtab.
peelSumSymbHelper ::
  (MonadSoP Symbol e Prop m) =>
  M.Map Symbol (S.Set Prop) ->
  Symbol ->
  m (Maybe (SoP Symbol, Symbol))
peelSumSymbHelper tab_props sym@(Idx (POR nms) _idx)
  | disjointAllTheWay tab_props nms = do
      -- Case 2
      pure $ Just (sop_one, sym)
--
peelSumSymbHelper tab_props sym@(Sum (POR nms) beg end)
  | disjointAllTheWay tab_props nms = do
      -- Case 1
      let end_p_1 = end .+. sop_one
      valid_slice <- beg FM.$<=$ end_p_1
      if valid_slice
        then pure $ Just $ (end_p_1 .-. beg, sym)
        else pure Nothing
--
peelSumSymbHelper _ sym@(Sum xs lb ub) = do
  -- Case 3
  non_empty_slice <- lb FM.$<=$ ub
  if not non_empty_slice
    then pure Nothing
    else peelSumSymbEquiv xs lb ub
  where
    -- try to peel of an end of the interval when
    --   the end is in the equivalence table
    peelSumSymbEquiv arr beg end = do
      equivs <- getEquivs
      mfst_el <- getEquivSoP equivs $ Idx arr beg
      mlst_el <- getEquivSoP equivs $ Idx arr end
      case (mfst_el, mlst_el) of
        (Just fst_el, Nothing) -> do
          let new_sum = Sum arr (beg .+. sop_one) end
          pure $ Just (fst_el .+. sym2SoP new_sum, sym)
        (Nothing, Just lst_el) -> do
          let new_sum = Sum arr beg (end .-. sop_one)
          pure $ Just (lst_el .+. sym2SoP new_sum, sym)
        (Just fst_el, Just lst_el) -> do
          let new_sum = Sum arr (beg .+. sop_one) (end .-. sop_one)
          pure $ Just (fst_el .+. lst_el .+. sym2SoP new_sum, sym)
        (Nothing, Nothing) -> pure Nothing
{--
peelSumSymbHelper _ sym@(Sum arr beg end) = do -- Case 3
  equivs <- getEquivs
  non_empty_slice <- beg FM.$<=$ end
  mfst_el <- getEquivSoP equivs $ Idx arr beg
  mlst_el <- getEquivSoP equivs $ Idx arr end
  --  mfst_el = M.lookup (Idx arr beg) equivs
  --  mlst_el = M.lookup (Idx arr end) equivs
  case (non_empty_slice, mfst_el, mlst_el) of
    (False, _, _) ->
      pure Nothing
    (True, Just fst_el, Nothing) -> do
      let new_sum = Sum arr (beg .+. sop_one) end
      pure $ Just (fst_el .+. sym2SoP new_sum, sym)
    (True, Nothing, Just lst_el) -> do
      let new_sum = Sum arr beg (end .-. sop_one)
      pure $ Just (lst_el .+. sym2SoP new_sum, sym)
    (True, Just fst_el, Just lst_el) -> do
      let new_sum = Sum arr (beg .+. sop_one) (end .-. sop_one)
      pure $ Just (fst_el .+. lst_el .+. sym2SoP new_sum, sym)
    (True, Nothing, Nothing) -> pure Nothing
--}
--
peelSumSymbHelper _ sym@(Idx arr idx) = do
  -- Case 4
  equivs <- getEquivs
  m_el <- getEquivSoP equivs $ Idx arr idx
  case m_el of
    Nothing -> pure Nothing
    Just el -> pure $ Just (el, sym)
--
peelSumSymbHelper _ _ =
  pure Nothing

peelSumSymb :: (MonadSoP Symbol e Prop m) => FoldFunTp m
peelSumSymb acc@(Just {}) _ = pure acc
peelSumSymb Nothing (sym, 1) = do
  -- \^ ToDo: extend for any multiplicity >= 1
  tab_props <- getProperties
  peelSumSymbHelper tab_props sym
peelSumSymb Nothing _ = pure Nothing

-- | tries to peel of one or both of the ends of a
--   Sum interval when the end has a more specialized range than the array.
peelSumOnRanges :: (MonadSoP Symbol e Prop m) => FoldFunTp m
peelSumOnRanges acc@(Just {}) _ = pure acc
peelSumOnRanges Nothing (sym@(Sum xs lb ub), 1) = do
  -- \^ ToDo: extend for any multiplicity >= 1
  non_empty_slice <- lb FM.$<=$ ub
  if not non_empty_slice
    then pure Nothing
    else peelSumSymbIneq xs lb ub
  where
    peelSumSymbIneq arr beg end = do
      ranges <- getRanges
      let (beg_sym, end_sym) = (Idx arr beg, Idx arr end)
      case (M.lookup beg_sym ranges, M.lookup end_sym ranges) of
        (Just _, Nothing) -> do
          let new_sum = Sum arr (beg .+. sop_one) end
          pure $ Just (sym2SoP beg_sym .+. sym2SoP new_sum, sym)
        (Nothing, Just _) -> do
          let new_sum = Sum arr beg (end .-. sop_one)
          pure $ Just (sym2SoP end_sym .+. sym2SoP new_sum, sym)
        (Just _, Just _) -> do
          let new_sum = Sum arr (beg .+. sop_one) (end .-. sop_one)
          pure $ Just (sym2SoP beg_sym .+. sym2SoP end_sym .+. sym2SoP new_sum, sym)
        (Nothing, Nothing) ->
          pure Nothing
peelSumOnRanges Nothing _ = pure Nothing

----------------------------------------
--- Common Infrastructure for Unary  ---
--- Simplifications of Sum of Slice  ---
----------------------------------------

elimEmptySums ::
  (MonadSoP Symbol e p m) => SoP Symbol -> m (SoP Symbol)
elimEmptySums sop = do
  sopFromList <$> (filterM predTerm $ sopToList sop)
  where
    emptySumSym (Sum _ lb ub) = lb FM.$>$ ub
    emptySumSym _ = pure False
    predTerm (Term ms, _) = do
      tmps <- mapM (emptySumSym . fst) $ MS.toOccurList ms
      pure $ all not tmps

unaryOpOnSumFP ::
  (MonadSoP Symbol e p m) =>
  (SoP Symbol -> Bool) ->
  FoldFunTp m ->
  SoP Symbol ->
  m (Bool, SoP Symbol)
unaryOpOnSumFP hasOpOnSym opOnSym sop
  | hasOpOnSym sop = do
      res <- unaryOpOnSum opOnSym sop
      case res of
        (False, _) -> pure (False, sop)
        (True, sop') -> do
          -- fix point
          (_, sop'') <- unaryOpOnSumFP hasOpOnSym opOnSym sop'
          pure (True, sop'')
  where

unaryOpOnSumFP _ _ sop = pure (False, sop)

unaryOpOnSum :: (MonadSoP Symbol e p m) => FoldFunTp m -> SoP Symbol -> m (Bool, SoP Symbol)
unaryOpOnSum opOnSym sop = do
  res <- foldM opOnTerm Nothing (M.toList (getTerms sop))
  case res of
    Nothing -> pure (False, sop)
    Just (old_term_sop, new_sop) ->
      pure (True, (sop .-. old_term_sop) .+. new_sop)
  where
    opOnTerm acc@(Just {}) _ = pure acc
    opOnTerm Nothing (t, k) = do
      mres <- foldM opOnSym Nothing $ MS.toOccurList $ getTerm t
      case mres of
        Nothing -> pure Nothing
        Just (sop_sym, sum_sym) -> do
          let ms' = MS.delete sum_sym $ getTerm t
              sop' = sop_sym .*. term2SoP (Term ms') k
          pure $ Just (term2SoP t k, sop')
