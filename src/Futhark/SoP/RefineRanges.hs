{-# LANGUAGE DataKinds #-}

-- | Functionality for processing the range environment
module Futhark.SoP.RefineRanges
  ( addIneqZeros,
  )
where

import Control.Monad (filterM, forM)
import Data.Map.Strict qualified as M
import Data.MultiSet qualified as MS
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.SoP.FourierMotzkin
import Futhark.SoP.Monad
import Futhark.SoP.SoP
import Futhark.SoP.Util

-- | Refine the environment with a set of 'PrimExp's with the assertion that @pe >= 0@
--   for each 'PrimExp' in the set.
addIneqZeros :: forall u e p m. (MonadSoP u e p m) => Set (SoP u >= 0) -> m ()
addIneqZeros sops = do
  ineq_cands <-
    mconcat
      <$> mapM mkRangeCands (S.toList sops)
  addRangeCands ineq_cands

-- | A range candidate; a candidate @'RangeCand' v sym sop@ means
--
--   > v*sym + sop >= 0
data RangeCand u = RangeCand
  { rangeCandScale :: Integer,
    rangeCandSym :: u,
    rangeCandSoP :: SoP u
  }
  deriving (Eq, Show, Ord)

-- | Make range candidates from a 'SoP' from its 'Term's.  A candidate
--   'Term' for the range env is found when:
--
--   1. It consists of a single symbol, @sym@.
--   2. @sym@ does not appear in the other 'Term's of the 'SoP'.
--
--   TODOs: try to give common factor first, e.g.,
--          @nx - nbq - n = 0@ is equivalent to
--          @n*(x-bq-1) >= 0@, hence, if we can prove
--          that @n >= 0@ we can derive @x >= bq+1@.
mkRangeCands :: (MonadSoP u e p m) => (SoP u >= 0) -> m (Set (RangeCand u))
mkRangeCands sop = do
  -- sop' <- substEquivs sop
  let sop' = sop
  let singleSymCands = mkSingleSymCands sop'
  factorCands <- factorCandsM sop'
  pure $ singleSymCands <> factorCands
  where
    factorCandsM sop' =
      mconcat
        <$> forM
          (sopFactors sop')
          ( \(rem, term) -> do
              ifM
                (term2SoP term 1 $>=$ zeroSoP)
                (pure $ mkSingleSymCands rem)
                (pure mempty)
          )
    mkSingleSymCands sop' =
      M.foldrWithKey mkRangeCand mempty $ getTerms $ sop'
    mkRangeCand (Term term) v cands
      | [sym] <- MS.toList term,
        sop' <- deleteTerm term sop,
        sym `notElem` free sop' =
          S.insert (RangeCand v sym sop') cands
      | otherwise = cands

-- | Refines a range in the range environment from a range
--   canditate.
--
--   @'refineRangeInEnv' (j, sym, sop)@ refines the existing range of
--   the symbol @sym@
--
--   > max{lbs} <= k*sym <= min{ubs}
--
--   by computing the 'lcm' of @j@ and @k@ to obtain the bounds
--
--   (1) @max{k_z*lbs} <= z*sym <= min{k_z*ubs}@
--   (2) @z*sym + j_z*sop <= 0@
--
--   where @z = 'lcm' k j@. If (2) refines (1) (i.e., it tightens the
--   upper or lower bounds on @sym@), it's merged with (1) and any
--   bounds that are looser than the bound introduced by (2) are
--   removed. If @j < 0@ (@j >= 0@), the upper (lower) bound on @sym@
--   may be tightened.
--
--   Returns a set of new range canditates: if @j < 0@ (@j >= 0@)
--   these are @lbs' <= -j_z * sop@ (@j_z * sop <= ubs'@) where @lbs'@
--   (@ubs'@) are the refined bounds from the previous step.
refineRangeInEnv ::
  (MonadSoP u e p m) =>
  RangeCand u ->
  m (Set (RangeCand u))
refineRangeInEnv (RangeCand j sym sop) = do
  Range lbs k ubs <- lookupRange sym
  let z = lcm k j
      j_z = z `div` j
      k_z = z `div` k
      lbs' = S.map (scaleSoP k_z) lbs
      ubs' = S.map (scaleSoP k_z) ubs
      sop' = (-j_z) `scaleSoP` sop
  if j < 0
    then do
      -- reject: ∃b.sop' >= b?
      -- remove: only keep b with b < new_bound = !(new_bound <= b)
      ubs'' <- mergeBound (sop' $>=$) ($<=$ sop') ubs' sop'
      addRange sym $ Range lbs' z ubs''
      -- New candidates: lbs <= sop' --> sop' - lbs >= 0
      mconcat <$> mapM (mkRangeCands . (sop' .-.)) (S.toList lbs')
    else do
      -- reject: ∃b.new_bound <= b?
      -- remove: only keep b with new_bound < b = !(new_bound >= b)
      lbs'' <- mergeBound (sop' $<=$) ($>=$ sop') lbs' sop'
      addRange sym $ Range lbs'' z ubs'
      -- New candidates: sop' <= ubs --> ubs - sop' >= 0
      mconcat <$> mapM (mkRangeCands . (.-. sop')) (S.toList ubs')
  where
    mergeBound reject remove bs sop' =
      ifM
        (anyM reject bs)
        (pure bs)
        (S.insert sop' <$> (S.fromList <$> filterM (fmap not . remove) (S.toList bs)))

-- | Candidate ranking. @'SymNotBound' > 'CompletesRange' > 'Default'@.
--
--   * 'SymNotBound': candidate symbol doesn't appear in the range environment.
--   * 'CompetesRange': candidate completes the range of a symbol in the range environment
--                      with a partial range.
--   * 'Default': all other candidates.
data CandRank
  = Default
  | CompletesRange
  | SymNotBound
  deriving (Ord, Eq)

addRangeCands :: (MonadSoP u e p m) => Set (RangeCand u) -> m ()
addRangeCands cand_set
  | S.null cand_set = pure ()
addRangeCands cand_set = do
  rs <- getRanges
  let cands = S.toList cand_set
      -- 1. Compute the transitive closure of the 'SoP' of
      --    each candidate through the range environment.
      tcs = map (transClosInRanges rs . free . rangeCandSoP) cands
      -- 2. Filter out the candidates that introduce cycles
      --    through the range environment. A cycle appears iff
      --    @sym@ appears in the transitive closure of the
      --    symbols appearing in its ranges.
      cands_tcs = filter (not . uncurry hasCycle) $ zip cands tcs
      -- 3. Choose the candidates whose transitive closure through the
      --    range env have the lowest number of free symbols (symbols
      --    which do not belong to the keys f the range env.)
      cands' = fst $ foldr (compareNumFreeInRangeEnv rs) (mempty, maxBound) cands_tcs
      -- 4. Rank the candidates.
      cands'' = fst $ foldr (compareRank rs) (mempty, Default) cands'
  case S.toList cands'' of
    [] -> pure ()
    cand : _ -> do
      -- Incorporate the constraints imposed by the top-scoring
      -- candidate into the range environment and continue,
      -- adding any newly generated candidates into the candidate
      -- set.
      new_cands <- refineRangeInEnv cand
      addRangeCands $
        S.delete cand $
          new_cands <> S.fromList (map fst cands_tcs)
  where
    -- A cycle appears if and only if @sym@ is in its
    -- transitive closure. Proof: Suppose a cycle appears
    -- via some @x@. Since the range environment is
    -- (inductively) assumed to be cycle-free, this means
    -- that the range of @sym@ depends on @x@
    -- and the range of @x@ depends on @sym@. It follows that
    -- @sym@ will be in its transitive closure.
    hasCycle cand tc =
      rangeCandSym cand `S.member` tc
    compareNumFreeInRangeEnv rs (cand, tc) (acc, n_acc) =
      case n_acc `compare` n_free of
        LT -> (acc, n_acc)
        EQ -> (S.insert cand acc, n_acc)
        GT -> (S.singleton cand, n_free)
      where
        n_free = length (tc S.\\ M.keysSet rs)
    rankCand rs (RangeCand k sym _)
      | sym `M.notMember` rs = SymNotBound
      | completesRange k (rs M.! sym) = CompletesRange
      | otherwise = Default
    completesRange v (Range lbs _ ubs) =
      (v < 0 && not (S.null lbs) && S.null ubs)
        || (v > 0 && S.null lbs && not (S.null ubs))
    compareRank rs cand (acc, acc_rank)
      | cand_rank > acc_rank = (S.singleton cand, cand_rank)
      | cand_rank == acc_rank = (S.insert cand acc, acc_rank)
      | otherwise = (acc, acc_rank)
      where
        cand_rank = rankCand rs cand
