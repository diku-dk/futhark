{-# LANGUAGE DataKinds #-}

-- | Functionality for refining the equivalence environment
module Futhark.SoP.RefineEquivs
  ( addEqZeros,
    addEq,
  )
where

import Control.Monad
import Data.Foldable (minimumBy)
import Data.Map.Strict qualified as M
import Data.MultiSet qualified as MS
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.SoP.Convert
import Futhark.SoP.Expression
import Futhark.SoP.FourierMotzkin
import Futhark.SoP.Monad
import Futhark.SoP.SoP
import Futhark.SoP.Util

addEq :: forall u e p m. (ToSoP u e, MonadSoP u e p m) => u -> SoP u -> m ()
addEq sym sop = do
  -- cands <- mkEquivCands (/= sym) $ sop .-. sym2SoP sym
  addLegalCands $ S.singleton $ EquivCand sym sop

-- | Refine the environment with a set of 'PrimExp's with the assertion that @pe = 0@
--   for each 'PrimExp' in the set.
addEqZeros :: forall u e p m. (ToSoP u e, MonadSoP u e p m) => Set (SoP u == 0) -> m (Set (SoP u >= 0))
addEqZeros sops = do
  -- Make equivalence candidates along with any extra constraints.
  (extra_inEqZs :: Set (SoP u >= 0), equiv_cands) <-
    mconcat <$> mapM addEquiv2CandSet (S.toList sops)
  -- Add one-by-one all legal equivalences to the algebraic
  -- environment, i.e., range and equivalence envs are updated
  -- as long as the new substitutions do not introduce cycles.
  addLegalCands equiv_cands
  -- Return the newly generated constraints.
  pure extra_inEqZs

-- | An equivalence candidate; a candidate @'EquivCand' sym sop@ means
--   @sym = sop@.
data EquivCand u = EquivCand
  { equivCandSym :: u,
    equivCandSoP :: SoP u
  }
  deriving (Eq, Show, Ord)

instance (Ord u) => Free u (EquivCand u) where
  free = free . equivCandSoP

instance (Ord u) => Substitute u (SoP u) (EquivCand u) where
  substitute subst (EquivCand sym sop) =
    EquivCand sym $ substitute subst sop

-- | A candidate for the equivalence env is found when:
--
--   (1) A term of the SoP has one symbol (here, named @sym@).
--   (2) @sym@'s value factor is @1@ or @-1@.
--   (3) @sym@ does not appear in the other terms of the 'SoP'.
--   (4) @sym@ is not already present in the equivalence environment.
--
--   ToDo: try to give common factor first, e.g.,
--         nx - nbq - n = 0 => n*(x-bq-1) = 0 => x = bq+1,
--         if we can prove that n != 0
mkEquivCands :: (MonadSoP u e p m) => (u -> Bool) -> SoP u -> m (Set (EquivCand u))
mkEquivCands p sop =
  pure (getTerms sop)
    >>= M.foldrWithKey mkEquivCand (pure mempty)
  where
    -- getTerms <$> substEquivs sop
    --  >>= M.foldrWithKey mkEquivCand (pure mempty)

    mkEquivCand (Term term) v mcands
      | abs v == 1,
        [sym] <- MS.toList term,
        sop' <- deleteTerm term sop,
        sym `notElem` free sop',
        p sym = do
          msop <- lookupSoP sym
          case msop of
            Nothing ->
              S.insert (EquivCand sym (scaleSoP (-v) sop')) <$> mcands
            Just {} ->
              mcands
      | otherwise = mcands

-- | Algebraic manipulation of 'EquivCand's. Potentially yields
--   additional constraints (inequalities).
--
--   Currently supports two cases:
--
--   1. Refinement of the modulo expression:
--      Assume the equivalence @sym = sop@, where @sym@ is
--      bound in the untranslatable environment as @sym -> pe1 % pe2@.
--      This means `pe1 % pe2 = sop` and we can do the
--      following re-writting:
--
--        (1) Check @pe2 >= 0@.
--        (2) Check the sum-of-products representation of @pe1@
--            is a single symbol, which is not in equivalence environment.
--
--      If (1) and (2) hold then:
--
--        * We rewrite: @pe1 = pe2 * q + sop@.
--        * Possibly add the constraints @0 <= sop <= pe2 - 1@.
--
--   2: TODO: try to give common factors and get simpler.
refineEquivCand :: forall u e p m. (ToSoP u e, MonadSoP u e p m) => EquivCand u -> m (Set (SoP u >= 0), EquivCand u)
-- refineEquivCand cand@(EquivCand sym sop)
--  | justPositive sop = pure (S.singleton $ sym2SoP sym, cand)
refineEquivCand cand@(EquivCand sym sop) = do
  mpe <- lookupUntransSym sym
  case mpe of
    Just pe
      | Just (pe1, pe2) <- moduloIsh pe -> do
          (f1, sop1) <- toSoPNum pe1
          (f2, sop2) <- toSoPNum pe2
          is_pos <- fmSolveGEq0 sop2
          case (f1, f2, justSym sop1, is_pos) of
            (1, 1, Just sym1, True) -> do
              msop <- lookupSoP sym1
              case msop of
                Just {} -> pure (mempty, cand)
                Nothing -> do
                  q <- mkNameM
                  let div_pe = divInsteadOfMod pe
                      q_sop = sym2SoP q
                      new_cand = EquivCand sym1 $ sop .+. q_sop .*. sop2
                      new_ineq = sop2 .-. (sop .+. int2SoP 1)
                      pe_ineq = S.fromList [new_ineq, sop]
                  addUntrans q div_pe
                  pure (pe_ineq, new_cand)
            _ -> pure (mempty, cand)
    _ -> do
      if (justPositive sop)
        then pure (S.singleton $ sym2SoP sym, cand)
        else pure (mempty, cand)

-- | Takes a 'PrimExp' @pe@ with the property that @pe = 0@ and
--   returns two sets @'addEquiv2CandSet' pe = (ineqs,cand)@:
--
--   * @ineqs@: a set of extra inequality constraints generated during the
--     creation/refinement of the mapping.
--   * @cands@: set of equivalence candidates.
addEquiv2CandSet ::
  (ToSoP u e, MonadSoP u e p m) =>
  SoP u == 0 ->
  m (Set (SoP u >= 0), Set (EquivCand u))
addEquiv2CandSet sop = do
  cands <- mkEquivCands (const True) sop
  (ineqss, cands') <- mapAndUnzipM refineEquivCand $ S.toList cands
  pure (mconcat ineqss, S.fromList cands')

-- | Add legal equivalence candidates to the environment.
addLegalCands :: (MonadSoP u e p m) => Set (EquivCand u) -> m ()
addLegalCands cand_set
  | S.null cand_set = pure ()
addLegalCands cand_set = do
  rs <- getRanges
  eqs <- getEquivs
  let -- Chose the candidate @sym -> sop@ whose @sop@ has
      -- the smallest number of symbols not present in the environment.
      env_syms = M.keysSet eqs <> M.keysSet rs
      cand = minimumBy (scoreCand env_syms) cand_set
  -- Check whether target substitution does not create cycles
  -- in the equivalence and range environments.
  if not $ validCand rs eqs cand
    then addLegalCands $ S.delete cand cand_set
    else do
      -- Apply substitution to equivalence and range envs
      -- and add the new binding to the equivalence env.
      modifyEnv $ \env ->
        env
          { equivs =
              M.insert (equivCandSym cand) (equivCandSoP cand) $
                M.map (subCand cand) eqs,
            ranges = M.map (subCand cand) rs
          }
      addLegalCands $ subCand cand $ S.delete cand cand_set
  where
    subCand (EquivCand sym sop) = substituteOne (sym, sop)
    scoreCand env_syms cand1 cand2 =
      let score cand = length $ free (equivCandSoP cand) S.\\ env_syms
       in score cand1 `compare` score cand2
    validCand rs eqs cand =
      not $
        -- Check if a candidate is already present in the
        -- equivalence environment.
        equivCandSym cand
          `elem` M.keysSet eqs
          -- Detect if an equivalence candidate would introduce
          -- a cycle into the equivalence environment.
          && any hasEquivCycle (M.toList eqs)
          -- Detect if an equivalence candidate would introduce
          -- a cycle into the range environment.
          && any hasRangeCycle (M.toList rs)
      where
        -- Since the equivalence environment contains the fully
        -- substituted bindings (accounting for predecessor
        -- substitutions), we do not need to (explicitly) compute the
        -- transititve closures.
        hasEquivCycle (sym, sop) =
          (sym `elem` free cand)
            && (equivCandSym cand `elem` free sop)
        hasRangeCycle (sym, range) =
          (sym `elem` transClosInRanges rs (free cand))
            && (equivCandSym cand `elem` free range)
