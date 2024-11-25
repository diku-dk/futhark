-- Index function substitution.
module Futhark.Analysis.Proofs.Substitute ((@)) where

import Control.Monad (unless)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.AlgebraBridge (simplify)
import Futhark.Analysis.Proofs.AlgebraBridge.Translate (rollbackAlgEnv)
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, repDomain, repIndexFn)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), astMap, identityMapper)
import Futhark.Analysis.Proofs.Unify (Replaceable (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), fv, renameSame)
import Futhark.Analysis.Proofs.Util (prettyBinding')
import Futhark.MonadFreshNames (newName, newVName)
import Futhark.SoP.Monad (UntransEnv (dir), getUntrans, lookupUntransPE)
import Futhark.SoP.SoP (sym2SoP)
import Language.Futhark (VName)

tracer :: (VName, IndexFn) -> IndexFn -> IndexFn -> IndexFnM IndexFn
tracer (vn, f@(IndexFn (Forall {}) _)) g@(IndexFn (Forall {}) _) res = do
  whenDebug $
    traceM $
      "🎭  "
        <> prettyBinding' vn f
        <> prettyBinding' ("\n    into _" :: String) g
        <> prettyBinding' ("\n          " :: String) res
  pure res
tracer _ _ res = pure res

-- Inline applications of f in g.
-- 'g @ (f_name, f)' substitutes name 'f_name' for indexfn 'f' in indexfn 'g'.
(@) :: IndexFn -> (VName, IndexFn) -> IndexFnM IndexFn
g @ (vn, f) = tracer (vn, f) g =<< inline (vn, f) g

inline :: (VName, IndexFn) -> IndexFn -> IndexFnM IndexFn
inline (f_name, f) g = do
  (f', g', app_reps) <- mkApplicationReps (f_name, f) g --  =<< renameIter g
  iter <- mergeIterators (iterator f') $ inlineIterator app_reps g'
  cs <- inlineCases app_reps f' g'
  pure (IndexFn iter cs)
  where
    inlineCases reps f' g' =
      simplify . cases $ do
        (f_cond, f_val) <- casesToList (body f')
        (g_cond, g_val) <- casesToList (body g')
        pure $
          foldl
            ( \(c, v) (app_name, arg) ->
                let f_cond' = sop2Symbol $ rep arg f_cond
                    s = mkRep app_name (rep arg f_val)
                 in (sop2Symbol (rep s c) :&& f_cond', rep s v)
            )
            (g_cond, g_val)
            reps

    inlineIterator _ (IndexFn Empty _) = Empty
    inlineIterator reps (IndexFn (Forall j dg) _)
      | all ((`notElem` fv dg) . fst) reps =
          Forall j dg
    inlineIterator reps (IndexFn (Forall j dg) _)
      | Just f_val <- justSingleCase f =
          Forall j $
            foldl
              ( \dom (app_name, arg) ->
                  repDomain (mkRep app_name (rep arg f_val)) dom
              )
              dg
              reps
    inlineIterator _ _ =
      error "Only single-case index functions may substitute into domain"

mergeIterators :: Iterator -> Iterator -> IndexFnM Iterator
mergeIterators Empty g_iter = pure g_iter
mergeIterators (Forall _ df) Empty =
  case df of
    Iota {} -> pure Empty
    Cat {} -> error "Might capture k?"
mergeIterators (Forall i df) (Forall j dg) = do
  assertEquivDomains df dg
  case (df, dg) of
    (Iota {}, Iota {}) -> do
      pure (Forall j dg)
    (Cat {}, Cat {}) -> do
      pure (Forall j dg)
    (Cat {}, Iota {}) -> do
      pure (Forall j $ repDomain (mkRep i $ Var j) df)
    (Iota {}, Cat {}) -> do
      pure (Forall j dg)

equivDomains :: Domain -> Domain -> IndexFnM Bool
equivDomains df@(Cat {}) dg@(Cat {}) = do
  isJust <$> (unify df dg :: IndexFnM (Maybe (Substitution Symbol)))
equivDomains df@(Iota {}) dg@(Cat _ m _) = do
  aligns_with_whole <- sameRange df dg
  aligns_with_k <- sameRange df (Iota m)
  pure $ aligns_with_whole || aligns_with_k
equivDomains df dg = sameRange df dg

sameRange :: Domain -> Domain -> IndexFnM Bool
sameRange df dg = do
  start_f <- rewrite (domainStart df)
  start_g <- rewrite (domainStart dg)
  end_f <- rewrite (domainEnd df)
  end_g <- rewrite (domainEnd dg)
  eq_start :: Maybe (Substitution Symbol) <- unify start_f start_g
  eq_end :: Maybe (Substitution Symbol) <- unify end_f end_g
  pure $ isJust eq_start && isJust eq_end

assertEquivDomains :: Domain -> Domain -> IndexFnM ()
assertEquivDomains dom_f dom_g =
  equivDomains dom_f dom_g >>= flip unless (error "assertSameRange: inequal ranges")

-- Given functions (f = \i -> ...) and (g = \j -> ...), returns:
-- 1. renamed f
-- 2. renamed g with each application f(e(j)) replaced by a (fresh) variable
--    unique to the argument e(j), and
-- 3. a list associating the names of those variables to their arguments:
--      [(f, {}), (f_1, {i |-> e_1(j)}), (f_2, {i |-> e_2(j)}), ...]
--    where the first element handles the case where f is used without
--    any argument, for instance, when f is a scalar.
mkApplicationReps :: (VName, IndexFn) -> IndexFn -> IndexFnM (IndexFn, IndexFn, [(VName, Replacement Symbol)])
mkApplicationReps (f_name, f) g = do
  k <- newVName "variables after this are quantifiers"
  let notQuantifier = (< k)
  (f', g') <- renameSame f g
  let legalName v =
        notQuantifier v
          || Just v == getCatIteratorVariable g'
          || hasSingleCase f'
  -- NOTE g can reference f in its domain, hence it is not enough to simply
  -- analyse the cases.
  -- Handle references `Var f` without argument.
  let var_rep = case (iterator f', iterator g') of
        (Forall i _, Forall j _) -> mkRep i (Var j) :: Replacement Symbol
        _ -> mempty
  -- Applications f(e(j)).
  -- TODO Using algenv for unintended purposes here; add new field to VEnv?
  rollbackAlgEnv $ do
    clearAlgEnv
    g'' <- astMap (identityMapper {mapOnSymbol = repAppByName legalName}) g'
    assocs <- map strip . M.assocs . dir <$> getUntrans
    pure (f', g'', (f_name, var_rep) : assocs)
  where
    hasSingleCase = isJust . justSingleCase

    strip (Algebra.Var vn, Idx _ idx) = (vn, toRep idx)
    strip _ = error "Impossible"

    toRep idx = case iterator f of
      Forall i _ -> mkRep i idx :: Replacement Symbol
      Empty -> error "Indexing into a scalar function"

    -- Replace applications of f by (fresh) names.
    -- We disallow substituting f into sums, if f has more than one case
    -- (it would be unclear what case value to substitute). This is enforced
    -- by checking for captured quantifiers in the function argument,
    -- for example, Sum_j (f[i] + j) is allowed, but Sum_j (f[j]) is not.
    -- (Unless f only has one case.)
    repAppByName legalName sym = case sym of
      Apply (Var x) [idx]
        | x == f_name,
          legalIdx idx ->
            repByName (Idx (Var x) idx)
      Idx (Var x) idx
        | x == f_name,
          legalIdx idx ->
            repByName (Idx (Var x) idx)
      Apply (Var x) [_]
        | x == f_name -> error "Capturing variables"
      Idx (Var x) _
        | x == f_name -> error "Capturing variables"
      _ -> pure sym
      where
        legalIdx = all legalName . fv

    repByName app = do
      f_at_idx <- Algebra.getVName <$> lookupUntransPE app
      pure (Var f_at_idx)

renameIter fn@(IndexFn (Forall i _) _) = do
  j <- sym2SoP . Var <$> newName i
  pure $ repIndexFn (mkRep i j) fn
renameIter fn = pure fn

-- XXX when to rewrite recurrences as sums?
-- When substitituting a recurrent index fn into some other index fn.
-- When querying the solver about an index fn (won't actually alter the representation of it).
-- ==> That is, NOT unless we have to.
--
-- XXX could we rewrite recurrences in terms of the index fn being substituted into?
