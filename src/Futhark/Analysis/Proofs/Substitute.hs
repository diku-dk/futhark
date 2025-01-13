-- Index function substitution.
module Futhark.Analysis.Proofs.Substitute ((@)) where

import Control.Monad (forM_, unless, when)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.AlgebraBridge (algebraContext, simplify, toAlgebra)
import Futhark.Analysis.Proofs.AlgebraBridge.Util (addRelSymbol)
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, repDomain)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Query (Query (CaseCheck), askQ, foreachCase, isYes)
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), astMap, identityMapper)
import Futhark.Analysis.Proofs.Unify (Replaceable (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), fv, renameM, renameSame)
import Futhark.Analysis.Proofs.Util (prettyBinding')
import Futhark.MonadFreshNames (newName, newVName)
import Futhark.SoP.SoP (SoP, justSym, sym2SoP)
import Futhark.Util.Pretty (prettyString)
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
  (f', g', app_reps) <- mkApplicationReps (f_name, f) g
  iter <- mergeIterators f' $ inlineIterator app_reps g'
  cs <- inlineCases app_reps f' g'
  IndexFn iter <$> simplify cs
  where
    inlineCases reps f' g' =
      renameM $ cases $ do
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

    mergeIterators (IndexFn Empty _) g_iter = pure g_iter
    mergeIterators (IndexFn (Forall _ df) cs) Empty =
      case df of
        Iota {} -> pure Empty
        Cat k _ _
          | k `elem` fv cs ->
              error $ "Might capture " <> prettyString k
          | otherwise ->
              pure Empty
    mergeIterators (IndexFn (Forall i df) _) (Forall j dg) = do
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
mkApplicationReps (f_name, f) g = rollbackAlgEnv $ do
  k <- newVName "variables after this are quantifiers"
  (f', g') <- renameSame f g
  let notQuantifier vn = vn < k || Just vn == catVar (iterator g')
  -- If f has multiple cases, we would not know which case to substitute in Sum j a b f(e(j)).
  let nonCapturingArg arg = hasSingleCase f || all notQuantifier (fv arg)
  -- Collect and replace applications f(e(j)) in g by fresh variables.
  -- Returns, for example, (g{f(i) |-> f_i}, [{f_i |-> f(i)}]).
  -- NOTE the domain of g is also traversed.
  (g_for_inlining, apps) <- forget $ do
    g'' <- repAppByFreshName nonCapturingArg =<< handleQuantifiers g'
    (g'',) . map mkApp . M.assocs <$> getMem
  -- If there are no f(e(j)), handle direct references to f without any args.
  let reps = case apps of
        [] ->
          let i_rep = case (iterator f', iterator g') of
                (Forall i _, Forall j _) -> mkRep i (Var j)
                _ -> mempty
           in [(f_name, i_rep)]
        _ -> apps
  -- NOTE Not using g_for_inlining in bounds checking because its iterator
  -- is potentially bogus before inlining f.
  checkIndexingWithinBounds
    f_name
    f'
    g'
    [arg | (_, app_rep) <- reps, (_i, arg) <- M.toList app_rep]
  pure (f', g_for_inlining, reps)
  where
    hasSingleCase = isJust . justSingleCase

    mkApp :: (Symbol, VName) -> (VName, Replacement Symbol)
    mkApp (Idx _ idx, v) = (v, toRep idx)
    mkApp _ = error "Impossible"

    toRep idx = case iterator f of
      Forall i _ -> mkRep i idx
      Empty -> error "Indexing into a scalar function"

    -- We need to add ranges on quantifiers in order to do bounds-checking.
    -- For example, if g = | True ⇒  ∑j∈(0 .. -1 + m) xs[j].
    handleQuantifiers = astMap m
      where
        m = ASTMapper {mapOnSymbol = handleQuant, mapOnSoP = pure}
        handleQuant (Sum j lb ub x) = do
          fresh_j <- newName j
          let j' = sym2SoP (Var fresh_j)
          addRelSymbol (lb :<= j' :&& j' :<= ub)
          let x' :: Symbol = fromJust . justSym $ rep (mkRep j j') x
          pure (Sum fresh_j lb ub x')
        handleQuant x = pure x

    repAppByFreshName nonCapturingArg = astMap m
      where
        m = identityMapper {mapOnSymbol = repApp}

        repApp (Apply (Var x) [arg])
          | x == f_name,
            nonCapturingArg arg =
              Var <$> remember (Idx (Var x) arg)
        repApp (Idx (Var x) arg)
          | x == f_name,
            nonCapturingArg arg =
              Var <$> remember (Idx (Var x) arg)
        repApp (Apply (Var x) [_])
          | x == f_name = error "Capturing variable(s)"
        repApp (Idx (Var x) _)
          | x == f_name = error "Capturing variable(s)"
        repApp sym = pure sym

-- When applying f to args inside g, is f defined at args?
checkIndexingWithinBounds :: VName -> IndexFn -> IndexFn -> [SoP Symbol] -> IndexFnM ()
checkIndexingWithinBounds _ _ _ [] = pure ()
checkIndexingWithinBounds f_name (IndexFn Empty _) _ args =
  unless (null args) $ error $ "Indexing into scalar " <> prettyString f_name
checkIndexingWithinBounds f_name f@(IndexFn (Forall _ dom) _) g args = algebraContext g $ do
  forM_ args $ \arg -> do
    doCheck (domainStart dom :<= arg) arg
    doCheck (arg :<= domainEnd dom) arg
  where
    fIsIn (p, q) = f_name `S.member` (fv p `S.union` fv q)

    doCheck bound arg =
      foreachCase g $ \i ->
        when (fIsIn $ getCase i $ body g) $ do
          c <- askQ (CaseCheck (const bound)) g i
          when (isYes c) $ do
            debugPrettyM "PASSED" bound
          unless (isYes c) $ do
            debugPrettyM "CASE " i
            debugM $
              "Failed bounds-checking:"
                <> "\nf:"
                <> prettyString f
                <> "\ng: "
                <> prettyString g
                <> "\nargs: "
                <> prettyString args
            alg_end <- toAlgebra (domainEnd dom)
            error $
              "Unsafe indexing: "
                <> prettyString (Idx (Var f_name) arg)
                <> " (failed to show: "
                <> prettyString bound
                <> ")."
                <> " (in algebra: "
                <> prettyString alg_end
                <> ")."

-- XXX when to rewrite recurrences as sums?
-- When substitituting a recurrent index fn into some other index fn.
-- When querying the solver about an index fn (won't actually alter the representation of it).
-- ==> That is, NOT unless we have to.
--
-- XXX could we rewrite recurrences in terms of the index fn being substituted into?
