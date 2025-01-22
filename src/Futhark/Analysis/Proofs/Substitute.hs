-- Index function substitution.
module Futhark.Analysis.Proofs.Substitute ((@)) where

import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Set qualified as S
import Debug.Trace (trace, traceM)
import Futhark.Analysis.Proofs.AlgebraBridge (simplify)
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, repDomain)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Traversals (ASTFolder (..), ASTMapper (..), astFold, astMap, identityMapper)
import Futhark.Analysis.Proofs.Unify (Replaceable (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), fv, renameSame)
import Futhark.Analysis.Proofs.Util (prettyBinding')
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP)
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

-- Substitute applications of f into g.
-- 'g @ (f_name, f)' substitutes name 'f_name' for indexfn 'f' in indexfn 'g'.
(@) :: IndexFn -> (VName, IndexFn) -> IndexFnM IndexFn
g @ (vn, f) = tracer (vn, f) g =<< (vn, f) `substituteInto` g

substituteInto :: (VName, IndexFn) -> IndexFn -> IndexFnM IndexFn
substituteInto (f_name, _) dest_fn
  | f_name `S.notMember` fv dest_fn =
      pure dest_fn
substituteInto (f_name, src_fn) dest_fn = do
  k <- newVName "variables after this are quantifiers"
  (f, g) <- renameSame src_fn dest_fn

  -- If f has multiple cases, we would not know which case to substitute
  -- into quantified symbols (e.g., Sum j a b f(e(j))).
  let legalArg e args =
        let notQuantifier vn = vn < k || Just vn == catVar (iterator g)
         in (hasSingleCase f || all (all notQuantifier . fv) args)
              || warning
        where
          warning =
            flip trace False $
              warningString $
                "Warning: Unable to substitute "
                  <> prettyString e
                  <> " in\n"
                  <> prettyString dest_fn
                  <> "\nfor\n"
                  <> prettyString src_fn

  app <- getApply legalArg g
  case app of
    Just apply -> do
      h <- substituteOnce f g apply
      (f_name, f) `substituteInto` h
    Nothing ->
      -- When converting expressions a function may be substituted without arguments.
      substituteOnce f g (Var f_name, [])
  where
    getApply argCheck = astFold (ASTFolder {foldOnSymbol = getApply_ argCheck}) Nothing

    getApply_ argCheck Nothing e@(Apply (Var vn) args)
      | vn == f_name,
        argCheck e args =
          pure $ Just (e, args)
    getApply_ argCheck Nothing e@(Idx (Var vn) arg)
      | vn == f_name,
        argCheck e [arg] =
          pure $ Just (e, [arg])
    getApply_ _ acc _ = pure acc

    hasSingleCase = isJust . justSingleCase

substituteOnce :: IndexFn -> IndexFn -> (Symbol, [SoP Symbol]) -> IndexFnM IndexFn
substituteOnce f g_non_repped (f_apply, args) = do
  vn <- newVName ("<" <> prettyString f_apply <> ">")
  g <- repApply vn g_non_repped
  it <- mergeIterators (iterator f) (subIterator vn $ iterator g)
  cs <- simplify $ cases $ do
    (p_f, v_f) <- casesToList (body f)
    (p_g, v_g) <- casesToList (body g)
    let s = mkRep vn (rep f_arg v_f)
    pure (sop2Symbol (rep s p_g) :&& sop2Symbol (rep f_arg p_f), rep s v_g)
  pure $
    IndexFn
      { iterator = it,
        body = cs
      }
  where
    f_arg :: Replacement Symbol =
      case iterator f of
        Empty ->
          case args of
            [] -> mempty
            _ -> error "Arguments supplied to scalar index function."
        Forall i _ ->
          case (iterator g_non_repped, args) of
            (Empty, []) -> mempty
            (Forall j _, []) -> mkRep i (Var j)
            (_, [arg]) -> mkRep i arg
            (_, _) -> error "Multi-dim not implemented yet."

    repApply vn =
      astMap
        ( identityMapper
            { mapOnSymbol = \e ->
                pure $ if e == f_apply then Var vn else e
            }
        )

    subIterator _ Empty = Empty
    subIterator vn (Forall j dg)
      | vn `notElem` fv dg =
          Forall j dg
    subIterator vn (Forall j dg)
      | Just v_f <- justSingleCase f =
          Forall j $ repDomain (mkRep vn (rep f_arg v_f)) dg
    subIterator _ _ =
      error "Only single-case index functions may substitute into domain."

-- Preserve Cat knowledge.
mergeIterators :: Iterator -> Iterator -> IndexFnM Iterator
mergeIterators (Forall i df@(Cat {})) (Forall j dg@(Iota {})) = do
  assertSameRange df dg
  pure (Forall j $ repDomain (mkRep i $ Var j) df)
mergeIterators _ g_iter = pure g_iter

assertSameRange :: Domain -> Domain -> IndexFnM ()
assertSameRange df dg = do
  start_f <- rewrite (domainStart df)
  start_g <- rewrite (domainStart dg)
  end_f <- rewrite (domainEnd df)
  end_g <- rewrite (domainEnd dg)
  eq_start :: Maybe (Substitution Symbol) <- unify start_f start_g
  eq_end :: Maybe (Substitution Symbol) <- unify end_f end_g
  unless (isJust eq_start && isJust eq_end) $
    error "assertSameRange: inequal ranges"
