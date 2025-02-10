-- Index function substitution.
module Futhark.Analysis.Proofs.Substitute ((@)) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Map qualified as M
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as S
import Debug.Trace (trace, traceM)
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, algebraContext, simplify, ($<), ($<=), ($>=))
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd, intervalStart, repDomain, repIndexFn)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Query (Answer (..), Query (CaseCheck), allCases, askQ, isYes)
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Traversals (ASTFolder (..), ASTMapper (..), astFold, astMap, identityMapper)
import Futhark.Analysis.Proofs.Unify (Replaceable (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), fv, renameSame)
import Futhark.Analysis.Proofs.Util (prettyBinding')
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, sym2SoP)
import Futhark.Util.Pretty (Pretty, prettyString)
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

-- Assumes f and g_non_repped have been renamed using renameSame.
substituteOnce :: IndexFn -> IndexFn -> (Symbol, [SoP Symbol]) -> IndexFnM IndexFn
substituteOnce f g_non_repped (f_apply, args) = do
  vn <- newVName ("<" <> prettyString f_apply <> ">")
  g <- repApply vn g_non_repped

  new_body <- simplify $ cases $ do
    (p_f, v_f) <- casesToList (body f)
    (p_g, v_g) <- casesToList (body g)
    let s = mkRep vn (rep f_arg v_f)
    pure (sop2Symbol (rep s p_g) :&& sop2Symbol (rep f_arg p_f), rep s v_g)

  let g_iter = subIterator vn (iterator g)
  same_range <- isSameRange (iterator f) g_iter
  let new_iter = case (iterator f, g_iter) of
        -- Propagate domain of f into g.
        (Forall i df@(Cat {}), Forall j (Iota {}))
          | same_range ->
              Forall j $ repDomain (mkRep i $ Var j) df
        _ -> g_iter

  -- If f has a segmented structure (Cat k _ _), make sure that
  -- k is not captured in the body of g after substitution.
  case iterator f of
    Forall i df@(Cat k _ _)
      | k `S.member` fv new_body -> do
          let t1 =
                if same_range
                  then do
                    -- Check that indexing into f is within segment bounds.
                    let bounds e = intervalStart df :<= e :&& e :<= intervalEnd df
                    in_segment <-
                      allCases
                        (askQ (CaseCheck (\_ -> bounds $ f_arg M.! i)))
                        (IndexFn new_iter (body g))
                    case in_segment of
                      Yes -> pure $ Just (sym2SoP (Var k))
                      Unknown -> pure Nothing
                  else pure Nothing
          let t2 = solveFor k (intervalStart df) (f_arg M.! i)
          let t3 = solveFor k (intervalEnd df) (f_arg M.! i)
          k_solution <- firstAlt [t1, t2, t3]
          case k_solution of
            Just res -> do
              printM 1338 $ "SOLVED: " <> prettyString res
              pure (repIndexFn (mkRep k res) $ IndexFn new_iter new_body)
            Nothing ->
              error $
                "Would capture k in g: f"
                  <> prettyString f_arg
                  <> "\n"
                  <> prettyString g
                  <> "\nwhere "
                  <> prettyString f_apply
                  <> " = \n"
                  <> prettyString f
    _ ->
      pure (IndexFn new_iter new_body)
  where
    -- Construct replacement from formal arguments of f to actual arguments.
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

-- Solve for x in e(x) = e'.
-- solveFor :: (Replaceable p Symbol) => VName -> p -> SoP Symbol -> IndexFnM (Maybe (SoP Symbol))
solveFor :: VName -> SoP Symbol -> SoP Symbol -> IndexFnM (Maybe (SoP Symbol))
solveFor x e1 e2 = do
  x_hole <- newVName "x_hole"
  -- (Not using mkRep because this has check disallowing Holes.)
  let e1' = rep (M.singleton x $ sym2SoP (Hole x_hole)) e1
  s :: Maybe (Replacement Symbol) <- fmap mapping <$> unify e1' e2
  pure $ s >>= (M.!? x_hole)

isSameRange :: Iterator -> Iterator -> IndexFnM Bool
isSameRange Empty Empty = pure True
isSameRange Empty _ = pure False
isSameRange _ Empty = pure False
isSameRange (Forall _ df) (Forall _ dg) = do
  case (df, dg) of
    (Iota {}, Iota {}) ->
      equiv df dg
    (Cat {}, Cat {}) ->
      equiv df dg
    _ -> do
      start_f <- rewrite (domainStart df)
      start_g <- rewrite (domainStart dg)
      end_f <- rewrite (domainEnd df)
      end_g <- rewrite (domainEnd dg)
      eq_start :: Maybe (Substitution Symbol) <- unify start_f start_g
      eq_end :: Maybe (Substitution Symbol) <- unify end_f end_g
      pure (isJust eq_start && isJust eq_end)
  where
    equiv a b = isJust <$> (unify a b :: IndexFnM (Maybe (Substitution Symbol)))

firstAlt :: (Monad f) => [f (Maybe (SoP Symbol))] -> f (Maybe (SoP Symbol))
firstAlt [] = pure Nothing
firstAlt (m : ms) = do
  x <- m
  printM 1338 $ "firstAlt m: " <> prettyString x
  case x of
    Just y -> pure (Just y)
    Nothing -> firstAlt ms
