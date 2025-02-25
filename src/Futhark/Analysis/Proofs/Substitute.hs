-- Index function substitution.
module Futhark.Analysis.Proofs.Substitute ((@), subst) where

import Control.Monad (unless)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S
import Debug.Trace (trace)
import Futhark.Analysis.Proofs.AlgebraBridge (algebraContext, simplify)
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd, intervalStart, repDomain, repIndexFn)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Query (Answer (..), Query (CaseCheck), allCases, askQ, foreachCase, isYes)
import Futhark.Analysis.Proofs.Rewrite (rewrite, rewriteWithoutRules)
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Traversals (ASTFolder (..), ASTMapper (..), astFold, astMap, identityMapper)
import Futhark.Analysis.Proofs.Unify (Replaceable (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), fv, renameM, renameSame)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, sym2SoP)
import Futhark.Util.Pretty (prettyString)
import Language.Futhark (VName)

-- If f has multiple cases, we would not know which case to substitute
-- into quantified symbols (e.g., Sum j a b f(e(j))).
legalArg :: VName -> IndexFn -> IndexFn -> Symbol -> [SoP Symbol] -> Bool
legalArg k g f e args =
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
            <> prettyString g
            <> "\nfor\n"
            <> prettyString f

    hasSingleCase = isJust . justSingleCase

-- Substitute applications of (f_name = f) into g.
-- Unlike `subst` this errors if the substitution fails and
-- will not check that indexing is within bounds.
-- We use @ for convenience when substituting intermediate results in Convert.
-- 'g @ (f_name, f)' substitutes name 'f_name' for indexfn 'f' in indexfn 'g'.
-- TODO remove this or deduplicate overlap with `subst`.
(@) :: IndexFn -> (VName, IndexFn) -> IndexFnM IndexFn
dest_fn @ (f_name, _)
  | f_name `S.notMember` fv dest_fn =
      pure dest_fn
dest_fn @ (f_name, src_fn) = do
  k <- newVName "variables after this are quantifiers"
  (f, g) <- renameSame src_fn dest_fn

  printM 1337 $
    "(" <> prettyString g <> ") @ " <> prettyString f_name <> " = " <> prettyString f
  app <- getApply (legalArg k g f) g
  case app of
    Just apply -> do
      printM 1337 $ " @ " <> prettyString apply
      h <- substituteOnce f g apply
      fromJust h @ (f_name, f)
    Nothing ->
      -- When converting expressions a function may be substituted without arguments.
      fromJust <$> substituteOnce f g (Var f_name, [])
  where
    getApply argCheck = astFold (ASTFolder {foldOnSymbol = getApply_ argCheck}) Nothing

    getApply_ argCheck Nothing e@(Apply (Var vn) args)
      | vn == f_name,
        argCheck e args =
          pure $ Just (e, args)
    getApply_ argCheck Nothing e@(Idx (Var vn) arg)
      | vn == f_name,
        argCheck e [arg] = do
          pure $ Just (e, [arg])
    getApply_ _ acc _ = pure acc

-- Substitution as defined in the paper.
-- Unlike @ this will attempt to substitute all indexing/applications
-- in the index function and allows those substitutions to fail (no-op).
-- Unlike @ this also checks bounds.
subst :: IndexFn -> IndexFnM IndexFn
subst indexfn = do
  k <- newVName "variables after this are quantifiers"
  g <- renameM indexfn
  subber (legalArg k g) g

subber :: (IndexFn -> Symbol -> [SoP Symbol] -> Bool) -> IndexFn -> IndexFnM IndexFn
subber argCheck g = do
  go mempty
  where
    go seen = do
      apply <- getApply seen g
      ixfns <- getIndexFns
      printM 1337 $ warningString "subst " <> prettyString seen
      printM 1337 $ "apply " <> prettyString apply
      printM 1337 $ "      " <> prettyString g
      case apply of
        Just (e, vn, args)
          | Just [f] <- ixfns M.!? vn,
            argCheck f e args -> do
              printM 1 . warningString $
                "Checking indexing within bounds " <> prettyString e
              c <- checkBounds f g (e, args)
              if c
                then do
                  g' <- substituteOnce f g (e, args)
                  case g' of
                    Just new_fn -> do
                      subst =<< rewriteWithoutRules new_fn
                    Nothing ->
                      go (S.insert (vn, args) seen)
                else go (S.insert (vn, args) seen)
        Just (_, vn, args)
          | otherwise ->
              go (S.insert (vn, args) seen)
        Nothing ->
          pure g

    getApply seen = astFold (ASTFolder {foldOnSymbol = getApply_ seen}) Nothing

    getApply_ seen Nothing e@(Apply (Var vn) args)
      | (vn, args) `S.notMember` seen =
          pure $ Just (e, vn, args)
    getApply_ seen Nothing e@(Idx (Var vn) arg)
      | (vn, [arg]) `S.notMember` seen =
          pure $ Just (e, vn, [arg])
    getApply_ _ acc _ = pure acc

-- Assumes f and g_non_repped have been renamed using renameSame.
substituteOnce :: IndexFn -> IndexFn -> (Symbol, [SoP Symbol]) -> IndexFnM (Maybe IndexFn)
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
                    if f_arg M.!? i == (sym2SoP . Var <$> iterVar g_iter)
                      then pure (sym2SoP . Var <$> catVar new_iter)
                      else -- Check that indexing into f is within segment bounds.
                      do
                        let bounds e = intervalStart df :<= e :&& e :<= intervalEnd df
                        in_segment <-
                          allCases
                            (askQ (CaseCheck (\_ -> bounds $ f_arg M.! i)))
                            (IndexFn new_iter (body g))
                        case in_segment of
                          Yes -> pure (sym2SoP . Var <$> catVar new_iter)
                          Unknown -> pure Nothing
                  else pure Nothing
          let t2 = solveFor k (intervalStart df) (f_arg M.! i)
          let t3 = solveFor k (intervalEnd df) (f_arg M.! i)
          k_solution <- firstAlt [t1, t2, t3]
          case k_solution of
            Just res -> do
              pure (Just $ repIndexFn (mkRep k res) $ IndexFn new_iter new_body)
            Nothing -> do
              printM 1337 $ "new_body " <> prettyString new_body
              printM 1337 . warningString $
                "Would capture k in g: f"
                  <> prettyString f_arg
                  <> "\n"
                  <> prettyString g
                  <> "\nwhere "
                  <> prettyString f_apply
                  <> " = \n"
                  <> prettyString f
              -- Create II array.
              -- let f_II = IndexFn (iterator f) (cases [(Bool True, sym2SoP (Var k))])
              -- printM 1337 . warningString $
              --   "Creating II array " <> prettyString f_II
              -- vn_II <- newVName "II"
              -- insertIndexFn vn_II [f_II]
              -- let res = sym2SoP (Idx (Var vn_II) (f_arg M.! i))
              -- pure (Just $ repIndexFn (mkRep k res) $ IndexFn new_iter new_body)
              pure Nothing
    _ ->
      pure (Just $ IndexFn new_iter new_body)
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

    iterVar (Forall i _) = Just i
    iterVar Empty = Nothing

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
  case x of
    Just y -> pure (Just y)
    Nothing -> firstAlt ms

checkBounds :: IndexFn -> IndexFn -> (Symbol, [SoP Symbol]) -> IndexFnM Bool
checkBounds (IndexFn Empty _) _ (_, []) = pure True
checkBounds (IndexFn Empty _) _ _ = error "checkBounds: indexing into scalar"
checkBounds f@(IndexFn (Forall _ df) _) g (f_apply, [f_arg]) = algebraContext g $ do
  beg <- rewrite $ domainStart df
  end <- rewrite $ domainEnd df
  case df of
    Iota {} -> do
      (&&)
        <$> doCheck (\_ -> beg :<= f_arg)
        <*> doCheck (\_ -> f_arg :<= end)
    Cat {} -> do
      interval_beg <- rewrite $ intervalStart df
      interval_end <- rewrite $ intervalEnd df
      (&&)
        <$> doCheck (\_ -> beg :<= f_arg :|| interval_beg :<= f_arg)
        <*> doCheck (\_ -> f_arg :<= end :|| interval_end :<= f_arg)
  where
    applyIn =
      astFold
        -- (ASTFolder { foldOnSymbol = \acc e -> (acc ||) <$> equiv e f_apply })
        (ASTFolder {foldOnSymbol = \acc e -> pure (acc || e == f_apply)})
        False

    doCheck :: (SoP Symbol -> Symbol) -> IndexFnM Bool
    doCheck bound =
      fmap and . foreachCase g $ \n -> do
        let (p_idx, e_idx) = getCase n $ body g
        need_to_check <- (||) <$> applyIn (sym2SoP p_idx) <*> applyIn e_idx
        if need_to_check
          then do
            c <- askQ (CaseCheck bound) g n
            unless (isYes c) $ do
              printExtraDebugInfo n
              -- error $
              printM 1 . warningString $
                "Unsafe indexing: "
                  <> prettyString f_apply
                  <> " (failed to show: "
                  <> prettyString p_idx
                  <> " => "
                  <> prettyString (bound e_idx)
                  <> ")."
            pure (isYes c)
          else do
            printM 1337 $
              "doCheck skip case: apply not in " <> prettyString (p_idx, e_idx)
            pure True
      where
        printExtraDebugInfo n = do
          env <- getAlgEnv
          printM 1337 $
            "Failed bounds-checking:"
              <> "\nf:"
              <> prettyString f
              <> "\ng: "
              <> prettyString g
              <> "\nCASE g: "
              <> show n
              <> "\nUnder AlgEnv:"
              <> prettyString env
checkBounds _ _ _ = error "checkBounds: multi-dim not implemented yet"
