-- Index function substitution.
{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.Properties.Substitute ((@), subst) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, when, (<=<))
import Control.Monad.RWS (lift)
import Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S
import Debug.Trace (trace)
import Futhark.Analysis.Properties.AlgebraBridge (answerFromBool, orM, simplify, ($==))
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus (domainEnd, intervalEnd, intervalStart, repCases, repDomain, repIndexFn)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Query (Answer (..), Query (CaseCheck), askQ)
import Futhark.Analysis.Properties.Rewrite (rewrite, rewriteWithoutRules)
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.Traversals
import Futhark.Analysis.Properties.Unify (Rep (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), fv, renameM)
import Futhark.Analysis.Properties.Util
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, sym2SoP)
import Futhark.Util.Pretty (Pretty, prettyString)
import Language.Futhark (VName)

-- If f has multiple cases, we would not know which case to substitute
-- into quantified symbols (e.g., Sum j a b f(e(j))).
legalArg :: VName -> IndexFn -> IndexFn -> Symbol -> [SoP Symbol] -> Bool
legalArg k g f e args =
  let notQuantifier vn = vn < k || or [Just vn == catVar it | it <- shape g]
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
dest_fn @ (f_name, f) = do
  k <- newVName "variables after this are quantifiers"
  g <- renameM dest_fn

  app <- getApply (legalArg k g f) g
  printM 1337 . gray $ prettyString g
  printM 1337 $ warningString "\t@ " <> gray (prettyString (fst <$> app))
  printM 1337 . gray $ "\t  where " <> prettyString f_name <> " =\n" <> prettyIndent 16 f
  case app of
    Just apply -> do
      h <- substituteOnce f g apply
      h' <- fromJust h @ (f_name, f)
      printM 1337 . gray $ "\t  ->\n" <> prettyIndent 16 h'
      pure h'
    Nothing ->
      -- When converting expressions a function may be substituted without arguments.
      fromJust <$> substituteOnce f g (Var f_name, [])
  where
    getApply argCheck = astFold (ASTFolder {foldOnSymbol = getApply_ argCheck}) Nothing

    getApply_ argCheck Nothing e@(Apply (Var vn) args)
      | vn == f_name,
        argCheck e args =
          pure $ Just (e, args)
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

-- Are you substituting xs[i] for xs = for i < e . true => xs[i]?
-- This happens when xs is a formal argument.
trivialSub :: (ReplacementBuilder v Symbol) => IndexFn -> Symbol -> [v] -> Bool
trivialSub (IndexFn [] _) _ _ = False
trivialSub (IndexFn [Forall i _] gs) e [arg] =
  repCases (mkRep i arg) gs == cases [(Bool True, sym2SoP e)]
trivialSub _ _ _ = error "trivialSub: multi-dim not implemented yet"

subber :: (IndexFn -> Symbol -> [SoP Symbol] -> Bool) -> IndexFn -> IndexFnM IndexFn
subber argCheck g = do
  go mempty
  where
    go seen = do
      apply <- getApply seen g
      ixfns <- getIndexFns
      when (isJust apply) $ do
        printM 1337 $ warningString "subst " <> prettyString seen
        printM 1337 . gray $ prettyIndent 4 g
        printM 1337 . gray $ "\t@ " <> prettyString apply
      case apply of
        Just (e, vn, args)
          | Just [f] <- ixfns M.!? vn,
            not (trivialSub f e args),
            argCheck f e args -> do
              g' <- substituteOnce f g (e, args)
              case g' of
                Just new_fn | new_fn /= g -> do
                  subst =<< rewriteWithoutRules new_fn
                _ ->
                  go (S.insert (vn, args) seen)
        Just (_, vn, args)
          | otherwise ->
              go (S.insert (vn, args) seen)
        Nothing ->
          pure g

    getApply seen = astFold (ASTFolder {foldOnSymbol = getApply_ seen}) Nothing

    getApply_ seen Nothing e@(Apply (Var vn) args)
      | (vn, args) `S.notMember` seen =
          pure $ Just (e, vn, args)
    getApply_ _ acc _ = pure acc

{-
              Substitution rules
-}
-- Basic support for multiple arguments is done.
-- (We should reconsider the Cat k abstraction before adding proper support.)
-- TODO git revert 31d79980a270d30a1c3183aafef1bcbb42ae8e3f
substituteOnce :: IndexFn -> IndexFn -> (Symbol, [SoP Symbol]) -> IndexFnM (Maybe IndexFn)
substituteOnce f g_presub (f_apply, actual_args) = do
  vn <- newVName ("<" <> prettyString f_apply <> ">")
  g <- repApply vn g_presub

  traverse simplify <=< applySubRules $
    g
      { shape =
          shape g <&> \case
            Forall j dg
              | vn `notElem` fv dg -> Forall j dg
              | Just e_f <- justSingleCase f ->
                  Forall j $ repDomain (mkRep vn (rep args e_f)) dg
            _ -> error "Not implemented yet: multi-case domain.",
        body = cases $ do
          (p_f, e_f) <- guards f
          (p_g, e_g) <- guards g
          let s = mkRep vn (rep args e_f)
          pure (sop2Symbol (rep s p_g) :&& sop2Symbol (rep args p_f), rep s e_g)
      }
  where
    -- Construct replacement from formal arguments of f to actual arguments.
    -- (`actual_args` may be empty for convience; used internally in Convert).
    args :: Replacement Symbol =
      case actual_args of
        []
          | [] <- shape f ->
              -- All source-program variable references should hit this case.
              mempty
          | [] <- shape g_presub ->
              -- This case is a HACK to allow substitution into preconditions.
              --   Checking precondition ((shape: [m]{i64| (>= 0)}): [m]nat_i64) for mk_flag_array_4631
              --   • | True ⇒    shape₄₆₁₉ ≥ 0
              --   	@
              --   	  where shape_4619 =
              --                   i₁₆₄₂₄ :: 0 .. m₄₆₈₇
              --                   forall i₁₆₄₂₄ . | True ⇒    shape₄₆₈₈[i₁₆₄₂₄]
              mempty
          | length (shape f) == length (shape g_presub) ->
              -- Case used internally in Convert (empty args for convenience).
              map_formal_args_to (shape g_presub <&> sym2SoP . Var . boundVar)
        _
          | length (shape f) == length actual_args ->
              -- All source-program indexing should hit this case.
              map_formal_args_to actual_args
          | otherwise ->
              error "Argument mismatch."
      where
        map_formal_args_to = mconcat . zipWith (mkRep . boundVar) (shape f)

    repApply vn =
      astMap
        ( identityMapper
            { mapOnSymbol = \e ->
                pure $ if e == f_apply then Var vn else e
            }
        )

    -- Side condition for Sub 2 and Sub 3:
    -- If f has a segmented domain, is f(arg) inside the k'th segment?
    arg_in_segment_of_f n = case (shape f !! n, shape g_presub !! n) of
      (Forall i df, Forall j _) -> do
        let arg_eq_j = pure . answerFromBool $ args M.! i == sym2SoP (Var j)
        let bounds e = intervalStart df :<= e :&& e :<= intervalEnd df
        let arg_in_segment_bounds =
              askQ
                (CaseCheck (\_ -> bounds $ args M.! i))
                g_presub
        arg_eq_j `orM` arg_in_segment_bounds

    -- Apply first matching rule for each dimension in g.
    -- XXX for now, this assumes g and f each have at most one dimension;
    -- need to consider each dimension in f always to make sure
    -- all Cat k's are handled.
    applySubRules g =
      runMaybeT $
        if null (shape g)
          then subRules g 0
          else foldM subRules g [0 .. length (shape g) - 1]

    subRules g n =
      sub0 g <|> sub1 n g <|> sub2 n g <|> sub3 n g <|> sub4 n g <|> subX n g

    -- This is rule is needed because we represent scalars as empty shapes rather
    -- than `for i < 1`, as is done in the paper.
    sub0 g = case shape f of
      [] -> pure g
      _ -> fail "No match."

    sub1 n g = case shape f !! n of
      Forall _ Iota {} -> pure g
      _ -> fail "No match."

    sub2 n g | n >= length (shape g) = fail "No match."
    sub2 n g = case (shape f !! n, shape g !! n) of
      (Forall i df@Cat {}, Forall j dg@Iota {}) -> do
        Yes <- lift (rewrite (domainEnd df) >>= ($== domainEnd dg))
        Yes <- lift (arg_in_segment_of_f n)
        pure $ g {shape = l <> [Forall j (repDomain (mkRep i $ Var j) df)] <> r}
        where
          (l, _old_iter : r) = splitAt n (shape g)
      _ -> fail "No match."

    sub3 n g | n >= length (shape g) = fail "No match."
    sub3 n g = case (shape f !! n, shape g !! n) of
      (Forall _ df@(Cat k _ _), Forall _ dg@(Cat k' _ _))
        | k `S.member` fv (body g) -> do
            True <- lift (df `unifiesWith` dg)
            Yes <- lift (arg_in_segment_of_f n)
            pure $ repIndexFn (mkRep k (sym2SoP $ Var k')) g
      _ -> fail "No match."

    sub4 n g = case shape f !! n of
      Forall i df@(Cat k _ _) -> do
        if k `S.member` fv (body g)
          then do
            Just arg <- hoistMaybe . pure $ args M.!? i
            Just solution <-
              lift . firstAlt . map (\e_k -> solveFor k e_k arg) $
                [intervalStart df, intervalEnd df]
            pure $ repIndexFn (mkRep k solution) g
          else pure g
      _ -> fail "No match."

    subX n g = case shape f !! n of
      Forall i df@(Cat k _ _) | k `S.member` fv (body g) -> do
        Just arg <- hoistMaybe . pure $ args M.!? i
        -- Create/lookup II array.
        let def_II = f {body = cases [(Bool True, sym2SoP (Var k))]}
        -- Check that f is not already this II array (e.g., defined by the user).
        Nothing :: Maybe (Substitution Symbol) <- lift $ unify f def_II
        (vn_II, f_II) <- lift $ lookupII df def_II
        lift $ insertIndexFn vn_II [f_II]
        pure (repIndexFn (mkRep k (sym2SoP (Apply (Var vn_II) [arg]))) g)
      _ -> fail "No match."

{-
              Utilities
-}
-- Solve for x in e(x) = e'.
solveFor :: VName -> SoP Symbol -> SoP Symbol -> IndexFnM (Maybe (SoP Symbol))
solveFor x e1 e2 = do
  x_hole <- newVName "x_hole"
  -- (Not using mkRep because this has check disallowing Holes.)
  let e1' = rep (M.singleton x $ sym2SoP (Hole x_hole)) e1
  s :: Maybe (Replacement Symbol) <- fmap mapping <$> unify e1' e2
  pure $ s >>= (M.!? x_hole)

firstAlt :: (Monad f) => [f (Maybe (SoP Symbol))] -> f (Maybe (SoP Symbol))
firstAlt [] = pure Nothing
firstAlt (m : ms) = do
  x <- m
  case x of
    Just y -> pure (Just y)
    Nothing -> firstAlt ms

gray :: String -> String
gray s = "\ESC[2m" <> s <> "\ESC[0m"

lookupII :: Domain -> IndexFn -> IndexFnM (VName, IndexFn)
lookupII dom def = do
  mapping <- getII
  v <- unisearch dom mapping
  case v of
    Just res -> pure res
    Nothing -> do
      vn <- newVName "II"
      insertII dom (vn, def)
      pure (vn, def)

-- Search a mapping using unification for equality checks.
unisearch :: (Ord v, Unify v Symbol, Pretty v) => v -> M.Map v a -> IndexFnM (Maybe a)
unisearch x mapping = do
  case mapping M.!? x of
    Just v ->
      -- Exact match.
      pure (Just v)
    Nothing -> do
      -- Search for matches using unification.
      matches :: [(a, Maybe (Substitution Symbol))] <-
        mapM (\(k, v) -> (v,) <$> unify k x) (M.toList mapping)
      case matches of
        [] -> pure Nothing
        [(v, _)] ->
          pure (Just v)
        _ -> error "unisearch: multiple matches"

unifiesWith :: (Unify v Symbol, Pretty v) => v -> v -> IndexFnM Bool
unifiesWith a b = do
  equiv :: Maybe (Substitution Symbol) <- unify a b
  pure $ isJust equiv
