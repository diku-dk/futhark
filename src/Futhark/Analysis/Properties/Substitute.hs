-- Index function substitution.
{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.Properties.Substitute ((@), subst) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, when, (<=<))
import Control.Monad.RWS (lift)
import Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set qualified as S
import Debug.Trace (trace)
import Futhark.Analysis.Properties.AlgebraBridge (answerFromBool, orM, simplify, ($==))
import Futhark.Analysis.Properties.Flatten (from1Dto2D, lookupII)
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus (domainEnd, intervalEnd, intervalStart, repCases, repDomain, repIndexFn)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Query (Answer (..), Query (CaseCheck), askQ)
import Futhark.Analysis.Properties.Rewrite (rewrite, rewriteWithoutRules, unifiesWith, solveIx)
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.Traversals
import Futhark.Analysis.Properties.Unify (Rep (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), fv, renameM)
import Futhark.Analysis.Properties.Util
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, justSym, sym2SoP, (.*.), (.+.))
import Futhark.Util.Pretty (prettyString)
import Language.Futhark (VName)

-- If f has multiple cases, we would not know which case to substitute
-- into quantified symbols (e.g., Sum j a b f(e(j))).
legalArg :: VName -> IndexFn -> IndexFn -> Symbol -> [SoP Symbol] -> Bool
legalArg k g f e args =
  let notQuantifier vn = vn < k || or [Just vn == catVar it | it <- concat $ shape g]
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
      -- This may fail when substituting into an uninterpreted function.
      fromMaybe dest_fn <$> substituteOnce f g (Var f_name, [])
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
-- This happens when xs is a formal argument. (So not relevant for flattened arrays.)
trivialSub :: Symbol -> IndexFn -> [SoP Symbol] -> Bool
trivialSub _ (IndexFn [] _) _ = False
trivialSub e f args
  | length (shape f) == length args,
    all ((== 1) . length) (shape f),
    Just e' <- justSym =<< justSingleCase f =
      sym2SoP e == rep dims2args e'
  | otherwise = False
  where
    dims2args =
      mkRepFromList $ zipWith (\[dim] arg -> (boundVar dim, arg)) (shape f) args

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
            not (trivialSub e f args),
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

-- TODO not sure this is useful
-- eliminateII :: IndexFn -> IndexFnM IndexFn
-- eliminateII f@(IndexFn [Forall i d@(Cat k _ _)] _) = do
--   res <- unisearch d =<< getII
--   case res of
--     Just (ii, _) -> do
--       printM 1 ("eliminateII: " <> prettyStr f)
--       -- TODO replace any II(e) where e is provably in `k`th segment.
--       -- For now, just replace II(i).
--       cs <- astMap (identityMapper {mapOnSymbol = repII ii}) (body f)
--       pure (f {body = cs})
--     Nothing -> pure f
--   where
--     repII vn (Apply (Var x) [arg])
--       | vn == x,
--         arg == sym2SoP (Var i) =
--           pure (Var k)
--     repII _ e = pure e
-- eliminateII f = pure f

{-
              Substitution rules
-}
-- Substitute `f(args)` for its value in `g`.
substituteOnce :: IndexFn -> IndexFn -> (Symbol, [SoP Symbol]) -> IndexFnM (Maybe IndexFn)
substituteOnce f g_presub (f_apply, actual_args) = do
  vn <- newVName ("<" <> prettyString f_apply <> ">")
  g <- repApply vn g_presub

  let new_shape =
        shape g
          <&> map
            ( \case
                Forall j dg
                  -- f(args) is not in dom(g).
                  | vn `notElem` fv dg -> Forall j dg
                  -- f(args) is in dom(g) and f has only one case.
                  | Just e_f <- justSingleCase f ->
                      Forall j $ repDomain (mkRep vn (rep args e_f)) dg
                  -- f(args) is in dom(g) and f has multiple cases.
                  | e_f <- flattenCases (body f) ->
                      Forall j $ repDomain (mkRep vn (rep args e_f)) dg
            )
  traverse (simplify <=< solveIx new_shape) <=< applySubRules $
  -- traverse (solveIx new_shape <=< simplify) <=< applySubRules $
  -- traverse simplify <=< applySubRules $
    g
      { shape = new_shape,
        body = cases $ do
          (p_f, e_f) <- guards f
          (p_g, e_g) <- guards g
          let s = mkRep vn (rep args e_f)
          pure (sop2Symbol (rep s p_g) :&& sop2Symbol (rep args p_f), rep s e_g)
      }
  where
    -- Construct replacement from formal arguments of f to actual arguments.
    args :: Replacement Symbol =
      case actual_args of
        []
          | [] <- shape f ->
              -- All source-program variable references should hit this case.
              mempty
          | [] <- shape g_presub ->
              -- This case is a HACK to allow substitution into preconditions:
              --   (shape: [m]{i64| (>= 0)})
              -- gives
              --   • | True ⇒    shape₄₆₁₉ ≥ 0
              --   	@
              --   	  where shape_4619 =
              --                   i₁₆₄₂₄ :: 0 .. m₄₆₈₇
              --                   forall i₁₆₄₂₄ . | True ⇒    shape₄₆₈₈[i₁₆₄₂₄]
              mempty
          | rank f == rank g_presub,
            map length (shape f) == map length (shape g_presub) ->
              -- This case is a convenience HACK to allow empty arguments in Convert.
              mconcat . zipWith mkArg (shape f) $
                concat (shape g_presub) <&> sym2SoP . Var . boundVar
        _
          | rank f == length actual_args ->
              -- All source-program indexing should hit this case.
              mconcat $ zipWith mkArg (shape f) actual_args
          | otherwise ->
              error "Argument mismatch."

    mkArg [Forall i _] = mkRep i
    mkArg [d1, d2] =
      -- Essentially SubFlat-Simplified from the supplementary material.
      mkRepFromList . from1Dto2D d1 d2
    mkArg _ = error "nd flatten not implemented yet."

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
      ([Forall i df], [Forall j _]) -> do
        let arg_eq_j = pure . answerFromBool $ args M.! i == sym2SoP (Var j)
        let bounds e = intervalStart df :<= e :&& e :<= intervalEnd df
        let arg_in_segment_bounds =
              askQ
                (CaseCheck (\_ -> bounds $ args M.! i))
                g_presub
        arg_eq_j `orM` arg_in_segment_bounds
      _ -> error "Not implemented yet"

    -- Apply first matching rule for each dimension in f.
    applySubRules g =
      runMaybeT $
        if null (shape g)
          then subRules g 0
          else foldM subRules g [0 .. rank f - 1]

    subRules g n =
      sub0 g <|> propFlattenSimplified n g <|> sub1 n g <|> sub2 n g <|> sub3 n g <|> sub4 n g <|> subX n g

    -- This is rule is needed because we represent scalars as empty shapes rather
    -- than `for i < 1`, as is done in the paper.
    sub0 g = case shape f of
      [] -> pure g
      _ -> fail "No match."

    -- Propagate flattened domain from f to g.
    --
    -- TODO this only tries to align the n'th dimension of f with the k'th
    -- dimension of g. But we could try any dimension in g.
    propFlattenSimplified n g | n >= rank g = fail "No match."
    propFlattenSimplified k g = case (shape g !! k, shape f !! k) of
      ([Forall i_1 (Iota e_1)], df@[Forall i_2 (Iota e_2), Forall i_3 (Iota e_3)])
        -- PropFlatten-Simplified from the supplementary material.
        -- (The case where `e_3` may depend on `i_2` is still handled by Cat in
        -- this implementation.)
        | i_2 `S.notMember` fv e_3 -> do
            printM 3 $ "propFlattenSimplified\n  |_ e_1 " <> prettyStr e_1
            printM 3 $ "  |_ e_2 " <> prettyStr e_2
            printM 3 $ "  |_ e_3 " <> prettyStr e_3
            ans <- lift (e_1 $== e_2 .*. e_3)
            case ans of
              Yes -> do
                e_row <- lift . rewrite $ sym2SoP (Var i_2) .*. e_3
                let s = mkRep i_1 (e_row .+. sym2SoP (Var i_3))
                pure $ g {shape = l <> (df : r), body = repCases s (body g)}
                error "propFlattenSimplified succeeded (I'd like to know first time when getting rid of Cat)"
              Unknown -> pure g
        where
          (l, _old_iter : r) = splitAt k (shape g)
      _ -> fail "No match."

    sub1 n g =
      if all (\case (Forall _ Iota {}) -> True; _ -> False) (shape f !! n)
        then pure g
        else fail "No match."

    -- Propagate Cat domain from f to g.
    --
    -- TODO this only tries to align the n'th dimension of f with the n'th
    -- dimension of g. But we could try any dimension in g.
    sub2 n g | n >= rank g = fail "No match."
    sub2 n g = case (shape f !! n, shape g !! n) of
      ([Forall i df@Cat {}], [Forall j dg@Iota {}]) -> do
        Yes <- lift (rewrite (domainEnd df) >>= ($== domainEnd dg))
        Yes <- lift (arg_in_segment_of_f n)
        pure $ g {shape = l <> [[Forall j (repDomain (mkRep i $ Var j) df)]] <> r}
        where
          (l, _old_iter : r) = splitAt n (shape g)
      _ -> fail "No match."

    -- f and g's Cat domains unify.
    --
    -- TODO this only tries to align the n'th dimension of f with the n'th
    -- dimension of g. But we could try any dimension in g.
    sub3 n g | n >= rank g = fail "No match."
    sub3 n g = case (shape f !! n, shape g !! n) of
      ([Forall _ df@(Cat k _ _)], [Forall _ dg@(Cat k' _ _)])
        | k `S.member` fv (body g) -> do
            True <- lift (df `unifiesWith` dg)
            Yes <- lift (arg_in_segment_of_f n)
            pure $ repIndexFn (mkRep k (sym2SoP $ Var k')) g
      _ -> fail "No match."

    -- f's Cat domain can be simplified away.
    sub4 n g = case shape f !! n of
      [Forall i df@(Cat k _ _)] -> do
        if k `S.member` fv (body g)
          then do
            Just arg <- hoistMaybe . pure $ args M.!? i
            Just solution <-
              lift . firstAlt . map (\e_k -> solveFor k e_k arg) $
                [intervalStart df, intervalEnd df]
            pure $ repIndexFn (mkRep k solution) g
          else pure g
      _ -> fail "No match."

    -- f's Cat domain handled using II array.
    subX n g = case shape f !! n of
      [Forall i df@(Cat k _ _)] | k `S.member` fv (body g) -> do
        Just arg <- hoistMaybe . pure $ args M.!? i
        -- Create/lookup II array.
        let def_II = f {body = cases [(Bool True, sym2SoP (Var k))]}
        -- Check that f is not already this II array (e.g., defined by the user).
        Nothing :: Maybe (Substitution Symbol) <- lift $ unify f def_II
        (vn_II, f_II) <- lift $ lookupII df def_II
        lift $ insertIndexFn vn_II [f_II]
        pure (repIndexFn (mkRep k (sym2SoP (Apply (Var vn_II) [arg]))) g)
      _ -> error "substitution rules non-exhaustive."

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
