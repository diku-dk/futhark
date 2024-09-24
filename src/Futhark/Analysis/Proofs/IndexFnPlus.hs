{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.IndexFnPlus where

import Control.Monad (guard)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.SymbolPlus ()
import Futhark.Analysis.Proofs.Unify (Renameable (..), Replaceable (..), Substitution (..), SubstitutionBuilder (..), Unify (..), freshName, sub, unifies_)
import Futhark.Analysis.Proofs.Util (prettyName)
import Futhark.FreshNames (VNameSource)
import Futhark.MonadFreshNames (MonadFreshNames (getNameSource), newName, newNameFromString)
import Futhark.SoP.SoP (SoP, int2SoP, justConstant, mapSymSoP, sopFromList, sopToLists, sym2SoP, (.*.), (.+.), (.-.))
import Futhark.SoP.SoP qualified as SoP
import Futhark.Util.Pretty (Pretty (pretty), commasep, parens, prettyString, stack, (<+>))
import Language.Futhark (VName)

instance Eq Domain where
  -- Since the whole domain must be covered by an index function,
  -- it is sufficient to check that starts and ends are equal.
  u == v =
    start u == start v && end u == end v
    where
      start :: Domain -> SoP Symbol
      start (Iota _) = int2SoP 0
      start (Cat k _ b) = rep (mkSub k (int2SoP 0 :: SoP Symbol)) b

      end (Iota n) = n .-. int2SoP 1
      end (Cat k m b) = rep (mkSub k m) b .-. int2SoP 1

instance Eq Iterator where
  (Forall _ u@(Cat k _ _)) == (Forall _ v@(Cat k' _ _)) = u == v && k == k'
  (Forall _ u) == (Forall _ v) = u == v
  Empty == Empty = True
  _ == _ = False

deriving instance Eq IndexFn

domainStart :: Domain -> SoP Symbol
domainStart (Iota _) = int2SoP 0
domainStart (Cat k _ b) = rep (mkSub k (int2SoP 0 :: SoP Symbol)) b

domainEnd :: Domain -> SoP Symbol
domainEnd (Iota n) = n .-. int2SoP 1
domainEnd (Cat k m b) = rep (mkSub k m) b .-. int2SoP 1

intervalEnd :: Domain -> SoP Symbol
intervalEnd (Cat k _ b) = rep (mkSub k (sym2SoP (Var k) .+. int2SoP 1)) b
intervalEnd (Iota _) = error "intervalEnd on iota"

-------------------------------------------------------------------------------
-- Pretty.
-------------------------------------------------------------------------------
instance (Pretty a, Pretty b) => Pretty (Cases a b) where
  pretty (Cases cs) =
    stack (map prettyCase (NE.toList cs))
    where
      prettyCase (p, e) = "|" <+> pretty p <+> "⇒ " <+> pretty e

instance Pretty Domain where
  pretty (Iota n) = "iota" <+> parens (pretty n)
  pretty dom@(Cat k m b) =
    "⊎"
      <> prettyName k
      <> "="
      <> "iota"
        <+> pretty m
        <+> "["
      <> commasep
        [ pretty b,
          "...",
          pretty (intervalEnd dom)
        ]
      <> ")"

instance Pretty Iterator where
  pretty (Forall i dom) =
    "∀" <> prettyName i <+> "∈" <+> pretty dom
  pretty Empty = ""

instance Pretty IndexFn where
  pretty (IndexFn iter e) = pretty iter <+> "." <+> pretty e

-------------------------------------------------------------------------------
-- Unification.
-------------------------------------------------------------------------------
repVName :: Substitution Symbol -> VName -> VName
repVName s vn
  | Var i <- sop2Symbol $ rep s (Var vn) =
      i
repVName _ _ = error "repVName substitutes for non-VName."

repCase :: (Ord u, Replaceable v1 u, Replaceable v2 u) => Substitution u -> (v1, v2) -> (u, SoP u)
repCase s (a, b) = (sop2Symbol (rep s a), rep s b)

repCases :: (Ord a, Replaceable v1 a, Replaceable v2 a) => Substitution a -> Cases v1 v2 -> Cases a (SoP a)
repCases s (Cases cs) = Cases $ NE.map (repCase s) cs

repDomain :: Substitution Symbol -> Domain -> Domain
repDomain s (Iota n) = Iota (rep s n)
repDomain s (Cat k m b) = Cat k (rep s m) (rep s b)

repIndexFn :: Substitution Symbol -> IndexFn -> IndexFn
repIndexFn s = rip
  where
    rip (IndexFn Empty body) = IndexFn Empty (repCases s body)
    rip (IndexFn (Forall i dom) body) =
      IndexFn (Forall (repVName s i) (repDomain s dom)) (repCases s body)

subIndexFn :: Substitution Symbol -> IndexFn -> IndexFnM IndexFn
subIndexFn s indexfn = repIndexFn s <$> rename_ (vns s) mempty indexfn

instance (Renameable a, Renameable b) => Renameable (Cases a b) where
  rename_ vns tau (Cases cs) = Cases <$> mapM re cs
    where
      re (p, q) = (,) <$> rename_ vns tau p <*> rename_ vns tau q

renameCat_ :: (MonadFreshNames m) => VNameSource -> M.Map VName VName -> VName -> SoP Symbol -> SoP Symbol -> m (Domain, (M.Map VName VName, VNameSource))
renameCat_ vns tau xn m b = do
  (xm, vns') <- freshName vns
  let tau' = M.insert xn xm tau
  dom <- Cat xm <$> rename_ vns' tau' m <*> rename_ vns' tau' b
  pure (dom, (tau', vns'))

instance Renameable Domain where
  rename_ vns tau (Cat xn m b) = fst <$> renameCat_ vns tau xn m b
  rename_ vns tau (Iota n) = Iota <$> rename_ vns tau n

instance Renameable IndexFn where
  rename_ vns tau indexfn = case indexfn of
    IndexFn Empty body -> IndexFn Empty <$> rename_ vns tau body
    -- NOTE that i is not renamed.
    IndexFn (Forall i (Cat xn m b)) body -> do
      (dom, (tau', vns')) <- renameCat_ vns tau xn m b
      IndexFn (Forall i dom) <$> rename_ vns' tau' body
    IndexFn (Forall i dom) body -> do
      dom' <- rename_ vns tau dom
      IndexFn (Forall i dom') <$> rename_ vns tau body

instance (MonadFreshNames m) => Unify Domain Symbol m where
  unify_ k (Iota n) (Iota m) = unify_ k n m
  unify_ k (Cat _ m1 b1) (Cat _ m2 b2) = do
    s <- unify_ k m1 m2
    (s <>) <$> unify_ k (rep s b1) (rep s b2)
  unify_ _ _ _ = fail "Incompatible domains"

instance (MonadFreshNames m) => Unify (Cases Symbol (SoP Symbol)) Symbol m where
  unify_ k (Cases cs1) (Cases cs2) = do
    s <- unifies_ k (zip (map fst xs) (map fst ys))
    s2 <- unifies_ k (zip (map (rep s . snd) xs) (map (rep s . snd) ys))
    pure $ s <> s2
    where
      xs = NE.toList cs1
      ys = NE.toList cs2

-- XXX we require that index function quantifiers (indexing variables) are unique!
instance (MonadFreshNames m) => Unify IndexFn Symbol m where
  unify_ k (IndexFn Empty body1) (IndexFn Empty body2) =
    unify_ k body1 body2
  unify_ k (IndexFn (Forall i dom1) body1) (IndexFn (Forall j dom2) body2) = do
    s <- unify_ k (Hole i) (Var j)
    s' <- (s <>) <$> unify_ k (repDomain s dom1) (repDomain s dom2)
    (s' <>) <$> unify_ k (repCases s' body1) (repCases s' body2)
  unify_ _ _ _ = fail "Incompatible iterators"

-------------------------------------------------------------------------------
-- Index function substitution.
-------------------------------------------------------------------------------
-- 'sub vn x y' substitutes name 'vn' for indexfn 'x' in indexfn 'y'.
subst :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
subst x for@(IndexFn (Forall i _) _) into@(IndexFn (Forall j _) _) = do
  -- debugM
  --   ( "🎭 substitute\n"
  --       <> prettyBinding x for
  --       <> "\ninto\n"
  --       <> prettyString into
  --   )
  i' <- sym2SoP . Var <$> newName i
  vns <- getNameSource
  for' <- rename vns for
  into' <- rename vns into
  subst' x (repIndexFn (mkSub i i') for') (repIndexFn (mkSub j i') into')
subst x q r = subst' x q r

subst' :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
subst' x (IndexFn Empty xs) (IndexFn iter_y ys) =
  -- No rule in document (substituting scalar into index function).
  pure $
    IndexFn
      iter_y
      ( cases $ do
          (xcond, xval) <- casesToList xs
          (ycond, yval) <- casesToList ys
          pure $ repCase (mkSub x xval) (ycond :&& xcond, yval)
      )
subst' x (IndexFn (Forall _ (Iota n)) xs) (IndexFn (Forall i (Iota n')) ys)
  | n == n' =
      pure $
        IndexFn
          (Forall i (Iota n))
          ( cases $ do
              (xcond, xval) <- casesToList xs
              (ycond, yval) <- casesToList ys
              let app = apply i (mkSub x xval)
              pure (sop2Symbol (app ycond) :&& xcond, mapSymSoP app yval)
          )
subst' _ _ _ = undefined

apply :: VName -> Substitution Symbol -> Symbol -> SoP Symbol
apply i s symbol = case symbol of
  Hole _ -> error "Apply on Hole."
  -- Idx (Var vn) j -> rep (mkSub i j) $ rep s (Var vn)
  _ -> rep s symbol

-------------------------------------------------------------------------------
-- Index function normalization.
-------------------------------------------------------------------------------
normalizeIndexFn :: IndexFn -> IndexFnM IndexFn
normalizeIndexFn fn = allCasesAreConstants fn >>= rewritePrefixSum

allCasesAreConstants :: IndexFn -> IndexFnM IndexFn
allCasesAreConstants v@(IndexFn _ (Cases ((Bool True, _) NE.:| []))) = pure v
allCasesAreConstants (IndexFn it (Cases cs))
  | Just sops <- mapM (justConstant . snd) cs = do
      let preds = NE.map fst cs
          sumOfIndicators =
            SoP.normalize . foldl1 (.+.) . NE.toList $
              NE.zipWith
                (\p x -> sym2SoP (Indicator p) .*. int2SoP x)
                preds
                sops
      -- tell ["Using simplification rule: integer-valued cases"]
      pure $ IndexFn it $ Cases (NE.singleton (Bool True, sumOfIndicators))
allCasesAreConstants v = pure v

rewritePrefixSum :: IndexFn -> IndexFnM IndexFn
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
--    | i == b => e1              (e1 may depend on i)
--    | i /= b => y[i-1] + e2     (e2 may depend on i)
--
-- e2 is a SoP with terms e2_0, ..., e2_l. Each term is a constant,
-- an indexing statement or an indicator of an indexing statement.
-- XXX Is this condition necessary in the revised system?
-- _______________________________________________________________
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
--    e1{b/i} + (Σ_{j=b+1}^i e2_0{j/i}) + ... + (Σ_{j=b+1}^i e2_l{j/i})
rewritePrefixSum indexfn@(IndexFn (Forall i dom) (Cases cs))
  | [(p1, e1), (p2, recur)] <- NE.toList cs,
    Just e2 <- justRecurrencePlusSoP recur = do
      let b = domainSegStart dom
      s_c <-
        unify
          ( cases
              [ (sym2SoP (Var i) :== b, e1),
                (sym2SoP (Var i) :/= b, e2)
              ]
          )
          (cases [(p1, e1), (p2, e2)])
      case s_c :: Maybe (Substitution Symbol) of
        Nothing -> pure indexfn
        Just _ -> do
          -- debugM $ "MATCHED rewritePrefixSum\n" <> prettyString indexfn
          j <- newNameFromString "j"
          e1_b <- sub (mkSub i b) e1
          e2_j <- sub (mkSub i (sym2SoP $ Var j)) e2
          let e2_sum = applyLinCombRule j (b .+. int2SoP 1) (sym2SoP $ Var i) e2_j
          let res =
                IndexFn
                  { iterator = Forall i dom,
                    body = cases [(Bool True, e1_b .+. e2_sum)]
                  }
          -- debugM $ "=> " <> prettyString res
          pure res
  where
    -- Returns the argument SoP without its recurrence term, if it is
    -- on the form `Recurrence + x` where x does not contain Recurrence
    -- anywhere. Otherwise returns Nothing.
    justRecurrencePlusSoP sop
      | ([_rec], other_terms) <- L.partition (== ([Recurrence], 1)) xs = do
          guard $ not (any ((Recurrence `elem`) . fst) other_terms)
          Just . sopFromList $ other_terms
      where
        xs = sopToLists sop
    justRecurrencePlusSoP _ = Nothing
rewritePrefixSum indexfn = pure indexfn
