{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.IndexFnPlus where

import Control.Monad (unless)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (isJust)
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.SymbolPlus (repVName, toSumOfSums)
import Futhark.Analysis.Proofs.Unify (Renameable (..), Replaceable (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), freshNameFromString, unifies_)
import Futhark.Analysis.Proofs.Util (prettyName)
import Futhark.FreshNames (VNameSource)
import Futhark.MonadFreshNames (MonadFreshNames (getNameSource), newName)
import Futhark.SoP.SoP (SoP, int2SoP, mapSymSoP, sym2SoP, (.+.), (.-.))
import Futhark.Util.Pretty (Pretty (pretty), commastack, line, parens, prettyString, stack, (<+>))
import Language.Futhark (VName)

instance Eq Domain where
  -- Since the whole domain must be covered by an index function,
  -- it is sufficient to check that starts and ends are equal.
  u == v =
    start u == start v && end u == end v
    where
      start :: Domain -> SoP Symbol
      start (Iota _) = int2SoP 0
      start (Cat k _ b) = rep (mkRep k (int2SoP 0 :: SoP Symbol)) b

      end (Iota n) = n .-. int2SoP 1
      end (Cat k m b) = rep (mkRep k m) b .-. int2SoP 1

instance Eq Iterator where
  (Forall _ u@(Cat k _ _)) == (Forall _ v@(Cat k' _ _)) = u == v && k == k'
  (Forall _ u) == (Forall _ v) = u == v
  Empty == Empty = True
  _ == _ = False

deriving instance Eq IndexFn

domainStart :: Domain -> SoP Symbol
domainStart (Iota _) = int2SoP 0
domainStart (Cat k _ b) = rep (mkRep k (int2SoP 0 :: SoP Symbol)) b

domainEnd :: Domain -> SoP Symbol
domainEnd (Iota n) = n .-. int2SoP 1
domainEnd (Cat k m b) = rep (mkRep k m) b .-. int2SoP 1

intervalEnd :: Domain -> SoP Symbol
intervalEnd (Cat k _ b) = rep (mkRep k (sym2SoP (Var k) .+. int2SoP 1)) b .-. int2SoP 1
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
  pretty (Iota n) = parens $ "0 .." <+> parens (pretty n)
  pretty dom@(Cat k m b) =
    "⊎"
      <> prettyName k
      <> "="
      <> parens ("0 .." <+> pretty (m .-. int2SoP 1))
        <+> "["
      <> commastack [pretty b, "...", pretty (intervalEnd dom)]
      <> "]"

instance Pretty Iterator where
  pretty (Forall i (Iota n)) =
    prettyName i <+> ":: 0 .." <+> pretty n
      <> line
      <> "forall "
      <> prettyName i <+> "."
  pretty (Forall i dom@(Cat k m seg)) =
    prettyName k <+> ":: 0 .." <+> pretty m
      <> line
      <> prettyName i <+> "::" <+> "⊎"
      <> prettyName k
        <+> "["
      <> commastack [pretty seg, "...", pretty (intervalEnd dom)]
      <> "]"
      <> line
      <> "forall "
      <> prettyName i <+> "."
  pretty Empty = ""

instance Pretty IndexFn where
  pretty (IndexFn iter e) = pretty iter <+> pretty e

-------------------------------------------------------------------------------
-- Unification.
-------------------------------------------------------------------------------
repCase :: (Ord u, Replaceable v1 u, Replaceable v2 u, Pretty u) => Replacement u -> (v1, v2) -> (u, SoP u)
repCase s (a, b) = (sop2Symbol (rep s a), rep s b)

repCases :: (Ord a, Replaceable v1 a, Replaceable v2 a, Pretty a) => Replacement a -> Cases v1 v2 -> Cases a (SoP a)
repCases s (Cases cs) = Cases $ NE.map (repCase s) cs

repDomain :: Replacement Symbol -> Domain -> Domain
repDomain s (Iota n) = Iota (rep s n)
repDomain s (Cat k m b) = Cat k (rep s m) (rep s b)

repIndexFn :: Replacement Symbol -> IndexFn -> IndexFn
repIndexFn s = rep'
  where
    rep' (IndexFn Empty body) = IndexFn Empty (repCases s body)
    rep' (IndexFn (Forall i dom) body) =
      IndexFn (Forall (repVName s i) (repDomain s dom)) (repCases s body)

subIndexFn :: Substitution Symbol -> IndexFn -> IndexFnM IndexFn
subIndexFn s indexfn = repIndexFn (mapping s) <$> rename (vns s) indexfn

instance (Renameable a, Renameable b) => Renameable (Cases a b) where
  rename_ vns tau (Cases cs) = Cases <$> mapM re cs
    where
      re (p, q) = (,) <$> rename_ vns tau p <*> rename_ vns tau q

renameCat_ :: (MonadFreshNames m) => VNameSource -> M.Map VName VName -> VName -> SoP Symbol -> SoP Symbol -> m (Domain, (M.Map VName VName, VNameSource))
renameCat_ vns tau xn m b = do
  (xm, vns') <- freshNameFromString vns "k"
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

instance Unify Domain Symbol where
  unify_ k (Iota n) (Iota m) = unify_ k n m
  unify_ k (Cat _ m1 b1) (Cat _ m2 b2) = do
    s <- unify_ k m1 m2
    (s <>) <$> unify_ k (rep s b1) (rep s b2)
  unify_ _ _ _ = fail "Incompatible domains"

instance Unify (Cases Symbol (SoP Symbol)) Symbol where
  unify_ k (Cases cs1) (Cases cs2) = do
    s <- unifies_ k (zip (map fst xs) (map fst ys))
    s2 <- unifies_ k (zip (map (rep s . snd) xs) (map (rep s . snd) ys))
    pure $ s <> s2
    where
      xs = NE.toList cs1
      ys = NE.toList cs2

instance Unify IndexFn Symbol where
  unify_ = unifyIndexFnWith unify_

unifyIndexFnWith ::
  (VName -> Cases Symbol (SoP Symbol) -> Cases Symbol (SoP Symbol) -> MaybeT IndexFnM (Replacement Symbol)) ->
  VName ->
  IndexFn ->
  IndexFn ->
  MaybeT IndexFnM (Replacement Symbol)
unifyIndexFnWith unifyBody k (IndexFn Empty body1) (IndexFn Empty body2) =
  unifyBody k body1 body2
unifyIndexFnWith unifyBody k (IndexFn (Forall i dom1) body1) (IndexFn (Forall j dom2) body2) = do
  s <- unify_ k (Hole i) (Var j)
  s' <- (s <>) <$> unify_ k (repDomain s dom1) (repDomain s dom2)
  (s' <>) <$> unifyBody k (repCases s' body1) (repCases s' body2)
unifyIndexFnWith _ _ _ _ = fail "Incompatible iterators"

-------------------------------------------------------------------------------
-- Index function substitution.
-------------------------------------------------------------------------------
-- 'sub vn x y' substitutes name 'vn' for indexfn 'x' in indexfn 'y'.
subst :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
subst x for@(IndexFn (Forall i _) _) into@(IndexFn (Forall j _) _) = do
  debugM
    ( "🎭 substitute "
        <> prettyString x
        <> " for\n"
        <> prettyString for
        <> "\ninto\n"
        <> prettyString into
    )
  i' <- sym2SoP . Var <$> newName i
  vns <- getNameSource
  for' <- rename vns for
  into' <- rename vns into
  subst' x (repIndexFn (mkRep i i') for') (repIndexFn (mkRep j i') into')
subst x q r = subst' x q r

-- Assumes that Forall-variables (i) of non-Empty iterators are equal.
subst' :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
subst' x (IndexFn Empty xs) (IndexFn iter_y ys) =
  -- Substitute scalar `x` into index function `y`.
  pure $
    IndexFn
      iter_y
      ( cases $ do
          (x_cond, x_val) <- casesToList xs
          (y_cond, y_val) <- casesToList ys
          pure $ repCase (mkRep x x_val) (y_cond :&& x_cond, y_val)
      )
subst' x_fn (IndexFn (Forall i (Iota n)) xs) (IndexFn (Forall _ (Iota n')) ys)
  | n == n' =
      pure $
        IndexFn
          (Forall i (Iota n))
          ( cases $ do
              (x_cond, x_val) <- casesToList xs
              (y_cond, y_val) <- casesToList ys
              let rip_x = rip x_fn i x_val
              pure (sop2Symbol . rip_x $ y_cond :&& x_cond, mapSymSoP rip_x y_val)
          )
subst' x_fn (IndexFn (Forall i dom_x@(Iota _)) xs) (IndexFn (Forall _ dom_y@(Cat k m seg)) ys) = do
  eqEnd1 :: Maybe (Substitution Symbol) <- unify (domainEnd dom_x) (domainEnd dom_y)
  eqEnd2 :: Maybe (Substitution Symbol) <- unify (domainEnd dom_x) (domainEnd (Iota m))
  unless (isJust eqEnd1 || isJust eqEnd2) $ error "subst' iota cat: Incompatible domains."
  pure $
    IndexFn
      (Forall i (Cat k m seg))
      ( cases $ do
          (x_cond, x_val) <- casesToList xs
          (y_cond, y_val) <- casesToList ys
          let rip_x = rip x_fn i x_val
          pure (sop2Symbol . rip_x $ y_cond :&& x_cond, mapSymSoP rip_x y_val)
      )
subst' _ _ _ = undefined

-- TODO Sad that we basically have to copy rep here;
--      everything but the actual substitutions could be delegated to
--      a helper function that takes a replacement as argument?
rip :: VName -> VName -> SoP Symbol -> Symbol -> SoP Symbol
rip fnName fnArg fnVal = apply mempty
  where
    applySoP = mapSymSoP . apply

    apply :: Replacement Symbol -> Symbol -> SoP Symbol
    apply s (Apply (Var f) [idx])
      | f == fnName =
          rep (M.insert fnArg idx s) fnVal
    apply s (Idx (Var f) idx)
      | f == fnName =
          rep (M.insert fnArg idx s) fnVal
    apply s (Var f)
      | f == fnName =
          rep s fnVal
    apply _ x@(Var _) = sym2SoP x
    apply _ x@(Hole _) = sym2SoP x
    apply s (Idx x idx) =
      sym2SoP $ Idx (sop2Symbol $ apply s x) (applySoP s idx)
    apply s (Sum j lb ub x) =
      let s' = addRep j (Var j) s
       in toSumOfSums j (applySoP s' lb) (applySoP s' ub) (apply s' x)
    apply s (Apply f xs) =
      sym2SoP $ Apply (sop2Symbol $ apply s f) (map (applySoP s) xs)
    apply s (Tuple xs) =
      sym2SoP $ Tuple (map (applySoP s) xs)
    apply _ x@(Bool _) = sym2SoP x
    apply _ Recurrence = sym2SoP Recurrence
    apply s sym = case sym of
      Not x -> sym2SoP . neg . sop2Symbol $ apply s x
      x :< y -> binop (:<) x y
      x :<= y -> binop (:<=) x y
      x :> y -> binop (:>) x y
      x :>= y -> binop (:>=) x y
      x :== y -> binop (:==) x y
      x :/= y -> binop (:/=) x y
      x :&& y -> binopS (:&&) x y
      x :|| y -> binopS (:||) x y
      where
        binop op x y = sym2SoP $ applySoP s x `op` applySoP s y
        binopS op x y = sym2SoP $ sop2Symbol (apply s x) `op` sop2Symbol (apply s y)
