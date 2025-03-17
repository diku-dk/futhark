-- Unification of expressions with bound variables.
-- This module lets us answer the question:
--   Are two expressions syntactically identical
--   up to the names of bound variables?
{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Properties.Unify
  ( Unify (..),
    Renameable (..),
    Replacement,
    ReplacementBuilder (..),
    Rep (..),
    Substitution (..),
    FreeVariables (..),
    Hole (justHole),
    freshName,
    freshNameFromString,
    renameSame,
    unifies_,
    sub,
    unifies,
    renameAnd,
    renameM,
    renamesM,
    repTuple,
    repPredicate,
  )
where

import Control.Monad (foldM, msum)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Futhark.Analysis.Properties.Monad (IndexFnM)
import Futhark.Analysis.Properties.Property (Predicate (..), Property (..))
import Futhark.Analysis.Properties.Util (prettyName)
import Futhark.FreshNames qualified as FreshNames
import Futhark.MonadFreshNames (MonadFreshNames (getNameSource), VNameSource, newNameFromString)
import Futhark.SoP.SoP (SoP, Term, int2SoP, justSym, mapSymM, mulSoPs, sopToList, sopToLists, term2SoP, termToList, toTerm)
import Futhark.SoP.SoP qualified as SoP
import Futhark.Util.Pretty (Pretty (pretty), braces, commastack)
import Language.Futhark (VName)

{-
              Utilities
-}
class FreeVariables a where
  fv :: a -> S.Set VName

instance FreeVariables VName where
  fv = S.singleton

instance (Ord u, FreeVariables u) => FreeVariables (SoP u) where
  fv x = S.unions [fv t | (ts, _) <- sopToLists x, t <- ts]

class Hole u where
  justHole :: u -> Maybe VName

instance (Hole u, Ord u) => Hole (SoP u) where
  justHole sop = justSym sop >>= justHole

{-
              Rename
-}
class Renameable u where
  -- Implements subC(id,tau,e) from Sieg and Kaufmann where
  -- tau is a renaming of bound variables. The context C is given by
  -- VNameSource in MonadFreshNames.
  rename_ :: (MonadFreshNames m) => VNameSource -> M.Map VName VName -> u -> m u

  -- Rename bound variables in u. Equivalent to subC(id,id,e).
  rename :: (MonadFreshNames m) => VNameSource -> u -> m u
  rename vns = rename_ vns mempty

instance (Renameable a, Renameable b) => Renameable (a, b) where
  rename_ vns tau (x, y) = (,) <$> rename_ vns tau x <*> rename_ vns tau y

instance (Renameable u) => Renameable [u] where
  rename_ vns tau = mapM (rename_ vns tau)


instance (Renameable u) => Renameable (Maybe u) where
  rename_ vns tau (Just x) = Just <$> rename_ vns tau x
  rename_ _ _ Nothing = pure Nothing

instance Renameable VName where
  rename_ _ tau x = pure $ M.findWithDefault x x tau

instance Renameable Integer where
  rename_ _ _ = pure

instance (Renameable u, Ord u) => Renameable (Term u) where
  rename_ vns tau x = toTerm <$> mapM (rename_ vns tau) (termToList x)

instance (Ord u, Renameable u) => Renameable (SoP u) where
  rename_ vns tau = mapSymM (rename_ vns tau)

renameM :: (MonadFreshNames m, Renameable u) => u -> m u
renameM x = getNameSource >>= flip rename x

-- Renames any number of renameables using the same name source for each.
renamesM :: (MonadFreshNames m, Traversable t, Renameable b) => t b -> m (t b)
renamesM xs = getNameSource >>= \vns -> mapM (rename vns) xs

-- Rename bound variables in `a` and `b`. Renamed variables are
-- identical, if `a` and `b` are syntactically equivalent.
renameSame :: (MonadFreshNames m, Renameable a, Renameable b) => a -> b -> m (a, b)
renameSame a b = do
  vns <- getNameSource
  a' <- rename vns a
  b' <- rename vns b
  pure (a', b')

-- | Produce a fresh name.
-- Like "FreshNames.newName", except it lets us reuse the same name
-- source in multiple places while ensuring that the monadic name source
-- continues to generate unique names afterwards.
freshName :: (MonadFreshNames m) => VNameSource -> m (VName, VNameSource)
freshName vns =
  -- All renamed names must have the same base name. Otherwise renaming two
  -- expressions with differently named bound variables will not produce
  -- identical renamings.
  freshNameFromString vns "j"

freshNameFromString :: (MonadFreshNames m) => VNameSource -> String -> m (VName, VNameSource)
freshNameFromString vns s = do
  x <- newNameFromString s
  -- Note that we are unnecessarily incrementing the monadic name source above.
  -- The monadic name source needs only be updated to the maximum tag over all
  -- places where freshName is used on successions of vns.
  let (j, vns') = FreshNames.newName vns x
  pure (j, vns')

{-
              Replace
-}
type Replacement u = M.Map VName (SoP u)

class Rep v u where
  -- Implements the replacement operation from Sieg and Kaufmann.
  rep :: Replacement u -> v -> SoP u

  repSelf :: Replacement u -> v -> v

class ReplacementBuilder v u where
  addRep :: VName -> v -> Replacement u -> Replacement u

  mkRep :: VName -> v -> Replacement u
  mkRep vn x = addRep vn x mempty

  mkRepFromList :: [(VName, v)] -> Replacement u
  mkRepFromList = foldl (\s (k,v) -> addRep k v s) mempty

instance (Ord u, Rep u u) => Rep (Term u, Integer) u where
  rep s (x, c) =
    SoP.scaleSoP c . foldr (mulSoPs . rep s) (int2SoP 1) . termToList $ x

  repSelf = undefined

instance (Ord u, Rep u u) => Rep (SoP u) u where
  rep s = SoP.mapSymSoP (rep s)

  repSelf = rep

instance (Ord u, Hole u) => ReplacementBuilder (SoP u) u where
  addRep _ x _
    | hasHole =
        error "Creating substitution for SoP with Hole. Are you trying to unify a Hole?"
    where
      hasHole = any (any (isJust . justHole) . fst) (sopToLists x)
  addRep vn x s = M.insert vn x s

{-
              Substitute
-}
-- Like replace, but rename beforehand.
sub :: (MonadFreshNames m, Renameable v, Rep v u) => Substitution u -> v -> m (SoP u)
sub s x = rep (mapping s) <$> rename (vns s) x

data Substitution u = Substitution
  { mapping :: Replacement u,
    vns :: VNameSource
  }

instance (Show u) => Show (Substitution u) where
  show = show . mapping

instance (Eq u, Ord u) => Eq (Substitution u) where
  a == b = mapping a == mapping b

instance Semigroup (Substitution u) where
  a <> b = Substitution {mapping = mapping a <> mapping b, vns = vns a <> vns b}

instance (Pretty v) => Pretty (Replacement v) where
  pretty = braces . commastack . map prettyKV . M.toList
    where
      prettyKV (k, v) = prettyName k <> " : " <> pretty v

instance (Pretty v) => Pretty (Substitution v) where
  pretty = pretty . mapping

{-
              Unify
-}
class (Renameable v) => Unify v u where
  -- `unify_ k eq` is the unification algorithm from Sieg and Kauffmann,
  -- Unification for quantified formulae, 1993.
  -- Check whether x is a bound variable by `x >= k`.
  unify_ :: VName -> v -> v -> MaybeT IndexFnM (Replacement u)

  -- Unification on {subC(id,id,e) ~= subC(id,id,e')}
  --                  = {rename(e) ~= rename(e')}.
  --
  -- Only the first argument may contain holes.
  unify :: (Pretty v) => v -> v -> IndexFnM (Maybe (Substitution u))
  unify = renameAnd unify_

renameAnd ::
  (Renameable t1, Renameable t2) =>
  (VName -> t1 -> t2 -> MaybeT IndexFnM (Replacement u)) ->
  t1 ->
  t2 ->
  IndexFnM (Maybe (Substitution u))
renameAnd unifier x y = runMaybeT $ do
  k <- newNameFromString "k"
  vns <- getNameSource
  a <- rename vns x
  b <- rename vns y
  s <- unifier k a b
  pure $ Substitution {mapping = s, vns = vns}

unifies_ ::
  ( Rep v u,
    Rep u u,
    Unify v u,
    Unify u u,
    Ord u,
    Hole u
  ) =>
  VName ->
  [v] ->
  [v] ->
  MaybeT IndexFnM (Replacement u)
unifies_ k xs ys
  | length xs == length ys = do
      go (zip xs ys)
  | otherwise = fail "different lengths"
  where
    go [] = pure mempty
    go (u : us) = do
      s0 <- uncurry (unify_ k) u
      foldM (\s (a, b) -> (s <>) <$> unify_ k (rep s a) (rep s b)) s0 us

unifies ::
  ( Rep v u,
    Rep u u,
    Unify v u,
    Unify u u,
    Ord u,
    Hole u
  ) =>
  [v] ->
  [v] ->
  IndexFnM (Maybe (Substitution u))
unifies = renameAnd unifies_

unifyAnyPerm ::
  ( Rep v u,
    Rep u u,
    Unify v u,
    Unify u u,
    Ord u,
    Hole u
  ) =>
  VName ->
  [v] ->
  [v] ->
  MaybeT IndexFnM (Replacement u)
unifyAnyPerm k xs ys
  | length xs == length ys =
      -- Extract left-most non-fail action, if there is one.
      msum $ map (unifies_ k xs) (L.permutations ys)
  | otherwise = fail "unifyAnyPerm unequal lengths"

instance (Rep u u, Unify u u, Hole u, Ord u) => Unify (Term u, Integer) u where
  -- Unify on permutations of symbols in term.
  unify_ k (xs, a) (ys, b)
    | Just h <- justSym (term2SoP xs a) >>= justHole =
        pure $ addRep h (term2SoP ys b) mempty
    -- Attempt to match hole to multiplicative constant b, if necessary.
    | a == 1,
      x' : xs' <- termToList xs,
      Just h <- justHole x',
      not (null ys) =
        msum
          [ unifyAnyPerm k (termToList xs) (termToList ys),
            do
              -- lift $ debugM $ "unify x = " <> prettyString xs <> "   y =" <> prettyString ys
              let s = addRep h (int2SoP b :: SoP u) mempty
              let x = rep s (term2SoP xs' 1)
              let y = rep s (term2SoP ys 1)
              (s <>) <$> unify_ k x y
          ]
    | a == b = unifyAnyPerm k (termToList xs) (termToList ys)
    | otherwise = fail "Unable to unify constants."

instance (Rep u u, Unify u u, Ord u, Hole u) => Unify (SoP u) u where
  -- Unify on permutations of terms.
  unify_ k x y
    | Just h <- justHole x = pure $ addRep h y mempty
    | otherwise = unifyAnyPerm k (sopToList x) (sopToList y)

instance (Ord a, Renameable a) => Renameable (Predicate a) where
  rename_ vns tau (Predicate xn e) = do
    (xm, vns') <- freshName vns
    let tau' = M.insert xn xm tau
    Predicate xm <$> rename_ vns' tau' e

instance (Ord a, Renameable a) => Renameable (Property a) where
  rename_ _ _ Boolean = pure Boolean
  rename_ vns tau (Disjoint s) =
    Disjoint . S.fromList <$> mapM (rename_ vns tau) (S.toList s)
  rename_ _ _ p@(Monotonic {}) = pure p
  rename_ vns tau (Injective x rcd) =
    Injective x <$> rename_ vns tau rcd
  rename_ vns tau (BijectiveRCD x rcd img) =
    BijectiveRCD x <$> rename_ vns tau rcd <*> rename_ vns tau img
  rename_ vns tau (FiltPartInv x pf pps) =
    FiltPartInv x <$> rename_ vns tau pf <*> mapM (rename_ vns tau) pps
  rename_ vns tau (FiltPart x y pf pps) =
    FiltPart x y <$> rename_ vns tau pf <*> mapM (rename_ vns tau) pps

instance Unify VName a where
  unify_ _ x y
    | x == y = pure mempty
    | otherwise = fail "no unify"

repTuple :: (Rep v1 u, Rep v2 u) => Replacement u -> (v1, v2) -> (SoP u, SoP u)
repTuple s (a, b) = do
  (rep s a, rep s b)

unifyTuple :: (Unify v1 u, Unify u u, Ord u, Hole u, Rep u u, Rep v2 u, Rep v3 u) => VName -> (v1, v2) -> (v1, v3) -> MaybeT IndexFnM (Replacement u)
unifyTuple k (a, b) (a', b') = do
  s <- unify_ k a a'
  (s <>) <$> unify_ k (rep s b) (rep s b')

repPredicate :: (Rep u u) =>Replacement u -> Predicate u -> Predicate u
repPredicate s (Predicate vn e) =
  let s' = M.delete vn s
   in Predicate vn (repSelf s' e)

instance (Ord a, Renameable a, Rep a a, Unify a a, Hole a) => Unify (Predicate a) a where
  unify_ k (Predicate _ e_x) (Predicate _ e_y) = do
    unify_ k e_x e_y

instance (Ord a, Renameable a, Rep a a, Unify a a, Hole a) => Unify (Property a) a where
  unify_ _ Boolean Boolean = pure mempty
  unify_ _ (Disjoint x) (Disjoint y) | x == y = pure mempty
  unify_ _ x@(Monotonic {}) y@(Monotonic {}) | x == y = pure mempty
  unify_ k (Injective x (Just rcd1)) (Injective y (Just rcd2)) = do
    s <- unify_ k x y
    (s <>) <$> unifyTuple k (repTuple s rcd1) (repTuple s rcd2)
  unify_ k (Injective x Nothing) (Injective y Nothing) =
    unify_ k x y
  unify_ k (BijectiveRCD x rcd1 img1) (BijectiveRCD y rcd2 img2) = do
    s1 <- unify_ k x y
    s2 <- unifyTuple k (repTuple s1 rcd1) (repTuple s1 rcd2)
    s3 <- unifyTuple k (repTuple (s1 <> s2) img1) (repTuple (s1 <> s2) img2)
    pure (s1 <> s2 <> s3)
  unify_ k (FiltPartInv x pf1 pps1) (FiltPartInv y pf2 pps2) = do
    s1 <- unify_ k x y
    let (pp1, splits1) = unzip pps1
    let (pp2, splits2) = unzip pps2
    s2 <- unifies_ k (map (rep s1) splits1) (map (rep s1) splits2)
    let s' = s1 <> s2
    (s' <>) <$> unifiesPredicates (zip (map (repPredicate s') $ pf1 : pp1) (map (repPredicate s') $ pf2 : pp2))
    where
      unifiesPredicates [] = pure mempty
      unifiesPredicates (u : us) = do
        s0 <- uncurry (unify_ k) u
        foldM (\s (a, b) -> (s <>) <$> unify_ k (repPredicate s a) (repPredicate s b)) s0 us
  unify_ _ _ _ = fail "no unify"

instance (FreeVariables u, FreeVariables v) => FreeVariables (u, v) where
  fv (a, b) = fv a <> fv b

instance (FreeVariables u) => FreeVariables (Predicate u) where
  fv (Predicate vn e) = fv vn <> fv e

instance (FreeVariables u, Ord u) => FreeVariables (Property u) where
  fv Boolean = mempty
  fv (Disjoint x) = x
  fv Monotonic {} = mempty
  fv (Injective x (Just rcd)) = fv x <> fv rcd
  fv (Injective x Nothing) = fv x
  fv (BijectiveRCD x rcd img) = fv x <> fv rcd <> fv img
  fv (FiltPartInv x pf pps) = fv x <> fv pf <> S.unions (map fv pps)
  fv (FiltPart x y pf pps) = fv x <> fv y <> fv pf <> S.unions (map fv pps)
