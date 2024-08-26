-- Unification of expressions with bound variables.
-- This module lets us answer the question:
--   Are two expressions syntactically identical
--   up to the names of bound variables?
module Futhark.Analysis.Proofs.Unify
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.SoP.SoP (SoP, sopToList, termToList, toTerm, mulSoPs, mapSymSoPM, sopToLists, mapSymSoP)
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark (VName)
import Futhark.MonadFreshNames (VNameSource, MonadFreshNames (getNameSource), newNameFromString, putNameSource)
import qualified Data.List as L
import Control.Monad (foldM)
import Debug.Trace (trace)

class Ord a => FreeVariables a where
  fv :: a -> S.Set VName

class Nameable u where
  mkName :: VNameSource -> (u, VNameSource)

class Renameable u where
  -- Implements subC(id,tau,e) from Sieg and Kaufmann where
  -- tau is a renaming of bound variables. The context C is given by 
  -- VNameSource in MonadFreshNames.
  rename_ :: MonadFreshNames m => M.Map VName VName -> u -> m u
  -- Rename bound variables in u. Equivalent to subC(id,id,e).
  rename :: MonadFreshNames m => u -> m u
  rename = rename_ mempty

type Substitution v = M.Map VName v

class Replaceable u v where
  -- Implements the replacement operation from Sieg and Kaufmann.
  rep :: Substitution v -> u -> v

class SubstitutionBuilder u v where
  addSub :: VName -> u -> Substitution v -> Substitution v

class (MonadFreshNames m, MonadFail m, Renameable u) => Unify u v m where
  -- `unify_ k eq` is the unification algorithm from Sieg and Kauffmann,
  -- Unification for quantified formulae, 1993.
  -- Check whether x is a bound variable by `x >= k`.
  unify_ :: VName -> u -> u -> m (Substitution v)
  unify :: u -> u -> m (Substitution v)
  unify e e' = do
    -- Unification on {subC(id,id,e) ~= subC(id,id,e')}
    --                  = {rename(e) ~= rename(e')}.
    k <- newNameFromString "k"
    c <- getNameSource
    a <- rename e
    putNameSource c
    b <- rename e'
    unify_ k a b

instance Renameable VName where
  rename_ tau x = pure $ M.findWithDefault x x tau

instance FreeVariables VName where
  fv = S.singleton

-- instance FreeVariables u => FreeVariables (SoP.Term u) where
--   fv = S.unions . map fv . termToList

instance FreeVariables u => FreeVariables (SoP u) where
  fv x = S.unions [fv t | (ts, _) <- sopToLists x, t <- ts]

instance (Ord u, Renameable u) => Renameable (SoP u) where
  rename_ tau = mapSymSoPM (rename_ tau)

instance (Ord u, Replaceable u (SoP u)) => Replaceable (SoP u) (SoP u) where
  rep s = mapSymSoP (rep s)

instance SubstitutionBuilder (SoP u) (SoP u) where
  addSub = M.insert

unifies :: ( Replaceable u (SoP v)
           , Replaceable v (SoP v)
           , Unify u (SoP v) m
           , Unify v (SoP v) m
           , Show v
           , Ord v) => VName -> [(u, u)] -> m (Substitution (SoP v))
unifies _ [] = pure mempty
unifies k (u:us) = do
  s0 <- uncurry (unify_ k) u
  foldM (\s (a, b) -> do
          s' :: Substitution (SoP v) <- unify_ k (rep s a) (rep s b)
          pure $ s <> s'
        ) s0 us

instance (Ord u, Replaceable u (SoP u)) => Replaceable (SoP.Term u) (SoP u) where
  rep s = foldr1 mulSoPs . map (rep s) . termToList

instance (Renameable u, Ord u) => Renameable (SoP.Term u) where
  rename_ tau x = toTerm <$> mapM (rename_ tau) (termToList x)

-- TODO recast permuted terms to SoP and remove this instance?
instance ( MonadFail m
         , MonadFreshNames m
         , Renameable u
         , Ord u
         , Show u
         , Unify u (SoP u) m
         , Replaceable u (SoP u)
         ) => Unify (SoP.Term u) (SoP u) m where
  unify_ _ x y | trace ("\nunify_ " <> unwords (map show [x, y])) False = undefined
  unify_ k x y
    | length xs == 1, length ys == 1 = unify_ k (head xs) (head ys)
    | length xs == length ys =
        -- Unify on permutation of symbols in a term.
        unifies k $ concatMap (`zip` ys) (L.permutations xs)
    | otherwise = pure mempty
    where
      xs = termToList x
      ys = termToList y

instance ( MonadFail m
         , MonadFreshNames m
         , Replaceable u (SoP u)
         , Renameable u
         , Show u
         , Unify u (SoP u) m
         , Ord u) => Unify (SoP u) (SoP u) m where
  unify_ _ x y | trace ("\nunify_ " <> unwords (map show [x, y])) False = undefined
  unify_ k x y
    | length xs == length ys =
        -- Unify on permutations of terms.
        -- TODO will filtering on constants work out here? I'm thinking the
        -- substitutions wouldn't contain that information anyway.
        -- unifies k [(term2SoP a ca, term2SoP b cb) | xs' <- L.permutations xs,
        unifies k [(a, b) | xs' <- L.permutations xs,
                            ((a, ca), (b, cb)) <- zip xs' ys,
                            ca == cb]
    | otherwise = pure mempty
    where
      xs = sopToList x
      ys = sopToList y
