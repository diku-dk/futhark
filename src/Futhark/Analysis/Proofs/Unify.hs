-- Unification of expressions with bound variables.
-- This module lets us answer the question:
--   Are two expressions syntactically identical
--   up to the names of bound variables?
{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Proofs.Unify
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.SoP.SoP (SoP, sopToList, termToList, toTerm, mulSoPs, sopToLists, Term, addSoPs, zeroSoP, int2SoP, sopFromList, justSym)
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark (VName)
import Futhark.MonadFreshNames (MonadFreshNames (getNameSource), newNameFromString, putNameSource)
import qualified Data.List as L
import Control.Monad (foldM, msum)
import Control.Monad.Trans.Maybe
import Futhark.Util.Pretty
import Futhark.Analysis.Proofs.Util (prettyName)

class Ord a => FreeVariables a where
  fv :: a -> S.Set VName

class Renameable u where
  -- Implements subC(id,tau,e) from Sieg and Kaufmann where
  -- tau is a renaming of bound variables. The context C is given by 
  -- VNameSource in MonadFreshNames.
  rename_ :: MonadFreshNames m => M.Map VName VName -> u -> m u
  -- Rename bound variables in u. Equivalent to subC(id,id,e).
  rename :: MonadFreshNames m => u -> m u
  rename = rename_ mempty

type Substitution v = M.Map VName v

instance Pretty v => Pretty (Substitution v) where
  pretty = braces . commastack . map (\(k,v) -> prettyName k <> " : " <> pretty v) . M.toList

class Replaceable u v where
  -- Implements the replacement operation from Sieg and Kaufmann.
  rep :: Substitution v -> u -> v

sub :: ( MonadFreshNames m
       , Renameable u
       , Replaceable u v) => Substitution v -> u -> m v
sub s x = rep s <$> rename x

class SubstitutionBuilder u v where
  addSub :: VName -> u -> Substitution v -> Substitution v

class (MonadFreshNames m, Renameable u) => Unify u v m where
  -- `unify_ k eq` is the unification algorithm from Sieg and Kauffmann,
  -- Unification for quantified formulae, 1993.
  -- Check whether x is a bound variable by `x >= k`.
  unify_ :: VName -> u -> u -> MaybeT m (Substitution v)
  unify :: u -> u -> m (Maybe (Substitution v))
  unify e e' = runMaybeT $ do
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

instance (Renameable u, Ord u) => Renameable (Term u, Integer) where
  rename_ tau (x, c) = (, c) . toTerm <$> mapM (rename_ tau) (termToList x)

instance (Ord u, Renameable u) => Renameable (SoP u) where
  rename_ tau = fmap sopFromList . mapM (rename_ tau) . sopToList

instance FreeVariables VName where
  fv = S.singleton

-- instance FreeVariables u => FreeVariables (Term u) where
--   fv = S.unions . map fv . termToList

instance FreeVariables u => FreeVariables (SoP u) where
  fv x = S.unions [fv t | (ts, _) <- sopToLists x, t <- ts]

instance (Ord u, Replaceable u (SoP u)) => Replaceable (Term u, Integer) (SoP u) where
  rep s (x, c) = SoP.scaleSoP c $ foldr (mulSoPs . rep s) (int2SoP 1) . termToList $ x

instance (Ord u, Replaceable u (SoP u)) => Replaceable (SoP u) (SoP u) where
  rep s = foldr (addSoPs . rep s) zeroSoP . sopToList

unifies_ :: ( Replaceable u (SoP v)
           , Replaceable v (SoP v)
           , Unify u (SoP v) m
           , Unify v (SoP v) m
           , Hole v
           , Ord v) => VName -> [(u, u)] -> MaybeT m (Substitution (SoP v))
unifies_ _ [] = pure mempty
unifies_ k (u:us) = do
  s0 <- uncurry (unify_ k) u
  foldM (\s (a, b) -> (s <>) <$> unify_ k (rep s a) (rep s b)) s0 us

unifies :: ( Replaceable u (SoP v)
           , Replaceable v (SoP v)
           , Unify u (SoP v) m
           , Unify v (SoP v) m
           , Hole v
           , Ord v) => [(u, u)] -> m (Maybe (Substitution (SoP v)))
unifies us = runMaybeT $ do
  let (as, bs) = unzip us
  k <- newNameFromString "k"
  c <- getNameSource
  as' <- mapM rename as
  putNameSource c
  bs' <- mapM rename bs
  unifies_ k (zip as' bs')

unifyAnyPerm :: ( Replaceable u (SoP v)
                , Replaceable v (SoP v)
                , Unify u (SoP v) m
                , Unify v (SoP v) m
                , Hole v
                , Ord v) => VName -> [u] -> [u] -> MaybeT m (Substitution (SoP v))
unifyAnyPerm k xs ys
  | length xs == length ys =
      -- Extract left-most non-fail action, if there is one.
      msum $ map (unifies_ k . zip xs) (L.permutations ys)
  | otherwise = fail "unifyAnyPerm unequal lengths"

instance ( MonadFreshNames m
         , Renameable u
         , Unify u (SoP u) m
         , Replaceable u (SoP u)
         , Hole u
         , Ord u) => Unify (Term u, Integer) (SoP u) m where
  -- Unify on permutations of symbols in term.
  unify_ k (x, a) (y, b)
    | a == b = unifyAnyPerm k (termToList x) (termToList y)
    | otherwise = fail "unequal constants"

class Hole u where
  justHole :: u -> Maybe VName

instance ( MonadFreshNames m
         , Replaceable u (SoP u)
         , Renameable u
         , Unify u (SoP u) m
         , Hole u
         , Ord u) => Unify (SoP u) (SoP u) m where
  -- Unify on permutations of terms.
  unify_ k x y
    | Just h <- justSym x >>= justHole = pure $ M.singleton h y
    | otherwise = unifyAnyPerm k (sopToList x) (sopToList y)
