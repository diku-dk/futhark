-- Unification of expressions with bound variables.
-- This module lets us answer the question:
--   Are two expressions syntactically identical
--   up to the names of bound variables?
{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Proofs.Unify
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.SoP.SoP (SoP, sopToList, termToList, toTerm, mulSoPs, mapSymSoPM, sopToLists, mapSymSoP, Rel (..), sopFromList, Term, addSoPs, zeroSoP, int2SoP)
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark (VName)
import Futhark.MonadFreshNames (MonadFreshNames (getNameSource), newNameFromString, putNameSource)
import qualified Data.List as L
import Control.Monad (foldM, msum)
import Control.Monad.Trans.Maybe
import Debug.Trace (trace, traceM)
import Futhark.Util.Pretty

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
  pretty = braces . commastack . map (\(k,v) -> pretty k <> " : " <> pretty v) . M.toList

class Replaceable u v where
  -- Implements the replacement operation from Sieg and Kaufmann.
  rep :: Substitution v -> u -> v

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
  rename_ tau = mapSymSoPM (rename_ tau)

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

instance SubstitutionBuilder (SoP u) (SoP u) where
  addSub = M.insert

unifies :: ( Replaceable u (SoP v)
           , Replaceable v (SoP v)
           , Unify u (SoP v) m
           , Unify v (SoP v) m
           , Pretty v
           , Pretty u
           , Show v
           , Ord v) => VName -> [(u, u)] -> MaybeT m (Substitution (SoP v))
unifies _ us | trace ("unifies " <> prettyString us) False = undefined
unifies _ [] = pure mempty
unifies k (u:us) = do
  s0 <- uncurry (unify_ k) u
  foldM (\s (a, b) -> do
          traceM ("# unifies SUB\n" <> prettyString s)
          s' :: Substitution (SoP v) <- unify_ k (rep s a) (rep s b)
          pure $ s <> s'
        ) s0 us

unifyAnyPermutation :: ( Replaceable u (SoP v)
                       , Replaceable v (SoP v)
                       , Unify u (SoP v) m
                       , Unify v (SoP v) m
                       , Pretty v
                       , Pretty u
                       , Show v
                       , Ord v) => VName -> [u] -> [u] -> MaybeT m (Substitution (SoP v))
unifyAnyPermutation k xs ys
  | length xs == length ys =
      first $ map (unifies k . zip xs) (L.permutations ys)
  | otherwise = fail "unifyAnyPermutation unequal lengths"
  where
    -- Extract left-most non-fail action, if there is one.
    first :: Monad m => [MaybeT m a] -> MaybeT m a
    first = msum

instance ( MonadFreshNames m
         , Renameable u
         , Unify u (SoP u) m
         , Replaceable u (SoP u)
         , Show u
         , Pretty u
         , Ord u) => Unify (Term u, Integer) (SoP u) m where
  -- unify_ _ x y | trace ("\nunify_ " <> unwords (map show [x, y])) False = undefined
   -- Unify on permutations of symbols in term.
  unify_ k (x, a) (y, b)
    | a == b = unifyAnyPermutation k (termToList x) (termToList y)
    | otherwise = fail "no unify: unequal constants" -- these dont unify

instance ( MonadFreshNames m
         , Replaceable u (SoP u)
         , Renameable u
         , Unify u (SoP u) m
         , Show u
         , Pretty u
         , Ord u) => Unify (SoP u) (SoP u) m where
  -- unify_ _ x y | trace ("\nunify_ " <> unwords (map show [x, y])) False = undefined
  -- Unify on permutations of terms.
  unify_ k x y = unifyAnyPermutation k (sopToList x) (sopToList y)

instance FreeVariables u => FreeVariables (Rel u) where
  fv rel = case rel of
    x :<: y -> fv x <> fv y
    x :<=: y -> fv x <> fv y
    x :>: y -> fv x <> fv y
    x :>=: y -> fv x <> fv y
    x :==: y -> fv x <> fv y
    x :/=: y -> fv x <> fv y
    x :&&: y -> fv x <> fv y
    x :||: y -> fv x <> fv y

instance (Ord u, Renameable u) => Renameable (Rel u) where
  rename_ tau rel = case rel of
    x :<: y -> f (:<:) x y
    x :<=: y -> f (:<=:) x y
    x :>: y -> f (:>:) x y
    x :>=: y -> f (:>=:) x y
    x :==: y -> f (:==:) x y
    x :/=: y -> f (:/=:) x y
    x :&&: y -> f (:&&:) x y
    x :||: y -> f (:||:) x y
    where
      f op x y = op <$> rename_ tau x <*> rename_ tau y

repRel :: (Ord u, Replaceable u (SoP u)) => Substitution (SoP u) -> Rel u -> Rel u
repRel s rel = case rel of
  x :<: y -> rep s x :<: rep s y
  x :<=: y -> rep s x :<=: rep s y
  x :>: y -> rep s x :>: rep s y
  x :>=: y -> rep s x :>=: rep s y
  x :==: y -> rep s x :==: rep s y
  x :/=: y -> rep s x :/=: rep s y
  x :&&: y -> repRel s x :&&: repRel s y
  x :||: y -> repRel s x :||: repRel s y

-- Please don't read this stupidity.
instance ( MonadFreshNames m
         , Replaceable u (SoP u)
         , Renameable u
         , Unify u (SoP u) m
         , Show u
         , Pretty u
         , Ord u) => Unify (Rel u) (SoP u) m where
  unify_ k = rip
    where
      rip :: Rel u -> Rel u -> MaybeT m (Substitution (SoP u))
      rip (a1 :<: b1) (a2 :<: b2) = rap (a1, a2) (b1, b2)
      rip (a1 :<=: b1) (a2 :<=: b2) = rap (a1, a2) (b1, b2)
      rip (a1 :>: b1) (a2 :>: b2) = rap (a1, a2) (b1, b2)
      rip (a1 :>=: b1) (a2 :>=: b2) = rap (a1, a2) (b1, b2)
      rip (a1 :==: b1) (a2 :==: b2) = rap (a1, a2) (b1, b2)
      rip (a1 :/=: b1) (a2 :/=: b2) = rap (a1, a2) (b1, b2)
      rip (x1 :&&: y1) (x2 :&&: y2) = do
        s :: Substitution (SoP u) <- rip x1 x2
        (s <>) <$> rip (repRel s y1) (repRel s y2)
      rip (x1 :||: y1) (x2 :||: y2) = do
        s :: Substitution (SoP u) <- rip x1 x2
        (s <>) <$> rip (repRel s y1) (repRel s y2)
      rip _ _ = fail "Incompatible"

      rap (x1, y1) (x2, y2) = do
        s :: Substitution (SoP u) <- unify_ k x1 x2
        (s <>) <$> unify_ k (rep s y1) (rep s y2)
