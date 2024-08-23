-- Unification of expressions with bound variables.
-- This module lets us answer the question:
--   Are two expressions syntactically identical
--   up to the names of bound variables?
module Futhark.Analysis.Proofs.Unify
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.SoP.SoP (SoP, sopToList)
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark (VName)
import Futhark.MonadFreshNames (VNameSource, MonadFreshNames (getNameSource), newNameFromString, putNameSource)
import qualified Data.List as L
import Data.Bifunctor (bimap)
import Control.Monad (foldM)

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

instance Ord u => FreeVariables (SoP u) where
  fv _ = undefined

instance Renameable (SoP u) where
  rename_ _tau _u = undefined

instance Replaceable (SoP u) (SoP u) where
  rep _ _ = undefined

instance SubstitutionBuilder (SoP u) (SoP u) where
  addSub = M.insert

unifies :: (MonadFreshNames m, MonadFail m, Replaceable u (SoP u)) => VName -> [(u, u)] -> m (Substitution (SoP u))
unifies _ [] = pure mempty
unifies k us = do
  foldM (\s (a, b) -> do
          s' <- unify_ k (rep s a) (rep s b)
          pure $ s <> s'
        ) mempty us

instance (MonadFail m, MonadFreshNames m, Replaceable u (SoP u)) => Unify (SoP u) (SoP u) m where
  unify_ k a b
    | length ts1 == length ts2 =
        let terms = do
              ts1_perm <- L.permutations ts1
              zip ts1_perm ts2
            subs = unifySoPTerms terms
        in fmap head subs
    | otherwise = pure mempty
    where
      ts1 = sopToList a
      ts2 = sopToList b

      unifySoPTerms ::
        [((SoP.Term u, Integer), (SoP.Term u, Integer))] ->
        m [Substitution (SoP u)]
      unifySoPTerms [] = pure mempty
      unifySoPTerms ((t1, t2):ts) = do
        s <- unifyOneSoPTerm t1 t2
        undefined

      unifyOneSoPTerm ::
        (SoP.Term u, Integer) ->
        (SoP.Term u, Integer) ->
        m [Substitution (SoP u)]
      unifyOneSoPTerm (t1, c1) (t2, c2)
        | length xs == length ys,
          c1 == c2 = undefined
            -- let xs'' = do
            --   xs' <- L.permutations xs
            --   zip xs' ys
            -- unifies $ 
        | otherwise = pure mempty
        where
          xs = SoP.termToList t1
          ys = SoP.termToList t2

  -- unifyOneSoP x y
  --   | length xs == length ys = do
  --       xs' <- L.permutations xs
  --       noSubProblems $
  --         unifySoPTerms $
  --           zip xs' ys
  --   | otherwise = mempty
  --   where
  --     xs = SoP.sopToList x
  --     ys = SoP.sopToList y
  --     unifySoPTerms :: [((SoP.Term Term, Integer), (SoP.Term Term, Integer))] -> [Subst]
  --     unifySoPTerms [] = pure mempty
  --     unifySoPTerms ((t1, t2) : es) = do
  --       s <- unifyOneSoPTerm t1 t2
  --       let es' =
  --             map
  --               ( \((t1', a), (t2', b)) ->
  --                   ((replace s t1', a), (replace s t2', b))
  --               )
  --               es
  --       fmap (s <>) $ unifySoPTerms es'


-- Unify sop1 with k terms with all k-combinations of terms in sop2?
