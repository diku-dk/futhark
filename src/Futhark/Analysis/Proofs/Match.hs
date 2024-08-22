-- Unification of expressions with bound variables.
-- This module lets us answer the question:
--   Are two expressions syntactically identical
--   up to the names of bound variables?
module Futhark.Analysis.Proofs.Match
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.SoP.SoP (SoP, Substitute(..))
import Language.Futhark (VName)
import Futhark.MonadFreshNames (VNameSource, MonadFreshNames (getNameSource), newNameFromString, putNameSource)

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

class Replaceable u where
  -- Implements the replacement operation from Sieg and Kaufmann.
  rep :: Substitution u -> u -> u

instance Substitute a b (Constraint u) where
  -- TODO implement rep from Sieg and Kaufmann.
  -- replace _ _ = undefined
  -- Lemma 1.1: subC(sigma,tau,e) = rep(sigma,subC(id,tau,e))
  -- Then implement substitute as subC(sigma, id, e) using Lemma 1.1:
  --   subC(sigma, id, e) = rep(sigma,subC(id,id,e)) = rep(sigma,rename(e))
  substitute _ _ = undefined

type Substitution u = M.Map VName (SoP u)
-- data Substitution u v = Substitution
--   { usub :: M.Map VName u,
--     vsub :: M.Map VName v
--   }

data Constraint u = u := u
infixr 4 :=

class (MonadFreshNames m, MonadFail m, Renameable u, FreeVariables u) => Unify u v m where
  -- `unify_ k eq` is the unification algorithm from Sieg and Kauffmann,
  -- Unification for quantified formulae, 1993.
  -- Check whether x is a bound variable by `x >= k`.
  unify_ :: VName -> Constraint u -> m (Substitution v)
  unify :: u -> u -> m (Substitution v)
  unify e e' = do
    -- Unification on {subC(id,id,e) ~= subC(id,id,e')}
    --                  = {rename(e) ~= rename(e')}.
    k <- newNameFromString "k"
    c <- getNameSource
    a <- rename e
    putNameSource c
    b <- rename e'
    unify_ k (a := b)

instance Ord u => FreeVariables (SoP u) where
  fv _ = undefined
  
instance Renameable (SoP u) where
  rename_ _tau _u = undefined

instance Unify u u m => Unify (SoP u) u m where
  unify_ _ (_sop1 := _sop2) = undefined
  -- Unify sop1 with k terms with all k-combinations of terms in sop2?
