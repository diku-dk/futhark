-- Unification of expressions with bound variables.
-- This module lets us answer the question:
--   Are two expressions syntactically identical
--   up to the names of bound variables?
module Futhark.Analysis.Proofs.Match
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Language.Futhark qualified as E
import Futhark.SoP.SoP (SoP, sym2SoP, Substitute(..))
import Language.Futhark (VName)

class Ord a => FreeIn a where
  freeIn :: a -> S.Set VName

class Renameable u where
  -- Rename bound variables in u.
  rename :: u -> u -- Implement subC(id,id,u) from Sieg and Kaufmann.

type Substitution u = M.Map VName (SoP u)

data Constraint u = u := u
infixr 4 :=

instance Substitute a b (Constraint u) where
  substitute _ _ = undefined

-- TODO derive MonadFail/Maybe rather than returning Maybes?
class (Renameable u, FreeIn u) => Unify u v where
  -- (One step of?) the unification algorithm from Sieg and Kauffmann,
  -- Unification for quantified formulae, 1993.
  unify_ :: Int -> Constraint u -> Maybe (Substitution v)
  unify :: Int -> u -> u -> Maybe (Substitution v)
  unify k a b = unify_ k (rename a := rename b)

isBoundVar :: Int -> VName -> Bool
isBoundVar k vn = E.baseTag vn >= k

instance Ord u => FreeIn (SoP u) where
  freeIn _ = undefined
  
instance Renameable (SoP u) where
  rename _ = undefined

instance Unify u u => Unify (SoP u) u where
  unify_ _ (_sop1 := _sop2) = undefined
  -- Unify sop1 with k terms with all k-combinations of terms in sop2?
