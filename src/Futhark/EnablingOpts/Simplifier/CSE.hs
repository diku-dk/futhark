-- |
--
-- Implementation of common-subexpression elimination for the
-- rebinder.
--
module Futhark.EnablingOpts.Simplifier.CSE
  ( DupeState
  , newDupeState
  , performCSE
  )
  where

import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M

import Futhark.InternalRep
import Futhark.Substitute

mkSubsts :: [Ident] -> [Ident] -> HM.HashMap VName VName
mkSubsts pat vs = HM.fromList $ zip (map identName pat) (map identName vs)

-- | State passed to 'performCSE'.
type DupeState = (M.Map Exp [Ident], HM.HashMap VName VName)

-- | The state that should be passed to the first call to
-- 'performCSE'.
newDupeState :: DupeState
newDupeState = (M.empty, HM.empty)

-- | The main CSE function.  Given a state, a pattern and an
-- expression to be bound to that pattern, return a replacement set of
-- bindings and a new state.  The function will look in the state to
-- determine whether the expression can be replaced with something
-- bound previously.
performCSE :: DupeState -> [Ident] -> Exp
           -> ([Binding], DupeState)
-- Arrays may be consumed, so don't eliminate expressions producing
-- unique arrays.  This is perhaps a bit too conservative - we could
-- track exactly which are being consumed and keep a blacklist.
performCSE (esubsts, nsubsts) pat e
  | any (getAll . uniqArray) pat =
    ([Let pat e], (esubsts, nsubsts))
  where uniqArray = All . not . basicType . identType <>
                    All . unique . identType
performCSE (esubsts, nsubsts) pat e =
  case M.lookup e' esubsts of
    Just vs -> (substPat vs, (esubsts, mkSubsts pat vs `HM.union` nsubsts))
    Nothing -> ([Let pat e'], (M.insert e' pat esubsts, nsubsts))
  where e' = substituteNames nsubsts e
        substPat vs = [ Let [p] (SubExp $ Var v) | (p,v) <- zip pat vs ]
