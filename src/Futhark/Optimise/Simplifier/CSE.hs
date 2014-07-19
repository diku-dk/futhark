-- |
--
-- Implementation of common-subexpression elimination for the
-- rebinder.
--
module Futhark.Optimise.Simplifier.CSE
  ( DupeState
  , newDupeState
  , performCSE
  )
  where

import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Lazy as M

import Futhark.Representation.Basic
import Futhark.Substitute
import Futhark.Optimise.Simplifier.TaggedBinding
import qualified Futhark.Analysis.UsageTable as UT

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
performCSE :: Substitute a =>
              DupeState -> TaggedBinding a
           -> ([TaggedBinding a], DupeState)
-- Arrays may be consumed, so don't eliminate expressions producing
-- unique arrays.  This is perhaps a bit too conservative - we could
-- track exactly which are being consumed and keep a blacklist.
performCSE (esubsts, nsubsts) bnd@(TaggedLet (pat,_) _)
  | any (getAll . uniqArray) pat =
    ([bnd], (esubsts, nsubsts))
  where uniqArray = All . not . basicType . identType <>
                    All . unique . identType
performCSE (esubsts, nsubsts) (TaggedLet (pat,patdata) (e,enames)) =
  case M.lookup e' esubsts of
    Just vs -> (substLets vs, (esubsts, mkSubsts pat vs `HM.union` nsubsts))
    Nothing -> ([TaggedLet (pat,patdata') (e',enames')],
                (M.insert e' pat esubsts, nsubsts))
  where e' = substituteNames nsubsts e
        enames' = substituteNames nsubsts enames
        patdata' = substituteNames nsubsts patdata
        substLets vs = [ TaggedLet ([p],[d])
                                   (SubExp $ Var v,
                                    UT.usages $ HS.singleton $ identName v) |
                         (p,d,v) <- zip3 pat patdata vs ]
