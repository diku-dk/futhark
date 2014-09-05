{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Lazy as M

import Futhark.Representation.AST
import Futhark.Substitute
import Futhark.Optimise.Simplifier.TaggedBinding
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Binder

mkSubsts :: Pattern lore -> Pattern lore -> HM.HashMap VName VName
mkSubsts pat vs = HM.fromList $ zip (patternNames pat) (patternNames vs)

-- | State passed to 'performCSE'.
type DupeState lore =
  (M.Map (Exp lore) (Pattern lore), HM.HashMap VName VName)

-- | The state that should be passed to the first call to
-- 'performCSE'.
newDupeState :: DupeState lore
newDupeState = (M.empty, HM.empty)

-- | The main CSE function.  Given a state, a pattern and an
-- expression to be bound to that pattern, return a replacement set of
-- bindings and a new state.  The function will look in the state to
-- determine whether the expression can be replaced with something
-- bound previously.
performCSE :: (Proper (Lore m), BindableM m) =>
              DupeState (Lore m) -> TaggedBinding (Lore m)
           -> m ([TaggedBinding (Lore m)], DupeState (Lore m))
-- Arrays may be consumed, so don't eliminate expressions producing
-- unique arrays.  This is perhaps a bit too conservative - we could
-- track exactly which are being consumed and keep a blacklist.
performCSE (esubsts, nsubsts) bnd@(TaggedLet (pat,_) _ _)
  | any (getAll . uniqArray) $ patternIdents pat =
    return ([bnd], (esubsts, nsubsts))
  where uniqArray = All . not . basicType . identType <>
                    All . unique . identType
performCSE (esubsts, nsubsts) (TaggedLet (pat,patdata) lore (e,enames)) =
  case M.lookup e' esubsts of
    Nothing -> return ([TaggedLet (pat,patdata') lore (e',enames')],
                       (M.insert e' pat esubsts, nsubsts))

    Just subpat -> do
      lets <- forM (zip3 (patternIdents pat) patdata $
                    patternIdents subpat) $ \(p,d,v) -> do
        let subst = SubExp $ Var v
        explore <- loreForExpM subst
        varlore <- loreForBindingM p
        return $
          TaggedLet (Pattern [Bindee p varlore],[d]) explore
          (subst, UT.usages $ HS.singleton $ identName v)
      return (lets, (esubsts, mkSubsts pat subpat `HM.union` nsubsts))
  where e' = substituteNames nsubsts e
        enames' = substituteNames nsubsts enames
        patdata' = substituteNames nsubsts patdata
