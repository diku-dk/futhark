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
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Lazy as M

import Futhark.Representation.AST
import Futhark.Substitute
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

-- | The main CSE function.  Given a set of consumed names, a state, a
-- pattern and an expression to be bound to that pattern, return a
-- replacement set of bindings and a new state.  The function will
-- look in the state to determine whether the expression can be
-- replaced with something bound previously.
performCSE :: (Proper (Lore m), BindableM m) =>
              Names -> DupeState (Lore m) -> Binding (Lore m)
           -> m ([Binding (Lore m)], DupeState (Lore m))
-- Do not eliminate arrays that have been consumed.
performCSE consumed (esubsts, nsubsts) bnd@(Let pat _ _)
  | any (`HS.member` consumed) $ patternNames pat =
    return ([bnd], (esubsts, nsubsts))
performCSE _ (esubsts, nsubsts) (Let pat lore e) =
  case M.lookup e' esubsts of
    Nothing -> return ([Let pat lore e'],
                       (M.insert e' pat esubsts, nsubsts))

    Just subpat -> do
      lets <-
        forM (zip (patternNames pat) $ patternIdents subpat) $ \(p,v) ->
          mkLetNamesM [p] $ PrimOp $ SubExp $ Var v
      return (lets, (esubsts, mkSubsts pat subpat `HM.union` nsubsts))
  where e' = substituteNames nsubsts e
