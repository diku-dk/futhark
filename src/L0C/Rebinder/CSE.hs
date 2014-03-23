-- |
--
-- Implementation of common-subexpression elimination for the
-- rebinder.
--
module L0C.Rebinder.CSE
  ( DupeState
  , newDupeState
  , performCSE
  , performMultipleCSE
  )
  where

import Data.Loc
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M

import L0C.InternalRep
import L0C.Substitute

mkSubsts :: [Ident] -> Exp -> HM.HashMap VName VName
mkSubsts pat (SubExps es _) = HM.fromList $ mapMaybe subst $ zip pat es
  where subst (v1, Var v2) = Just (identName v1, identName v2)
        subst _            = Nothing
mkSubsts _ _ = HM.empty

-- | State passed to 'performCSE'.
type DupeState = (M.Map Exp Exp, HM.HashMap VName VName)

-- | The state that should be passed to the first call to
-- 'performCSE'.
newDupeState :: DupeState
newDupeState = (M.empty, HM.empty)

-- | The main CSE function.  Given a state, a pattern and an
-- expression to be bound to that pattern, return a replacement
-- expression and a new state.  The function will look in the state to
-- determine whether the expression can be replaced with something
-- bound previously.
performCSE :: DupeState -> [Ident] -> Exp
           -> (Exp, DupeState)
-- Arrays may be consumed, so don't eliminate expressions producing
-- unique arrays.  This is perhaps a bit too conservative - we could
-- track exactly which are being consumed and keep a blacklist.
performCSE (esubsts, nsubsts) pat e
  | any (getAll . uniqArray) pat =
    (e, (esubsts, nsubsts))
  where uniqArray = All . not . basicType . identType <>
                    All . unique . identType
performCSE (esubsts, nsubsts) pat e =
  case M.lookup e' esubsts of
    Just e'' -> (e'', (esubsts, mkSubsts pat e'' `HM.union` nsubsts))
    Nothing -> (e', (M.insert e' pate esubsts, nsubsts))
  where e' = substituteNames nsubsts e
        pate = SubExps (map Var pat) $ srclocOf e

-- | Run CSE over several expression alternatives, all of which use
-- the same pattern.  The same state is used to perform CSE on all of
-- the expressions in parallel, and the final state is a combination
-- of the resulting states.
performMultipleCSE :: DupeState -> [Ident] -> [Exp]
                   -> ([Exp], DupeState)
performMultipleCSE ds pat es =
  let (es',dss) = unzip $ map (performCSE ds pat) es
  in (es', mconcat dss)
