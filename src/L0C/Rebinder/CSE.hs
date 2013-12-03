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

import Control.Monad
import Control.Monad.Writer
import Control.Applicative

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Lazy as M

import L0C.L0
import L0C.Substitute

-- | Convert a pattern to an expression given the value returned by
-- that pattern.  Uses a 'Maybe' type as patterns using 'Wildcard's
-- cannot be thus converted.
patToExp :: TupIdent -> Maybe Exp
patToExp (Wildcard _ _)   = Nothing
patToExp (Id idd)         = Just $ Var idd
patToExp (TupId pats loc) = TupLit <$> mapM patToExp pats <*> pure loc

mkSubsts :: TupIdent -> Exp -> HM.HashMap VName VName
mkSubsts pat e = execWriter $ subst pat e
  where subst (Id idd1) (Var idd2) =
          tell $ HM.singleton (identName idd1) (identName idd2)
        subst (TupId pats _) (TupLit es _) =
          zipWithM_ subst pats es
        subst _ _ =
          return ()

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
performCSE :: DupeState -> TupIdent -> Exp
           -> (Exp, DupeState)
-- Arrays may be consumed, so don't eliminate expressions producing
-- unique arrays.  This is perhaps a bit too conservative - we could
-- track exactly which are being consumed and keep a blacklist.
performCSE (esubsts, nsubsts) pat e
  | any (getAll . uniqArray) $ HS.toList $ patIdents pat =
    (e, (esubsts, nsubsts))
  where uniqArray = All . not . basicType . identType <>
                    All . unique . identType
performCSE (esubsts, nsubsts) pat e =
  case M.lookup e' esubsts of
    Just e'' -> (e'', (esubsts, mkSubsts pat e'' `HM.union` nsubsts))
    Nothing -> (e',
                case patToExp pat of
                  Nothing   -> (esubsts, nsubsts)
                  Just pate -> (M.insert e' pate esubsts, nsubsts))
  where e' = substituteNames nsubsts e

-- | Run CSE over several expression alternatives, all of which use
-- the same pattern.  The same state is used to perform CSE on all of
-- the expressions in parallel, and the final state is a combination
-- of the resulting states.
performMultipleCSE :: DupeState -> TupIdent -> [Exp]
                   -> ([Exp], DupeState)
performMultipleCSE ds pat es =
  let (es',dss) = unzip $ map (performCSE ds pat) es
  in (es', mconcat dss)
