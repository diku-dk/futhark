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

import qualified Data.Map as M
import qualified Data.Set as S

import L0C.L0
import L0C.Substitute

-- | Convert a pattern to an expression given the value returned by
-- that pattern.  Uses a 'Maybe' type as patterns using 'Wildcard's
-- cannot be thus converted.
patToExp :: TupIdent -> Maybe Exp
patToExp (Wildcard _ _)   = Nothing
patToExp (Id idd)         = Just $ Var idd
patToExp (TupId pats loc) = TupLit <$> mapM patToExp pats <*> pure loc

mkSubsts :: TupIdent -> Exp -> M.Map VName VName
mkSubsts pat e = execWriter $ subst pat e
  where subst (Id idd1) (Var idd2) =
          tell $ M.singleton (identName idd1) (identName idd2)
        subst (TupId pats _) (TupLit es _) =
          zipWithM_ subst pats es
        subst _ _ =
          return ()

-- | State passed to 'performCSE'.
type DupeState = (M.Map Exp Exp, M.Map VName VName)

-- | The state that should be passed to the first call to
-- 'performCSE'.
newDupeState :: DupeState
newDupeState = (M.empty, M.empty)

-- | The main CSE function.  Given a state, a pattern and an
-- expression to be bound to that pattern, return a replacement
-- expression and a new state.  The function will look in the state to
-- determine whether the expression can be replaced with something
-- bound previously.
performCSE :: DupeState -> TupIdent -> Exp
           -> (Exp, DupeState)
-- Arrays may be consumed, so don't eliminate expressions producing
-- arrays.  This is perhaps a bit too conservative - we could track
-- exactly which are being consumed and keep a blacklist.
performCSE (esubsts, nsubsts) pat e
  | any (not . basicType . identType) $ S.toList $ patIdents pat =
    (e, (esubsts, nsubsts))
performCSE (esubsts, nsubsts) pat e =
  case M.lookup (substituteNames nsubsts e) esubsts of
    Just e' -> (e', (esubsts, mkSubsts pat e' `M.union` nsubsts))
    Nothing -> (e,
                case patToExp pat of
                  Nothing   -> (esubsts, nsubsts)
                  Just pate -> (M.insert e pate esubsts, nsubsts))

-- | Run CSE over several pattern-expression pairs.
performMultipleCSE :: DupeState -> TupIdent -> [Exp]
                   -> ([Exp], DupeState)
performMultipleCSE ds pat es =
  let (es',dss) = unzip $ map (performCSE ds pat) es
  in (es', mconcat dss)
