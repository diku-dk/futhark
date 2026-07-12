module Futhark.Solve.GLPK (glpk) where

import Control.Monad
import Data.Bifunctor
import Data.LinearProgram
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Solve.LP qualified as F
import System.IO.Silently

linearProgToGLPK :: (Ord v, Num a) => F.LinearProg v a -> LP v a
linearProgToGLPK prog =
  LP
    { direction = cOptType $ F.optType prog,
      objective = cObj $ F.objective prog,
      constraints = map cConstraint $ F.constraints prog,
      varBounds = bounds,
      varTypes = kinds
    }
  where
    cOptType F.Maximize = Max
    cOptType F.Minimize = Min
    cObj = fst . cLSum

    cLSum (F.LSum m) =
      ( M.mapKeys fromJust $ M.filterWithKey (\k _ -> isJust k) m,
        fromMaybe 0 (m M.!? Nothing)
      )

    cConstraint (F.Constraint ctype l r) =
      let (linfunc, c) = cLSum $ l F.~-~ r
          bound =
            case ctype of
              F.Equal -> Equ (-c)
              F.LessEq -> UBound (-c)
       in Constr Nothing linfunc bound

    bounds = M.fromList $ (,LBound 0) <$> varList
    kinds = M.fromList $ (,IntVar) <$> varList

    varList = S.toList $ F.vars prog

glpk :: (Ord v, Real a) => F.LinearProg v a -> IO (Maybe (Int, M.Map v Int))
glpk lp = do
  (output, res) <- capture $ glpk' lp
  pure $ do
    guard $ "PROBLEM HAS NO INTEGER FEASIBLE SOLUTION" `notElem` lines output
    res

glpk' :: (Ord v, Real a) => F.LinearProg v a -> IO (Maybe (Int, M.Map v Int))
glpk' lp
  | F.isConstant (F.objective lp) -- FIXME
    =
      pure $ pure (0, M.fromList $ map (,0) $ S.toList $ F.vars lp)
  | otherwise = do
      (_, mres) <- glpSolveVars opts $ linearProgToGLPK lp
      pure $ bimap truncate (fmap truncate) <$> mres
  where
    opts = mipDefaults {msgLev = MsgAll}
