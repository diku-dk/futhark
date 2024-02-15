module Futhark.Solve.GLPK (glpk) where

import Data.LinearProgram
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Solve.LP qualified as F

linearProgToGLPK :: (Show v, Ord v, Eq a, Num a, Group a) => F.LinearProg v a -> (LP v a)
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

glpk ::
  (Show v, Ord v, Show a, Eq a, Real a, Group a) =>
  F.LinearProg v a ->
  IO (Maybe (Int, M.Map v Int))
glpk lp = do
  (_, mres) <- glpSolveVars mipDefaults $ linearProgToGLPK lp
  pure $ (\(opt, vs) -> (truncate opt, fmap truncate vs)) <$> mres
