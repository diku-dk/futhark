-- | Re-implementation of the necessary functions to check for self-overlap and disjointness
module Futhark.Analysis.CosminSolver (selfOverlap, lessThan, disjoint, SolverInput (..)) where

import Data.Map qualified as M
import Futhark.Analysis.PrimExp
import Futhark.IR.Mem.Interval (Interval (..))
import Futhark.IR.Syntax
--import Futhark.Util.Pretty

import Debug.Trace

-- | Input for CosminSolver
data SolverInput = SolverInput
  { -- | The types of the values in scope
    scope :: M.Map VName Type,
    -- | A list of 'PrimExp' that have been asserted.
    assertions :: [PrimExp VName],
    -- | Pairs where the 'VName' is known to be strictly less than the 'PrimExp'.
    less_thans :: [(VName, PrimExp VName)],
    -- | 'PrimExp' that is known to greater than or equal to zero.
    non_negatives :: [PrimExp VName]
  }

prettySolver :: SolverInput -> String
prettySolver env =
    "Solver Environment:\n\tAssertions\n\t\t" ++
    prettyString (assertions env) ++ "\n\tLessThans:\n\t\t" ++
    concatMap (\(nm,pe) -> prettyString nm ++ " < " ++ prettyString pe ++ ", ") (less_thans env) ++ "\n\tPositives:\n\t\t" ++
    concatMap (\p -> prettyString p ++ " >= 0, ") (non_negatives env) ++ "\n\n"
    
-- | Given 'SolverInput', returns 'Nothing' if there is no overlap or 'Just
-- the_problem_interval' if the 'Interval' list self-overlaps.
selfOverlap :: SolverInput -> [Interval] -> Maybe Interval
selfOverlap _ [] = Nothing
selfOverlap _ [_] = Nothing
selfOverlap solver_input is =
  selfOverlap' 0 (reverse is)
  where
    selfOverlap' :: TPrimExp Int64 VName -> [Interval] -> Maybe Interval
    selfOverlap' acc (x : xs) =
      let interval_span = (lowerBound x + numElements x - 1) * stride x
          res = lessThan solver_input acc (stride x)
       in if res
            then selfOverlap' (acc + interval_span) xs
            else Just x
    selfOverlap' _ [] = Nothing

-- | Given 'SolverInput', returns 'True' if the two 'Interval' are disjoint.
disjoint :: SolverInput -> Interval -> Interval -> Bool
disjoint solver_input (Interval lb1 ne1 _) (Interval lb2 ne2 _) =
  lessThanOrEqual solver_input (lb1 + ne1) lb2
    || lessThanOrEqual solver_input (lb2 + ne2) lb1
    || lessThanOrEqual solver_input ne1 (0 :: TPrimExp Int64 VName)
    || lessThanOrEqual solver_input ne2 (0 :: TPrimExp Int64 VName)

-- | Given 'SolverInput', returns 'True' if the first 'PrimExp' is proven to be less
-- than or equal to the second.
lessThanOrEqual :: SolverInput -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> Bool
lessThanOrEqual solver_input pe1 pe2 =
  lessThan solver_input pe1 (pe2 + 1)

-- | Given 'SolverInput', returns 'True' if the first 'PrimExp' is proven to be
-- strictly less the second.
lessThan :: SolverInput -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> Bool
lessThan solver_input pe1 pe2 =
  trace ("Problem: " ++ prettyString pe1 ++ " < " ++ prettyString pe2 ++ " given: \n" ++ prettySolver solver_input)
    False -- TODO
