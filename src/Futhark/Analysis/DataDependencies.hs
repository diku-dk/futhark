-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies,
    dataDependencies,
    findNecessaryForReturned,
  )
where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Futhark.IR

-- | A mapping from a variable name @v@, to those variables on which
-- the value of @v@ is dependent.  The intuition is that we could
-- remove all other variables, and @v@ would still be computable.
-- This also includes names bound in loops or by lambdas.
type Dependencies = M.Map VName Names

-- | Compute the data dependencies for an entire body.
dataDependencies :: ASTRep rep => Body rep -> Dependencies
dataDependencies = dataDependencies' M.empty

dataDependencies' ::
  ASTRep rep =>
  Dependencies ->
  Body rep ->
  Dependencies
dataDependencies' startdeps = foldl grow startdeps . bodyStms
  where
    grow deps (Let pat _ (Match c cases defbody _)) =
      let cases_deps = map (dataDependencies' deps . caseBody) cases
          defbody_deps = dataDependencies' deps defbody
          cdeps = foldMap (depsOf deps) c
          comb (pe, se_cases_deps, se_defbody_deps) =
            ( patElemName pe,
              mconcat $
                se_cases_deps
                  ++ [freeIn pe, cdeps, se_defbody_deps]
                  ++ map (depsOfVar deps) (namesToList $ freeIn pe)
            )
          branchdeps =
            M.fromList $
              map comb $
                zip3
                  (patElems pat)
                  ( L.transpose . zipWith (map . depsOf) cases_deps $
                      map (map resSubExp . bodyResult . caseBody) cases
                  )
                  (map (depsOf defbody_deps . resSubExp) (bodyResult defbody))
       in M.unions $ [branchdeps, deps, defbody_deps] ++ cases_deps
    grow deps (Let pat _ e) =
      let free = freeIn pat <> freeIn e
          freeDeps = mconcat $ map (depsOfVar deps) $ namesToList free
       in M.fromList [(name, freeDeps) | name <- patNames pat] `M.union` deps

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _) = mempty
depsOf deps (Var v) = depsOfVar deps v

depsOfVar :: Dependencies -> VName -> Names
depsOfVar deps name = oneName name <> M.findWithDefault mempty name deps

-- | @findNecessaryForReturned p merge deps@ computes which of the
-- loop parameters (@merge@) are necessary for the result of the loop,
-- where @p@ given a loop parameter indicates whether the final value
-- of that parameter is live after the loop.  @deps@ is the data
-- dependencies of the loop body.  This is computed by straightforward
-- fixpoint iteration.
findNecessaryForReturned ::
  (Param dec -> Bool) ->
  [(Param dec, SubExp)] ->
  M.Map VName Names ->
  Names
findNecessaryForReturned usedAfterLoop merge_and_res allDependencies =
  iterateNecessary mempty
    <> namesFromList (map paramName $ filter usedAfterLoop $ map fst merge_and_res)
  where
    iterateNecessary prev_necessary
      | necessary == prev_necessary = necessary
      | otherwise = iterateNecessary necessary
      where
        necessary = mconcat $ map dependencies returnedResultSubExps
        usedAfterLoopOrNecessary param =
          usedAfterLoop param || paramName param `nameIn` prev_necessary
        returnedResultSubExps =
          map snd $ filter (usedAfterLoopOrNecessary . fst) merge_and_res
        dependencies (Constant _) =
          mempty
        dependencies (Var v) =
          M.findWithDefault (oneName v) v allDependencies
