-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies,
    dataDependencies,
    depsOf,
    depsOfArrays,
    lambdaDependencies,
    reductionDependencies,
    findNecessaryForReturned,
  )
where

-- import Data.Function ((&))
import Data.List qualified as L
import Data.Map.Strict qualified as M
-- import Debug.Trace
import Futhark.IR

-- | A mapping from a variable name @v@, to those variables on which
-- the value of @v@ is dependent.  The intuition is that we could
-- remove all other variables, and @v@ would still be computable.
-- This also includes names bound in loops or by lambdas.
type Dependencies = M.Map VName Names

-- | Compute the data dependencies for an entire body.
dataDependencies :: (ASTRep rep) => Body rep -> Dependencies
dataDependencies = dataDependencies' M.empty

dataDependencies' ::
  (ASTRep rep) =>
  Dependencies ->
  Body rep ->
  Dependencies
dataDependencies' startdeps = foldl grow startdeps . bodyStms
  where
    -- grow _deps (Let _pat _ (WithAcc _ _)) = undefined
    grow deps (Let pat _ (Op op)) =
      -- TODO transitive dependencies; reduce res is still
      -- not directly related to input array. But may just
      -- be the way the example code is written; try to simplify.
      let res = opDependencies op
          res' = map (depsOfNames deps) res
       in M.fromList (zip (patNames pat) res') `M.union` deps
    -- let deps' = dprint "@dataDependencies opCase deps@" deps
    --     res =
    --       opDependencies op
    --         & dprint ("@[1/2]dataDependencies opCase Let " ++ show (patNames pat) ++ "=@")
    --     res' =
    --       map (depsOfNames deps) res
    --         & dprint ("@[2/2]dataDependencies opCase Let " ++ show (patNames pat) ++ "=@")
    --  in M.fromList (zip (patNames pat) res') `M.union` deps'
    --       & dprint "@dataDependencies opCase@"
    -- where
    --   dprint msg x = Debug.Trace.trace (msg ++ "\n " ++ show x ++ "\n") x
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
          free_deps = depsOfNames deps free
       in M.fromList [(name, free_deps) | name <- patNames pat] `M.union` deps

--       & dprint "@dataDependencies@"
-- where
--   dprint msg x = Debug.Trace.trace (msg ++ "\n " ++ show x ++ "\n") x

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _) = mempty
depsOf deps (Var v) = depsOfVar deps v

depsOfVar :: Dependencies -> VName -> Names
depsOfVar deps name = oneName name <> M.findWithDefault mempty name deps

depsOfRes :: Dependencies -> SubExpRes -> Names
depsOfRes deps (SubExpRes _ se) = depsOf deps se

-- | Extend @names@ with direct dependencies in @deps@.
depsOfNames :: Dependencies -> Names -> Names
depsOfNames deps names = mconcat $ map (depsOfVar deps) $ namesToList names

depsOfArrays :: SubExp -> [VName] -> [Names]
depsOfArrays size = map (\arr -> oneName arr <> depsOf mempty size)

-- | Determine the variables on which the results of applying
-- anonymous function @lam@ to @inputs@ depend.
lambdaDependencies ::
  (ASTRep rep) =>
  Dependencies ->
  Lambda rep ->
  [Names] ->
  [Names]
lambdaDependencies deps lam inputs =
  let names_in_scope = freeIn lam <> mconcat inputs
      deps_in = M.fromList $ zip (boundByLambda lam) inputs
      deps' = dataDependencies' (deps_in <> deps) (lambdaBody lam)
   in map
        (namesIntersection names_in_scope . depsOfRes deps')
        (bodyResult $ lambdaBody lam)

-- | Like 'lambdaDependencies', but @lam@ is a binary operation
-- with a neutral element.
reductionDependencies ::
  (ASTRep rep) =>
  Dependencies ->
  Lambda rep ->
  [SubExp] ->
  [Names] ->
  [Names]
reductionDependencies deps lam neutrals inputs =
  let neutrals' = map (depsOf deps) neutrals
   in lambdaDependencies deps lam (zipWith (<>) neutrals' inputs)

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
