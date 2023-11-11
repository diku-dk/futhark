-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies,
    dataDependencies,
    depsOf,
    depsOf',
    depsOfArrays,
    depsOfShape,
    lambdaDependencies,
    reductionDependencies,
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
dataDependencies :: (ASTRep rep) => Body rep -> Dependencies
dataDependencies = dataDependencies' M.empty

dataDependencies' ::
  (ASTRep rep) =>
  Dependencies ->
  Body rep ->
  Dependencies
dataDependencies' startdeps = foldl grow startdeps . bodyStms
  where
    grow deps (Let pat _ (WithAcc inputs lam)) =
      let input_deps = foldMap depsOfWithAccInput inputs
          -- Dependencies of each input reduction are concatenated.
          -- Input to lam is cert_1, ..., cert_n, acc_1, ..., acc_n.
          lam_deps = lambdaDependencies deps lam (input_deps <> input_deps)
          transitive = map (depsOfNames deps) lam_deps
       in M.fromList (zip (patNames pat) transitive) `M.union` deps
      where
        depsOfArrays' shape =
          map (\arr -> oneName arr <> depsOfShape shape)
        depsOfWithAccInput (shape, arrs, Nothing) =
          depsOfArrays' shape arrs
        depsOfWithAccInput (shape, arrs, Just (lam', nes)) =
          reductionDependencies deps lam' nes (depsOfArrays' shape arrs)
    grow deps (Let pat _ (Op op)) =
      let op_deps = map (depsOfNames deps) (opDependencies op)
          pat_deps = map (depsOfNames deps . freeIn) (patElems pat)
       in if length op_deps /= length pat_deps
            then
              error . unlines $
                [ "dataDependencies':",
                  "Pattern size: " <> show (length pat_deps),
                  "Op deps size: " <> show (length op_deps),
                  "Expression:\n",
                  prettyString op
                ]
            else
              M.fromList (zip (patNames pat) $ zipWith (<>) pat_deps op_deps)
                `M.union` deps
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

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _) = mempty
depsOf deps (Var v) = depsOfVar deps v

depsOf' :: SubExp -> Names
depsOf' (Constant _) = mempty
depsOf' (Var v) = depsOfVar mempty v

depsOfVar :: Dependencies -> VName -> Names
depsOfVar deps name = oneName name <> M.findWithDefault mempty name deps

depsOfRes :: Dependencies -> SubExpRes -> Names
depsOfRes deps (SubExpRes _ se) = depsOf deps se

-- | Extend @names@ with direct dependencies in @deps@.
depsOfNames :: Dependencies -> Names -> Names
depsOfNames deps names = mconcat $ map (depsOfVar deps) $ namesToList names

depsOfArrays :: SubExp -> [VName] -> [Names]
depsOfArrays size = map (\arr -> oneName arr <> depsOf mempty size)

depsOfShape :: Shape -> Names
depsOfShape shape = mconcat $ map (depsOf mempty) (shapeDims shape)

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
reductionDependencies deps lam nes inputs =
  let nes' = map (depsOf deps) nes
   in lambdaDependencies deps lam (zipWith (<>) nes' inputs)

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
