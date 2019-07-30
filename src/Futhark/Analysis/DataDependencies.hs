{-# LANGUAGE FlexibleContexts #-}
-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies
  , dataDependencies
  , findNecessaryForReturned
  )
  where

import qualified Data.Map.Strict as M

import Futhark.Representation.AST

-- | A mapping from a variable name @v@, to those variables on which
-- the value of @v@ is dependent.  The intuition is that we could
-- remove all other variables, and @v@ would still be computable.
-- This also includes names bound in loops or by lambdas.
type Dependencies = M.Map VName Names

-- | Compute the data dependencies for an entire body.
dataDependencies :: Attributes lore => Body lore -> Dependencies
dataDependencies = dataDependencies' M.empty

dataDependencies' :: Attributes lore =>
                     Dependencies -> Body lore -> Dependencies
dataDependencies' startdeps = foldl grow startdeps . bodyStms
  where grow deps (Let pat _ (If c tb fb _)) =
          let tdeps = dataDependencies' deps tb
              fdeps = dataDependencies' deps fb
              cdeps = depsOf deps c
              comb (pe, tres, fres) =
                (patElemName pe,
                 mconcat $ [freeIn pe, cdeps, depsOf tdeps tres, depsOf fdeps fres] ++
                 map (depsOfVar deps) (namesToList $ freeIn pe))
              branchdeps =
                M.fromList $ map comb $ zip3 (patternElements pat)
                (bodyResult tb)
                (bodyResult fb)
          in M.unions [branchdeps, deps, tdeps, fdeps]

        grow deps (Let pat _ e) =
          let free = freeIn pat <> freeIn e
              freeDeps = mconcat $ map (depsOfVar deps) $ namesToList free
          in M.fromList [ (name, freeDeps) | name <- patternNames pat ] `M.union` deps

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _) = mempty
depsOf deps (Var v)   = depsOfVar deps v

depsOfVar :: Dependencies -> VName -> Names
depsOfVar deps name = oneName name <> M.findWithDefault mempty name deps

findNecessaryForReturned :: (Param attr -> Bool) -> [(Param attr, SubExp)]
                         -> M.Map VName Names
                         -> Names
findNecessaryForReturned usedAfterLoop merge_and_res allDependencies =
  iterateNecessary mempty
  where iterateNecessary prev_necessary
          | necessary == prev_necessary = necessary
          | otherwise                   = iterateNecessary necessary
          where necessary = mconcat $ map dependencies returnedResultSubExps
                usedAfterLoopOrNecessary param =
                  usedAfterLoop param || paramName param `nameIn` prev_necessary
                returnedResultSubExps =
                  map snd $ filter (usedAfterLoopOrNecessary . fst) merge_and_res
                dependencies (Constant _) =
                  mempty
                dependencies (Var v)      =
                  M.findWithDefault (oneName v) v allDependencies
