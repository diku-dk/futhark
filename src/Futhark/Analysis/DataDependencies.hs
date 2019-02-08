{-# LANGUAGE FlexibleContexts #-}
-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies
  , dataDependencies
  , findNecessaryForReturned
  )
  where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
                 S.unions $ [freeIn pe, cdeps, depsOf tdeps tres, depsOf fdeps fres] ++
                 map (depsOfVar deps) (S.toList $ freeIn pe))
              branchdeps =
                M.fromList $ map comb $ zip3 (patternElements pat)
                (bodyResult tb)
                (bodyResult fb)
          in M.unions [branchdeps, deps, tdeps, fdeps]

        grow deps (Let pat _ e) =
          let free = freeIn pat <> freeInExp e
              freeDeps = S.unions $ map (depsOfVar deps) $ S.toList free
          in M.fromList [ (name, freeDeps) | name <- patternNames pat ] `M.union` deps

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _) = S.empty
depsOf deps (Var v)   = depsOfVar deps v

depsOfVar :: Dependencies -> VName -> Names
depsOfVar deps name = S.insert name $ M.findWithDefault S.empty name deps

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
                  usedAfterLoop param || paramName param `S.member` prev_necessary
                returnedResultSubExps =
                  map snd $ filter (usedAfterLoopOrNecessary . fst) merge_and_res
                dependencies (Constant _) =
                  S.empty
                dependencies (Var v)      =
                  M.findWithDefault (S.singleton v) v allDependencies
