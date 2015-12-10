{-# LANGUAGE FlexibleContexts #-}
-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies
  , dataDependencies
  )
  where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Futhark.Representation.AST
import Futhark.Binder (Proper)

-- | A mapping from a variable name @v@, to those variables on which
-- the value of @v@ is dependent.  The intuition is that we could
-- remove all other variables, and @v@ would still be computable.
-- This also includes names bound in loops or by lambdas.
type Dependencies = HM.HashMap VName Names

-- | Compute the data dependencies for an entire body.
dataDependencies :: Proper lore => Body lore -> Dependencies
dataDependencies = dataDependencies' HM.empty

dataDependencies' :: Proper lore =>
                     Dependencies -> Body lore -> Dependencies
dataDependencies' startdeps = foldl grow startdeps . bodyBindings
  where grow deps (Let pat _ (If c tb fb _)) =
          let tdeps = dataDependencies' deps tb
              fdeps = dataDependencies' deps fb
              cdeps = depsOf deps c
              comb (v, tres, fres) =
                (identName v, HS.unions [cdeps, depsOf tdeps tres, depsOf fdeps fres])
              branchdeps =
                HM.fromList $ map comb $ zip3 (patternIdents pat)
                (bodyResult tb)
                (bodyResult fb)
          in HM.unions [branchdeps, deps, tdeps, fdeps]

        grow deps (Let pat _ e) =
          let free = freeInExp e
              freeDeps = HS.unions $ map (depsOfVar deps) $ HS.toList free
          in HM.fromList [ (name, freeDeps) | name <- patternNames pat ] `HM.union` deps

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _) = HS.empty
depsOf deps (Var v)   = depsOfVar deps v

depsOfVar :: Dependencies -> VName -> Names
depsOfVar deps name = HS.insert name $ HM.lookupDefault HS.empty name deps
