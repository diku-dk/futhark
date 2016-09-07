{-# LANGUAGE FlexibleContexts #-}
-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies
  , dataDependencies
  )
  where

import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Prelude

import Futhark.Representation.AST

-- | A mapping from a variable name @v@, to those variables on which
-- the value of @v@ is dependent.  The intuition is that we could
-- remove all other variables, and @v@ would still be computable.
-- This also includes names bound in loops or by lambdas.
type Dependencies = HM.HashMap VName Names

-- | Compute the data dependencies for an entire body.
dataDependencies :: Attributes lore => Body lore -> Dependencies
dataDependencies = dataDependencies' HM.empty

dataDependencies' :: Attributes lore =>
                     Dependencies -> Body lore -> Dependencies
dataDependencies' startdeps = foldl grow startdeps . bodyBindings
  where grow deps (Let pat _ (If c tb fb _)) =
          let tdeps = dataDependencies' deps tb
              fdeps = dataDependencies' deps fb
              cdeps = depsOf deps c
              comb (pe, tres, fres) =
                (patElemName pe,
                 HS.unions $ [freeIn pe, cdeps, depsOf tdeps tres, depsOf fdeps fres] ++
                 map (depsOfVar deps) (HS.toList $ freeIn pe))
              branchdeps =
                HM.fromList $ map comb $ zip3 (patternValueElements pat)
                (bodyResult tb)
                (bodyResult fb)
          in HM.unions [branchdeps, deps, tdeps, fdeps]

        grow deps (Let pat _ e) =
          let free = freeIn pat <> freeInExp e
              freeDeps = HS.unions $ map (depsOfVar deps) $ HS.toList free
          in HM.fromList [ (name, freeDeps) | name <- patternNames pat ] `HM.union` deps

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _) = HS.empty
depsOf deps (Var v)   = depsOfVar deps v

depsOfVar :: Dependencies -> VName -> Names
depsOfVar deps name = HS.insert name $ HM.lookupDefault HS.empty name deps
