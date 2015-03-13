{-# LANGUAGE FlexibleContexts #-}
-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies
  , dataDependencies
  )
  where

import Data.Maybe

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import qualified Futhark.Representation.AST.Lore as Lore
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
                (resultSubExps $ bodyResult tb)
                (resultSubExps $ bodyResult fb)
          in HM.unions [branchdeps, deps, tdeps, fdeps]

        grow deps (Let pat _ (LoopOp (DoLoop respat merge form body))) =
          let deps' = deps `HM.union` HM.fromList
                      [ (fparamName v, depsOf deps e) | (v,e) <- merge ]
              bodydeps = dataDependencies' deps' body
              formdeps = case form of ForLoop _ bound -> depsOf deps bound
                                      WhileLoop _     -> HS.empty
              comb v e =
                (identName v, HS.unions [formdeps, depsOf bodydeps e])
              mergedeps = HM.fromList $ zipWith comb (map (fparamIdent . fst) merge) $
                          resultSubExps $ bodyResult body
          in HM.fromList [ (name, depsOfVar mergedeps res)
                           | (name, res) <- zip (patternNames pat) respat ]
             `HM.union` HM.unions [deps, bodydeps]

        grow deps (Let pat _ (LoopOp (Map cs fun arrs))) =
          let pardeps = mkDeps (lambdaParams fun) $
                        soacArgDeps deps cs $ map (depsOfVar deps) arrs
              deps' = dataDependencies' (pardeps `HM.union` deps) $
                      lambdaBody fun
              resdeps = HM.fromList $ zip (patternNames pat) $
                        lambdaDeps deps' fun
          in resdeps `HM.union` deps'

        grow deps (Let pat _ (LoopOp (Reduce cs fun args))) =
          foldDeps deps pat cs fun acc arr
          where (acc,arr) = unzip args

        grow deps (Let pat _ (LoopOp (Scan cs fun args))) =
          foldDeps deps pat cs fun acc arr
          where (acc,arr) = unzip args

        grow deps (Let pat _ (LoopOp (Redomap cs outerfun innerfun acc arr))) =
          let (deps', seconddeps) =
                foldDeps' deps cs innerfun
                (map (depsOf deps) acc) (map (depsOfVar deps) arr)
              (outerdeps, names) =
                foldDeps' deps cs outerfun seconddeps seconddeps
          in mkDeps (patternIdents pat) names `HM.union` outerdeps `HM.union` deps'

        grow deps (Let pat _ e) =
          let free = freeInExp e
              freeDeps = HS.unions $ map (depsOfVar deps) $ HS.toList free
          in HM.fromList [ (name, freeDeps) | name <- patternNames pat ] `HM.union` deps

foldDeps' :: Proper lore =>
             Dependencies
          -> Certificates -> Lambda lore -> [Names] -> [Names]
          -> (Dependencies, [Names])
foldDeps' deps cs fun acc arr =
  let pardeps = HM.fromList $ zip (map identName $ lambdaParams fun) $
                soacArgDeps deps cs $ acc++arr
      deps' = dataDependencies' (pardeps `HM.union` deps) $ lambdaBody fun
  in (deps', lambdaDeps deps' fun)

foldDeps :: Proper lore =>
            Dependencies
         -> Pattern lore -> Certificates -> Lambda lore -> [SubExp] -> [VName]
         -> HM.HashMap VName Names
foldDeps deps pat cs fun acc arr =
  let pardeps = HM.fromList $ zip (map identName $ lambdaParams fun) $
                soacArgDeps deps cs $
                map (depsOf deps) acc ++ map (depsOfVar deps) arr
      deps' = dataDependencies' (pardeps `HM.union` deps) $ lambdaBody fun
      resdeps = HM.fromList $ zip (patternNames pat) $
                lambdaDeps deps' fun
  in resdeps `HM.union` deps'

lambdaDeps :: FreeIn (Lore.Exp lore) => Dependencies -> Lambda lore -> [Names]
lambdaDeps deps fun =
  map (depsOf deps) $ resultSubExps $ bodyResult $ lambdaBody fun

soacArgDeps :: Dependencies
            -> [VName] -> [Names] -> [HS.HashSet VName]
soacArgDeps deps cs args =
  let cdeps = HS.unions $ map (depsOf deps . Var) cs
  in map (HS.union cdeps) args

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _) = HS.empty
depsOf deps (Var v)   = depsOfVar deps v

depsOfVar :: Dependencies -> VName -> Names
depsOfVar deps name = HS.insert name $ fromMaybe HS.empty $ HM.lookup name deps

mkDeps :: [IdentBase shape] -> [Names] -> Dependencies
mkDeps idents names = HM.fromList $ zip (map identName idents) names
