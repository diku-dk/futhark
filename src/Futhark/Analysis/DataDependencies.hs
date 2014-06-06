-- | Facilities for inspecting the data dependencies of a program.
module Futhark.Analysis.DataDependencies
  ( Dependencies
  , dataDependencies
  )
  where

import Data.Maybe

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Futhark.InternalRep

-- | A mapping from a variable name @v@, to those variables on which
-- the value of @v@ is dependent.  The intuition is that we could
-- remove all other variables, and @v@ would still be computable.
-- This also includes names bound in loops or by lambdas.
type Dependencies = HM.HashMap VName Names

-- | Compute the data dependencies for an entire body.
dataDependencies :: Body -> Dependencies
dataDependencies = dataDependencies' HM.empty

dataDependencies' :: Dependencies -> Body -> Dependencies
dataDependencies' startdeps = foldl grow startdeps . bodyBindings
  where grow deps (Let pat (If c tb fb _ _)) =
          let tdeps = dataDependencies' deps tb
              fdeps = dataDependencies' deps fb
              cdeps = depsOf deps c
              comb (v, tres, fres) =
                (identName v, HS.unions [cdeps, depsOf tdeps tres, depsOf fdeps fres])
              branchdeps =
                HM.fromList $ map comb $ zip3 pat (resultSubExps $ bodyResult tb)
                                                  (resultSubExps $ bodyResult fb)
          in HM.unions [branchdeps, deps, tdeps, fdeps]

        grow deps (Let pat (DoLoop respat merge _ bound body _)) =
          let deps' = deps `HM.union` HM.fromList
                      [ (identName v, depsOf deps e) | (v,e) <- merge ]
              bodydeps = dataDependencies' deps' body
              bounddeps = depsOf deps bound
              comb v e =
                (identName v, HS.unions [bounddeps, depsOf bodydeps e])
              mergedeps = HM.fromList $ zipWith comb (map fst merge) $
                          resultSubExps $ bodyResult body
          in HM.fromList [ (identName v, nameDeps (identName res) mergedeps)
                           | (v, res) <- zip pat respat ]
             `HM.union` HM.unions [deps, bodydeps]

        grow deps (Let pat (Map cs fun arrs _)) =
          let pardeps = mkDeps (lambdaParams fun) $
                        soacArgDeps deps cs $ map (depsOf deps) arrs
              deps' = dataDependencies' (pardeps `HM.union` deps) $
                      lambdaBody fun
              resdeps = HM.fromList $ zip (map identName pat) $
                        lambdaDeps deps' fun
          in resdeps `HM.union` deps'

        grow deps (Let pat (Filter cs fun arrs outersize _)) =
          let pardeps = mkDeps (lambdaParams fun) $
                        soacArgDeps deps cs $ map (depsOf deps) arrs
              deps' = dataDependencies' (pardeps `HM.union` deps) $
                      lambdaBody fun
              resdeps = mkDeps pat $ repeat $ HS.unions $
                        depsOf deps outersize : lambdaDeps deps' fun
          in resdeps `HM.union` deps'

        grow deps (Let pat (Reduce cs fun args _)) =
          foldDeps deps pat cs fun acc arr
          where (acc,arr) = unzip args

        grow deps (Let pat (Scan cs fun args _)) =
          foldDeps deps pat cs fun acc arr
          where (acc,arr) = unzip args

        grow deps (Let pat (Redomap cs outerfun innerfun acc arr _)) =
          let (deps', seconddeps) =
                foldDeps' deps cs innerfun (map (depsOf deps) acc) (map (depsOf deps) arr)
              (outerdeps, names) =
                foldDeps' deps cs outerfun seconddeps seconddeps
          in mkDeps pat names `HM.union` outerdeps `HM.union` deps'

        grow deps (Let pat e) =
          let free = freeNamesInExp e
              freeDeps = HS.unions $ map (`nameDeps` deps) $ HS.toList free
          in HM.fromList [ (identName v, freeDeps) | v <- pat ] `HM.union` deps

foldDeps' :: Dependencies
          -> [Ident] -> Lambda -> [Names] -> [Names]
          -> (Dependencies, [Names])
foldDeps' deps cs fun acc arr =
  let pardeps = HM.fromList $ zip (map identName $ lambdaParams fun) $
                soacArgDeps deps cs $ acc++arr
      deps' = dataDependencies' (pardeps `HM.union` deps) $ lambdaBody fun
  in (deps', lambdaDeps deps' fun)

foldDeps :: Dependencies
         -> [Ident] -> [Ident] -> Lambda -> [SubExp] -> [SubExp]
         -> HM.HashMap VName Names
foldDeps deps pat cs fun acc arr =
  let pardeps = HM.fromList $ zip (map identName $ lambdaParams fun) $
                soacArgDeps deps cs $ map (depsOf deps) $ acc++arr
      deps' = dataDependencies' (pardeps `HM.union` deps) $ lambdaBody fun
      resdeps = HM.fromList $ zip (map identName pat) $
                lambdaDeps deps' fun
  in resdeps `HM.union` deps'

lambdaDeps :: Dependencies -> Lambda -> [Names]
lambdaDeps deps fun =
  map (depsOf deps) $ resultSubExps $ bodyResult $ lambdaBody fun

soacArgDeps :: Dependencies
            -> [Ident] -> [Names] -> [HS.HashSet VName]
soacArgDeps deps cs args =
  let cdeps = HS.unions $ map (depsOf deps . Var) cs
  in map (HS.union cdeps) args

nameDeps :: VName -> Dependencies -> Names
nameDeps name deps = HS.insert name $ fromMaybe HS.empty $ HM.lookup name deps

depsOf :: Dependencies -> SubExp -> Names
depsOf _ (Constant _ _) = HS.empty
depsOf deps (Var v)     = nameDeps (identName v) deps

mkDeps :: [IdentBase als shape] -> [Names] -> Dependencies
mkDeps idents names = HM.fromList $ zip (map identName idents) names
