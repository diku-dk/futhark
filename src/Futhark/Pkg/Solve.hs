{-# LANGUAGE OverloadedStrings #-}
-- | Dependency solver
--
-- This is a relatively simple problem due to the choice of the
-- Minimum Package Version algorithm.  In fact, the only failure mode
-- is referencing an unknown package or revision.
module Futhark.Pkg.Solve
  ( solveDeps
  , solveDepsPure
  , PkgRevDepInfo
  ) where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid ((<>))

import Control.Monad.Free

import Futhark.Pkg.Info
import Futhark.Pkg.Types

import Prelude

data PkgOp a = OpGetDeps PkgPath SemVer (Maybe T.Text) (PkgRevDeps -> a)

instance Functor PkgOp where
  fmap f (OpGetDeps p v h c) = OpGetDeps p v h (f . c)

-- | A rough build list is like a build list, but may contain packages
-- that are not reachable from the root, and also contains a *reason*
-- for why each package is included.  It is sorted, such that any
-- package in the list can only be included due to a package later in
-- the list.  If the reason is 'Nothing', then it is a root package.
-- Unnecessary packages may be present if they were dependencies of
-- package revisions that were later bumped.
newtype RoughBuildList = RoughBuildList [(PkgPath, (SemVer, Maybe (PkgPath, SemVer)))]
                       deriving (Show)

emptyRoughBuildList :: RoughBuildList
emptyRoughBuildList = RoughBuildList mempty

-- | Construct a 'BuildList' from a 'RoughBuildList'.  This involves
-- pruning all packages that cannot be reached from the specified
-- root.
buildList :: RoughBuildList -> BuildList
buildList (RoughBuildList []) =
  BuildList mempty
buildList (RoughBuildList ((p, (p_v, cause)) : l)) =
  let BuildList m = buildList $ RoughBuildList l
      keep = case cause of
               Just (cause_p, cause_v) -> Just cause_v == M.lookup cause_p m
               Nothing                 -> True
  in if keep
     then BuildList $ M.insertWith max p p_v m
     else BuildList m

type SolveM = StateT RoughBuildList (Free PkgOp)

getDeps :: PkgPath -> SemVer -> Maybe T.Text -> SolveM PkgRevDeps
getDeps p v h = lift $ Free $ OpGetDeps p v h return

notAlreadySeen :: (PkgPath, (SemVer, a)) -> SolveM Bool
notAlreadySeen x = do
  RoughBuildList l <- get
  return $ pkgVerPairs x `notElem` map pkgVerPairs l
  where pkgVerPairs (p, (v, _)) = (p, v)

ensureFulfilled :: (PkgPath, (SemVer, Maybe T.Text)) -> SolveM ()
ensureFulfilled (dep, (v, maybe_h)) = do
  PkgRevDeps dep_deps <- getDeps dep v maybe_h
  new_deps <- filterM notAlreadySeen $ M.toList dep_deps
  modify $ \(RoughBuildList l) ->
    RoughBuildList $ [(p, (p_v, Just (dep, v))) | (p, (p_v, _)) <- new_deps] ++ l
  mapM_ ensureFulfilled new_deps

-- | Given a list of immediate dependency minimum version constraints,
-- find dependency versions that fit, including transitive
-- dependencies.
doSolveDeps :: PkgRevDeps -> SolveM ()
doSolveDeps (PkgRevDeps deps) = do
  put $ RoughBuildList [(p, (p_v, Nothing)) | (p, (p_v, _)) <- M.toList deps]
  mapM_ ensureFulfilled $ M.toList deps

-- | Run the solver, producing both a package registry containing
-- a cache of the lookups performed, as well as a build list.
solveDeps :: MonadPkgRegistry m =>
             PkgRevDeps -> m BuildList
solveDeps deps = fmap buildList $ step $ execStateT (doSolveDeps deps) emptyRoughBuildList
  where step (Pure x) = return x
        step (Free (OpGetDeps p v h c)) = do
          pinfo <- lookupPackageRev p v

          checkHash p v pinfo h

          d <- fmap pkgRevDeps . getManifest $ pkgRevGetManifest pinfo
          step $ c d

        checkHash _ _ _ Nothing = return ()
        checkHash p v pinfo (Just h)
          | h == pkgRevCommit pinfo = return ()
          | otherwise = fail $ T.unpack $ "Package " <> p <> " " <> prettySemVer v <>
                        " has commit hash " <> pkgRevCommit pinfo <>
                        ", but expected " <> h <> " from package manifest."

-- | A mapping of package revisions to the dependencies of that
-- package.  Can be considered a 'PkgRegistry' without the option of
-- obtaining more information from the Internet.  Probably useful only
-- for testing the solver.
type PkgRevDepInfo = M.Map (PkgPath, SemVer) PkgRevDeps

-- | Perform package resolution with only pre-known information.  This
-- is useful for testing.
solveDepsPure :: PkgRevDepInfo -> PkgRevDeps -> Either T.Text BuildList
solveDepsPure r deps = fmap buildList $ step $ execStateT (doSolveDeps deps) emptyRoughBuildList
  where step (Pure x) = Right x
        step (Free (OpGetDeps p v _ c)) = do
          let errmsg = "Unknown package/version: " <> p <> "-" <> prettySemVer v
          d <- maybe (Left errmsg) Right $ M.lookup (p,v) r
          step $ c d
