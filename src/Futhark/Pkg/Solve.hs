-- | Dependency solver
--
-- This is a relatively simple problem due to the choice of the
-- Minimum Package Version algorithm.  In fact, the only failure mode
-- is referencing an unknown package or revision.
module Futhark.Pkg.Solve
  ( solveDeps,
    solveDepsPure,
    PkgRevDepInfo,
  )
where

import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.State
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Pkg.Info
import Futhark.Pkg.Types
import Prelude

data PkgOp a = OpGetDeps PkgPath SemVer (Maybe T.Text) (PkgRevDeps -> a)

instance Functor PkgOp where
  fmap f (OpGetDeps p v h c) = OpGetDeps p v h (f . c)

-- | A rough build list is like a build list, but may contain packages
-- that are not reachable from the root.  Also contains the
-- dependencies of each package.
newtype RoughBuildList = RoughBuildList (M.Map PkgPath (SemVer, [PkgPath]))
  deriving (Show)

emptyRoughBuildList :: RoughBuildList
emptyRoughBuildList = RoughBuildList mempty

depRoots :: PkgRevDeps -> S.Set PkgPath
depRoots (PkgRevDeps m) = S.fromList $ M.keys m

-- Construct a 'BuildList' from a 'RoughBuildList'.  This involves
-- pruning all packages that cannot be reached from the root.
buildList :: S.Set PkgPath -> RoughBuildList -> BuildList
buildList roots (RoughBuildList pkgs) =
  BuildList $ execState (mapM_ addPkg roots) mempty
  where
    addPkg p = case M.lookup p pkgs of
      Nothing -> pure ()
      Just (v, deps) -> do
        listed <- gets $ M.member p
        modify $ M.insert p v
        unless listed $ mapM_ addPkg deps

type SolveM = StateT RoughBuildList (F PkgOp)

getDeps :: PkgPath -> SemVer -> Maybe T.Text -> SolveM PkgRevDeps
getDeps p v h = lift $ liftF $ OpGetDeps p v h id

-- | Given a list of immediate dependency minimum version constraints,
-- find dependency versions that fit, including transitive
-- dependencies.
doSolveDeps :: PkgRevDeps -> SolveM ()
doSolveDeps (PkgRevDeps deps) = mapM_ add $ M.toList deps
  where
    add (p, (v, maybe_h)) = do
      RoughBuildList l <- get
      case M.lookup p l of
        -- Already satisfied?
        Just (cur_v, _) | v <= cur_v -> pure ()
        -- No; add 'p' and its dependencies.
        _ -> do
          PkgRevDeps p_deps <- getDeps p v maybe_h
          put $ RoughBuildList $ M.insert p (v, M.keys p_deps) l
          mapM_ add $ M.toList p_deps

-- | Run the solver, producing both a package registry containing
-- a cache of the lookups performed, as well as a build list.
solveDeps ::
  (MonadPkgRegistry m) =>
  CacheDir ->
  PkgRevDeps ->
  m BuildList
solveDeps cachedir deps =
  buildList (depRoots deps)
    <$> runF
      (execStateT (doSolveDeps deps) emptyRoughBuildList)
      pure
      step
  where
    step (OpGetDeps p v h c) = do
      pinfo <- lookupPackageRev cachedir p v

      checkHash p v pinfo h

      d <- fmap pkgRevDeps . getManifest $ pkgRevGetManifest pinfo
      c d

    checkHash _ _ _ Nothing = pure ()
    checkHash p v pinfo (Just h)
      | h == pkgRevCommit pinfo = pure ()
      | otherwise =
          fail $
            T.unpack $
              "Package "
                <> p
                <> " "
                <> prettySemVer v
                <> " has commit hash "
                <> pkgRevCommit pinfo
                <> ", but expected "
                <> h
                <> " from package manifest."

-- | A mapping of package revisions to the dependencies of that
-- package.  Can be considered a 'PkgRegistry' without the option of
-- obtaining more information from the Internet.  Probably useful only
-- for testing the solver.
type PkgRevDepInfo = M.Map (PkgPath, SemVer) PkgRevDeps

-- | Perform package resolution with only pre-known information.  This
-- is useful for testing.
solveDepsPure :: PkgRevDepInfo -> PkgRevDeps -> Either T.Text BuildList
solveDepsPure r deps =
  buildList (depRoots deps)
    <$> runF
      (execStateT (doSolveDeps deps) emptyRoughBuildList)
      Right
      step
  where
    step (OpGetDeps p v _ c) = do
      let errmsg = "Unknown package/version: " <> p <> "-" <> prettySemVer v
      d <- maybe (Left errmsg) Right $ M.lookup (p, v) r
      c d
