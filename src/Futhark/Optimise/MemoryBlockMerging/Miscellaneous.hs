{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | I didn't know where else to put this.  Perpetually in need of a cleanup.
module Futhark.Optimise.MemoryBlockMerging.Miscellaneous where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Monoid()
import Data.Maybe (isJust, fromMaybe)
import System.IO.Unsafe (unsafePerformIO) -- Just for debugging!

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       (ExplicitMemory, InKernel)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.Aliases
import Futhark.Util (unixEnvironment)
import Futhark.Util.Pretty (Pretty)

import Futhark.Optimise.MemoryBlockMerging.Types


usesDebugging :: Bool
usesDebugging = isJust $ lookup "FUTHARK_DEBUG" unixEnvironment

usesDebuggingJSON :: Bool
usesDebuggingJSON = isJust $ lookup "FUTHARK_DEBUG_JSON" unixEnvironment

withDebug :: IO () -> a -> a
withDebug debug x
  | usesDebugging = unsafePerformIO debug `seq` x
  | otherwise = x

doDebug :: Monad m => IO () -> m ()
doDebug debug = withDebug debug $ return ()

withDebugJSON :: IO () -> a -> a
withDebugJSON debug x
  | usesDebuggingJSON = unsafePerformIO debug `seq` x
  | otherwise = x

-- If a property is commutative in a map, build a map that reflects it.  A bit
-- crude.  We could also just use a function that calculates this whenever
-- needed.
makeCommutativeMap :: Ord v => M.Map v (S.Set v) -> M.Map v (S.Set v)
makeCommutativeMap m =
  let names = S.toList (S.union (M.keysSet m) (S.unions (M.elems m)))
      assocs = map (\n ->
                      let existing = lookupEmptyable n m
                          newly_found = S.unions $ map (\(k, v) ->
                                                          if S.member n v
                                                          then S.singleton k
                                                          else S.empty) $ M.assocs m
                          ns = S.union existing newly_found
                      in (n, ns)) names
  in M.fromList assocs

insertOrUpdate :: (Ord k, Ord v) => k -> v -> M.Map k (S.Set v) -> M.Map k (S.Set v)
insertOrUpdate k v = M.alter (insertOrNew v) k
  where insertOrNew :: Ord a => a -> Maybe (S.Set a) -> Maybe (S.Set a)
        insertOrNew x m = Just $ case m of
          Just s -> S.insert x s
          Nothing -> S.singleton x

cleanupMapping :: Ord v => M.Map v (S.Set v) -> M.Map v (S.Set v)
cleanupMapping = M.filter (not . S.null)

prettySet :: Pretty a => S.Set a -> String
prettySet = L.intercalate ", " . map pretty . S.toList

lookupEmptyable :: (Ord a, Monoid b) => a -> M.Map a b -> b
lookupEmptyable x m = fromMaybe mempty $ M.lookup x m

-- Works for now.
fromJust :: String -> Maybe a -> a
fromJust _ (Just x) = x
fromJust mistake Nothing = error mistake

maybeFromBoolM :: Monad m => (a -> m Bool) -> (a -> m (Maybe a))
maybeFromBoolM f a = do
  res <- f a
  return $ if res
           then Just a
           else Nothing

onJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
onJust may f = case may of
  Just x -> f x
  Nothing -> return ()

expandWithAliases :: MemAliases -> M.Map VName Names -> M.Map VName Names
expandWithAliases mem_aliases = fixpointIterate expand
  where expand :: M.Map VName Names -> M.Map VName Names
        expand mems_map =
          M.fromList (map (\(mem, mems) ->
                             (mem, S.unions (mems : map (`lookupEmptyable` mem_aliases)
                                             (S.toList mems))))
                      (M.assocs mems_map))

fixpointIterate :: Eq a => (a -> a) -> a -> a
fixpointIterate f x
  | f x == x = x
  | otherwise = fixpointIterate f (f x)

fromVar :: SubExp -> Maybe VName
fromVar (Var v) = Just v
fromVar _ = Nothing

-- Map on both ExplicitMemory and InKernel.
class FullMap lore where
  fullMapExpM :: Monad m => Mapper lore lore m -> KernelMapper InKernel InKernel m
              -> Exp lore -> m (Exp lore)

instance FullMap ExplicitMemory where
  fullMapExpM mapper mapper_kernel e =
    case e of
      Op (ExpMem.Inner kernel) -> do
        kernel' <- mapKernelM mapper_kernel kernel
        return $ Op (ExpMem.Inner kernel')
      _ -> mapExpM mapper e

instance FullMap InKernel where
  fullMapExpM mapper _ = mapExpM mapper

-- Walk on both ExplicitMemory and InKernel.
class FullWalk lore where
  fullWalkExpM :: Monad m => Walker lore m -> KernelWalker InKernel m
               -> Exp lore -> m ()

instance FullWalk ExplicitMemory where
  fullWalkExpM walker walker_kernel e = do
    walkExpM walker e
    case e of
      Op (ExpMem.Inner kernel) ->
        walkKernelM walker_kernel kernel
      _ -> return ()

instance FullWalk InKernel where
  fullWalkExpM walker _ = walkExpM walker

-- FIXME: Integrate this into the above type class.
class FullWalkAliases lore where
  fullWalkAliasesExpM :: Monad m => Walker (Aliases lore) m
                      -> KernelWalker (Aliases InKernel) m
                      -> Exp (Aliases lore) -> m ()

instance FullWalkAliases ExplicitMemory where
  fullWalkAliasesExpM walker walker_kernel e = do
    walkExpM walker e
    case e of
      Op (ExpMem.Inner kernel) ->
        walkKernelM walker_kernel kernel
      _ -> return ()

instance FullWalkAliases InKernel where
  fullWalkAliasesExpM walker _ = walkExpM walker
