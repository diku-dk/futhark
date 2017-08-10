{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | I didn't know where else to put this.  Perpetually in need of a cleanup.
module Futhark.Optimise.MemoryBlockMerging.Miscellaneous where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO) -- Just for debugging!

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       (ExplicitMemory, InKernel)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.Aliases
import Futhark.Analysis.PrimExp.Convert
import Futhark.Util (unixEnvironment)
import Futhark.Util.Pretty (Pretty)

import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
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

insertOrUpdate :: (Ord k, Ord v) => k -> v ->
                  M.Map k (S.Set v) -> M.Map k (S.Set v)
insertOrUpdate k v = M.alter (insertOrNew (S.singleton v)) k

insertOrUpdateMany :: (Ord k, Ord v) => k -> S.Set v ->
                      M.Map k (S.Set v) -> M.Map k (S.Set v)
insertOrUpdateMany k vs = M.alter (insertOrNew vs) k

insertOrNew :: Ord a => S.Set a -> Maybe (S.Set a) -> Maybe (S.Set a)
insertOrNew xs m = Just $ case m of
  Just s -> S.union xs s
  Nothing -> xs

removeEmptyMaps :: Ord k => M.Map k (S.Set v) -> M.Map k (S.Set v)
removeEmptyMaps = M.filter (not . S.null)

newDeclarationsStm :: Stm lore -> [VName]
newDeclarationsStm (Let (Pattern patctxelems patvalelems) _ e) =
  let new_decls0 = map patElemName (patctxelems ++ patvalelems)
      new_decls1 = case e of
        DoLoop mergectxparams mergevalparams _loopform _body ->
          -- Technically not a declaration for the current expression, but very
          -- close.
          map (paramName . fst) (mergectxparams ++ mergevalparams)
        _ -> []
      new_decls = new_decls0 ++ new_decls1
  in new_decls

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

expandWithAliases :: forall v. Ord v => MemAliases -> M.Map v Names -> M.Map v Names
expandWithAliases mem_aliases = fixpointIterate expand
  where expand :: M.Map v Names -> M.Map v Names
        expand mems_map =
          M.fromList (map (\(v, mems) ->
                             (v, S.unions (mems : map (`lookupEmptyable` mem_aliases)
                                           (S.toList mems))))
                      (M.assocs mems_map))

fixpointIterate :: Eq a => (a -> a) -> a -> a
fixpointIterate f x
  | f x == x = x
  | otherwise = fixpointIterate f (f x)

fromVar :: SubExp -> Maybe VName
fromVar (Var v) = Just v
fromVar _ = Nothing

-- Replace variables with subtrees of their constituents wherever possible.  It
-- naively expands a PrimExp as much as the input map allows, and can enable
-- more expressions to have it in scope, since it will likely consist of fewer
-- variables.
expandPrimExp :: M.Map VName (ExpMem.PrimExp VName) -> ExpMem.PrimExp VName
              -> ExpMem.PrimExp VName
expandPrimExp var_to_pe = fixpointIterate (substituteInPrimExp var_to_pe)

expandIxFun :: M.Map VName (ExpMem.PrimExp VName) -> ExpMem.IxFun -> ExpMem.IxFun
expandIxFun var_to_pe = fixpointIterate (IxFun.substituteInIxFun var_to_pe)

(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
m <&&> n = (&&) <$> m <*> n

(<||>) :: Monad m => m Bool -> m Bool -> m Bool
m <||> n = (||) <$> m <*> n

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f xs = or <$> mapM f xs

whenM :: Monad m => m Bool -> m () -> m ()
whenM b m = do
  b' <- b
  when b' m

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f xs = do
  xs' <- filterM f xs
  return $ case xs' of
    x' : _ -> Just x'
    _ -> Nothing

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = do
  xs' <- mapM f xs
  return $ catMaybes xs'

sortByKeyM :: (Ord t, Monad m) => (a -> m t) -> [a] -> m [a]
sortByKeyM f xs = do
  rs <- mapM f xs
  return $ map fst $ L.sortBy (compare `on` snd) $ zip xs rs

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
  fullMapExpM mapper mapper_kernel e = case e of
    Op (ExpMem.Inner ke) -> Op . ExpMem.Inner <$> case ke of
      ExpMem.Combine a b c body ->
        ExpMem.Combine a b c <$> mapOnKernelBody mapper_kernel body
      ExpMem.GroupReduce a lambda b ->
        ExpMem.GroupReduce a
        <$> mapOnKernelLambda mapper_kernel lambda
        <*> pure b
      ExpMem.GroupScan a lambda b ->
        ExpMem.GroupScan a
        <$> mapOnKernelLambda mapper_kernel lambda
        <*> pure b
      ExpMem.GroupStream a b (ExpMem.GroupStreamLambda a1 b1 params0 params1 gsbody) c d ->
        ExpMem.GroupStream a b
        <$> (ExpMem.GroupStreamLambda a1 b1
             <$> mapM (mapOnKernelLParam mapper_kernel) params0
             <*> mapM (mapOnKernelLParam mapper_kernel) params1
             <*> mapOnKernelBody mapper_kernel gsbody
            )
        <*> pure c <*> pure d
      _ -> return ke
    _ -> mapExpM mapper e

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
  fullWalkExpM walker walker_kernel e = case e of
    Op (ExpMem.Inner ke) -> case ke of
      ExpMem.Combine _ _ _ body ->
        walkOnKernelBody walker_kernel body
      ExpMem.GroupReduce _ lambda _ ->
        walkOnKernelLambda walker_kernel lambda
      ExpMem.GroupScan _ lambda _ ->
        walkOnKernelLambda walker_kernel lambda
      ExpMem.GroupStream _ _ gslambda _ _ ->
        walkOnGroupStreamLambda walker_kernel gslambda
      _ -> return ()
    _ -> walkExpM walker e

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
  fullWalkAliasesExpM walker walker_kernel e = case e of
    Op (ExpMem.Inner ke) -> case ke of
      ExpMem.Combine _ _ _ body ->
        walkOnKernelBody walker_kernel body
      ExpMem.GroupReduce _ lambda _ ->
        walkOnKernelLambda walker_kernel lambda
      ExpMem.GroupScan _ lambda _ ->
        walkOnKernelLambda walker_kernel lambda
      ExpMem.GroupStream _ _ gslambda _ _ ->
        walkOnGroupStreamLambda walker_kernel gslambda
      _ -> return ()
    _ -> walkExpM walker e

walkOnGroupStreamLambda :: Monad m => KernelWalker lore m
                        -> ExpMem.GroupStreamLambda lore -> m ()
walkOnGroupStreamLambda walker_kernel (ExpMem.GroupStreamLambda _ _
                                       params0 params1 gsbody) = do
  mapM_ (walkOnKernelLParam walker_kernel) params0
  mapM_ (walkOnKernelLParam walker_kernel) params1
  walkOnKernelBody walker_kernel gsbody
