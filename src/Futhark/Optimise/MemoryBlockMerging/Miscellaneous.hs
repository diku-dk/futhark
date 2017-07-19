{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | I didn't know where else to put this.  Perpetually in need of a cleanup.
module Futhark.Optimise.MemoryBlockMerging.Miscellaneous where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Monoid()
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import System.IO.Unsafe (unsafePerformIO) -- Just for debugging!

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       (ExplicitMemory, InKernel, ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Text.PrettyPrint.Mainland (Pretty)
import Futhark.Util (unixEnvironment)

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
--                 . M.mapWithKey S.delete

prettySet :: Pretty a => S.Set a -> String
prettySet = L.intercalate ", " . map pretty . S.toList


createsNewArrayWithoutKernel :: Exp ExplicitMemory -> Bool
createsNewArrayWithoutKernel e = case e of
  Op (ExpMem.Inner ExpMem.Kernel{}) -> True -- Necessary?
  _ -> createsNewArrayBase e

createsNewArrayInKernel :: Exp InKernel -> Bool
createsNewArrayInKernel e = case e of
  Op (ExpMem.Inner ExpMem.GroupReduce{}) -> True
  Op (ExpMem.Inner ExpMem.GroupScan{}) -> True
  Op (ExpMem.Inner ExpMem.GroupStream{}) -> True
  Op (ExpMem.Inner ExpMem.Combine{}) -> True
  _ -> createsNewArrayBase e

createsNewArrayBase :: ExplicitMemorish lore
                    => Exp lore -> Bool
createsNewArrayBase e = case e of
  BasicOp Partition{} -> True
  BasicOp Replicate{} -> True
  BasicOp Iota{} -> True
  BasicOp Manifest{} -> True
  BasicOp ExpMem.Copy{} -> True
  BasicOp Concat{} -> True
  BasicOp ArrayLit{} -> True

  -- While a Scratch does create a new array, it does not actually read or write
  -- anything to it, so it is not interesting from a memory use viewpoint.
  -- Instead we focus on (in sequential code) loops over memory, and how they
  -- read and write.  Loops over scalars should be ignored.  See FirstUse.hs for
  -- an example.

  -- BasicOp Scratch{} -> True

  _ -> False

class ArrayUtils lore where
  createsNewArray :: Exp lore -> Bool

instance ArrayUtils ExplicitMemory where
  createsNewArray = createsNewArrayWithoutKernel

instance ArrayUtils InKernel where
  createsNewArray = createsNewArrayInKernel


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

transformFromVarMemMappings :: VarMemMappings MemoryLoc -> FunDef ExplicitMemory
                            -> FunDef ExplicitMemory
transformFromVarMemMappings var_to_mem fundef =
  let body' = transformBody $ funDefBody fundef
  in fundef { funDefBody = body' }

  where transformBody :: Body ExplicitMemory -> Body ExplicitMemory
        transformBody (Body () bnds res) =
          let bnds' = map transformStm bnds
          in Body () bnds' res

        transformStm :: Stm ExplicitMemory -> Stm ExplicitMemory
        transformStm (Let (Pattern patctxelems patvalelems) () e) =
          let patvalelems' = map transformPatValElem patvalelems
              e' = mapExp mapper e
              e'' = case e' of
                DoLoop mergectxparams mergevalparams loopform body ->
                  -- More special loop handling because of its extra
                  -- pattern-like info.
                  let mergevalparams' = map transformMergeValParam mergevalparams
                      mergectxparams' = map transformMergeCtxParam mergectxparams

                  -- The body of a loop can return a memory block in its
                  -- results.  This is the memory block used by a variable which
                  -- is also part of the results.  If the memory block of that
                  -- variable is changed, we need a way to record that the
                  -- memory block in the body result also needs to change.
                  --
                  -- Should be fine?
                      zipped = zip [(0::Int)..] (patctxelems ++ patvalelems) -- should maybe be patvalelems' (important?)

                      findMemLinks (i, PatElem _x _binding (ExpMem.ArrayMem _ _ _ xmem _)) =
                        case L.find (\(_, PatElem ymem _ _) -> ymem == xmem) zipped of
                          Just (j, _) -> Just (j, i)
                          Nothing -> Nothing
                      findMemLinks _ = Nothing

                      mem_links = mapMaybe findMemLinks zipped

                      res = bodyResult body

                      fixResRecord i se
                        | Var _mem <- se
                        , Just j <- L.lookup i mem_links
                        , Var related_var <- res L.!! j
                        , Just mem_new <- M.lookup related_var var_to_mem =
                            Var $ memLocName mem_new
                        | otherwise = se

                      res' = zipWith fixResRecord [(0::Int)..] res
                      body' = body { bodyResult = res' }
                  in DoLoop mergectxparams' mergevalparams' loopform body'
                _ -> e'
          in Let (Pattern patctxelems patvalelems') () e''
          where mapper = identityMapper { mapOnBody = \_ body -> return $ transformBody body }

        -- Update the actual memory block referred to by a context memory block
        -- in a loop.
        transformMergeCtxParam :: (FParam ExplicitMemory, SubExp)
                               -> (FParam ExplicitMemory, SubExp)
        transformMergeCtxParam (param@(Param ctxmem ExpMem.MemMem{}), mem) =
          let mem' = fromMaybe mem ((Var . memLocName) <$> M.lookup ctxmem var_to_mem)
          in (param, mem')
        transformMergeCtxParam t = t

        transformMergeValParam :: (FParam ExplicitMemory, SubExp)
                               -> (FParam ExplicitMemory, SubExp)
        transformMergeValParam (Param x membound, se) =
          let membound' = newMemBound membound x
          in (Param x membound', se)

        transformPatValElem :: PatElem ExplicitMemory -> PatElem ExplicitMemory
        transformPatValElem (PatElem x bindage membound) =
          let membound' = newMemBound membound x
          in PatElem x bindage membound'

        newMemBound membound = fromMaybe membound . newMemBound' membound

        newMemBound' :: ExpMem.MemBound u -> VName -> Maybe (ExpMem.MemBound u)
        newMemBound' membound x
          | ExpMem.ArrayMem pt shape u _ _ <- membound
          , Just (MemoryLoc mem ixfun) <- M.lookup x var_to_mem =
              Just $ ExpMem.ArrayMem pt shape u mem ixfun
          | otherwise = Nothing
