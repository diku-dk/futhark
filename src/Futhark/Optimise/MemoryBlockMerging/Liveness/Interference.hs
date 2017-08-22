{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find memory block interferences.
module Futhark.Optimise.MemoryBlockMerging.Liveness.Interference
  ( findInterferences
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (
  ExplicitMemorish, ExplicitMemory, InKernel)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


data Context = Context { ctxVarToMem :: VarMemMappings MemorySrc
                       , ctxFirstUses :: FirstUses
                       , ctxLastUses :: LastUses
                       }
  deriving (Show)

type InterferencesList = [(VName, Names)]

getInterferencesMap :: InterferencesList -> Interferences
getInterferencesMap = M.unionsWith S.union . map (uncurry M.singleton)

type CurrentlyAlive = Names

newtype FindM lore a = FindM
  { unFindM :: RWS Context InterferencesList CurrentlyAlive a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter InterferencesList,
            MonadState CurrentlyAlive)

type LoreConstraints lore = (ExplicitMemorish lore,
                             KernelInterferences lore,
                             CanShare lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

awaken :: VName -> FindM lore ()
awaken mem = modify $ S.insert mem

kill :: VName -> FindM lore ()
kill mem = modify $ S.delete mem

recordCurrentInterferences :: FindM lore ()
recordCurrentInterferences = do
  current <- get
  -- Interferences are commutative.  Reflect that in the resulting data.
  forM_ (S.toList current) $ \mem -> do
    let current' = S.delete mem current
    tell [(mem, current')]

-- | Find all interferences.
findInterferences :: LoreConstraints lore =>
                     VarMemMappings MemorySrc -> MemAliases ->
                     FirstUses -> LastUses -> FunDef lore
                  -> Interferences
findInterferences var_to_mem mem_aliases first_uses last_uses fundef =
  let context = Context { ctxVarToMem = var_to_mem
                        , ctxFirstUses = first_uses
                        , ctxLastUses = last_uses
                        }
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFunDefFParam
        lookInBody $ funDefBody fundef
      interferences = removeEmptyMaps $ makeCommutativeMap
                      $ expandWithAliases mem_aliases $ getInterferencesMap
                      $ snd $ evalRWS m context S.empty
  in interferences

lookInFunDefFParam :: LoreConstraints lore =>
                      FParam lore -> FindM lore ()
lookInFunDefFParam (Param var _) = do
  first_uses_var <- lookupEmptyable var <$> asks ctxFirstUses
  mapM_ awaken $ S.toList first_uses_var
  recordCurrentInterferences

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds res) = do
  mapM_ lookInStm bnds
  mapM_ lookInRes res

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds res) = do
  mapM_ lookInStm bnds
  mapM_ (lookInRes . kernelResultSubExp) res

isNoOp :: Exp lore -> Bool
isNoOp (BasicOp bop) = case bop of
  Scratch{} -> True
  _ -> False
isNoOp _ = False

awakenFirstUses :: [PatElem lore] -> FindM lore ()
awakenFirstUses patvalelems =
  forM_ patvalelems $ \(PatElem var _ _) -> do
    first_uses_var <- lookupEmptyable var <$> asks ctxFirstUses
    mapM_ awaken $ S.toList first_uses_var

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm stm@(Let (Pattern _patctxelems patvalelems) _ e)
  | isNoOp e =
      awakenFirstUses patvalelems
    -- There is no reason to record interferences if the current statement will
    -- not generate any code in the end.  We have this check to use the result
    -- index sharing analysis on loop bodies and not get bogged down by the
    -- result of a Scratch statement hanging around.
  | otherwise = do
      awakenFirstUses patvalelems

      stm_exceptions <- interferenceExceptions stm
      ((), stm_interferences) <- censor (const []) $ listen $ do
        recordCurrentInterferences
        fullWalkExpM walker walker_kernel e
      let stm_interferences' =
            map (\(k, vs) ->
                    (k, S.fromList
                        $ filter (\v -> not ((k, v) `L.elem` stm_exceptions
                                             || (v, k) `L.elem` stm_exceptions))
                        $ S.toList vs))
            stm_interferences
      tell stm_interferences'

      extra_kernel_interferences <- findKernelRaceConditionInterferences e
      tell extra_kernel_interferences

      forM_ patvalelems $ \(PatElem var _ _) -> do
        last_uses_var <- lookupEmptyable (FromStm var) <$> asks ctxLastUses
        mapM_ kill $ S.toList last_uses_var

      let debug = do
            putStrLn $ replicate 70 '~'
            putStrLn "lookInStm:"
            print stm
            putStrLn ("exceptions: " ++ show stm_exceptions)
            putStrLn "interferences': "
            forM_ (M.assocs $ getInterferencesMap stm_interferences') $ \(v, ns) ->
              putStrLn ("  " ++ pretty v ++ ": " ++ prettySet ns)
            unless (L.null extra_kernel_interferences)
              $ putStrLn ("extra kernel interferences: " ++
                          show extra_kernel_interferences)
            putStrLn $ replicate 70 '~'
      doDebug debug

        where walker = identityWalker
                { walkOnBody = lookInBody }
              walker_kernel = identityKernelWalker
                { walkOnKernelBody = coerce . lookInBody
                , walkOnKernelKernelBody = coerce . lookInKernelBody
                , walkOnKernelLambda = coerce . lookInBody . lambdaBody
                }

lookInRes :: LoreConstraints lore =>
             SubExp -> FindM lore ()
lookInRes (Var v) = do
  last_uses_var <- lookupEmptyable (FromRes v) <$> asks ctxLastUses
  mapM_ kill $ S.toList last_uses_var
lookInRes _ = return ()

-- Use index analysis to find any exceptions to the naive interference recorded
-- for an expression.
interferenceExceptions :: LoreConstraints lore =>
                          Stm lore -> FindM lore [(VName, VName)]
interferenceExceptions stm@(Let (Pattern _patctxelems patvalelems) _ e) = do
  -- If a memory is lastly used in this expression, we want to check it.
  readmems <- lastUsesInStm stm
  -- For every last memory use, we check it with every currently live memory.
  -- This is a bit broad, but easy.
  writemems_potential <- get

  concat <$>
    mapM (\readmem ->
            concat <$> mapM (
             \writemem -> do
               -- Can the read memory and the (potentially) written memory
               -- coexist?
               can_share <- canShare readmem writemem patvalelems e
               return [(readmem, writemem) | can_share])
            (S.toList writemems_potential))
    (S.toList readmems)

lastUsesInStm :: LoreConstraints lore =>
                 Stm lore -> FindM lore Names
lastUsesInStm stm@(Let _ _ e) = do
  let m = do
        lookLUInStm stm
        fullWalkExpM lu_walker lu_walker_kernel e
  last_uses <- asks ctxLastUses
  return $ snd $ evalRWS m last_uses ()
  where lookLUInStm :: Stm lore -> RWS LastUses Names () ()
        lookLUInStm (Let (Pattern _patctxelems patvalelems) _ _) =
          forM_ patvalelems $ \(PatElem patname _ _) -> do
            lus <- lookupEmptyable (FromStm patname) <$> ask
            tell lus

        lu_walker = identityWalker
          { walkOnBody = mapM_ lookLUInStm . bodyStms }
        lu_walker_kernel = identityKernelWalker
          { walkOnKernelBody = mapM_ lookLUInStm . bodyStms
          , walkOnKernelKernelBody = mapM_ lookLUInStm . kernelBodyStms
          , walkOnKernelLambda = mapM_ lookLUInStm . bodyStms . lambdaBody
          }

firstUsesInExp :: LoreConstraints lore =>
                  Exp lore -> FindM lore [(VName, ExpMem.IxFun)]
firstUsesInExp e = do
  let m = lookFUInExp e
  first_uses <- asks ctxFirstUses
  return $ snd $ evalRWS m first_uses ()
  where lookFUInStm :: LoreConstraints lore =>
                       Stm lore -> RWS FirstUses [(VName, ExpMem.IxFun)] () ()
        lookFUInStm (Let (Pattern _patctxelems patvalelems) _ e_stm) = do
          forM_ patvalelems $ \(PatElem patname _ membound) ->
            case membound of
              ExpMem.ArrayMem _ _ _ _ ixfun -> do
                fus <- lookupEmptyable patname <$> ask
                forM_ fus $ \fu -> tell [(fu, ixfun)]
              _ -> return ()
          lookFUInExp e_stm

        lookFUInExp :: LoreConstraints lore =>
                       Exp lore -> RWS FirstUses [(VName, ExpMem.IxFun)] () ()
        lookFUInExp = fullWalkExpM fu_walker fu_walker_kernel
          where fu_walker = identityWalker
                  { walkOnBody = mapM_ lookFUInStm . bodyStms }
                fu_walker_kernel = identityKernelWalker
                  { walkOnKernelBody = mapM_ lookFUInStm . bodyStms
                  , walkOnKernelKernelBody = mapM_ lookFUInStm . kernelBodyStms
                  , walkOnKernelLambda = mapM_ lookFUInStm . bodyStms . lambdaBody
                  }

class KernelInterferences lore where
  findKernelRaceConditionInterferences :: Exp lore -> FindM lore [(VName, Names)]

instance KernelInterferences ExplicitMemory where
  findKernelRaceConditionInterferences e = case e of
    Op (ExpMem.Inner Kernel{}) -> do
      fus <- firstUsesInExp e
      return $ map (\(t, u) -> (fst t, S.singleton (fst u)))
        $ filter interferes [(t, u) | t <- fus, u <- fus]
    _ -> return []
    where interferes ((v0, ixfun0), (v1, ixfun1)) =
            v0 /= v1 && (ixFunHasIndex ixfun0 || ixFunHasIndex ixfun1) &&
            not (ixFunsContained ixfun0 ixfun1)

-- Does an index function contain an Index expression?
--
-- If the index function of the memory annotation uses an index, it means that
-- the array creation does not refer to the entire array.  It is an array
-- creation, but only partially: It creates part of the array, and another part
-- is created in another loop iteration or kernel thread.  The danger in
-- declaring this memory a first use lies in how it can then be reused later in
-- the iteration/thread by some memory with a *different* index in its memory
-- annotation index function, which can affect reads in other threads.
ixFunHasIndex :: ExpMem.IxFun -> Bool
ixFunHasIndex ixfun = case ixfun of
  IxFun.Direct _ -> False
  IxFun.Permute ixfun' _ -> ixFunHasIndex ixfun'
  IxFun.Rotate ixfun' _ -> ixFunHasIndex ixfun'
  IxFun.Index{} -> True
  IxFun.Reshape ixfun' _ -> ixFunHasIndex ixfun'
  IxFun.Repeat ixfun' _ _ -> ixFunHasIndex ixfun'

-- FIXME: This can be less conservative.  The two index functions should fully
-- overlap in their index ranges.
ixFunsContained :: ExpMem.IxFun -> ExpMem.IxFun -> Bool
ixFunsContained ixfun0 ixfun1 = ixfun0 `primEq` ixfun1
  where primEq a b = case (a, b) of
          (IxFun.Direct sa, IxFun.Direct sb) ->
            sa == sb
          (IxFun.Permute a1 pa, IxFun.Permute b1 pb) ->
            a1 `primEq` b1 && pa == pb
          (IxFun.Rotate a1 ia, IxFun.Rotate b1 ib) ->
            a1 `primEq` b1 && ia == ib
          (IxFun.Index a1 sa, IxFun.Index b1 sb) ->
            a1 `primEq` b1 && sa == sb
          (IxFun.Reshape a1 sa, IxFun.Reshape b1 sb) ->
            a1 `primEq` b1 && sa == sb
          (IxFun.Repeat a1 ssa sa, IxFun.Repeat b1 ssb sb) ->
            a1 `primEq` b1 && ssa == ssb && sa == sb
          _ -> False

instance KernelInterferences InKernel where
  findKernelRaceConditionInterferences _ = return []

-- If an expression reads from one array and writes to another array, we want to
-- check if the two arrays can share the same memory.  If that is the case, we
-- can say that they do *not* interfere, which they otherwise would.
-- Conservatively, it is the case when a loop body looks like
--
--     ...
--     read from source at index i
--     ...
--     write to destination at index i
--     ...
--
-- where there are no additional reads or writes in the '...' parts, and when a
-- kernel body looks like
--
--     ...
--     read from source at index i
--     ...
--
-- and returns in the destination memory.
--
-- Essentially, if the index is the same for both operations, and neither array
-- is used anywhere else in the body, we're fine.
--
-- This is too conservative for some cases.  For instance, we currently handle
-- only the simplest of kernels (one input, one output, returns in thread).
--
-- FIXME: Add support for kernels with multiple gtids, and loop bodies with
-- nestings -- both required to support multidimensional maps.  Remember to
-- check that the index functions match, e.g. if one of the arrays is
-- transposed.
--
-- FIXME: Add support for other 'KernelResult's?
--
-- FIXME: Add support for other 'KernelExp's?

class CanShare lore where
  canShare :: VName -> VName -> [PatElem lore] -> Exp lore -> FindM lore Bool

instance CanShare ExplicitMemory where
  canShare readmem writemem patvalelems e = case e of
    DoLoop _ _ loopform loopbody ->
      canShareLoopBody readmem writemem loopform loopbody
    Op (ExpMem.Inner (Kernel _ _ kernelspace _ kernelbody)) ->
      coerce $ canShareKernel readmem writemem patvalelems kernelspace kernelbody
    _ -> return False

instance CanShare InKernel where
  canShare readmem writemem _patvalelems e = case e of
    DoLoop _ _ loopform loopbody ->
      canShareLoopBody readmem writemem loopform loopbody
    Op (ExpMem.Inner _ke) -> return False -- FIXME
    _ -> return False

canShareLoopBody :: LoreConstraints lore =>
                    VName -> VName -> LoopForm lore ->
                    Body lore -> FindM lore Bool
canShareLoopBody readmem writemem loopform body
  | ForLoop i _ _ _ <- loopform =
      canShareReadWriteCheck readmem writemem i $ bodyStms body
  | otherwise = return False

canShareKernel :: VName -> VName -> [PatElem InKernel] -> KernelSpace ->
                  KernelBody InKernel -> FindM InKernel Bool
canShareKernel readmem writemem patvalelems kernelspace kernelbody
  -- Very conservative checks right now.
  | [PatElem resvar _ _] <- patvalelems
  , [(gtid, _gsize)] <- spaceDimensions kernelspace
  , [ThreadsReturn{}] <- kernelBodyResult kernelbody = do
      resvar_mem <- M.lookup resvar <$> asks ctxVarToMem
      if (memSrcName <$> resvar_mem) == Just writemem
        then canShareReadCheck readmem gtid $ kernelBodyStms kernelbody
        else return False
  | otherwise = return False

isBasicOp :: Exp lore -> Bool
isBasicOp BasicOp{} = True
isBasicOp _ = False

canShareReadCheck :: LoreConstraints lore =>
                     VName -> VName -> [Stm lore] -> FindM lore Bool
canShareReadCheck readmem index stms
  | all (isBasicOp . stmExp) stms = do
    stms' <- filterM (memUsedInExp readmem . stmExp) stms

    res <- case stms' of
      [Let (Pattern [] [PatElem _ BindVar _]) _
       (BasicOp (Index _ readvar [DimFix (Var e_index)]))] -> do
        readvar_mem <- M.lookup readvar <$> asks ctxVarToMem
        return (e_index == index
                && (memSrcName <$> readvar_mem) == Just readmem)
      _ -> return False

    let debug = do
          putStrLn $ replicate 70 '~'
          putStrLn ("canShareReadCheck?  readmem: "
                    ++ pretty readmem ++ "; index: "
                    ++ pretty index)
          print stms'
          putStrLn $ replicate 70 '~'
    withDebug debug $ return res
  | otherwise = return False

canShareReadWriteCheck :: LoreConstraints lore =>
                          VName -> VName -> VName -> [Stm lore] -> FindM lore Bool
canShareReadWriteCheck readmem writemem index stms
  | all (isBasicOp . stmExp) stms = do
    stms' <- filterM (\stm ->
                        memUsedInExp readmem (stmExp stm) <||>
                        memUsedInPatElems writemem
                        (patternValueElements (stmPattern stm))
                     ) stms

    res <- case stms' of
      [Let (Pattern [] [PatElem _ BindVar _]) _
       (BasicOp (Index _ readvar [DimFix (Var e_index_read)])),

       Let (Pattern [] [PatElem writevar (BindInPlace _ _ [DimFix (Var e_index_write)]) _]) _
       (BasicOp SubExp{})] -> do
        readvar_mem <- M.lookup readvar <$> asks ctxVarToMem
        writevar_mem <- M.lookup writevar <$> asks ctxVarToMem
        return (e_index_read == index
                && e_index_write == index
                && (memSrcName <$> readvar_mem) == Just readmem
                && (memSrcName <$> writevar_mem) == Just writemem)
      _ -> return False

    let debug = do
          putStrLn $ replicate 70 '~'
          putStrLn ("canShareReadWriteCheck?  readmem: "
                    ++ pretty readmem ++ "; writemem: "
                    ++ pretty writemem ++ "; index: "
                    ++ pretty index)
          print (length stms')
          forM_ stms' print
          print res
          putStrLn $ replicate 70 '~'
    withDebug debug $ return res
  | otherwise = return False

memUsedInExp :: LoreConstraints lore =>
                VName -> Exp lore -> FindM lore Bool
memUsedInExp mem e = do
  e_mems <- catMaybes <$> mapM (\v -> M.lookup v <$> asks ctxVarToMem)
            (S.toList $ freeInExp e)
  return $ case e_mems of
    [e_mem] -> memSrcName e_mem == mem
    _ -> False

memUsedInPatElems :: LoreConstraints lore =>
                     VName -> [PatElem lore] -> FindM lore Bool
memUsedInPatElems mem patelems = do
  patelems_mems <- catMaybes <$> mapM (\(PatElem v _ _) ->
                                         M.lookup v <$> asks ctxVarToMem)
                   patelems
  return $ case patelems_mems of
    [patelems_mem] -> memSrcName patelems_mem == mem
    _ -> False
