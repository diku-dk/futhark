{-# LANGUAGE TypeFamilies, LambdaCase #-}
module Futhark.CodeGen.KernelImpGen
  ( compileProg
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List
import Data.Monoid

import Prelude

import Futhark.MonadFreshNames
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import qualified Futhark.CodeGen.KernelImp as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.Util

compileProg :: Prog -> Either String Imp.Program
compileProg = ImpGen.compileProg kernelCompiler

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.ExpCompiler Imp.Kernel

kernelCompiler (ImpGen.Destination dest) (LoopOp (Map _ lam arrs)) = do
  -- The number of threads - one per input element.
  num_threads <- ImpGen.compileSubExp <$> arraysSize 0 <$> mapM lookupType arrs

  -- Extract allocations from the body.
  (body, thread_allocs) <- either fail return $ extractKernelAllocations lam

  -- We expand the allocations by multiplying their size with the
  -- number of kernel threads.
  let expanded_allocs =
        HM.map (Imp.BinOp Times num_threads . ImpGen.compileSubExp) thread_allocs

  -- Fix every reference to the memory blocks to be offset by the
  -- thread number.
  thread_num <- newVName "thread_num"
  let alloc_offsets =
        HM.map (SE.STimes (SE.Id thread_num Int) . SE.intSubExpToScalExp) thread_allocs
      body' = offsetMemorySummariesInBody alloc_offsets body

  allocMemoryBlocks expanded_allocs $ makeAllMemoryGlobal $ do
    kernelbody <- ImpGen.collect $
                  ImpGen.declaringLParams (lambdaParams lam) $ do
                    zipWithM_ (readThreadParams thread_num) (lambdaParams lam) arrs
                    ImpGen.compileBindings (bodyBindings body') $
                      zipWithM_ (writeThreadResult thread_num) dest $ bodyResult body'

    arr_mems <- forM arrs $ \arr -> do
      ImpGen.MemLocation mem _ _ <- ImpGen.arrayLocation arr
      return mem

    copy_in <- liftM catMaybes $
               forM (HS.toList $ HS.fromList arr_mems <> freeInBody body <>
                     mconcat (map freeIn $ lambdaParams lam)) $ \var ->
      if var `elem` map paramName (lambdaParams lam)
        then return Nothing
        else do t <- lookupType var
                case t of
                  Array {} -> return Nothing
                  Mem memsize -> Just <$> Imp.CopyMemory var <$> ImpGen.subExpToDimSize memsize
                  Basic bt ->
                    if bt == Cert
                    then return Nothing
                    else return $ Just $ Imp.CopyScalar var bt

    -- Copy what memory to copy out.  Must be allocated on device before
    -- kernel execution anyway.
    copy_out <- liftM catMaybes $ forM dest $ \case
      (ImpGen.ArrayDestination
       (ImpGen.CopyIntoMemory
        (ImpGen.MemLocation mem _ _)) _) -> do
        memsize <- ImpGen.entryMemSize <$> ImpGen.lookupMemory mem
        return $ Just $ Imp.CopyMemory mem memsize
      _ ->
        return Nothing

    kernel_size <- ImpGen.subExpToDimSize =<< (arraysSize 0 <$> mapM lookupType arrs)

    ImpGen.emit $ Imp.Op Imp.Kernel {
        Imp.kernelThreadNum = thread_num
      , Imp.kernelBody = kernelbody
      , Imp.kernelCopyIn = copy_in
      , Imp.kernelCopyOut = copy_out
      , Imp.kernelSize = kernel_size
      }
    return ImpGen.Done

kernelCompiler _ e =
  return $ ImpGen.CompileExp e

-- | Change every memory block to be in the global address space.
-- This is fairly hacky and can be improved once the Futhark-level
-- memory representation supports address spaces.  This only affects
-- generated code - we still need to make sure that the memory is
-- actually present on the device (and declared as variables in the
-- kernel).
makeAllMemoryGlobal :: ImpGen.ImpM Imp.Kernel a
                    -> ImpGen.ImpM Imp.Kernel a
makeAllMemoryGlobal =
  local $ \env -> env { ImpGen.envVtable = HM.map globalMemory $ ImpGen.envVtable env }
  where globalMemory (ImpGen.MemVar entry) =
          ImpGen.MemVar entry { ImpGen.entryMemSpace = Just "global" }
        globalMemory entry =
          entry

allocMemoryBlocks :: HM.HashMap VName Imp.Exp -> ImpGen.ImpM Imp.Kernel a
                  -> ImpGen.ImpM Imp.Kernel a
allocMemoryBlocks = allocMemoryBlocks' . HM.toList
  where allocMemoryBlocks' [] m = m
        allocMemoryBlocks' ((memname, size):allocs) m = do
          sizename <- newVName "size"
          let sizeentry = ImpGen.ScalarVar $ ImpGen.ScalarEntry Int
              mementry = ImpGen.MemVar ImpGen.MemEntry {
                  ImpGen.entryMemSize = Imp.VarSize sizename
                , ImpGen.entryMemSpace = Nothing
                }
          ImpGen.declaringVarEntry sizename sizeentry $ do
            ImpGen.emit $ Imp.SetScalar sizename size
            ImpGen.declaringVarEntry memname mementry $ do
              ImpGen.emit $ Imp.Allocate memname $ Imp.ScalarVar sizename
              allocMemoryBlocks' allocs m

writeThreadResult :: VName -> ImpGen.ValueDestination -> SubExp
                  -> ImpGen.ImpM Imp.Kernel ()
writeThreadResult thread_num
  (ImpGen.ArrayDestination
   (ImpGen.CopyIntoMemory
    destloc@(ImpGen.MemLocation mem _ _)) _) se = do
  set <- subExpType se
  space <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory mem
  let i = ImpGen.elements $ Imp.ScalarVar thread_num
  case set of
    Basic bt ->
      ImpGen.compileResultSubExp (ImpGen.ArrayElemDestination mem bt space i) se
    _ -> do
      memloc <- ImpGen.indexArray destloc [ImpGen.varIndex thread_num]
      let dest = ImpGen.ArrayDestination (ImpGen.CopyIntoMemory memloc) $
                 replicate (arrayRank set) Nothing
      ImpGen.compileResultSubExp dest se
writeThreadResult _ _ _ =
  fail "Cannot handle kernel that does not return an array."

readThreadParams :: VName -> LParam -> VName
                 -> ImpGen.ImpM Imp.Kernel ()
readThreadParams thread_num param arr = do
  t <- lookupType arr
  when (arrayRank t == 1) $ do
    (srcmem, space, srcoffset) <-
      ImpGen.fullyIndexArray arr [SE.Id thread_num Int]
    ImpGen.emit $ Imp.SetScalar (paramName param) $
      ImpGen.index srcmem srcoffset (elemType t) space

-- | Returns a map from memory block names to their size in bytes,
-- as well as the lambda body where all the allocations have been removed.
-- Only looks at allocations in the immediate body - if there are any
-- further down, we will fail later.  If the size of one of the
-- allocations is not free in the lambda, we return 'Left' and an
-- error message.
extractKernelAllocations :: Lambda -> Either String (Body, HM.HashMap VName SubExp)
extractKernelAllocations lam = do
  (allocs, bnds) <- mapAccumLM isAlloc HM.empty lambdaBindings
  return ((lambdaBody lam) { bodyBindings = catMaybes bnds }, allocs)
  where boundHere = HS.fromList $
                    map paramName (lambdaParams lam) ++
                    concatMap (patternNames . bindingPattern) lambdaBindings

        lambdaBindings = bodyBindings $ lambdaBody lam

        isAlloc _ (Let (Pattern [] [patElem]) () (PrimOp (Alloc (Var v))))
          | v `HS.member` boundHere =
            throwError $ "Size " ++ pretty v ++
            " for block " ++ pretty patElem ++
            " is not lambda-invariant"

        isAlloc allocs (Let (Pattern [] [patElem]) () (PrimOp (Alloc size))) =
          return (HM.insert (patElemName patElem) size allocs, Nothing)

        isAlloc allocs bnd =
          return (allocs, Just bnd)

offsetMemorySummariesInBinding :: HM.HashMap VName SE.ScalExp -> Binding -> Binding
offsetMemorySummariesInBinding offsets (Let pat attr e) =
  Let
  (offsetMemorySummariesInPattern offsets pat)
  attr
  (offsetMemorySummariesInExp offsets e)

offsetMemorySummariesInPattern :: HM.HashMap VName SE.ScalExp -> Pattern -> Pattern
offsetMemorySummariesInPattern offsets (Pattern ctx vals) =
  Pattern (map inspect ctx) (map inspect vals)
  where inspect patElem =
          patElem { patElemLore =
                       offsetMemorySummariesInMemSummary offsets $ patElemLore patElem }

offsetMemorySummariesInFParam :: HM.HashMap VName SE.ScalExp -> FParam -> FParam
offsetMemorySummariesInFParam offsets fparam =
  fparam { paramLore = offsetMemorySummariesInMemSummary offsets $ paramLore fparam }

offsetMemorySummariesInMemSummary :: HM.HashMap VName SE.ScalExp -> MemSummary -> MemSummary
offsetMemorySummariesInMemSummary offsets (MemSummary mem ixfun)
  | Just offset <- HM.lookup mem offsets =
      MemSummary mem $ IxFun.offsetUnderlying ixfun offset
offsetMemorySummariesInMemSummary _ summary =
  summary

offsetMemorySummariesInExp :: HM.HashMap VName SE.ScalExp -> Exp -> Exp
offsetMemorySummariesInExp offsets (LoopOp (DoLoop res merge form body)) =
  LoopOp $ DoLoop res (zip mergeparams' mergeinit) form body'
  where (mergeparams, mergeinit) = unzip merge
        body' = offsetMemorySummariesInBody offsets body
        mergeparams' = map (offsetMemorySummariesInFParam offsets) mergeparams
offsetMemorySummariesInExp offsets e = mapExp recurse e
  where recurse = identityMapper { mapOnBody = return . offsetMemorySummariesInBody offsets
                                 }

offsetMemorySummariesInBody :: HM.HashMap VName SE.ScalExp -> Body -> Body
offsetMemorySummariesInBody offsets (Body attr bnds res) =
  Body attr (map (offsetMemorySummariesInBinding offsets) bnds) res
