{-# LANGUAGE TypeFamilies, LambdaCase #-}
module Futhark.CodeGen.KernelImpGen
  ( compileProg
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.KernelImp as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.Analysis.ScalExp as SE
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun

type CallKernelGen = ImpGen.ImpM Imp.CallKernel
type InKernelGen = ImpGen.ImpM Imp.InKernel

callKernelOperations :: ImpGen.Operations Imp.CallKernel
callKernelOperations =
  ImpGen.Operations { ImpGen.opsExpCompiler = kernelCompiler
                    , ImpGen.opsCopyCompiler = copyCompiler
                    }

inKernelOperations :: ImpGen.Operations Imp.InKernel
inKernelOperations = ImpGen.defaultOperations

compileProg :: Prog -> Either String Imp.Program
compileProg = ImpGen.compileProg callKernelOperations $ Imp.Space "device"

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.ExpCompiler Imp.CallKernel

kernelCompiler
  (ImpGen.Destination dest)
  (LoopOp (Kernel _ w global_thread_index ispace inps returns body)) = do

  kernel_size <- ImpGen.subExpToDimSize w

  let global_thread_index_param = Imp.ScalarParam global_thread_index Int
      shape = map (ImpGen.compileSubExp . snd) ispace
      indices = map fst ispace

  let indices_lparams = [ Param (Ident index $ Basic Int) Scalar | index <- indices ]
      bound_in_kernel = global_thread_index : indices ++ map kernelInputName inps
      kernel_bnds = bodyBindings body

      index_expressions = unflattenIndex shape $ Imp.ScalarVar global_thread_index
      set_indices = forM_ (zip indices index_expressions) $ \(i, x) ->
        ImpGen.emit $ Imp.SetScalar i x

      read_params = mapM_ readKernelInput inps

      perms = map snd returns
      write_result =
        sequence_ $ zipWith3 (writeThreadResult indices) perms dest $ bodyResult body

  makeAllMemoryGlobal $ do
    kernelbody <- ImpGen.subImpM inKernelOperations $
                  ImpGen.withParams [global_thread_index_param] $
                  ImpGen.declaringLParams (indices_lparams++map kernelInputParam inps) $ do
                    ImpGen.comment "compute thread index" set_indices
                    ImpGen.comment "read kernel parameters" read_params
                    ImpGen.compileBindings kernel_bnds $
                     ImpGen.comment "write kernel result" write_result

    -- Find the memory blocks containing the output arrays.
    let dest_mems = mapMaybe destMem dest
        destMem (ImpGen.ArrayDestination
                 (ImpGen.CopyIntoMemory
                  (ImpGen.MemLocation mem _ _)) _) =
          Just mem
        destMem _ =
          Nothing

    -- Compute the variables that we need to pass to the kernel.
    reads_from <- readsFromSet $
                  freeIn kernelbody `HS.difference`
                  HS.fromList (dest_mems <> bound_in_kernel)

    -- Compute what memory to copy out.  Must be allocated on device
    -- before kernel execution anyway.
    writes_to <- liftM catMaybes $ forM dest $ \case
      (ImpGen.ArrayDestination
       (ImpGen.CopyIntoMemory
        (ImpGen.MemLocation mem _ _)) _) -> do
        memsize <- ImpGen.entryMemSize <$> ImpGen.lookupMemory mem
        return $ Just $ Imp.MemoryUse mem memsize
      _ ->
        return Nothing

    ImpGen.emit $ Imp.Op $ Imp.Kernel Imp.GenericKernel {
        Imp.kernelThreadNum = global_thread_index
      , Imp.kernelBody = kernelbody
      , Imp.kernelUses = nub $ reads_from ++ writes_to
      , Imp.kernelSize = kernel_size
      }
    return ImpGen.Done

-- We generate a simple kernel for itoa and replicate.
kernelCompiler target (PrimOp (Iota n)) = do
  i <- newVName "i"
  global_thread_index <- newVName "global_thread_index"
  kernelCompiler target $
    LoopOp $ Kernel [] n global_thread_index [(i,n)] [] [(Basic Int,[0])] (Body () [] [Var i])
kernelCompiler target (PrimOp (Replicate n v)) = do
  i <- newVName "i"
  global_thread_index <- newVName "global_thread_index"
  t <- subExpType v
  kernelCompiler target $
    LoopOp $ Kernel [] n global_thread_index [(i,n)] [] [(t,[0..arrayRank t])] (Body () [] [v])
kernelCompiler _ e =
  return $ ImpGen.CompileExp e

copyCompiler :: ImpGen.CopyCompiler Imp.CallKernel
copyCompiler bt
  destloc@(ImpGen.MemLocation destmem destshape destIxFun)
  srcloc@(ImpGen.MemLocation srcmem _ srcIxFun)

  | Just (destoffset, srcoffset) <- isMapTranspose bt destloc srcloc,
    [num_arrays, size_x, size_y] <- map ImpGen.sizeToExp destshape =
  ImpGen.emit $ Imp.Op $ Imp.MapTranspose bt destmem destoffset srcmem srcoffset
  num_arrays size_x size_y

  | otherwise = do
  global_thread_index <- newVName "copy_global_thread_index"

  -- Note that the shape of the destination and the source are
  -- necessarily the same.
  let shape = map ImpGen.sizeToExp destshape
      shape_se = map ImpGen.sizeToScalExp destshape
      dest_is = unflattenIndex shape_se $ ImpGen.varIndex global_thread_index
      src_is = dest_is

  makeAllMemoryGlobal $ do
    (_, destspace, destidx) <- ImpGen.fullyIndexArray' destloc dest_is bt
    (_, srcspace, srcidx) <- ImpGen.fullyIndexArray' srcloc src_is bt

    let body = ImpGen.write destmem destidx bt destspace $
               ImpGen.index srcmem srcidx bt srcspace

    destmem_size <- ImpGen.entryMemSize <$> ImpGen.lookupMemory destmem
    let writes_to = [Imp.MemoryUse destmem destmem_size]

    reads_from <- readsFromSet $
                  HS.singleton srcmem <>
                  freeIn destIxFun <> freeIn srcIxFun <> freeIn destshape

    kernel_size <- newVName "copy_kernel_size"
    ImpGen.emit $ Imp.DeclareScalar kernel_size Int
    ImpGen.emit $ Imp.SetScalar kernel_size $ product shape

    ImpGen.emit $ Imp.Op $ Imp.Kernel Imp.GenericKernel {
        Imp.kernelThreadNum = global_thread_index
      , Imp.kernelSize = Imp.VarSize kernel_size
      , Imp.kernelUses = nub $ reads_from ++ writes_to
      , Imp.kernelBody = body
      }

readsFromSet :: Names -> ImpGen.ImpM op [Imp.KernelUse]
readsFromSet free =
  liftM catMaybes $
  forM (HS.toList free) $ \var -> do
    t <- lookupType var
    case t of
      Array {} -> return Nothing
      Mem memsize -> Just <$> (Imp.MemoryUse var <$>
                               ImpGen.subExpToDimSize memsize)
      Basic bt ->
        if bt == Cert
        then return Nothing
        else return $ Just $ Imp.ScalarUse var bt

-- | Change every memory block to be in the global address space.
-- This is fairly hacky and can be improved once the Futhark-level
-- memory representation supports address spaces.  This only affects
-- generated code - we still need to make sure that the memory is
-- actually present on the device (and declared as variables in the
-- kernel).
makeAllMemoryGlobal :: CallKernelGen a
                    -> CallKernelGen a
makeAllMemoryGlobal =
  local $ \env -> env { ImpGen.envVtable = HM.map globalMemory $ ImpGen.envVtable env
                      , ImpGen.envDefaultSpace = Imp.Space "global"
                      }
  where globalMemory (ImpGen.MemVar entry) =
          ImpGen.MemVar entry { ImpGen.entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

writeThreadResult :: [VName] -> [Int] -> ImpGen.ValueDestination -> SubExp
                  -> InKernelGen ()
writeThreadResult thread_idxs perm
  (ImpGen.ArrayDestination
   (ImpGen.CopyIntoMemory
    (ImpGen.MemLocation mem dims ixfun)) _) se = do
  set <- subExpType se

  let ixfun' = IxFun.permute ixfun perm
      destloc' = ImpGen.MemLocation mem (rearrangeShape perm dims) ixfun'

  space <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory mem
  let is = map ImpGen.varIndex thread_idxs
  case set of
    Basic bt -> do
      (_, _, elemOffset) <-
        ImpGen.fullyIndexArray' destloc' is bt
      ImpGen.compileResultSubExp (ImpGen.ArrayElemDestination mem bt space elemOffset) se
    _ -> do
      memloc <- ImpGen.indexArray destloc' is
      let dest = ImpGen.ArrayDestination (ImpGen.CopyIntoMemory memloc) $
                 replicate (arrayRank set) Nothing
      ImpGen.compileResultSubExp dest se
writeThreadResult _ _ _ _ =
  fail "Cannot handle kernel that does not return an array."

readKernelInput :: KernelInput ExplicitMemory
                -> InKernelGen ()
readKernelInput inp =
  when (basicType t) $ do
    (srcmem, space, srcoffset) <-
      ImpGen.fullyIndexArray arr $ map SE.intSubExpToScalExp is
    ImpGen.emit $ Imp.SetScalar name $
      ImpGen.index srcmem srcoffset (elemType t) space
  where arr = kernelInputArray inp
        name = kernelInputName inp
        t = kernelInputType inp
        is = kernelInputIndices inp

isMapTranspose :: BasicType -> ImpGen.MemLocation -> ImpGen.MemLocation
               -> Maybe (Imp.Exp, Imp.Exp)
isMapTranspose bt
  (ImpGen.MemLocation _ _ destIxFun)
  (ImpGen.MemLocation _ _ srcIxFun)
  | Just (dest_offset, perm) <- IxFun.rearrangeWithOffset destIxFun,
    Just src_offset <- IxFun.linearWithOffset srcIxFun bt_size,
    perm == [0, 2, 1] =
    isOk dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun bt_size,
    Just (src_offset, perm) <- IxFun.rearrangeWithOffset srcIxFun,
    perm == [0, 2, 1] =
    isOk dest_offset src_offset
  | otherwise =
    Nothing
  where bt_size = ImpGen.basicScalarSize bt
        isOk dest_offset src_offset = do
          dest_offset' <- ImpGen.scalExpToImpExp dest_offset
          src_offset' <- ImpGen.scalExpToImpExp src_offset
          return (dest_offset', src_offset')
