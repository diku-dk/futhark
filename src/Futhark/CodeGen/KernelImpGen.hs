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

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.KernelImp as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen

compileProg :: Prog -> Either String Imp.Program
compileProg = ImpGen.compileProg kernelCompiler $ Imp.Space "device"

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.ExpCompiler Imp.Kernel

kernelCompiler target@(ImpGen.Destination dest) (LoopOp (Map _ w lam arrs)) = do
  global_thread_index <- newVName "global_thread_index"
  let global_thread_index_param = Imp.ScalarParam global_thread_index Int

  (kernel_size, indices, params, read_params, write_result, kernel_bnds) <-
    getKernel target global_thread_index w lam arrs

  let indices_lparams = [ Param (Ident index $ Basic Int) Scalar | index <- indices ]
      bound_in_kernel = global_thread_index : indices ++ map paramName params

  makeAllMemoryGlobal $ do
    kernelbody <- ImpGen.collect $
                  ImpGen.withParams [global_thread_index_param] $
                  ImpGen.declaringLParams (indices_lparams++params) $ do
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
    reads_from <- liftM catMaybes $
                  forM (HS.toList $ freeIn kernelbody) $ \var ->
      if var `elem` dest_mems ++ bound_in_kernel
        then return Nothing
        else do t <- lookupType var
                case t of
                  Array {} -> return Nothing
                  Mem memsize -> Just <$> (Imp.MemoryUse var <$>
                                           ImpGen.subExpToDimSize memsize)
                  Basic bt ->
                    if bt == Cert
                    then return Nothing
                    else return $ Just $ Imp.ScalarUse var bt

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

    ImpGen.emit $ Imp.Op Imp.Kernel {
        Imp.kernelThreadNum = global_thread_index
      , Imp.kernelBody = kernelbody
      , Imp.kernelUses = nub $ reads_from ++ writes_to
      , Imp.kernelSize = kernel_size
      }
    return ImpGen.Done

-- We generate a simple kernel for itoa and replicate.
kernelCompiler target (PrimOp (Iota n)) = do
  i <- newVName "i"
  let fun = Lambda i [] (Body () [] [Var i]) [Basic Int]
  kernelCompiler target $ LoopOp $ Map [] n fun []
kernelCompiler target (PrimOp (Replicate n v)) = do
  i <- newVName "i"
  t <- subExpType v
  let fun = Lambda i [] (Body () [] [v]) [t]
  kernelCompiler target $ LoopOp $ Map [] n fun []

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
  local $ \env -> env { ImpGen.envVtable = HM.map globalMemory $ ImpGen.envVtable env
                      , ImpGen.envDefaultSpace = Imp.Space "global"
                      }
  where globalMemory (ImpGen.MemVar entry) =
          ImpGen.MemVar entry { ImpGen.entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

writeThreadResult :: [VName] -> ImpGen.ValueDestination -> SubExp
                  -> ImpGen.ImpM Imp.Kernel ()
writeThreadResult thread_idxs
  (ImpGen.ArrayDestination
   (ImpGen.CopyIntoMemory
    destloc@(ImpGen.MemLocation mem _ _)) _) se = do
  set <- subExpType se
  space <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory mem
  let is = map ImpGen.varIndex thread_idxs
  case set of
    Basic bt -> do
      (_, _, elemOffset) <-
        ImpGen.fullyIndexArray' destloc is bt
      ImpGen.compileResultSubExp (ImpGen.ArrayElemDestination mem bt space elemOffset) se
    _ -> do
      memloc <- ImpGen.indexArray destloc is
      let dest = ImpGen.ArrayDestination (ImpGen.CopyIntoMemory memloc) $
                 replicate (arrayRank set) Nothing
      ImpGen.compileResultSubExp dest se
writeThreadResult _ _ _ =
  fail "Cannot handle kernel that does not return an array."

readThreadParams :: VName -> LParam -> VName
                 -> ImpGen.ImpM Imp.Kernel ()
readThreadParams thread_index param arr = do
  t <- lookupType arr
  when (arrayRank t == 1) $ do
    (srcmem, space, srcoffset) <-
      ImpGen.fullyIndexArray arr [ImpGen.varIndex thread_index]
    ImpGen.emit $ Imp.SetScalar (paramName param) $
      ImpGen.index srcmem srcoffset (elemType t) space

setIndexVariable :: VName -> SubExp -> VName -> Imp.Exp -> Imp.Code
setIndexVariable index w global_thread_index inner_size =
  Imp.SetScalar index $
  (Imp.ScalarVar global_thread_index `Imp.UnsignedDivide` inner_size)
   `impMod` ImpGen.compileSubExp w
  where impMod x 1 = x
        impMod x y = Imp.UnsignedMod x y

type ReadParams = ImpGen.ImpM Imp.Kernel ()
type WriteResult = ImpGen.ImpM Imp.Kernel ()

getKernel :: ImpGen.Destination -> VName
          -> SubExp -> Lambda -> [VName]
          -> ImpGen.ImpM Imp.Kernel
             (Imp.DimSize, [VName], [LParam], ReadParams, WriteResult, [Binding])
getKernel dest global_thread_index w lam arrs = do
  kernel_size <- newVName "kernel_size"
  let (inner_kernel_size, indices, params, read_params, write_result, kernel_bnds) =
        lookForInnerMaps []  dest global_thread_index w lam arrs
  ImpGen.emit $ Imp.DeclareScalar kernel_size Int
  ImpGen.emit $ Imp.SetScalar kernel_size $
    inner_kernel_size * ImpGen.compileSubExp w
  return (Imp.VarSize kernel_size, indices, params,
          read_params, write_result, kernel_bnds)

lookForInnerMaps :: [VName]
                 -> ImpGen.Destination -> VName
                 -> SubExp -> Lambda -> [VName]
                 -> (Imp.Exp, [VName], [LParam], ReadParams, WriteResult, [Binding])
lookForInnerMaps outer_indices target@(ImpGen.Destination dest) global_thread_index w lam arrs
  | Body () [bnd] res <- lambdaBody lam, -- Body has a single binding
    map Var (patternNames $ bindingPattern bnd) == res, -- Returned verbatim
    LoopOp (Map _ inner_w inner_lam inner_arrs) <- bindingExp bnd = -- And the binding is a map
      let (inner_kernel_size, indices, params,
           inner_read_params, write_result, kernel_bnds) =
            lookForInnerMaps (outer_indices++[lambdaIndex lam]) target
            global_thread_index inner_w inner_lam inner_arrs
          this_kernel_size =
            inner_kernel_size * ImpGen.compileSubExp inner_w
          set_index =
            setIndexVariable (lambdaIndex lam) w global_thread_index this_kernel_size
      in (this_kernel_size,
          lambdaIndex lam : indices,
          lambdaParams lam ++ params,
          ImpGen.emit set_index >> read_params >> inner_read_params,
          write_result,
          kernel_bnds)

  | otherwise =
      let write_result = zipWithM_ (writeThreadResult thread_indices)
                         dest $ bodyResult $ lambdaBody lam
          set_index = setIndexVariable (lambdaIndex lam) w global_thread_index 1
      in (1,
          [lambdaIndex lam],
          lambdaParams lam,
          ImpGen.emit set_index >> read_params,
          write_result,
          bodyBindings $ lambdaBody lam)
  where thread_indices = outer_indices ++ [lambdaIndex lam]
        read_params = zipWithM_ (readThreadParams $ lambdaIndex lam) (lambdaParams lam) arrs
