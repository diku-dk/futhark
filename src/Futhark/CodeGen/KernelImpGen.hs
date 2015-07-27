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

import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.KernelImp as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen

compileProg :: Prog -> Either String Imp.Program
compileProg = ImpGen.compileProg kernelCompiler $ Imp.Space "device"

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.ExpCompiler Imp.Kernel

kernelCompiler (ImpGen.Destination dest) (LoopOp (Map _ w lam arrs)) = do
  -- The number of threads - one per input element.
  let body = lambdaBody lam
      thread_num = lambdaIndex lam
      thread_num_param = Imp.ScalarParam (lambdaIndex lam) Int

  makeAllMemoryGlobal $ do
    kernelbody <- ImpGen.collect $
                  ImpGen.withParam thread_num_param $
                  ImpGen.declaringLParams (lambdaParams lam) $ do
                    zipWithM_ (readThreadParams thread_num) (lambdaParams lam) arrs
                    ImpGen.compileBindings (bodyBindings body) $
                      zipWithM_ (writeThreadResult thread_num) dest $ bodyResult body

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
      if var `elem` thread_num : dest_mems ++ map paramName (lambdaParams lam)
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

    kernel_size <- ImpGen.subExpToDimSize w

    ImpGen.emit $ Imp.Op Imp.Kernel {
        Imp.kernelThreadNum = thread_num
      , Imp.kernelBody = kernelbody
      , Imp.kernelUses = nub $ reads_from ++ writes_to
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
  local $ \env -> env { ImpGen.envVtable = HM.map globalMemory $ ImpGen.envVtable env
                      , ImpGen.envDefaultSpace = Imp.Space "global"
                      }
  where globalMemory (ImpGen.MemVar entry) =
          ImpGen.MemVar entry { ImpGen.entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

writeThreadResult :: VName -> ImpGen.ValueDestination -> SubExp
                  -> ImpGen.ImpM Imp.Kernel ()
writeThreadResult thread_num
  (ImpGen.ArrayDestination
   (ImpGen.CopyIntoMemory
    destloc@(ImpGen.MemLocation mem _ _)) _) se = do
  set <- subExpType se
  space <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory mem
  let i = ImpGen.varIndex thread_num
  case set of
    Basic bt -> do
      (_, _, elemOffset) <-
        ImpGen.fullyIndexArray' destloc [i] bt
      ImpGen.compileResultSubExp (ImpGen.ArrayElemDestination mem bt space elemOffset) se
    _ -> do
      memloc <- ImpGen.indexArray destloc [i]
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
