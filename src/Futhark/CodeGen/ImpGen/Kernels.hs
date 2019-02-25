{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.CodeGen.ImpGen.Kernels
  ( compileProg
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.List

import Prelude hiding (quot)

import Futhark.Error
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpCode.Kernels (bytes)
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.CodeGen.ImpGen.Kernels.SegRed
import Futhark.CodeGen.ImpGen (sFor, sWhen,
                               sOp,
                               dPrim, dPrim_, dPrimV)
import Futhark.CodeGen.ImpGen.Kernels.Transpose
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Util.IntegralExp (quotRoundingUp, quot, IntegralExp)

callKernelOperations :: ImpGen.Operations ExplicitMemory Imp.HostOp
callKernelOperations =
  ImpGen.Operations { ImpGen.opsExpCompiler = expCompiler
                    , ImpGen.opsCopyCompiler = callKernelCopy
                    , ImpGen.opsOpCompiler = opCompiler
                    , ImpGen.opsStmsCompiler = ImpGen.defCompileStms
                    }

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError Imp.Program)
compileProg prog =
  fmap (setDefaultSpace (Imp.Space "device")) <$>
  ImpGen.compileProg callKernelOperations (Imp.Space "device") [Imp.Space "local"] prog

opCompiler :: Pattern ExplicitMemory -> Op ExplicitMemory
           -> CallKernelGen ()
opCompiler dest (Alloc e space) =
  ImpGen.compileAlloc dest e space
opCompiler dest (Inner kernel) =
  kernelCompiler dest kernel

sizeClassWithEntryPoint :: Name -> Imp.SizeClass -> Imp.SizeClass
sizeClassWithEntryPoint fname (Imp.SizeThreshold path) =
  Imp.SizeThreshold $ map f path
  where f (name, x) = (keyWithEntryPoint fname name, x)
sizeClassWithEntryPoint _ size_class = size_class

kernelCompiler :: Pattern ExplicitMemory -> Kernel InKernel
               -> CallKernelGen ()

kernelCompiler (Pattern _ [pe]) (GetSize key size_class) = do
  fname <- asks ImpGen.envFunction
  sOp $ Imp.GetSize (patElemName pe) (keyWithEntryPoint fname key) $
    sizeClassWithEntryPoint fname size_class

kernelCompiler (Pattern _ [pe]) (CmpSizeLe key size_class x) = do
  fname <- asks ImpGen.envFunction
  let size_class' = sizeClassWithEntryPoint fname size_class
  sOp . Imp.CmpSizeLe (patElemName pe) (keyWithEntryPoint fname key) size_class'
    =<< ImpGen.compileSubExp x

kernelCompiler (Pattern _ [pe]) (GetSizeMax size_class) =
  sOp $ Imp.GetSizeMax (patElemName pe) size_class

kernelCompiler pat (Kernel desc space _ kernel_body) = do
  (constants, init_constants) <- kernelInitialisation space

  kernel_body' <-
    makeAllMemoryGlobal $ ImpGen.subImpM_ (inKernelOperations constants) $ do
    init_constants
    compileKernelBody pat constants kernel_body

  let bound_in_kernel =
        M.keys $
        scopeOfKernelSpace space <>
        scopeOf (kernelBodyStms kernel_body)
  (uses, local_memory) <- computeKernelUses kernel_body' bound_in_kernel

  forM_ (kernelHints desc) $ \(s,v) -> do
    ty <- case v of
      Constant pv -> return $ Prim $ primValueType pv
      Var vn -> lookupType vn
    unless (primType ty) $ fail $ concat [ "debugKernelHint '", s, "'"
                                         , " in kernel '", kernelName desc, "'"
                                         , " did not have primType value." ]

    ImpGen.compileSubExp v >>= ImpGen.emit . Imp.DebugPrint s (elemType ty)

  sOp $ Imp.CallKernel Imp.Kernel
            { Imp.kernelBody = kernel_body'
            , Imp.kernelLocalMemory = local_memory
            , Imp.kernelUses = uses
            , Imp.kernelNumGroups = [ImpGen.compileSubExpOfType int32 $ spaceNumGroups space]
            , Imp.kernelGroupSize = [ImpGen.compileSubExpOfType int32 $ spaceGroupSize space]
            , Imp.kernelName = nameFromString $ kernelName desc ++ "_" ++
                               show (baseTag $ kernelGlobalThreadIdVar constants)
            }

kernelCompiler pat (SegRed space comm red_op nes _ body) =
  compileSegRed pat space comm red_op nes body

kernelCompiler pat e =
  compilerBugS $ "ImpGen.kernelCompiler: Invalid pattern\n  " ++
  pretty pat ++ "\nfor expression\n  " ++ pretty e

expCompiler :: ImpGen.ExpCompiler ExplicitMemory Imp.HostOp

-- We generate a simple kernel for itoa and replicate.
expCompiler (Pattern _ [pe]) (BasicOp (Iota n x s et)) = do
  n' <- ImpGen.compileSubExp n
  x' <- ImpGen.compileSubExp x
  s' <- ImpGen.compileSubExp s
  destloc <- ImpGen.entryArrayLocation <$> ImpGen.lookupArray (patElemName pe)
  (constants, set_constants) <- simpleKernelConstants n' "iota"

  sKernel constants "iota" $ do
    set_constants
    let gtid = kernelGlobalThreadId constants
    sWhen (kernelThreadActive constants) $ do
      (destmem, destspace, destidx) <-
        ImpGen.fullyIndexArray' destloc [gtid] (IntType et)

      ImpGen.emit $
        Imp.Write destmem destidx (IntType et) destspace Imp.Nonvolatile $
        Imp.ConvOpExp (SExt Int32 et) gtid * s' + x'

expCompiler (Pattern _ [pe]) (BasicOp (Replicate (Shape ds) se)) = do
  t <- subExpType se

  dims <- mapM ImpGen.compileSubExp $ ds ++ arrayDims t
  (constants, set_constants) <-
    simpleKernelConstants (product dims) "replicate"

  let is' = unflattenIndex dims $ kernelGlobalThreadId constants

  sKernel constants "replicate" $ do
    set_constants
    sWhen (kernelThreadActive constants) $
      ImpGen.copyDWIM (patElemName pe) is' se $ drop (length ds) is'

-- Allocation in the "local" space is just a placeholder.
expCompiler _ (Op (Alloc _ (Space "local"))) =
  return ()

expCompiler dest e =
  ImpGen.defCompileExp dest e

callKernelCopy :: ImpGen.CopyCompiler ExplicitMemory Imp.HostOp
callKernelCopy bt
  destloc@(ImpGen.MemLocation destmem destshape destIxFun)
  srcloc@(ImpGen.MemLocation srcmem srcshape srcIxFun)
  n
  | Just (destoffset, srcoffset,
          num_arrays, size_x, size_y,
          src_elems, dest_elems) <- isMapTransposeKernel bt destloc srcloc = do

      fname <- mapTransposeForType bt
      ImpGen.emit $ Imp.Call [] fname
        [Imp.MemArg destmem, Imp.ExpArg destoffset,
         Imp.MemArg srcmem, Imp.ExpArg srcoffset,
         Imp.ExpArg num_arrays, Imp.ExpArg size_x, Imp.ExpArg size_y,
         Imp.ExpArg src_elems, Imp.ExpArg dest_elems]

  | bt_size <- primByteSize bt,
    ixFunMatchesInnerShape
      (Shape $ map Imp.sizeToExp destshape) destIxFun,
    ixFunMatchesInnerShape
      (Shape $ map Imp.sizeToExp srcshape) srcIxFun,
    Just destoffset <-
      IxFun.linearWithOffset destIxFun bt_size,
    Just srcoffset  <-
      IxFun.linearWithOffset srcIxFun bt_size = do
        let row_size = product $ map ImpGen.dimSizeToExp $ drop 1 srcshape
        srcspace <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory srcmem
        destspace <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory destmem
        ImpGen.emit $ Imp.Copy
          destmem (bytes destoffset) destspace
          srcmem (bytes srcoffset) srcspace $
          (n * row_size) `Imp.withElemType` bt

  | otherwise = do

  -- Note that the shape of the destination and the source are
  -- necessarily the same.
  let shape = map Imp.sizeToExp srcshape
      shape_se = map (Imp.innerExp . ImpGen.dimSizeToExp) srcshape
      kernel_size = Imp.innerExp n * product (drop 1 shape)

  (constants, set_constants) <- simpleKernelConstants kernel_size "copy"

  sKernel constants "copy" $ do
    set_constants

    let gtid = kernelGlobalThreadId constants
        dest_is = unflattenIndex shape_se gtid
        src_is = dest_is

    (_, destspace, destidx) <- ImpGen.fullyIndexArray' destloc dest_is bt
    (_, srcspace, srcidx) <- ImpGen.fullyIndexArray' srcloc src_is bt

    sWhen (gtid .<. kernel_size) $ ImpGen.emit $
      Imp.Write destmem destidx bt destspace Imp.Nonvolatile $
      Imp.index srcmem srcidx bt srcspace Imp.Nonvolatile

mapTransposeForType :: PrimType -> ImpGen.ImpM ExplicitMemory Imp.HostOp Name
mapTransposeForType bt = do
  -- XXX: The leading underscore is to avoid clashes with a
  -- programmer-defined function of the same name (this is a bad
  -- solution...).
  let fname = nameFromString $ "_" <> mapTransposeName bt

  exists <- ImpGen.hasFunction fname
  unless exists $ ImpGen.emitFunction fname $ mapTransposeFunction bt

  return fname

mapTransposeName :: PrimType -> String
mapTransposeName bt = "map_transpose_" ++ pretty bt

mapTransposeFunction :: PrimType -> Imp.Function
mapTransposeFunction bt =
  Imp.Function False [] params transpose_code [] []

  where params = [memparam destmem, intparam destoffset,
                  memparam srcmem, intparam srcoffset,
                  intparam num_arrays, intparam x, intparam y,
                  intparam in_elems, intparam out_elems]

        space = Space "device"
        memparam v = Imp.MemParam v space
        intparam v = Imp.ScalarParam v $ IntType Int32

        [destmem, destoffset, srcmem, srcoffset,
         num_arrays, x, y, in_elems, out_elems,
         mulx, muly, block] =
           zipWith (VName . nameFromString)
           ["destmem",
             "destoffset",
             "srcmem",
             "srcoffset",
             "num_arrays",
             "x_elems",
             "y_elems",
             "in_elems",
             "out_elems",
             -- The following is only used for low width/height
             -- transpose kernels
             "mulx",
             "muly",
             "block"
            ]
           [0..]

        v32 v = Imp.var v int32

        block_dim_int = 16

        block_dim :: IntegralExp a => a
        block_dim = 16

        -- When an input array has either width==1 or height==1, performing a
        -- transpose will be the same as performing a copy.  If 'input_size' or
        -- 'output_size' is not equal to width*height, then this trick will not
        -- work when there are more than one array to process, as it is a per
        -- array limit. We could copy each array individually, but currently we
        -- do not.
        can_use_copy =
          let in_out_eq = CmpOpExp (CmpEq $ IntType Int32) (v32 in_elems) (v32 out_elems)
              onearr = CmpOpExp (CmpEq $ IntType Int32) (v32 num_arrays) 1
              noprob_widthheight = CmpOpExp (CmpEq $ IntType Int32)
                                     (v32 x * v32 y)
                                     (v32 in_elems)
              height_is_one = CmpOpExp (CmpEq $ IntType Int32) (v32 y) 1
              width_is_one = CmpOpExp (CmpEq $ IntType Int32) (v32 x) 1
          in BinOpExp LogAnd
               in_out_eq
               (BinOpExp LogAnd
                 (BinOpExp LogOr onearr noprob_widthheight)
                 (BinOpExp LogOr width_is_one height_is_one))

        transpose_code =
          Imp.If input_is_empty mempty $ mconcat
          [ Imp.DeclareScalar muly (IntType Int32)
          , Imp.SetScalar muly $ block_dim `quot` v32 x
          , Imp.DeclareScalar mulx (IntType Int32)
          , Imp.SetScalar mulx $ block_dim `quot` v32 y
          , Imp.If can_use_copy copy_code $
            Imp.If should_use_lowwidth (callTransposeKernel TransposeLowWidth) $
            Imp.If should_use_lowheight (callTransposeKernel TransposeLowHeight) $
            Imp.If should_use_small (callTransposeKernel TransposeSmall) $
            callTransposeKernel TransposeNormal]

        input_is_empty =
          v32 num_arrays .==. 0 .||. v32 x .==. 0 .||. v32 y .==. 0

        should_use_small = BinOpExp LogAnd
          (CmpOpExp (CmpSle Int32) (v32 x) (block_dim `quot` 2))
          (CmpOpExp (CmpSle Int32) (v32 y) (block_dim `quot` 2))

        should_use_lowwidth = BinOpExp LogAnd
          (CmpOpExp (CmpSle Int32) (v32 x) (block_dim `quot` 2))
          (CmpOpExp (CmpSlt Int32) block_dim (v32 y))

        should_use_lowheight = BinOpExp LogAnd
          (CmpOpExp (CmpSle Int32) (v32 y) (block_dim `quot` 2))
          (CmpOpExp (CmpSlt Int32) block_dim (v32 x))

        copy_code =
          let num_bytes =
                v32 in_elems * Imp.LeafExp (Imp.SizeOf bt) (IntType Int32)
          in Imp.Copy
               destmem (Imp.Count $ v32 destoffset) space
               srcmem (Imp.Count $ v32 srcoffset) space
               (Imp.Count num_bytes)

        callTransposeKernel =
          Imp.Op . Imp.CallKernel .
          mapTransposeKernel (mapTransposeName bt) block_dim_int
          (destmem, v32 destoffset, srcmem, v32 srcoffset,
            v32 x, v32 y, v32 in_elems, v32 out_elems,
            v32 mulx, v32 muly, v32 num_arrays,
            block) bt

isMapTransposeKernel :: PrimType -> ImpGen.MemLocation -> ImpGen.MemLocation
                     -> Maybe (Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp)
isMapTransposeKernel bt
  (ImpGen.MemLocation _ _ destIxFun)
  (ImpGen.MemLocation _ _ srcIxFun)
  | Just (dest_offset, perm_and_destshape) <- IxFun.rearrangeWithOffset destIxFun bt_size,
    (perm, destshape) <- unzip perm_and_destshape,
    srcshape' <- IxFun.shape srcIxFun,
    Just src_offset <- IxFun.linearWithOffset srcIxFun bt_size,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk (product srcshape') (product destshape) destshape swap r1 r2 dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun bt_size,
    Just (src_offset, perm_and_srcshape) <- IxFun.rearrangeWithOffset srcIxFun bt_size,
    (perm, srcshape) <- unzip perm_and_srcshape,
    destshape' <- IxFun.shape destIxFun,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk (product srcshape) (product destshape') srcshape id r1 r2 dest_offset src_offset
  | otherwise =
    Nothing
  where bt_size = primByteSize bt
        swap (x,y) = (y,x)

        isOk src_elems dest_elems shape f r1 r2 dest_offset src_offset = do
          let (num_arrays, size_x, size_y) = getSizes shape f r1 r2
          return (dest_offset, src_offset,
                  num_arrays, size_x, size_y,
                  src_elems, dest_elems)

        getSizes shape f r1 r2 =
          let (mapped, notmapped) = splitAt r1 shape
              (pretrans, posttrans) = f $ splitAt r2 notmapped
          in (product mapped, product pretrans, product posttrans)

simpleKernelConstants :: Imp.Exp -> String
                      -> CallKernelGen (KernelConstants, ImpGen.ImpM InKernel Imp.KernelOp ())
simpleKernelConstants kernel_size desc = do
  thread_gtid <- newVName $ desc ++ "_gtid"
  thread_ltid <- newVName $ desc ++ "_ltid"
  group_id <- newVName $ desc ++ "_gid"
  (group_size, num_groups) <- computeMapKernelGroups kernel_size
  let set_constants = do
        dPrim_ thread_gtid int32
        dPrim_ thread_ltid int32
        dPrim_ group_id int32
        sOp (Imp.GetGlobalId thread_gtid 0)
        sOp (Imp.GetLocalId thread_ltid 0)
        sOp (Imp.GetGroupId group_id 0)

  return (KernelConstants
          (Imp.var thread_gtid int32) (Imp.var thread_ltid int32) (Imp.var group_id int32)
          thread_gtid thread_ltid group_id
          group_size num_groups (group_size*num_groups) 0
          [] (Imp.var thread_gtid int32 .<. kernel_size) mempty,

          set_constants)

computeMapKernelGroups :: Imp.Exp -> CallKernelGen (Imp.Exp, Imp.Exp)
computeMapKernelGroups kernel_size = do
  group_size <- dPrim "group_size" int32
  fname <- asks ImpGen.envFunction
  let group_size_var = Imp.var group_size int32
      group_size_key = keyWithEntryPoint fname $ nameFromString $ pretty group_size
  sOp $ Imp.GetSize group_size group_size_key Imp.SizeGroup
  num_groups <- dPrimV "num_groups" $ kernel_size `quotRoundingUp` Imp.ConvOpExp (SExt Int32 Int32) group_size_var
  return (Imp.var group_size int32, Imp.var num_groups int32)

compileKernelBody :: Pattern InKernel
                  -> KernelConstants
                  -> KernelBody InKernel
                  -> InKernelGen ()
compileKernelBody pat constants kbody =
  compileKernelStms constants (stmsToList $ kernelBodyStms kbody) $
  zipWithM_ (compileKernelResult constants) (patternElements pat) $
  kernelBodyResult kbody

compileKernelResult :: KernelConstants -> PatElem InKernel -> KernelResult
                    -> InKernelGen ()

compileKernelResult constants pe (ThreadsReturn OneResultPerGroup what) = do
  i <- newVName "i"

  in_local_memory <- arrayInLocalMemory what
  let me = kernelLocalThreadId constants

  if not in_local_memory then do
    who' <- ImpGen.compileSubExp $ intConst Int32 0
    sWhen (me .==. who') $
      ImpGen.copyDWIM (patElemName pe) [kernelGroupId constants] what []
    else do
      -- If the result of the group is an array in local memory, we
      -- store it by collective copying among all the threads of the
      -- group.  TODO: also do this if the array is in global memory
      -- (but this is a bit more tricky, synchronisation-wise).
      --
      -- We do the reads/writes multidimensionally, but the loop is
      -- single-dimensional.
      ws <- mapM ImpGen.compileSubExp . arrayDims =<< subExpType what
      -- Compute how many elements this thread is responsible for.
      -- Formula: (w - ltid) / group_size (rounded up).
      let w = product ws
          ltid = kernelLocalThreadId constants
          group_size = kernelGroupSize constants
          to_write = (w - ltid) `quotRoundingUp` group_size
          is = unflattenIndex ws $ ImpGen.varIndex i * group_size + ltid

      sFor i Int32 to_write $
        ImpGen.copyDWIM (patElemName pe) (kernelGroupId constants : is) what is

compileKernelResult constants pe (ThreadsReturn AllThreads what) =
  ImpGen.copyDWIM (patElemName pe) [kernelGlobalThreadId constants] what []

compileKernelResult constants pe (ThreadsReturn (ThreadsPerGroup limit) what) =
  sWhen (isActive limit) $
  ImpGen.copyDWIM (patElemName pe) [kernelGroupId constants] what []

compileKernelResult constants pe (ThreadsReturn ThreadsInSpace what) = do
  let is = map (ImpGen.varIndex . fst) $ kernelDimensions constants
  sWhen (kernelThreadActive constants) $ ImpGen.copyDWIM (patElemName pe) is what []

compileKernelResult constants pe (ConcatReturns SplitContiguous _ per_thread_elems moffset what) = do
  dest_loc <- ImpGen.entryArrayLocation <$> ImpGen.lookupArray (patElemName pe)
  let dest_loc_offset = ImpGen.offsetArray dest_loc offset
      dest' = ImpGen.arrayDestination dest_loc_offset
  ImpGen.copyDWIMDest dest' [] (Var what) []
  where offset = case moffset of
                   Nothing -> ImpGen.compileSubExpOfType int32 per_thread_elems *
                              kernelGlobalThreadId constants
                   Just se -> ImpGen.compileSubExpOfType int32 se

compileKernelResult constants pe (ConcatReturns (SplitStrided stride) _ _ moffset what) = do
  dest_loc <- ImpGen.entryArrayLocation <$> ImpGen.lookupArray (patElemName pe)
  let dest_loc' = ImpGen.strideArray
                  (ImpGen.offsetArray dest_loc offset) $
                  ImpGen.compileSubExpOfType int32 stride
      dest' = ImpGen.arrayDestination dest_loc'
  ImpGen.copyDWIMDest dest' [] (Var what) []
  where offset = case moffset of
                   Nothing -> kernelGlobalThreadId constants
                   Just se -> ImpGen.compileSubExpOfType int32 se

compileKernelResult constants pe (WriteReturn rws _arr dests) = do
  rws' <- mapM ImpGen.compileSubExp rws
  forM_ dests $ \(is, e) -> do
    is' <- mapM ImpGen.compileSubExp is
    let condInBounds i rw = 0 .<=. i .&&. i .<. rw
        write = foldl (.&&.) (kernelThreadActive constants) $
                zipWith condInBounds is' rws'
    sWhen write $ ImpGen.copyDWIM (patElemName pe) (map (ImpGen.compileSubExpOfType int32) is) e []

compileKernelResult _ _ KernelInPlaceReturn{} =
  -- Already in its place... said it was a hack.
  return ()

arrayInLocalMemory :: SubExp -> InKernelGen Bool
arrayInLocalMemory (Var name) = do
  res <- ImpGen.lookupVar name
  case res of
    ImpGen.ArrayVar _ entry ->
      (Space "local"==) . ImpGen.entryMemSpace <$>
      ImpGen.lookupMemory (ImpGen.memLocationName (ImpGen.entryArrayLocation entry))
    _ -> return False
arrayInLocalMemory Constant{} = return False
