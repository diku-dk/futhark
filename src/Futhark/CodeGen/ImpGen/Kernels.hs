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
import Futhark.CodeGen.ImpGen.Kernels.SegGenRed
import Futhark.CodeGen.ImpGen (sFor, sWhen, sOp, (<--))
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
opCompiler (Pattern _ [pe]) (Inner (GetSize key size_class)) = do
  fname <- asks ImpGen.envFunction
  sOp $ Imp.GetSize (patElemName pe) (keyWithEntryPoint fname key) $
    sizeClassWithEntryPoint fname size_class
opCompiler (Pattern _ [pe]) (Inner (CmpSizeLe key size_class x)) = do
  fname <- asks ImpGen.envFunction
  let size_class' = sizeClassWithEntryPoint fname size_class
  sOp . Imp.CmpSizeLe (patElemName pe) (keyWithEntryPoint fname key) size_class'
    =<< ImpGen.compileSubExp x
opCompiler (Pattern _ [pe]) (Inner (GetSizeMax size_class)) =
  sOp $ Imp.GetSizeMax (patElemName pe) size_class
opCompiler dest (Inner (HostOp kernel)) =
  kernelCompiler dest kernel
opCompiler (Pattern _ pes) (Inner (Husk hspace red_op nes ts _ (Body _ bnds ses))) = do
  i <- newVName "i"
  interm_red <- replicateM (length node_red_res) $ newVName "interm_red"
  interm_red_mem <- replicateM (length node_red_res) $ newVName "interm_red_mem"
  src_mem_names <- getArrayMemLocs src
  src_mem_bytes <- getMemSizes src_mem_names
  ImpGen.dScope Nothing $ scopeOfHuskSpace hspace
  ImpGen.dLParams red_op_params
  num_nodes <- ImpGen.dPrim "num_nodes" int32
  src_elems_e <- ImpGen.compileSubExp src_elems
  nes_e <- mapM ImpGen.compileSubExp nes
  zipWithM_ (<--) (map paramName red_acc_params) nes_e
  body_code <- ImpGen.collect $ ImpGen.localNodeCount (Imp.var num_nodes int32) $ do
    mapM_ allocAndPart $ zip4 parts parts_mem src_mem_names src_mem_bytes
    ImpGen.compileStms (freeIn ses) (stmsToList bnds) $ do
      mapM_ (\(n, m, t) -> allocInterm n m (t `arrayOfRow` Var num_nodes)) $ zip3 interm_red interm_red_mem red_ts
      zipWithM_ (\x y -> ImpGen.copyDWIM x [] y []) interm_red $ map Var node_red_res
      sFor i Int32 (Imp.var num_nodes int32) $ do
        zipWithM_ (\x y -> ImpGen.copyDWIM x [] y [Imp.var i int32])
                  (map paramName red_next_params) $ map Var interm_red
        ImpGen.compileBody' red_acc_params $ lambdaBody red_op
      zipWithM_ (\x y -> ImpGen.copyDWIM x [] y []) (map patElemName red_pes) $
                  map (Var . paramName) red_acc_params
      map_pes_mem <- getArrayMemLocs $ map patElemName map_pes
      map_pes_mem_sizes <- getMemSizes map_pes_mem
      node_res_map_mem <- getArrayMemLocs node_map_res
      node_res_map_mem_sizes <- getMemSizes node_res_map_mem
      mapM_ collect $ zip4 map_pes_mem map_pes_mem_sizes node_res_map_mem node_res_map_mem_sizes
  sOp $ Imp.Husk (interm_red ++ interm_red_mem) src_elems_e parts_elems num_nodes body_code
  where HuskSpace src src_elems parts parts_elems parts_mem = hspace
        red_op_params = lambdaParams red_op
        (red_acc_params, red_next_params) = splitAt (length nes) red_op_params
        (node_red_res, node_map_res) = splitAt (length nes) $ getVarNames ses
        red_ts = take (length nes) ts
        (red_pes, map_pes) = splitAt (length nes) pes
        getVarNames [] = []
        getVarNames (Var n : vs) = n : getVarNames vs
        getVarNames (_ : vs) = getVarNames vs
        allocAndPart (Param name (MemArray t s _ _), part_mem, src_mem, src_bytes) = do
          part_bytes <- ImpGen.sAllocNamedArray name part_mem t s DefaultSpace
          ImpGen.emit $ Imp.Partition part_mem part_bytes src_mem src_bytes DefaultSpace
        allocAndPart _ = fail "Partition is not an array."
        allocInterm name mem (Array et size _) =
          void $ ImpGen.sAllocNamedArray name mem et size DefaultSpace
        allocInterm _ _ _ = fail "Intermediate is not an array."
        getArrayMemLocs arr = 
          map (ImpGen.memLocationName . ImpGen.entryArrayLocation) <$> mapM ImpGen.lookupArray arr
        getMemSizes mem = 
          map (Imp.memSizeToExp . ImpGen.entryMemSize) <$> mapM ImpGen.lookupMemory mem
        collect (dest_mem, dest_bytes, src_mem, part_bytes) =
          ImpGen.emit $ Imp.Collect dest_mem dest_bytes src_mem part_bytes DefaultSpace
opCompiler pat e =
  compilerBugS $ "ImpGen.opCompiler: Invalid pattern\n  " ++
  pretty pat ++ "\nfor expression\n  " ++ pretty e

sizeClassWithEntryPoint :: Name -> Imp.SizeClass -> Imp.SizeClass
sizeClassWithEntryPoint fname (Imp.SizeThreshold path) =
  Imp.SizeThreshold $ map f path
  where f (name, x) = (keyWithEntryPoint fname name, x)
sizeClassWithEntryPoint _ size_class = size_class

kernelCompiler :: Pattern ExplicitMemory -> Kernel InKernel
               -> CallKernelGen ()

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

kernelCompiler pat (SegGenRed space ops _ body) =
  compileSegGenRed pat space ops body

expCompiler :: ImpGen.ExpCompiler ExplicitMemory Imp.HostOp

-- We generate a simple kernel for itoa and replicate.
expCompiler (Pattern _ [pe]) (BasicOp (Iota n x s et)) = do
  n' <- ImpGen.compileSubExp n
  x' <- ImpGen.compileSubExp x
  s' <- ImpGen.compileSubExp s

  sIota (patElemName pe) n' x' s' et

expCompiler (Pattern _ [pe]) (BasicOp (Replicate shape se)) =
  sReplicate (patElemName pe) shape se

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
      num_nodes <- asks ImpGen.envNodeCount

      fname <- mapTransposeForType bt num_nodes
      ImpGen.emit $ Imp.Call [] fname $
        maybe [] (\e -> [Imp.ExpArg e]) num_nodes ++
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

  | otherwise = sCopy bt destloc srcloc n

mapTransposeForType :: PrimType -> Maybe Imp.Exp -> ImpGen.ImpM ExplicitMemory Imp.HostOp Name
mapTransposeForType bt num_nodes = do
  -- XXX: The leading underscore is to avoid clashes with a
  -- programmer-defined function of the same name (this is a bad
  -- solution...).
  let fname = nameFromString $ "_" <> mapTransposeName bt num_nodes

  num_nodes_param_name <- newVName "num_nodes"
  let num_nodes_param = fmap pure (const num_nodes_param_name) num_nodes

  exists <- ImpGen.hasFunction fname
  unless exists $ ImpGen.emitFunction fname $ mapTransposeFunction bt num_nodes_param

  return fname

mapTransposeName :: PrimType -> Maybe a -> String
mapTransposeName bt num_nodes = "map_transpose_" ++ pretty bt ++ maybe "" (const "_nodes") num_nodes

mapTransposeFunction :: PrimType -> Maybe VName -> Imp.Function
mapTransposeFunction bt num_nodes_param =
  Imp.Function False [] params transpose_code [] [] num_nodes_param

  where params = maybe [] (\n -> [intparam n]) num_nodes_param ++
                 [memparam destmem, intparam destoffset,
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
          mapTransposeKernel (mapTransposeName bt num_nodes_param) block_dim_int
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
