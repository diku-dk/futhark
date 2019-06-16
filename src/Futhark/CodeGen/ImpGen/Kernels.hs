{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.CodeGen.ImpGen.Kernels
  ( Futhark.CodeGen.ImpGen.Kernels.compileProg
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Set as S
import Data.List

import Prelude hiding (quot)

import Futhark.Error
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpCode.Kernels (bytes)
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.CodeGen.ImpGen.Kernels.SegMap
import Futhark.CodeGen.ImpGen.Kernels.SegRed
import Futhark.CodeGen.ImpGen.Kernels.SegScan
import Futhark.CodeGen.ImpGen.Kernels.SegGenRed
import Futhark.CodeGen.ImpGen.Kernels.Transpose
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Util.IntegralExp (quot, IntegralExp)

callKernelOperations :: Operations ExplicitMemory Imp.HostOp
callKernelOperations =
  Operations { opsExpCompiler = expCompiler
                    , opsCopyCompiler = callKernelCopy
                    , opsOpCompiler = opCompiler
                    , opsStmsCompiler = defCompileStms
                    , opsAllocCompilers = mempty
                    }

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError Imp.Program)
compileProg prog =
  fmap (setDefaultSpace (Imp.Space "device")) <$>
  Futhark.CodeGen.ImpGen.compileProg callKernelOperations (Imp.Space "device") prog

opCompiler :: Pattern ExplicitMemory -> Op ExplicitMemory
           -> CallKernelGen ()
opCompiler dest (Alloc e space) =
  compileAlloc dest e space
opCompiler (Pattern _ [pe]) (Inner (GetSize key size_class)) = do
  fname <- asks envFunction
  sOp $ Imp.GetSize (patElemName pe) (keyWithEntryPoint fname key) $
    sizeClassWithEntryPoint fname size_class
opCompiler (Pattern _ [pe]) (Inner (CmpSizeLe key size_class x)) = do
  fname <- asks envFunction
  let size_class' = sizeClassWithEntryPoint fname size_class
  sOp . Imp.CmpSizeLe (patElemName pe) (keyWithEntryPoint fname key) size_class'
    =<< toExp x
opCompiler (Pattern _ [pe]) (Inner (GetSizeMax size_class)) =
  sOp $ Imp.GetSizeMax (patElemName pe) size_class
opCompiler dest (Inner (HostOp kernel)) =
  kernelCompiler dest kernel
opCompiler (Pattern _ pes) (Inner (Husk hspace red_op nes ts (Body _ bnds ses))) = do
  husk_func@(Imp.HuskFunction _ _ node_id) <- newHuskFunction
  i <- newVName "i"
  interm_red <- replicateM (length node_red_res) $ newVName "interm_red"
  interm_red_mem <- replicateM (length node_red_res) $ newVName "interm_red_mem"
  src_mem_names <- getArrayMemLocs src
  dScope Nothing $ scopeOfHuskSpace hspace
  dLParams red_op_params
  num_nodes <- dPrim "num_nodes" int32
  src_elems_e <- toExp src_elems
  map_pes_mems <- getArrayMemLocs (map patElemName map_pes)
  zipWithM_ (\x y -> copyDWIM x [] y []) (map paramName red_acc_params) nes
  interm_code <- collect $
    mapM_ (\(n, m, t) -> allocInterm n m (t `arrayOfRow` Var num_nodes)) $ zip3 interm_red interm_red_mem red_ts
  body_code <- compileHuskFun husk_func $ do
    max_part_elems <- dPrimV "max_parts_elems" $ one_val + BinOpExp (SDiv Int32) (src_elems_e - one_val) (Imp.var num_nodes int32)
    dPrimV_ parts_offset $ Imp.var node_id int32 * Imp.var max_part_elems int32
    dPrimV_ parts_elems $ BinOpExp (SMin Int32) (Imp.var max_part_elems int32) (src_elems_e - Imp.var parts_offset int32)
    mapM_ (allocAndPart husk_func) $ zip4 parts parts_mem parts_sizes src_mem_names
    compileStms (freeIn ses) bnds $ do
      zipWithM_ (\x y -> copyDWIM x [Imp.var node_id int32] y []) interm_red $ map Var node_red_res
      node_map_res_arrs <- mapM lookupArray node_map_res
      zipWithM_ (combineMapResult husk_func) node_map_res_arrs map_pes_mems
    mapM_ ((\x -> emit $ Imp.Free x DefaultSpace) . paramName) parts_mem
  non_param_mem <- filterM isMem $ S.toList $ freeIn body_code `S.difference` S.fromList (src_mem_names ++ map_pes_mems ++ interm_red_mem)
  red_code <- collect $ do
      sFor i Int32 (Imp.var num_nodes int32) $ do
        zipWithM_ (\x y -> copyDWIM x [] y [Imp.var i int32])
                  (map paramName red_next_params) $ map Var interm_red
        compileBody' red_acc_params $ lambdaBody red_op
      zipWithM_ (\x y -> copyDWIM x [] y []) (map patElemName red_pes) $
                  map (Var . paramName) red_acc_params
  bparams <- catMaybes <$> mapM (\x -> getParam x <$> lookupType x)
    (S.toList $ freeIn body_code `S.difference` S.fromList (Imp.hfunctionParams husk_func))
  sOp $ Imp.Husk (interm_red ++ interm_red_mem) num_nodes bparams non_param_mem husk_func interm_code body_code red_code
  where HuskSpace src src_elems parts parts_elems parts_offset parts_mem parts_sizes = hspace
        one_val = ValueExp $ IntValue $ Int32Value 1
        zero_val = ValueExp $ IntValue $ Int32Value 0
        red_op_params = lambdaParams red_op
        (red_acc_params, red_next_params) = splitAt (length nes) red_op_params
        (node_red_res, node_map_res) = splitAt (length nes) $ mapMaybe subExpVar ses
        red_ts = take (length nes) ts
        (red_pes, map_pes) = splitAt (length nes) pes
        getParam name (Prim t) = Just $ Imp.ScalarParam name t
        getParam name (Mem space) = Just $ Imp.MemParam name space
        getParam _ Array{} = Nothing
        isMem name = do
          v <- lookupVar name
          case v of
            MemVar{} -> return True
            _ -> return False
        memSize pt s = Imp.LeafExp (Imp.SizeOf pt) int32 * product (map (toExp' int32) (shapeDims s))
        allocAndPart hfunc (Param part (MemArray t s _ _), Param part_mem _, part_size, src_mem) = do
          dPrimV_ part_size $ memSize t s
          let part_size_b = Imp.bytes $ Imp.var part_size int32
              offset_bytes = Imp.bytes $ memSize t $ Shape $ map (replaceVar parts_elems parts_offset) $ shapeDims s
          sAllocNamedArray part part_mem t s part_size_b DefaultSpace
          emit $ Imp.PeerCopy part_mem (Imp.bytes zero_val) (Imp.var (Imp.hfunctionNodeId hfunc) int32)
            DefaultSpace src_mem offset_bytes zero_val DefaultSpace part_size_b
        allocAndPart _ _ = fail "Peer destination is not an array."
        allocInterm name mem (Array et size _) =
          sAllocNamedArray name mem et size (Imp.bytes $ memSize et size) DefaultSpace
        allocInterm _ _ _ = fail "Intermediate is not an array."
        getArrayMemLocs arr = map (memLocationName . entryArrayLocation) <$> mapM lookupArray arr
        replaceVar n1 r (Var n2) = if n1 == n2 then Var r else Var n2
        replaceVar _ _ e = e
        combineMapResult hfunc res_arr@(ArrayEntry res_mem_loc t) pe_mem =
          let res_arr_dims = map dimSizeToSubExp $ entryArrayShape res_arr
              res_bytes = Imp.bytes $ memSize t $ Shape res_arr_dims
              offset_bytes = Imp.bytes $ memSize t $ Shape $ map (replaceVar parts_elems parts_offset) res_arr_dims
              res_mem = memLocationName res_mem_loc
          in emit $ Imp.PeerCopy pe_mem offset_bytes zero_val DefaultSpace res_mem (Imp.bytes zero_val)
              (Imp.var (Imp.hfunctionNodeId hfunc) int32) DefaultSpace res_bytes
opCompiler pat e =
  compilerBugS $ "opCompiler: Invalid pattern\n  " ++
  pretty pat ++ "\nfor expression\n  " ++ pretty e

newHuskFunction :: CallKernelGen Imp.HuskFunction
newHuskFunction =
  Imp.HuskFunction
  <$> newVName "husk"
  <*> newVName "husk_context"
  <*> newVName "node_id"

sizeClassWithEntryPoint :: Name -> Imp.SizeClass -> Imp.SizeClass
sizeClassWithEntryPoint fname (Imp.SizeThreshold path) =
  Imp.SizeThreshold $ map f path
  where f (name, x) = (keyWithEntryPoint fname name, x)
sizeClassWithEntryPoint _ size_class = size_class

kernelCompiler :: Pattern ExplicitMemory -> Kernel InKernel
               -> CallKernelGen ()

kernelCompiler pat (Kernel desc space _ kernel_body) = do
  (constants, init_constants) <- kernelInitialisationSetSpace space $ return ()

  forM_ (kernelHints desc) $ \(s,v) -> do
    ty <- case v of
      Constant pv -> return $ Prim $ primValueType pv
      Var vn -> lookupType vn
    unless (primType ty) $ fail $ concat [ "debugKernelHint '", s, "'"
                                         , " in kernel '", kernelName desc, "'"
                                         , " did not have primType value." ]

    emit $ Imp.DebugPrint s $ Just (elemType ty, toExp' (elemType ty) v)

  let virt_groups = toExp' int32 (spaceNumVirtGroups space)
  sKernel constants (kernelName desc) $ do
    init_constants
    virtualiseGroups constants virt_groups $ \group_id -> do
      let flat_id =
            if kernelGroupIdVar constants /= group_id
            then Imp.vi32 group_id * kernelGroupSize constants + kernelLocalThreadId constants
            else kernelGlobalThreadId constants
      setSpaceIndices flat_id space
      compileKernelStms constants (kernelBodyStms kernel_body) $
        zipWithM_ (compileKernelResult constants) (patternElements pat) $
        kernelBodyResult kernel_body

kernelCompiler pat (SegMap space _ body) =
  compileSegMap pat space body

kernelCompiler pat (SegRed space comm red_op nes _ body) =
  compileSegRed pat space comm red_op nes body

kernelCompiler pat (SegScan space red_op nes _ kbody) =
  compileSegScan pat space red_op nes kbody

kernelCompiler pat (SegGenRed space ops _ body) =
  compileSegGenRed pat space ops body

expCompiler :: ExpCompiler ExplicitMemory Imp.HostOp

-- We generate a simple kernel for itoa and replicate.
expCompiler (Pattern _ [pe]) (BasicOp (Iota n x s et)) = do
  n' <- toExp n
  x' <- toExp x
  s' <- toExp s

  sIota (patElemName pe) n' x' s' et

expCompiler (Pattern _ [pe]) (BasicOp (Replicate shape se)) =
  sReplicate (patElemName pe) shape se

-- Allocation in the "local" space is just a placeholder.
expCompiler _ (Op (Alloc _ (Space "local"))) =
  return ()

expCompiler dest e =
  defCompileExp dest e

callKernelCopy :: CopyCompiler ExplicitMemory Imp.HostOp
callKernelCopy bt
  destloc@(MemLocation destmem destshape destIxFun)
  srcloc@(MemLocation srcmem srcshape srcIxFun)
  n
  | Just (destoffset, srcoffset,
          num_arrays, size_x, size_y,
          src_elems, dest_elems) <- isMapTransposeKernel bt destloc srcloc = do

      node_id <- getNodeId
      fname <- mapTransposeForType bt
      emit $ Imp.Call [] fname
        [Imp.ExpArg node_id, Imp.MemArg destmem, Imp.ExpArg destoffset,
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
        let row_size = product $ map dimSizeToExp $ drop 1 srcshape
        srcspace <- entryMemSpace <$> lookupMemory srcmem
        destspace <- entryMemSpace <$> lookupMemory destmem
        emit $ Imp.Copy
          destmem (bytes destoffset) destspace
          srcmem (bytes srcoffset) srcspace $
          (n * row_size) `Imp.withElemType` bt

  | otherwise = sCopy bt destloc srcloc n

mapTransposeForType :: PrimType -> ImpM ExplicitMemory Imp.HostOp Name
mapTransposeForType bt = do
  -- XXX: The leading underscore is to avoid clashes with a
  -- programmer-defined function of the same name (this is a bad
  -- solution...).
  let fname = nameFromString $ "_" <> mapTransposeName bt

  exists <- hasFunction fname
  unless exists $ emitFunction fname $ mapTransposeFunction bt

  return fname

mapTransposeName :: PrimType -> String
mapTransposeName bt = "map_transpose_" ++ pretty bt

mapTransposeFunction :: PrimType -> Imp.Function
mapTransposeFunction bt =
  Imp.Function False [] params transpose_code [] [] $ Imp.var nodeid int32

  where params = [intparam nodeid, memparam destmem, intparam destoffset,
                  memparam srcmem, intparam srcoffset,
                  intparam num_arrays, intparam x, intparam y,
                  intparam in_elems, intparam out_elems]

        space = Space "device"
        memparam v = Imp.MemParam v space
        intparam v = Imp.ScalarParam v $ IntType Int32

        [nodeid, destmem, destoffset, srcmem, srcoffset,
         num_arrays, x, y, in_elems, out_elems,
         mulx, muly, block] =
           zipWith (VName . nameFromString)
           ["nodeid",
             "destmem",
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

isMapTransposeKernel :: PrimType -> MemLocation -> MemLocation
                     -> Maybe (Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp)
isMapTransposeKernel bt
  (MemLocation _ _ destIxFun)
  (MemLocation _ _ srcIxFun)
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
