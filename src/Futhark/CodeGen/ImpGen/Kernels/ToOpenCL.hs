{-# LANGUAGE QuasiQuotes #-}
-- | This module defines a translation from imperative code with
-- kernels to imperative code with OpenCL calls.
module Futhark.CodeGen.ImpGen.Kernels.ToOpenCL
  ( kernelsToOpenCL
  )
  where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Writer
import Data.List
import Data.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Prelude

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.Representation.AST.Attributes.Types (int32)
import qualified Futhark.CodeGen.OpenCL.Kernels as Kernels
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.ImpCode.Kernels hiding (Program, GetNumGroups, GetGroupSize, GetTileSize)
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import Futhark.CodeGen.ImpCode.OpenCL hiding (Program)
import qualified Futhark.CodeGen.ImpCode.OpenCL as ImpOpenCL
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeString)
import Futhark.Util.Pretty (pretty, prettyOneLine)

-- | Translate a kernels-program to an OpenCL-program.
kernelsToOpenCL :: ImpKernels.Program
                -> Either String ImpOpenCL.Program
kernelsToOpenCL prog = do
  (prog', ToOpenCL extra_funs kernels requirements) <-
    runWriterT $ traverse onHostOp prog
  let kernel_names = HM.keys kernels
      opencl_code = openClCode $ HM.elems kernels
      opencl_prelude = pretty $ genOpenClPrelude requirements
  return $ ImpOpenCL.Program opencl_code opencl_prelude kernel_names $
    ImpOpenCL.Functions (HM.toList extra_funs) <> prog'

pointerQuals ::  Monad m => String -> m [C.TypeQual]
pointerQuals "global"     = return [C.ctyquals|__global|]
pointerQuals "local"      = return [C.ctyquals|__local|]
pointerQuals "private"    = return [C.ctyquals|__private|]
pointerQuals "constant"   = return [C.ctyquals|__constant|]
pointerQuals "write_only" = return [C.ctyquals|__write_only|]
pointerQuals "read_only"  = return [C.ctyquals|__read_only|]
pointerQuals "kernel"     = return [C.ctyquals|__kernel|]
pointerQuals s            = fail $ "'" ++ s ++ "' is not an OpenCL kernel address space."

type UsedFunctions = [(String,C.Func)] -- The ordering is important!

data OpenClRequirements =
  OpenClRequirements { _kernelUsedFunctions :: UsedFunctions
                     , _kernelUsedTypes :: HS.HashSet PrimType
                     , _kernelConstants :: [(VName, KernelConstExp)]
                     }

instance Monoid OpenClRequirements where
  mempty = OpenClRequirements mempty mempty mempty

  OpenClRequirements used1 ts1 consts1 `mappend` OpenClRequirements used2 ts2 consts2 =
    OpenClRequirements (nubBy cmpFst $ used1 <> used2) (ts1 <> ts2) (consts1 <> consts2)
    where cmpFst (x, _) (y, _) = x == y

data ToOpenCL = ToOpenCL { clExtraFuns :: HM.HashMap Name ImpOpenCL.Function
                         , clKernels :: HM.HashMap KernelName C.Func
                         , clRequirements :: OpenClRequirements
                         }

instance Monoid ToOpenCL where
  mempty =
    ToOpenCL mempty mempty mempty
  ToOpenCL f1 k1 r1 `mappend` ToOpenCL f2 k2 r2 =
    ToOpenCL (f1<>f2) (k1<>k2) (r1<>r2)

type OnKernelM = WriterT ToOpenCL (Either String)

onHostOp :: HostOp -> OnKernelM OpenCL
onHostOp (CallKernel k) = onKernel k
onHostOp (ImpKernels.GetNumGroups v) =
  return $ GetNumGroups v
onHostOp (ImpKernels.GetGroupSize v) =
  return $ GetGroupSize v
onHostOp (ImpKernels.GetTileSize v) =
  return $ GetTileSize v

onKernel :: CallKernel -> OnKernelM OpenCL

onKernel called@(Map kernel) = do
  let (funbody, s) =
        GenericC.runCompilerM (Functions []) inKernelOperations blankNameSource mempty $ do
          size <- GenericC.compileExp $ mapKernelSize kernel
          let check = [C.citem|if ($id:(mapKernelThreadNum kernel) >= $exp:size) return;|]
          body <- GenericC.blockScope $ GenericC.compileCode $ mapKernelBody kernel
          return $ check : body

      used_funs = GenericC.compUserState s

      params = mapMaybe useAsParam $ mapKernelUses kernel

      kernel_funs = functionsCalled $ mapKernelBody kernel

  tell ToOpenCL
    { clExtraFuns = mempty
    , clKernels = HM.singleton (mapKernelName kernel)
                  [C.cfun|__kernel void $id:(mapKernelName kernel) ($params:params) {
                     const uint $id:(mapKernelThreadNum kernel) = get_global_id(0);
                     $items:funbody
                  }|]
    , clRequirements = OpenClRequirements
                       (used_funs ++ requiredFunctions kernel_funs)
                       (typesInKernel called)
                       (mapMaybe useAsConst $ mapKernelUses kernel)
    }

  return $ LaunchKernel
    (calledKernelName called) (kernelArgs called) kernel_size workgroup_size

  where (kernel_size, workgroup_size) = kernelAndWorkgroupSize called

onKernel called@(AnyKernel kernel) = do
  let (kernel_body, s) =
        GenericC.runCompilerM (Functions []) inKernelOperations blankNameSource mempty $
        GenericC.blockScope $ GenericC.compileCode $ kernelBody kernel

      used_funs = GenericC.compUserState s

      use_params = mapMaybe useAsParam $ kernelUses kernel

      kernel_funs = functionsCalled $ kernelBody kernel

      (local_memory_params, local_memory_init) =
        unzip $
        flip evalState (blankNameSource :: VNameSource) $
        mapM prepareLocalMemory $ kernelLocalMemory kernel

      params = catMaybes local_memory_params ++ use_params

  tell ToOpenCL { clExtraFuns = mempty
                , clKernels = HM.singleton name
                              [C.cfun|__kernel void $id:name ($params:params) {
                                  $items:local_memory_init
                                  $items:kernel_body
                                  }|]
               , clRequirements = OpenClRequirements
                                  (used_funs ++ requiredFunctions kernel_funs)
                                  (typesInKernel called)
                                  (mapMaybe useAsConst $ kernelUses kernel)
               }

  return $ LaunchKernel
    (calledKernelName called) (kernelArgs called) kernel_size workgroup_size

  where prepareLocalMemory (mem, Left _) = do
          mem_aligned <- newVName $ baseString mem ++ "_aligned"
          return (Just [C.cparam|__local volatile typename int64_t* $id:mem_aligned|],
                  [C.citem|__local volatile char* restrict $id:mem = $id:mem_aligned;|])
        prepareLocalMemory (mem, Right size) = do
          let size' = compilePrimExp size
          return (Nothing,
                  [C.citem|ALIGNED_LOCAL_MEMORY($id:mem, $exp:size');|])
        name = calledKernelName called
        (kernel_size, workgroup_size) = kernelAndWorkgroupSize called

onKernel (MapTranspose bt
          destmem destoffset
          srcmem srcoffset
          num_arrays x_elems y_elems in_elems out_elems) = do
  generateTransposeFunction bt
  return $ HostCode $ Call [] (transposeName bt)
    [MemArg destmem, ExpArg destoffset,
     MemArg srcmem, ExpArg srcoffset,
     ExpArg num_arrays, ExpArg x_elems, ExpArg y_elems,
     ExpArg in_elems, ExpArg out_elems]

useAsParam :: KernelUse -> Maybe C.Param
useAsParam (ScalarUse name bt) =
  let ctp = GenericC.primTypeToCType bt
  in Just [C.cparam|$ty:ctp $id:name|]
useAsParam (MemoryUse name _) =
  Just [C.cparam|__global unsigned char *$id:name|]
useAsParam ConstUse{} =
  Nothing

useAsConst :: KernelUse -> Maybe (VName, KernelConstExp)
useAsConst (ConstUse v e) = Just (v,e)
useAsConst _ = Nothing

requiredFunctions :: HS.HashSet Name -> [(String, C.Func)]
requiredFunctions kernel_funs =
  let used_in_kernel = (`HS.member` kernel_funs) . nameFromString . fst
      funs32_used = filter used_in_kernel funs32
      funs64_used = filter used_in_kernel funs64

      funs32 = [("log32", c_log32),
                ("sqrt32", c_sqrt32),
                ("exp32", c_exp32),
                ("sin32", c_sin32),
                ("cos32", c_cos32),
                ("asin32", c_asin32),
                ("acos32", c_acos32),
                ("atan2", c_atan32),
                ("atan2_32", c_atan2_32),
                ("isnan32", c_isnan32),
                ("isinf32", c_isinf32)]

      funs64 = [("log64", c_log64),
                ("sqrt64", c_sqrt64),
                ("exp64", c_exp64),
                ("sin64", c_sin64),
                ("cos64", c_cos64),
                ("asin64", c_asin64),
                ("acos64", c_acos64),
                ("atan64", c_atan64),
                ("atan2_64", c_atan2_64),
                ("isnan64", c_isnan64),
                ("isinf64", c_isinf64)]
  in funs32_used ++ funs64_used

openClCode :: [C.Func] -> String
openClCode kernels =
  pretty [C.cunit|$edecls:funcs|]
  where funcs =
          [[C.cedecl|$func:kernel_func|] |
           kernel_func <- kernels ]

genOpenClPrelude :: OpenClRequirements -> [C.Definition]
genOpenClPrelude (OpenClRequirements used_funs ts consts) =
  [[C.cedecl|$esc:("#pragma OPENCL EXTENSION cl_khr_fp64 : enable")|] | uses_float64] ++
  [C.cunit|
/* Some OpenCL programs dislike empty progams, or programs with no kernels.
 * Declare a dummy kernel to ensure they remain our friends. */
__kernel void dummy_kernel(__global unsigned char *dummy, int n)
{
    const int thread_gid = get_global_id(0);
    if (thread_gid >= n) return;
}

typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long int64_t;

typedef uchar uint8_t;
typedef ushort uint16_t;
typedef uint uint32_t;
typedef ulong uint64_t;

$esc:("#define ALIGNED_LOCAL_MEMORY(m,size) __local unsigned char m[size] __attribute__ ((align))")
|] ++
  cIntOps ++ cFloat32Ops ++
  (if uses_float64 then cFloat64Ops ++ cFloatConvOps else []) ++
  [ [C.cedecl|$func:used_fun|] | (_, used_fun) <- used_funs ] ++
  [ [C.cedecl|$esc:def|] | def <- map constToDefine consts ]
  where uses_float64 = FloatType Float64 `HS.member` ts
        constToDefine (name, e) =
          let e' = compilePrimExp e
          in unwords ["#define", zEncodeString (pretty name), "("++prettyOneLine e'++")"]

compilePrimExp :: PrimExp KernelConst -> C.Exp
compilePrimExp e = runIdentity $ GenericC.compilePrimExp compileKernelConst e
  where compileKernelConst GroupSizeConst = return [C.cexp|DEFAULT_GROUP_SIZE|]
        compileKernelConst NumGroupsConst = return [C.cexp|DEFAULT_NUM_GROUPS|]
        compileKernelConst TileSizeConst = return [C.cexp|DEFAULT_TILE_SIZE|]

mapKernelName :: MapKernel -> String
mapKernelName k = "kernel_"++ mapKernelDesc k ++ "_" ++
                  show (baseTag $ mapKernelThreadNum k)

calledKernelName :: CallKernel -> String
calledKernelName (Map k) =
  mapKernelName k
calledKernelName (AnyKernel k) =
  kernelDesc k ++ "_kernel_" ++ show (baseTag $ kernelName k)
calledKernelName (MapTranspose bt _ _ _ _ _ _ _ _ _) =
  transposeKernelName bt

kernelArgs :: CallKernel -> [KernelArg]
kernelArgs (Map kernel) =
  mapMaybe useToArg $ mapKernelUses kernel
kernelArgs (AnyKernel kernel) =
  mapMaybe (fmap (SharedMemoryKArg . memSizeToExp) . localMemorySize)
  (kernelLocalMemory kernel) ++
  mapMaybe useToArg (kernelUses kernel)
  where localMemorySize (_, Left size) = Just size
        localMemorySize (_, Right{}) = Nothing
kernelArgs (MapTranspose bt destmem destoffset srcmem srcoffset _ x_elems y_elems in_elems out_elems) =
  [ MemKArg destmem
  , ValueKArg destoffset int32
  , MemKArg srcmem
  , ValueKArg srcoffset int32
  , ValueKArg x_elems int32
  , ValueKArg y_elems int32
  , ValueKArg in_elems int32
  , ValueKArg out_elems int32
  , SharedMemoryKArg shared_memory
  ]
  where shared_memory =
          bytes $ (transposeBlockDim + 1) * transposeBlockDim *
          LeafExp (SizeOf bt) (IntType Int32)

kernelAndWorkgroupSize :: CallKernel -> ([Exp], [Exp])
kernelAndWorkgroupSize (Map kernel) =
  ([sizeToExp (mapKernelNumGroups kernel) *
    sizeToExp (mapKernelGroupSize kernel)],
   [sizeToExp $ mapKernelGroupSize kernel])
kernelAndWorkgroupSize (AnyKernel kernel) =
  ([sizeToExp (kernelNumGroups kernel) *
    sizeToExp (kernelGroupSize kernel)],
   [sizeToExp $ kernelGroupSize kernel])
kernelAndWorkgroupSize (MapTranspose _ _ _ _ _ num_arrays x_elems y_elems _ _) =
  transposeKernelAndGroupSize num_arrays x_elems y_elems

--- Generating C

inKernelOperations :: GenericC.Operations KernelOp UsedFunctions
inKernelOperations = GenericC.Operations
                     { GenericC.opsCompiler = kernelOps
                     , GenericC.opsMemoryType = kernelMemoryType
                     , GenericC.opsWriteScalar = GenericC.writeScalarPointerWithQuals pointerQuals
                     , GenericC.opsReadScalar = GenericC.readScalarPointerWithQuals pointerQuals
                     , GenericC.opsAllocate = cannotAllocate
                     , GenericC.opsDeallocate = cannotDeallocate
                     , GenericC.opsCopy = copyInKernel
                     , GenericC.opsFatMemory = False
                     }
  where kernelOps :: GenericC.OpCompiler KernelOp UsedFunctions
        kernelOps (GetGroupId v i) =
          GenericC.stm [C.cstm|$id:v = get_group_id($int:i);|]
        kernelOps (GetLocalId v i) =
          GenericC.stm [C.cstm|$id:v = get_local_id($int:i);|]
        kernelOps (GetLocalSize v i) =
          GenericC.stm [C.cstm|$id:v = get_local_size($int:i);|]
        kernelOps (GetGlobalId v i) =
          GenericC.stm [C.cstm|$id:v = get_global_id($int:i);|]
        kernelOps (GetGlobalSize v i) =
          GenericC.stm [C.cstm|$id:v = get_global_size($int:i);|]
        kernelOps (GetLockstepWidth v) =
          GenericC.stm [C.cstm|$id:v = LOCKSTEP_WIDTH;|]
        kernelOps Barrier =
          GenericC.stm [C.cstm|barrier(CLK_LOCAL_MEM_FENCE);|]

        cannotAllocate :: GenericC.Allocate KernelOp UsedFunctions
        cannotAllocate _ =
          fail "Cannot allocate memory in kernel"

        cannotDeallocate :: GenericC.Deallocate KernelOp UsedFunctions
        cannotDeallocate _ _ =
          fail "Cannot deallocate memory in kernel"

        copyInKernel :: GenericC.Copy KernelOp UsedFunctions
        copyInKernel _ _ _ _ _ _ _ =
          fail "Cannot bulk copy in kernel."

        kernelMemoryType space = do
          quals <- pointerQuals space
          return [C.cty|$tyquals:quals $ty:defaultMemBlockType|]

--- Handling transpositions

transposeKernelName :: PrimType -> String
transposeKernelName bt = "fut_kernel_map_transpose_" ++ pretty bt

transposeName :: PrimType -> Name
transposeName bt = nameFromString $ "map_transpose_opencl_" ++ pretty bt

generateTransposeFunction :: PrimType -> OnKernelM ()
generateTransposeFunction bt =
  tell ToOpenCL
    { clExtraFuns = HM.singleton (transposeName bt) $
                    ImpOpenCL.Function False [] params transpose_code [] []
    , clKernels = HM.singleton (transposeKernelName bt) $
                  Kernels.mapTranspose (transposeKernelName bt) bt'
    , clRequirements = mempty
    }
  where bt' = GenericC.primTypeToCType bt
        space = ImpOpenCL.Space "device"
        memparam s i = MemParam (ID (nameFromString s, i)) space
        intparam s i = ScalarParam (ID (nameFromString s, i)) $ IntType Int32

        params@[destmem_p, destoffset_p, srcmem_p, srcoffset_p,
                num_arrays_p, x_p, y_p, in_p, out_p] =
          zipWith ($) [memparam "destmem",
                       intparam "destoffset",
                       memparam "srcmem",
                       intparam "srcoffset",
                       intparam "num_arrays",
                       intparam "x_elems",
                       intparam "y_elems",
                       intparam "in_elems",
                       intparam "out_elems"]
                      [0..]

        asExp param =
          ImpOpenCL.LeafExp (ImpOpenCL.ScalarVar (paramName param)) (IntType Int32)

        asArg (MemParam name _) =
          MemKArg name
        asArg (ScalarParam name t) =
          ValueKArg (ImpOpenCL.LeafExp (ImpOpenCL.ScalarVar name) t) t

        normal_kernel_args =
          map asArg [destmem_p, destoffset_p, srcmem_p, srcoffset_p,
                     x_p, y_p, in_p, out_p] ++
          [SharedMemoryKArg shared_memory]

        shared_memory =
          bytes $ (transposeBlockDim + 1) * transposeBlockDim *
          LeafExp (SizeOf bt) (IntType Int32)

        transpose_code =
          ImpOpenCL.Op $ LaunchKernel
          (transposeKernelName bt) normal_kernel_args kernel_size workgroup_size
        (kernel_size, workgroup_size) =
          transposeKernelAndGroupSize (asExp num_arrays_p) (asExp x_p) (asExp y_p)

transposeKernelAndGroupSize :: ImpOpenCL.Exp -> ImpOpenCL.Exp -> ImpOpenCL.Exp
                            -> ([ImpOpenCL.Exp], [ImpOpenCL.Exp])
transposeKernelAndGroupSize num_arrays x_elems y_elems =
  ([roundedToBlockDim x_elems,
    roundedToBlockDim y_elems,
    num_arrays],
   [transposeBlockDim, transposeBlockDim, 1])
  where roundedToBlockDim e =
          e + ((transposeBlockDim -
                (e `impRem` transposeBlockDim)) `impRem`
               transposeBlockDim)
        impRem = BinOpExp $ SRem Int32

--- Checking requirements

useToArg :: KernelUse -> Maybe KernelArg
useToArg (MemoryUse mem _) = Just $ MemKArg mem
useToArg (ScalarUse v bt)  = Just $ ValueKArg (LeafExp (ScalarVar v) bt) bt
useToArg ConstUse{}        = Nothing

typesInKernel :: CallKernel -> HS.HashSet PrimType
typesInKernel (Map kernel) = typesInCode $ mapKernelBody kernel
typesInKernel (AnyKernel kernel) = typesInCode $ kernelBody kernel
typesInKernel MapTranspose{} = mempty

typesInCode :: ImpKernels.KernelCode -> HS.HashSet PrimType
typesInCode Skip = mempty
typesInCode (c1 :>>: c2) = typesInCode c1 <> typesInCode c2
typesInCode (For _ it e c) = IntType it `HS.insert` typesInExp e <> typesInCode c
typesInCode (While e c) = typesInExp e <> typesInCode c
typesInCode DeclareMem{} = mempty
typesInCode (DeclareScalar _ t) = HS.singleton t
typesInCode (Allocate _ (Count e) _) = typesInExp e
typesInCode (Copy _ (Count e1) _ _ (Count e2) _ (Count e3)) =
  typesInExp e1 <> typesInExp e2 <> typesInExp e3
typesInCode (Write _ (Count e1) t _ _ e2) =
  typesInExp e1 <> HS.singleton t <> typesInExp e2
typesInCode (SetScalar _ e) = typesInExp e
typesInCode SetMem{} = mempty
typesInCode (Call _ _ es) = mconcat $ map typesInArg es
  where typesInArg MemArg{} = mempty
        typesInArg (ExpArg e) = typesInExp e
typesInCode (If e c1 c2) =
  typesInExp e <> typesInCode c1 <> typesInCode c2
typesInCode (Assert e _) = typesInExp e
typesInCode (Comment _ c) = typesInCode c
typesInCode (DebugPrint _ _ e) = typesInExp e
typesInCode Op{} = mempty

typesInExp :: Exp -> HS.HashSet PrimType
typesInExp (ValueExp v) = HS.singleton $ primValueType v
typesInExp (BinOpExp _ e1 e2) = typesInExp e1 <> typesInExp e2
typesInExp (CmpOpExp _ e1 e2) = typesInExp e1 <> typesInExp e2
typesInExp (ConvOpExp op e) = HS.fromList [from, to] <> typesInExp e
  where (from, to) = convTypes op
typesInExp (UnOpExp _ e) = typesInExp e
typesInExp (LeafExp (Index _ (Count e) t _ _) _) = HS.singleton t <> typesInExp e
typesInExp (LeafExp ScalarVar{} _) = mempty
typesInExp (LeafExp (SizeOf t) _) = HS.singleton t
