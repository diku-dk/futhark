module Futhark.CodeGen.Backends.SPIRV
  ( SingleEntryShader(..)
  , ReservedSpec (..)
  , reservedSpecList
  , runCompilerM
  , CompilerM
  , CompilerState
  , newCompilerState
  , getResult
  , getEntryPoints
  , entryPointName
  , getDescriptorSets
  , compileKernel
  , finalizedShader
  , kernelToShader
  ) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Bits
import Data.Binary.IEEE754
import Data.Word
import Data.List
import Data.Maybe

import Futhark.CodeGen.Backends.SPIRV.Operations
import Futhark.CodeGen.ImpCode hiding (Scalar, Function)
import Futhark.CodeGen.ImpCode.Kernels hiding (Code, Scalar, Function)
import Futhark.Representation.AST.Attributes.Types

data SingleEntryShader = SEShader {
    shaderEntryPoint :: VName
  , shaderDescriptorSetSize :: Int
  , shaderCode :: [Word32]
  }

data VarScope = Local
              | Global
              | UniformConstant
              | Private
              | FunctionLocal
              | StorageBuffer
              | Input
              -- | ^ Only used by builtin variables
  deriving (Eq, Ord)

data SPIRVType = Scalar PrimType
               | Array SPIRVType (Maybe Word32)
               | Vector SPIRVType Word32
               -- | ^ Only used by builtin variables
               | Struct [SPIRVType]
               -- | ^ Only used by interop buffers
               | Void
               | Function SPIRVType
               | Pointer SPIRVType VarScope
  deriving (Eq, Ord)

type ExprInfo = (Word32, SPIRVType)

data VarDec = Inline
            | LocalMem
            | InterOp
            | Constant
  deriving (Eq, Ord)

type VarInfo = (Word32, SPIRVType, VarDec)

data Builtin = GlobalInvocationId
             | WorkgroupId
             | NumWorkgroups
             | LocalInvocationId
  deriving (Eq, Ord)

type Descriptor = (Word32, KernelUse)
type DescriptorSet = [Descriptor]
type DescriptorSetMap = M.Map VName DescriptorSet

data ReservedSpec = WorkgroupSizeXSpec
                  | LockstepWidthSpec
  deriving (Eq, Ord)

reservedSpecList = [ WorkgroupSizeXSpec
                   , LockstepWidthSpec
                   ]

reservedSpecId :: ReservedSpec -> Word32
reservedSpecId res = fromIntegral $ maybe 0 (1+) $ elemIndex res reservedSpecList

reservedSpecType :: ReservedSpec -> SPIRVType
reservedSpecType WorkgroupSizeXSpec = Scalar int32
reservedSpecType LockstepWidthSpec  = Scalar int32

reservedSpecRef :: ReservedSpec -> (Word32, SPIRVType)
reservedSpecRef res = (reservedSpecId res, reservedSpecType res)

data Reserved = WorkgroupSize
  deriving (Eq, Ord)

reservedList = [ WorkgroupSize ]

reservedId :: Reserved -> Word32
reservedId res =
  fromIntegral $ length reservedSpecList + maybe 0 (1+) (elemIndex res reservedList)

totalReservedIdCount :: Word32
totalReservedIdCount = fromIntegral $ length reservedSpecList + length reservedList

data CompilerState = CompilerState {
    compCurrentMaxId :: Word32
  , compVarRefs :: M.Map VName VarInfo
  -- | ^ Type is the variable type without the required pointer wrapper
  , compTypeRefs :: M.Map SPIRVType Word32
  , compConstRefs :: M.Map PrimValue Word32
  -- | ^ Constants used inside the SPIR-V program
  , compEntryPoints :: [(VName, Word32)]
  , compResult :: [Word32]
  , compGLSLExtId :: Maybe Word32
  , compBuiltinRefs :: M.Map Builtin Word32
  , compDescriptors :: DescriptorSetMap
  , compSpecConstRefs :: [(Word32, SPIRVType)]
  , compArrayLeastAccessSize :: M.Map VName Int
  }

newCompilerState :: CompilerState
newCompilerState = CompilerState { compCurrentMaxId = totalReservedIdCount
                                 , compVarRefs = M.empty
                                 , compTypeRefs = M.empty
                                 , compConstRefs = M.empty
                                 , compEntryPoints = []
                                 , compResult = []
                                 , compGLSLExtId = Nothing
                                 , compBuiltinRefs = M.empty
                                 , compDescriptors = M.empty
                                 , compSpecConstRefs = map reservedSpecRef reservedSpecList
                                 , compArrayLeastAccessSize = M.empty
                                 }

type CompilerM = State CompilerState

runCompilerM :: CompilerState -> CompilerM a -> a
runCompilerM cs comp = evalState comp cs

newId :: CompilerM Word32
newId = do
  modify $ \s -> s { compCurrentMaxId = compCurrentMaxId s + 1 }
  gets compCurrentMaxId

insertType :: SPIRVType -> CompilerM Word32
insertType t = do
  t_id <- newId
  modify $ \s -> s { compTypeRefs = M.insert t t_id $ compTypeRefs s }
  return t_id

getTypeId :: SPIRVType -> CompilerM Word32
getTypeId t = do
  s <- get
  maybe (ensureAllSubTypeIds t >> insertType t) return $ (M.!?) (compTypeRefs s) t

ensureTypeId :: SPIRVType -> CompilerM ()
ensureTypeId t = void $ getTypeId t

ensureAllSubTypeIds :: SPIRVType -> CompilerM ()
ensureAllSubTypeIds (Function st)  = ensureTypeId st
ensureAllSubTypeIds (Array st _)   = ensureTypeId st
ensureAllSubTypeIds (Pointer st _) = ensureTypeId st
ensureAllSubTypeIds (Vector st _)  = ensureTypeId st
ensureAllSubTypeIds (Struct sts)   = mapM_ ensureTypeId sts
ensureAllSubTypeIds _              = return ()

getConstId :: PrimValue -> CompilerM Word32
getConstId v = do
  s <- get
  case (M.!?) (compConstRefs s) v of
    Just a  -> return a
    Nothing -> do
      v_id <- newId
      modify $ \s_n -> s_n { compConstRefs = M.insert v v_id $ compConstRefs s_n }
      return v_id

getBuiltinId :: Builtin -> CompilerM Word32
getBuiltinId builtin = do
  s <- get
  case (M.!?) (compBuiltinRefs s) builtin of
    Just a  -> return a
    Nothing -> do
      b_id <- newId
      modify $ \s_n -> s_n { compBuiltinRefs = M.insert builtin b_id $ compBuiltinRefs s_n }
      return b_id

clearVars :: CompilerM ()
clearVars = modify $ \s -> s { compVarRefs = M.empty }

getVarInfo :: VName -> CompilerM VarInfo
getVarInfo v = do
  s <- get
  return $ (M.!) (compVarRefs s) v

suggestArrayLeastAccessSize :: VName -> Int -> CompilerM ()
suggestArrayLeastAccessSize vn size = do
  s <- get
  let n = case (M.!?) (compArrayLeastAccessSize s) vn of
            Just curr_size -> M.adjust (min size) vn $ compArrayLeastAccessSize s
            Nothing        -> M.insert vn size $ compArrayLeastAccessSize s
  modify $ \s_n -> s_n { compArrayLeastAccessSize = n }

getCapabilities :: [Word32]
getCapabilities = concatMap opCapability [ cShaderCapability
                                         , cStorageBuffer8BitAccessCapability
                                         , cInt16Capability
                                         , cInt64Capability
                                         , cFloat64Capability
                                         --, cInt8Capability
                                         ]

getExtensions :: [Word32]
getExtensions = concatMap (opExtension . encodeString) [ "SPV_KHR_8bit_storage" ]

getGLSLExt :: CompilerM [Word32]
getGLSLExt = do
  glsl_id <- gets compGLSLExtId
  return $ case glsl_id of
              Nothing  -> opMemoryModel cSimpleMemoryModel
              Just gid -> opExtInstImport (encodeString "GLSL.std.450") gid ++ 
                            opMemoryModel cGLSLMemoryModel

entryPointName :: VName -> String
entryPointName name = baseString name ++ "_" ++ show (baseTag name)

spaceToScope :: Space -> VarScope
spaceToScope (Space "global")   = Global
spaceToScope (Space "constant") = UniformConstant
spaceToScope (Space "private")  = Private
spaceToScope _                  = Local

decSpaceToScope :: VarDec -> Space -> VarScope
decSpaceToScope InterOp _ = StorageBuffer
-- | ^ Input must must be accessed through storage buffer scope
decSpaceToScope _ space   = spaceToScope space

builtinToBuiltinId :: Builtin -> Word32
builtinToBuiltinId GlobalInvocationId = cBuiltinGlobalInvocationId
builtinToBuiltinId WorkgroupId        = cBuiltinWorkgroupId
builtinToBuiltinId NumWorkgroups      = cBuiltinNumWorkgroups
builtinToBuiltinId LocalInvocationId  = cBuiltinLocalInvocationId

insertVarInline :: VName -> VarScope -> SPIRVType -> CompilerM Word32
insertVarInline name scope t = do
  let p_t = Pointer t scope -- Variables must be of pointer type
      storage = scopeToStorageClass scope
  (var_id, _) <- insertReturnOp p_t $ opVariable storage
  ensureTypeId p_t
  modify $ \s -> s { compVarRefs = M.insert name (var_id, t, Inline) $ compVarRefs s }
  return var_id

getUseName :: KernelUse -> VName
getUseName (ScalarUse name _)  = name
getUseName (MemoryUse name _)  = name
getUseName (ConstUse name _)   = name

getUseType :: KernelUse -> CompilerM SPIRVType
getUseType (ScalarUse _ t)  = return $ Scalar t
getUseType (MemoryUse vn _) = do
  access_size_map <- gets compArrayLeastAccessSize
  let elem_size = (M.!) access_size_map vn
      t         = Scalar $ byteSizeToIntPrimType elem_size
  return $ Array t Nothing
getUseType (ConstUse _ exp) = return $ Scalar $ primExpType exp

getUseNameType :: KernelUse -> CompilerM (VName, SPIRVType)
getUseNameType use = do
  use_t <- getUseType use
  return (getUseName use, use_t)

insertDescriptorAccess :: Word32 -> KernelUse -> CompilerM ()
insertDescriptorAccess id use = do
  (name, t) <- getUseNameType use
  let p_t = Pointer t StorageBuffer
      s_t = Struct [t]
  zero_id <- getConstId $ IntValue $ Int32Value 0
  ensureTypeId p_t
  ensureTypeId s_t
  (var_id, _) <- insertReturnOp p_t $ opAccessChain id zero_id
  modify $ \s -> s { compVarRefs = M.insert name (var_id, t, InterOp) $ compVarRefs s }

getResult :: CompilerM [Word32]
getResult = gets compResult

getEntryPoints :: CompilerM [VName]
getEntryPoints = map fst <$> gets compEntryPoints

getDescriptorSets :: CompilerM (M.Map VName DescriptorSet)
getDescriptorSets = gets compDescriptors

getGLSLExtId :: CompilerM Word32
getGLSLExtId = do
  s <- get
  case compGLSLExtId s of
    Just ext_id -> return ext_id
    Nothing -> do
      id <- newId
      modify $ \s_n -> s_n { compGLSLExtId = Just id }
      return id

appendCode :: [Word32] -> CompilerM ()
appendCode code = modify $ \s -> s { compResult = compResult s ++ code }

addEntryPoint :: VName -> Word32 -> CompilerM ()
addEntryPoint name entry_id = modify $ \s -> s { compEntryPoints = (name, entry_id) : compEntryPoints s }

addDescriptors :: VName -> [KernelUse] -> CompilerM ()
addDescriptors kernel uses = do
  id_uses <- mapM (\u -> newId >>= (\id -> return (id, u))) uses
  modify $ \s -> s { compDescriptors = M.insert kernel id_uses $ compDescriptors s }

registerConstUse :: KernelUse -> CompilerM ()
registerConstUse (ConstUse name exp) = do
  let t = Scalar $ primExpType exp
  var_id <- newId
  modify $ \s -> s { compVarRefs       = M.insert name (var_id, t, Constant) $ compVarRefs s,
                     compSpecConstRefs = compSpecConstRefs s ++ [(var_id, t)] }
registerConstUse _                   = return ()

insertLocalMemory :: LocalMemoryUse -> CompilerM ()
insertLocalMemory (name, size) = do
  size_id <- newId
  access_size_map <- gets compArrayLeastAccessSize
  let elem_size = (M.!) access_size_map name
      elem_t    = Scalar $ byteSizeToIntPrimType elem_size
      t         = Array elem_t $ Just size_id
      p_t       = Pointer t Local
      size_t    = Scalar int32
  (var_id, _) <- insertReturnOp p_t $ opVariable cStorageClassWorkgroup
  ensureTypeId p_t
  modify $ \s -> s { compVarRefs       = M.insert name (var_id, t, LocalMem) $ compVarRefs s,
                     compSpecConstRefs = compSpecConstRefs s ++ [(size_id, size_t)] }

getNonSpecConstUses :: [KernelUse] -> [KernelUse]
getNonSpecConstUses []                  = []
getNonSpecConstUses (ConstUse _ _ : ks) = getNonSpecConstUses ks
getNonSpecConstUses (use : ks)          = use : getNonSpecConstUses ks

scopeToStorageClass :: VarScope -> Word32
scopeToStorageClass StorageBuffer   = cStorageClassStorageBuffer
scopeToStorageClass FunctionLocal   = cStorageClassFunction
scopeToStorageClass Local           = cStorageClassWorkgroup
scopeToStorageClass Global          = cStorageClassCrossWorkgroup
scopeToStorageClass Private         = cStorageClassPrivate
scopeToStorageClass UniformConstant = cStorageClassUniformConstant
scopeToStorageClass Input           = cStorageClassInput

insertReturnOp :: SPIRVType -> (Word32 -> Word32 -> [Word32]) -> CompilerM ExprInfo
insertReturnOp t f = do
  t_id <- getTypeId t
  ret_id <- newId
  appendCode $ f t_id ret_id
  return (ret_id, t)

glslReturnOp :: GLSLInstr -> [Word32] -> SPIRVType -> CompilerM ExprInfo
glslReturnOp instr ops t = do
  glsl_id <- getGLSLExtId
  insertReturnOp t $ opExtInst instr ops glsl_id

insertEntryHead :: VName -> CompilerM ()
insertEntryHead name = do
  void_func_t <- getTypeId $ Function Void
  (entry_id, _) <- insertReturnOp Void $ opFunction cFunctionControlNone void_func_t
  addEntryPoint name entry_id
  desc_map <- gets compDescriptors
  newId >>= appendCode . opLabel
  mapM_ (uncurry insertDescriptorAccess) $ (M.!) desc_map name

insertEntryTail :: CompilerM ()
insertEntryTail = do
  appendCode opReturn
  appendCode opFunctionEnd
  clearVars

insertCompareEqual :: PrimType -> Word32 -> Word32 -> CompilerM ExprInfo
insertCompareEqual (FloatType _) x_id y_id = insertReturnOp (Scalar Bool) $ opFOrdEqual x_id y_id
insertCompareEqual (IntType _) x_id y_id   = insertReturnOp (Scalar Bool) $ opIEqual x_id y_id
insertCompareEqual Bool x_id y_id          = insertReturnOp (Scalar Bool) $ opLogicalEqual x_id y_id
insertCompareEqual _ _ _                   = fail "Equality of certs not supported."

insertStore :: Word32 -> Word32 -> CompilerM ()
insertStore to_id from_id = appendCode $ opStore to_id from_id []

insertInternalArrayWrite :: VName -> VarScope -> PrimType -> Int -> PrimValue -> CompilerM ()
insertInternalArrayWrite dest scope r_pt i val = do
  let aligned_i = i * primByteSize r_pt
  val_id <- getConstId val
  i_id <- getConstId $ IntValue $ Int32Value $ fromIntegral aligned_i
  insertArrayWrite dest i_id val_id r_pt scope Nonvolatile

insertLoop :: CompilerM Word32 -> CompilerM () -> CompilerM () -> CompilerM ()
insertLoop check body continue = do
  start_id <- newId
  check_id <- newId
  body_id <- newId
  continue_id <- newId
  end_id <- newId
  appendCode $ opBranch start_id
  appendCode $ opLabel start_id
  appendCode $ opLoopMerge end_id continue_id
  appendCode $ opBranch check_id
  appendCode $ opLabel check_id
  cond_id <- check
  appendCode $ opBranchConditional cond_id body_id end_id
  appendCode $ opLabel body_id
  body
  appendCode $ opBranch continue_id
  appendCode $ opLabel continue_id
  continue
  appendCode $ opBranch start_id
  appendCode $ opLabel end_id

insertIf :: Word32 -> CompilerM () -> CompilerM () -> CompilerM ()
insertIf cond_id t_branch f_branch = do
  true_id <- newId
  false_id <- newId
  end_id <- newId
  appendCode $ opSelectionMerge end_id
  appendCode $ opBranchConditional cond_id true_id false_id
  appendCode $ opLabel true_id
  t_branch
  appendCode $ opBranch end_id
  appendCode $ opLabel false_id
  f_branch
  appendCode $ opBranch end_id
  appendCode $ opLabel end_id

insertVarLoad :: VarInfo -> CompilerM ExprInfo
insertVarLoad (var_id, var_t, Constant) = return (var_id, var_t)
insertVarLoad (var_id, var_t, _) = insertReturnOp var_t $ opLoad var_id []

byteSizeToIntPrimType :: Int -> PrimType
byteSizeToIntPrimType 1 = int8
byteSizeToIntPrimType 2 = int16
byteSizeToIntPrimType 4 = int32
byteSizeToIntPrimType 8 = int64

atomicOpArrayAndExp :: AtomicOp -> (VName, Exp, Maybe Exp)
atomicOpArrayAndExp (AtomicAdd _ rname _ e) = (rname, e, Nothing)
atomicOpArrayAndExp (AtomicSMax _ rname _ e) = (rname, e, Nothing)
atomicOpArrayAndExp (AtomicSMin _ rname _ e) = (rname, e, Nothing)
atomicOpArrayAndExp (AtomicUMax _ rname _ e) = (rname, e, Nothing)
atomicOpArrayAndExp (AtomicUMin _ rname _ e) = (rname, e, Nothing)
atomicOpArrayAndExp (AtomicAnd _ rname _ e) = (rname, e, Nothing)
atomicOpArrayAndExp (AtomicOr _ rname _ e) = (rname, e, Nothing)
atomicOpArrayAndExp (AtomicXor _ rname _ e) = (rname, e, Nothing)
atomicOpArrayAndExp (AtomicCmpXchg _ rname _ le re) = (rname, le, Just re)
atomicOpArrayAndExp (AtomicXchg _ rname _ e) = (rname, e, Nothing)

-- Bitcast assumes equal size (Boolean values are size-less)
insertBitcast :: PrimType -> PrimType -> Word32 -> CompilerM ExprInfo
insertBitcast Cert _ _        = fail "Cert bitcasts are not supported"
insertBitcast _ Cert _        = fail "Cert bitcasts are not supported"
insertBitcast to_pt from_pt from_id
  | to_pt == from_pt = return (from_id, Scalar from_pt)
insertBitcast Bool from_pt from_id = do
  zero_id <- getConstId $ blankPrimValue from_pt
  insertCompareEqual from_pt zero_id from_id
insertBitcast to_pt@(IntType to_it) Bool from_id = do
  zero_id <- getConstId $ IntValue $ intValue to_it 0
  one_id  <- getConstId $ IntValue $ intValue to_it 1
  insertReturnOp (Scalar to_pt) $ opSelect from_id one_id zero_id
insertBitcast to_pt@(FloatType to_ft) Bool from_id = do
  zero_id <- getConstId $ FloatValue $ floatValue to_ft 0.0
  one_id  <- getConstId $ FloatValue $ floatValue to_ft 1.0
  insertReturnOp (Scalar to_pt) $ opSelect from_id one_id zero_id
insertBitcast to_pt _ from_id = insertReturnOp (Scalar to_pt) (opBitcast from_id)

insertShiftRightConst :: SPIRVType -> Word32 -> Int -> CompilerM Word32
insertShiftRightConst _ val_id 0            = return val_id
insertShiftRightConst val_t val_id shift_by = do
  sb_id <- getConstId $ IntValue $ Int32Value $ fromIntegral shift_by
  fst <$> insertReturnOp val_t (opShiftRightLogical val_id sb_id)

insertShiftLeftConst :: SPIRVType -> Word32 -> Int -> CompilerM Word32
insertShiftLeftConst _ val_id 0            = return val_id
insertShiftLeftConst val_t val_id shift_by = do
  sb_id <- getConstId $ IntValue $ Int32Value $ fromIntegral shift_by
  fst <$> insertReturnOp val_t (opShiftLeftLogical val_id sb_id)

insertIntConversion :: SPIRVType -> SPIRVType -> Word32 -> CompilerM Word32
insertIntConversion to_t from_t from_id
  | to_t == from_t = return from_id
  | otherwise      = fst <$> insertReturnOp to_t (opUConvert from_id)

insertIndexIncrConst :: Word32 -> Int -> CompilerM Word32
insertIndexIncrConst i_id 0 = return i_id
insertIndexIncrConst i_id v = do
  v_id <- getConstId $ IntValue $ Int32Value $ fromIntegral v
  fst <$> insertReturnOp (Scalar int32) (opIAdd i_id v_id)

insertArrayWriteChunk :: SPIRVType -> VarScope -> Volatility -> Word32 -> Word32 -> Word32 -> CompilerM ()
insertArrayWriteChunk chunk_t scope vol arr_id i_id b_id = do
  let pc_t = Pointer chunk_t scope
      mem_a = getMemoryAccessType vol
  (chain_id, _) <- insertReturnOp pc_t $ opAccessChain arr_id i_id
  appendCode $ opStore chain_id b_id [mem_a]

insertArrayWrite :: VName -> Word32 -> Word32 -> PrimType -> VarScope -> Volatility -> CompilerM ()
insertArrayWrite arr i_id val_id val_pt scope vol = do
  (arr_id, _, _) <- getVarInfo arr
  access_size_map <- gets compArrayLeastAccessSize
  let elem_size = (M.!) access_size_map arr
      r_size = primByteSize val_pt
      chunks = r_size `div` elem_size
      int_pt = byteSizeToIntPrimType r_size
      val_t  = Scalar val_pt
      int_t  = Scalar int_pt
      c_t    = Scalar $ byteSizeToIntPrimType elem_size
  c_id <- fst <$> insertBitcast int_pt val_pt val_id
  shf_ids <- mapM (insertShiftRightConst int_t c_id . (elem_size*8*)) [0..chunks-1]
  vs_ids <- mapM (insertIntConversion c_t int_t) shf_ids
  is_ids <- mapM (insertIndexIncrConst i_id . fromIntegral) [0..chunks-1]
  zipWithM_ (insertArrayWriteChunk c_t scope vol arr_id) is_ids vs_ids

insertArrayReadChunk :: SPIRVType -> VarScope -> Volatility -> Word32 -> Word32 -> CompilerM Word32
insertArrayReadChunk chunk_t scope vol arr_id i_id = do
  let pc_t  = Pointer chunk_t scope
      mem_a = getMemoryAccessType vol
  (chain_id, _) <- insertReturnOp pc_t $ opAccessChain arr_id i_id
  fst <$> insertReturnOp chunk_t (opLoad chain_id [mem_a])

insertArrayRead :: VName -> Word32 -> PrimType -> VarScope -> Volatility -> CompilerM ExprInfo
insertArrayRead arr i_id r_pt scope vol = do
  (arr_id, _, _) <- getVarInfo arr
  access_size_map <- gets compArrayLeastAccessSize
  let elem_size = (M.!) access_size_map arr
      r_t    = Scalar r_pt
      r_size = primByteSize r_pt
      chunks = r_size `div` elem_size
      c_t    = Scalar $ byteSizeToIntPrimType elem_size
      int_pt = byteSizeToIntPrimType r_size
      int_t  = Scalar int_pt
      shf_is = map (elem_size*8*) [0..chunks-1]
  is_ids <- mapM (insertIndexIncrConst i_id . fromIntegral) [0..chunks-1]
  vs_ids <- mapM (insertArrayReadChunk c_t scope vol arr_id) is_ids
  vis_ids <- mapM (insertIntConversion int_t c_t) vs_ids
  shf_ids <- zipWithM (insertShiftLeftConst int_t) vis_ids shf_is
  zero_id <- getConstId $ blankPrimValue int_pt
  iv_id <- foldM (\x y -> fst <$> insertReturnOp int_t (opIAdd x y)) zero_id shf_ids
  insertBitcast r_pt int_pt iv_id

readBuiltinTo :: Builtin -> Int32 -> VName -> CompilerM ()
readBuiltinTo builtin i target = do
  let r_t   = Scalar int32
      vr_t  = Vector r_t 3
      pr_t  = Pointer r_t Input
      pvr_t = Pointer vr_t Input
  ensureTypeId pvr_t
  var_id <- getBuiltinId builtin
  i_id <- getConstId $ IntValue $ Int32Value i
  (chain_id, _) <- insertReturnOp pr_t $ opAccessChain var_id i_id
  (load_id, _) <- insertReturnOp r_t $ opLoad chain_id []
  (target_id, _, _) <- getVarInfo target
  appendCode $ opStore target_id load_id []

readWorkgroupSizeTo :: Word32 -> VName -> CompilerM ()
readWorkgroupSizeTo i target = do
  let r_t   = Scalar int32
      ws_id = reservedId WorkgroupSize
  (v_id, _) <- insertReturnOp r_t $ opCompositeExtract ws_id i
  (target_id, _, _) <- getVarInfo target
  appendCode $ opStore target_id v_id []

getMemoryAccessType :: Volatility -> Word32
getMemoryAccessType Volatile = cMemoryAccessVolatile
getMemoryAccessType Nonvolatile = cMemoryAccessNone

getPrimTypeDeclaration :: PrimType -> Word32 -> [Word32]
getPrimTypeDeclaration t@(IntType _) id   = opTypeInt (fromIntegral (primBitSize t)) cNoSignedness id
getPrimTypeDeclaration t@(FloatType _) id = opTypeFloat (fromIntegral (primBitSize t)) id
getPrimTypeDeclaration Bool id            = opTypeBool id
getPrimTypeDeclaration Cert _             = []

getScalarTypeDeclaration :: SPIRVType -> Word32 -> CompilerM [Word32]
getScalarTypeDeclaration (Scalar t) id = return $ getPrimTypeDeclaration t id
getScalarTypeDeclaration _ _           = return []

getScalarTypeDeclarations :: CompilerM [Word32]
getScalarTypeDeclarations = do
  type_map <- gets compTypeRefs
  types <- sequence $ M.elems $ M.mapWithKey getScalarTypeDeclaration type_map
  return $ concat types

getNonScalarTypeDeclaration :: SPIRVType -> Word32 -> CompilerM [Word32]
getNonScalarTypeDeclaration (Scalar _) _           = return []
getNonScalarTypeDeclaration Void id                = return $ opTypeVoid id
getNonScalarTypeDeclaration (Vector t len) id      = do
  t_id <- getTypeId t
  return $ opTypeVector t_id len id
getNonScalarTypeDeclaration (Pointer t scope) id   = do
  let storage = scopeToStorageClass scope
  t_id <- getTypeId t
  return $ opTypePointer storage t_id id
getNonScalarTypeDeclaration (Function t) id        = do
  t_id <- getTypeId t
  return $ opTypeFunction t_id id
getNonScalarTypeDeclaration (Struct ts) id         = do
  t_ids <- mapM getTypeId ts
  return $ opTypeStruct t_ids id
getNonScalarTypeDeclaration (Array t s) id         = do
  t_id <- getTypeId t
  return $ maybe (opTypeRuntimeArray t_id id) (\size -> opTypeArray t_id size id) s

getNonScalarTypeDeclarations :: CompilerM [Word32]
getNonScalarTypeDeclarations = do
  type_map <- gets compTypeRefs
  types <- sequence $ M.elems $ M.mapWithKey getNonScalarTypeDeclaration type_map
  return $ concat types

getBuiltinVarDeclaration :: Builtin -> Word32 -> CompilerM [Word32]
getBuiltinVarDeclaration _ id = do
  let t   = Scalar int32
      vt  = Vector t 3
      pvt = Pointer vt Input
  t_id <- getTypeId pvt
  return $ opVariable cStorageClassInput t_id id

getBuiltinVarDeclarations :: CompilerM [Word32]
getBuiltinVarDeclarations = do
  builtin_map <- gets compBuiltinRefs
  builtin <- sequence $ M.elems $ M.mapWithKey getBuiltinVarDeclaration builtin_map
  return $ concat builtin

getDescVarDeclaration :: Word32 -> KernelUse -> CompilerM [Word32]
getDescVarDeclaration id use = do
  t <- getUseType use
  pt_id <- getTypeId $ Pointer (Struct [t]) StorageBuffer
  return $ opVariable cStorageClassStorageBuffer pt_id id

getDescVarDeclarations :: CompilerM [Word32]
getDescVarDeclarations = do
  desc_map <- gets compDescriptors
  descs <- sequence $ M.elems $ M.map (mapM (uncurry getDescVarDeclaration)) desc_map
  return $ concat $ concat descs

intValueToWords :: IntValue -> [Word32]
intValueToWords (Int8Value v)  = [fromIntegral v]
intValueToWords (Int16Value v) = [fromIntegral v]
intValueToWords (Int32Value v) = [fromIntegral v]
intValueToWords (Int64Value v) = [fromIntegral v, fromIntegral $ shiftR v 32]

floatValueToWords :: FloatValue -> [Word32]
floatValueToWords (Float32Value v) = [floatToWord v]
floatValueToWords (Float64Value v) = let w = doubleToWord v
                                     in [fromIntegral w, fromIntegral $ shiftR w 32]

getConstDeclaration :: PrimValue -> Word32 -> CompilerM [Word32]
getConstDeclaration (BoolValue b) id = do
  let op = if b then opConstantTrue else opConstantFalse
  t_id <- getTypeId $ Scalar Bool
  return $ op t_id id
getConstDeclaration (IntValue i) id = do
  let lit = intValueToWords i
  t_id <- getTypeId $ Scalar $ IntType $ intValueType i
  return $ opConstant lit t_id id
getConstDeclaration (FloatValue f) id = do
  let lit = floatValueToWords f
  t_id <- getTypeId $ Scalar $ FloatType $ floatValueType f
  return $ opConstant lit t_id id
getConstDeclaration Checked _ = return []

getConstDeclarations :: CompilerM [Word32]
getConstDeclarations = do
  const_map <- gets compConstRefs
  consts <- sequence $ M.elems $ M.mapWithKey getConstDeclaration const_map
  return $ concat consts

getSpecConstDeclaration :: (Word32, SPIRVType) -> CompilerM [Word32]
getSpecConstDeclaration (id, t@(Scalar Bool)) = do
  t_id <- getTypeId t
  return $ opSpecConstantFalse t_id id
getSpecConstDeclaration (id, t@(Scalar pt)) = do
  let num_lits = primBitSize pt `div` 32
      default_lit = replicate num_lits 1
  t_id <- getTypeId t
  return $ opSpecConstant default_lit t_id id
getSpecConstDeclaration _ = return []

getSpecConstDeclarations :: CompilerM [Word32]
getSpecConstDeclarations = do
  spec_refs <- gets compSpecConstRefs
  spec_decls <- mapM getSpecConstDeclaration spec_refs 
  return $ concat spec_decls

getReservedDeclarations :: CompilerM [Word32]
getReservedDeclarations = do
  let ws_x_id = reservedSpecId WorkgroupSizeXSpec
      ws_id   = reservedId WorkgroupSize
  one_id  <- getConstId $ IntValue $ Int32Value 1
  vec3_id <- getTypeId $ Vector (Scalar int32) 3
  return $ opSpecConstantComposite [ws_x_id, one_id, one_id] vec3_id ws_id

getEntryPointDeclaration :: VName -> Word32 -> [Word32] -> [Word32]
getEntryPointDeclaration name id inputs =
  let name_string = encodeString $ entryPointName name
  in opEntryPoint cExecutionModelGLCompute id name_string inputs

getEntryPointDeclarations :: CompilerM [Word32]
getEntryPointDeclarations = do
  builtin_ids <- M.elems <$> gets compBuiltinRefs
  concatMap (\(n, id) -> getEntryPointDeclaration n id builtin_ids) <$> gets compEntryPoints

getExecutionModeDeclaration :: Word32 -> [Word32]
getExecutionModeDeclaration id = opExecutionMode id cExecutionModeLocalSize [1, 1, 1]

getExecutionModeDeclarations :: CompilerM [Word32]
getExecutionModeDeclarations =
  concatMap (getExecutionModeDeclaration . snd) <$> gets compEntryPoints

getTypeDecoration :: SPIRVType -> Word32 -> [Word32]
getTypeDecoration (Array _ _) id  = opDecorate id cDecorationArrayStride [1]
getTypeDecoration (Struct _) id =
  opDecorate id cDecorationBlock [] ++
  opMemberDecorate id 0 cDecorationOffset [0]
getTypeDecoration _ _           = []

getTypeDecorations :: CompilerM [Word32]
getTypeDecorations =
  concat . M.elems . M.mapWithKey getTypeDecoration <$> gets compTypeRefs

getBuiltinDecoration :: Builtin -> Word32 -> [Word32]
getBuiltinDecoration builtin id = opDecorate id cDecorationBuiltin [bid]
  where bid = builtinToBuiltinId builtin

getBuiltinDecorations :: CompilerM [Word32]
getBuiltinDecorations =
  concat . M.elems . M.mapWithKey getBuiltinDecoration <$> gets compBuiltinRefs

getSpecConstDecoration :: Word32 -> Word32 -> [Word32]
getSpecConstDecoration id const_id = opDecorate id cDecorationSpecId [const_id]

getSpecConstDecorations :: CompilerM [Word32]
getSpecConstDecorations = do
  spec_ids <- map fst <$> gets compSpecConstRefs
  let const_ids = map fromIntegral [0..length spec_ids - 1]
  return $ concat $ zipWith getSpecConstDecoration spec_ids const_ids

getReservedDecorations :: CompilerM [Word32]
getReservedDecorations =
  let ws_id = reservedId WorkgroupSize
  in return $ opDecorate ws_id cDecorationBuiltin [cBuiltinWorkgroupSize]

getKernelDescriptorDecorations :: Word32 -> Word32 -> Descriptor -> [Word32]
getKernelDescriptorDecorations set binding (id, _) =
  opDecorate id cDecorationDescriptorSet [set] ++
  opDecorate id cDecorationBinding [binding]

getKernelDescriptorSetDecorations :: Word32 -> DescriptorSet -> [Word32]
getKernelDescriptorSetDecorations set desc_set =
  let desc_is = map fromIntegral [0..length desc_set - 1]
  in concat $ zipWith (getKernelDescriptorDecorations set) desc_is desc_set

getDescriptorDecorations :: CompilerM [Word32]
getDescriptorDecorations = do
  descs <- M.elems <$> gets compDescriptors
  let set_is = map fromIntegral [0..length descs - 1]
  return $ concat $ zipWith getKernelDescriptorSetDecorations set_is descs

getDecorations :: CompilerM [Word32]
getDecorations = do
  desc_deco     <- getDescriptorDecorations
  reserved_deco <- getReservedDecorations
  spec_deco     <- getSpecConstDecorations
  type_deco     <- getTypeDecorations
  builtin_deco  <- getBuiltinDecorations
  return $ concat [type_deco, builtin_deco, reserved_deco, spec_deco, desc_deco]

compileLeaf :: ExpLeaf -> CompilerM ExprInfo
compileLeaf (SizeOf t) = do
  s_id <- getConstId $ IntValue $ Int32Value $ primByteSize t
  return (s_id, Scalar int32)
compileLeaf (ScalarVar src) = getVarInfo src >>= insertVarLoad 
compileLeaf (Index src (Count iexp) restype space vol) = do
  let mem_access = getMemoryAccessType vol
  (_, _, src_dec) <- getVarInfo src
  let scope = decSpaceToScope src_dec space
  (i_id, _) <- compileExp iexp
  insertArrayRead src i_id restype scope vol

compileExp :: Exp -> CompilerM ExprInfo
compileExp = compilePrimExp compileLeaf

compilePrimExp :: (v -> CompilerM ExprInfo) -> PrimExp v -> CompilerM ExprInfo
compilePrimExp f (LeafExp v _) = f v
compilePrimExp _ (ValueExp val) = do
  id <- getConstId val
  let t = Scalar $ primValueType val
  return (id, t)
compilePrimExp f (UnOpExp uop x) = do
  (x_id, _) <- compilePrimExp f x
  let t = Scalar $ unOpType uop
  case uop of
    Complement{} -> case t of
                      (Scalar Bool)          -> insertReturnOp t $ opLogicalNot x_id
                      (Scalar (IntType _))   -> insertReturnOp t $ opNot x_id
                      (Scalar (FloatType _)) -> fail "Float complement not implemented" -- TODO: Fix 
                      (Scalar Cert)          -> fail "Cert complement is not supported" 
    Not{}        -> insertReturnOp t $ opLogicalNot x_id
    Abs{}        -> glslReturnOp glslSAbs [x_id] t
    FAbs{}       -> glslReturnOp glslFAbs [x_id] t
    SSignum{}    -> glslReturnOp glslSSign [x_id] t
    USignum{}    -> glslReturnOp glslFSign [x_id] t
compilePrimExp f (CmpOpExp cmp x y) = do
  (x_id, _) <- compilePrimExp f x
  (y_id, _) <- compilePrimExp f y
  let t = Scalar Bool
  case cmp of
    CmpEq{}  -> insertCompareEqual (cmpOpType cmp) x_id y_id
    FCmpLt{} -> insertReturnOp t $ opFOrdLessThan x_id y_id
    FCmpLe{} -> insertReturnOp t $ opFOrdLessThanEqual x_id y_id
    CmpSle{} -> insertReturnOp t $ opSLessThanEqual x_id y_id
    CmpSlt{} -> insertReturnOp t $ opSLessThan x_id y_id
    CmpUle{} -> insertReturnOp t $ opULessThanEqual x_id y_id
    CmpUlt{} -> insertReturnOp t $ opULessThan x_id y_id
    CmpLlt{} -> do
      (n_x_id, _) <- insertReturnOp t $ opNot x_id
      insertReturnOp t $ opLogicalAnd n_x_id y_id
    CmpLle{} -> do
      (n_x_id, _) <- insertReturnOp t $ opNot x_id
      insertReturnOp t $ opLogicalOr n_x_id y_id
compilePrimExp f (ConvOpExp conv x) = do
  (x_id, x_t) <- compilePrimExp f x
  let (_, r_it) = convOpType conv
      r_t       = Scalar r_it
  case conv of
    ZExt{}   -> if x_t == r_t then return (x_id, x_t)
                              else insertReturnOp r_t $ opUConvert x_id
    SExt{}   -> if x_t == r_t then return (x_id, x_t)
                              else insertReturnOp r_t $ opSConvert x_id
    FPConv{} -> if x_t == r_t then return (x_id, x_t)
                              else insertReturnOp r_t $ opFConvert x_id
    FPToUI{} -> insertReturnOp r_t $ opConvertFToU x_id
    FPToSI{} -> insertReturnOp r_t $ opConvertFToS x_id
    UIToFP{} -> insertReturnOp r_t $ opConvertUToF x_id
    SIToFP{} -> insertReturnOp r_t $ opConvertSToF x_id
compilePrimExp f (BinOpExp bop x y) = do
  (x_id, _) <- compilePrimExp f x
  (y_id, _) <- compilePrimExp f y
  let t = Scalar $ binOpType bop
  case bop of
    Add{}    -> insertReturnOp t $ opIAdd x_id y_id
    FAdd{}   -> insertReturnOp t $ opFAdd x_id y_id
    Sub{}    -> insertReturnOp t $ opISub x_id y_id
    FSub{}   -> insertReturnOp t $ opFSub x_id y_id
    Mul{}    -> insertReturnOp t $ opIMul x_id y_id
    FMul{}   -> insertReturnOp t $ opFMul x_id y_id
    FDiv{}   -> insertReturnOp t $ opFDiv x_id y_id
    UDiv{}   -> insertReturnOp t $ opUDiv x_id y_id
    SDiv{}   -> insertReturnOp t $ opSDiv x_id y_id
    UMod{}   -> insertReturnOp t $ opUMod x_id y_id
    SMod{}   -> insertReturnOp t $ opSMod x_id y_id
    SQuot{}  -> insertReturnOp t $ opUDiv x_id y_id
    SRem{}   -> insertReturnOp t $ opSRem x_id y_id
    FMin{}   -> glslReturnOp glslFMin [x_id, y_id] t
    SMin{}   -> glslReturnOp glslSMin [x_id, y_id] t
    UMin{}   -> glslReturnOp glslUMin [x_id, y_id] t
    FMax{}   -> glslReturnOp glslFMax [x_id, y_id] t
    UMax{}   -> glslReturnOp glslUMax [x_id, y_id] t
    SMax{}   -> glslReturnOp glslSMax [x_id, y_id] t
    Pow{}    -> fail "Integer Pow not implemented" -- TODO: Fix!
    FPow{}   -> glslReturnOp glslPow [x_id, y_id] t
    Xor{}    -> insertReturnOp t $ opBitwiseXor x_id y_id
    And{}    -> insertReturnOp t $ opBitwiseAnd x_id y_id
    Or{}     -> insertReturnOp t $ opBitwiseOr x_id y_id
    Shl{}    -> insertReturnOp t $ opShiftLeftLogical x_id y_id
    LShr{}   -> insertReturnOp t $ opShiftRightLogical x_id y_id
    AShr{}   -> insertReturnOp t $ opShiftRightArithmetic x_id y_id
    LogAnd{} -> insertReturnOp t $ opLogicalAnd x_id y_id
    LogOr{}  -> insertReturnOp t $ opLogicalOr x_id y_id
compilePrimExp f (FunExp "isnan32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  insertReturnOp (Scalar t) $ opIsNan e_id
compilePrimExp f (FunExp "isinf32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  insertReturnOp (Scalar t) $ opIsInf e_id
compilePrimExp f (FunExp "round32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslRound [e_id] (Scalar t)
compilePrimExp f (FunExp "log32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslLog [e_id] (Scalar t)
compilePrimExp f (FunExp "log2_32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslLog2 [e_id] (Scalar t)
compilePrimExp f (FunExp "log10_32" [e] t) = do
  base_id <- getConstId $ FloatValue $ Float32Value 10.0
  (e_id, _) <- compilePrimExp f e
  (numer_id, _) <- glslReturnOp glslLog [e_id] (Scalar t)
  (denom_id, _) <- glslReturnOp glslLog [base_id] (Scalar t)
  insertReturnOp (Scalar t) $ opFDiv numer_id denom_id
compilePrimExp f (FunExp "exp32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslExp [e_id] (Scalar t)
compilePrimExp f (FunExp "sqrt32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslSqrt [e_id] (Scalar t)
compilePrimExp f (FunExp "sin32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslSin [e_id] (Scalar t)
compilePrimExp f (FunExp "cos32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslCos [e_id] (Scalar t)
compilePrimExp f (FunExp "tan32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslTan [e_id] (Scalar t)
compilePrimExp f (FunExp "asin32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslASin [e_id] (Scalar t)
compilePrimExp f (FunExp "acos32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslACos [e_id] (Scalar t)
compilePrimExp f (FunExp "atan32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  glslReturnOp glslATan [e_id] (Scalar t)
compilePrimExp f (FunExp "atan2_32" [le, re] t) = do
  (le_id, _) <- compilePrimExp f le
  (re_id, _) <- compilePrimExp f re
  glslReturnOp glslATan2 [le_id, re_id] (Scalar t)
compilePrimExp f (FunExp "to_bits32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  insertBitcast int32 float32 e_id
compilePrimExp f (FunExp "from_bits32" [e] t) = do
  (e_id, _) <- compilePrimExp f e
  insertBitcast float32 int32 e_id
compilePrimExp _ (FunExp h _ _) = fail $ "FunExp " ++ pretty h ++ " not implemented."

compileAtomicOp :: AtomicOp -> CompilerM ()
compileAtomicOp op = do
  let int_t = Scalar int32
      ptr_t = Pointer int_t StorageBuffer
  (s_op, old, arr, i, e) <- unpack op
  (arr_id, _, _) <- getVarInfo arr
  (old_id, _, _) <- getVarInfo old
  dev_scope_id <- getConstId $ IntValue $ Int32Value $ fromIntegral cScopeDevice
  sem_id <- getConstId $ IntValue $ Int32Value $ fromIntegral $
              cMemorySemanticsUniformMemory .|. cMemorySemanticsSequentiallyConsistent
  (i_id, _) <- compileExp i
  (e_id, _) <- compileExp e
  (ptr_id, _) <- insertReturnOp ptr_t $ opAccessChain arr_id i_id
  (r_id, _) <- insertReturnOp int_t $ s_op ptr_id dev_scope_id sem_id e_id
  insertStore old_id r_id
  where unpack (AtomicAdd old arr (Count i) e) = return (opAtomicIAdd, old, arr, i, e)
        unpack (AtomicSMax old arr (Count i) e) = return (opAtomicSMax, old, arr, i, e)
        unpack (AtomicSMin old arr (Count i) e) = return (opAtomicSMin, old, arr, i, e)
        unpack (AtomicUMax old arr (Count i) e) = return (opAtomicUMax, old, arr, i, e)
        unpack (AtomicUMin old arr (Count i) e) = return (opAtomicUMin, old, arr, i, e)
        unpack (AtomicAnd old arr (Count i) e) = return (opAtomicAnd, old, arr, i, e)
        unpack (AtomicOr old arr (Count i) e) = return (opAtomicOr, old, arr, i, e)
        unpack (AtomicXor old arr (Count i) e) = return (opAtomicXor, old, arr, i, e)
        unpack (AtomicXchg old arr (Count i) e) = return (opAtomicExchange, old, arr, i, e)
        unpack (AtomicCmpXchg old arr (Count i) cmp e) = do
          (cmp_id, _) <- compileExp cmp
          let op_f p sc sm v = opAtomicCompareExchange p sc sm sm v cmp_id
          return (op_f, old, arr, i, e)

compileKernelOp :: KernelOp -> CompilerM ()
compileKernelOp (GetGroupId name i) = readBuiltinTo WorkgroupId (fromIntegral i) name
compileKernelOp (GetLocalId name i) = readBuiltinTo LocalInvocationId (fromIntegral i) name
compileKernelOp (GetGlobalSize name i) = readBuiltinTo NumWorkgroups (fromIntegral i) name
compileKernelOp (GetGlobalId name i) = readBuiltinTo GlobalInvocationId (fromIntegral i) name
compileKernelOp (GetLocalSize name i) = readWorkgroupSizeTo (fromIntegral i) name
compileKernelOp (GetLockstepWidth name) = do
  (var_id, _, _) <- getVarInfo name
  insertStore var_id $ reservedSpecId LockstepWidthSpec
compileKernelOp (Atomic op) = compileAtomicOp op
compileKernelOp Barrier = do
  wg_scope_id <- getConstId $ IntValue $ Int32Value $ fromIntegral cScopeWorkgroup
  sem_id <- getConstId $ IntValue $ Int32Value $ fromIntegral $
              cMemorySemanticsWorkgroupMemory .|. cMemorySemanticsSequentiallyConsistent
  appendCode $ opControlBarrier wg_scope_id wg_scope_id sem_id
compileKernelOp MemFence = do
  dev_scope_id <- getConstId $ IntValue $ Int32Value $ fromIntegral cScopeDevice
  sem_id <- getConstId $ IntValue $ Int32Value $ fromIntegral $
              cMemorySemanticsUniformMemory .|. cMemorySemanticsSequentiallyConsistent
  appendCode $ opMemoryBarrier dev_scope_id sem_id

compileCode :: Code KernelOp -> CompilerM ()
compileCode Skip = return ()
compileCode (Op op) = compileKernelOp op
compileCode (lc :>>: rc) =  compileCode lc >> compileCode rc
compileCode (Comment _ c) = compileCode c
-- ^ SPIR-V does not support comments
compileCode (DeclareScalar name t) = return ()
-- ^ Used earlier
compileCode (SetScalar dest src) = do
  (var_id, _, _) <- getVarInfo dest
  (src_id, _) <- compileExp src
  insertStore var_id src_id
compileCode (DeclareArray name space t vs) = do
  let len     = length vs
      scope   = spaceToScope space
  zipWithM_ (insertInternalArrayWrite name scope t) [0..len-1] vs
compileCode (DeclareMem name space) = fail "DeclareMem not implemented."
compileCode (SetMem dest src space) = fail "SetMem not implemented."
compileCode (Copy dest (Count destoffset) destspace src (Count srcoffset) space (Count size)) = fail "Copy not implemented."
compileCode (Write dest (Count idx) elemtype space vol elemexp) = do
  (elem_id, _) <- compileExp elemexp
  (i_id, _) <- compileExp idx
  (_, _, dest_dec) <- getVarInfo dest
  let scope = decSpaceToScope dest_dec space
  insertArrayWrite dest i_id elem_id elemtype scope vol
compileCode (Call dests fname args) = fail $ "Call to " ++ pretty fname ++ " not implemented."
compileCode (Assert e (ErrorMsg parts) (loc, locs)) = return () -- TODO: Fix?
compileCode DebugPrint{} = return ()
compileCode (If cond tbranch fbranch) = do
  (cond_id, _) <- compileExp cond
  insertIf cond_id (compileCode tbranch) (compileCode fbranch)
compileCode (While cond body) =
  insertLoop (fst <$> compileExp cond) (compileCode body) $ return ()
compileCode (For i it bound body) = do
  var_i@(i_id, _, _) <- getVarInfo i
  init_id <- getConstId $ IntValue $ intValue it 0
  appendCode $ opStore i_id init_id []
  insertLoop (check i_id) (compileCode body) (continue var_i)
  where sit = Scalar $ IntType it
        check i_id = do
          (condi_id, _) <- insertReturnOp sit $ opLoad i_id []
          (bound_id, _) <- compileExp bound
          (check_id, _) <- insertReturnOp (Scalar Bool) $ opULessThan condi_id bound_id
          return check_id
        continue var_i@(i_id, _, _) = do
          one_id <- getConstId $ IntValue $ intValue it 1
          (i_val_id, _) <- insertVarLoad var_i
          (inc_id, _) <- insertReturnOp sit $ opIAdd i_val_id one_id 
          insertStore i_id inc_id
compileCode _ = fail "Expression not supported in kernel."

-- | Inline variables must be declared in the first block
compileEarlyDecls :: Code KernelOp -> CompilerM ()
compileEarlyDecls (DeclareScalar name t) =
  void $ insertVarInline name FunctionLocal (Scalar t)
compileEarlyDecls (lc :>>: rc) =  compileEarlyDecls lc >> compileEarlyDecls rc
compileEarlyDecls (Comment _ c) = compileEarlyDecls c
compileEarlyDecls (DeclareArray name space t vs) = do
  access_size_map <- gets compArrayLeastAccessSize
  let elem_size = (M.!) access_size_map name
      t         = Scalar $ byteSizeToIntPrimType elem_size
      len       = length vs
      scope     = spaceToScope space
  void $ insertVarInline name scope $ Array t $ Just $ fromIntegral len
compileEarlyDecls (If cond tbranch fbranch) =
  compileEarlyDecls tbranch >> compileEarlyDecls fbranch
compileEarlyDecls (While cond body) = compileEarlyDecls body
compileEarlyDecls (For i it _ body) = do
  let sit = Scalar $ IntType it
  void $ insertVarInline i FunctionLocal sit
  compileEarlyDecls body
compileEarlyDecls _ = return ()

analyzeExpArrayAccessSizes :: Exp -> CompilerM ()
analyzeExpArrayAccessSizes (LeafExp (Index src _ restype _ _) _) =
  let size = primByteSize restype
  in suggestArrayLeastAccessSize src size
analyzeExpArrayAccessSizes _ = return ()

analyzeCodeArrayAccessSizes :: Code KernelOp -> CompilerM ()
analyzeCodeArrayAccessSizes (Op (Atomic op)) = do
  let (arr, e, me) = atomicOpArrayAndExp op
  suggestArrayLeastAccessSize arr 4
  analyzeExpArrayAccessSizes e
  maybe (return ()) analyzeExpArrayAccessSizes me
analyzeCodeArrayAccessSizes (lc :>>: rc) = do
  analyzeCodeArrayAccessSizes lc
  analyzeCodeArrayAccessSizes rc
analyzeCodeArrayAccessSizes (Comment _ c) = analyzeCodeArrayAccessSizes c
analyzeCodeArrayAccessSizes (Copy _ (Count destoffset) _ _ (Count srcoffset) _ (Count size)) = do
  analyzeExpArrayAccessSizes destoffset
  analyzeExpArrayAccessSizes srcoffset
  analyzeExpArrayAccessSizes size
analyzeCodeArrayAccessSizes (Write dest (Count idx) elemtype _ _ elemexp) = do
  analyzeExpArrayAccessSizes elemexp
  analyzeExpArrayAccessSizes idx
  let size = primByteSize elemtype
  suggestArrayLeastAccessSize dest size
analyzeCodeArrayAccessSizes (DeclareArray name _ t _) =
  let size = primByteSize t
  in suggestArrayLeastAccessSize name size
analyzeCodeArrayAccessSizes (SetScalar _ src) = analyzeExpArrayAccessSizes src
analyzeCodeArrayAccessSizes (If cond tbranch fbranch) = do
  analyzeExpArrayAccessSizes cond
  analyzeCodeArrayAccessSizes tbranch
  analyzeCodeArrayAccessSizes fbranch
analyzeCodeArrayAccessSizes (While cond body) = do
  analyzeExpArrayAccessSizes cond
  analyzeCodeArrayAccessSizes body
analyzeCodeArrayAccessSizes (For _ _ bound body) = do
  analyzeExpArrayAccessSizes bound
  analyzeCodeArrayAccessSizes body
analyzeCodeArrayAccessSizes _ = return ()

compileKernel :: CallKernel -> CompilerM ()
compileKernel (Map kernel) = do
  let name = mapKernelThreadNum kernel
      uses = mapKernelUses kernel
      body = mapKernelBody kernel
      int_t = Scalar int32
  inside_id <- newId
  oob_id <- newId
  analyzeCodeArrayAccessSizes body
  mapM_ registerConstUse uses
  addDescriptors name $ getNonSpecConstUses uses
  insertEntryHead name
  insertVarInline name FunctionLocal int_t
  tid_var <- getVarInfo name
  compileEarlyDecls body
  readBuiltinTo GlobalInvocationId 0 name
  (ks_id, _) <- compileExp $ mapKernelSize kernel
  (tid_id, _) <- insertVarLoad tid_var
  (cond_id, _) <- insertReturnOp (Scalar Bool) $ opULessThanEqual ks_id tid_id
  appendCode $ opSelectionMerge oob_id
  appendCode $ opBranchConditional cond_id oob_id inside_id
  appendCode $ opLabel inside_id
  compileCode body
  appendCode $ opBranch oob_id
  appendCode $ opLabel oob_id
  insertEntryTail
compileKernel (AnyKernel kernel) = do
  let name = kernelName kernel
      uses = kernelUses kernel
      body = kernelBody kernel
  analyzeCodeArrayAccessSizes body
  mapM_ registerConstUse uses
  addDescriptors name $ getNonSpecConstUses uses
  mapM_ insertLocalMemory $ kernelLocalMemory kernel
  insertEntryHead name
  compileEarlyDecls body
  compileCode body
  insertEntryTail
compileKernel (MapTranspose t name e1 n1 e2 e3 e4 e5 e6 e7) = return () -- TODO: Fix

finalizedShader :: CompilerM [Word32]
finalizedShader = do
  glsl_ext     <- getGLSLExt
  entries      <- getEntryPointDeclarations
  exec_modes   <- getExecutionModeDeclarations
  decos        <- getDecorations
  res_consts   <- getReservedDeclarations
  builtin_vars <- getBuiltinVarDeclarations
  consts       <- getConstDeclarations
  spec_consts  <- getSpecConstDeclarations
  desc_vars    <- getDescVarDeclarations
  scalar_types <- getScalarTypeDeclarations
  nons_types   <- getNonScalarTypeDeclarations
  -- | ^ Seperate from scalar types as they may require consts before
  max_id       <- gets compCurrentMaxId
  code_body    <- gets compResult
  return $ concat [ genHeader max_id
                  , getCapabilities
                  , getExtensions
                  , glsl_ext
                  , entries
                  , exec_modes
                  , decos
                  , scalar_types
                  , consts
                  , spec_consts
                  , nons_types
                  , builtin_vars
                  , res_consts
                  , desc_vars
                  , code_body
                  ]

kernelToShader :: CallKernel -> SingleEntryShader
kernelToShader kernel = runCompilerM newCompilerState $ do
  compileKernel kernel
  shader        <- finalizedShader
  name          <- fst . head <$> gets compEntryPoints
  desc_set_size <- length . flip (M.!) name <$> gets compDescriptors
  return $ SEShader name desc_set_size shader