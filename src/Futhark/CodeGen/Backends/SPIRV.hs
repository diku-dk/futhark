{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Futhark.CodeGen.Backends.SPIRV
  ( EntryPointName
  , transposeEntryPointName
  , SingleEntryShader(..)
  , ReservedSpec (..)
  , reservedSpecList
  , runCompilerM
  , CompilerM
  , CompilerState
  , newCompilerState
  , getResult
  , getEntryPoints
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

import Futhark.CodeGen.Backends.SPIRV.Operations
import Futhark.CodeGen.ImpCode hiding (Scalar, Function)
import Futhark.CodeGen.ImpCode.Kernels hiding (Code, Scalar, Function)
import Futhark.Representation.AST.Attributes.Types
import Futhark.MonadFreshNames

type EntryPointName = String

transposeEntryPointName :: PrimType -> EntryPointName
transposeEntryPointName pt = "vulkan_transpose_" ++ pretty pt

data SingleEntryShader = SEShader {
    shaderEntryPoint :: EntryPointName
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
type DescriptorSetMap = M.Map EntryPointName DescriptorSet

data ReservedSpec = WorkgroupSizeXSpec
                  | LockstepWidthSpec
  deriving (Eq, Ord)

reservedSpecList :: [ReservedSpec]
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

reservedList :: [Reserved]
reservedList = [ WorkgroupSize ]

reservedId :: Reserved -> Word32
reservedId res =
  fromIntegral $ length reservedSpecList + maybe 0 (1+) (elemIndex res reservedList)

totalReservedIdCount :: Word32
totalReservedIdCount = fromIntegral $ length reservedSpecList + length reservedList

data CompilerState = CompilerState {
    compCurrentMaxId :: Word32
  , compNameSrc :: VNameSource
  -- | ^ Used only by transpose kernels to access corresponding descriptors
  , compVarRefs :: M.Map VName VarInfo
  -- | ^ Type is the variable type without the required pointer wrapper
  , compTypeRefs :: M.Map SPIRVType Word32
  , compConstRefs :: M.Map PrimValue Word32
  -- | ^ Constants used inside the SPIR-V program
  , compEntryPoints :: [(EntryPointName, Word32)]
  , compResult :: [Word32]
  , compGLSLExtId :: Maybe Word32
  , compBuiltinRefs :: M.Map Builtin Word32
  , compDescriptors :: DescriptorSetMap
  , compSpecConstRefs :: [(Word32, SPIRVType)]
  , compArrayLeastAccessSize :: M.Map VName Int
  }

newCompilerState :: CompilerState
newCompilerState = CompilerState { compCurrentMaxId = totalReservedIdCount
                                 , compNameSrc = blankNameSource
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

instance MonadFreshNames CompilerM where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

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
            Just _  -> M.adjust (min size) vn $ compArrayLeastAccessSize s
            Nothing -> M.insert vn size $ compArrayLeastAccessSize s
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
getUseName (ScalarUse name _) = name
getUseName (MemoryUse name _) = name
getUseName (ConstUse name _)  = name

getUseType :: KernelUse -> CompilerM SPIRVType
getUseType (ScalarUse _ t)    = return $ Scalar t
getUseType (MemoryUse name _) = do
  access_size_map <- gets compArrayLeastAccessSize
  let elem_size = (M.!) access_size_map name
      t         = Scalar $ byteSizeToIntPrimType elem_size
  return $ Array t Nothing
getUseType (ConstUse _ e)     = return $ Scalar $ primExpType e

insertDescriptorAccess :: Word32 -> KernelUse -> CompilerM ()
insertDescriptorAccess desc_id use = do
  t <- getUseType use
  let p_t = Pointer t StorageBuffer
      s_t = Struct [t]
  zero_id <- getConstId $ IntValue $ Int32Value 0
  ensureTypeId p_t
  ensureTypeId s_t
  (var_id, _) <- insertReturnOp p_t $ opAccessChain desc_id zero_id
  modify $ \s -> s { compVarRefs = M.insert (getUseName use) (var_id, t, InterOp) $ compVarRefs s }

getResult :: CompilerM [Word32]
getResult = gets compResult

getEntryPoints :: CompilerM [String]
getEntryPoints = map fst <$> gets compEntryPoints

getDescriptorSets :: CompilerM (M.Map String DescriptorSet)
getDescriptorSets = gets compDescriptors

getGLSLExtId :: CompilerM Word32
getGLSLExtId = do
  s <- get
  case compGLSLExtId s of
    Just ext_id -> return ext_id
    Nothing -> do
      ext_id <- newId
      modify $ \s_n -> s_n { compGLSLExtId = Just ext_id }
      return ext_id

appendCode :: [Word32] -> CompilerM ()
appendCode code = modify $ \s -> s { compResult = compResult s ++ code }

addEntryPoint :: EntryPointName -> Word32 -> CompilerM ()
addEntryPoint name entry_id = modify $ \s -> s { compEntryPoints = (name, entry_id) : compEntryPoints s }

addDescriptors :: EntryPointName -> [KernelUse] -> CompilerM ()
addDescriptors kernel uses = do
  id_uses <- mapM (\u -> newId >>= (\u_id -> return (u_id, u))) uses
  modify $ \s -> s { compDescriptors = M.insert kernel id_uses $ compDescriptors s }

registerConstUse :: KernelUse -> CompilerM ()
registerConstUse (ConstUse name e) = do
  let t = Scalar $ primExpType e
  var_id <- newId
  modify $ \s -> s { compVarRefs       = M.insert name (var_id, t, Constant) $ compVarRefs s,
                     compSpecConstRefs = compSpecConstRefs s ++ [(var_id, t)] }
registerConstUse _                   = return ()

insertLocalMemory :: LocalMemoryUse -> CompilerM ()
insertLocalMemory (name, _) = do
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

getDescriptorUses :: [KernelUse] -> [KernelUse]
getDescriptorUses []                  = []
getDescriptorUses (ConstUse _ _ : ks) = getDescriptorUses ks
getDescriptorUses (use : ks)          = use : getDescriptorUses ks

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

insertEntryHead :: EntryPointName -> CompilerM ()
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
  (is_zero_id, _) <- insertCompareEqual from_pt zero_id from_id
  insertReturnOp (Scalar Bool) $ opLogicalNot is_zero_id
insertBitcast to_pt@(IntType to_it) Bool from_id = do
  zero_id <- getConstId $ IntValue $ intValue to_it (0 :: Integer)
  one_id  <- getConstId $ IntValue $ intValue to_it (1 :: Integer)
  insertReturnOp (Scalar to_pt) $ opSelect from_id one_id zero_id
insertBitcast to_pt@(FloatType to_ft) Bool from_id = do
  zero_id <- getConstId $ FloatValue $ floatValue to_ft (0.0 :: Double)
  one_id  <- getConstId $ FloatValue $ floatValue to_ft (1.0 :: Double)
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

readBuiltin :: Builtin -> Int32 -> CompilerM ExprInfo
readBuiltin builtin i = do
  let r_t   = Scalar int32
      vr_t  = Vector r_t 3
      pr_t  = Pointer r_t Input
      pvr_t = Pointer vr_t Input
  ensureTypeId pvr_t
  var_id <- getBuiltinId builtin
  i_id <- getConstId $ IntValue $ Int32Value i
  (chain_id, _) <- insertReturnOp pr_t $ opAccessChain var_id i_id
  insertReturnOp r_t $ opLoad chain_id []

readBuiltinTo :: Builtin -> Int32 -> VName -> CompilerM ()
readBuiltinTo builtin i target = do
  (builtin_id, _) <- readBuiltin builtin i
  (target_id, _, _) <- getVarInfo target
  insertStore target_id builtin_id

readWorkgroupSizeTo :: Word32 -> VName -> CompilerM ()
readWorkgroupSizeTo i target = do
  let r_t   = Scalar int32
      ws_id = reservedId WorkgroupSize
  (v_id, _) <- insertReturnOp r_t $ opCompositeExtract ws_id i
  (target_id, _, _) <- getVarInfo target
  insertStore target_id v_id

getMemoryAccessType :: Volatility -> Word32
getMemoryAccessType Volatile = cMemoryAccessVolatile
getMemoryAccessType Nonvolatile = cMemoryAccessNone

getPrimTypeDeclaration :: PrimType -> Word32 -> [Word32]
getPrimTypeDeclaration t@(IntType _) t_id   = opTypeInt (fromIntegral (primBitSize t)) cNoSignedness t_id
getPrimTypeDeclaration t@(FloatType _) t_id = opTypeFloat (fromIntegral (primBitSize t)) t_id
getPrimTypeDeclaration Bool t_id            = opTypeBool t_id
getPrimTypeDeclaration Cert _               = []

getScalarTypeDeclaration :: SPIRVType -> Word32 -> CompilerM [Word32]
getScalarTypeDeclaration (Scalar t) t_id = return $ getPrimTypeDeclaration t t_id
getScalarTypeDeclaration _ _             = return []

getScalarTypeDeclarations :: CompilerM [Word32]
getScalarTypeDeclarations = do
  type_map <- gets compTypeRefs
  types <- sequence $ M.elems $ M.mapWithKey getScalarTypeDeclaration type_map
  return $ concat types

getNonScalarTypeDeclaration :: SPIRVType -> Word32 -> CompilerM [Word32]
getNonScalarTypeDeclaration (Scalar _) _           = return []
getNonScalarTypeDeclaration Void d_id              = return $ opTypeVoid d_id
getNonScalarTypeDeclaration (Vector t len) d_id    = do
  t_id <- getTypeId t
  return $ opTypeVector t_id len d_id
getNonScalarTypeDeclaration (Pointer t scope) d_id = do
  let storage = scopeToStorageClass scope
  t_id <- getTypeId t
  return $ opTypePointer storage t_id d_id
getNonScalarTypeDeclaration (Function t) d_id      = do
  t_id <- getTypeId t
  return $ opTypeFunction t_id d_id
getNonScalarTypeDeclaration (Struct ts) d_id       = do
  t_ids <- mapM getTypeId ts
  return $ opTypeStruct t_ids d_id
getNonScalarTypeDeclaration (Array t s) d_id       = do
  t_id <- getTypeId t
  return $ maybe (opTypeRuntimeArray t_id d_id) (\size -> opTypeArray t_id size d_id) s

getNonScalarTypeDeclarations :: CompilerM [Word32]
getNonScalarTypeDeclarations = do
  type_map <- gets compTypeRefs
  types <- sequence $ M.elems $ M.mapWithKey getNonScalarTypeDeclaration type_map
  return $ concat types

getBuiltinVarDeclaration :: Builtin -> Word32 -> CompilerM [Word32]
getBuiltinVarDeclaration _ builtin_id = do
  let t   = Scalar int32
      vt  = Vector t 3
      pvt = Pointer vt Input
  t_id <- getTypeId pvt
  return $ opVariable cStorageClassInput t_id builtin_id

getBuiltinVarDeclarations :: CompilerM [Word32]
getBuiltinVarDeclarations = do
  builtin_map <- gets compBuiltinRefs
  builtin <- sequence $ M.elems $ M.mapWithKey getBuiltinVarDeclaration builtin_map
  return $ concat builtin

getDescVarDeclaration :: Word32 -> KernelUse -> CompilerM [Word32]
getDescVarDeclaration desc_id use = do
  t <- getUseType use
  pt_id <- getTypeId $ Pointer (Struct [t]) StorageBuffer
  return $ opVariable cStorageClassStorageBuffer pt_id desc_id

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
getConstDeclaration (BoolValue b) c_id = do
  let op = if b then opConstantTrue else opConstantFalse
  t_id <- getTypeId $ Scalar Bool
  return $ op t_id c_id
getConstDeclaration (IntValue i) c_id = do
  let lit = intValueToWords i
  t_id <- getTypeId $ Scalar $ IntType $ intValueType i
  return $ opConstant lit t_id c_id
getConstDeclaration (FloatValue f) c_id = do
  let lit = floatValueToWords f
  t_id <- getTypeId $ Scalar $ FloatType $ floatValueType f
  return $ opConstant lit t_id c_id
getConstDeclaration Checked _ = return []

getConstDeclarations :: CompilerM [Word32]
getConstDeclarations = do
  const_map <- gets compConstRefs
  consts <- sequence $ M.elems $ M.mapWithKey getConstDeclaration const_map
  return $ concat consts

getSpecConstDeclaration :: (Word32, SPIRVType) -> CompilerM [Word32]
getSpecConstDeclaration (spec_id, t@(Scalar Bool)) = do
  t_id <- getTypeId t
  return $ opSpecConstantFalse t_id spec_id
getSpecConstDeclaration (spec_id, t@(Scalar pt)) = do
  let num_lits = primBitSize pt `div` 32
      default_lit = replicate num_lits 1
  t_id <- getTypeId t
  return $ opSpecConstant default_lit t_id spec_id
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

getEntryPointDeclaration :: EntryPointName -> Word32 -> [Word32] -> [Word32]
getEntryPointDeclaration name entry_id inputs =
  let name_string = encodeString name
  in opEntryPoint cExecutionModelGLCompute entry_id name_string inputs

getEntryPointDeclarations :: CompilerM [Word32]
getEntryPointDeclarations = do
  builtin_ids <- M.elems <$> gets compBuiltinRefs
  concatMap (\(n, e_id) -> getEntryPointDeclaration n e_id builtin_ids) <$> gets compEntryPoints

getExecutionModeDeclaration :: Word32 -> [Word32]
getExecutionModeDeclaration exec_id = opExecutionMode exec_id cExecutionModeLocalSize [1, 1, 1]

getExecutionModeDeclarations :: CompilerM [Word32]
getExecutionModeDeclarations =
  concatMap (getExecutionModeDeclaration . snd) <$> gets compEntryPoints

getTypeDecoration :: SPIRVType -> Word32 -> [Word32]
getTypeDecoration (Array _ _) t_id  = opDecorate t_id cDecorationArrayStride [1]
-- | ^ Always byte-index
getTypeDecoration (Struct _) t_id =
  opDecorate t_id cDecorationBlock [] ++
  opMemberDecorate t_id 0 cDecorationOffset [0]
getTypeDecoration _ _           = []

getTypeDecorations :: CompilerM [Word32]
getTypeDecorations =
  concat . M.elems . M.mapWithKey getTypeDecoration <$> gets compTypeRefs

getBuiltinDecoration :: Builtin -> Word32 -> [Word32]
getBuiltinDecoration builtin b_id = opDecorate b_id cDecorationBuiltin [bid]
  where bid = builtinToBuiltinId builtin

getBuiltinDecorations :: CompilerM [Word32]
getBuiltinDecorations =
  concat . M.elems . M.mapWithKey getBuiltinDecoration <$> gets compBuiltinRefs

getSpecConstDecoration :: Word32 -> Word32 -> [Word32]
getSpecConstDecoration spec_id const_id = opDecorate spec_id cDecorationSpecId [const_id]

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
getKernelDescriptorDecorations set binding (desc_id, _) =
  opDecorate desc_id cDecorationDescriptorSet [set] ++
  opDecorate desc_id cDecorationBinding [binding]

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

fun64To32 :: String -> [Word32] -> CompilerM ExprInfo
fun64To32 fun arg_ids = do
  args32 <- mapM (insertReturnOp (Scalar float32) . opFConvert) arg_ids
  (r32_id, _) <- compileFunCall (fun ++ "32") $ map fst args32
  insertReturnOp (Scalar float64) $ opFConvert r32_id

compileFunCall :: String -> [Word32] -> CompilerM ExprInfo
compileFunCall "isnan32" [a_id]          = insertReturnOp (Scalar float32) $ opIsNan a_id
compileFunCall "isinf32" [a_id]          = insertReturnOp (Scalar float32) $ opIsInf a_id
compileFunCall "round32" [a_id]          = glslReturnOp glslRound [a_id] (Scalar float32)
compileFunCall "log32" [a_id]            = glslReturnOp glslLog [a_id] (Scalar float32)
compileFunCall "log2_32" [a_id]          = glslReturnOp glslLog2 [a_id] (Scalar float32)
compileFunCall "exp32" [a_id]            = glslReturnOp glslExp [a_id] (Scalar float32)
compileFunCall "sqrt32" [a_id]           = glslReturnOp glslSqrt [a_id] (Scalar float32)
compileFunCall "sin32" [a_id]            = glslReturnOp glslSin [a_id] (Scalar float32)
compileFunCall "cos32" [a_id]            = glslReturnOp glslCos [a_id] (Scalar float32)
compileFunCall "tan32" [a_id]            = glslReturnOp glslTan [a_id] (Scalar float32)
compileFunCall "asin32" [a_id]           = glslReturnOp glslASin [a_id] (Scalar float32)
compileFunCall "acos32" [a_id]           = glslReturnOp glslACos [a_id] (Scalar float32)
compileFunCall "atan32" [a_id]           = glslReturnOp glslATan [a_id] (Scalar float32)
compileFunCall "atan2_32" [a1_id, a2_id] = glslReturnOp glslATan2 [a1_id, a2_id] (Scalar float32)
compileFunCall "pow32" [a1_id, a2_id]    = glslReturnOp glslPow [a1_id, a2_id] (Scalar float32)
compileFunCall "to_bits32" [a_id]        = insertBitcast int32 float32 a_id
compileFunCall "from_bits32" [a_id]      = insertBitcast float32 int32 a_id
compileFunCall "log10_32" [a_id]         = do
  base_id <- getConstId $ FloatValue $ Float32Value 10.0
  (numer_id, _) <- glslReturnOp glslLog [a_id] (Scalar float32)
  (denom_id, _) <- glslReturnOp glslLog [base_id] (Scalar float32)
  insertReturnOp (Scalar float32) $ opFDiv numer_id denom_id
compileFunCall "isnan64" [a_id]          = insertReturnOp (Scalar float64) $ opIsNan a_id
compileFunCall "isinf64" [a_id]          = insertReturnOp (Scalar float64) $ opIsInf a_id
compileFunCall "round64" [a_id]          = glslReturnOp glslRound [a_id] (Scalar float64)
compileFunCall "sqrt64" [a_id]           = glslReturnOp glslSqrt [a_id] (Scalar float64)
compileFunCall "to_bits64" [a_id]        = insertBitcast int64 float64 a_id
compileFunCall "from_bits64" [a_id]      = insertBitcast float64 int64 a_id
compileFunCall "pow64" arg_ids           = fun64To32 "pow" arg_ids
compileFunCall "log64" arg_ids           = fun64To32 "log" arg_ids
compileFunCall "log2_64" arg_ids         = fun64To32 "log2_" arg_ids
compileFunCall "exp64" arg_ids           = fun64To32 "exp" arg_ids
compileFunCall "sin64" arg_ids           = fun64To32 "sin" arg_ids
compileFunCall "cos64" arg_ids           = fun64To32 "cos" arg_ids
compileFunCall "tan64" arg_ids           = fun64To32 "tan" arg_ids
compileFunCall "asin64" arg_ids          = fun64To32 "asin" arg_ids
compileFunCall "acos64" arg_ids          = fun64To32 "acos" arg_ids
compileFunCall "atan64" arg_ids          = fun64To32 "atan" arg_ids
compileFunCall "atan2_64" arg_ids        = fun64To32 "atan2_" arg_ids
compileFunCall "log10_64" arg_ids        = fun64To32 "log10_" arg_ids
compileFunCall h _                       = fail $ "Call to " ++ pretty h ++ " not implemented."

compileLeaf :: ExpLeaf -> CompilerM ExprInfo
compileLeaf (SizeOf t) = do
  s_id <- getConstId $ IntValue $ Int32Value $ primByteSize t
  return (s_id, Scalar int32)
compileLeaf (ScalarVar src) = getVarInfo src >>= insertVarLoad 
compileLeaf (Index src (Count iexp) restype space vol) = do
  (_, _, src_dec) <- getVarInfo src
  let scope = decSpaceToScope src_dec space
  (i_id, _) <- compileExp iexp
  insertArrayRead src i_id restype scope vol

compileExp :: Exp -> CompilerM ExprInfo
compileExp = compilePrimExp compileLeaf

compilePrimExp :: (v -> CompilerM ExprInfo) -> PrimExp v -> CompilerM ExprInfo
compilePrimExp f (LeafExp v _) = f v
compilePrimExp _ (ValueExp val) = do
  c_id <- getConstId val
  let t = Scalar $ primValueType val
  return (c_id, t)
compilePrimExp f (UnOpExp uop x) = do
  (x_id, _) <- compilePrimExp f x
  let pt = unOpType uop
      t = Scalar pt
  case uop of
    Complement{} -> case pt of
                      Bool          -> insertReturnOp t $ opLogicalNot x_id
                      (IntType _)   -> insertReturnOp t $ opNot x_id
                      (FloatType _) -> do
                        let int_t = byteSizeToIntPrimType $ primByteSize pt
                        (i_id, _) <- insertBitcast int_t pt x_id
                        (c_id, _) <- insertReturnOp t $ opNot i_id
                        insertBitcast pt int_t c_id
                      Cert          -> fail "Cert complement is not supported" 
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
  let pt = binOpType bop
      t = Scalar pt
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
    FPow{}   -> compileFunCall ("pow" ++ show (primBitSize pt)) [x_id, y_id]
    Xor{}    -> insertReturnOp t $ opBitwiseXor x_id y_id
    And{}    -> insertReturnOp t $ opBitwiseAnd x_id y_id
    Or{}     -> insertReturnOp t $ opBitwiseOr x_id y_id
    Shl{}    -> insertReturnOp t $ opShiftLeftLogical x_id y_id
    LShr{}   -> insertReturnOp t $ opShiftRightLogical x_id y_id
    AShr{}   -> insertReturnOp t $ opShiftRightArithmetic x_id y_id
    LogAnd{} -> insertReturnOp t $ opLogicalAnd x_id y_id
    LogOr{}  -> insertReturnOp t $ opLogicalOr x_id y_id
compilePrimExp f (FunExp h args _) = do
  arg_ids <- mapM (compilePrimExp f) args
  compileFunCall h $ map fst arg_ids

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
compileCode (DeclareScalar _ _) = return ()
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
compileCode (Call [dest] fname args) = do
  (dest_id, _, _) <- getVarInfo dest
  arg_ids <- mapM compileArg args
  (fres_id, _) <- compileFunCall (pretty fname) $ map fst arg_ids
  insertStore dest_id fres_id
  where compileArg (MemArg vn) = getVarInfo vn >>= insertVarLoad
        compileArg (ExpArg e)  = compileExp e
compileCode Assert{} = return ()
compileCode DebugPrint{} = return ()
compileCode (If cond tbranch fbranch) = do
  (cond_id, _) <- compileExp cond
  insertIf cond_id (compileCode tbranch) (compileCode fbranch)
compileCode (While cond body) =
  insertLoop (fst <$> compileExp cond) (compileCode body) $ return ()
compileCode (For i it bound body) = do
  var_i@(i_id, _, _) <- getVarInfo i
  init_id <- getConstId $ IntValue $ intValue it (0::Integer)
  insertStore i_id init_id
  insertLoop (check i_id) (compileCode body) (continue var_i)
  where sit = Scalar $ IntType it
        check i_id = do
          (condi_id, _) <- insertReturnOp sit $ opLoad i_id []
          (bound_id, _) <- compileExp bound
          (check_id, _) <- insertReturnOp (Scalar Bool) $ opULessThan condi_id bound_id
          return check_id
        continue var_i@(i_id, _, _) = do
          one_id <- getConstId $ IntValue $ intValue it (1::Integer)
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
compileEarlyDecls (DeclareArray name space _ vs) = do
  access_size_map <- gets compArrayLeastAccessSize
  let elem_size = (M.!) access_size_map name
      t         = Scalar $ byteSizeToIntPrimType elem_size
      len       = length vs
      scope     = spaceToScope space
  void $ insertVarInline name scope $ Array t $ Just $ fromIntegral len
compileEarlyDecls (If _ tbranch fbranch) =
  compileEarlyDecls tbranch >> compileEarlyDecls fbranch
compileEarlyDecls (While _ body) = compileEarlyDecls body
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
  let g_var = mapKernelThreadNum kernel
      name = pretty g_var
      uses = mapKernelUses kernel
      body = mapKernelBody kernel
      int_t = Scalar int32
  inside_id <- newId
  oob_id <- newId
  analyzeCodeArrayAccessSizes body
  mapM_ registerConstUse uses
  addDescriptors name $ getDescriptorUses uses
  insertEntryHead name
  _ <- insertVarInline g_var FunctionLocal int_t
  compileEarlyDecls body
  (g_var_id, _, _) <- getVarInfo g_var
  (g_id, _) <- readBuiltin GlobalInvocationId 0
  insertStore g_var_id g_id
  (ks_id, _) <- compileExp $ mapKernelSize kernel
  (cond_id, _) <- insertReturnOp (Scalar Bool) $ opULessThanEqual ks_id g_id
  appendCode $ opSelectionMerge oob_id
  appendCode $ opBranchConditional cond_id oob_id inside_id
  appendCode $ opLabel inside_id
  compileCode body
  appendCode $ opBranch oob_id
  appendCode $ opLabel oob_id
  insertEntryTail
compileKernel (AnyKernel kernel) = do
  let name = pretty $ kernelName kernel
      uses = kernelUses kernel
      body = kernelBody kernel
  analyzeCodeArrayAccessSizes body
  mapM_ registerConstUse uses
  addDescriptors name $ getDescriptorUses uses
  mapM_ insertLocalMemory $ kernelLocalMemory kernel
  insertEntryHead name
  compileEarlyDecls body
  compileCode body
  insertEntryTail
compileKernel (MapTranspose bt _ _ _ _ _ _ _ _ _) = do
  let int32_t = Scalar int32
      name = transposeEntryPointName bt
  dest        <- newVName "dest"
  dest_offset <- newVName "dest_offset"
  src         <- newVName "src"
  src_offset  <- newVName "src_offset"
  x_elems     <- newVName "x_elems"
  y_elems     <- newVName "y_elems"
  in_elems    <- newVName "in_elems"
  let uses = [ MemoryUse dest $ ConstSize 0
             , ScalarUse dest_offset int32
             , MemoryUse src $ ConstSize 0
             , ScalarUse src_offset int32
             , ScalarUse x_elems int32
             , ScalarUse y_elems int32
             , ScalarUse in_elems int32
             ]
  suggestArrayLeastAccessSize dest $ primByteSize bt
  suggestArrayLeastAccessSize src $ primByteSize bt
  addDescriptors name uses
  insertEntryHead name
  (dest_offset_id, _) <- getVarInfo dest_offset >>= insertVarLoad
  (src_offset_id, _)  <- getVarInfo src_offset >>= insertVarLoad
  (x_elems_id, _)     <- getVarInfo x_elems >>= insertVarLoad
  (y_elems_id, _)     <- getVarInfo y_elems >>= insertVarLoad
  (in_elems_id, _)    <- getVarInfo in_elems >>= insertVarLoad
  inside_id           <- newId
  oob_id              <- newId
  (g_id, _) <- readBuiltin GlobalInvocationId 0
  (cond_id, _) <- insertReturnOp (Scalar Bool) $ opULessThanEqual in_elems_id g_id
  appendCode $ opSelectionMerge oob_id
  appendCode $ opBranchConditional cond_id oob_id inside_id
  appendCode $ opLabel inside_id
  bt_size_id <- getConstId $ IntValue $ Int32Value $ primByteSize bt
  (aligned_g_id, _) <- insertReturnOp int32_t $ opIMul g_id bt_size_id
  (i_id, _) <- insertReturnOp int32_t $ opIAdd aligned_g_id src_offset_id
  (val_id, _) <- insertArrayRead src i_id bt StorageBuffer Nonvolatile
  (x_id, _) <- insertReturnOp int32_t $ opSDiv g_id x_elems_id
  (y_id, _) <- insertReturnOp int32_t $ opSRem g_id x_elems_id
  (y_id', _) <- insertReturnOp int32_t $ opIMul y_id y_elems_id
  (tr_id, _) <- insertReturnOp int32_t $ opIAdd x_id y_id'
  (aligned_tr_id, _) <- insertReturnOp int32_t $ opIMul tr_id bt_size_id
  (i_t_id, _) <- insertReturnOp int32_t $ opIAdd aligned_tr_id dest_offset_id
  insertArrayWrite dest i_t_id val_id bt StorageBuffer Nonvolatile
  appendCode $ opBranch oob_id
  appendCode $ opLabel oob_id
  insertEntryTail

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