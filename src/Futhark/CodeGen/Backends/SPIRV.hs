module Futhark.CodeGen.Backends.SPIRV
  ( runCompilerM
  , CompilerM
  , CompilerState
  , newCompilerState
  , Descriptor
  , DescriptorSet
  , DescriptorSetMap
  , getResult
  , getEntryPoints
  , entryPointName
  , getDescriptorSets
  , compileKernel
  , finalizedProgram
  ) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Bits
import Data.Binary.IEEE754
import Data.Word
import Data.List

import Debug.Trace

import Futhark.CodeGen.Backends.SPIRV.Operations
import Futhark.CodeGen.ImpCode hiding (Scalar, Function)
import Futhark.CodeGen.ImpCode.Kernels hiding (Code, Scalar, Function)
import Futhark.Representation.AST.Attributes.Types

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
               | Array (Maybe Word32)
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
                  | WorkgroupSizeYSpec
                  | WorkgroupSizeZSpec
                  | WorkgroupSizeSpec
                  | LockstepWidthSpec
  deriving (Eq, Ord)

reservedList = [ WorkgroupSizeXSpec 
               , WorkgroupSizeYSpec
               , WorkgroupSizeZSpec
               , LockstepWidthSpec
               , WorkgroupSizeSpec
               ]

reservedSpecDecorCount = 4
-- | ^ Specified how many (from the start of the list) need specialization decor

reservedId :: ReservedSpec -> Word32
reservedId res = fromIntegral $ maybe 0 (1+) $ elemIndex res reservedList

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
  , compSpecConstRefs :: M.Map VName (Word32, KernelConstExp)
  }

newCompilerState :: CompilerState
newCompilerState = CompilerState { compCurrentMaxId = fromIntegral $ length reservedList
                                 , compVarRefs = M.empty
                                 , compTypeRefs = M.empty
                                 , compConstRefs = M.empty
                                 , compEntryPoints = []
                                 , compResult = []
                                 , compGLSLExtId = Nothing
                                 , compBuiltinRefs = M.empty
                                 , compDescriptors = M.empty
                                 , compSpecConstRefs = M.empty
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
ensureAllSubTypeIds (Array _)      = ensureTypeId $ Scalar int8
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

getCapabilities :: [Word32]
getCapabilities = concatMap opCapability [ cShaderCapability
                                         , cStorageBuffer8BitAccessCapability
                                         , cInt16Capability
                                         , cInt64Capability
                                         , cInt64AtomicsCapability
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
              Just gid -> opMemoryModel cGLSLMemoryModel ++
                            opExtInstImport (encodeString "GLSL.std.450") gid
                            
entryPointName :: VName -> String
entryPointName name = baseString name ++ "_" ++ show (baseTag name)

spaceToScope :: Space -> VarScope
spaceToScope (Space "global")   = Global
spaceToScope (Space "constant") = UniformConstant
spaceToScope (Space "private")  = Private
spaceToScope _                  = Local

builtinToBuiltinId :: Builtin -> Word32
builtinToBuiltinId GlobalInvocationId = cBuiltinGlobalInvocationId
builtinToBuiltinId WorkgroupId        = cBuiltinWorkgroupId
builtinToBuiltinId NumWorkgroups      = cBuiltinNumWorkgroups
builtinToBuiltinId LocalInvocationId  = cBuiltinLocalInvocationId

insertVarInline :: VName -> VarScope -> SPIRVType -> CompilerM Word32
insertVarInline name scope t = do
  let p_t = Pointer t scope -- Variables must be of pointer type
      storage = scopeToStorageClass scope
  (var_id, _) <- liftReturnOp p_t $ opVariable storage
  ensureTypeId p_t
  modify $ \s -> s { compVarRefs = M.insert name (var_id, t, Inline) $ compVarRefs s }
  return var_id

getUseName :: KernelUse -> VName
getUseName (ScalarUse name _)  = name
getUseName (MemoryUse name _)  = name
getUseName (ConstUse name _)   = name

getUseType :: KernelUse -> SPIRVType
getUseType (ScalarUse _ t)  = Scalar t
getUseType (MemoryUse _ _)  = Array Nothing
getUseType (ConstUse _ exp) = Scalar $ primExpType exp

getUseNameType :: KernelUse -> (VName, SPIRVType)
getUseNameType use = (getUseName use, getUseType use)

insertDescriptorAccess :: Word32 -> KernelUse -> CompilerM ()
insertDescriptorAccess id use = do
  let (name, t) = getUseNameType use
      p_t = Pointer t StorageBuffer
      s_t = Struct [t]
  zero_id <- getConstId $ IntValue $ Int32Value 0
  ensureTypeId p_t
  ensureTypeId s_t
  (var_id, _) <- liftReturnOp p_t $ opAccessChain id zero_id
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
      put $ s { compGLSLExtId = Just id }
      return id

appendCode :: [Word32] -> CompilerM ()
appendCode code = modify $ \s -> s { compResult = compResult s ++ code }

addEntryPoint :: VName -> Word32 -> CompilerM ()
addEntryPoint name entry_id = modify $ \s -> s { compEntryPoints = (name, entry_id) : compEntryPoints s }

addDescriptors :: VName -> [KernelUse] -> CompilerM ()
addDescriptors kernel uses = do
  id_uses <- mapM (\u -> newId >>= (\id -> return (id, u))) uses
  modify $ \s -> s { compDescriptors = M.insert kernel id_uses $ compDescriptors s }

addSpecConst :: VName -> SPIRVType -> KernelConstExp -> CompilerM ()
addSpecConst name t exp =  do
  s <- get 
  case (M.!?) (compSpecConstRefs s) name of
    Just (id, _) -> modify $ \s -> s { compVarRefs = M.insert name (id, t, Constant) $ compVarRefs s}
    Nothing      -> do
      var_id <- newId
      modify $ \s -> s { compVarRefs       = M.insert name (var_id, t, Constant) $ compVarRefs s,
                         compSpecConstRefs = M.insert name (var_id, exp) $ compSpecConstRefs s }

registerSpecConsts :: KernelUse -> CompilerM ()
registerSpecConsts (ConstUse name exp) = do
  let t = Scalar $ primExpType exp
  addSpecConst name t exp
registerSpecConsts _                   = return ()

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

liftReturnOp :: SPIRVType -> (Word32 -> Word32 -> [Word32]) -> CompilerM ExprInfo
liftReturnOp t f = do
  t_id <- getTypeId t
  ret_id <- newId
  appendCode $ f t_id ret_id
  return (ret_id, t)

glslReturnOp :: GLSLInstr -> [Word32] -> SPIRVType -> CompilerM ExprInfo
glslReturnOp instr ops t = do
  glsl_id <- getGLSLExtId
  liftReturnOp t $ opExtInst instr ops glsl_id

insertLabel :: Word32 -> CompilerM ()
insertLabel id = appendCode $ opLabel id

insertBranch :: Word32 -> CompilerM ()
insertBranch target_id = appendCode $ opBranch target_id

insertBranchConditional :: Word32 -> Word32 -> Word32 -> CompilerM ()
insertBranchConditional cond_id true_id false_id =
  appendCode $ opBranchConditional cond_id true_id false_id

insertLoopMerge :: Word32 -> Word32 -> CompilerM ()
insertLoopMerge merge_id continue_id = appendCode $ opLoopMerge merge_id continue_id

insertStore :: Word32 -> Word32 -> CompilerM ()
insertStore to_id from_id = appendCode $ opStore to_id from_id []

insertInternalArrayWrite :: VName -> VarScope -> PrimType -> Int -> PrimValue -> CompilerM ()
insertInternalArrayWrite dest scope r_pt i val = do
  let aligned_i = i * primByteSize r_pt
  val_id <- getConstId val
  i_id <- getConstId $ IntValue $ Int32Value $ fromIntegral aligned_i
  insertArrayWrite dest i_id val_id r_pt Nonvolatile -- TODO: Need to add scope

insertLoop :: CompilerM Word32 -> CompilerM () -> CompilerM ()
insertLoop check body = do
  start_id <- newId
  check_id <- newId
  body_id <- newId
  end_id <- newId
  insertBranch start_id
  insertLabel start_id
  insertLoopMerge end_id start_id
  insertBranch check_id
  insertLabel check_id
  cond_id <- check
  insertBranchConditional cond_id body_id end_id
  insertLabel body_id
  body
  insertBranch start_id
  insertLabel end_id

insertVarLoad :: VarInfo -> CompilerM ExprInfo
insertVarLoad (var_id, var_t, Constant) = return (var_id, var_t)
insertVarLoad (var_id, var_t, _) = liftReturnOp var_t $ opLoad var_id []

byteSizeToIntPrimType :: Int -> PrimType
byteSizeToIntPrimType 1 = int8
byteSizeToIntPrimType 2 = int16
byteSizeToIntPrimType 4 = int32
byteSizeToIntPrimType 8 = int64

insertShiftRightConstByte :: SPIRVType -> Word32 -> Int -> CompilerM Word32
insertShiftRightConstByte _ val_id 0            = return val_id
insertShiftRightConstByte val_t val_id shift_by = do
  sb_id <- getConstId $ IntValue $ Int32Value $ fromIntegral shift_by
  fst <$> liftReturnOp val_t (opShiftRightLogical val_id sb_id)

insertShiftLeftConstByte :: SPIRVType -> Word32 -> Int -> CompilerM Word32
insertShiftLeftConstByte _ val_id 0            = return val_id
insertShiftLeftConstByte val_t val_id shift_by = do
  sb_id <- getConstId $ IntValue $ Int32Value $ fromIntegral shift_by
  fst <$> liftReturnOp val_t (opShiftLeftLogical val_id sb_id)

insertIntConversion :: SPIRVType -> SPIRVType -> Word32 -> CompilerM Word32
insertIntConversion to_t from_t from_id
  | to_t == from_t = return from_id
  | otherwise      = fst <$> liftReturnOp to_t (opUConvert from_id)

insertIndexIncrConst :: Word32 -> Int -> CompilerM Word32
insertIndexIncrConst i_id 0 = return i_id
insertIndexIncrConst i_id v = do
  v_id <- getConstId $ IntValue $ Int32Value $ fromIntegral v
  fst <$> liftReturnOp (Scalar int32) (opIAdd i_id v_id)

insertArrayWriteByte :: Volatility -> Word32 -> Word32 -> Word32 -> CompilerM ()
insertArrayWriteByte vol arr_id i_id b_id = do
  let pb_t = Pointer (Scalar int8) StorageBuffer
      mem_a = getMemoryAccessType vol
  (chain_id, _) <- liftReturnOp pb_t $ opAccessChain arr_id i_id
  appendCode $ opStore chain_id b_id [mem_a]

insertArrayWrite :: VName -> Word32 -> Word32 -> PrimType -> Volatility -> CompilerM ()
insertArrayWrite arr i_id val_id val_pt vol = do
  let val_t = Scalar val_pt
      bytes = primByteSize val_pt
      b_t   = Scalar int8
      int_t = Scalar $ byteSizeToIntPrimType bytes
  (arr_id, _, _) <- getVarInfo arr
  c_id <- if b_t == val_t then return val_id
                          else fst <$> liftReturnOp int_t (opBitcast val_id)
  shf_ids <- mapM (insertShiftRightConstByte int_t c_id . (8*)) [0..bytes-1]
  vs_ids <- mapM (insertIntConversion b_t int_t) shf_ids
  is_ids <- mapM (insertIndexIncrConst i_id . fromIntegral) [0..bytes-1]
  zipWithM_ (insertArrayWriteByte vol arr_id) is_ids vs_ids

insertArrayReadByte :: Volatility -> Word32 -> Word32 -> CompilerM Word32
insertArrayReadByte vol arr_id i_id = do
  let b_t   = Scalar int8
      pb_t  = Pointer b_t StorageBuffer
      mem_a = getMemoryAccessType vol
  (chain_id, _) <- liftReturnOp pb_t $ opAccessChain arr_id i_id
  fst <$> liftReturnOp b_t (opLoad chain_id [mem_a])

insertArrayRead :: VName -> Word32 -> PrimType -> Volatility -> CompilerM ExprInfo
insertArrayRead arr i_id r_pt vol = do
  let r_t    = Scalar r_pt
      bytes  = primByteSize r_pt
      b_t    = Scalar int8
      int_pt = byteSizeToIntPrimType bytes
      int_t  = Scalar int_pt
  (arr_id, _, _) <- getVarInfo arr
  is_ids <- mapM (insertIndexIncrConst i_id . fromIntegral) [0..bytes-1]
  vs_ids <- mapM (insertArrayReadByte vol arr_id) is_ids
  vis_ids <- mapM (insertIntConversion int_t b_t) vs_ids
  let shf_is = reverse $ map (8*) [0..bytes-1]
  shf_ids <- zipWithM (insertShiftLeftConstByte int_t) vis_ids shf_is
  zero_id <- getConstId $ blankPrimValue int_pt
  iv_id <- foldM (\x y -> fst <$> liftReturnOp int_t (opIAdd x y)) zero_id shf_ids
  liftReturnOp r_t (opBitcast iv_id)

readBuiltinTo :: Builtin -> Int32 -> VName -> CompilerM ()
readBuiltinTo builtin i target = do
  let r_t   = Scalar int32
      vr_t  = Vector r_t 3
      pr_t  = Pointer r_t Input
      pvr_t = Pointer vr_t Input
  ensureTypeId pvr_t
  var_id <- getBuiltinId builtin
  i_id <- getConstId $ IntValue $ Int32Value i
  (chain_id, _) <- liftReturnOp pr_t $ opAccessChain var_id i_id
  (load_id, _) <- liftReturnOp r_t $ opLoad chain_id []
  (target_id, _, _) <- getVarInfo target
  appendCode $ opStore target_id load_id []

readWorkgroupSizeTo :: Word32 -> VName -> CompilerM ()
readWorkgroupSizeTo i target = do
  let r_t   = Scalar int32
      ws_id = reservedId WorkgroupSizeSpec
  (v_id, _) <- liftReturnOp r_t $ opCompositeExtract ws_id i
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

getSPIRVTypeDeclaration :: SPIRVType -> Word32 -> CompilerM [Word32]
getSPIRVTypeDeclaration Void id                = return $ opTypeVoid id
getSPIRVTypeDeclaration (Scalar t) id          = return $ getPrimTypeDeclaration t id
getSPIRVTypeDeclaration (Vector t len) id      = do
  t_id <- getTypeId t
  return $ opTypeVector t_id len id
getSPIRVTypeDeclaration (Pointer t scope) id   = do
  let storage = scopeToStorageClass scope
  t_id <- getTypeId t
  return $ opTypePointer storage t_id id
getSPIRVTypeDeclaration (Function t) id        = do
  t_id <- getTypeId t
  return $ opTypeFunction t_id id
getSPIRVTypeDeclaration (Struct ts) id         = do
  t_ids <- mapM getTypeId ts
  return $ opTypeStruct t_ids id
getSPIRVTypeDeclaration (Array (Just size)) id = do
  t_id <- getTypeId $ Scalar int8
  return $ opTypeArray t_id size id
getSPIRVTypeDeclaration (Array Nothing) id     = do
  t_id <- getTypeId $ Scalar int8
  return $ opTypeRuntimeArray t_id id

getSPIRVTypeDeclarations :: CompilerM [Word32]
getSPIRVTypeDeclarations = do
  type_map <- gets compTypeRefs
  types <- sequence $ M.elems $ M.mapWithKey getSPIRVTypeDeclaration type_map
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
  let t = getUseType use
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

getDefaultSpecConst :: SPIRVType -> Word32 -> CompilerM [Word32]
getDefaultSpecConst t@(Scalar Bool) id = do
  t_id <- getTypeId t
  return $ opSpecConstantFalse t_id id
getDefaultSpecConst t@(Scalar pt) id = do
  let num_lits = primBitSize pt `div` 32
      default_lit = replicate num_lits 0
  t_id <- getTypeId t
  return $ opSpecConstant default_lit t_id id
getDefaultSpecConst _ _ = return []

getSpecConstDeclaration :: VName -> (Word32, KernelConstExp) -> CompilerM [Word32]
getSpecConstDeclaration name (id, _) = do
  (_, var_t, _) <- getVarInfo name
  getDefaultSpecConst var_t id

getSpecConstDeclarations :: CompilerM [Word32]
getSpecConstDeclarations = do
  spec_const_map <- gets compSpecConstRefs
  sconsts <- sequence $ M.elems $ M.mapWithKey getSpecConstDeclaration spec_const_map
  return $ concat sconsts

getReservedSpecDeclarations :: CompilerM [Word32]
getReservedSpecDeclarations = do
  let ws_x_id = reservedId WorkgroupSizeXSpec
      ws_y_id = reservedId WorkgroupSizeYSpec
      ws_z_id = reservedId WorkgroupSizeZSpec
      ws_id   = reservedId WorkgroupSizeSpec
      lsw_id  = reservedId LockstepWidthSpec
  int_id  <- getTypeId $ Scalar int32
  vec3_id <- getTypeId $ Vector (Scalar int32) 3
  return $ concat [ opSpecConstant [256] int_id ws_x_id
                  , opSpecConstant [1] int_id ws_y_id
                  , opSpecConstant [1] int_id ws_z_id
                  , opSpecConstant [1] int_id lsw_id
                  , opSpecConstantComposite [ws_x_id, ws_y_id, ws_z_id] vec3_id ws_id
                  ]

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
getTypeDecoration (Array _) id  = opDecorate id cDecorationArrayStride [1]
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

getSpecConstDecoration :: Word32 -> [Word32]
getSpecConstDecoration id = opDecorate id cDecorationSpecId [id]

getSpecConstDecorations :: CompilerM [Word32]
getSpecConstDecorations =
  concatMap (getSpecConstDecoration . fst) . M.elems <$> gets compSpecConstRefs

getReservedSpecDecorations :: CompilerM [Word32]
getReservedSpecDecorations =
  let idRange   = map fromIntegral [1..reservedSpecDecorCount]
      ws_id     = reservedId WorkgroupSizeSpec
      ws_deco   = opDecorate ws_id cDecorationBuiltin [cBuiltinWorkgroupSize]
      res_decos = concatMap (\id -> opDecorate id cDecorationSpecId [id]) idRange
  in return $ ws_deco ++ res_decos

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
  spec_deco     <- getSpecConstDecorations
  reserved_deco <- getReservedSpecDecorations
  type_deco     <- getTypeDecorations
  builtin_deco  <- getBuiltinDecorations
  return $ concat [type_deco, builtin_deco, spec_deco, reserved_deco, desc_deco]

compileLeaf :: ExpLeaf -> CompilerM ExprInfo
compileLeaf (SizeOf t) = do
  s_id <- getConstId $ IntValue $ Int32Value $ primByteSize t
  return (s_id, Scalar int32)
compileLeaf (ScalarVar src) = getVarInfo src >>= insertVarLoad 
compileLeaf (Index src (Count iexp) restype _ vol) = do
  let mem_access = getMemoryAccessType vol
  (i_id, _) <- compileExp iexp
  insertArrayRead src i_id restype vol

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
    Complement{} -> liftReturnOp t $ opNot x_id
    Not{}        -> liftReturnOp t $ opNot x_id
    Abs{}        -> glslReturnOp glslSAbs [x_id] t
    FAbs{}       -> glslReturnOp glslFAbs [x_id] t
    SSignum{}    -> glslReturnOp glslSSign [x_id] t
    USignum{}    -> glslReturnOp glslFSign [x_id] t
compilePrimExp f (CmpOpExp cmp x y) = do
  (x_id, _) <- compilePrimExp f x
  (y_id, _) <- compilePrimExp f y
  case cmp of
    CmpEq{}  -> case cmpOpType cmp of
                  FloatType _ -> liftReturnOp (Scalar Bool) $ opFOrdEqual x_id y_id
                  IntType _   -> liftReturnOp (Scalar Bool) $ opIEqual x_id y_id
                  Bool        -> liftReturnOp (Scalar Bool) $ opLogicalEqual x_id y_id
                  _           -> fail "Equality of certs not supported."
    FCmpLt{} -> liftReturnOp (Scalar Bool) $ opFOrdLessThan x_id y_id
    FCmpLe{} -> liftReturnOp (Scalar Bool) $ opFOrdLessThanEqual x_id y_id
    CmpLlt{} -> return (0, Scalar Bool) -- TODO: Fix
    CmpLle{} -> return (0, Scalar Bool) -- TODO: Fix
    CmpSle{} -> liftReturnOp (Scalar Bool) $ opSLessThanEqual x_id y_id
    CmpSlt{} -> liftReturnOp (Scalar Bool) $ opSLessThan x_id y_id
    CmpUle{} -> liftReturnOp (Scalar Bool) $ opULessThanEqual x_id y_id
    CmpUlt{} -> liftReturnOp (Scalar Bool) $ opULessThan x_id y_id
compilePrimExp f (ConvOpExp conv x) = do
  (x_id, _) <- compilePrimExp f x
  let (_, r_it) = convOpType conv
      r_t       = Scalar r_it
  case conv of
    ZExt{}   -> liftReturnOp r_t $ opUConvert x_id
    SExt{}   -> liftReturnOp r_t $ opSConvert x_id
    FPConv{} -> liftReturnOp r_t $ opFConvert x_id
    FPToUI{} -> liftReturnOp r_t $ opConvertFToU x_id
    FPToSI{} -> liftReturnOp r_t $ opConvertFToS x_id
    UIToFP{} -> liftReturnOp r_t $ opConvertUToF x_id
    SIToFP{} -> liftReturnOp r_t $ opConvertSToF x_id
compilePrimExp f (BinOpExp bop x y) = do
  (x_id, _) <- compilePrimExp f x
  (y_id, _) <- compilePrimExp f y
  let t = Scalar $ binOpType bop
  case bop of
    Add{}    -> liftReturnOp t $ opIAdd x_id y_id
    FAdd{}   -> liftReturnOp t $ opFAdd x_id y_id
    Sub{}    -> liftReturnOp t $ opISub x_id y_id
    FSub{}   -> liftReturnOp t $ opFSub x_id y_id
    Mul{}    -> liftReturnOp t $ opIMul x_id y_id
    FMul{}   -> liftReturnOp t $ opFMul x_id y_id
    FDiv{}   -> liftReturnOp t $ opFDiv x_id y_id
    UDiv{}   -> liftReturnOp t $ opUDiv x_id y_id
    SDiv{}   -> liftReturnOp t $ opSDiv x_id y_id
    UMod{}   -> liftReturnOp t $ opUMod x_id y_id
    SMod{}   -> liftReturnOp t $ opSMod x_id y_id
    SQuot{}  -> return (0, Scalar Bool) -- TODO: Fix
    SRem{}   -> liftReturnOp t $ opSRem x_id y_id
    FMin{}   -> glslReturnOp glslFMin [x_id, y_id] t
    SMin{}   -> glslReturnOp glslSMin [x_id, y_id] t
    UMin{}   -> glslReturnOp glslUMin [x_id, y_id] t
    FMax{}   -> glslReturnOp glslFMax [x_id, y_id] t
    UMax{}   -> glslReturnOp glslUMax [x_id, y_id] t
    SMax{}   -> glslReturnOp glslSMax [x_id, y_id] t
    Pow{}    -> return (0, Scalar Bool) -- TODO: Fix
    FPow{}   -> glslReturnOp glslPow [x_id, y_id] t
    Xor{}    -> liftReturnOp t $ opBitwiseXor x_id y_id
    And{}    -> liftReturnOp t $ opBitwiseAnd x_id y_id
    Or{}     -> liftReturnOp t $ opBitwiseOr x_id y_id
    Shl{}    -> liftReturnOp t $ opShiftLeftLogical x_id y_id
    LShr{}   -> liftReturnOp t $ opShiftRightLogical x_id y_id
    AShr{}   -> liftReturnOp t $ opShiftRightArithmetic x_id y_id
    LogAnd{} -> liftReturnOp t $ opLogicalAnd x_id y_id
    LogOr{}  -> liftReturnOp t $ opLogicalOr x_id y_id
compilePrimExp f (FunExp h args _) = return (0, Scalar Bool) -- TODO: Fix

compileAtomicOp :: AtomicOp -> CompilerM ()
compileAtomicOp (AtomicAdd store rname (Count c) e) = return () -- TODO: Fix
compileAtomicOp (AtomicSMax store rname (Count c) e) = return () -- TODO: Fix
compileAtomicOp (AtomicSMin store rname (Count c) e) = return () -- TODO: Fix
compileAtomicOp (AtomicUMax store rname (Count c) e) = return () -- TODO: Fix
compileAtomicOp (AtomicUMin store rname (Count c) e) = return () -- TODO: Fix
compileAtomicOp (AtomicAnd store rname (Count c) e) = return () -- TODO: Fix
compileAtomicOp (AtomicOr store rname (Count c) e) = return () -- TODO: Fix
compileAtomicOp (AtomicXor store rname (Count c) e) = return () -- TODO: Fix
compileAtomicOp (AtomicCmpXchg store rname (Count c) le re) = return () -- TODO: Fix
compileAtomicOp (AtomicXchg store rname (Count c) e) = return () -- TODO: Fix

compileKernelOp :: KernelOp -> CompilerM ()
compileKernelOp (GetGroupId name i) = readBuiltinTo WorkgroupId (fromIntegral i) name
compileKernelOp (GetLocalId name i) = readBuiltinTo LocalInvocationId (fromIntegral i) name
compileKernelOp (GetGlobalSize name i) = readBuiltinTo NumWorkgroups (fromIntegral i) name
compileKernelOp (GetGlobalId name i) = readBuiltinTo GlobalInvocationId (fromIntegral i) name
compileKernelOp (GetLocalSize name i) = readWorkgroupSizeTo (fromIntegral i) name
compileKernelOp (GetLockstepWidth name) = do
  (var_id, _, _) <- getVarInfo name
  insertStore var_id $ reservedId LockstepWidthSpec
compileKernelOp (Atomic op) = compileAtomicOp op
compileKernelOp Barrier = appendCode $ opControlBarrier cScopeWorkgroup cScopeWorkgroup $
  cMemorySemanticsAcquireRelease .|. cMemorySemanticsSequentiallyConsistent
compileKernelOp MemFence = appendCode $ opMemoryBarrier cScopeWorkgroup $
  cMemorySemanticsAcquireRelease .|. cMemorySemanticsSequentiallyConsistent

compileCode :: Code KernelOp -> CompilerM ()
compileCode Skip = return ()
compileCode (Op op) = compileKernelOp op
compileCode (lc :>>: rc) =  compileCode lc >> compileCode rc
compileCode (Comment _ c) = compileCode c
-- ^ SPIR-V does not support comments
compileCode (DeclareScalar name t) = void $ insertVarInline name FunctionLocal (Scalar t)
compileCode (SetScalar dest src) = do
  (var_id, _, _) <- getVarInfo dest
  (src_id, _) <- compileExp src
  insertStore var_id src_id
compileCode (DeclareArray name space t vs) = do
  let len     = length vs
      scope   = spaceToScope space
  void $ insertVarInline name scope $ Array $ Just $ fromIntegral len
  zipWithM_ (insertInternalArrayWrite name scope t) [0..len-1] vs
compileCode (DeclareMem name space) = return () -- TODO: Fix
compileCode (SetMem dest src space) = return () -- TODO: Fix
compileCode (Copy dest (Count destoffset) destspace src (Count srcoffset) space (Count size)) = return () -- TODO: Fix
compileCode (Write dest (Count idx) elemtype _ vol elemexp) = do
  (elem_id, _) <- compileExp elemexp
  (i_id, _) <- compileExp idx
  insertArrayWrite dest i_id elem_id elemtype vol
compileCode (Call dests fname args) = return () -- TODO: Fix
compileCode (Assert e (ErrorMsg parts) (loc, locs)) = return () -- TODO: Fix
compileCode DebugPrint{} = return ()
compileCode (If cond tbranch fbranch) = do
  true_id <- newId
  false_id <- newId
  end_id <- newId
  (cond_id, _) <- compileExp cond
  appendCode $ opSelectionMerge end_id
  insertBranchConditional cond_id true_id false_id
  insertLabel true_id
  compileCode tbranch
  insertBranch end_id
  insertLabel false_id
  compileCode fbranch
  insertBranch end_id
  insertLabel end_id
compileCode (While cond body) = insertLoop (fst <$> compileExp cond) $ compileCode body
compileCode (For i it bound body) = do
  i_id <- insertVarInline i FunctionLocal sit
  init_id <- getConstId $ IntValue $ intValue it 0
  appendCode $ opStore i_id init_id []
  insertLoop (check i_id) $ compileCode body
  where sit = Scalar $ IntType it
        check i_id = do
          (condi_id, _) <- liftReturnOp sit $ opLoad i_id []
          (bound_id, _) <- compileExp bound
          (check_id, _) <- liftReturnOp (Scalar Bool) $ opULessThan condi_id bound_id
          return check_id
compileCode _ = fail "Expression not supported in kernel."

compileKernel :: CallKernel -> CompilerM ()
compileKernel (Map kernel) = do
  let name = mapKernelThreadNum kernel
      uses = mapKernelUses kernel
      body = mapKernelBody kernel
  mapM_ registerSpecConsts uses
  addDescriptors name $ getNonSpecConstUses uses
  void_func_t <- getTypeId $ Function Void
  (entry_id, _) <- liftReturnOp Void $ opFunction cFunctionControlNone void_func_t
  desc_map <- gets compDescriptors
  newId >>= insertLabel
  mapM_ (uncurry insertDescriptorAccess) $ (M.!) desc_map name
  compileCode body
  appendCode opReturn
  appendCode opFunctionEnd
  addEntryPoint name entry_id
  clearVars
compileKernel (AnyKernel kernel) = do
  let name = kernelName kernel
      uses = kernelUses kernel
      body = kernelBody kernel
  mapM_ registerSpecConsts uses
  addDescriptors name $ getNonSpecConstUses uses
  void_func_t <- getTypeId $ Function Void
  (entry_id, _) <- liftReturnOp Void $ opFunction cFunctionControlNone void_func_t
  desc_map <- gets compDescriptors
  newId >>= insertLabel
  mapM_ (uncurry insertDescriptorAccess) $ (M.!) desc_map name
  compileCode body
  appendCode opReturn
  appendCode opFunctionEnd
  addEntryPoint name entry_id
  clearVars
compileKernel (MapTranspose t name e1 n1 e2 e3 e4 e5 e6 e7) = return () -- TODO: Fix

finalizedProgram :: CompilerM [Word32]
finalizedProgram = do
  glsl_ext     <- getGLSLExt
  entries      <- getEntryPointDeclarations
  exec_modes   <- getExecutionModeDeclarations
  decos        <- getDecorations
  res_consts   <- getReservedSpecDeclarations
  builtin_vars <- getBuiltinVarDeclarations
  consts       <- getConstDeclarations
  spec_consts  <- getSpecConstDeclarations
  desc_vars    <- getDescVarDeclarations
  types        <- getSPIRVTypeDeclarations
  max_id       <- gets compCurrentMaxId
  code_body    <- gets compResult
  return $ concat [ genHeader max_id
                  , getCapabilities
                  , getExtensions
                  , glsl_ext
                  , entries
                  , exec_modes
                  , decos
                  , types
                  , builtin_vars
                  , consts
                  , spec_consts
                  , res_consts
                  , desc_vars
                  , code_body
                  ]
