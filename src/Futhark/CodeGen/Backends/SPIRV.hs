module Futhark.CodeGen.Backends.SPIRV
  ( runCompilerM
  , CompilerM
  , CompilerState
  , newCompilerState
  , getResult
  , getEntryPoints
  , compileKernel
  , finalizedProgram
  ) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Bits
import Data.Binary.IEEE754
import Data.Word

import Futhark.CodeGen.Backends.SPIRV.Operations
import Futhark.CodeGen.ImpCode hiding (Scalar, Function)
import Futhark.CodeGen.ImpCode.Kernels hiding (Code, Scalar, Function)
import Futhark.Representation.AST.Attributes.Types

data VarScope = Internal
              | Uniform
              | Input
              -- | ^ Only used by builtin variables
  deriving (Eq, Ord)

data SPIRVType = Scalar PrimType
               | StaticArray SPIRVType Word32
               | Void
               | Memory VName
               | Function SPIRVType
               | Pointer SPIRVType VarScope
               | Vector PrimType Word32
               -- | ^ Only used by builtin variables
  deriving (Eq, Ord)

type ExprInfo = (Word32, SPIRVType)

data VarDec = Inline
            | InterOp
            | Constant
  deriving (Eq, Ord)

type VarInfo = (Word32, SPIRVType, VarDec)

data Builtin = GlobalInvocationId
             | WorkgroupId
             | WorkgroupSize
             | NumWorkgroups
             | LocalInvocationId
  deriving (Eq, Ord)

data CompilerState = CompilerState {
    compCurrentMaxId :: Word32
  , compVarRefs :: M.Map VName VarInfo
  -- | ^ Type is the variable type without the required pointer wrapper
  , compTypeRefs :: M.Map SPIRVType Word32
  , compConstRefs :: M.Map PrimValue Word32
  -- | ^ Constants used inside the SPIR-V program
  , compMemElemTypes :: M.Map VName PrimType
  -- | ^ Types of memory is determined by the first write
  , compEntryPoints :: [(String, Word32)]
  , compResult :: [Word32]
  , compGLSLExtId :: Maybe Word32
  , compBuiltinRefs :: M.Map Builtin Word32
  , compUses :: M.Map VName (Word32, KernelUse)
  }

newCompilerState :: CompilerState
newCompilerState = CompilerState { compCurrentMaxId = 0
                                 , compVarRefs = M.empty
                                 , compTypeRefs = M.empty
                                 , compConstRefs = M.empty
                                 , compMemElemTypes = M.empty
                                 , compEntryPoints = []
                                 , compResult = []
                                 , compGLSLExtId = Nothing
                                 , compBuiltinRefs = M.empty
                                 , compUses = M.empty
                                 }

type CompilerM = State CompilerState

runCompilerM :: CompilerState -> CompilerM a -> (a, CompilerState)
runCompilerM cs comp = runState comp cs

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
getTypeId t@(Memory name) = do
  s <- get
  getTypeId $ Scalar $ (M.!) (compMemElemTypes s) name
getTypeId t = do
  s <- get
  maybe (ensureAllSubTypeIds t >> insertType t) return $ (M.!?) (compTypeRefs s) t

setMemElemType :: VName -> PrimType -> CompilerM ()
setMemElemType name t = do
  s <- get
  put $ s { compMemElemTypes = M.insert name t $ compMemElemTypes s }

ensureTypeId :: SPIRVType -> CompilerM ()
ensureTypeId t = void $ getTypeId t

ensureAllSubTypeIds :: SPIRVType -> CompilerM ()
ensureAllSubTypeIds (Function st)      = ensureTypeId st
ensureAllSubTypeIds (StaticArray st _) = ensureTypeId st
ensureAllSubTypeIds (Pointer st _)     = ensureTypeId st
ensureAllSubTypeIds (Vector t _)       = ensureTypeId $ Scalar t
ensureAllSubTypeIds _                  = return ()

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

getVarInfo :: VName -> CompilerM VarInfo
getVarInfo v = do
  s <- get
  return $ (M.!) (compVarRefs s) v

addVarInline :: VName -> SPIRVType -> CompilerM Word32
addVarInline name t = do
  let p_t = Pointer t Internal -- Variables must be of pointer type
  (var_id, _) <- liftReturnOp p_t $ opVariable cStorageClassFunction
  ensureTypeId p_t
  modify $ \s -> s { compVarRefs = M.insert name (var_id, t, Inline) $ compVarRefs s }
  return var_id

getResult :: CompilerM [Word32]
getResult = gets compResult

getEntryPoints :: CompilerM [(String, Word32)]
getEntryPoints = gets compEntryPoints

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

addEntryPoint :: String -> Word32 -> CompilerM ()
addEntryPoint name entry_id = modify $ \s -> s { compEntryPoints = (name, entry_id) : compEntryPoints s }

addUse :: VName -> SPIRVType -> KernelUse -> CompilerM ()
addUse name t use =  do
  s <- get 
  if M.member name $ compVarRefs s
    then return ()
    else do
      var_id <- newId
      modify $ \s -> s { compVarRefs = M.insert name (var_id, t, InterOp) $ compVarRefs s,
                         compUses    = M.insert name (var_id, use) $ compUses s }

registerKernelUse :: KernelUse -> CompilerM ()
registerKernelUse use@(MemoryUse name _) = addUse name (Memory name) use
registerKernelUse use@(ConstUse name _) = return () -- Todo: Fix (What about type?)
registerKernelUse use@(ScalarUse name t) = do
  ensureTypeId $ Scalar t
  addUse name (Scalar t) use

typeToScope :: SPIRVType -> VarScope
typeToScope (Memory _) = Uniform
typeToScope _          = Internal

scopeToStorageClass :: VarScope -> Word32
scopeToStorageClass Uniform  = cStorageClassUniform
scopeToStorageClass Internal = cStorageClassFunction
scopeToStorageClass Input    = cStorageClassInput

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
insertStore to_id from_id = appendCode $ opStore to_id from_id

insertInternalArrayWrite :: Word32 -> SPIRVType -> Int -> PrimValue -> CompilerM ()
insertInternalArrayWrite arr_id r_t i val = do
  val_id <- getConstId val
  i_id <- getConstId $ IntValue $ Int32Value $ fromIntegral i
  (chain_id, _) <- liftReturnOp (Pointer r_t Internal) $ opAccessChain arr_id i_id
  insertStore chain_id val_id

insertEntryPoint :: String -> Code KernelOp -> CompilerM ()
insertEntryPoint name body = do
  void_func_t <- getTypeId $ Function Void
  (entry_id, _) <- liftReturnOp Void $ opFunction cFunctionControlNone void_func_t
  newId >>= insertLabel
  compileCode body
  appendCode opReturn
  appendCode opFunctionEnd
  addEntryPoint name entry_id

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
insertVarLoad (var_id, var_t, _) = liftReturnOp var_t $ opLoad var_id cMemoryAccessNone

readBuiltinTo :: Builtin -> Int32 -> VName -> CompilerM ()
readBuiltinTo builtin i target = do
  let r_t = Scalar int32
  var_id <- getBuiltinId builtin
  i_id <- getConstId $ IntValue $ Int32Value i
  (chain_id, _) <- liftReturnOp (Pointer r_t Input) $ opAccessChain var_id i_id
  (load_id, _) <- liftReturnOp r_t $ opLoad chain_id cMemoryAccessNone
  (target_id, _, _) <- getVarInfo target
  appendCode $ opStore target_id load_id

getMemoryAccessType :: Volatility -> Word32
getMemoryAccessType Volatile = cMemoryAccessVolatile
getMemoryAccessType Nonvolatile = cMemoryAccessNone

getPrimTypeDeclaration :: PrimType -> Word32 -> [Word32]
getPrimTypeDeclaration t@(IntType _) id   = opTypeInt (fromIntegral (primBitSize t)) cNoSignedness id
getPrimTypeDeclaration t@(FloatType _) id = opTypeFloat (fromIntegral (primBitSize t)) id
getPrimTypeDeclaration Bool id            = opTypeBool id
getPrimTypeDeclaration Cert _             = []

getSPIRVTypeDeclaration :: SPIRVType -> Word32 -> CompilerM [Word32]
getSPIRVTypeDeclaration Void id                 = return $ opTypeVoid id
getSPIRVTypeDeclaration (Scalar t) id           = return $ getPrimTypeDeclaration t id
getSPIRVTypeDeclaration (Memory _) _            = return []
getSPIRVTypeDeclaration (Vector t len) id       = do
  t_id <- getTypeId $ Scalar t
  return $ opTypeVector t_id len id
getSPIRVTypeDeclaration (Pointer t scope) id    = do
  let storage = scopeToStorageClass scope
  t_id <- getTypeId t
  return $ opTypePointer storage t_id id
getSPIRVTypeDeclaration (Function t) id         = do
  t_id <- getTypeId t
  return $ opTypeFunction t_id id
getSPIRVTypeDeclaration (StaticArray t size) id = do
  t_id <- getTypeId t
  return $ opTypeArray t_id size id

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
getConstDeclaration Checked _ = return [] -- Todo: Fix

getEntryPointDeclaration :: String -> Word32 -> [Word32]
getEntryPointDeclaration name id = opEntryPoint cExecutionModelGLCompute id $ encodeString name

compileLeaf :: ExpLeaf -> CompilerM ExprInfo
compileLeaf (SizeOf t) = do
  s_id <- getConstId $ IntValue $ Int32Value $ primByteSize t
  return (s_id, Scalar int32)
compileLeaf (ScalarVar src) = getVarInfo src >>= insertVarLoad 
compileLeaf (Index src (Count iexp) restype (Space space) vol) = do
  let mem_access = getMemoryAccessType vol
      r_t = Scalar restype
  (var_id, var_t, _) <- getVarInfo src
  case var_t of
    Memory _ -> setMemElemType src restype
    _        -> return ()
  let scope = typeToScope var_t
  (i_id, _) <- compileExp iexp
  (chain_id, _) <- liftReturnOp (Pointer r_t scope) $ opAccessChain var_id i_id
  liftReturnOp r_t $ opLoad chain_id mem_access

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
                  _           -> return (0, Scalar Bool) -- TODO: Fix?
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
compileKernelOp (GetLocalSize name i) = readBuiltinTo WorkgroupSize (fromIntegral i) name
compileKernelOp (GetGlobalSize name i) = readBuiltinTo NumWorkgroups (fromIntegral i) name
compileKernelOp (GetGlobalId name i) = readBuiltinTo GlobalInvocationId (fromIntegral i) name
compileKernelOp (GetLockstepWidth name) = return () -- TODO: Fix
compileKernelOp (Atomic op) = compileAtomicOp op
compileKernelOp Barrier = appendCode $ opControlBarrier cScopeWorkgroup cScopeWorkgroup $
  cMemorySemanticsAcquireRelease .|. cMemorySemanticsSequentiallyConsistent
compileKernelOp MemFence = appendCode $ opMemoryBarrier cScopeWorkgroup $
  cMemorySemanticsAcquireRelease .|. cMemorySemanticsSequentiallyConsistent

compileCode :: Code KernelOp -> CompilerM ()
compileCode Skip = return ()
compileCode (lc :>>: rc) =  compileCode lc >> compileCode rc
compileCode (Comment _ c) = compileCode c
-- ^ SPIR-V does not support comments
compileCode (DeclareScalar name t) = void $ addVarInline name $ Scalar t
compileCode (SetScalar dest src) = do
  (var_id, _, _) <- getVarInfo dest
  (src_id, _) <- compileExp src
  insertStore var_id src_id
compileCode (DeclareArray name (Space space) t vs) = do
  let len = length vs
  var_id <- addVarInline name $ StaticArray (Scalar t) $ fromIntegral len
  zipWithM_ (insertInternalArrayWrite var_id (Scalar t)) [0..len-1] vs
compileCode (DeclareMem name space) = return () -- TODO: Fix
compileCode (Allocate name (Count e) space) = return () -- TODO: Fix
compileCode (Free name space) = return () -- TODO: Fix
compileCode (Copy dest (Count destoffset) destspace src (Count srcoffset) DefaultSpace (Count size)) = return () -- TODO: Fix
compileCode (Write dest (Count idx) elemtype (Space space) vol elemexp) = do
  let r_t = Scalar elemtype
  setMemElemType dest elemtype
  (elem_id, _) <- compileExp elemexp
  (dest_id, _, _) <- getVarInfo dest
  (i_id, _) <- compileExp idx
  (chain_id, _) <- liftReturnOp (Pointer r_t Uniform) $ opAccessChain dest_id i_id
  appendCode $ opStore chain_id elem_id
compileCode (SetMem dest src space) = return () -- TODO: Fix
compileCode (Call dests fname args) = return () -- TODO: Fix
compileCode (Assert e (ErrorMsg parts) (loc, locs)) = return () -- TODO: Fix
compileCode (DebugPrint s t e) = return () -- TODO: Fix
compileCode (Op op) = compileKernelOp op
compileCode (If cond tbranch fbranch) = do
  true_id <- newId
  false_id <- newId
  end_id <- newId
  (cond_id, _) <- compileExp cond
  appendCode $ opSelectionMerge end_id
  insertBranchConditional cond_id true_id false_id
  insertBranch true_id
  compileCode tbranch
  insertBranch end_id
  insertLabel false_id
  compileCode fbranch
  insertBranch end_id
  insertLabel end_id
compileCode (While cond body) = insertLoop (fst <$> compileExp cond) $ compileCode body
compileCode (For i it bound body) = do
  i_id <- addVarInline i sit
  init_id <- getConstId $ IntValue $ intValue it 0
  appendCode $ opStore i_id init_id
  insertLoop (check i_id) $ compileCode body
  where sit = Scalar $ IntType it
        check i_id = do
          (condi_id, _) <- liftReturnOp sit $ opLoad i_id cMemoryAccessNone
          (bound_id, _) <- compileExp bound
          (check_id, _) <- liftReturnOp (Scalar Bool) $ opULessThan condi_id bound_id
          return check_id

compileKernel :: CallKernel -> CompilerM ()
compileKernel (Map kernel) = do
  let name = baseString $ mapKernelThreadNum kernel
  mapM_ registerKernelUse $ mapKernelUses kernel
  insertEntryPoint name $ mapKernelBody kernel
compileKernel (AnyKernel kernel) = do
  let name = baseString $ kernelName kernel
  mapM_ registerKernelUse $ kernelUses kernel
  insertEntryPoint name $ kernelBody kernel
compileKernel (MapTranspose t name e1 n1 e2 e3 e4 e5 e6 e7) = return () -- TODO: Fix

finalizedProgram :: CompilerM [Word32]
finalizedProgram = do
  code_body <- gets compResult
  -- TODO: Entry points
  -- TODO: Insert all decorations
  -- TODO: Insert used builtin
  -- TODO: Insert all interop vars
  -- TODO: Insert all spec constants
  entry_map <- getEntryPoints
  let entries = map (uncurry getEntryPointDeclaration) entry_map
  const_map <- gets compConstRefs
  consts <- sequence $ M.elems $ M.mapWithKey getConstDeclaration const_map
  type_map <- gets compTypeRefs
  types <- sequence $ M.elems $ M.mapWithKey getSPIRVTypeDeclaration type_map
  max_id <- gets compCurrentMaxId
  glsl_id <- gets compGLSLExtId
  let header = genHeader max_id glsl_id
  return $ header ++ concat entries ++ concat types ++ concat consts ++ code_body
