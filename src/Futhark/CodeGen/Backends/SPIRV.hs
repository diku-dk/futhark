{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Futhark.CodeGen.Backends.SPIRV (kernelToShader) where
import Data.Function (on)
import Data.Maybe
import Data.List
import Control.Monad (void, when, unless)
import qualified Data.Set as S
import qualified Data.Map as M
import Futhark.CodeGen.Backends.SPIRV.Builder hiding (Builder)
import qualified Futhark.CodeGen.Backends.SPIRV.Builder as SPIRVB (Builder)
import Futhark.CodeGen.ImpCode hiding (Constant, Function, (.|.))
import Futhark.CodeGen.ImpCode.Kernels hiding (Code, Constant, Function, (.|.))
import Futhark.CodeGen.ImpCode.Vulkan (EntryPointArg, SpecConstExp(..), SingleEntryShader(..), EntryPointArg(..))
import Futhark.CodeGen.Backends.SPIRV.ArrayAccess (ArrayAccesses, getArrayAccesses)
import Futhark.CodeGen.Backends.SPIRV.Operations
import Futhark.IR.Prop.Types (int32, int64, float32, float64)
import Futhark.IR.Syntax.Core (SubExp(..))

data ScalarInfo = ScalarInfo
  { varId :: Id,
    varType :: SPIRVType,
    varVol :: Volatility
  }
  deriving (Show)
data FutConstInfo = FutConstInfo
  { fcId :: Id,
    fcType :: SPIRVType
  }
  deriving (Show)
data ArrayInfo = ArrayInfo
  { arrId :: Id,
    arrElemType :: SPIRVType
  }
  deriving (Show)

data Var
  = VarScalar ScalarInfo
  | VarFutConst FutConstInfo
  | VarArray ArrayInfo
  deriving (Show)

fromScalarInfo :: Var -> ScalarInfo
fromScalarInfo (VarScalar v) = v
fromScalarInfo (VarFutConst _) =
  error "Expected scalar variable, got Futhark const"
fromScalarInfo (VarArray _) = error "Expected scalar variable, got array"

fromArrayInfo :: Var -> ArrayInfo
fromArrayInfo (VarScalar _) = error "Expected array variable, got scalar"
fromArrayInfo (VarFutConst _) =
  error "Expected array variable, got Futhark const"
fromArrayInfo (VarArray v) = v

data ImpCodeToShader = ImpCodeToShader
  { shVars :: M.Map VName Var,
    shInterfaceVars :: [(EntryPointArg, BindingId)],
    shSpecConsts :: [(SpecConstExp, SpecId)],
    shArrayElemTypes :: M.Map VName PrimType
  }
  deriving (Show)

type Builder = SPIRVB.Builder ImpCodeToShader

-- | Translate a Futhark @Volatility@ to SPIR-V @MemoryAccess@ flags
volatilityToMemoryAccess :: Volatility -> Maybe MemoryAccess
volatilityToMemoryAccess Volatile = Just $ MemoryAccess [MemoryAccessVolatile]
volatilityToMemoryAccess Nonvolatile = Nothing

addScalar :: VName -> ScalarInfo -> Builder ()
addScalar v info = modifyUserState $
  \s -> s { shVars = M.insert v (VarScalar info) (shVars s) }

addFutConst :: VName -> FutConstInfo -> Builder ()
addFutConst v info = modifyUserState $
  \s -> s { shVars = M.insert v (VarFutConst info) (shVars s) }

addArray :: VName -> ArrayInfo -> Builder ()
addArray v info = modifyUserState $
  \s -> s { shVars = M.insert v (VarArray info) (shVars s) }

getVar :: VName -> Builder Var
getVar v =
  getUserStateS $
    fromMaybe (error $ "Variable does not exist: " ++ show v)
    . M.lookup v . shVars

getScalar :: VName -> Builder ScalarInfo
getScalar v = fromScalarInfo <$> getVar v

getArray :: VName -> Builder ArrayInfo
getArray v = fromArrayInfo <$> getVar v

loadScalar :: VName -> Builder Id
loadScalar v = do
  s <- getVar v
  case s of
    VarScalar (ScalarInfo vId vType vVol) ->
      let memAccess = volatilityToMemoryAccess vVol
      in instrRT vType $ OpLoad vId memAccess
    VarFutConst (FutConstInfo cId _) ->
      return cId
    _ -> error  "Expected scalar or const variable, got array"

getFutConst :: VName -> Builder Id
getFutConst v = do
  s <- getVar v
  case s of
    VarFutConst (FutConstInfo cId _) ->
      return cId
    _ -> error $ "No such Futhark constant: " ++ show v

storeScalar :: VName -> Id -> Builder ()
storeScalar v i = do
  ScalarInfo vId vType vVol <- getScalar v
  let memAccess = volatilityToMemoryAccess vVol
  assertType vType i
  instr $ OpStore vId i memAccess

modifyScalar :: VName -> (Id -> Builder Id) -> Builder ()
modifyScalar v f = do
  ScalarInfo vId vType vVol <- getScalar v
  let memAccess = volatilityToMemoryAccess vVol
  loaded <- instrRT vType $ OpLoad vId memAccess
  storeMe <- f loaded
  assertType vType storeMe
  instr $ OpStore vId storeMe memAccess

loadArray :: VName -> Id -> Volatility -> Builder Id
loadArray v idx vol = do
  ArrayInfo aId aType <- getArray v
  (accessId, _) <- makeArrayAccess aId idx
  let memAccess = volatilityToMemoryAccess vol
  instrRT aType $ OpLoad accessId memAccess

storeArray :: VName -> Id -> Id -> Volatility -> Builder ()
storeArray v idx storeMe vol = do
  ArrayInfo aId _ <- getArray v
  (accessId, _) <- makeArrayAccess aId idx
  let memAccess = volatilityToMemoryAccess vol
  instr $ OpStore accessId storeMe memAccess

declareFunctionScalar :: VName -> SPIRVType -> Volatility -> Maybe SPIRVConstant -> Builder Id
declareFunctionScalar v t vol initValue = do
  i <- makeFunctionVar t initValue
  addScalar v $ ScalarInfo i t vol
  return i

as :: PrimType -> Id -> Builder Id
as dstPrimType i = do
  srcType <- getType i
  case (dstPrimType, srcType) of
    (_, TScalar srcPrimType) | dstPrimType == srcPrimType ->
      return i
    (IntType _, TScalar (IntType _)) ->
      instrRT (TScalar dstPrimType) $ OpUConvert i
    (FloatType _, TScalar (FloatType _)) ->
      instrRT (TScalar dstPrimType) $ OpFConvert i
    _ -> error $ "Bad conversion: " ++ show srcType ++
                  " to " ++ show (TScalar dstPrimType)

-- Insert a loop. @check@ must return an Id of a Bool
loop :: Builder Id -> Builder () -> Builder () -> Builder ()
loop check body continue = do
  startLabel <- newId
  checkLabel <- newId
  bodyLabel <- newId
  contLabel <- newId
  endLabel <- newId
  instr $ OpBranch startLabel
  instr $ OpLabel startLabel
  instr $ OpLoopMerge endLabel contLabel $ LoopControl [LoopControlNone]
  instr $ OpBranch checkLabel
  instr $ OpLabel checkLabel
  cond <- check
  assertType (TScalar Bool) cond
  instr $ OpBranchConditional cond bodyLabel endLabel []
  instr $ OpLabel bodyLabel
  body
  instr $ OpBranch contLabel
  instr $ OpLabel contLabel
  continue
  instr $ OpBranch startLabel
  instr $ OpLabel endLabel

if_ :: Builder Id -> Builder () -> Builder () -> Builder ()
if_ check tbranch fbranch = do
  trueLabel <- newId
  falseLabel <- newId
  endLabel <- newId
  cond <- check
  assertType (TScalar Bool) cond
  instr $ OpSelectionMerge endLabel $ SelectionControl [SelectionControlNone]
  instr $ OpBranchConditional cond trueLabel falseLabel []
  instr $ OpLabel trueLabel
  tbranch
  instr $ OpBranch endLabel
  instr $ OpLabel falseLabel
  fbranch
  instr $ OpBranch endLabel
  instr $ OpLabel endLabel

compileLeafExp :: ExpLeaf -> Builder Id
compileLeafExp (ScalarVar v) = loadScalar v
compileLeafExp (SizeOf t) =
  getConstId $ Const $ IntValue $ Int32Value $ primByteSize t
compileLeafExp (Index v (Count idxExp) t _ vol) = do
  idx <- compileExp $ untyped idxExp
  loaded <- loadArray v idx vol
  assertType (TScalar t) loaded
  return loaded

compilePrimExp :: PrimExp ExpLeaf -> Builder Id
compilePrimExp (LeafExp leaf pt) = compileLeafExp leaf >>= as pt
compilePrimExp (ValueExp val) = getConstId (Const val)
compilePrimExp (BinOpExp bop e1 e2) = do
  let pt = binOpType bop
      t = TScalar pt
  x <- compileExp e1 >>= as pt
  y <- compileExp e2 >>= as pt
  case bop of
    Add _ _ -> instrRT t $ OpIAdd x y
    FAdd _ -> instrRT t $ OpFAdd x y
    Sub _ _ -> instrRT t $ OpISub x y
    FSub _ -> instrRT t $ OpFSub x y
    Mul _ _ -> instrRT t $ OpIMul x y
    FMul _ -> instrRT t $ OpFMul x y
    UDiv _ _ -> instrRT t $ OpUDiv x y
    UDivUp _ _ -> unsignedDivisionUp t x y
    SDiv _ _ -> signedDivision t x y
    SDivUp _ _ -> signedDivisionUp t x y
    FDiv _ -> instrRT t $ OpFDiv x y
    FMod _ -> floatModulo t x y
    UMod _ _ -> instrRT t $ OpUMod x y
    SMod _ _ -> instrRT t $ OpSMod x y
    SQuot _ _ -> instrRT t $ OpUDiv x y
    SRem _ _ ->
      -- OpSRem doesn't do what we want here (at least not on my machine - is
      -- it a bug?)
      signedRemainder t x y
    SMin _ -> glslInstr t $ GlslSMin x y
    UMin _ -> glslInstr t $ GlslUMin x y
    FMin _ -> glslInstr t $ GlslFMin x y
    SMax _ -> glslInstr t $ GlslSMax x y
    UMax _ -> glslInstr t $ GlslUMax x y
    FMax _ -> glslInstr t $ GlslFMax x y
    Shl _ -> instrRT t $ OpShiftLeftLogical x y
    LShr _ -> instrRT t $ OpShiftRightLogical x y
    AShr _ -> instrRT t $ OpShiftRightArithmetic x y
    And _ -> instrRT t $ OpBitwiseAnd x y
    Or _ -> instrRT t $ OpBitwiseOr x y
    Xor _ -> instrRT t $ OpBitwiseXor x y
    Pow _ -> integerPower t x y
    FPow _ ->
      case t of
        TScalar (FloatType Float32) -> glslInstr t $ GlslPow x y
        TScalar (FloatType Float64) ->
          withF64AsF32_2 (x, y) $ \(x', y') ->
            glslInstr t $ GlslPow x' y'
        _ -> error "Unsupported type for FPow"
    LogAnd -> instrRT t $ OpLogicalAnd x y
    LogOr -> instrRT t $ OpLogicalOr x y
compilePrimExp (CmpOpExp cop e1 e2) = do
  let bool = TScalar Bool
      pt = cmpOpType cop
  x <- compileExp e1 >>= as pt
  y <- compileExp e2 >>= as pt
  case cop of
    CmpEq (IntType _) -> instrRT bool $ OpIEqual x y
    CmpEq (FloatType _) -> instrRT bool $ OpFOrdEqual x y
    CmpEq Bool -> instrRT bool $ OpLogicalEqual x y
    CmpEq _ -> error "Unsupported type for CmpEq"
    CmpUlt _ -> instrRT bool $ OpULessThan x y
    CmpUle _ -> instrRT bool $ OpULessThanEqual x y
    CmpSlt _ -> instrRT bool $ OpSLessThan x y
    CmpSle _ -> instrRT bool $ OpSLessThanEqual x y
    FCmpLt _ -> instrRT bool $ OpFOrdLessThan x y
    FCmpLe _ -> instrRT bool $ OpFOrdLessThanEqual x y
    CmpLlt -> do
      not_x <- instrRT bool $ OpLogicalNot x
      instrRT bool $ OpLogicalAnd not_x y
    CmpLle -> do
      not_x <- instrRT bool $ OpLogicalNot x
      instrRT bool $ OpLogicalOr not_x y
compilePrimExp (UnOpExp uop e) = do
  x <- compileExp e >>= as (unOpType uop)
  case uop of
    Not -> instrRT (TScalar Bool) $ OpLogicalNot x
    Complement t -> instrRT (TScalar $ IntType t) $ OpNot x
    Abs t -> glslInstr (TScalar $ IntType t) $ GlslSAbs x
    FAbs t -> glslInstr (TScalar $ FloatType t) $ GlslFAbs x
    SSignum t -> glslInstr (TScalar $ IntType t) $ GlslSSign x
    USignum t -> do
      const0 <- getConstId (Const $ IntValue $ intValue t (0 :: Integer))
      const1 <- getConstId (Const $ IntValue $ intValue t (1 :: Integer))
      isZero <- instrRT (TScalar Bool) $ OpIEqual x const0
      instrRT (TScalar $ IntType t) $ OpSelect isZero const0 const1
compilePrimExp (ConvOpExp cop e) = do
  x <- compileExp e >>= as (fst $ convOpType cop)
  case cop of
    ZExt _ t -> instrRT (TScalar $ IntType t) $ OpUConvert x
    SExt _ t -> instrRT (TScalar $ IntType t) $ OpSConvert x
    FPConv _ t -> instrRT (TScalar $ FloatType t) $ OpFConvert x
    FPToUI tf ti -> do
      -- "Behavior is undefined if Result Type is not wide enough to hold the
      -- converted value."
      unless (intCanDefinitelyHold ti tf) $ error "FPToUI: Dst type possibly too small"
      instrRT (TScalar $ IntType ti) $ OpConvertFToU x
    FPToSI tf ti -> do
      -- "Behavior is undefined if Result Type is not wide enough to hold the
      -- converted value."
      unless (intCanDefinitelyHold ti tf) $ error "FPToSI: Dst type possibly too small"
      instrRT (TScalar $ IntType ti) $ OpConvertFToS x
    UIToFP _ t -> instrRT (TScalar $ FloatType t) $ OpConvertUToF x
    SIToFP _ t -> instrRT (TScalar $ FloatType t) $ OpConvertSToF x
    IToB t -> do
      const0 <- getConstId (Const $ IntValue $ intValue t (0 :: Integer))
      instrRT (TScalar Bool) $ OpINotEqual x const0
    BToI t -> do
      const0 <- getConstId (Const $ IntValue $ intValue t (0 :: Integer))
      const1 <- getConstId (Const $ IntValue $ intValue t (1 :: Integer))
      instrRT (TScalar Bool) $ OpSelect x const1 const0
    where
      intCanDefinitelyHold Int32 Float32 = True
      intCanDefinitelyHold Int64 Float32 = True
      intCanDefinitelyHold Int64 Float64 = True
      intCanDefinitelyHold _ _ = False
compilePrimExp (FunExp fn argExps primType) = do
  args <- mapM compileExp argExps
  compileFunCall fn args primType

withF64AsF32 :: Id -> (Id -> Builder Id) -> Builder Id
withF64AsF32 f64 f = do
  f32 <- instrRT (TScalar float32) $ OpFConvert f64
  f32' <- f f32
  instrRT (TScalar float64) $ OpFConvert f32'

withF64AsF32_2 :: (Id, Id) -> ((Id, Id) -> Builder Id) -> Builder Id
withF64AsF32_2 (f64a, f64b) f = do
  f32a <- instrRT (TScalar float32) $ OpFConvert f64a
  f32b <- instrRT (TScalar float32) $ OpFConvert f64b
  f32' <- f (f32a, f32b)
  instrRT (TScalar float64) $ OpFConvert f32'

compileFunCall :: String -> [Id] -> PrimType -> Builder Id
compileFunCall "sqrt32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslSqrt x
compileFunCall "sqrt64" [x] (FloatType Float64) =
  glslInstr (TScalar float64) $ GlslSqrt x
compileFunCall "log32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslLog x
compileFunCall "log64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "log32" [y] float32
compileFunCall "log10_32" [x] (FloatType Float32) = do
  a <- glslInstr (TScalar float32) $ GlslLog x
  const10 <- getConstId (Const $ FloatValue $ Float32Value 10.0)
  b <- glslInstr (TScalar float32) $ GlslLog const10
  instrRT (TScalar float32) $ OpFDiv a b
compileFunCall "log10_64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "log10_32" [y] float32
compileFunCall "log2_32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslLog2 x
compileFunCall "log2_64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "log2_32" [y] float32
compileFunCall "exp32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslExp x
compileFunCall "exp64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "exp32" [y] float32
compileFunCall "sin32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslSin x
compileFunCall "sin64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "sin32" [y] float32
compileFunCall "cos32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslCos x
compileFunCall "cos64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "cos32" [y] float32
compileFunCall "cosh32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslCosh x
compileFunCall "cosh64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "cosh32" [y] float32
compileFunCall "tan32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslTan x
compileFunCall "tan64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "tan32" [y] float32
compileFunCall "tanh32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslTanh x
compileFunCall "tanh64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "tanh32" [y] float32
compileFunCall "asin32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslAsin x
compileFunCall "asin64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "asin32" [y] float32
compileFunCall "asinh32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslAsinh x
compileFunCall "asinh64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "asinh32" [y] float32
compileFunCall "acos32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslAcos x
compileFunCall "acos64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "acos32" [y] float32
compileFunCall "acosh32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslAcosh x
compileFunCall "acosh64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "acosh32" [y] float32
compileFunCall "atan32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslAtan x
compileFunCall "atan64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "atan32" [y] float32
compileFunCall "atanh32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslAtanh x
compileFunCall "atanh64" [x] (FloatType Float64) =
  withF64AsF32 x $ \y -> compileFunCall "atanh32" [y] float32
compileFunCall "round32" [x] (FloatType Float32) =
  -- NB: Rounding towards even numbers
  glslInstr (TScalar float32) $ GlslRoundEven x
compileFunCall "round64" [x] (FloatType Float64) =
  -- NB: Rounding towards even numbers
  glslInstr (TScalar float64) $ GlslRoundEven x
compileFunCall "ceil32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslCeil x
compileFunCall "ceil64" [x] (FloatType Float64) =
  glslInstr (TScalar float64) $ GlslCeil x
compileFunCall "floor32" [x] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslFloor x
compileFunCall "floor64" [x] (FloatType Float64) =
  glslInstr (TScalar float64) $ GlslFloor x
compileFunCall "popc8" [x] t@(IntType _) =
  instrRT (TScalar t) $ OpBitCount x
compileFunCall "popc16" [x] t@(IntType _) =
  instrRT (TScalar t) $ OpBitCount x
compileFunCall "popc32" [x] t@(IntType _) =
  instrRT (TScalar t) $ OpBitCount x
compileFunCall "popc64" [x] t@(IntType _) =
  instrRT (TScalar t) $ OpBitCount x
compileFunCall "atan2_32" [x, y] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslAtan2 x y
compileFunCall "atan2_64" [x, y] (FloatType Float64) =
  withF64AsF32_2 (x, y) $ \(x', y') ->
    compileFunCall "atan2_32" [x', y'] float32
compileFunCall "isinf32" [x] (FloatType Float32) =
  instrRT (TScalar Bool) $ OpIsInf x
compileFunCall "isinf64" [x] (FloatType Float64) =
  instrRT (TScalar Bool) $ OpIsInf x
compileFunCall "isnan32" [x] (FloatType Float32) =
  instrRT (TScalar Bool) $ OpIsNan x
compileFunCall "isnan64" [x] (FloatType Float64) =
  instrRT (TScalar Bool) $ OpIsNan x
compileFunCall "to_bits32" [x] (IntType Int32) =
  instrRT (TScalar int32) $ OpBitcast x
compileFunCall "to_bits64" [x] (IntType Int64) =
  instrRT (TScalar int64) $ OpBitcast x
compileFunCall "from_bits32" [x] (FloatType Float32) =
  instrRT (TScalar float32) $ OpBitcast x
compileFunCall "from_bits64" [x] (FloatType Float64) =
  instrRT (TScalar float64) $ OpBitcast x
compileFunCall "mad32" [x, y, z] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslFma x y z
compileFunCall "mad64" [x, y, z] (FloatType Float64) =
  glslInstr (TScalar float64) $ GlslFma x y z
compileFunCall "fma32" [x, y, z] (FloatType Float32) =
  glslInstr (TScalar float32) $ GlslFma x y z
compileFunCall "fma64" [x, y, z] (FloatType Float64) =
  glslInstr (TScalar float64) $ GlslFma x y z
compileFunCall fn args retType =
  error $ fn ++ " with arity " ++ show (length args)
            ++ " and return type " ++ show retType ++ " is unsupported"

compileExp :: PrimExp ExpLeaf -> Builder Id
compileExp e = do
  i <- compilePrimExp e
  assertType (TScalar $ primExpType e) i
  return i

scopeAndMemorySemantics :: Bool -> Builder (Id, Id)
scopeAndMemorySemantics isLocal = do
  scopeId <- getConstId $ Const $ IntValue $ Int32Value $ fromIntegral $
                fromList $ spirvSerialize scope
  memSemId <- getConstId $ Const $ IntValue $ Int32Value $ fromIntegral $
                fromList $ spirvSerialize memSem
  return (scopeId, memSemId)
  where
    scope = if isLocal then ScopeWorkgroup else ScopeDevice
    memSem =
      if isLocal
      then MemorySemantics
            [MemorySemanticsSequentiallyConsistent,
             MemorySemanticsWorkgroupMemory]
      else MemorySemantics
            [MemorySemanticsSequentiallyConsistent,
             MemorySemanticsWorkgroupMemory,
             MemorySemanticsUniformMemory]

fromList :: [a] -> a
fromList [x] = x
fromList _ = error "Expected 1-element list"

compileAtomicOp :: Space -> AtomicOp -> Builder ()
compileAtomicOp _ aop = do
  (f, primType, old, arr, idxExp, valExp) <- unpack aop
  let t = TScalar primType
  oldType <- varType <$> getScalar old
  arrVar <- getArray arr
  unless (t == oldType && t == arrElemType arrVar) $ error "Type mismatch"
  val <- compileExp valExp >>= as primType
  idx <- compileExp $ untyped idxExp
  (accessId, sClass) <- makeArrayAccess (arrId arrVar) idx
  let isLocal =
        case sClass of
          StorageClassStorageBuffer -> False
          StorageClassWorkgroup -> True
          _ -> error "Bad StorageClass"
  (scopeId, memSemId) <- scopeAndMemorySemantics isLocal
  o <- instrRT t $ f accessId scopeId memSemId val
  storeScalar old o
  where
    unpack (AtomicAdd t old arr (Count i) e) =
      return (OpAtomicIAdd, IntType t, old, arr, i, e)
    unpack (AtomicFAdd _ _ _ _ _) = error "AtomicFAdd unsupported"
    unpack (AtomicSMax t old arr (Count i) e) =
      return (OpAtomicSMax, IntType t, old, arr, i, e)
    unpack (AtomicSMin t old arr (Count i) e) =
      return (OpAtomicSMin, IntType t, old, arr, i, e)
    unpack (AtomicUMax t old arr (Count i) e) =
      return (OpAtomicUMax, IntType t, old, arr, i, e)
    unpack (AtomicUMin t old arr (Count i) e) =
      return (OpAtomicUMin, IntType t, old, arr, i, e)
    unpack (AtomicAnd t old arr (Count i) e) =
      return (OpAtomicAnd, IntType t, old, arr, i, e)
    unpack (AtomicOr t old arr (Count i) e) =
      return (OpAtomicOr, IntType t, old, arr, i, e)
    unpack (AtomicXor t old arr (Count i) e) =
      return (OpAtomicXor, IntType t, old, arr, i, e)
    unpack (AtomicXchg t old arr (Count i) e) =
      return (OpAtomicExchange, t, old, arr, i, e)
    unpack (AtomicCmpXchg t old arr (Count i) cmpExp e) = do
      cmp <- compileExp cmpExp
      let f p scope memSem val =
            OpAtomicCompareExchange p scope memSem memSem val cmp
      return (f, t, old, arr, i, e)

compileLocalAlloc :: VName -> Exp -> PrimType -> Builder ()
compileLocalAlloc v cExp elemPrimType = do
  -- We assume cExp is either
  --  1) a (64-bit unsigned) product of two @ExpLeaf@s:
  --      i)  A SizeOf exp that matches the element type found in shArrayElemTypes
  --      ii) A ScalarVar exp that maps to a VarFutConst (using loadFutConst)
  --  2) a (64-bit unsigned) product of two @ExpLeaf@s:
  --      i)  A SizeOf exp that matches the element type found in shArrayElemTypes
  --      ii) A ValueExp 64-bit unsigned int
  --  3) a (64-bit unsigned) SizeOf exp that matches the element type found in
  --     shArrayElemTypes
  --  4) a (64-bit unsigned) ValueExp indicating the byte-size of the array. In
  --     this case, the appropriate entry in shArrayElemTypes is relied upon.
  let elemType = TScalar elemPrimType
  case fromProduct cExp of
    Left (et, sizeVar) | et == elemPrimType -> do
      cId <- getFutConst sizeVar -- sizeVar must be a futconstant
      i <- makeLocalMemArray elemType cId
      addArray v $ ArrayInfo i elemType
    Right (Just et, numElems) | et == elemPrimType -> do
      i <- makeLocalMemArrayConst elemType $ IntValue $ Int64Value numElems
      addArray v $ ArrayInfo i elemType
    Right (Nothing, byteSize) -> do
      let (numElems, m) = byteSize `divMod` primByteSize elemPrimType
      unless (m == 0) $
        error "Array byte size not divisible by element type byte size"
      i <- makeLocalMemArrayConst elemType $ IntValue $ Int64Value numElems
      addArray v $ ArrayInfo i elemType
    _ ->
      error "SizeOf in Count Exp does not match array element type"
  where
    fromProduct (BinOpExp (Mul Int64 OverflowUndef)
                 (LeafExp (SizeOf et) (IntType Int64))
                 (LeafExp (ScalarVar sizeVar) (IntType Int64))) =
      Left $ (et, sizeVar)
    fromProduct (BinOpExp (Mul Int64 OverflowUndef)
                 (LeafExp (ScalarVar sizeVar) (IntType Int64))
                 (LeafExp (SizeOf et) (IntType Int64))) =
      Left $ (et, sizeVar)
    fromProduct (BinOpExp (Mul Int64 OverflowUndef)
                 (ValueExp (IntValue (Int64Value n)))
                 (LeafExp (SizeOf et) (IntType Int64))) =
      Right $ (Just et, n)
    fromProduct (BinOpExp (Mul Int64 OverflowUndef)
                 (LeafExp (SizeOf et) (IntType Int64))
                 (ValueExp (IntValue (Int64Value n)))) =
      Right $ (Just et, n)
    fromProduct (LeafExp (SizeOf et) (IntType Int64)) =
      Right $ (Just et, 1)
    fromProduct (ValueExp (IntValue (Int64Value n))) =
      Right $ (Nothing, n)
    fromProduct e = error $ "Not a simple product: " ++ show e

compileKernelOp :: KernelOp -> Builder ()
compileKernelOp (GetGroupId v n) =
  getBuiltIn BuiltInWorkgroupId n >>= storeScalar v
compileKernelOp (GetLocalId v n) =
  getBuiltIn BuiltInLocalInvocationId n >>= storeScalar v
compileKernelOp (GetLocalSize v n) =
  getBuiltIn BuiltInWorkgroupSize n >>= instrRT (TScalar int64) . OpUConvert >>= storeScalar v
compileKernelOp (GetGlobalSize v n) = do
  wgSize <- getBuiltIn BuiltInWorkgroupSize n
  numWgs <- getBuiltIn BuiltInNumWorkgroups n
  globSize <- instrRT (TScalar int32) $ OpIMul wgSize numWgs
  -- XXX: Does the ImpCode expect this to be int32 or int64?
  globSize64 <- instrRT (TScalar int64) $ OpUConvert globSize
  storeScalar v globSize64
compileKernelOp (GetGlobalId v n) =
  getBuiltIn BuiltInGlobalInvocationId n >>= storeScalar v
compileKernelOp (GetLockstepWidth v) =
  -- There is no defined lockstep width in Vulkan
  getConstId (Const $ IntValue $ Int32Value 1) >>= storeScalar v
compileKernelOp (Atomic space aop) = compileAtomicOp space aop
compileKernelOp (Barrier fence) = do
  (scopeId, memSemId) <- scopeAndMemorySemantics $ fence == FenceLocal
  wgScopeId <- getConstId $ Const $ IntValue $ Int32Value $ fromIntegral $
                  fromList $ spirvSerialize ScopeWorkgroup
  -- spirv-val output:
  --  "in Vulkan environment Execution Scope is limited to Workgroup and
  --  Subgroup"
  instr $ OpControlBarrier wgScopeId scopeId memSemId
compileKernelOp (MemFence fence) = do
  (scopeId, memSemId) <- scopeAndMemorySemantics $ fence == FenceLocal
  instr $ OpMemoryBarrier scopeId memSemId
compileKernelOp (LocalAlloc v (Count cExp)) = do
  maybeElemPrimType <- getUserStateS $ M.lookup v . shArrayElemTypes
  case maybeElemPrimType of
    Just elemPrimType -> compileLocalAlloc v (untyped cExp) elemPrimType
    Nothing ->
      -- ImpCode will sometimes allocate local memory that is never accessed.
      return ()
compileKernelOp (ErrorSync fence) =
  -- No error support
  compileKernelOp (Barrier fence)

compileCode :: Code KernelOp -> Builder ()
compileCode Skip = return ()
compileCode (c1 :>>: c2)  = compileCode c1 >> compileCode c2
compileCode (For it bound body) = do
  let itInit = Const $ IntValue $ intValue intType (0 :: Integer)
  void $ declareFunctionScalar it itType Nonvolatile (Just itInit)
  loop check (compileCode body) continue
  where
    IntType intType = primExpType bound
    itType = TScalar $ IntType intType
    check = do
      itId <- loadScalar it
      boundId <- compileExp bound
      instrRT (TScalar Bool) $ OpULessThan itId boundId
    continue = modifyScalar it $ \itId -> do
      let oneConst = Const $ IntValue $ intValue intType (1 :: Integer)
      one <- getConstId oneConst
      instrRT itType $ OpIAdd itId one
compileCode (While cond body) =
  loop (compileExp $ untyped cond) (compileCode body) $ return ()
compileCode (DeclareMem v (ScalarSpace elemSubExps elemPrimType)) = do
  let elemType = TScalar elemPrimType
      t = TArray elemType $ Just $ Const $ IntValue $ Int32Value $ numElems
  i <- makeFunctionVar t Nothing
  constInit <- getConstId $ ConstComposite t consts
  instr $ OpStore i constInit Nothing
  addArray v $ ArrayInfo i elemType
  mapM_ (\(idx, varToStore) -> do
    idxId <- getConstId $ Const $ IntValue $ Int64Value idx
    storeMe <- loadScalar varToStore
    storeArray v idxId storeMe Nonvolatile) vars
  where
    numElems = genericLength elemSubExps
    subExpToConst (Constant val) = Const val
    subExpToConst (Var _) = Const $ IntValue $ Int32Value 0
    consts = map subExpToConst elemSubExps
    subExpToVar (i, Var v') = Just (i, v')
    subExpToVar (_, Constant _) = Nothing
    vars = mapMaybe subExpToVar $ zip [0..] elemSubExps
compileCode (DeclareMem _ (Space "local")) = return ()
compileCode (DeclareMem _ _) =
  error "Unsuppored space for DeclareMem"
compileCode (DeclareScalar v vol t) = unless (t == Cert) $
  void $ declareFunctionScalar v (TScalar t) vol Nothing
compileCode (DeclareArray _v _space _t _contents) =
  -- Is this ever even used in kernel code?
  error "DeclareArray unsupported"
compileCode (Allocate _ _ _) =
  -- We cannot dynamically allocate or free memory in SPIR-V shaders (same for
  -- CUDA and OpenCL).
  -- Local memory is declared with a KernelOp (LocalAlloc)
  error "Cannot use Allocate in SPIR-V shaders"
compileCode (Free _ _) =
  -- We cannot dynamically allocate or free memory in SPIR-V shaders (same for
  -- CUDA and OpenCL), but Futhark still inserts these in some programs.
  return ()
compileCode (Copy _ _ _ _ _ _ _) =
  error "Copy unsupported"
compileCode (Write v (Count idxExp) _ _ vol writeExp) = do
  -- We can safely ignore the PrimType in Write, as we have already verified
  -- that arrays are only accessed with a single element type.
  idx <- compileExp $ untyped idxExp
  writeMe <- compileExp writeExp
  storeArray v idx writeMe vol
compileCode (SetScalar v e) = do
  storeMe <- compileExp e
  storeScalar v storeMe
compileCode (SetMem _ _ _) = error "SetMem unsupported"
compileCode (Call [res] fn argExps) = do
  args <- mapM onArgExp argExps
  t <- varType <$> getScalar res
  case t of
    TScalar primType -> do
      fnRes <- compileFunCall (pretty fn) args primType
      storeScalar res fnRes
    _ -> error "Bad type"
  where
    onArgExp (MemArg _) = error "MemArgs unsupported"
    onArgExp (ExpArg e) = compileExp e
compileCode (Call _ _ _) = error "Call used in an unsupported manner"
compileCode (If condExp tbranch fbranch) =
  if_ (compileExp $ untyped condExp) (compileCode tbranch) (compileCode fbranch)
compileCode (Assert _ _ _) = return ()
compileCode (Comment _ c) = compileCode c
compileCode (DebugPrint _ _) = return ()
compileCode (Op op) = compileKernelOp op

declareInterfaceVar :: KernelUse -> Builder ()
declareInterfaceVar (ScalarUse v primType) = do
  let t = TScalar primType
  (i, bindingId) <- makeInterfaceVar t
  let arg = ValueKArg (LeafExp (ScalarVar v) primType) primType
  modifyUserState $
    \s -> s { shInterfaceVars = (arg, bindingId) : shInterfaceVars s }
  addScalar v $ ScalarInfo i t Nonvolatile
declareInterfaceVar (MemoryUse v) = do
  -- Assume every MemoryUse is actually used
  primType <- getUserStateS $
    fromMaybe (error $ "Unused MemoryUse: " ++ show v) .
      M.lookup v . shArrayElemTypes
  when (primType == Bool) $ error "Bool in interface vars not supported"
  let elemType = TScalar primType
  (i, bindingId) <- makeInterfaceVar (TArray elemType Nothing)
  let arg = MemKArg v
  modifyUserState $ \s ->
    s { shInterfaceVars = (arg, bindingId) : shInterfaceVars s }
  addArray v $ ArrayInfo i elemType
declareInterfaceVar (ConstUse v kce@(LeafExp (SizeConst _) _)) = do
  let t = TScalar $ primExpType kce
  specId <- newSpecId
  constId <- getConstId $ SpecConst (IntValue $ Int64Value 0x0BADBADBADBADBAD) specId
  addFutConst v $ FutConstInfo constId t
  let specConst = SpecConstKernelExp v kce
  modifyUserState $ \s ->
    s { shSpecConsts = (specConst, specId) : shSpecConsts s }
declareInterfaceVar (ConstUse _ _) =
  -- If we were to support @KernelConstExp@s that weren't just a single Name,
  -- we would need to do some OpSpecConstantOp magic
  error "Unsupported KernelConstExp"

compileKernel :: Kernel -> Builder ()
compileKernel k = do
  mapM_ declareInterfaceVar (kernelUses k)
  compileCode (kernelBody k)
  return ()

getArrayElementTypes :: ArrayAccesses -> M.Map VName PrimType
getArrayElementTypes = M.fromList . concatMap toMapEntries
  where
    toMapEntries (s1, s2) = toMapEntries' (S.toList s1, S.toList s2)
    toMapEntries' (_, []) = [] -- Unused arrays
    toMapEntries' (names, [t]) = map (, t) names
    toMapEntries' (_, _) =
      error "Array(s) accessed with more than one element type"

kernelToShader' :: Kernel -> (SingleEntryShader, [(EntryPointArg, Word32)], [(SpecConstExp, Word32)])
kernelToShader' k =
  let arrayAccesses = getArrayAccesses $ kernelBody k
      arrayElemTypes = getArrayElementTypes arrayAccesses
      s = ImpCodeToShader
                { shVars = M.empty,
                  shInterfaceVars = [],
                  shSpecConsts = [],
                  shArrayElemTypes = arrayElemTypes
                }
      name = pretty $ kernelName k
      exp1 = ValueExp $ IntValue $ Int32Value 1
      (s', shader, (wgX, wgY, wgZ)) = runBuilder name (compileKernel k) s
      wgSizeExps = kernelGroupSize k ++ repeat exp1
      wgSpecConsts = zip (map SpecConstExp wgSizeExps) [wgX, wgY, wgZ]
      eArgs = map (\(arg, bId) -> (arg, fromBindingId bId)) $ shInterfaceVars s'
      specConsts = map (\(sp, spId) -> (sp, fromSpecId spId)) $ shSpecConsts s' ++ wgSpecConsts
      seShader = SEShader name (length eArgs) shader
  in (seShader, eArgs, (specConsts))

kernelToShader :: Kernel -> (SingleEntryShader, [EntryPointArg], [SpecConstExp])
kernelToShader k =
  let (seShader, eArgs, specConsts) = kernelToShader' k
  in (seShader, fromAssocList eArgs, fromAssocList specConsts)
  where
    fromAssocList :: [(a, Word32)] -> [a]
    fromAssocList = fromAssocList' 0 . sortBy (compare `on` snd)
    fromAssocList' :: Word32 -> [(a, Word32)] -> [a]
    fromAssocList' n ((x, y) : xs)
      | n == y = x : fromAssocList' (n + 1) xs
      | otherwise = error "fromAssocList mismatch"
    fromAssocList' _ [] = []
