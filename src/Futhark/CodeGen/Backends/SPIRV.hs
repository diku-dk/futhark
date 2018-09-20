module Futhark.CodeGen.Backends.SPIRV
  ( runCompilerM
  , CompilerState
  , newCompilerState
  , compileCode
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import qualified Data.Map.Strict as M
import Data.Bits
import Data.Word

import Futhark.CodeGen.Backends.SPIRV.Operations
import Futhark.CodeGen.ImpCode

type ExprInfo = (Word32, PrimType)

data CompilerState = CompilerState {
    compCurrentMaxId :: Word32
  , compVarRefs :: M.Map VName ExprInfo
  , compTypeRefs :: M.Map PrimType Word32
  , compConstRefs :: M.Map PrimValue Word32
  , compResult :: [Word32]
  }

newCompilerState :: CompilerState
newCompilerState = CompilerState { compCurrentMaxId = 0
                                 , compVarRefs = M.empty
                                 , compTypeRefs = M.empty
                                 , compConstRefs = M.empty
                                 , compResult = []
                                 }

type CompilerM = State CompilerState

runCompilerM :: CompilerState -> CompilerM a -> (a, CompilerState)
runCompilerM cs comp = runState comp cs

newId :: CompilerM Word32
newId = do
  modify $ \s -> s { compCurrentMaxId = compCurrentMaxId s + 1 }
  s <- get
  return $ compCurrentMaxId s

getTypeId :: PrimType -> CompilerM Word32
getTypeId t = do
  s <- get
  case (M.!?) (compTypeRefs s) t of
    Just a  -> return a
    Nothing -> do
      t_id <- newId
      put $ s { compTypeRefs = M.insert t t_id $ compTypeRefs s }
      return t_id

getConstId :: PrimValue -> CompilerM Word32
getConstId v = do
  s <- get
  case (M.!?) (compConstRefs s) v of
    Just a  -> return a
    Nothing -> do
      v_id <- newId
      put $ s { compConstRefs = M.insert v v_id $ compConstRefs s }
      return v_id

getVarInfo :: VName -> CompilerM ExprInfo
getVarInfo v = do
  s <- get
  return $ (M.!) (compVarRefs s) v

appendCode :: [Word32] -> CompilerM ()
appendCode code = modify $ \s -> s { compResult = compResult s ++ code }

liftReturnOp :: PrimType -> (Word32 -> Word32 -> [Word32]) -> CompilerM ExprInfo
liftReturnOp t f = do
  t_id <- getTypeId t
  ret_id <- newId
  appendCode $ f t_id ret_id
  return (ret_id, t)

insertLabel :: Word32 -> CompilerM ()
insertLabel id = appendCode $ opLabel id

insertBranch :: Word32 -> CompilerM ()
insertBranch target_id = appendCode $ opBranch target_id

insertBranchConditional :: Word32 -> Word32 -> Word32 -> CompilerM ()
insertBranchConditional cond_id true_id false_id =
  appendCode $ opBranchConditional cond_id true_id false_id

insertLoopMerge :: Word32 -> Word32 -> CompilerM ()
insertLoopMerge merge_id continue_id = appendCode $ opLoopMerge merge_id continue_id

compileLeaf :: ExpLeaf -> CompilerM ExprInfo
compileLeaf (ScalarVar src) = getVarInfo src
compileLeaf (Index src (Count iexp) restype (Space space) vol) = return (0, Bool) -- TODO: Fix
compileLeaf (SizeOf t) = return (0, Bool) -- TODO: Fix

compileExp :: Exp -> CompilerM ExprInfo
compileExp = compilePrimExp compileLeaf

compilePrimExp :: (v -> CompilerM ExprInfo) -> PrimExp v -> CompilerM ExprInfo
compilePrimExp f (LeafExp v _) = f v
compilePrimExp _ (ValueExp val) = do
  id <- getConstId val
  let t = primValueType val
  return (id, t)
compilePrimExp f (UnOpExp uop x) = do
  (x_id, _) <- compilePrimExp f x
  let t = unOpType uop
  case uop of
    Complement{} -> liftReturnOp t $ opNot x_id
    Not{}        -> liftReturnOp t $ opNot x_id
    Abs{}        -> return (0, Bool) -- TODO: Fix
    FAbs{}       -> return (0, Bool) -- TODO: Fix
    SSignum{}    -> return (0, Bool) -- TODO: Fix
    USignum{}    -> return (0, Bool) -- TODO: Fix
compilePrimExp f (CmpOpExp cmp x y) = do
  (x_id, _) <- compilePrimExp f x
  (y_id, _) <- compilePrimExp f y
  case cmp of
    CmpEq{}  -> return (0, Bool) -- TODO: Fix
    FCmpLt{} -> return (0, Bool) -- TODO: Fix
    FCmpLe{} -> return (0, Bool) -- TODO: Fix
    CmpLlt{} -> return (0, Bool) -- TODO: Fix
    CmpLle{} -> return (0, Bool) -- TODO: Fix
    CmpSle{} -> liftReturnOp Bool $ opSLessThanEqual x_id y_id
    CmpSlt{} -> liftReturnOp Bool $ opSLessThan x_id y_id
    CmpUle{} -> liftReturnOp Bool $ opULessThanEqual x_id y_id
    CmpUlt{} -> liftReturnOp Bool $ opULessThan x_id y_id
compilePrimExp f (ConvOpExp conv x) = do
  (x_id, _) <- compilePrimExp f x
  let (_, r_t) = convOpType conv
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
  let t = binOpType bop
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
    SQuot{}  -> return (0, Bool) -- TODO: Fix
    SRem{}   -> liftReturnOp t $ opSRem x_id y_id
    FMin{}   -> return (0, Bool) -- TODO: Fix
    SMin{}   -> return (0, Bool) -- TODO: Fix
    UMin{}   -> return (0, Bool) -- TODO: Fix
    FMax{}   -> return (0, Bool) -- TODO: Fix
    UMax{}   -> return (0, Bool) -- TODO: Fix
    SMax{}   -> return (0, Bool) -- TODO: Fix
    Pow{}    -> return (0, Bool) -- TODO: Fix
    FPow{}   -> return (0, Bool) -- TODO: Fix
    Xor{}    -> liftReturnOp t $ opBitwiseXor x_id y_id
    And{}    -> liftReturnOp t $ opBitwiseAnd x_id y_id
    Or{}     -> liftReturnOp t $ opBitwiseOr x_id y_id
    Shl{}    -> liftReturnOp t $ opShiftLeftLogical x_id y_id
    LShr{}   -> liftReturnOp t $ opShiftRightLogical x_id y_id
    AShr{}   -> liftReturnOp t $ opShiftRightArithmetic x_id y_id
    LogAnd{} -> liftReturnOp t $ opLogicalAnd x_id y_id
    LogOr{}  -> liftReturnOp t $ opLogicalOr x_id y_id
compilePrimExp f (FunExp h args _) = return (0, Bool) -- TODO: Fix

compileCode :: Code op -> CompilerM ()
compileCode Skip = return ()
compileCode (lc :>>: rc) =  compileCode lc >> compileCode rc
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
compileCode (While cond body) = do
  start_id <- newId
  check_id <- newId
  body_id <- newId
  end_id <- newId
  insertLabel start_id
  insertLoopMerge end_id start_id
  insertBranch check_id
  insertLabel check_id
  (cond_id, _) <- compileExp cond
  insertBranchConditional cond_id body_id end_id
  insertLabel body_id
  compileCode body
  insertBranch start_id
  insertLabel end_id
compileCode (For i it bound body) = return () -- TODO: Fix
compileCode (DeclareMem name space) = return () -- TODO: Fix
compileCode (SetScalar dest src) = return () -- TODO: Fix
compileCode (DeclareArray name (Space space) t vs) = return () -- TODO: Fix
compileCode (Allocate name (Count e) space) = return () -- TODO: Fix
compileCode (Free name space) = return () -- TODO: Fix
compileCode (Copy dest (Count destoffset) destspace src (Count srcoffset) DefaultSpace (Count size)) = return () -- TODO: Fix
compileCode (Write dest (Count idx) elemtype (Space space) vol elemexp) = return () -- TODO: Fix
compileCode (SetScalar name exp) = return () -- TODO: Fix
compileCode (SetMem dest src space) = return () -- TODO: Fix
compileCode (Call dests fname args) = return () -- TODO: Fix
compileCode (Assert e (ErrorMsg parts) (loc, locs)) = return () -- TODO: Fix
compileCode (Comment _ c) = compileCode c
-- ^ SPIR-V does not support comments
compileCode (DebugPrint s t e) = return () -- TODO: Fix
compileCode op = return () -- TODO: Fix
