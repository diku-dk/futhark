module Futhark.CodeGen.Backends.SPIRV.Operations 
  ( GLSLInstr
  , encodeString
  , glslFAbs
  , glslSAbs
  , glslFSign
  , glslSSign
  , glslPow
  , glslFMin
  , glslUMin
  , glslSMin
  , glslFMax
  , glslUMax
  , glslSMax
  , cMemoryAccessNone
  , cMemoryAccessVolatile
  , cScopeWorkgroup
  , cStorageClassUniform
  , cStorageClassFunction
  , cStorageClassInput
  , cMemorySemanticsAcquireRelease
  , cMemorySemanticsSequentiallyConsistent
  , cFunctionControlNone
  , cNoSignedness
  , cExecutionModelGLCompute
  , genHeader
  , opExtInstImport
  , opExtInst
  , opMemoryModel
  , opEntryPoint
  , opCapability
  , opTypeVoid
  , opTypeBool
  , opTypeInt
  , opTypeFloat
  , opTypeVector
  , opTypeArray
  , opTypeRuntimeArray
  , opTypePointer
  , opTypeFunction
  , opConstantTrue
  , opConstantFalse
  , opConstant
  , opFunction
  , opFunctionEnd
  , opVariable
  , opLoad
  , opStore
  , opAccessChain
  , opConvertFToU
  , opConvertFToS
  , opConvertSToF
  , opConvertUToF
  , opUConvert
  , opSConvert
  , opFConvert
  , opIAdd
  , opFAdd
  , opISub
  , opFSub
  , opIMul
  , opFMul
  , opUDiv
  , opSDiv
  , opFDiv
  , opUMod
  , opSRem
  , opSMod
  , opLogicalEqual
  , opLogicalOr
  , opIEqual
  , opFOrdEqual
  , opLogicalAnd
  , opULessThan
  , opSLessThan
  , opFOrdLessThan
  , opULessThanEqual
  , opSLessThanEqual
  , opFOrdLessThanEqual
  , opShiftRightLogical
  , opShiftRightArithmetic
  , opShiftLeftLogical
  , opBitwiseOr
  , opBitwiseXor
  , opBitwiseAnd
  , opNot
  , opControlBarrier
  , opMemoryBarrier
  , opLoopMerge
  , opSelectionMerge
  , opLabel
  , opBranch
  , opBranchConditional
  , opReturn
  ) where

import Data.Word
import Data.Bits
import Data.Char

type GLSLInstr = Word32
type SPIRVInstr = Word32

-- | Aux
encodeString :: String -> [Word32]
encodeString [] = []
encodeString (c1:c2:c3:c4:cs) =
  let ws = map (fromIntegral . ord) [c1,c2,c3,c4]
      bs = zipWith (\w i -> w `shift` (8 * i)) ws [0..3]
      wd = foldl (.|.) 0 bs
  in wd : encodeString cs
encodeString cs = encodeString $ (++) cs $ replicate (4 - length cs) $ chr 0

-- | SPIR-V constants
cMagicNumber :: Word32
cMagicNumber = 0x07230203

cVersion :: Word32
cVersion = 0x00010000

cGenerator :: Word32
cGenerator = 0

cSchema :: Word32
cSchema = 0

cShaderCapability :: Word32
cShaderCapability = 1

cLogicalAddressing :: Word32
cLogicalAddressing = 0

cSimpleMemoryModel :: Word32
cSimpleMemoryModel = 0

cGLSLMemoryModel :: Word32
cGLSLMemoryModel = 1

cMemoryAccessNone :: Word32
cMemoryAccessNone = 0

cMemoryAccessVolatile :: Word32
cMemoryAccessVolatile = 0

cScopeWorkgroup :: Word32
cScopeWorkgroup = 2

cStorageClassUniform :: Word32
cStorageClassUniform = 2

cStorageClassFunction :: Word32
cStorageClassFunction = 7

cStorageClassInput :: Word32
cStorageClassInput = 1

cMemorySemanticsAcquireRelease :: Word32
cMemorySemanticsAcquireRelease = 4

cMemorySemanticsSequentiallyConsistent :: Word32
cMemorySemanticsSequentiallyConsistent = 16

cFunctionControlNone :: Word32
cFunctionControlNone = 0

cNoSignedness :: Word32
cNoSignedness = 0

cExecutionModelGLCompute :: Word32
cExecutionModelGLCompute = 5

-- | GLSL instructions

glslFAbs :: GLSLInstr
glslFAbs = 4

glslSAbs :: GLSLInstr
glslSAbs = 5

glslFSign :: GLSLInstr
glslFSign = 6

glslSSign :: GLSLInstr
glslSSign = 7

glslPow :: GLSLInstr
glslPow = 26

glslFMin :: GLSLInstr
glslFMin = 37

glslUMin :: GLSLInstr
glslUMin = 38

glslSMin :: GLSLInstr
glslSMin = 39

glslFMax :: GLSLInstr
glslFMax = 40

glslUMax :: GLSLInstr
glslUMax = 41

glslSMax :: GLSLInstr
glslSMax = 42

-- | SPIR-V header

genHeader :: Word32 -> Maybe Word32 -> [Word32]
genHeader max_id glsl_id =
  [ cMagicNumber
  , cVersion
  , cGenerator
  , max_id + 1 -- Bound
  , cSchema
  ] ++
  opCapability ++
  case glsl_id of
    Nothing  -> opMemoryModel cSimpleMemoryModel
    Just gid -> opMemoryModel cGLSLMemoryModel ++ opExtInstImport (encodeString "GLSL.std.450") gid

-- | SPIR-V opcodes

opcExtInstImport :: SPIRVInstr
opcExtInstImport = 11

opcExtInst :: SPIRVInstr
opcExtInst = 12

opcMemoryModel :: SPIRVInstr
opcMemoryModel = 14

opcEntryPoint :: SPIRVInstr
opcEntryPoint = 15

opcCapability :: SPIRVInstr
opcCapability = 17

opcTypeVoid :: SPIRVInstr
opcTypeVoid = 19

opcTypeBool :: SPIRVInstr
opcTypeBool = 20

opcTypeInt :: SPIRVInstr
opcTypeInt = 21

opcTypeFloat :: SPIRVInstr
opcTypeFloat = 22

opcTypeVector :: SPIRVInstr
opcTypeVector = 23

opcTypeArray :: SPIRVInstr
opcTypeArray = 28

opcTypeRuntimeArray :: SPIRVInstr
opcTypeRuntimeArray = 29

opcTypePointer :: SPIRVInstr
opcTypePointer = 32

opcTypeFunction :: SPIRVInstr
opcTypeFunction = 33

opcConstantTrue :: SPIRVInstr
opcConstantTrue = 41

opcConstantFalse :: SPIRVInstr
opcConstantFalse = 42

opcConstant :: SPIRVInstr
opcConstant = 43

opcFunction :: SPIRVInstr
opcFunction = 54

opcFunctionEnd :: SPIRVInstr
opcFunctionEnd = 56

opcVariable :: SPIRVInstr
opcVariable = 59

opcLoad :: SPIRVInstr
opcLoad = 61

opcStore :: SPIRVInstr
opcStore = 62

opcAccessChain :: SPIRVInstr
opcAccessChain = 65

opcConvertFToU :: SPIRVInstr
opcConvertFToU = 109

opcConvertFToS :: SPIRVInstr
opcConvertFToS = 110

opcConvertSToF :: SPIRVInstr
opcConvertSToF = 111

opcConvertUToF :: SPIRVInstr
opcConvertUToF = 112

opcUConvert :: SPIRVInstr
opcUConvert = 113

opcSConvert :: SPIRVInstr
opcSConvert = 114

opcFConvert :: SPIRVInstr
opcFConvert = 115

opcIAdd :: SPIRVInstr
opcIAdd = 128

opcFAdd :: SPIRVInstr
opcFAdd = 129

opcISub :: SPIRVInstr
opcISub = 130

opcFSub :: SPIRVInstr
opcFSub = 131

opcIMul :: SPIRVInstr
opcIMul = 132

opcFMul :: SPIRVInstr
opcFMul = 133

opcUDiv :: SPIRVInstr
opcUDiv = 134

opcSDiv :: SPIRVInstr
opcSDiv = 135

opcFDiv :: SPIRVInstr
opcFDiv = 136

opcUMod :: SPIRVInstr
opcUMod = 137

opcSRem :: SPIRVInstr
opcSRem = 138

opcSMod :: SPIRVInstr
opcSMod = 139

opcLogicalEqual :: SPIRVInstr
opcLogicalEqual = 164

opcLogicalOr :: SPIRVInstr
opcLogicalOr = 166

opcLogicalAnd :: SPIRVInstr
opcLogicalAnd = 167

opcIEqual :: SPIRVInstr
opcIEqual = 170

opcULessThan :: SPIRVInstr
opcULessThan = 176

opcSLessThan :: SPIRVInstr
opcSLessThan = 177

opcULessThanEqual :: SPIRVInstr
opcULessThanEqual = 178

opcSLessThanEqual :: SPIRVInstr
opcSLessThanEqual = 179

opcFOrdEqual :: SPIRVInstr
opcFOrdEqual = 180

opcFOrdLessThan :: SPIRVInstr
opcFOrdLessThan = 184

opcFOrdLessThanEqual :: SPIRVInstr
opcFOrdLessThanEqual = 188

opcShiftRightLogical :: SPIRVInstr
opcShiftRightLogical = 194

opcShiftRightArithmetic :: SPIRVInstr
opcShiftRightArithmetic = 195

opcShiftLeftLogical :: SPIRVInstr
opcShiftLeftLogical = 196

opcBitwiseOr :: SPIRVInstr
opcBitwiseOr = 197

opcBitwiseXor :: SPIRVInstr
opcBitwiseXor = 198

opcBitwiseAnd :: SPIRVInstr
opcBitwiseAnd = 199

opcNot :: SPIRVInstr
opcNot = 200

opcControlBarrier :: SPIRVInstr
opcControlBarrier = 224

opcMemoryBarrier :: SPIRVInstr
opcMemoryBarrier = 225

opcLoopMerge :: SPIRVInstr
opcLoopMerge = 246

opcSelectionMerge :: SPIRVInstr
opcSelectionMerge = 247

opcLabel :: SPIRVInstr
opcLabel = 248

opcBranch :: SPIRVInstr
opcBranch = 249

opcBranchConditional :: SPIRVInstr
opcBranchConditional = 250

opcReturn :: SPIRVInstr
opcReturn = 253

-- | SPIR-V operations

makeOperation :: SPIRVInstr -> [Word32] -> [Word32]
makeOperation opcode args =
  let opLen = fromIntegral $ length args + 1
      opcodeAndFlag = shift opLen 16 .|. opcode
  in opcodeAndFlag : args

opExtInstImport :: [Word32] -> Word32 -> [Word32]
opExtInstImport ext_name r_id = makeOperation opcExtInstImport $ r_id : ext_name

opExtInst :: GLSLInstr -> [Word32] -> Word32 -> Word32 -> Word32 -> [Word32]
opExtInst instr ops ext t_id r_id = makeOperation opcExtInst $ [t_id, r_id, ext, instr] ++ ops
  
opCapability :: [Word32]
opCapability = makeOperation opcCapability [cShaderCapability]

opTypeVoid :: Word32 -> [Word32]
opTypeVoid r_id = makeOperation opcTypeVoid [r_id]

opTypeBool :: Word32 -> [Word32]
opTypeBool r_id = makeOperation opcTypeBool [r_id]

opTypeInt :: Word32 -> Word32 -> Word32 -> [Word32]
opTypeInt width sign r_id = makeOperation opcTypeInt [r_id, width, sign]

opTypeFloat :: Word32 -> Word32 -> [Word32]
opTypeFloat width r_id = makeOperation opcTypeFloat [r_id, width]

opTypeVector :: Word32 -> Word32 -> Word32 -> [Word32]
opTypeVector elem_id len r_id = makeOperation opcTypeVector [r_id, elem_id, len]

opTypeArray :: Word32 -> Word32 -> Word32 -> [Word32]
opTypeArray elem_id len r_id = makeOperation opcTypeArray [r_id, elem_id, len]

opTypeRuntimeArray :: Word32 -> Word32 -> [Word32]
opTypeRuntimeArray elem_id r_id = makeOperation opcTypeRuntimeArray [r_id, elem_id]

opTypePointer :: Word32 -> Word32 -> Word32 -> [Word32]
opTypePointer storage t_id r_id = makeOperation opcTypePointer [r_id, storage, t_id]

opTypeFunction :: Word32 -> Word32 -> [Word32]
opTypeFunction rt_id r_id = makeOperation opcTypeFunction [r_id, rt_id]

opConstantTrue :: Word32 -> Word32 -> [Word32]
opConstantTrue t_id r_id = makeOperation opcConstantTrue [t_id, r_id]

opConstantFalse :: Word32 -> Word32 -> [Word32]
opConstantFalse t_id r_id = makeOperation opcConstantFalse [t_id, r_id]

opConstant :: [Word32] -> Word32 -> Word32 -> [Word32]
opConstant literals t_id r_id = makeOperation opcConstant $ [t_id, r_id] ++ literals

opFunction :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFunction func_ctrl func_t_id t_id r_id = makeOperation opcFunction [t_id, r_id, func_ctrl, func_t_id]

opFunctionEnd :: [Word32]
opFunctionEnd = makeOperation opcFunctionEnd []

opMemoryModel :: Word32 -> [Word32]
opMemoryModel mem_model = makeOperation opcMemoryModel [cLogicalAddressing, mem_model]

opEntryPoint :: Word32 -> Word32 -> [Word32] -> [Word32]
opEntryPoint exec_model entry_id name = makeOperation opcEntryPoint $ [exec_model, entry_id] ++ name

opVariable :: Word32 -> Word32 -> Word32 -> [Word32]
opVariable storage_class t_id r_id = makeOperation opcVariable [t_id, r_id, storage_class]

opLoad :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLoad ptr_id mem t_id r_id = makeOperation opcLoad [t_id, r_id, ptr_id, mem]

opStore :: Word32 -> Word32 -> [Word32]
opStore to_id from_id = makeOperation opcStore [to_id, from_id]

opAccessChain :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAccessChain src_id ind_id t_id r_id = makeOperation opcAccessChain [t_id, r_id, src_id, ind_id]

opConvertFToU :: Word32 -> Word32 -> Word32 -> [Word32]
opConvertFToU op_id t_id r_id = makeOperation opcConvertFToU [t_id, r_id, op_id]

opConvertFToS :: Word32 -> Word32 -> Word32 -> [Word32]
opConvertFToS op_id t_id r_id = makeOperation opcConvertFToS [t_id, r_id, op_id]

opConvertSToF :: Word32 -> Word32 -> Word32 -> [Word32]
opConvertSToF op_id t_id r_id = makeOperation opcConvertSToF [t_id, r_id, op_id]

opConvertUToF :: Word32 -> Word32 -> Word32 -> [Word32]
opConvertUToF op_id t_id r_id = makeOperation opcConvertUToF [t_id, r_id, op_id]

opUConvert :: Word32 -> Word32 -> Word32 -> [Word32]
opUConvert op_id t_id r_id = makeOperation opcUConvert [t_id, r_id, op_id]

opSConvert :: Word32 -> Word32 -> Word32 -> [Word32]
opSConvert op_id t_id r_id = makeOperation opcSConvert [t_id, r_id, op_id]

opFConvert :: Word32 -> Word32 -> Word32 -> [Word32]
opFConvert op_id t_id r_id = makeOperation opcFConvert [t_id, r_id, op_id]

opIAdd :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opIAdd op1_id op2_id t_id r_id = makeOperation opcIAdd [t_id, r_id, op1_id, op2_id]

opFAdd :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFAdd op1_id op2_id t_id r_id = makeOperation opcFAdd [t_id, r_id, op1_id, op2_id]

opISub :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opISub op1_id op2_id t_id r_id = makeOperation opcISub [t_id, r_id, op1_id, op2_id]

opFSub :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFSub op1_id op2_id t_id r_id = makeOperation opcFSub [t_id, r_id, op1_id, op2_id]

opIMul :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opIMul op1_id op2_id t_id r_id = makeOperation opcIMul [t_id, r_id, op1_id, op2_id]

opFMul :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFMul op1_id op2_id t_id r_id = makeOperation opcFMul [t_id, r_id, op1_id, op2_id]

opUDiv :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opUDiv op1_id op2_id t_id r_id = makeOperation opcUDiv [t_id, r_id, op1_id, op2_id]

opSDiv :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opSDiv op1_id op2_id t_id r_id = makeOperation opcSDiv [t_id, r_id, op1_id, op2_id]

opFDiv :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFDiv op1_id op2_id t_id r_id = makeOperation opcFDiv [t_id, r_id, op1_id, op2_id]

opUMod :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opUMod op1_id op2_id t_id r_id = makeOperation opcUMod [t_id, r_id, op1_id, op2_id]

opSRem :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opSRem op1_id op2_id t_id r_id = makeOperation opcSRem [t_id, r_id, op1_id, op2_id]

opSMod :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opSMod op1_id op2_id t_id r_id = makeOperation opcSMod [t_id, r_id, op1_id, op2_id]

opLogicalEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLogicalEqual op1_id op2_id t_id r_id = makeOperation opcLogicalEqual [t_id, r_id, op1_id, op2_id]

opLogicalOr :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLogicalOr op1_id op2_id t_id r_id = makeOperation opcLogicalOr [t_id, r_id, op1_id, op2_id]

opIEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opIEqual op1_id op2_id t_id r_id = makeOperation opcIEqual [t_id, r_id, op1_id, op2_id]

opLogicalAnd :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLogicalAnd op1_id op2_id t_id r_id = makeOperation opcLogicalAnd [t_id, r_id, op1_id, op2_id]

opULessThan :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opULessThan op1_id op2_id t_id r_id = makeOperation opcULessThan [t_id, r_id, op1_id, op2_id]

opSLessThan :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opSLessThan op1_id op2_id t_id r_id = makeOperation opcSLessThan [t_id, r_id, op1_id, op2_id]

opULessThanEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opULessThanEqual op1_id op2_id t_id r_id = makeOperation opcULessThanEqual [t_id, r_id, op1_id, op2_id]

opSLessThanEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opSLessThanEqual op1_id op2_id t_id r_id = makeOperation opcSLessThanEqual [t_id, r_id, op1_id, op2_id]

opFOrdEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFOrdEqual op1_id op2_id t_id r_id = makeOperation opcFOrdEqual [t_id, r_id, op1_id, op2_id]

opFOrdLessThan :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFOrdLessThan op1_id op2_id t_id r_id = makeOperation opcFOrdLessThan [t_id, r_id, op1_id, op2_id]

opFOrdLessThanEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFOrdLessThanEqual op1_id op2_id t_id r_id = makeOperation opcFOrdLessThanEqual [t_id, r_id, op1_id, op2_id]

opShiftRightLogical :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opShiftRightLogical op1_id op2_id t_id r_id = makeOperation opcShiftRightLogical [t_id, r_id, op1_id, op2_id]

opShiftRightArithmetic :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opShiftRightArithmetic op1_id op2_id t_id r_id = makeOperation opcShiftRightArithmetic [t_id, r_id, op1_id, op2_id]

opShiftLeftLogical :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opShiftLeftLogical op1_id op2_id t_id r_id = makeOperation opcShiftLeftLogical [t_id, r_id, op1_id, op2_id]

opBitwiseOr :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opBitwiseOr op1_id op2_id t_id r_id = makeOperation opcBitwiseOr [t_id, r_id, op1_id, op2_id]

opBitwiseXor :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opBitwiseXor op1_id op2_id t_id r_id = makeOperation opcBitwiseXor [t_id, r_id, op1_id, op2_id]

opBitwiseAnd :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opBitwiseAnd op1_id op2_id t_id r_id = makeOperation opcBitwiseAnd [t_id, r_id, op1_id, op2_id]

opNot :: Word32 -> Word32 -> Word32 -> [Word32]
opNot op_id t_id r_id = makeOperation opcNot [t_id, r_id, op_id]

opControlBarrier :: Word32 -> Word32 -> Word32 -> [Word32]
opControlBarrier exec_scope mem_scope sem = makeOperation opcControlBarrier [exec_scope, mem_scope, sem]

opMemoryBarrier :: Word32 -> Word32 -> [Word32]
opMemoryBarrier mem_scope sem = makeOperation opcMemoryBarrier [mem_scope, sem]

opLoopMerge :: Word32 -> Word32 -> [Word32]
opLoopMerge merge_id continue_id = makeOperation opcLoopMerge [merge_id, continue_id, 0]

opSelectionMerge :: Word32 -> [Word32]
opSelectionMerge merge_id = makeOperation opcSelectionMerge [merge_id, 0]

opLabel :: Word32 -> [Word32]
opLabel r_id = makeOperation opcLabel [r_id]

opBranch :: Word32 -> [Word32]
opBranch target_id = makeOperation opcBranch [target_id]

opBranchConditional :: Word32 -> Word32 -> Word32 -> [Word32]
opBranchConditional cond_id t_id f_id = makeOperation opcBranchConditional [cond_id, t_id, f_id]

opReturn :: [Word32]
opReturn = makeOperation opcReturn []