module Futhark.CodeGen.Backends.SPIRV.Operations 
  ( GLSLInstr
  , encodeString
  , glslRound
  , glslFAbs
  , glslSAbs
  , glslFSign
  , glslSSign
  , glslSin
  , glslCos
  , glslTan
  , glslASin
  , glslACos
  , glslATan
  , glslATan2
  , glslPow
  , glslLog
  , glslExp
  , glslLog2
  , glslSqrt
  , glslFMin
  , glslUMin
  , glslSMin
  , glslFMax
  , glslUMax
  , glslSMax
  , cMemoryAccessNone
  , cMemoryAccessVolatile
  , cSimpleMemoryModel
  , cGLSLMemoryModel
  , cShaderCapability
  , cAddressesCapability
  , cFloat64Capability
  , cInt64Capability
  , cInt64AtomicsCapability
  , cInt16Capability
  , cInt8Capability
  , cVariablePointersStorageBufferCapability
  , cStorageBuffer8BitAccessCapability
  , cScopeDevice
  , cScopeWorkgroup
  , cStorageClassStorageBuffer
  , cStorageClassUniform
  , cStorageClassWorkgroup
  , cStorageClassCrossWorkgroup
  , cStorageClassPrivate
  , cStorageClassUniformConstant
  , cStorageClassFunction
  , cStorageClassInput
  , cMemorySemanticsAcquireRelease
  , cMemorySemanticsSequentiallyConsistent
  , cMemorySemanticsUniformMemory
  , cMemorySemanticsWorkgroupMemory
  , cFunctionControlNone
  , cNoSignedness
  , cExecutionModelGLCompute
  , cExecutionModeLocalSize
  , cDecorationSpecId
  , cDecorationBlock
  , cDecorationBufferBlock
  , cDecorationArrayStride
  , cDecorationBuiltin
  , cDecorationBinding
  , cDecorationDescriptorSet
  , cDecorationOffset
  , cBuiltinNumWorkgroups
  , cBuiltinWorkgroupSize
  , cBuiltinWorkgroupId
  , cBuiltinLocalInvocationId
  , cBuiltinGlobalInvocationId
  , genHeader
  , opExtension
  , opExtInstImport
  , opExtInst
  , opMemoryModel
  , opEntryPoint
  , opExecutionMode
  , opCapability
  , opTypeVoid
  , opTypeBool
  , opTypeInt
  , opTypeFloat
  , opTypeVector
  , opTypeArray
  , opTypeRuntimeArray
  , opTypeStruct
  , opTypePointer
  , opTypeFunction
  , opConstantTrue
  , opConstantFalse
  , opConstant
  , opSpecConstantFalse
  , opSpecConstant
  , opSpecConstantComposite
  , opFunction
  , opFunctionEnd
  , opVariable
  , opLoad
  , opStore
  , opAccessChain
  , opDecorate
  , opMemberDecorate
  , opCompositeExtract
  , opConvertFToU
  , opConvertFToS
  , opConvertSToF
  , opConvertUToF
  , opUConvert
  , opSConvert
  , opFConvert
  , opConvertPtrToU
  , opConvertUToPtr
  , opBitcast
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
  , opIsNan
  , opIsInf
  , opLogicalEqual
  , opLogicalOr
  , opSelect
  , opIEqual
  , opFOrdEqual
  , opLogicalAnd
  , opLogicalNot
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
  , opAtomicExchange
  , opAtomicCompareExchange
  , opAtomicIAdd
  , opAtomicSMin
  , opAtomicUMin
  , opAtomicSMax
  , opAtomicUMax
  , opAtomicAnd
  , opAtomicOr
  , opAtomicXor
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
encodeString s = encodeString' $ s ++ [chr 0]
-- | ^ String must be null-terminated

encodeString' :: String -> [Word32]
encodeString' [] = []
encodeString' (c1:c2:c3:c4:cs) =
  let ws = map (fromIntegral . ord) [c1,c2,c3,c4]
      bs = zipWith (\w i -> w `shift` (8 * i)) ws [0..3]
      wd = foldl (.|.) 0 bs
  in wd : encodeString' cs
encodeString' cs = encodeString' $ (++) cs $ replicate (4 - length cs) $ chr 0

-- | SPIR-V constants
cMagicNumber :: Word32
cMagicNumber = 0x07230203

cVersion :: Word32
cVersion = 0x00010300

cGenerator :: Word32
cGenerator = 0

cSchema :: Word32
cSchema = 0

cShaderCapability :: Word32
cShaderCapability = 1

cAddressesCapability :: Word32
cAddressesCapability = 4

cFloat64Capability :: Word32
cFloat64Capability = 10

cInt64Capability :: Word32
cInt64Capability = 11

cInt64AtomicsCapability :: Word32
cInt64AtomicsCapability = 12

cInt16Capability :: Word32
cInt16Capability = 22

cInt8Capability :: Word32
cInt8Capability = 39

cVariablePointersStorageBufferCapability :: Word32
cVariablePointersStorageBufferCapability = 4441

cStorageBuffer8BitAccessCapability :: Word32
cStorageBuffer8BitAccessCapability = 4448

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

cScopeDevice :: Word32
cScopeDevice = 1

cScopeWorkgroup :: Word32
cScopeWorkgroup = 2

cStorageClassUniformConstant :: Word32
cStorageClassUniformConstant = 0

cStorageClassInput :: Word32
cStorageClassInput = 1

cStorageClassUniform :: Word32
cStorageClassUniform = 2

cStorageClassWorkgroup :: Word32
cStorageClassWorkgroup = 4

cStorageClassCrossWorkgroup :: Word32
cStorageClassCrossWorkgroup = 5

cStorageClassPrivate :: Word32
cStorageClassPrivate = 6

cStorageClassFunction :: Word32
cStorageClassFunction = 7

cStorageClassStorageBuffer :: Word32
cStorageClassStorageBuffer = 12

cMemorySemanticsAcquireRelease :: Word32
cMemorySemanticsAcquireRelease = 8

cMemorySemanticsSequentiallyConsistent :: Word32
cMemorySemanticsSequentiallyConsistent = 16

cMemorySemanticsUniformMemory :: Word32
cMemorySemanticsUniformMemory = 64

cMemorySemanticsWorkgroupMemory :: Word32
cMemorySemanticsWorkgroupMemory = 256

cFunctionControlNone :: Word32
cFunctionControlNone = 0

cNoSignedness :: Word32
cNoSignedness = 0

cExecutionModelGLCompute :: Word32
cExecutionModelGLCompute = 5

cExecutionModeLocalSize :: Word32
cExecutionModeLocalSize = 17

cDecorationSpecId :: Word32
cDecorationSpecId = 1

cDecorationBlock :: Word32
cDecorationBlock = 2

cDecorationBufferBlock :: Word32
cDecorationBufferBlock = 3

cDecorationArrayStride :: Word32
cDecorationArrayStride = 6

cDecorationBuiltin :: Word32
cDecorationBuiltin = 11

cDecorationBinding :: Word32
cDecorationBinding = 33

cDecorationDescriptorSet :: Word32
cDecorationDescriptorSet = 34

cDecorationOffset :: Word32
cDecorationOffset = 35

cBuiltinNumWorkgroups :: Word32
cBuiltinNumWorkgroups = 24

cBuiltinWorkgroupSize :: Word32
cBuiltinWorkgroupSize = 25

cBuiltinWorkgroupId :: Word32
cBuiltinWorkgroupId = 26

cBuiltinLocalInvocationId :: Word32
cBuiltinLocalInvocationId = 27

cBuiltinGlobalInvocationId :: Word32
cBuiltinGlobalInvocationId = 28

-- | GLSL instructions

glslRound :: GLSLInstr
glslRound = 1

glslFAbs :: GLSLInstr
glslFAbs = 4

glslSAbs :: GLSLInstr
glslSAbs = 5

glslFSign :: GLSLInstr
glslFSign = 6

glslSSign :: GLSLInstr
glslSSign = 7

glslSin :: GLSLInstr
glslSin = 13

glslCos :: GLSLInstr
glslCos = 14

glslTan :: GLSLInstr
glslTan = 15

glslASin :: GLSLInstr
glslASin = 16

glslACos :: GLSLInstr
glslACos = 17

glslATan :: GLSLInstr
glslATan = 18

glslATan2 :: GLSLInstr
glslATan2 = 25

glslPow :: GLSLInstr
glslPow = 26

glslLog :: GLSLInstr
glslLog = 28

glslExp :: GLSLInstr
glslExp = 29

glslLog2 :: GLSLInstr
glslLog2 = 30

glslSqrt :: GLSLInstr
glslSqrt = 31

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

genHeader :: Word32 -> [Word32]
genHeader max_id =
  [ cMagicNumber
  , cVersion
  , cGenerator
  , max_id + 1 -- Bound
  , cSchema
  ]
  

-- | SPIR-V opcodes

opcExtension :: SPIRVInstr
opcExtension = 10

opcExtInstImport :: SPIRVInstr
opcExtInstImport = 11

opcExtInst :: SPIRVInstr
opcExtInst = 12

opcMemoryModel :: SPIRVInstr
opcMemoryModel = 14

opcEntryPoint :: SPIRVInstr
opcEntryPoint = 15

opcExecutionMode :: SPIRVInstr
opcExecutionMode = 16

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

opcTypeStruct :: SPIRVInstr
opcTypeStruct = 30

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

opcSpecConstantFalse :: SPIRVInstr
opcSpecConstantFalse = 49

opcSpecConstant :: SPIRVInstr
opcSpecConstant = 50

opcSpecConstantComposite :: SPIRVInstr
opcSpecConstantComposite = 51

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

opcDecorate :: SPIRVInstr
opcDecorate = 71

opcMemberDecorate :: SPIRVInstr
opcMemberDecorate = 72

opcCompositeExtract :: SPIRVInstr
opcCompositeExtract = 81

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

opcConvertPtrToU :: SPIRVInstr
opcConvertPtrToU = 117

opcConvertUToPtr :: SPIRVInstr
opcConvertUToPtr = 120

opcBitcast :: SPIRVInstr
opcBitcast = 124

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

opcIsNan :: SPIRVInstr
opcIsNan = 156

opcIsInf :: SPIRVInstr
opcIsInf = 157

opcLogicalEqual :: SPIRVInstr
opcLogicalEqual = 164

opcLogicalOr :: SPIRVInstr
opcLogicalOr = 166

opcLogicalAnd :: SPIRVInstr
opcLogicalAnd = 167

opcLogicalNot :: SPIRVInstr
opcLogicalNot = 168

opcSelect :: SPIRVInstr
opcSelect = 169

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

opcAtomicExchange :: SPIRVInstr
opcAtomicExchange = 229

opcAtomicCompareExchange :: SPIRVInstr
opcAtomicCompareExchange = 230

opcAtomicIAdd :: SPIRVInstr
opcAtomicIAdd = 234

opcAtomicSMin :: SPIRVInstr
opcAtomicSMin = 236

opcAtomicUMin :: SPIRVInstr
opcAtomicUMin = 237

opcAtomicSMax :: SPIRVInstr
opcAtomicSMax = 238

opcAtomicUMax :: SPIRVInstr
opcAtomicUMax = 239

opcAtomicAnd :: SPIRVInstr
opcAtomicAnd = 240

opcAtomicOr :: SPIRVInstr
opcAtomicOr = 241

opcAtomicXor :: SPIRVInstr
opcAtomicXor = 242

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

opExtension :: [Word32] -> [Word32]
opExtension = makeOperation opcExtension

opExtInstImport :: [Word32] -> Word32 -> [Word32]
opExtInstImport ext_name r_id = makeOperation opcExtInstImport $ r_id : ext_name

opExtInst :: GLSLInstr -> [Word32] -> Word32 -> Word32 -> Word32 -> [Word32]
opExtInst instr ops ext t_id r_id = makeOperation opcExtInst $ [t_id, r_id, ext, instr] ++ ops

opExecutionMode :: Word32 -> Word32 -> [Word32] -> [Word32]
opExecutionMode entry mode lits = makeOperation opcExecutionMode $ [entry, mode] ++ lits
  
opCapability :: Word32 -> [Word32]
opCapability cap = makeOperation opcCapability [cap]

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

opTypeStruct :: [Word32] -> Word32 -> [Word32]
opTypeStruct t_ids r_id = makeOperation opcTypeStruct $ r_id : t_ids

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

opSpecConstantFalse :: Word32 -> Word32 -> [Word32]
opSpecConstantFalse t_id r_id = makeOperation opcSpecConstantFalse [t_id, r_id]

opSpecConstant :: [Word32] -> Word32 -> Word32 -> [Word32]
opSpecConstant literals t_id r_id = makeOperation opcSpecConstant $ [t_id, r_id] ++ literals

opSpecConstantComposite :: [Word32] -> Word32 -> Word32 -> [Word32]
opSpecConstantComposite constits t_id r_id = makeOperation opcSpecConstantComposite $ [t_id, r_id] ++ constits

opFunction :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opFunction func_ctrl func_t_id t_id r_id = makeOperation opcFunction [t_id, r_id, func_ctrl, func_t_id]

opFunctionEnd :: [Word32]
opFunctionEnd = makeOperation opcFunctionEnd []

opMemoryModel :: Word32 -> [Word32]
opMemoryModel mem_model = makeOperation opcMemoryModel [cLogicalAddressing, mem_model]

opEntryPoint :: Word32 -> Word32 -> [Word32] -> [Word32] -> [Word32]
opEntryPoint exec_model entry_id name inputs = makeOperation opcEntryPoint $ [exec_model, entry_id] ++ name ++ inputs

opVariable :: Word32 -> Word32 -> Word32 -> [Word32]
opVariable storage_class t_id r_id = makeOperation opcVariable [t_id, r_id, storage_class]

opLoad :: Word32 -> [Word32] -> Word32 -> Word32 -> [Word32]
opLoad ptr_id mem t_id r_id = makeOperation opcLoad $ [t_id, r_id, ptr_id] ++ mem

opStore :: Word32 -> Word32 -> [Word32] -> [Word32]
opStore to_id from_id mem_access = makeOperation opcStore $ [to_id, from_id] ++ mem_access

opAccessChain :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAccessChain src_id ind_id t_id r_id = makeOperation opcAccessChain [t_id, r_id, src_id, ind_id]

opDecorate :: Word32 -> Word32 -> [Word32] -> [Word32]
opDecorate target_id dec lits = makeOperation opcDecorate $ [target_id, dec] ++ lits

opMemberDecorate :: Word32 -> Word32 -> Word32 -> [Word32] -> [Word32]
opMemberDecorate struct_id member dec lits = makeOperation opcMemberDecorate $ [struct_id, member, dec] ++ lits

opCompositeExtract :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opCompositeExtract comp_id i_id t_id r_id = makeOperation opcCompositeExtract [t_id, r_id, comp_id, i_id]

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

opConvertPtrToU :: Word32 -> Word32 -> Word32 -> [Word32]
opConvertPtrToU ptr_id t_id r_id = makeOperation opcConvertPtrToU [t_id, r_id, ptr_id]

opConvertUToPtr :: Word32 -> Word32 -> Word32 -> [Word32]
opConvertUToPtr val_id t_id r_id = makeOperation opcConvertUToPtr [t_id, r_id, val_id]

opBitcast :: Word32 -> Word32 -> Word32 -> [Word32]
opBitcast op_id t_id r_id = makeOperation opcBitcast [t_id, r_id, op_id]

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

opIsNan :: Word32 -> Word32 -> Word32 -> [Word32]
opIsNan op_id t_id r_id = makeOperation opcIsNan [t_id, r_id, op_id]

opIsInf :: Word32 -> Word32 -> Word32 -> [Word32]
opIsInf op_id t_id r_id = makeOperation opcIsInf [t_id, r_id, op_id]

opLogicalEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLogicalEqual op1_id op2_id t_id r_id = makeOperation opcLogicalEqual [t_id, r_id, op1_id, op2_id]

opLogicalOr :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLogicalOr op1_id op2_id t_id r_id = makeOperation opcLogicalOr [t_id, r_id, op1_id, op2_id]

opSelect :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opSelect cond_id op1_id op2_id t_id r_id = makeOperation opcSelect [t_id, r_id, cond_id, op1_id, op2_id]

opIEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opIEqual op1_id op2_id t_id r_id = makeOperation opcIEqual [t_id, r_id, op1_id, op2_id]

opLogicalAnd :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLogicalAnd op1_id op2_id t_id r_id = makeOperation opcLogicalAnd [t_id, r_id, op1_id, op2_id]

opLogicalNot :: Word32 -> Word32 -> Word32 -> [Word32]
opLogicalNot op_id t_id r_id = makeOperation opcLogicalNot [t_id, r_id, op_id]

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

opAtomicExchange :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicExchange ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicExchange [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

opAtomicCompareExchange :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicCompareExchange ptr_id scope_id eq_sem_id neq_sem_id val_id cmp_id t_id r_id =
  makeOperation opcAtomicCompareExchange [t_id, r_id, ptr_id, scope_id, eq_sem_id, neq_sem_id, val_id, cmp_id]

opAtomicIAdd :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicIAdd ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicIAdd [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

opAtomicSMin :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicSMin ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicSMin [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

opAtomicUMin :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicUMin  ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicUMin [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

opAtomicSMax :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicSMax ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicSMax [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

opAtomicUMax :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicUMax ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicUMax [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

opAtomicAnd :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicAnd ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicAnd [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

opAtomicOr :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicOr ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicOr [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

opAtomicXor :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opAtomicXor ptr_id scope_id sem_id val_id t_id r_id =
  makeOperation opcAtomicXor [t_id, r_id, ptr_id, scope_id, sem_id, val_id]

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