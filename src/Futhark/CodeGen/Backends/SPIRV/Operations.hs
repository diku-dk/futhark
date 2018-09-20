module Futhark.CodeGen.Backends.SPIRV.Operations 
  ( opCapability
  , opMemoryModel
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
  , opLogicalOr
  , opLogicalAnd
  , opULessThan
  , opSLessThan
  , opULessThanEqual
  , opSLessThanEqual
  , opShiftRightLogical
  , opShiftRightArithmetic
  , opShiftLeftLogical
  , opBitwiseOr
  , opBitwiseXor
  , opBitwiseAnd
  , opNot
  , opLoopMerge
  , opSelectionMerge
  , opLabel
  , opBranch
  , opBranchConditional
  ) where

import Data.Word
import Data.Bits

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

cBitwiseAddressing :: Word32
cBitwiseAddressing = 0

cSimpleMemoryModel :: Word32
cSimpleMemoryModel = 0

-- | SPIR-V header

genHeader :: Word32 -> [Word32]
genHeader maxId =
  [ cMagicNumber
  , cVersion
  , cGenerator
  , maxId + 1 -- Bound
  , cSchema
  ] ++
  opCapability ++
  opMemoryModel

-- | SPIR-V opcodes

opcCapability :: Word32
opcCapability = 17

opcMemoryModel :: Word32
opcMemoryModel = 14

opcConvertFToU :: Word32
opcConvertFToU = 109

opcConvertFToS :: Word32
opcConvertFToS = 110

opcConvertSToF :: Word32
opcConvertSToF = 111

opcConvertUToF :: Word32
opcConvertUToF = 112

opcUConvert :: Word32
opcUConvert = 113

opcSConvert :: Word32
opcSConvert = 114

opcFConvert :: Word32
opcFConvert = 115

opcIAdd :: Word32
opcIAdd = 128

opcFAdd :: Word32
opcFAdd = 129

opcISub :: Word32
opcISub = 130

opcFSub :: Word32
opcFSub = 131

opcIMul :: Word32
opcIMul = 132

opcFMul :: Word32
opcFMul = 133

opcUDiv :: Word32
opcUDiv = 134

opcSDiv :: Word32
opcSDiv = 135

opcFDiv :: Word32
opcFDiv = 136

opcUMod :: Word32
opcUMod = 137

opcSRem :: Word32
opcSRem = 138

opcSMod :: Word32
opcSMod = 139

opcLogicalOr :: Word32
opcLogicalOr = 166

opcLogicalAnd :: Word32
opcLogicalAnd = 167

opcULessThan :: Word32
opcULessThan = 176

opcSLessThan :: Word32
opcSLessThan = 177

opcULessThanEqual :: Word32
opcULessThanEqual = 178

opcSLessThanEqual :: Word32
opcSLessThanEqual = 179

opcShiftRightLogical :: Word32
opcShiftRightLogical = 194

opcShiftRightArithmetic :: Word32
opcShiftRightArithmetic = 195

opcShiftLeftLogical :: Word32
opcShiftLeftLogical = 196

opcBitwiseOr :: Word32
opcBitwiseOr = 197

opcBitwiseXor :: Word32
opcBitwiseXor = 198

opcBitwiseAnd :: Word32
opcBitwiseAnd = 199

opcNot :: Word32
opcNot = 200

opcLoopMerge :: Word32
opcLoopMerge = 246

opcSelectionMerge :: Word32
opcSelectionMerge = 247

opcLabel :: Word32
opcLabel = 248

opcBranch :: Word32
opcBranch = 249

opcBranchConditional :: Word32
opcBranchConditional = 250

-- | SPIR-V operations

makeOperation :: Word32 -> [Word32] -> [Word32]
makeOperation opcode args =
  let opLen = fromIntegral $ length args + 1
      opcodeAndFlag = shift opLen 16 .|. opcode
  in opcodeAndFlag : args

opCapability :: [Word32]
opCapability = makeOperation opcCapability [cShaderCapability]

opMemoryModel :: [Word32]
opMemoryModel = makeOperation opcMemoryModel [cBitwiseAddressing, cSimpleMemoryModel]

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

opLogicalOr :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLogicalOr op1_id op2_id t_id r_id = makeOperation opcLogicalOr [t_id, r_id, op1_id, op2_id]

opLogicalAnd :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opLogicalAnd op1_id op2_id t_id r_id = makeOperation opcLogicalAnd [t_id, r_id, op1_id, op2_id]

opULessThan :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opULessThan op1_id op2_id t_id r_id = makeOperation opcULessThan [t_id, r_id, op1_id, op2_id]

opSLessThan :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opSLessThan op1_id op2_id t_id r_id = makeOperation opcSLessThan [t_id, r_id, op1_id, op2_id]

opULessThanEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opULessThanEqual op1_id op2_id t_id r_id = makeOperation opcULessThan [t_id, r_id, op1_id, op2_id]

opSLessThanEqual :: Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
opSLessThanEqual op1_id op2_id t_id r_id = makeOperation opcSLessThan [t_id, r_id, op1_id, op2_id]

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