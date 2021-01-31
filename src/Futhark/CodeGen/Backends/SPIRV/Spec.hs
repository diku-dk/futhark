-- This file was generated automatically
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Futhark.CodeGen.Backends.SPIRV.Spec
  ( module Futhark.CodeGen.Backends.SPIRV.Spec
  )
where
import Data.Word
import Data.List
import Data.Bits
import Futhark.IR.Primitive (floatToWord, doubleToWord)
import Data.Function (on)
import Codec.Binary.UTF8.String (encode)
import Data.Maybe

class SPIRVInfo a where
  infoCapabilities :: a -> [Capability]
  infoCapabilities _ = []
  infoExtensions :: a -> [String]
  infoExtensions _ = []
  infoVersionRange :: a -> SPIRVVersionRange
  infoVersionRange _ = SPIRVVersionRange (Nothing, Nothing)

instance SPIRVInfo a => SPIRVInfo [a] where
  infoCapabilities = concat . map infoCapabilities
  infoExtensions = concat . map infoExtensions
  infoVersionRange = mconcat . map infoVersionRange

class SPIRVSerialize a where
  spirvSerialize :: a -> [Word32]

instance SPIRVSerialize a => SPIRVSerialize [a] where
  spirvSerialize = concatMap spirvSerialize

instance SPIRVSerialize a => SPIRVSerialize (Maybe a) where
  spirvSerialize = maybe [] spirvSerialize

data SPIRVNumber
  = -- The boolean denotes signedness
    SPIRVInt Bool SPIRVWord
  | -- "A literal 32-bit float is always one operand, directly holding a 32-bit
    -- IEEE 754 floating-point representation."
    SPIRVFloat32 Float
  | -- "A literal 64-bit float is always two operands, directly holding a
    -- 64-bit IEEE 754 representation. The low-order 32 bits appear in the
    -- first operand.
    SPIRVFloat64 Double
  deriving (Show, Eq, Ord)
instance SPIRVInfo SPIRVNumber where
  -- default
instance SPIRVSerialize SPIRVNumber where
  spirvSerialize (SPIRVInt _ w) = spirvSerialize w
  spirvSerialize (SPIRVFloat32 f) = [floatToWord f]
  spirvSerialize (SPIRVFloat64 d) = spirvSerialize . Word64 $ doubleToWord d

data SPIRVWord
  = Word8 Word8
  | Word16 Word16
  | Word32 Word32
  | Word64 Word64
  deriving (Show, Eq, Ord)
instance SPIRVInfo SPIRVWord where
  -- default
instance SPIRVSerialize SPIRVWord where
  -- See https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#_instructions_2
  spirvSerialize (Word8 w) = [fromIntegral w]
  spirvSerialize (Word16 w) = [fromIntegral w]
  spirvSerialize (Word32 w) = [w]
  spirvSerialize (Word64 w) = map fromIntegral [w .&. 0xFFFFFFFF, w `shiftR` 32]

newtype Id = Id Word32
  deriving (Show, Eq, Ord)
instance SPIRVInfo Id where
  infoCapabilities _ = []
  infoExtensions _ = []
  infoVersionRange _ = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize Id where
  spirvSerialize (Id w) = [w]

newtype SPIRVString = String String
  deriving (Show, Eq, Ord)

instance SPIRVInfo SPIRVString where
  infoCapabilities _ = []
  infoExtensions _ = []
  infoVersionRange _ = SPIRVVersionRange (Nothing, Nothing)

instance SPIRVSerialize SPIRVString where
  spirvSerialize (String s) = ser . map fromIntegral $ encode s ++ [0]
    where
      ser (x0:x1:x2:x3:xs) =
        (x0 .|. x1 `shiftL` 8 .|. x2 `shiftL` 16 .|.  x3 `shiftL` 24) : ser xs
      ser [] = []
      ser xs = ser $ xs ++ replicate (4 - length xs) 0

data SPIRVVersion = SPIRVVersion Int Int
  deriving (Show)

instance Eq SPIRVVersion where
  (SPIRVVersion ma1 mi1) == (SPIRVVersion ma2 mi2) = ma1 == ma2 && mi1 == mi2

instance Ord SPIRVVersion where
  (SPIRVVersion ma1 mi1) <= (SPIRVVersion ma2 mi2) =
    ma1 < ma2 || (ma1 == ma2 && mi1 <= mi2)

instance SPIRVSerialize SPIRVVersion where
  spirvSerialize (SPIRVVersion ma mi) = [ma' `shiftL` 16 .|. mi' `shiftL` 8]
    where
      ma' = fromIntegral (fromIntegral ma :: Word8)
      mi' = fromIntegral (fromIntegral mi :: Word8)

newtype SPIRVVersionRange = SPIRVVersionRange (Maybe SPIRVVersion, Maybe SPIRVVersion)
instance Semigroup SPIRVVersionRange where
  (SPIRVVersionRange (f1, l1)) <> (SPIRVVersionRange (f2, l2)) =
    SPIRVVersionRange (f max f1 f2, f min l1 l2)
    where
      f choose (Just a) (Just b) = Just $ choose a b
      f choose Nothing x = x
      f choose x Nothing = x

instance Monoid SPIRVVersionRange where
  mempty = SPIRVVersionRange (Nothing, Nothing)

data SPIRVHeader = SPIRVHeader
  { headerVersion :: SPIRVVersion
  , headerGenerator :: Word16
  , headerGeneratorVersion ::  Word16
  , headerIdBound :: Word32
  } deriving (Show, Eq)

instance SPIRVSerialize SPIRVHeader where
  spirvSerialize hdr =
    [ magic,
      version,
      generator `shiftL` 16 .|. generatorVersion,
      headerIdBound hdr,
      0 -- Reserved
    ]
    where
      [version] = spirvSerialize $ headerVersion hdr
      generator = fromIntegral $ headerGenerator hdr
      generatorVersion = fromIntegral $ headerGeneratorVersion hdr

-- Helper
makeHeader :: SPIRVVersion -> Word32 -> SPIRVHeader
makeHeader version idBound =
  SPIRVHeader
    { headerVersion = version
    , headerGenerator = 0
    , headerGeneratorVersion = 0
    , headerIdBound = idBound
    }

trueThenFalse :: [Bool] -> Bool
trueThenFalse = all not . dropWhile id

makeInstructionHeader :: Int -> Int -> Word32
makeInstructionHeader opcode wcount = wcount' `shiftL` 16 .|. opcode'
  where
    opcode' = fromIntegral (fromIntegral opcode :: Word16)
    wcount' = fromIntegral (fromIntegral wcount :: Word16)

type RInstruction = IdResult -> Instruction
type RTInstruction = IdResultType -> IdResult -> Instruction

-- Header ends here
----------------------------------------

magic :: Word32
magic = 119734787
data ImageOperandsBit = ImageOperandsNone | ImageOperandsBias IdRef | ImageOperandsLod IdRef | ImageOperandsGrad IdRef IdRef | ImageOperandsConstOffset IdRef | ImageOperandsOffset IdRef | ImageOperandsConstOffsets IdRef | ImageOperandsSample IdRef | ImageOperandsMinLod IdRef | ImageOperandsMakeTexelAvailable IdScope | ImageOperandsMakeTexelAvailableKHR IdScope | ImageOperandsMakeTexelVisible IdScope | ImageOperandsMakeTexelVisibleKHR IdScope | ImageOperandsNonPrivateTexel | ImageOperandsNonPrivateTexelKHR | ImageOperandsVolatileTexel | ImageOperandsVolatileTexelKHR | ImageOperandsSignExtend | ImageOperandsZeroExtend
  deriving (Show, Eq, Ord)
newtype ImageOperands = ImageOperands [ImageOperandsBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo ImageOperandsBit where
  infoCapabilities (ImageOperandsNone) = []
  infoCapabilities (ImageOperandsBias x0) = [CapabilityShader]
  infoCapabilities (ImageOperandsLod x0) = []
  infoCapabilities (ImageOperandsGrad x0 x1) = []
  infoCapabilities (ImageOperandsConstOffset x0) = []
  infoCapabilities (ImageOperandsOffset x0) = [CapabilityImageGatherExtended]
  infoCapabilities (ImageOperandsConstOffsets x0) = [CapabilityImageGatherExtended]
  infoCapabilities (ImageOperandsSample x0) = []
  infoCapabilities (ImageOperandsMinLod x0) = [CapabilityMinLod]
  infoCapabilities (ImageOperandsMakeTexelAvailable x0) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ImageOperandsMakeTexelAvailableKHR x0) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ImageOperandsMakeTexelVisible x0) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ImageOperandsMakeTexelVisibleKHR x0) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ImageOperandsNonPrivateTexel) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ImageOperandsNonPrivateTexelKHR) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ImageOperandsVolatileTexel) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ImageOperandsVolatileTexelKHR) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ImageOperandsSignExtend) = []
  infoCapabilities (ImageOperandsZeroExtend) = []
  infoExtensions (ImageOperandsNone) = []
  infoExtensions (ImageOperandsBias x0) = []
  infoExtensions (ImageOperandsLod x0) = []
  infoExtensions (ImageOperandsGrad x0 x1) = []
  infoExtensions (ImageOperandsConstOffset x0) = []
  infoExtensions (ImageOperandsOffset x0) = []
  infoExtensions (ImageOperandsConstOffsets x0) = []
  infoExtensions (ImageOperandsSample x0) = []
  infoExtensions (ImageOperandsMinLod x0) = []
  infoExtensions (ImageOperandsMakeTexelAvailable x0) = []
  infoExtensions (ImageOperandsMakeTexelAvailableKHR x0) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (ImageOperandsMakeTexelVisible x0) = []
  infoExtensions (ImageOperandsMakeTexelVisibleKHR x0) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (ImageOperandsNonPrivateTexel) = []
  infoExtensions (ImageOperandsNonPrivateTexelKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (ImageOperandsVolatileTexel) = []
  infoExtensions (ImageOperandsVolatileTexelKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (ImageOperandsSignExtend) = []
  infoExtensions (ImageOperandsZeroExtend) = []
  infoVersionRange (ImageOperandsNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsBias x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsLod x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsGrad x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsConstOffset x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsOffset x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsConstOffsets x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsSample x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsMinLod x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageOperandsMakeTexelAvailable x0) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ImageOperandsMakeTexelAvailableKHR x0) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ImageOperandsMakeTexelVisible x0) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ImageOperandsMakeTexelVisibleKHR x0) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ImageOperandsNonPrivateTexel) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ImageOperandsNonPrivateTexelKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ImageOperandsVolatileTexel) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ImageOperandsVolatileTexelKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ImageOperandsSignExtend) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (ImageOperandsZeroExtend) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
instance SPIRVInfo ImageOperands where
  infoCapabilities (ImageOperands l) = infoCapabilities l
  infoExtensions (ImageOperands l) = infoExtensions l
  infoVersionRange (ImageOperands l) = infoVersionRange l
instance SPIRVSerialize ImageOperands where
  spirvSerialize (ImageOperands bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: ImageOperandsBit -> (Word32, [Word32])
      serializeBit (ImageOperandsNone) = (0, concat [])
      serializeBit (ImageOperandsBias x0) = (1, concat [spirvSerialize x0])
      serializeBit (ImageOperandsLod x0) = (2, concat [spirvSerialize x0])
      serializeBit (ImageOperandsGrad x0 x1) = (4, concat [spirvSerialize x0, spirvSerialize x1])
      serializeBit (ImageOperandsConstOffset x0) = (8, concat [spirvSerialize x0])
      serializeBit (ImageOperandsOffset x0) = (16, concat [spirvSerialize x0])
      serializeBit (ImageOperandsConstOffsets x0) = (32, concat [spirvSerialize x0])
      serializeBit (ImageOperandsSample x0) = (64, concat [spirvSerialize x0])
      serializeBit (ImageOperandsMinLod x0) = (128, concat [spirvSerialize x0])
      serializeBit (ImageOperandsMakeTexelAvailable x0) = (256, concat [spirvSerialize x0])
      serializeBit (ImageOperandsMakeTexelAvailableKHR x0) = (256, concat [spirvSerialize x0])
      serializeBit (ImageOperandsMakeTexelVisible x0) = (512, concat [spirvSerialize x0])
      serializeBit (ImageOperandsMakeTexelVisibleKHR x0) = (512, concat [spirvSerialize x0])
      serializeBit (ImageOperandsNonPrivateTexel) = (1024, concat [])
      serializeBit (ImageOperandsNonPrivateTexelKHR) = (1024, concat [])
      serializeBit (ImageOperandsVolatileTexel) = (2048, concat [])
      serializeBit (ImageOperandsVolatileTexelKHR) = (2048, concat [])
      serializeBit (ImageOperandsSignExtend) = (4096, concat [])
      serializeBit (ImageOperandsZeroExtend) = (8192, concat [])
data FPFastMathModeBit = FPFastMathModeNone | FPFastMathModeNotNaN | FPFastMathModeNotInf | FPFastMathModeNSZ | FPFastMathModeAllowRecip | FPFastMathModeFast
  deriving (Show, Eq, Ord)
newtype FPFastMathMode = FPFastMathMode [FPFastMathModeBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo FPFastMathModeBit where
  infoCapabilities (FPFastMathModeNone) = []
  infoCapabilities (FPFastMathModeNotNaN) = [CapabilityKernel]
  infoCapabilities (FPFastMathModeNotInf) = [CapabilityKernel]
  infoCapabilities (FPFastMathModeNSZ) = [CapabilityKernel]
  infoCapabilities (FPFastMathModeAllowRecip) = [CapabilityKernel]
  infoCapabilities (FPFastMathModeFast) = [CapabilityKernel]
  infoExtensions (FPFastMathModeNone) = []
  infoExtensions (FPFastMathModeNotNaN) = []
  infoExtensions (FPFastMathModeNotInf) = []
  infoExtensions (FPFastMathModeNSZ) = []
  infoExtensions (FPFastMathModeAllowRecip) = []
  infoExtensions (FPFastMathModeFast) = []
  infoVersionRange (FPFastMathModeNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FPFastMathModeNotNaN) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FPFastMathModeNotInf) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FPFastMathModeNSZ) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FPFastMathModeAllowRecip) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FPFastMathModeFast) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVInfo FPFastMathMode where
  infoCapabilities (FPFastMathMode l) = infoCapabilities l
  infoExtensions (FPFastMathMode l) = infoExtensions l
  infoVersionRange (FPFastMathMode l) = infoVersionRange l
instance SPIRVSerialize FPFastMathMode where
  spirvSerialize (FPFastMathMode bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: FPFastMathModeBit -> (Word32, [Word32])
      serializeBit (FPFastMathModeNone) = (0, concat [])
      serializeBit (FPFastMathModeNotNaN) = (1, concat [])
      serializeBit (FPFastMathModeNotInf) = (2, concat [])
      serializeBit (FPFastMathModeNSZ) = (4, concat [])
      serializeBit (FPFastMathModeAllowRecip) = (8, concat [])
      serializeBit (FPFastMathModeFast) = (16, concat [])
data SelectionControlBit = SelectionControlNone | SelectionControlFlatten | SelectionControlDontFlatten
  deriving (Show, Eq, Ord)
newtype SelectionControl = SelectionControl [SelectionControlBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo SelectionControlBit where
  infoCapabilities (SelectionControlNone) = []
  infoCapabilities (SelectionControlFlatten) = []
  infoCapabilities (SelectionControlDontFlatten) = []
  infoExtensions (SelectionControlNone) = []
  infoExtensions (SelectionControlFlatten) = []
  infoExtensions (SelectionControlDontFlatten) = []
  infoVersionRange (SelectionControlNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SelectionControlFlatten) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SelectionControlDontFlatten) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVInfo SelectionControl where
  infoCapabilities (SelectionControl l) = infoCapabilities l
  infoExtensions (SelectionControl l) = infoExtensions l
  infoVersionRange (SelectionControl l) = infoVersionRange l
instance SPIRVSerialize SelectionControl where
  spirvSerialize (SelectionControl bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: SelectionControlBit -> (Word32, [Word32])
      serializeBit (SelectionControlNone) = (0, concat [])
      serializeBit (SelectionControlFlatten) = (1, concat [])
      serializeBit (SelectionControlDontFlatten) = (2, concat [])
data LoopControlBit = LoopControlNone | LoopControlUnroll | LoopControlDontUnroll | LoopControlDependencyInfinite | LoopControlDependencyLength LiteralInteger | LoopControlMinIterations LiteralInteger | LoopControlMaxIterations LiteralInteger | LoopControlIterationMultiple LiteralInteger | LoopControlPeelCount LiteralInteger | LoopControlPartialCount LiteralInteger
  deriving (Show, Eq, Ord)
newtype LoopControl = LoopControl [LoopControlBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo LoopControlBit where
  infoCapabilities (LoopControlNone) = []
  infoCapabilities (LoopControlUnroll) = []
  infoCapabilities (LoopControlDontUnroll) = []
  infoCapabilities (LoopControlDependencyInfinite) = []
  infoCapabilities (LoopControlDependencyLength x0) = []
  infoCapabilities (LoopControlMinIterations x0) = []
  infoCapabilities (LoopControlMaxIterations x0) = []
  infoCapabilities (LoopControlIterationMultiple x0) = []
  infoCapabilities (LoopControlPeelCount x0) = []
  infoCapabilities (LoopControlPartialCount x0) = []
  infoExtensions (LoopControlNone) = []
  infoExtensions (LoopControlUnroll) = []
  infoExtensions (LoopControlDontUnroll) = []
  infoExtensions (LoopControlDependencyInfinite) = []
  infoExtensions (LoopControlDependencyLength x0) = []
  infoExtensions (LoopControlMinIterations x0) = []
  infoExtensions (LoopControlMaxIterations x0) = []
  infoExtensions (LoopControlIterationMultiple x0) = []
  infoExtensions (LoopControlPeelCount x0) = []
  infoExtensions (LoopControlPartialCount x0) = []
  infoVersionRange (LoopControlNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (LoopControlUnroll) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (LoopControlDontUnroll) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (LoopControlDependencyInfinite) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (LoopControlDependencyLength x0) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (LoopControlMinIterations x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (LoopControlMaxIterations x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (LoopControlIterationMultiple x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (LoopControlPeelCount x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (LoopControlPartialCount x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
instance SPIRVInfo LoopControl where
  infoCapabilities (LoopControl l) = infoCapabilities l
  infoExtensions (LoopControl l) = infoExtensions l
  infoVersionRange (LoopControl l) = infoVersionRange l
instance SPIRVSerialize LoopControl where
  spirvSerialize (LoopControl bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: LoopControlBit -> (Word32, [Word32])
      serializeBit (LoopControlNone) = (0, concat [])
      serializeBit (LoopControlUnroll) = (1, concat [])
      serializeBit (LoopControlDontUnroll) = (2, concat [])
      serializeBit (LoopControlDependencyInfinite) = (4, concat [])
      serializeBit (LoopControlDependencyLength x0) = (8, concat [spirvSerialize x0])
      serializeBit (LoopControlMinIterations x0) = (16, concat [spirvSerialize x0])
      serializeBit (LoopControlMaxIterations x0) = (32, concat [spirvSerialize x0])
      serializeBit (LoopControlIterationMultiple x0) = (64, concat [spirvSerialize x0])
      serializeBit (LoopControlPeelCount x0) = (128, concat [spirvSerialize x0])
      serializeBit (LoopControlPartialCount x0) = (256, concat [spirvSerialize x0])
data FunctionControlBit = FunctionControlNone | FunctionControlInline | FunctionControlDontInline | FunctionControlPure | FunctionControlConst
  deriving (Show, Eq, Ord)
newtype FunctionControl = FunctionControl [FunctionControlBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo FunctionControlBit where
  infoCapabilities (FunctionControlNone) = []
  infoCapabilities (FunctionControlInline) = []
  infoCapabilities (FunctionControlDontInline) = []
  infoCapabilities (FunctionControlPure) = []
  infoCapabilities (FunctionControlConst) = []
  infoExtensions (FunctionControlNone) = []
  infoExtensions (FunctionControlInline) = []
  infoExtensions (FunctionControlDontInline) = []
  infoExtensions (FunctionControlPure) = []
  infoExtensions (FunctionControlConst) = []
  infoVersionRange (FunctionControlNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionControlInline) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionControlDontInline) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionControlPure) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionControlConst) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVInfo FunctionControl where
  infoCapabilities (FunctionControl l) = infoCapabilities l
  infoExtensions (FunctionControl l) = infoExtensions l
  infoVersionRange (FunctionControl l) = infoVersionRange l
instance SPIRVSerialize FunctionControl where
  spirvSerialize (FunctionControl bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: FunctionControlBit -> (Word32, [Word32])
      serializeBit (FunctionControlNone) = (0, concat [])
      serializeBit (FunctionControlInline) = (1, concat [])
      serializeBit (FunctionControlDontInline) = (2, concat [])
      serializeBit (FunctionControlPure) = (4, concat [])
      serializeBit (FunctionControlConst) = (8, concat [])
data MemorySemanticsBit = MemorySemanticsRelaxed | MemorySemanticsNone | MemorySemanticsAcquire | MemorySemanticsRelease | MemorySemanticsAcquireRelease | MemorySemanticsSequentiallyConsistent | MemorySemanticsUniformMemory | MemorySemanticsSubgroupMemory | MemorySemanticsWorkgroupMemory | MemorySemanticsCrossWorkgroupMemory | MemorySemanticsAtomicCounterMemory | MemorySemanticsImageMemory | MemorySemanticsOutputMemory | MemorySemanticsOutputMemoryKHR | MemorySemanticsMakeAvailable | MemorySemanticsMakeAvailableKHR | MemorySemanticsMakeVisible | MemorySemanticsMakeVisibleKHR | MemorySemanticsVolatile
  deriving (Show, Eq, Ord)
newtype MemorySemantics = MemorySemantics [MemorySemanticsBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo MemorySemanticsBit where
  infoCapabilities (MemorySemanticsRelaxed) = []
  infoCapabilities (MemorySemanticsNone) = []
  infoCapabilities (MemorySemanticsAcquire) = []
  infoCapabilities (MemorySemanticsRelease) = []
  infoCapabilities (MemorySemanticsAcquireRelease) = []
  infoCapabilities (MemorySemanticsSequentiallyConsistent) = []
  infoCapabilities (MemorySemanticsUniformMemory) = [CapabilityShader]
  infoCapabilities (MemorySemanticsSubgroupMemory) = []
  infoCapabilities (MemorySemanticsWorkgroupMemory) = []
  infoCapabilities (MemorySemanticsCrossWorkgroupMemory) = []
  infoCapabilities (MemorySemanticsAtomicCounterMemory) = [CapabilityAtomicStorage]
  infoCapabilities (MemorySemanticsImageMemory) = []
  infoCapabilities (MemorySemanticsOutputMemory) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemorySemanticsOutputMemoryKHR) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemorySemanticsMakeAvailable) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemorySemanticsMakeAvailableKHR) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemorySemanticsMakeVisible) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemorySemanticsMakeVisibleKHR) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemorySemanticsVolatile) = [CapabilityVulkanMemoryModel]
  infoExtensions (MemorySemanticsRelaxed) = []
  infoExtensions (MemorySemanticsNone) = []
  infoExtensions (MemorySemanticsAcquire) = []
  infoExtensions (MemorySemanticsRelease) = []
  infoExtensions (MemorySemanticsAcquireRelease) = []
  infoExtensions (MemorySemanticsSequentiallyConsistent) = []
  infoExtensions (MemorySemanticsUniformMemory) = []
  infoExtensions (MemorySemanticsSubgroupMemory) = []
  infoExtensions (MemorySemanticsWorkgroupMemory) = []
  infoExtensions (MemorySemanticsCrossWorkgroupMemory) = []
  infoExtensions (MemorySemanticsAtomicCounterMemory) = []
  infoExtensions (MemorySemanticsImageMemory) = []
  infoExtensions (MemorySemanticsOutputMemory) = []
  infoExtensions (MemorySemanticsOutputMemoryKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (MemorySemanticsMakeAvailable) = []
  infoExtensions (MemorySemanticsMakeAvailableKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (MemorySemanticsMakeVisible) = []
  infoExtensions (MemorySemanticsMakeVisibleKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (MemorySemanticsVolatile) = ["SPV_KHR_vulkan_memory_model"]
  infoVersionRange (MemorySemanticsRelaxed) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsAcquire) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsRelease) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsAcquireRelease) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsSequentiallyConsistent) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsUniformMemory) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsSubgroupMemory) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsWorkgroupMemory) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsCrossWorkgroupMemory) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsAtomicCounterMemory) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsImageMemory) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemorySemanticsOutputMemory) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemorySemanticsOutputMemoryKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemorySemanticsMakeAvailable) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemorySemanticsMakeAvailableKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemorySemanticsMakeVisible) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemorySemanticsMakeVisibleKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemorySemanticsVolatile) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
instance SPIRVInfo MemorySemantics where
  infoCapabilities (MemorySemantics l) = infoCapabilities l
  infoExtensions (MemorySemantics l) = infoExtensions l
  infoVersionRange (MemorySemantics l) = infoVersionRange l
instance SPIRVSerialize MemorySemantics where
  spirvSerialize (MemorySemantics bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: MemorySemanticsBit -> (Word32, [Word32])
      serializeBit (MemorySemanticsRelaxed) = (0, concat [])
      serializeBit (MemorySemanticsNone) = (0, concat [])
      serializeBit (MemorySemanticsAcquire) = (2, concat [])
      serializeBit (MemorySemanticsRelease) = (4, concat [])
      serializeBit (MemorySemanticsAcquireRelease) = (8, concat [])
      serializeBit (MemorySemanticsSequentiallyConsistent) = (16, concat [])
      serializeBit (MemorySemanticsUniformMemory) = (64, concat [])
      serializeBit (MemorySemanticsSubgroupMemory) = (128, concat [])
      serializeBit (MemorySemanticsWorkgroupMemory) = (256, concat [])
      serializeBit (MemorySemanticsCrossWorkgroupMemory) = (512, concat [])
      serializeBit (MemorySemanticsAtomicCounterMemory) = (1024, concat [])
      serializeBit (MemorySemanticsImageMemory) = (2048, concat [])
      serializeBit (MemorySemanticsOutputMemory) = (4096, concat [])
      serializeBit (MemorySemanticsOutputMemoryKHR) = (4096, concat [])
      serializeBit (MemorySemanticsMakeAvailable) = (8192, concat [])
      serializeBit (MemorySemanticsMakeAvailableKHR) = (8192, concat [])
      serializeBit (MemorySemanticsMakeVisible) = (16384, concat [])
      serializeBit (MemorySemanticsMakeVisibleKHR) = (16384, concat [])
      serializeBit (MemorySemanticsVolatile) = (32768, concat [])
data MemoryAccessBit = MemoryAccessNone | MemoryAccessVolatile | MemoryAccessAligned LiteralInteger | MemoryAccessNontemporal | MemoryAccessMakePointerAvailable IdScope | MemoryAccessMakePointerAvailableKHR IdScope | MemoryAccessMakePointerVisible IdScope | MemoryAccessMakePointerVisibleKHR IdScope | MemoryAccessNonPrivatePointer | MemoryAccessNonPrivatePointerKHR
  deriving (Show, Eq, Ord)
newtype MemoryAccess = MemoryAccess [MemoryAccessBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo MemoryAccessBit where
  infoCapabilities (MemoryAccessNone) = []
  infoCapabilities (MemoryAccessVolatile) = []
  infoCapabilities (MemoryAccessAligned x0) = []
  infoCapabilities (MemoryAccessNontemporal) = []
  infoCapabilities (MemoryAccessMakePointerAvailable x0) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemoryAccessMakePointerAvailableKHR x0) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemoryAccessMakePointerVisible x0) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemoryAccessMakePointerVisibleKHR x0) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemoryAccessNonPrivatePointer) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemoryAccessNonPrivatePointerKHR) = [CapabilityVulkanMemoryModel]
  infoExtensions (MemoryAccessNone) = []
  infoExtensions (MemoryAccessVolatile) = []
  infoExtensions (MemoryAccessAligned x0) = []
  infoExtensions (MemoryAccessNontemporal) = []
  infoExtensions (MemoryAccessMakePointerAvailable x0) = []
  infoExtensions (MemoryAccessMakePointerAvailableKHR x0) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (MemoryAccessMakePointerVisible x0) = []
  infoExtensions (MemoryAccessMakePointerVisibleKHR x0) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (MemoryAccessNonPrivatePointer) = []
  infoExtensions (MemoryAccessNonPrivatePointerKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoVersionRange (MemoryAccessNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemoryAccessVolatile) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemoryAccessAligned x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemoryAccessNontemporal) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemoryAccessMakePointerAvailable x0) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemoryAccessMakePointerAvailableKHR x0) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemoryAccessMakePointerVisible x0) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemoryAccessMakePointerVisibleKHR x0) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemoryAccessNonPrivatePointer) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemoryAccessNonPrivatePointerKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
instance SPIRVInfo MemoryAccess where
  infoCapabilities (MemoryAccess l) = infoCapabilities l
  infoExtensions (MemoryAccess l) = infoExtensions l
  infoVersionRange (MemoryAccess l) = infoVersionRange l
instance SPIRVSerialize MemoryAccess where
  spirvSerialize (MemoryAccess bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: MemoryAccessBit -> (Word32, [Word32])
      serializeBit (MemoryAccessNone) = (0, concat [])
      serializeBit (MemoryAccessVolatile) = (1, concat [])
      serializeBit (MemoryAccessAligned x0) = (2, concat [spirvSerialize x0])
      serializeBit (MemoryAccessNontemporal) = (4, concat [])
      serializeBit (MemoryAccessMakePointerAvailable x0) = (8, concat [spirvSerialize x0])
      serializeBit (MemoryAccessMakePointerAvailableKHR x0) = (8, concat [spirvSerialize x0])
      serializeBit (MemoryAccessMakePointerVisible x0) = (16, concat [spirvSerialize x0])
      serializeBit (MemoryAccessMakePointerVisibleKHR x0) = (16, concat [spirvSerialize x0])
      serializeBit (MemoryAccessNonPrivatePointer) = (32, concat [])
      serializeBit (MemoryAccessNonPrivatePointerKHR) = (32, concat [])
data KernelProfilingInfoBit = KernelProfilingInfoNone | KernelProfilingInfoCmdExecTime
  deriving (Show, Eq, Ord)
newtype KernelProfilingInfo = KernelProfilingInfo [KernelProfilingInfoBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo KernelProfilingInfoBit where
  infoCapabilities (KernelProfilingInfoNone) = []
  infoCapabilities (KernelProfilingInfoCmdExecTime) = [CapabilityKernel]
  infoExtensions (KernelProfilingInfoNone) = []
  infoExtensions (KernelProfilingInfoCmdExecTime) = []
  infoVersionRange (KernelProfilingInfoNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (KernelProfilingInfoCmdExecTime) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVInfo KernelProfilingInfo where
  infoCapabilities (KernelProfilingInfo l) = infoCapabilities l
  infoExtensions (KernelProfilingInfo l) = infoExtensions l
  infoVersionRange (KernelProfilingInfo l) = infoVersionRange l
instance SPIRVSerialize KernelProfilingInfo where
  spirvSerialize (KernelProfilingInfo bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: KernelProfilingInfoBit -> (Word32, [Word32])
      serializeBit (KernelProfilingInfoNone) = (0, concat [])
      serializeBit (KernelProfilingInfoCmdExecTime) = (1, concat [])
data SourceLanguage = SourceLanguageUnknown | SourceLanguageESSL | SourceLanguageGLSL | SourceLanguageOpenCL_C | SourceLanguageOpenCL_CPP | SourceLanguageHLSL
  deriving (Show, Eq, Ord)
instance SPIRVInfo SourceLanguage where
  infoCapabilities (SourceLanguageUnknown) = []
  infoCapabilities (SourceLanguageESSL) = []
  infoCapabilities (SourceLanguageGLSL) = []
  infoCapabilities (SourceLanguageOpenCL_C) = []
  infoCapabilities (SourceLanguageOpenCL_CPP) = []
  infoCapabilities (SourceLanguageHLSL) = []
  infoExtensions (SourceLanguageUnknown) = []
  infoExtensions (SourceLanguageESSL) = []
  infoExtensions (SourceLanguageGLSL) = []
  infoExtensions (SourceLanguageOpenCL_C) = []
  infoExtensions (SourceLanguageOpenCL_CPP) = []
  infoExtensions (SourceLanguageHLSL) = []
  infoVersionRange (SourceLanguageUnknown) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SourceLanguageESSL) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SourceLanguageGLSL) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SourceLanguageOpenCL_C) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SourceLanguageOpenCL_CPP) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SourceLanguageHLSL) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize SourceLanguage where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (SourceLanguageUnknown) = (0, concat [])
      ser (SourceLanguageESSL) = (1, concat [])
      ser (SourceLanguageGLSL) = (2, concat [])
      ser (SourceLanguageOpenCL_C) = (3, concat [])
      ser (SourceLanguageOpenCL_CPP) = (4, concat [])
      ser (SourceLanguageHLSL) = (5, concat [])
data ExecutionModel = ExecutionModelVertex | ExecutionModelTessellationControl | ExecutionModelTessellationEvaluation | ExecutionModelGeometry | ExecutionModelFragment | ExecutionModelGLCompute | ExecutionModelKernel
  deriving (Show, Eq, Ord)
instance SPIRVInfo ExecutionModel where
  infoCapabilities (ExecutionModelVertex) = [CapabilityShader]
  infoCapabilities (ExecutionModelTessellationControl) = [CapabilityTessellation]
  infoCapabilities (ExecutionModelTessellationEvaluation) = [CapabilityTessellation]
  infoCapabilities (ExecutionModelGeometry) = [CapabilityGeometry]
  infoCapabilities (ExecutionModelFragment) = [CapabilityShader]
  infoCapabilities (ExecutionModelGLCompute) = [CapabilityShader]
  infoCapabilities (ExecutionModelKernel) = [CapabilityKernel]
  infoExtensions (ExecutionModelVertex) = []
  infoExtensions (ExecutionModelTessellationControl) = []
  infoExtensions (ExecutionModelTessellationEvaluation) = []
  infoExtensions (ExecutionModelGeometry) = []
  infoExtensions (ExecutionModelFragment) = []
  infoExtensions (ExecutionModelGLCompute) = []
  infoExtensions (ExecutionModelKernel) = []
  infoVersionRange (ExecutionModelVertex) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModelTessellationControl) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModelTessellationEvaluation) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModelGeometry) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModelFragment) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModelGLCompute) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModelKernel) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize ExecutionModel where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (ExecutionModelVertex) = (0, concat [])
      ser (ExecutionModelTessellationControl) = (1, concat [])
      ser (ExecutionModelTessellationEvaluation) = (2, concat [])
      ser (ExecutionModelGeometry) = (3, concat [])
      ser (ExecutionModelFragment) = (4, concat [])
      ser (ExecutionModelGLCompute) = (5, concat [])
      ser (ExecutionModelKernel) = (6, concat [])
data AddressingModel = AddressingModelLogical | AddressingModelPhysical32 | AddressingModelPhysical64 | AddressingModelPhysicalStorageBuffer64 | AddressingModelPhysicalStorageBuffer64EXT
  deriving (Show, Eq, Ord)
instance SPIRVInfo AddressingModel where
  infoCapabilities (AddressingModelLogical) = []
  infoCapabilities (AddressingModelPhysical32) = [CapabilityAddresses]
  infoCapabilities (AddressingModelPhysical64) = [CapabilityAddresses]
  infoCapabilities (AddressingModelPhysicalStorageBuffer64) = [CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (AddressingModelPhysicalStorageBuffer64EXT) = [CapabilityPhysicalStorageBufferAddresses]
  infoExtensions (AddressingModelLogical) = []
  infoExtensions (AddressingModelPhysical32) = []
  infoExtensions (AddressingModelPhysical64) = []
  infoExtensions (AddressingModelPhysicalStorageBuffer64) = ["SPV_EXT_physical_storage_buffer", "SPV_KHR_physical_storage_buffer"]
  infoExtensions (AddressingModelPhysicalStorageBuffer64EXT) = ["SPV_EXT_physical_storage_buffer"]
  infoVersionRange (AddressingModelLogical) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (AddressingModelPhysical32) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (AddressingModelPhysical64) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (AddressingModelPhysicalStorageBuffer64) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (AddressingModelPhysicalStorageBuffer64EXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
instance SPIRVSerialize AddressingModel where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (AddressingModelLogical) = (0, concat [])
      ser (AddressingModelPhysical32) = (1, concat [])
      ser (AddressingModelPhysical64) = (2, concat [])
      ser (AddressingModelPhysicalStorageBuffer64) = (5348, concat [])
      ser (AddressingModelPhysicalStorageBuffer64EXT) = (5348, concat [])
data MemoryModel = MemoryModelSimple | MemoryModelGLSL450 | MemoryModelOpenCL | MemoryModelVulkan | MemoryModelVulkanKHR
  deriving (Show, Eq, Ord)
instance SPIRVInfo MemoryModel where
  infoCapabilities (MemoryModelSimple) = [CapabilityShader]
  infoCapabilities (MemoryModelGLSL450) = [CapabilityShader]
  infoCapabilities (MemoryModelOpenCL) = [CapabilityKernel]
  infoCapabilities (MemoryModelVulkan) = [CapabilityVulkanMemoryModel]
  infoCapabilities (MemoryModelVulkanKHR) = [CapabilityVulkanMemoryModel]
  infoExtensions (MemoryModelSimple) = []
  infoExtensions (MemoryModelGLSL450) = []
  infoExtensions (MemoryModelOpenCL) = []
  infoExtensions (MemoryModelVulkan) = []
  infoExtensions (MemoryModelVulkanKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoVersionRange (MemoryModelSimple) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemoryModelGLSL450) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemoryModelOpenCL) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (MemoryModelVulkan) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (MemoryModelVulkanKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
instance SPIRVSerialize MemoryModel where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (MemoryModelSimple) = (0, concat [])
      ser (MemoryModelGLSL450) = (1, concat [])
      ser (MemoryModelOpenCL) = (2, concat [])
      ser (MemoryModelVulkan) = (3, concat [])
      ser (MemoryModelVulkanKHR) = (3, concat [])
data ExecutionMode = ExecutionModeInvocations LiteralInteger | ExecutionModeSpacingEqual | ExecutionModeSpacingFractionalEven | ExecutionModeSpacingFractionalOdd | ExecutionModeVertexOrderCw | ExecutionModeVertexOrderCcw | ExecutionModePixelCenterInteger | ExecutionModeOriginUpperLeft | ExecutionModeOriginLowerLeft | ExecutionModeEarlyFragmentTests | ExecutionModePointMode | ExecutionModeXfb | ExecutionModeDepthReplacing | ExecutionModeDepthGreater | ExecutionModeDepthLess | ExecutionModeDepthUnchanged | ExecutionModeLocalSize LiteralInteger LiteralInteger LiteralInteger | ExecutionModeLocalSizeHint LiteralInteger LiteralInteger LiteralInteger | ExecutionModeInputPoints | ExecutionModeInputLines | ExecutionModeInputLinesAdjacency | ExecutionModeTriangles | ExecutionModeInputTrianglesAdjacency | ExecutionModeQuads | ExecutionModeIsolines | ExecutionModeOutputVertices LiteralInteger | ExecutionModeOutputPoints | ExecutionModeOutputLineStrip | ExecutionModeOutputTriangleStrip | ExecutionModeVecTypeHint LiteralInteger | ExecutionModeContractionOff | ExecutionModeInitializer | ExecutionModeFinalizer | ExecutionModeSubgroupSize LiteralInteger | ExecutionModeSubgroupsPerWorkgroup LiteralInteger | ExecutionModeSubgroupsPerWorkgroupId IdRef | ExecutionModeLocalSizeId IdRef IdRef IdRef | ExecutionModeLocalSizeHintId IdRef | ExecutionModeDenormPreserve LiteralInteger | ExecutionModeDenormFlushToZero LiteralInteger | ExecutionModeSignedZeroInfNanPreserve LiteralInteger | ExecutionModeRoundingModeRTE LiteralInteger | ExecutionModeRoundingModeRTZ LiteralInteger
  deriving (Show, Eq, Ord)
instance SPIRVInfo ExecutionMode where
  infoCapabilities (ExecutionModeInvocations x0) = [CapabilityGeometry]
  infoCapabilities (ExecutionModeSpacingEqual) = [CapabilityTessellation]
  infoCapabilities (ExecutionModeSpacingFractionalEven) = [CapabilityTessellation]
  infoCapabilities (ExecutionModeSpacingFractionalOdd) = [CapabilityTessellation]
  infoCapabilities (ExecutionModeVertexOrderCw) = [CapabilityTessellation]
  infoCapabilities (ExecutionModeVertexOrderCcw) = [CapabilityTessellation]
  infoCapabilities (ExecutionModePixelCenterInteger) = [CapabilityShader]
  infoCapabilities (ExecutionModeOriginUpperLeft) = [CapabilityShader]
  infoCapabilities (ExecutionModeOriginLowerLeft) = [CapabilityShader]
  infoCapabilities (ExecutionModeEarlyFragmentTests) = [CapabilityShader]
  infoCapabilities (ExecutionModePointMode) = [CapabilityTessellation]
  infoCapabilities (ExecutionModeXfb) = [CapabilityTransformFeedback]
  infoCapabilities (ExecutionModeDepthReplacing) = [CapabilityShader]
  infoCapabilities (ExecutionModeDepthGreater) = [CapabilityShader]
  infoCapabilities (ExecutionModeDepthLess) = [CapabilityShader]
  infoCapabilities (ExecutionModeDepthUnchanged) = [CapabilityShader]
  infoCapabilities (ExecutionModeLocalSize x0 x1 x2) = []
  infoCapabilities (ExecutionModeLocalSizeHint x0 x1 x2) = [CapabilityKernel]
  infoCapabilities (ExecutionModeInputPoints) = [CapabilityGeometry]
  infoCapabilities (ExecutionModeInputLines) = [CapabilityGeometry]
  infoCapabilities (ExecutionModeInputLinesAdjacency) = [CapabilityGeometry]
  infoCapabilities (ExecutionModeTriangles) = [CapabilityGeometry, CapabilityTessellation]
  infoCapabilities (ExecutionModeInputTrianglesAdjacency) = [CapabilityGeometry]
  infoCapabilities (ExecutionModeQuads) = [CapabilityTessellation]
  infoCapabilities (ExecutionModeIsolines) = [CapabilityTessellation]
  infoCapabilities (ExecutionModeOutputVertices x0) = [CapabilityGeometry, CapabilityTessellation]
  infoCapabilities (ExecutionModeOutputPoints) = [CapabilityGeometry]
  infoCapabilities (ExecutionModeOutputLineStrip) = [CapabilityGeometry]
  infoCapabilities (ExecutionModeOutputTriangleStrip) = [CapabilityGeometry]
  infoCapabilities (ExecutionModeVecTypeHint x0) = [CapabilityKernel]
  infoCapabilities (ExecutionModeContractionOff) = [CapabilityKernel]
  infoCapabilities (ExecutionModeInitializer) = [CapabilityKernel]
  infoCapabilities (ExecutionModeFinalizer) = [CapabilityKernel]
  infoCapabilities (ExecutionModeSubgroupSize x0) = [CapabilitySubgroupDispatch]
  infoCapabilities (ExecutionModeSubgroupsPerWorkgroup x0) = [CapabilitySubgroupDispatch]
  infoCapabilities (ExecutionModeSubgroupsPerWorkgroupId x0) = [CapabilitySubgroupDispatch]
  infoCapabilities (ExecutionModeLocalSizeId x0 x1 x2) = []
  infoCapabilities (ExecutionModeLocalSizeHintId x0) = [CapabilityKernel]
  infoCapabilities (ExecutionModeDenormPreserve x0) = [CapabilityDenormPreserve]
  infoCapabilities (ExecutionModeDenormFlushToZero x0) = [CapabilityDenormFlushToZero]
  infoCapabilities (ExecutionModeSignedZeroInfNanPreserve x0) = [CapabilitySignedZeroInfNanPreserve]
  infoCapabilities (ExecutionModeRoundingModeRTE x0) = [CapabilityRoundingModeRTE]
  infoCapabilities (ExecutionModeRoundingModeRTZ x0) = [CapabilityRoundingModeRTZ]
  infoExtensions (ExecutionModeInvocations x0) = []
  infoExtensions (ExecutionModeSpacingEqual) = []
  infoExtensions (ExecutionModeSpacingFractionalEven) = []
  infoExtensions (ExecutionModeSpacingFractionalOdd) = []
  infoExtensions (ExecutionModeVertexOrderCw) = []
  infoExtensions (ExecutionModeVertexOrderCcw) = []
  infoExtensions (ExecutionModePixelCenterInteger) = []
  infoExtensions (ExecutionModeOriginUpperLeft) = []
  infoExtensions (ExecutionModeOriginLowerLeft) = []
  infoExtensions (ExecutionModeEarlyFragmentTests) = []
  infoExtensions (ExecutionModePointMode) = []
  infoExtensions (ExecutionModeXfb) = []
  infoExtensions (ExecutionModeDepthReplacing) = []
  infoExtensions (ExecutionModeDepthGreater) = []
  infoExtensions (ExecutionModeDepthLess) = []
  infoExtensions (ExecutionModeDepthUnchanged) = []
  infoExtensions (ExecutionModeLocalSize x0 x1 x2) = []
  infoExtensions (ExecutionModeLocalSizeHint x0 x1 x2) = []
  infoExtensions (ExecutionModeInputPoints) = []
  infoExtensions (ExecutionModeInputLines) = []
  infoExtensions (ExecutionModeInputLinesAdjacency) = []
  infoExtensions (ExecutionModeTriangles) = []
  infoExtensions (ExecutionModeInputTrianglesAdjacency) = []
  infoExtensions (ExecutionModeQuads) = []
  infoExtensions (ExecutionModeIsolines) = []
  infoExtensions (ExecutionModeOutputVertices x0) = []
  infoExtensions (ExecutionModeOutputPoints) = []
  infoExtensions (ExecutionModeOutputLineStrip) = []
  infoExtensions (ExecutionModeOutputTriangleStrip) = []
  infoExtensions (ExecutionModeVecTypeHint x0) = []
  infoExtensions (ExecutionModeContractionOff) = []
  infoExtensions (ExecutionModeInitializer) = []
  infoExtensions (ExecutionModeFinalizer) = []
  infoExtensions (ExecutionModeSubgroupSize x0) = []
  infoExtensions (ExecutionModeSubgroupsPerWorkgroup x0) = []
  infoExtensions (ExecutionModeSubgroupsPerWorkgroupId x0) = []
  infoExtensions (ExecutionModeLocalSizeId x0 x1 x2) = []
  infoExtensions (ExecutionModeLocalSizeHintId x0) = []
  infoExtensions (ExecutionModeDenormPreserve x0) = ["SPV_KHR_float_controls"]
  infoExtensions (ExecutionModeDenormFlushToZero x0) = ["SPV_KHR_float_controls"]
  infoExtensions (ExecutionModeSignedZeroInfNanPreserve x0) = ["SPV_KHR_float_controls"]
  infoExtensions (ExecutionModeRoundingModeRTE x0) = ["SPV_KHR_float_controls"]
  infoExtensions (ExecutionModeRoundingModeRTZ x0) = ["SPV_KHR_float_controls"]
  infoVersionRange (ExecutionModeInvocations x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeSpacingEqual) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeSpacingFractionalEven) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeSpacingFractionalOdd) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeVertexOrderCw) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeVertexOrderCcw) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModePixelCenterInteger) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeOriginUpperLeft) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeOriginLowerLeft) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeEarlyFragmentTests) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModePointMode) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeXfb) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeDepthReplacing) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeDepthGreater) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeDepthLess) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeDepthUnchanged) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeLocalSize x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeLocalSizeHint x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeInputPoints) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeInputLines) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeInputLinesAdjacency) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeTriangles) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeInputTrianglesAdjacency) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeQuads) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeIsolines) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeOutputVertices x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeOutputPoints) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeOutputLineStrip) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeOutputTriangleStrip) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeVecTypeHint x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeContractionOff) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ExecutionModeInitializer) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (ExecutionModeFinalizer) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (ExecutionModeSubgroupSize x0) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (ExecutionModeSubgroupsPerWorkgroup x0) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (ExecutionModeSubgroupsPerWorkgroupId x0) = SPIRVVersionRange (Just (SPIRVVersion 1 2), Nothing)
  infoVersionRange (ExecutionModeLocalSizeId x0 x1 x2) = SPIRVVersionRange (Just (SPIRVVersion 1 2), Nothing)
  infoVersionRange (ExecutionModeLocalSizeHintId x0) = SPIRVVersionRange (Just (SPIRVVersion 1 2), Nothing)
  infoVersionRange (ExecutionModeDenormPreserve x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (ExecutionModeDenormFlushToZero x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (ExecutionModeSignedZeroInfNanPreserve x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (ExecutionModeRoundingModeRTE x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (ExecutionModeRoundingModeRTZ x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
instance SPIRVSerialize ExecutionMode where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (ExecutionModeInvocations x0) = (0, concat [spirvSerialize x0])
      ser (ExecutionModeSpacingEqual) = (1, concat [])
      ser (ExecutionModeSpacingFractionalEven) = (2, concat [])
      ser (ExecutionModeSpacingFractionalOdd) = (3, concat [])
      ser (ExecutionModeVertexOrderCw) = (4, concat [])
      ser (ExecutionModeVertexOrderCcw) = (5, concat [])
      ser (ExecutionModePixelCenterInteger) = (6, concat [])
      ser (ExecutionModeOriginUpperLeft) = (7, concat [])
      ser (ExecutionModeOriginLowerLeft) = (8, concat [])
      ser (ExecutionModeEarlyFragmentTests) = (9, concat [])
      ser (ExecutionModePointMode) = (10, concat [])
      ser (ExecutionModeXfb) = (11, concat [])
      ser (ExecutionModeDepthReplacing) = (12, concat [])
      ser (ExecutionModeDepthGreater) = (14, concat [])
      ser (ExecutionModeDepthLess) = (15, concat [])
      ser (ExecutionModeDepthUnchanged) = (16, concat [])
      ser (ExecutionModeLocalSize x0 x1 x2) = (17, concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2])
      ser (ExecutionModeLocalSizeHint x0 x1 x2) = (18, concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2])
      ser (ExecutionModeInputPoints) = (19, concat [])
      ser (ExecutionModeInputLines) = (20, concat [])
      ser (ExecutionModeInputLinesAdjacency) = (21, concat [])
      ser (ExecutionModeTriangles) = (22, concat [])
      ser (ExecutionModeInputTrianglesAdjacency) = (23, concat [])
      ser (ExecutionModeQuads) = (24, concat [])
      ser (ExecutionModeIsolines) = (25, concat [])
      ser (ExecutionModeOutputVertices x0) = (26, concat [spirvSerialize x0])
      ser (ExecutionModeOutputPoints) = (27, concat [])
      ser (ExecutionModeOutputLineStrip) = (28, concat [])
      ser (ExecutionModeOutputTriangleStrip) = (29, concat [])
      ser (ExecutionModeVecTypeHint x0) = (30, concat [spirvSerialize x0])
      ser (ExecutionModeContractionOff) = (31, concat [])
      ser (ExecutionModeInitializer) = (33, concat [])
      ser (ExecutionModeFinalizer) = (34, concat [])
      ser (ExecutionModeSubgroupSize x0) = (35, concat [spirvSerialize x0])
      ser (ExecutionModeSubgroupsPerWorkgroup x0) = (36, concat [spirvSerialize x0])
      ser (ExecutionModeSubgroupsPerWorkgroupId x0) = (37, concat [spirvSerialize x0])
      ser (ExecutionModeLocalSizeId x0 x1 x2) = (38, concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2])
      ser (ExecutionModeLocalSizeHintId x0) = (39, concat [spirvSerialize x0])
      ser (ExecutionModeDenormPreserve x0) = (4459, concat [spirvSerialize x0])
      ser (ExecutionModeDenormFlushToZero x0) = (4460, concat [spirvSerialize x0])
      ser (ExecutionModeSignedZeroInfNanPreserve x0) = (4461, concat [spirvSerialize x0])
      ser (ExecutionModeRoundingModeRTE x0) = (4462, concat [spirvSerialize x0])
      ser (ExecutionModeRoundingModeRTZ x0) = (4463, concat [spirvSerialize x0])
data StorageClass = StorageClassUniformConstant | StorageClassInput | StorageClassUniform | StorageClassOutput | StorageClassWorkgroup | StorageClassCrossWorkgroup | StorageClassPrivate | StorageClassFunction | StorageClassGeneric | StorageClassPushConstant | StorageClassAtomicCounter | StorageClassImage | StorageClassStorageBuffer | StorageClassPhysicalStorageBuffer | StorageClassPhysicalStorageBufferEXT
  deriving (Show, Eq, Ord)
instance SPIRVInfo StorageClass where
  infoCapabilities (StorageClassUniformConstant) = []
  infoCapabilities (StorageClassInput) = []
  infoCapabilities (StorageClassUniform) = [CapabilityShader]
  infoCapabilities (StorageClassOutput) = [CapabilityShader]
  infoCapabilities (StorageClassWorkgroup) = []
  infoCapabilities (StorageClassCrossWorkgroup) = []
  infoCapabilities (StorageClassPrivate) = [CapabilityShader]
  infoCapabilities (StorageClassFunction) = []
  infoCapabilities (StorageClassGeneric) = [CapabilityGenericPointer]
  infoCapabilities (StorageClassPushConstant) = [CapabilityShader]
  infoCapabilities (StorageClassAtomicCounter) = [CapabilityAtomicStorage]
  infoCapabilities (StorageClassImage) = []
  infoCapabilities (StorageClassStorageBuffer) = [CapabilityShader]
  infoCapabilities (StorageClassPhysicalStorageBuffer) = [CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (StorageClassPhysicalStorageBufferEXT) = [CapabilityPhysicalStorageBufferAddresses]
  infoExtensions (StorageClassUniformConstant) = []
  infoExtensions (StorageClassInput) = []
  infoExtensions (StorageClassUniform) = []
  infoExtensions (StorageClassOutput) = []
  infoExtensions (StorageClassWorkgroup) = []
  infoExtensions (StorageClassCrossWorkgroup) = []
  infoExtensions (StorageClassPrivate) = []
  infoExtensions (StorageClassFunction) = []
  infoExtensions (StorageClassGeneric) = []
  infoExtensions (StorageClassPushConstant) = []
  infoExtensions (StorageClassAtomicCounter) = []
  infoExtensions (StorageClassImage) = []
  infoExtensions (StorageClassStorageBuffer) = ["SPV_KHR_storage_buffer_storage_class", "SPV_KHR_variable_pointers"]
  infoExtensions (StorageClassPhysicalStorageBuffer) = ["SPV_EXT_physical_storage_buffer", "SPV_KHR_physical_storage_buffer"]
  infoExtensions (StorageClassPhysicalStorageBufferEXT) = ["SPV_EXT_physical_storage_buffer"]
  infoVersionRange (StorageClassUniformConstant) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassInput) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassUniform) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassOutput) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassWorkgroup) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassCrossWorkgroup) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassPrivate) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassFunction) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassGeneric) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassPushConstant) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassAtomicCounter) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassImage) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (StorageClassStorageBuffer) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (StorageClassPhysicalStorageBuffer) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (StorageClassPhysicalStorageBufferEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
instance SPIRVSerialize StorageClass where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (StorageClassUniformConstant) = (0, concat [])
      ser (StorageClassInput) = (1, concat [])
      ser (StorageClassUniform) = (2, concat [])
      ser (StorageClassOutput) = (3, concat [])
      ser (StorageClassWorkgroup) = (4, concat [])
      ser (StorageClassCrossWorkgroup) = (5, concat [])
      ser (StorageClassPrivate) = (6, concat [])
      ser (StorageClassFunction) = (7, concat [])
      ser (StorageClassGeneric) = (8, concat [])
      ser (StorageClassPushConstant) = (9, concat [])
      ser (StorageClassAtomicCounter) = (10, concat [])
      ser (StorageClassImage) = (11, concat [])
      ser (StorageClassStorageBuffer) = (12, concat [])
      ser (StorageClassPhysicalStorageBuffer) = (5349, concat [])
      ser (StorageClassPhysicalStorageBufferEXT) = (5349, concat [])
data Dim = Dim1D | Dim2D | Dim3D | DimCube | DimRect | DimBuffer | DimSubpassData
  deriving (Show, Eq, Ord)
instance SPIRVInfo Dim where
  infoCapabilities (Dim1D) = [CapabilitySampled1D, CapabilityImage1D]
  infoCapabilities (Dim2D) = [CapabilityShader, CapabilityKernel, CapabilityImageMSArray]
  infoCapabilities (Dim3D) = []
  infoCapabilities (DimCube) = [CapabilityShader, CapabilityImageCubeArray]
  infoCapabilities (DimRect) = [CapabilitySampledRect, CapabilityImageRect]
  infoCapabilities (DimBuffer) = [CapabilitySampledBuffer, CapabilityImageBuffer]
  infoCapabilities (DimSubpassData) = [CapabilityInputAttachment]
  infoExtensions (Dim1D) = []
  infoExtensions (Dim2D) = []
  infoExtensions (Dim3D) = []
  infoExtensions (DimCube) = []
  infoExtensions (DimRect) = []
  infoExtensions (DimBuffer) = []
  infoExtensions (DimSubpassData) = []
  infoVersionRange (Dim1D) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (Dim2D) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (Dim3D) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DimCube) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DimRect) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DimBuffer) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DimSubpassData) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize Dim where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (Dim1D) = (0, concat [])
      ser (Dim2D) = (1, concat [])
      ser (Dim3D) = (2, concat [])
      ser (DimCube) = (3, concat [])
      ser (DimRect) = (4, concat [])
      ser (DimBuffer) = (5, concat [])
      ser (DimSubpassData) = (6, concat [])
data SamplerAddressingMode = SamplerAddressingModeNone | SamplerAddressingModeClampToEdge | SamplerAddressingModeClamp | SamplerAddressingModeRepeat | SamplerAddressingModeRepeatMirrored
  deriving (Show, Eq, Ord)
instance SPIRVInfo SamplerAddressingMode where
  infoCapabilities (SamplerAddressingModeNone) = [CapabilityKernel]
  infoCapabilities (SamplerAddressingModeClampToEdge) = [CapabilityKernel]
  infoCapabilities (SamplerAddressingModeClamp) = [CapabilityKernel]
  infoCapabilities (SamplerAddressingModeRepeat) = [CapabilityKernel]
  infoCapabilities (SamplerAddressingModeRepeatMirrored) = [CapabilityKernel]
  infoExtensions (SamplerAddressingModeNone) = []
  infoExtensions (SamplerAddressingModeClampToEdge) = []
  infoExtensions (SamplerAddressingModeClamp) = []
  infoExtensions (SamplerAddressingModeRepeat) = []
  infoExtensions (SamplerAddressingModeRepeatMirrored) = []
  infoVersionRange (SamplerAddressingModeNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SamplerAddressingModeClampToEdge) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SamplerAddressingModeClamp) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SamplerAddressingModeRepeat) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SamplerAddressingModeRepeatMirrored) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize SamplerAddressingMode where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (SamplerAddressingModeNone) = (0, concat [])
      ser (SamplerAddressingModeClampToEdge) = (1, concat [])
      ser (SamplerAddressingModeClamp) = (2, concat [])
      ser (SamplerAddressingModeRepeat) = (3, concat [])
      ser (SamplerAddressingModeRepeatMirrored) = (4, concat [])
data SamplerFilterMode = SamplerFilterModeNearest | SamplerFilterModeLinear
  deriving (Show, Eq, Ord)
instance SPIRVInfo SamplerFilterMode where
  infoCapabilities (SamplerFilterModeNearest) = [CapabilityKernel]
  infoCapabilities (SamplerFilterModeLinear) = [CapabilityKernel]
  infoExtensions (SamplerFilterModeNearest) = []
  infoExtensions (SamplerFilterModeLinear) = []
  infoVersionRange (SamplerFilterModeNearest) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (SamplerFilterModeLinear) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize SamplerFilterMode where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (SamplerFilterModeNearest) = (0, concat [])
      ser (SamplerFilterModeLinear) = (1, concat [])
data ImageFormat = ImageFormatUnknown | ImageFormatRgba32f | ImageFormatRgba16f | ImageFormatR32f | ImageFormatRgba8 | ImageFormatRgba8Snorm | ImageFormatRg32f | ImageFormatRg16f | ImageFormatR11fG11fB10f | ImageFormatR16f | ImageFormatRgba16 | ImageFormatRgb10A2 | ImageFormatRg16 | ImageFormatRg8 | ImageFormatR16 | ImageFormatR8 | ImageFormatRgba16Snorm | ImageFormatRg16Snorm | ImageFormatRg8Snorm | ImageFormatR16Snorm | ImageFormatR8Snorm | ImageFormatRgba32i | ImageFormatRgba16i | ImageFormatRgba8i | ImageFormatR32i | ImageFormatRg32i | ImageFormatRg16i | ImageFormatRg8i | ImageFormatR16i | ImageFormatR8i | ImageFormatRgba32ui | ImageFormatRgba16ui | ImageFormatRgba8ui | ImageFormatR32ui | ImageFormatRgb10a2ui | ImageFormatRg32ui | ImageFormatRg16ui | ImageFormatRg8ui | ImageFormatR16ui | ImageFormatR8ui
  deriving (Show, Eq, Ord)
instance SPIRVInfo ImageFormat where
  infoCapabilities (ImageFormatUnknown) = []
  infoCapabilities (ImageFormatRgba32f) = [CapabilityShader]
  infoCapabilities (ImageFormatRgba16f) = [CapabilityShader]
  infoCapabilities (ImageFormatR32f) = [CapabilityShader]
  infoCapabilities (ImageFormatRgba8) = [CapabilityShader]
  infoCapabilities (ImageFormatRgba8Snorm) = [CapabilityShader]
  infoCapabilities (ImageFormatRg32f) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg16f) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR11fG11fB10f) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR16f) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRgba16) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRgb10A2) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg16) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg8) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR16) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR8) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRgba16Snorm) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg16Snorm) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg8Snorm) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR16Snorm) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR8Snorm) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRgba32i) = [CapabilityShader]
  infoCapabilities (ImageFormatRgba16i) = [CapabilityShader]
  infoCapabilities (ImageFormatRgba8i) = [CapabilityShader]
  infoCapabilities (ImageFormatR32i) = [CapabilityShader]
  infoCapabilities (ImageFormatRg32i) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg16i) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg8i) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR16i) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR8i) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRgba32ui) = [CapabilityShader]
  infoCapabilities (ImageFormatRgba16ui) = [CapabilityShader]
  infoCapabilities (ImageFormatRgba8ui) = [CapabilityShader]
  infoCapabilities (ImageFormatR32ui) = [CapabilityShader]
  infoCapabilities (ImageFormatRgb10a2ui) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg32ui) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg16ui) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatRg8ui) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR16ui) = [CapabilityStorageImageExtendedFormats]
  infoCapabilities (ImageFormatR8ui) = [CapabilityStorageImageExtendedFormats]
  infoExtensions (ImageFormatUnknown) = []
  infoExtensions (ImageFormatRgba32f) = []
  infoExtensions (ImageFormatRgba16f) = []
  infoExtensions (ImageFormatR32f) = []
  infoExtensions (ImageFormatRgba8) = []
  infoExtensions (ImageFormatRgba8Snorm) = []
  infoExtensions (ImageFormatRg32f) = []
  infoExtensions (ImageFormatRg16f) = []
  infoExtensions (ImageFormatR11fG11fB10f) = []
  infoExtensions (ImageFormatR16f) = []
  infoExtensions (ImageFormatRgba16) = []
  infoExtensions (ImageFormatRgb10A2) = []
  infoExtensions (ImageFormatRg16) = []
  infoExtensions (ImageFormatRg8) = []
  infoExtensions (ImageFormatR16) = []
  infoExtensions (ImageFormatR8) = []
  infoExtensions (ImageFormatRgba16Snorm) = []
  infoExtensions (ImageFormatRg16Snorm) = []
  infoExtensions (ImageFormatRg8Snorm) = []
  infoExtensions (ImageFormatR16Snorm) = []
  infoExtensions (ImageFormatR8Snorm) = []
  infoExtensions (ImageFormatRgba32i) = []
  infoExtensions (ImageFormatRgba16i) = []
  infoExtensions (ImageFormatRgba8i) = []
  infoExtensions (ImageFormatR32i) = []
  infoExtensions (ImageFormatRg32i) = []
  infoExtensions (ImageFormatRg16i) = []
  infoExtensions (ImageFormatRg8i) = []
  infoExtensions (ImageFormatR16i) = []
  infoExtensions (ImageFormatR8i) = []
  infoExtensions (ImageFormatRgba32ui) = []
  infoExtensions (ImageFormatRgba16ui) = []
  infoExtensions (ImageFormatRgba8ui) = []
  infoExtensions (ImageFormatR32ui) = []
  infoExtensions (ImageFormatRgb10a2ui) = []
  infoExtensions (ImageFormatRg32ui) = []
  infoExtensions (ImageFormatRg16ui) = []
  infoExtensions (ImageFormatRg8ui) = []
  infoExtensions (ImageFormatR16ui) = []
  infoExtensions (ImageFormatR8ui) = []
  infoVersionRange (ImageFormatUnknown) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba32f) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba16f) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR32f) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba8Snorm) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg32f) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg16f) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR11fG11fB10f) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR16f) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgb10A2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba16Snorm) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg16Snorm) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg8Snorm) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR16Snorm) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR8Snorm) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba32i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba16i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba8i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR32i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg32i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg16i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg8i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR16i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR8i) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba32ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba16ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgba8ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR32ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRgb10a2ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg32ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg16ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatRg8ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR16ui) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageFormatR8ui) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize ImageFormat where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (ImageFormatUnknown) = (0, concat [])
      ser (ImageFormatRgba32f) = (1, concat [])
      ser (ImageFormatRgba16f) = (2, concat [])
      ser (ImageFormatR32f) = (3, concat [])
      ser (ImageFormatRgba8) = (4, concat [])
      ser (ImageFormatRgba8Snorm) = (5, concat [])
      ser (ImageFormatRg32f) = (6, concat [])
      ser (ImageFormatRg16f) = (7, concat [])
      ser (ImageFormatR11fG11fB10f) = (8, concat [])
      ser (ImageFormatR16f) = (9, concat [])
      ser (ImageFormatRgba16) = (10, concat [])
      ser (ImageFormatRgb10A2) = (11, concat [])
      ser (ImageFormatRg16) = (12, concat [])
      ser (ImageFormatRg8) = (13, concat [])
      ser (ImageFormatR16) = (14, concat [])
      ser (ImageFormatR8) = (15, concat [])
      ser (ImageFormatRgba16Snorm) = (16, concat [])
      ser (ImageFormatRg16Snorm) = (17, concat [])
      ser (ImageFormatRg8Snorm) = (18, concat [])
      ser (ImageFormatR16Snorm) = (19, concat [])
      ser (ImageFormatR8Snorm) = (20, concat [])
      ser (ImageFormatRgba32i) = (21, concat [])
      ser (ImageFormatRgba16i) = (22, concat [])
      ser (ImageFormatRgba8i) = (23, concat [])
      ser (ImageFormatR32i) = (24, concat [])
      ser (ImageFormatRg32i) = (25, concat [])
      ser (ImageFormatRg16i) = (26, concat [])
      ser (ImageFormatRg8i) = (27, concat [])
      ser (ImageFormatR16i) = (28, concat [])
      ser (ImageFormatR8i) = (29, concat [])
      ser (ImageFormatRgba32ui) = (30, concat [])
      ser (ImageFormatRgba16ui) = (31, concat [])
      ser (ImageFormatRgba8ui) = (32, concat [])
      ser (ImageFormatR32ui) = (33, concat [])
      ser (ImageFormatRgb10a2ui) = (34, concat [])
      ser (ImageFormatRg32ui) = (35, concat [])
      ser (ImageFormatRg16ui) = (36, concat [])
      ser (ImageFormatRg8ui) = (37, concat [])
      ser (ImageFormatR16ui) = (38, concat [])
      ser (ImageFormatR8ui) = (39, concat [])
data ImageChannelOrder = ImageChannelOrderR | ImageChannelOrderA | ImageChannelOrderRG | ImageChannelOrderRA | ImageChannelOrderRGB | ImageChannelOrderRGBA | ImageChannelOrderBGRA | ImageChannelOrderARGB | ImageChannelOrderIntensity | ImageChannelOrderLuminance | ImageChannelOrderRx | ImageChannelOrderRGx | ImageChannelOrderRGBx | ImageChannelOrderDepth | ImageChannelOrderDepthStencil | ImageChannelOrdersRGB | ImageChannelOrdersRGBx | ImageChannelOrdersRGBA | ImageChannelOrdersBGRA | ImageChannelOrderABGR
  deriving (Show, Eq, Ord)
instance SPIRVInfo ImageChannelOrder where
  infoCapabilities (ImageChannelOrderR) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderA) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderRG) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderRA) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderRGB) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderRGBA) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderBGRA) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderARGB) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderIntensity) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderLuminance) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderRx) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderRGx) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderRGBx) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderDepth) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderDepthStencil) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrdersRGB) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrdersRGBx) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrdersRGBA) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrdersBGRA) = [CapabilityKernel]
  infoCapabilities (ImageChannelOrderABGR) = [CapabilityKernel]
  infoExtensions (ImageChannelOrderR) = []
  infoExtensions (ImageChannelOrderA) = []
  infoExtensions (ImageChannelOrderRG) = []
  infoExtensions (ImageChannelOrderRA) = []
  infoExtensions (ImageChannelOrderRGB) = []
  infoExtensions (ImageChannelOrderRGBA) = []
  infoExtensions (ImageChannelOrderBGRA) = []
  infoExtensions (ImageChannelOrderARGB) = []
  infoExtensions (ImageChannelOrderIntensity) = []
  infoExtensions (ImageChannelOrderLuminance) = []
  infoExtensions (ImageChannelOrderRx) = []
  infoExtensions (ImageChannelOrderRGx) = []
  infoExtensions (ImageChannelOrderRGBx) = []
  infoExtensions (ImageChannelOrderDepth) = []
  infoExtensions (ImageChannelOrderDepthStencil) = []
  infoExtensions (ImageChannelOrdersRGB) = []
  infoExtensions (ImageChannelOrdersRGBx) = []
  infoExtensions (ImageChannelOrdersRGBA) = []
  infoExtensions (ImageChannelOrdersBGRA) = []
  infoExtensions (ImageChannelOrderABGR) = []
  infoVersionRange (ImageChannelOrderR) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderA) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderRG) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderRA) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderRGB) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderRGBA) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderBGRA) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderARGB) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderIntensity) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderLuminance) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderRx) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderRGx) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderRGBx) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderDepth) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderDepthStencil) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrdersRGB) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrdersRGBx) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrdersRGBA) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrdersBGRA) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelOrderABGR) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize ImageChannelOrder where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (ImageChannelOrderR) = (0, concat [])
      ser (ImageChannelOrderA) = (1, concat [])
      ser (ImageChannelOrderRG) = (2, concat [])
      ser (ImageChannelOrderRA) = (3, concat [])
      ser (ImageChannelOrderRGB) = (4, concat [])
      ser (ImageChannelOrderRGBA) = (5, concat [])
      ser (ImageChannelOrderBGRA) = (6, concat [])
      ser (ImageChannelOrderARGB) = (7, concat [])
      ser (ImageChannelOrderIntensity) = (8, concat [])
      ser (ImageChannelOrderLuminance) = (9, concat [])
      ser (ImageChannelOrderRx) = (10, concat [])
      ser (ImageChannelOrderRGx) = (11, concat [])
      ser (ImageChannelOrderRGBx) = (12, concat [])
      ser (ImageChannelOrderDepth) = (13, concat [])
      ser (ImageChannelOrderDepthStencil) = (14, concat [])
      ser (ImageChannelOrdersRGB) = (15, concat [])
      ser (ImageChannelOrdersRGBx) = (16, concat [])
      ser (ImageChannelOrdersRGBA) = (17, concat [])
      ser (ImageChannelOrdersBGRA) = (18, concat [])
      ser (ImageChannelOrderABGR) = (19, concat [])
data ImageChannelDataType = ImageChannelDataTypeSnormInt8 | ImageChannelDataTypeSnormInt16 | ImageChannelDataTypeUnormInt8 | ImageChannelDataTypeUnormInt16 | ImageChannelDataTypeUnormShort565 | ImageChannelDataTypeUnormShort555 | ImageChannelDataTypeUnormInt101010 | ImageChannelDataTypeSignedInt8 | ImageChannelDataTypeSignedInt16 | ImageChannelDataTypeSignedInt32 | ImageChannelDataTypeUnsignedInt8 | ImageChannelDataTypeUnsignedInt16 | ImageChannelDataTypeUnsignedInt32 | ImageChannelDataTypeHalfFloat | ImageChannelDataTypeFloat | ImageChannelDataTypeUnormInt24 | ImageChannelDataTypeUnormInt101010_2
  deriving (Show, Eq, Ord)
instance SPIRVInfo ImageChannelDataType where
  infoCapabilities (ImageChannelDataTypeSnormInt8) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeSnormInt16) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnormInt8) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnormInt16) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnormShort565) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnormShort555) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnormInt101010) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeSignedInt8) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeSignedInt16) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeSignedInt32) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnsignedInt8) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnsignedInt16) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnsignedInt32) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeHalfFloat) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeFloat) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnormInt24) = [CapabilityKernel]
  infoCapabilities (ImageChannelDataTypeUnormInt101010_2) = [CapabilityKernel]
  infoExtensions (ImageChannelDataTypeSnormInt8) = []
  infoExtensions (ImageChannelDataTypeSnormInt16) = []
  infoExtensions (ImageChannelDataTypeUnormInt8) = []
  infoExtensions (ImageChannelDataTypeUnormInt16) = []
  infoExtensions (ImageChannelDataTypeUnormShort565) = []
  infoExtensions (ImageChannelDataTypeUnormShort555) = []
  infoExtensions (ImageChannelDataTypeUnormInt101010) = []
  infoExtensions (ImageChannelDataTypeSignedInt8) = []
  infoExtensions (ImageChannelDataTypeSignedInt16) = []
  infoExtensions (ImageChannelDataTypeSignedInt32) = []
  infoExtensions (ImageChannelDataTypeUnsignedInt8) = []
  infoExtensions (ImageChannelDataTypeUnsignedInt16) = []
  infoExtensions (ImageChannelDataTypeUnsignedInt32) = []
  infoExtensions (ImageChannelDataTypeHalfFloat) = []
  infoExtensions (ImageChannelDataTypeFloat) = []
  infoExtensions (ImageChannelDataTypeUnormInt24) = []
  infoExtensions (ImageChannelDataTypeUnormInt101010_2) = []
  infoVersionRange (ImageChannelDataTypeSnormInt8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeSnormInt16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnormInt8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnormInt16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnormShort565) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnormShort555) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnormInt101010) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeSignedInt8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeSignedInt16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeSignedInt32) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnsignedInt8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnsignedInt16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnsignedInt32) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeHalfFloat) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeFloat) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnormInt24) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ImageChannelDataTypeUnormInt101010_2) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize ImageChannelDataType where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (ImageChannelDataTypeSnormInt8) = (0, concat [])
      ser (ImageChannelDataTypeSnormInt16) = (1, concat [])
      ser (ImageChannelDataTypeUnormInt8) = (2, concat [])
      ser (ImageChannelDataTypeUnormInt16) = (3, concat [])
      ser (ImageChannelDataTypeUnormShort565) = (4, concat [])
      ser (ImageChannelDataTypeUnormShort555) = (5, concat [])
      ser (ImageChannelDataTypeUnormInt101010) = (6, concat [])
      ser (ImageChannelDataTypeSignedInt8) = (7, concat [])
      ser (ImageChannelDataTypeSignedInt16) = (8, concat [])
      ser (ImageChannelDataTypeSignedInt32) = (9, concat [])
      ser (ImageChannelDataTypeUnsignedInt8) = (10, concat [])
      ser (ImageChannelDataTypeUnsignedInt16) = (11, concat [])
      ser (ImageChannelDataTypeUnsignedInt32) = (12, concat [])
      ser (ImageChannelDataTypeHalfFloat) = (13, concat [])
      ser (ImageChannelDataTypeFloat) = (14, concat [])
      ser (ImageChannelDataTypeUnormInt24) = (15, concat [])
      ser (ImageChannelDataTypeUnormInt101010_2) = (16, concat [])
data FPRoundingMode = FPRoundingModeRTE | FPRoundingModeRTZ | FPRoundingModeRTP | FPRoundingModeRTN
  deriving (Show, Eq, Ord)
instance SPIRVInfo FPRoundingMode where
  infoCapabilities (FPRoundingModeRTE) = []
  infoCapabilities (FPRoundingModeRTZ) = []
  infoCapabilities (FPRoundingModeRTP) = []
  infoCapabilities (FPRoundingModeRTN) = []
  infoExtensions (FPRoundingModeRTE) = []
  infoExtensions (FPRoundingModeRTZ) = []
  infoExtensions (FPRoundingModeRTP) = []
  infoExtensions (FPRoundingModeRTN) = []
  infoVersionRange (FPRoundingModeRTE) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FPRoundingModeRTZ) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FPRoundingModeRTP) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FPRoundingModeRTN) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize FPRoundingMode where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (FPRoundingModeRTE) = (0, concat [])
      ser (FPRoundingModeRTZ) = (1, concat [])
      ser (FPRoundingModeRTP) = (2, concat [])
      ser (FPRoundingModeRTN) = (3, concat [])
data LinkageType = LinkageTypeExport | LinkageTypeImport
  deriving (Show, Eq, Ord)
instance SPIRVInfo LinkageType where
  infoCapabilities (LinkageTypeExport) = [CapabilityLinkage]
  infoCapabilities (LinkageTypeImport) = [CapabilityLinkage]
  infoExtensions (LinkageTypeExport) = []
  infoExtensions (LinkageTypeImport) = []
  infoVersionRange (LinkageTypeExport) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (LinkageTypeImport) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize LinkageType where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (LinkageTypeExport) = (0, concat [])
      ser (LinkageTypeImport) = (1, concat [])
data AccessQualifier = AccessQualifierReadOnly | AccessQualifierWriteOnly | AccessQualifierReadWrite
  deriving (Show, Eq, Ord)
instance SPIRVInfo AccessQualifier where
  infoCapabilities (AccessQualifierReadOnly) = [CapabilityKernel]
  infoCapabilities (AccessQualifierWriteOnly) = [CapabilityKernel]
  infoCapabilities (AccessQualifierReadWrite) = [CapabilityKernel]
  infoExtensions (AccessQualifierReadOnly) = []
  infoExtensions (AccessQualifierWriteOnly) = []
  infoExtensions (AccessQualifierReadWrite) = []
  infoVersionRange (AccessQualifierReadOnly) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (AccessQualifierWriteOnly) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (AccessQualifierReadWrite) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize AccessQualifier where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (AccessQualifierReadOnly) = (0, concat [])
      ser (AccessQualifierWriteOnly) = (1, concat [])
      ser (AccessQualifierReadWrite) = (2, concat [])
data FunctionParameterAttribute = FunctionParameterAttributeZext | FunctionParameterAttributeSext | FunctionParameterAttributeByVal | FunctionParameterAttributeSret | FunctionParameterAttributeNoAlias | FunctionParameterAttributeNoCapture | FunctionParameterAttributeNoWrite | FunctionParameterAttributeNoReadWrite
  deriving (Show, Eq, Ord)
instance SPIRVInfo FunctionParameterAttribute where
  infoCapabilities (FunctionParameterAttributeZext) = [CapabilityKernel]
  infoCapabilities (FunctionParameterAttributeSext) = [CapabilityKernel]
  infoCapabilities (FunctionParameterAttributeByVal) = [CapabilityKernel]
  infoCapabilities (FunctionParameterAttributeSret) = [CapabilityKernel]
  infoCapabilities (FunctionParameterAttributeNoAlias) = [CapabilityKernel]
  infoCapabilities (FunctionParameterAttributeNoCapture) = [CapabilityKernel]
  infoCapabilities (FunctionParameterAttributeNoWrite) = [CapabilityKernel]
  infoCapabilities (FunctionParameterAttributeNoReadWrite) = [CapabilityKernel]
  infoExtensions (FunctionParameterAttributeZext) = []
  infoExtensions (FunctionParameterAttributeSext) = []
  infoExtensions (FunctionParameterAttributeByVal) = []
  infoExtensions (FunctionParameterAttributeSret) = []
  infoExtensions (FunctionParameterAttributeNoAlias) = []
  infoExtensions (FunctionParameterAttributeNoCapture) = []
  infoExtensions (FunctionParameterAttributeNoWrite) = []
  infoExtensions (FunctionParameterAttributeNoReadWrite) = []
  infoVersionRange (FunctionParameterAttributeZext) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionParameterAttributeSext) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionParameterAttributeByVal) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionParameterAttributeSret) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionParameterAttributeNoAlias) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionParameterAttributeNoCapture) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionParameterAttributeNoWrite) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (FunctionParameterAttributeNoReadWrite) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize FunctionParameterAttribute where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (FunctionParameterAttributeZext) = (0, concat [])
      ser (FunctionParameterAttributeSext) = (1, concat [])
      ser (FunctionParameterAttributeByVal) = (2, concat [])
      ser (FunctionParameterAttributeSret) = (3, concat [])
      ser (FunctionParameterAttributeNoAlias) = (4, concat [])
      ser (FunctionParameterAttributeNoCapture) = (5, concat [])
      ser (FunctionParameterAttributeNoWrite) = (6, concat [])
      ser (FunctionParameterAttributeNoReadWrite) = (7, concat [])
data Decoration = DecorationRelaxedPrecision | DecorationSpecId LiteralInteger | DecorationBlock | DecorationBufferBlock | DecorationRowMajor | DecorationColMajor | DecorationArrayStride LiteralInteger | DecorationMatrixStride LiteralInteger | DecorationGLSLShared | DecorationGLSLPacked | DecorationCPacked | DecorationBuiltIn BuiltIn | DecorationNoPerspective | DecorationFlat | DecorationPatch | DecorationCentroid | DecorationSample | DecorationInvariant | DecorationRestrict | DecorationAliased | DecorationVolatile | DecorationConstant | DecorationCoherent | DecorationNonWritable | DecorationNonReadable | DecorationUniform | DecorationUniformId IdScope | DecorationSaturatedConversion | DecorationStream LiteralInteger | DecorationLocation LiteralInteger | DecorationComponent LiteralInteger | DecorationIndex LiteralInteger | DecorationBinding LiteralInteger | DecorationDescriptorSet LiteralInteger | DecorationOffset LiteralInteger | DecorationXfbBuffer LiteralInteger | DecorationXfbStride LiteralInteger | DecorationFuncParamAttr FunctionParameterAttribute | DecorationFPRoundingMode FPRoundingMode | DecorationFPFastMathMode FPFastMathMode | DecorationLinkageAttributes LiteralString LinkageType | DecorationNoContraction | DecorationInputAttachmentIndex LiteralInteger | DecorationAlignment LiteralInteger | DecorationMaxByteOffset LiteralInteger | DecorationAlignmentId IdRef | DecorationMaxByteOffsetId IdRef | DecorationNoSignedWrap | DecorationNoUnsignedWrap | DecorationNonUniform | DecorationNonUniformEXT | DecorationRestrictPointer | DecorationRestrictPointerEXT | DecorationAliasedPointer | DecorationAliasedPointerEXT | DecorationCounterBuffer IdRef | DecorationUserSemantic LiteralString
  deriving (Show, Eq, Ord)
instance SPIRVInfo Decoration where
  infoCapabilities (DecorationRelaxedPrecision) = [CapabilityShader]
  infoCapabilities (DecorationSpecId x0) = [CapabilityShader, CapabilityKernel]
  infoCapabilities (DecorationBlock) = [CapabilityShader]
  infoCapabilities (DecorationBufferBlock) = [CapabilityShader]
  infoCapabilities (DecorationRowMajor) = [CapabilityMatrix]
  infoCapabilities (DecorationColMajor) = [CapabilityMatrix]
  infoCapabilities (DecorationArrayStride x0) = [CapabilityShader]
  infoCapabilities (DecorationMatrixStride x0) = [CapabilityMatrix]
  infoCapabilities (DecorationGLSLShared) = [CapabilityShader]
  infoCapabilities (DecorationGLSLPacked) = [CapabilityShader]
  infoCapabilities (DecorationCPacked) = [CapabilityKernel]
  infoCapabilities (DecorationBuiltIn x0) = []
  infoCapabilities (DecorationNoPerspective) = [CapabilityShader]
  infoCapabilities (DecorationFlat) = [CapabilityShader]
  infoCapabilities (DecorationPatch) = [CapabilityTessellation]
  infoCapabilities (DecorationCentroid) = [CapabilityShader]
  infoCapabilities (DecorationSample) = [CapabilitySampleRateShading]
  infoCapabilities (DecorationInvariant) = [CapabilityShader]
  infoCapabilities (DecorationRestrict) = []
  infoCapabilities (DecorationAliased) = []
  infoCapabilities (DecorationVolatile) = []
  infoCapabilities (DecorationConstant) = [CapabilityKernel]
  infoCapabilities (DecorationCoherent) = []
  infoCapabilities (DecorationNonWritable) = []
  infoCapabilities (DecorationNonReadable) = []
  infoCapabilities (DecorationUniform) = [CapabilityShader]
  infoCapabilities (DecorationUniformId x0) = [CapabilityShader]
  infoCapabilities (DecorationSaturatedConversion) = [CapabilityKernel]
  infoCapabilities (DecorationStream x0) = [CapabilityGeometryStreams]
  infoCapabilities (DecorationLocation x0) = [CapabilityShader]
  infoCapabilities (DecorationComponent x0) = [CapabilityShader]
  infoCapabilities (DecorationIndex x0) = [CapabilityShader]
  infoCapabilities (DecorationBinding x0) = [CapabilityShader]
  infoCapabilities (DecorationDescriptorSet x0) = [CapabilityShader]
  infoCapabilities (DecorationOffset x0) = [CapabilityShader]
  infoCapabilities (DecorationXfbBuffer x0) = [CapabilityTransformFeedback]
  infoCapabilities (DecorationXfbStride x0) = [CapabilityTransformFeedback]
  infoCapabilities (DecorationFuncParamAttr x0) = [CapabilityKernel]
  infoCapabilities (DecorationFPRoundingMode x0) = []
  infoCapabilities (DecorationFPFastMathMode x0) = [CapabilityKernel]
  infoCapabilities (DecorationLinkageAttributes x0 x1) = [CapabilityLinkage]
  infoCapabilities (DecorationNoContraction) = [CapabilityShader]
  infoCapabilities (DecorationInputAttachmentIndex x0) = [CapabilityInputAttachment]
  infoCapabilities (DecorationAlignment x0) = [CapabilityKernel]
  infoCapabilities (DecorationMaxByteOffset x0) = [CapabilityAddresses]
  infoCapabilities (DecorationAlignmentId x0) = [CapabilityKernel]
  infoCapabilities (DecorationMaxByteOffsetId x0) = [CapabilityAddresses]
  infoCapabilities (DecorationNoSignedWrap) = []
  infoCapabilities (DecorationNoUnsignedWrap) = []
  infoCapabilities (DecorationNonUniform) = [CapabilityShaderNonUniform]
  infoCapabilities (DecorationNonUniformEXT) = [CapabilityShaderNonUniform]
  infoCapabilities (DecorationRestrictPointer) = [CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (DecorationRestrictPointerEXT) = [CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (DecorationAliasedPointer) = [CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (DecorationAliasedPointerEXT) = [CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (DecorationCounterBuffer x0) = []
  infoCapabilities (DecorationUserSemantic x0) = []
  infoExtensions (DecorationRelaxedPrecision) = []
  infoExtensions (DecorationSpecId x0) = []
  infoExtensions (DecorationBlock) = []
  infoExtensions (DecorationBufferBlock) = []
  infoExtensions (DecorationRowMajor) = []
  infoExtensions (DecorationColMajor) = []
  infoExtensions (DecorationArrayStride x0) = []
  infoExtensions (DecorationMatrixStride x0) = []
  infoExtensions (DecorationGLSLShared) = []
  infoExtensions (DecorationGLSLPacked) = []
  infoExtensions (DecorationCPacked) = []
  infoExtensions (DecorationBuiltIn x0) = []
  infoExtensions (DecorationNoPerspective) = []
  infoExtensions (DecorationFlat) = []
  infoExtensions (DecorationPatch) = []
  infoExtensions (DecorationCentroid) = []
  infoExtensions (DecorationSample) = []
  infoExtensions (DecorationInvariant) = []
  infoExtensions (DecorationRestrict) = []
  infoExtensions (DecorationAliased) = []
  infoExtensions (DecorationVolatile) = []
  infoExtensions (DecorationConstant) = []
  infoExtensions (DecorationCoherent) = []
  infoExtensions (DecorationNonWritable) = []
  infoExtensions (DecorationNonReadable) = []
  infoExtensions (DecorationUniform) = []
  infoExtensions (DecorationUniformId x0) = []
  infoExtensions (DecorationSaturatedConversion) = []
  infoExtensions (DecorationStream x0) = []
  infoExtensions (DecorationLocation x0) = []
  infoExtensions (DecorationComponent x0) = []
  infoExtensions (DecorationIndex x0) = []
  infoExtensions (DecorationBinding x0) = []
  infoExtensions (DecorationDescriptorSet x0) = []
  infoExtensions (DecorationOffset x0) = []
  infoExtensions (DecorationXfbBuffer x0) = []
  infoExtensions (DecorationXfbStride x0) = []
  infoExtensions (DecorationFuncParamAttr x0) = []
  infoExtensions (DecorationFPRoundingMode x0) = []
  infoExtensions (DecorationFPFastMathMode x0) = []
  infoExtensions (DecorationLinkageAttributes x0 x1) = []
  infoExtensions (DecorationNoContraction) = []
  infoExtensions (DecorationInputAttachmentIndex x0) = []
  infoExtensions (DecorationAlignment x0) = []
  infoExtensions (DecorationMaxByteOffset x0) = []
  infoExtensions (DecorationAlignmentId x0) = []
  infoExtensions (DecorationMaxByteOffsetId x0) = []
  infoExtensions (DecorationNoSignedWrap) = ["SPV_KHR_no_integer_wrap_decoration"]
  infoExtensions (DecorationNoUnsignedWrap) = ["SPV_KHR_no_integer_wrap_decoration"]
  infoExtensions (DecorationNonUniform) = []
  infoExtensions (DecorationNonUniformEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (DecorationRestrictPointer) = ["SPV_EXT_physical_storage_buffer", "SPV_KHR_physical_storage_buffer"]
  infoExtensions (DecorationRestrictPointerEXT) = ["SPV_EXT_physical_storage_buffer"]
  infoExtensions (DecorationAliasedPointer) = ["SPV_EXT_physical_storage_buffer", "SPV_KHR_physical_storage_buffer"]
  infoExtensions (DecorationAliasedPointerEXT) = ["SPV_EXT_physical_storage_buffer"]
  infoExtensions (DecorationCounterBuffer x0) = []
  infoExtensions (DecorationUserSemantic x0) = []
  infoVersionRange (DecorationRelaxedPrecision) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationSpecId x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationBlock) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationBufferBlock) = SPIRVVersionRange (Nothing, Just (SPIRVVersion 1 3))
  infoVersionRange (DecorationRowMajor) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationColMajor) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationArrayStride x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationMatrixStride x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationGLSLShared) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationGLSLPacked) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationCPacked) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationBuiltIn x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationNoPerspective) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationFlat) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationPatch) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationCentroid) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationSample) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationInvariant) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationRestrict) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationAliased) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationVolatile) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationConstant) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationCoherent) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationNonWritable) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationNonReadable) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationUniform) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationUniformId x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (DecorationSaturatedConversion) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationStream x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationLocation x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationComponent x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationIndex x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationBinding x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationDescriptorSet x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationOffset x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationXfbBuffer x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationXfbStride x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationFuncParamAttr x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationFPRoundingMode x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationFPFastMathMode x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationLinkageAttributes x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationNoContraction) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationInputAttachmentIndex x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationAlignment x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DecorationMaxByteOffset x0) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (DecorationAlignmentId x0) = SPIRVVersionRange (Just (SPIRVVersion 1 2), Nothing)
  infoVersionRange (DecorationMaxByteOffsetId x0) = SPIRVVersionRange (Just (SPIRVVersion 1 2), Nothing)
  infoVersionRange (DecorationNoSignedWrap) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (DecorationNoUnsignedWrap) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (DecorationNonUniform) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (DecorationNonUniformEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (DecorationRestrictPointer) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (DecorationRestrictPointerEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (DecorationAliasedPointer) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (DecorationAliasedPointerEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (DecorationCounterBuffer x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (DecorationUserSemantic x0) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
instance SPIRVSerialize Decoration where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (DecorationRelaxedPrecision) = (0, concat [])
      ser (DecorationSpecId x0) = (1, concat [spirvSerialize x0])
      ser (DecorationBlock) = (2, concat [])
      ser (DecorationBufferBlock) = (3, concat [])
      ser (DecorationRowMajor) = (4, concat [])
      ser (DecorationColMajor) = (5, concat [])
      ser (DecorationArrayStride x0) = (6, concat [spirvSerialize x0])
      ser (DecorationMatrixStride x0) = (7, concat [spirvSerialize x0])
      ser (DecorationGLSLShared) = (8, concat [])
      ser (DecorationGLSLPacked) = (9, concat [])
      ser (DecorationCPacked) = (10, concat [])
      ser (DecorationBuiltIn x0) = (11, concat [spirvSerialize x0])
      ser (DecorationNoPerspective) = (13, concat [])
      ser (DecorationFlat) = (14, concat [])
      ser (DecorationPatch) = (15, concat [])
      ser (DecorationCentroid) = (16, concat [])
      ser (DecorationSample) = (17, concat [])
      ser (DecorationInvariant) = (18, concat [])
      ser (DecorationRestrict) = (19, concat [])
      ser (DecorationAliased) = (20, concat [])
      ser (DecorationVolatile) = (21, concat [])
      ser (DecorationConstant) = (22, concat [])
      ser (DecorationCoherent) = (23, concat [])
      ser (DecorationNonWritable) = (24, concat [])
      ser (DecorationNonReadable) = (25, concat [])
      ser (DecorationUniform) = (26, concat [])
      ser (DecorationUniformId x0) = (27, concat [spirvSerialize x0])
      ser (DecorationSaturatedConversion) = (28, concat [])
      ser (DecorationStream x0) = (29, concat [spirvSerialize x0])
      ser (DecorationLocation x0) = (30, concat [spirvSerialize x0])
      ser (DecorationComponent x0) = (31, concat [spirvSerialize x0])
      ser (DecorationIndex x0) = (32, concat [spirvSerialize x0])
      ser (DecorationBinding x0) = (33, concat [spirvSerialize x0])
      ser (DecorationDescriptorSet x0) = (34, concat [spirvSerialize x0])
      ser (DecorationOffset x0) = (35, concat [spirvSerialize x0])
      ser (DecorationXfbBuffer x0) = (36, concat [spirvSerialize x0])
      ser (DecorationXfbStride x0) = (37, concat [spirvSerialize x0])
      ser (DecorationFuncParamAttr x0) = (38, concat [spirvSerialize x0])
      ser (DecorationFPRoundingMode x0) = (39, concat [spirvSerialize x0])
      ser (DecorationFPFastMathMode x0) = (40, concat [spirvSerialize x0])
      ser (DecorationLinkageAttributes x0 x1) = (41, concat [spirvSerialize x0, spirvSerialize x1])
      ser (DecorationNoContraction) = (42, concat [])
      ser (DecorationInputAttachmentIndex x0) = (43, concat [spirvSerialize x0])
      ser (DecorationAlignment x0) = (44, concat [spirvSerialize x0])
      ser (DecorationMaxByteOffset x0) = (45, concat [spirvSerialize x0])
      ser (DecorationAlignmentId x0) = (46, concat [spirvSerialize x0])
      ser (DecorationMaxByteOffsetId x0) = (47, concat [spirvSerialize x0])
      ser (DecorationNoSignedWrap) = (4469, concat [])
      ser (DecorationNoUnsignedWrap) = (4470, concat [])
      ser (DecorationNonUniform) = (5300, concat [])
      ser (DecorationNonUniformEXT) = (5300, concat [])
      ser (DecorationRestrictPointer) = (5355, concat [])
      ser (DecorationRestrictPointerEXT) = (5355, concat [])
      ser (DecorationAliasedPointer) = (5356, concat [])
      ser (DecorationAliasedPointerEXT) = (5356, concat [])
      ser (DecorationCounterBuffer x0) = (5634, concat [spirvSerialize x0])
      ser (DecorationUserSemantic x0) = (5635, concat [spirvSerialize x0])
data BuiltIn = BuiltInPosition | BuiltInPointSize | BuiltInClipDistance | BuiltInCullDistance | BuiltInVertexId | BuiltInInstanceId | BuiltInPrimitiveId | BuiltInInvocationId | BuiltInLayer | BuiltInViewportIndex | BuiltInTessLevelOuter | BuiltInTessLevelInner | BuiltInTessCoord | BuiltInPatchVertices | BuiltInFragCoord | BuiltInPointCoord | BuiltInFrontFacing | BuiltInSampleId | BuiltInSamplePosition | BuiltInSampleMask | BuiltInFragDepth | BuiltInHelperInvocation | BuiltInNumWorkgroups | BuiltInWorkgroupSize | BuiltInWorkgroupId | BuiltInLocalInvocationId | BuiltInGlobalInvocationId | BuiltInLocalInvocationIndex | BuiltInWorkDim | BuiltInGlobalSize | BuiltInEnqueuedWorkgroupSize | BuiltInGlobalOffset | BuiltInGlobalLinearId | BuiltInSubgroupSize | BuiltInSubgroupMaxSize | BuiltInNumSubgroups | BuiltInNumEnqueuedSubgroups | BuiltInSubgroupId | BuiltInSubgroupLocalInvocationId | BuiltInVertexIndex | BuiltInInstanceIndex | BuiltInSubgroupEqMask | BuiltInSubgroupGeMask | BuiltInSubgroupGtMask | BuiltInSubgroupLeMask | BuiltInSubgroupLtMask | BuiltInSubgroupEqMaskKHR | BuiltInSubgroupGeMaskKHR | BuiltInSubgroupGtMaskKHR | BuiltInSubgroupLeMaskKHR | BuiltInSubgroupLtMaskKHR | BuiltInBaseVertex | BuiltInBaseInstance | BuiltInDrawIndex | BuiltInDeviceIndex | BuiltInViewIndex
  deriving (Show, Eq, Ord)
instance SPIRVInfo BuiltIn where
  infoCapabilities (BuiltInPosition) = [CapabilityShader]
  infoCapabilities (BuiltInPointSize) = [CapabilityShader]
  infoCapabilities (BuiltInClipDistance) = [CapabilityClipDistance]
  infoCapabilities (BuiltInCullDistance) = [CapabilityCullDistance]
  infoCapabilities (BuiltInVertexId) = [CapabilityShader]
  infoCapabilities (BuiltInInstanceId) = [CapabilityShader]
  infoCapabilities (BuiltInPrimitiveId) = [CapabilityGeometry, CapabilityTessellation]
  infoCapabilities (BuiltInInvocationId) = [CapabilityGeometry, CapabilityTessellation]
  infoCapabilities (BuiltInLayer) = [CapabilityGeometry, CapabilityShaderLayer]
  infoCapabilities (BuiltInViewportIndex) = [CapabilityMultiViewport, CapabilityShaderViewportIndex]
  infoCapabilities (BuiltInTessLevelOuter) = [CapabilityTessellation]
  infoCapabilities (BuiltInTessLevelInner) = [CapabilityTessellation]
  infoCapabilities (BuiltInTessCoord) = [CapabilityTessellation]
  infoCapabilities (BuiltInPatchVertices) = [CapabilityTessellation]
  infoCapabilities (BuiltInFragCoord) = [CapabilityShader]
  infoCapabilities (BuiltInPointCoord) = [CapabilityShader]
  infoCapabilities (BuiltInFrontFacing) = [CapabilityShader]
  infoCapabilities (BuiltInSampleId) = [CapabilitySampleRateShading]
  infoCapabilities (BuiltInSamplePosition) = [CapabilitySampleRateShading]
  infoCapabilities (BuiltInSampleMask) = [CapabilityShader]
  infoCapabilities (BuiltInFragDepth) = [CapabilityShader]
  infoCapabilities (BuiltInHelperInvocation) = [CapabilityShader]
  infoCapabilities (BuiltInNumWorkgroups) = []
  infoCapabilities (BuiltInWorkgroupSize) = []
  infoCapabilities (BuiltInWorkgroupId) = []
  infoCapabilities (BuiltInLocalInvocationId) = []
  infoCapabilities (BuiltInGlobalInvocationId) = []
  infoCapabilities (BuiltInLocalInvocationIndex) = []
  infoCapabilities (BuiltInWorkDim) = [CapabilityKernel]
  infoCapabilities (BuiltInGlobalSize) = [CapabilityKernel]
  infoCapabilities (BuiltInEnqueuedWorkgroupSize) = [CapabilityKernel]
  infoCapabilities (BuiltInGlobalOffset) = [CapabilityKernel]
  infoCapabilities (BuiltInGlobalLinearId) = [CapabilityKernel]
  infoCapabilities (BuiltInSubgroupSize) = [CapabilityKernel, CapabilityGroupNonUniform]
  infoCapabilities (BuiltInSubgroupMaxSize) = [CapabilityKernel]
  infoCapabilities (BuiltInNumSubgroups) = [CapabilityKernel, CapabilityGroupNonUniform]
  infoCapabilities (BuiltInNumEnqueuedSubgroups) = [CapabilityKernel]
  infoCapabilities (BuiltInSubgroupId) = [CapabilityKernel, CapabilityGroupNonUniform]
  infoCapabilities (BuiltInSubgroupLocalInvocationId) = [CapabilityKernel, CapabilityGroupNonUniform]
  infoCapabilities (BuiltInVertexIndex) = [CapabilityShader]
  infoCapabilities (BuiltInInstanceIndex) = [CapabilityShader]
  infoCapabilities (BuiltInSubgroupEqMask) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupGeMask) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupGtMask) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupLeMask) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupLtMask) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupEqMaskKHR) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupGeMaskKHR) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupGtMaskKHR) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupLeMaskKHR) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInSubgroupLtMaskKHR) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (BuiltInBaseVertex) = [CapabilityDrawParameters]
  infoCapabilities (BuiltInBaseInstance) = [CapabilityDrawParameters]
  infoCapabilities (BuiltInDrawIndex) = [CapabilityDrawParameters]
  infoCapabilities (BuiltInDeviceIndex) = [CapabilityDeviceGroup]
  infoCapabilities (BuiltInViewIndex) = [CapabilityMultiView]
  infoExtensions (BuiltInPosition) = []
  infoExtensions (BuiltInPointSize) = []
  infoExtensions (BuiltInClipDistance) = []
  infoExtensions (BuiltInCullDistance) = []
  infoExtensions (BuiltInVertexId) = []
  infoExtensions (BuiltInInstanceId) = []
  infoExtensions (BuiltInPrimitiveId) = []
  infoExtensions (BuiltInInvocationId) = []
  infoExtensions (BuiltInLayer) = []
  infoExtensions (BuiltInViewportIndex) = []
  infoExtensions (BuiltInTessLevelOuter) = []
  infoExtensions (BuiltInTessLevelInner) = []
  infoExtensions (BuiltInTessCoord) = []
  infoExtensions (BuiltInPatchVertices) = []
  infoExtensions (BuiltInFragCoord) = []
  infoExtensions (BuiltInPointCoord) = []
  infoExtensions (BuiltInFrontFacing) = []
  infoExtensions (BuiltInSampleId) = []
  infoExtensions (BuiltInSamplePosition) = []
  infoExtensions (BuiltInSampleMask) = []
  infoExtensions (BuiltInFragDepth) = []
  infoExtensions (BuiltInHelperInvocation) = []
  infoExtensions (BuiltInNumWorkgroups) = []
  infoExtensions (BuiltInWorkgroupSize) = []
  infoExtensions (BuiltInWorkgroupId) = []
  infoExtensions (BuiltInLocalInvocationId) = []
  infoExtensions (BuiltInGlobalInvocationId) = []
  infoExtensions (BuiltInLocalInvocationIndex) = []
  infoExtensions (BuiltInWorkDim) = []
  infoExtensions (BuiltInGlobalSize) = []
  infoExtensions (BuiltInEnqueuedWorkgroupSize) = []
  infoExtensions (BuiltInGlobalOffset) = []
  infoExtensions (BuiltInGlobalLinearId) = []
  infoExtensions (BuiltInSubgroupSize) = []
  infoExtensions (BuiltInSubgroupMaxSize) = []
  infoExtensions (BuiltInNumSubgroups) = []
  infoExtensions (BuiltInNumEnqueuedSubgroups) = []
  infoExtensions (BuiltInSubgroupId) = []
  infoExtensions (BuiltInSubgroupLocalInvocationId) = []
  infoExtensions (BuiltInVertexIndex) = []
  infoExtensions (BuiltInInstanceIndex) = []
  infoExtensions (BuiltInSubgroupEqMask) = []
  infoExtensions (BuiltInSubgroupGeMask) = []
  infoExtensions (BuiltInSubgroupGtMask) = []
  infoExtensions (BuiltInSubgroupLeMask) = []
  infoExtensions (BuiltInSubgroupLtMask) = []
  infoExtensions (BuiltInSubgroupEqMaskKHR) = ["SPV_KHR_shader_ballot"]
  infoExtensions (BuiltInSubgroupGeMaskKHR) = ["SPV_KHR_shader_ballot"]
  infoExtensions (BuiltInSubgroupGtMaskKHR) = ["SPV_KHR_shader_ballot"]
  infoExtensions (BuiltInSubgroupLeMaskKHR) = ["SPV_KHR_shader_ballot"]
  infoExtensions (BuiltInSubgroupLtMaskKHR) = ["SPV_KHR_shader_ballot"]
  infoExtensions (BuiltInBaseVertex) = ["SPV_KHR_shader_draw_parameters"]
  infoExtensions (BuiltInBaseInstance) = ["SPV_KHR_shader_draw_parameters"]
  infoExtensions (BuiltInDrawIndex) = ["SPV_KHR_shader_draw_parameters", "SPV_NV_mesh_shader"]
  infoExtensions (BuiltInDeviceIndex) = ["SPV_KHR_device_group"]
  infoExtensions (BuiltInViewIndex) = ["SPV_KHR_multiview"]
  infoVersionRange (BuiltInPosition) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInPointSize) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInClipDistance) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInCullDistance) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInVertexId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInInstanceId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInPrimitiveId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInInvocationId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInLayer) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInViewportIndex) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInTessLevelOuter) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInTessLevelInner) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInTessCoord) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInPatchVertices) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInFragCoord) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInPointCoord) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInFrontFacing) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInSampleId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInSamplePosition) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInSampleMask) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInFragDepth) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInHelperInvocation) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInNumWorkgroups) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInWorkgroupSize) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInWorkgroupId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInLocalInvocationId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInGlobalInvocationId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInLocalInvocationIndex) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInWorkDim) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInGlobalSize) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInEnqueuedWorkgroupSize) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInGlobalOffset) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInGlobalLinearId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInSubgroupSize) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInSubgroupMaxSize) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInNumSubgroups) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInNumEnqueuedSubgroups) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInSubgroupId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInSubgroupLocalInvocationId) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInVertexIndex) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInInstanceIndex) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (BuiltInSubgroupEqMask) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupGeMask) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupGtMask) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupLeMask) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupLtMask) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupEqMaskKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupGeMaskKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupGtMaskKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupLeMaskKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInSubgroupLtMaskKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInBaseVertex) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInBaseInstance) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInDrawIndex) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInDeviceIndex) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (BuiltInViewIndex) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
instance SPIRVSerialize BuiltIn where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (BuiltInPosition) = (0, concat [])
      ser (BuiltInPointSize) = (1, concat [])
      ser (BuiltInClipDistance) = (3, concat [])
      ser (BuiltInCullDistance) = (4, concat [])
      ser (BuiltInVertexId) = (5, concat [])
      ser (BuiltInInstanceId) = (6, concat [])
      ser (BuiltInPrimitiveId) = (7, concat [])
      ser (BuiltInInvocationId) = (8, concat [])
      ser (BuiltInLayer) = (9, concat [])
      ser (BuiltInViewportIndex) = (10, concat [])
      ser (BuiltInTessLevelOuter) = (11, concat [])
      ser (BuiltInTessLevelInner) = (12, concat [])
      ser (BuiltInTessCoord) = (13, concat [])
      ser (BuiltInPatchVertices) = (14, concat [])
      ser (BuiltInFragCoord) = (15, concat [])
      ser (BuiltInPointCoord) = (16, concat [])
      ser (BuiltInFrontFacing) = (17, concat [])
      ser (BuiltInSampleId) = (18, concat [])
      ser (BuiltInSamplePosition) = (19, concat [])
      ser (BuiltInSampleMask) = (20, concat [])
      ser (BuiltInFragDepth) = (22, concat [])
      ser (BuiltInHelperInvocation) = (23, concat [])
      ser (BuiltInNumWorkgroups) = (24, concat [])
      ser (BuiltInWorkgroupSize) = (25, concat [])
      ser (BuiltInWorkgroupId) = (26, concat [])
      ser (BuiltInLocalInvocationId) = (27, concat [])
      ser (BuiltInGlobalInvocationId) = (28, concat [])
      ser (BuiltInLocalInvocationIndex) = (29, concat [])
      ser (BuiltInWorkDim) = (30, concat [])
      ser (BuiltInGlobalSize) = (31, concat [])
      ser (BuiltInEnqueuedWorkgroupSize) = (32, concat [])
      ser (BuiltInGlobalOffset) = (33, concat [])
      ser (BuiltInGlobalLinearId) = (34, concat [])
      ser (BuiltInSubgroupSize) = (36, concat [])
      ser (BuiltInSubgroupMaxSize) = (37, concat [])
      ser (BuiltInNumSubgroups) = (38, concat [])
      ser (BuiltInNumEnqueuedSubgroups) = (39, concat [])
      ser (BuiltInSubgroupId) = (40, concat [])
      ser (BuiltInSubgroupLocalInvocationId) = (41, concat [])
      ser (BuiltInVertexIndex) = (42, concat [])
      ser (BuiltInInstanceIndex) = (43, concat [])
      ser (BuiltInSubgroupEqMask) = (4416, concat [])
      ser (BuiltInSubgroupGeMask) = (4417, concat [])
      ser (BuiltInSubgroupGtMask) = (4418, concat [])
      ser (BuiltInSubgroupLeMask) = (4419, concat [])
      ser (BuiltInSubgroupLtMask) = (4420, concat [])
      ser (BuiltInSubgroupEqMaskKHR) = (4416, concat [])
      ser (BuiltInSubgroupGeMaskKHR) = (4417, concat [])
      ser (BuiltInSubgroupGtMaskKHR) = (4418, concat [])
      ser (BuiltInSubgroupLeMaskKHR) = (4419, concat [])
      ser (BuiltInSubgroupLtMaskKHR) = (4420, concat [])
      ser (BuiltInBaseVertex) = (4424, concat [])
      ser (BuiltInBaseInstance) = (4425, concat [])
      ser (BuiltInDrawIndex) = (4426, concat [])
      ser (BuiltInDeviceIndex) = (4438, concat [])
      ser (BuiltInViewIndex) = (4440, concat [])
data Scope = ScopeCrossDevice | ScopeDevice | ScopeWorkgroup | ScopeSubgroup | ScopeInvocation | ScopeQueueFamily | ScopeQueueFamilyKHR
  deriving (Show, Eq, Ord)
instance SPIRVInfo Scope where
  infoCapabilities (ScopeCrossDevice) = []
  infoCapabilities (ScopeDevice) = []
  infoCapabilities (ScopeWorkgroup) = []
  infoCapabilities (ScopeSubgroup) = []
  infoCapabilities (ScopeInvocation) = []
  infoCapabilities (ScopeQueueFamily) = [CapabilityVulkanMemoryModel]
  infoCapabilities (ScopeQueueFamilyKHR) = [CapabilityVulkanMemoryModel]
  infoExtensions (ScopeCrossDevice) = []
  infoExtensions (ScopeDevice) = []
  infoExtensions (ScopeWorkgroup) = []
  infoExtensions (ScopeSubgroup) = []
  infoExtensions (ScopeInvocation) = []
  infoExtensions (ScopeQueueFamily) = []
  infoExtensions (ScopeQueueFamilyKHR) = []
  infoVersionRange (ScopeCrossDevice) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ScopeDevice) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ScopeWorkgroup) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ScopeSubgroup) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ScopeInvocation) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (ScopeQueueFamily) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (ScopeQueueFamilyKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
instance SPIRVSerialize Scope where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (ScopeCrossDevice) = (0, concat [])
      ser (ScopeDevice) = (1, concat [])
      ser (ScopeWorkgroup) = (2, concat [])
      ser (ScopeSubgroup) = (3, concat [])
      ser (ScopeInvocation) = (4, concat [])
      ser (ScopeQueueFamily) = (5, concat [])
      ser (ScopeQueueFamilyKHR) = (5, concat [])
data GroupOperation = GroupOperationReduce | GroupOperationInclusiveScan | GroupOperationExclusiveScan | GroupOperationClusteredReduce
  deriving (Show, Eq, Ord)
instance SPIRVInfo GroupOperation where
  infoCapabilities (GroupOperationReduce) = [CapabilityKernel, CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformBallot]
  infoCapabilities (GroupOperationInclusiveScan) = [CapabilityKernel, CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformBallot]
  infoCapabilities (GroupOperationExclusiveScan) = [CapabilityKernel, CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformBallot]
  infoCapabilities (GroupOperationClusteredReduce) = [CapabilityGroupNonUniformClustered]
  infoExtensions (GroupOperationReduce) = []
  infoExtensions (GroupOperationInclusiveScan) = []
  infoExtensions (GroupOperationExclusiveScan) = []
  infoExtensions (GroupOperationClusteredReduce) = []
  infoVersionRange (GroupOperationReduce) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GroupOperationInclusiveScan) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GroupOperationExclusiveScan) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GroupOperationClusteredReduce) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
instance SPIRVSerialize GroupOperation where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (GroupOperationReduce) = (0, concat [])
      ser (GroupOperationInclusiveScan) = (1, concat [])
      ser (GroupOperationExclusiveScan) = (2, concat [])
      ser (GroupOperationClusteredReduce) = (3, concat [])
data KernelEnqueueFlags = KernelEnqueueFlagsNoWait | KernelEnqueueFlagsWaitKernel | KernelEnqueueFlagsWaitWorkGroup
  deriving (Show, Eq, Ord)
instance SPIRVInfo KernelEnqueueFlags where
  infoCapabilities (KernelEnqueueFlagsNoWait) = [CapabilityKernel]
  infoCapabilities (KernelEnqueueFlagsWaitKernel) = [CapabilityKernel]
  infoCapabilities (KernelEnqueueFlagsWaitWorkGroup) = [CapabilityKernel]
  infoExtensions (KernelEnqueueFlagsNoWait) = []
  infoExtensions (KernelEnqueueFlagsWaitKernel) = []
  infoExtensions (KernelEnqueueFlagsWaitWorkGroup) = []
  infoVersionRange (KernelEnqueueFlagsNoWait) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (KernelEnqueueFlagsWaitKernel) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (KernelEnqueueFlagsWaitWorkGroup) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize KernelEnqueueFlags where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (KernelEnqueueFlagsNoWait) = (0, concat [])
      ser (KernelEnqueueFlagsWaitKernel) = (1, concat [])
      ser (KernelEnqueueFlagsWaitWorkGroup) = (2, concat [])
data Capability = CapabilityMatrix | CapabilityShader | CapabilityGeometry | CapabilityTessellation | CapabilityAddresses | CapabilityLinkage | CapabilityKernel | CapabilityVector16 | CapabilityFloat16Buffer | CapabilityFloat16 | CapabilityFloat64 | CapabilityInt64 | CapabilityInt64Atomics | CapabilityImageBasic | CapabilityImageReadWrite | CapabilityImageMipmap | CapabilityPipes | CapabilityGroups | CapabilityDeviceEnqueue | CapabilityLiteralSampler | CapabilityAtomicStorage | CapabilityInt16 | CapabilityTessellationPointSize | CapabilityGeometryPointSize | CapabilityImageGatherExtended | CapabilityStorageImageMultisample | CapabilityUniformBufferArrayDynamicIndexing | CapabilitySampledImageArrayDynamicIndexing | CapabilityStorageBufferArrayDynamicIndexing | CapabilityStorageImageArrayDynamicIndexing | CapabilityClipDistance | CapabilityCullDistance | CapabilityImageCubeArray | CapabilitySampleRateShading | CapabilityImageRect | CapabilitySampledRect | CapabilityGenericPointer | CapabilityInt8 | CapabilityInputAttachment | CapabilitySparseResidency | CapabilityMinLod | CapabilitySampled1D | CapabilityImage1D | CapabilitySampledCubeArray | CapabilitySampledBuffer | CapabilityImageBuffer | CapabilityImageMSArray | CapabilityStorageImageExtendedFormats | CapabilityImageQuery | CapabilityDerivativeControl | CapabilityInterpolationFunction | CapabilityTransformFeedback | CapabilityGeometryStreams | CapabilityStorageImageReadWithoutFormat | CapabilityStorageImageWriteWithoutFormat | CapabilityMultiViewport | CapabilitySubgroupDispatch | CapabilityNamedBarrier | CapabilityPipeStorage | CapabilityGroupNonUniform | CapabilityGroupNonUniformVote | CapabilityGroupNonUniformArithmetic | CapabilityGroupNonUniformBallot | CapabilityGroupNonUniformShuffle | CapabilityGroupNonUniformShuffleRelative | CapabilityGroupNonUniformClustered | CapabilityGroupNonUniformQuad | CapabilityShaderLayer | CapabilityShaderViewportIndex | CapabilityDrawParameters | CapabilityStorageBuffer16BitAccess | CapabilityStorageUniformBufferBlock16 | CapabilityUniformAndStorageBuffer16BitAccess | CapabilityStorageUniform16 | CapabilityStoragePushConstant16 | CapabilityStorageInputOutput16 | CapabilityDeviceGroup | CapabilityMultiView | CapabilityVariablePointersStorageBuffer | CapabilityVariablePointers | CapabilityStorageBuffer8BitAccess | CapabilityUniformAndStorageBuffer8BitAccess | CapabilityStoragePushConstant8 | CapabilityDenormPreserve | CapabilityDenormFlushToZero | CapabilitySignedZeroInfNanPreserve | CapabilityRoundingModeRTE | CapabilityRoundingModeRTZ | CapabilityShaderNonUniform | CapabilityShaderNonUniformEXT | CapabilityRuntimeDescriptorArray | CapabilityRuntimeDescriptorArrayEXT | CapabilityInputAttachmentArrayDynamicIndexing | CapabilityInputAttachmentArrayDynamicIndexingEXT | CapabilityUniformTexelBufferArrayDynamicIndexing | CapabilityUniformTexelBufferArrayDynamicIndexingEXT | CapabilityStorageTexelBufferArrayDynamicIndexing | CapabilityStorageTexelBufferArrayDynamicIndexingEXT | CapabilityUniformBufferArrayNonUniformIndexing | CapabilityUniformBufferArrayNonUniformIndexingEXT | CapabilitySampledImageArrayNonUniformIndexing | CapabilitySampledImageArrayNonUniformIndexingEXT | CapabilityStorageBufferArrayNonUniformIndexing | CapabilityStorageBufferArrayNonUniformIndexingEXT | CapabilityStorageImageArrayNonUniformIndexing | CapabilityStorageImageArrayNonUniformIndexingEXT | CapabilityInputAttachmentArrayNonUniformIndexing | CapabilityInputAttachmentArrayNonUniformIndexingEXT | CapabilityUniformTexelBufferArrayNonUniformIndexing | CapabilityUniformTexelBufferArrayNonUniformIndexingEXT | CapabilityStorageTexelBufferArrayNonUniformIndexing | CapabilityStorageTexelBufferArrayNonUniformIndexingEXT | CapabilityVulkanMemoryModel | CapabilityVulkanMemoryModelKHR | CapabilityVulkanMemoryModelDeviceScope | CapabilityVulkanMemoryModelDeviceScopeKHR | CapabilityPhysicalStorageBufferAddresses | CapabilityPhysicalStorageBufferAddressesEXT
  deriving (Show, Eq, Ord)
instance SPIRVInfo Capability where
  infoCapabilities (CapabilityMatrix) = []
  infoCapabilities (CapabilityShader) = [CapabilityMatrix]
  infoCapabilities (CapabilityGeometry) = [CapabilityShader]
  infoCapabilities (CapabilityTessellation) = [CapabilityShader]
  infoCapabilities (CapabilityAddresses) = []
  infoCapabilities (CapabilityLinkage) = []
  infoCapabilities (CapabilityKernel) = []
  infoCapabilities (CapabilityVector16) = [CapabilityKernel]
  infoCapabilities (CapabilityFloat16Buffer) = [CapabilityKernel]
  infoCapabilities (CapabilityFloat16) = []
  infoCapabilities (CapabilityFloat64) = []
  infoCapabilities (CapabilityInt64) = []
  infoCapabilities (CapabilityInt64Atomics) = [CapabilityInt64]
  infoCapabilities (CapabilityImageBasic) = [CapabilityKernel]
  infoCapabilities (CapabilityImageReadWrite) = [CapabilityImageBasic]
  infoCapabilities (CapabilityImageMipmap) = [CapabilityImageBasic]
  infoCapabilities (CapabilityPipes) = [CapabilityKernel]
  infoCapabilities (CapabilityGroups) = []
  infoCapabilities (CapabilityDeviceEnqueue) = [CapabilityKernel]
  infoCapabilities (CapabilityLiteralSampler) = [CapabilityKernel]
  infoCapabilities (CapabilityAtomicStorage) = [CapabilityShader]
  infoCapabilities (CapabilityInt16) = []
  infoCapabilities (CapabilityTessellationPointSize) = [CapabilityTessellation]
  infoCapabilities (CapabilityGeometryPointSize) = [CapabilityGeometry]
  infoCapabilities (CapabilityImageGatherExtended) = [CapabilityShader]
  infoCapabilities (CapabilityStorageImageMultisample) = [CapabilityShader]
  infoCapabilities (CapabilityUniformBufferArrayDynamicIndexing) = [CapabilityShader]
  infoCapabilities (CapabilitySampledImageArrayDynamicIndexing) = [CapabilityShader]
  infoCapabilities (CapabilityStorageBufferArrayDynamicIndexing) = [CapabilityShader]
  infoCapabilities (CapabilityStorageImageArrayDynamicIndexing) = [CapabilityShader]
  infoCapabilities (CapabilityClipDistance) = [CapabilityShader]
  infoCapabilities (CapabilityCullDistance) = [CapabilityShader]
  infoCapabilities (CapabilityImageCubeArray) = [CapabilitySampledCubeArray]
  infoCapabilities (CapabilitySampleRateShading) = [CapabilityShader]
  infoCapabilities (CapabilityImageRect) = [CapabilitySampledRect]
  infoCapabilities (CapabilitySampledRect) = [CapabilityShader]
  infoCapabilities (CapabilityGenericPointer) = [CapabilityAddresses]
  infoCapabilities (CapabilityInt8) = []
  infoCapabilities (CapabilityInputAttachment) = [CapabilityShader]
  infoCapabilities (CapabilitySparseResidency) = [CapabilityShader]
  infoCapabilities (CapabilityMinLod) = [CapabilityShader]
  infoCapabilities (CapabilitySampled1D) = []
  infoCapabilities (CapabilityImage1D) = [CapabilitySampled1D]
  infoCapabilities (CapabilitySampledCubeArray) = [CapabilityShader]
  infoCapabilities (CapabilitySampledBuffer) = []
  infoCapabilities (CapabilityImageBuffer) = [CapabilitySampledBuffer]
  infoCapabilities (CapabilityImageMSArray) = [CapabilityShader]
  infoCapabilities (CapabilityStorageImageExtendedFormats) = [CapabilityShader]
  infoCapabilities (CapabilityImageQuery) = [CapabilityShader]
  infoCapabilities (CapabilityDerivativeControl) = [CapabilityShader]
  infoCapabilities (CapabilityInterpolationFunction) = [CapabilityShader]
  infoCapabilities (CapabilityTransformFeedback) = [CapabilityShader]
  infoCapabilities (CapabilityGeometryStreams) = [CapabilityGeometry]
  infoCapabilities (CapabilityStorageImageReadWithoutFormat) = [CapabilityShader]
  infoCapabilities (CapabilityStorageImageWriteWithoutFormat) = [CapabilityShader]
  infoCapabilities (CapabilityMultiViewport) = [CapabilityGeometry]
  infoCapabilities (CapabilitySubgroupDispatch) = [CapabilityDeviceEnqueue]
  infoCapabilities (CapabilityNamedBarrier) = [CapabilityKernel]
  infoCapabilities (CapabilityPipeStorage) = [CapabilityPipes]
  infoCapabilities (CapabilityGroupNonUniform) = []
  infoCapabilities (CapabilityGroupNonUniformVote) = [CapabilityGroupNonUniform]
  infoCapabilities (CapabilityGroupNonUniformArithmetic) = [CapabilityGroupNonUniform]
  infoCapabilities (CapabilityGroupNonUniformBallot) = [CapabilityGroupNonUniform]
  infoCapabilities (CapabilityGroupNonUniformShuffle) = [CapabilityGroupNonUniform]
  infoCapabilities (CapabilityGroupNonUniformShuffleRelative) = [CapabilityGroupNonUniform]
  infoCapabilities (CapabilityGroupNonUniformClustered) = [CapabilityGroupNonUniform]
  infoCapabilities (CapabilityGroupNonUniformQuad) = [CapabilityGroupNonUniform]
  infoCapabilities (CapabilityShaderLayer) = []
  infoCapabilities (CapabilityShaderViewportIndex) = []
  infoCapabilities (CapabilityDrawParameters) = [CapabilityShader]
  infoCapabilities (CapabilityStorageBuffer16BitAccess) = []
  infoCapabilities (CapabilityStorageUniformBufferBlock16) = []
  infoCapabilities (CapabilityUniformAndStorageBuffer16BitAccess) = [CapabilityStorageBuffer16BitAccess, CapabilityStorageUniformBufferBlock16]
  infoCapabilities (CapabilityStorageUniform16) = [CapabilityStorageBuffer16BitAccess, CapabilityStorageUniformBufferBlock16]
  infoCapabilities (CapabilityStoragePushConstant16) = []
  infoCapabilities (CapabilityStorageInputOutput16) = []
  infoCapabilities (CapabilityDeviceGroup) = []
  infoCapabilities (CapabilityMultiView) = [CapabilityShader]
  infoCapabilities (CapabilityVariablePointersStorageBuffer) = [CapabilityShader]
  infoCapabilities (CapabilityVariablePointers) = [CapabilityVariablePointersStorageBuffer]
  infoCapabilities (CapabilityStorageBuffer8BitAccess) = []
  infoCapabilities (CapabilityUniformAndStorageBuffer8BitAccess) = [CapabilityStorageBuffer8BitAccess]
  infoCapabilities (CapabilityStoragePushConstant8) = []
  infoCapabilities (CapabilityDenormPreserve) = []
  infoCapabilities (CapabilityDenormFlushToZero) = []
  infoCapabilities (CapabilitySignedZeroInfNanPreserve) = []
  infoCapabilities (CapabilityRoundingModeRTE) = []
  infoCapabilities (CapabilityRoundingModeRTZ) = []
  infoCapabilities (CapabilityShaderNonUniform) = [CapabilityShader]
  infoCapabilities (CapabilityShaderNonUniformEXT) = [CapabilityShader]
  infoCapabilities (CapabilityRuntimeDescriptorArray) = [CapabilityShader]
  infoCapabilities (CapabilityRuntimeDescriptorArrayEXT) = [CapabilityShader]
  infoCapabilities (CapabilityInputAttachmentArrayDynamicIndexing) = [CapabilityInputAttachment]
  infoCapabilities (CapabilityInputAttachmentArrayDynamicIndexingEXT) = [CapabilityInputAttachment]
  infoCapabilities (CapabilityUniformTexelBufferArrayDynamicIndexing) = [CapabilitySampledBuffer]
  infoCapabilities (CapabilityUniformTexelBufferArrayDynamicIndexingEXT) = [CapabilitySampledBuffer]
  infoCapabilities (CapabilityStorageTexelBufferArrayDynamicIndexing) = [CapabilityImageBuffer]
  infoCapabilities (CapabilityStorageTexelBufferArrayDynamicIndexingEXT) = [CapabilityImageBuffer]
  infoCapabilities (CapabilityUniformBufferArrayNonUniformIndexing) = [CapabilityShaderNonUniform]
  infoCapabilities (CapabilityUniformBufferArrayNonUniformIndexingEXT) = [CapabilityShaderNonUniform]
  infoCapabilities (CapabilitySampledImageArrayNonUniformIndexing) = [CapabilityShaderNonUniform]
  infoCapabilities (CapabilitySampledImageArrayNonUniformIndexingEXT) = [CapabilityShaderNonUniform]
  infoCapabilities (CapabilityStorageBufferArrayNonUniformIndexing) = [CapabilityShaderNonUniform]
  infoCapabilities (CapabilityStorageBufferArrayNonUniformIndexingEXT) = [CapabilityShaderNonUniform]
  infoCapabilities (CapabilityStorageImageArrayNonUniformIndexing) = [CapabilityShaderNonUniform]
  infoCapabilities (CapabilityStorageImageArrayNonUniformIndexingEXT) = [CapabilityShaderNonUniform]
  infoCapabilities (CapabilityInputAttachmentArrayNonUniformIndexing) = [CapabilityInputAttachment, CapabilityShaderNonUniform]
  infoCapabilities (CapabilityInputAttachmentArrayNonUniformIndexingEXT) = [CapabilityInputAttachment, CapabilityShaderNonUniform]
  infoCapabilities (CapabilityUniformTexelBufferArrayNonUniformIndexing) = [CapabilitySampledBuffer, CapabilityShaderNonUniform]
  infoCapabilities (CapabilityUniformTexelBufferArrayNonUniformIndexingEXT) = [CapabilitySampledBuffer, CapabilityShaderNonUniform]
  infoCapabilities (CapabilityStorageTexelBufferArrayNonUniformIndexing) = [CapabilityImageBuffer, CapabilityShaderNonUniform]
  infoCapabilities (CapabilityStorageTexelBufferArrayNonUniformIndexingEXT) = [CapabilityImageBuffer, CapabilityShaderNonUniform]
  infoCapabilities (CapabilityVulkanMemoryModel) = []
  infoCapabilities (CapabilityVulkanMemoryModelKHR) = []
  infoCapabilities (CapabilityVulkanMemoryModelDeviceScope) = []
  infoCapabilities (CapabilityVulkanMemoryModelDeviceScopeKHR) = []
  infoCapabilities (CapabilityPhysicalStorageBufferAddresses) = [CapabilityShader]
  infoCapabilities (CapabilityPhysicalStorageBufferAddressesEXT) = [CapabilityShader]
  infoExtensions (CapabilityMatrix) = []
  infoExtensions (CapabilityShader) = []
  infoExtensions (CapabilityGeometry) = []
  infoExtensions (CapabilityTessellation) = []
  infoExtensions (CapabilityAddresses) = []
  infoExtensions (CapabilityLinkage) = []
  infoExtensions (CapabilityKernel) = []
  infoExtensions (CapabilityVector16) = []
  infoExtensions (CapabilityFloat16Buffer) = []
  infoExtensions (CapabilityFloat16) = []
  infoExtensions (CapabilityFloat64) = []
  infoExtensions (CapabilityInt64) = []
  infoExtensions (CapabilityInt64Atomics) = []
  infoExtensions (CapabilityImageBasic) = []
  infoExtensions (CapabilityImageReadWrite) = []
  infoExtensions (CapabilityImageMipmap) = []
  infoExtensions (CapabilityPipes) = []
  infoExtensions (CapabilityGroups) = ["SPV_AMD_shader_ballot"]
  infoExtensions (CapabilityDeviceEnqueue) = []
  infoExtensions (CapabilityLiteralSampler) = []
  infoExtensions (CapabilityAtomicStorage) = []
  infoExtensions (CapabilityInt16) = []
  infoExtensions (CapabilityTessellationPointSize) = []
  infoExtensions (CapabilityGeometryPointSize) = []
  infoExtensions (CapabilityImageGatherExtended) = []
  infoExtensions (CapabilityStorageImageMultisample) = []
  infoExtensions (CapabilityUniformBufferArrayDynamicIndexing) = []
  infoExtensions (CapabilitySampledImageArrayDynamicIndexing) = []
  infoExtensions (CapabilityStorageBufferArrayDynamicIndexing) = []
  infoExtensions (CapabilityStorageImageArrayDynamicIndexing) = []
  infoExtensions (CapabilityClipDistance) = []
  infoExtensions (CapabilityCullDistance) = []
  infoExtensions (CapabilityImageCubeArray) = []
  infoExtensions (CapabilitySampleRateShading) = []
  infoExtensions (CapabilityImageRect) = []
  infoExtensions (CapabilitySampledRect) = []
  infoExtensions (CapabilityGenericPointer) = []
  infoExtensions (CapabilityInt8) = []
  infoExtensions (CapabilityInputAttachment) = []
  infoExtensions (CapabilitySparseResidency) = []
  infoExtensions (CapabilityMinLod) = []
  infoExtensions (CapabilitySampled1D) = []
  infoExtensions (CapabilityImage1D) = []
  infoExtensions (CapabilitySampledCubeArray) = []
  infoExtensions (CapabilitySampledBuffer) = []
  infoExtensions (CapabilityImageBuffer) = []
  infoExtensions (CapabilityImageMSArray) = []
  infoExtensions (CapabilityStorageImageExtendedFormats) = []
  infoExtensions (CapabilityImageQuery) = []
  infoExtensions (CapabilityDerivativeControl) = []
  infoExtensions (CapabilityInterpolationFunction) = []
  infoExtensions (CapabilityTransformFeedback) = []
  infoExtensions (CapabilityGeometryStreams) = []
  infoExtensions (CapabilityStorageImageReadWithoutFormat) = []
  infoExtensions (CapabilityStorageImageWriteWithoutFormat) = []
  infoExtensions (CapabilityMultiViewport) = []
  infoExtensions (CapabilitySubgroupDispatch) = []
  infoExtensions (CapabilityNamedBarrier) = []
  infoExtensions (CapabilityPipeStorage) = []
  infoExtensions (CapabilityGroupNonUniform) = []
  infoExtensions (CapabilityGroupNonUniformVote) = []
  infoExtensions (CapabilityGroupNonUniformArithmetic) = []
  infoExtensions (CapabilityGroupNonUniformBallot) = []
  infoExtensions (CapabilityGroupNonUniformShuffle) = []
  infoExtensions (CapabilityGroupNonUniformShuffleRelative) = []
  infoExtensions (CapabilityGroupNonUniformClustered) = []
  infoExtensions (CapabilityGroupNonUniformQuad) = []
  infoExtensions (CapabilityShaderLayer) = []
  infoExtensions (CapabilityShaderViewportIndex) = []
  infoExtensions (CapabilityDrawParameters) = ["SPV_KHR_shader_draw_parameters"]
  infoExtensions (CapabilityStorageBuffer16BitAccess) = ["SPV_KHR_16bit_storage"]
  infoExtensions (CapabilityStorageUniformBufferBlock16) = ["SPV_KHR_16bit_storage"]
  infoExtensions (CapabilityUniformAndStorageBuffer16BitAccess) = ["SPV_KHR_16bit_storage"]
  infoExtensions (CapabilityStorageUniform16) = ["SPV_KHR_16bit_storage"]
  infoExtensions (CapabilityStoragePushConstant16) = ["SPV_KHR_16bit_storage"]
  infoExtensions (CapabilityStorageInputOutput16) = ["SPV_KHR_16bit_storage"]
  infoExtensions (CapabilityDeviceGroup) = ["SPV_KHR_device_group"]
  infoExtensions (CapabilityMultiView) = ["SPV_KHR_multiview"]
  infoExtensions (CapabilityVariablePointersStorageBuffer) = ["SPV_KHR_variable_pointers"]
  infoExtensions (CapabilityVariablePointers) = ["SPV_KHR_variable_pointers"]
  infoExtensions (CapabilityStorageBuffer8BitAccess) = ["SPV_KHR_8bit_storage"]
  infoExtensions (CapabilityUniformAndStorageBuffer8BitAccess) = ["SPV_KHR_8bit_storage"]
  infoExtensions (CapabilityStoragePushConstant8) = ["SPV_KHR_8bit_storage"]
  infoExtensions (CapabilityDenormPreserve) = ["SPV_KHR_float_controls"]
  infoExtensions (CapabilityDenormFlushToZero) = ["SPV_KHR_float_controls"]
  infoExtensions (CapabilitySignedZeroInfNanPreserve) = ["SPV_KHR_float_controls"]
  infoExtensions (CapabilityRoundingModeRTE) = ["SPV_KHR_float_controls"]
  infoExtensions (CapabilityRoundingModeRTZ) = ["SPV_KHR_float_controls"]
  infoExtensions (CapabilityShaderNonUniform) = []
  infoExtensions (CapabilityShaderNonUniformEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityRuntimeDescriptorArray) = []
  infoExtensions (CapabilityRuntimeDescriptorArrayEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityInputAttachmentArrayDynamicIndexing) = []
  infoExtensions (CapabilityInputAttachmentArrayDynamicIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityUniformTexelBufferArrayDynamicIndexing) = []
  infoExtensions (CapabilityUniformTexelBufferArrayDynamicIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityStorageTexelBufferArrayDynamicIndexing) = []
  infoExtensions (CapabilityStorageTexelBufferArrayDynamicIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityUniformBufferArrayNonUniformIndexing) = []
  infoExtensions (CapabilityUniformBufferArrayNonUniformIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilitySampledImageArrayNonUniformIndexing) = []
  infoExtensions (CapabilitySampledImageArrayNonUniformIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityStorageBufferArrayNonUniformIndexing) = []
  infoExtensions (CapabilityStorageBufferArrayNonUniformIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityStorageImageArrayNonUniformIndexing) = []
  infoExtensions (CapabilityStorageImageArrayNonUniformIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityInputAttachmentArrayNonUniformIndexing) = []
  infoExtensions (CapabilityInputAttachmentArrayNonUniformIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityUniformTexelBufferArrayNonUniformIndexing) = []
  infoExtensions (CapabilityUniformTexelBufferArrayNonUniformIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityStorageTexelBufferArrayNonUniformIndexing) = []
  infoExtensions (CapabilityStorageTexelBufferArrayNonUniformIndexingEXT) = ["SPV_EXT_descriptor_indexing"]
  infoExtensions (CapabilityVulkanMemoryModel) = []
  infoExtensions (CapabilityVulkanMemoryModelKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (CapabilityVulkanMemoryModelDeviceScope) = []
  infoExtensions (CapabilityVulkanMemoryModelDeviceScopeKHR) = ["SPV_KHR_vulkan_memory_model"]
  infoExtensions (CapabilityPhysicalStorageBufferAddresses) = ["SPV_EXT_physical_storage_buffer", "SPV_KHR_physical_storage_buffer"]
  infoExtensions (CapabilityPhysicalStorageBufferAddressesEXT) = ["SPV_EXT_physical_storage_buffer"]
  infoVersionRange (CapabilityMatrix) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityShader) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityGeometry) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityTessellation) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityAddresses) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityLinkage) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityKernel) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityVector16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityFloat16Buffer) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityFloat16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityFloat64) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityInt64) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityInt64Atomics) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageBasic) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageReadWrite) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageMipmap) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityPipes) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityGroups) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityDeviceEnqueue) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityLiteralSampler) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityAtomicStorage) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityInt16) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityTessellationPointSize) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityGeometryPointSize) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageGatherExtended) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityStorageImageMultisample) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityUniformBufferArrayDynamicIndexing) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilitySampledImageArrayDynamicIndexing) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityStorageBufferArrayDynamicIndexing) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityStorageImageArrayDynamicIndexing) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityClipDistance) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityCullDistance) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageCubeArray) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilitySampleRateShading) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageRect) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilitySampledRect) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityGenericPointer) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityInt8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityInputAttachment) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilitySparseResidency) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityMinLod) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilitySampled1D) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImage1D) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilitySampledCubeArray) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilitySampledBuffer) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageBuffer) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageMSArray) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityStorageImageExtendedFormats) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityImageQuery) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityDerivativeControl) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityInterpolationFunction) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityTransformFeedback) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityGeometryStreams) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityStorageImageReadWithoutFormat) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityStorageImageWriteWithoutFormat) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilityMultiViewport) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (CapabilitySubgroupDispatch) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (CapabilityNamedBarrier) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (CapabilityPipeStorage) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (CapabilityGroupNonUniform) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityGroupNonUniformVote) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityGroupNonUniformArithmetic) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityGroupNonUniformBallot) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityGroupNonUniformShuffle) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityGroupNonUniformShuffleRelative) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityGroupNonUniformClustered) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityGroupNonUniformQuad) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityShaderLayer) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityShaderViewportIndex) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityDrawParameters) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityStorageBuffer16BitAccess) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityStorageUniformBufferBlock16) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityUniformAndStorageBuffer16BitAccess) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityStorageUniform16) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityStoragePushConstant16) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityStorageInputOutput16) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityDeviceGroup) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityMultiView) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityVariablePointersStorageBuffer) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityVariablePointers) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (CapabilityStorageBuffer8BitAccess) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityUniformAndStorageBuffer8BitAccess) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStoragePushConstant8) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityDenormPreserve) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (CapabilityDenormFlushToZero) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (CapabilitySignedZeroInfNanPreserve) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (CapabilityRoundingModeRTE) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (CapabilityRoundingModeRTZ) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (CapabilityShaderNonUniform) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityShaderNonUniformEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityRuntimeDescriptorArray) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityRuntimeDescriptorArrayEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityInputAttachmentArrayDynamicIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityInputAttachmentArrayDynamicIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityUniformTexelBufferArrayDynamicIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityUniformTexelBufferArrayDynamicIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStorageTexelBufferArrayDynamicIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStorageTexelBufferArrayDynamicIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityUniformBufferArrayNonUniformIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityUniformBufferArrayNonUniformIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilitySampledImageArrayNonUniformIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilitySampledImageArrayNonUniformIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStorageBufferArrayNonUniformIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStorageBufferArrayNonUniformIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStorageImageArrayNonUniformIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStorageImageArrayNonUniformIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityInputAttachmentArrayNonUniformIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityInputAttachmentArrayNonUniformIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityUniformTexelBufferArrayNonUniformIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityUniformTexelBufferArrayNonUniformIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStorageTexelBufferArrayNonUniformIndexing) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityStorageTexelBufferArrayNonUniformIndexingEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityVulkanMemoryModel) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityVulkanMemoryModelKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityVulkanMemoryModelDeviceScope) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityVulkanMemoryModelDeviceScopeKHR) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityPhysicalStorageBufferAddresses) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
  infoVersionRange (CapabilityPhysicalStorageBufferAddressesEXT) = SPIRVVersionRange (Just (SPIRVVersion 1 5), Nothing)
instance SPIRVSerialize Capability where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (CapabilityMatrix) = (0, concat [])
      ser (CapabilityShader) = (1, concat [])
      ser (CapabilityGeometry) = (2, concat [])
      ser (CapabilityTessellation) = (3, concat [])
      ser (CapabilityAddresses) = (4, concat [])
      ser (CapabilityLinkage) = (5, concat [])
      ser (CapabilityKernel) = (6, concat [])
      ser (CapabilityVector16) = (7, concat [])
      ser (CapabilityFloat16Buffer) = (8, concat [])
      ser (CapabilityFloat16) = (9, concat [])
      ser (CapabilityFloat64) = (10, concat [])
      ser (CapabilityInt64) = (11, concat [])
      ser (CapabilityInt64Atomics) = (12, concat [])
      ser (CapabilityImageBasic) = (13, concat [])
      ser (CapabilityImageReadWrite) = (14, concat [])
      ser (CapabilityImageMipmap) = (15, concat [])
      ser (CapabilityPipes) = (17, concat [])
      ser (CapabilityGroups) = (18, concat [])
      ser (CapabilityDeviceEnqueue) = (19, concat [])
      ser (CapabilityLiteralSampler) = (20, concat [])
      ser (CapabilityAtomicStorage) = (21, concat [])
      ser (CapabilityInt16) = (22, concat [])
      ser (CapabilityTessellationPointSize) = (23, concat [])
      ser (CapabilityGeometryPointSize) = (24, concat [])
      ser (CapabilityImageGatherExtended) = (25, concat [])
      ser (CapabilityStorageImageMultisample) = (27, concat [])
      ser (CapabilityUniformBufferArrayDynamicIndexing) = (28, concat [])
      ser (CapabilitySampledImageArrayDynamicIndexing) = (29, concat [])
      ser (CapabilityStorageBufferArrayDynamicIndexing) = (30, concat [])
      ser (CapabilityStorageImageArrayDynamicIndexing) = (31, concat [])
      ser (CapabilityClipDistance) = (32, concat [])
      ser (CapabilityCullDistance) = (33, concat [])
      ser (CapabilityImageCubeArray) = (34, concat [])
      ser (CapabilitySampleRateShading) = (35, concat [])
      ser (CapabilityImageRect) = (36, concat [])
      ser (CapabilitySampledRect) = (37, concat [])
      ser (CapabilityGenericPointer) = (38, concat [])
      ser (CapabilityInt8) = (39, concat [])
      ser (CapabilityInputAttachment) = (40, concat [])
      ser (CapabilitySparseResidency) = (41, concat [])
      ser (CapabilityMinLod) = (42, concat [])
      ser (CapabilitySampled1D) = (43, concat [])
      ser (CapabilityImage1D) = (44, concat [])
      ser (CapabilitySampledCubeArray) = (45, concat [])
      ser (CapabilitySampledBuffer) = (46, concat [])
      ser (CapabilityImageBuffer) = (47, concat [])
      ser (CapabilityImageMSArray) = (48, concat [])
      ser (CapabilityStorageImageExtendedFormats) = (49, concat [])
      ser (CapabilityImageQuery) = (50, concat [])
      ser (CapabilityDerivativeControl) = (51, concat [])
      ser (CapabilityInterpolationFunction) = (52, concat [])
      ser (CapabilityTransformFeedback) = (53, concat [])
      ser (CapabilityGeometryStreams) = (54, concat [])
      ser (CapabilityStorageImageReadWithoutFormat) = (55, concat [])
      ser (CapabilityStorageImageWriteWithoutFormat) = (56, concat [])
      ser (CapabilityMultiViewport) = (57, concat [])
      ser (CapabilitySubgroupDispatch) = (58, concat [])
      ser (CapabilityNamedBarrier) = (59, concat [])
      ser (CapabilityPipeStorage) = (60, concat [])
      ser (CapabilityGroupNonUniform) = (61, concat [])
      ser (CapabilityGroupNonUniformVote) = (62, concat [])
      ser (CapabilityGroupNonUniformArithmetic) = (63, concat [])
      ser (CapabilityGroupNonUniformBallot) = (64, concat [])
      ser (CapabilityGroupNonUniformShuffle) = (65, concat [])
      ser (CapabilityGroupNonUniformShuffleRelative) = (66, concat [])
      ser (CapabilityGroupNonUniformClustered) = (67, concat [])
      ser (CapabilityGroupNonUniformQuad) = (68, concat [])
      ser (CapabilityShaderLayer) = (69, concat [])
      ser (CapabilityShaderViewportIndex) = (70, concat [])
      ser (CapabilityDrawParameters) = (4427, concat [])
      ser (CapabilityStorageBuffer16BitAccess) = (4433, concat [])
      ser (CapabilityStorageUniformBufferBlock16) = (4433, concat [])
      ser (CapabilityUniformAndStorageBuffer16BitAccess) = (4434, concat [])
      ser (CapabilityStorageUniform16) = (4434, concat [])
      ser (CapabilityStoragePushConstant16) = (4435, concat [])
      ser (CapabilityStorageInputOutput16) = (4436, concat [])
      ser (CapabilityDeviceGroup) = (4437, concat [])
      ser (CapabilityMultiView) = (4439, concat [])
      ser (CapabilityVariablePointersStorageBuffer) = (4441, concat [])
      ser (CapabilityVariablePointers) = (4442, concat [])
      ser (CapabilityStorageBuffer8BitAccess) = (4448, concat [])
      ser (CapabilityUniformAndStorageBuffer8BitAccess) = (4449, concat [])
      ser (CapabilityStoragePushConstant8) = (4450, concat [])
      ser (CapabilityDenormPreserve) = (4464, concat [])
      ser (CapabilityDenormFlushToZero) = (4465, concat [])
      ser (CapabilitySignedZeroInfNanPreserve) = (4466, concat [])
      ser (CapabilityRoundingModeRTE) = (4467, concat [])
      ser (CapabilityRoundingModeRTZ) = (4468, concat [])
      ser (CapabilityShaderNonUniform) = (5301, concat [])
      ser (CapabilityShaderNonUniformEXT) = (5301, concat [])
      ser (CapabilityRuntimeDescriptorArray) = (5302, concat [])
      ser (CapabilityRuntimeDescriptorArrayEXT) = (5302, concat [])
      ser (CapabilityInputAttachmentArrayDynamicIndexing) = (5303, concat [])
      ser (CapabilityInputAttachmentArrayDynamicIndexingEXT) = (5303, concat [])
      ser (CapabilityUniformTexelBufferArrayDynamicIndexing) = (5304, concat [])
      ser (CapabilityUniformTexelBufferArrayDynamicIndexingEXT) = (5304, concat [])
      ser (CapabilityStorageTexelBufferArrayDynamicIndexing) = (5305, concat [])
      ser (CapabilityStorageTexelBufferArrayDynamicIndexingEXT) = (5305, concat [])
      ser (CapabilityUniformBufferArrayNonUniformIndexing) = (5306, concat [])
      ser (CapabilityUniformBufferArrayNonUniformIndexingEXT) = (5306, concat [])
      ser (CapabilitySampledImageArrayNonUniformIndexing) = (5307, concat [])
      ser (CapabilitySampledImageArrayNonUniformIndexingEXT) = (5307, concat [])
      ser (CapabilityStorageBufferArrayNonUniformIndexing) = (5308, concat [])
      ser (CapabilityStorageBufferArrayNonUniformIndexingEXT) = (5308, concat [])
      ser (CapabilityStorageImageArrayNonUniformIndexing) = (5309, concat [])
      ser (CapabilityStorageImageArrayNonUniformIndexingEXT) = (5309, concat [])
      ser (CapabilityInputAttachmentArrayNonUniformIndexing) = (5310, concat [])
      ser (CapabilityInputAttachmentArrayNonUniformIndexingEXT) = (5310, concat [])
      ser (CapabilityUniformTexelBufferArrayNonUniformIndexing) = (5311, concat [])
      ser (CapabilityUniformTexelBufferArrayNonUniformIndexingEXT) = (5311, concat [])
      ser (CapabilityStorageTexelBufferArrayNonUniformIndexing) = (5312, concat [])
      ser (CapabilityStorageTexelBufferArrayNonUniformIndexingEXT) = (5312, concat [])
      ser (CapabilityVulkanMemoryModel) = (5345, concat [])
      ser (CapabilityVulkanMemoryModelKHR) = (5345, concat [])
      ser (CapabilityVulkanMemoryModelDeviceScope) = (5346, concat [])
      ser (CapabilityVulkanMemoryModelDeviceScopeKHR) = (5346, concat [])
      ser (CapabilityPhysicalStorageBufferAddresses) = (5347, concat [])
      ser (CapabilityPhysicalStorageBufferAddressesEXT) = (5347, concat [])
type IdResultType = Id
type IdResult = Id
type IdMemorySemantics = Id
type IdScope = Id
type IdRef = Id
type LiteralInteger = SPIRVWord
type LiteralString = SPIRVString
type LiteralContextDependentNumber = SPIRVNumber
type LiteralExtInstInteger = SPIRVWord
newtype LiteralSpecConstantOpInteger = LiteralSpecConstantOpInteger Instruction
  deriving (Show, Eq, Ord)
instance SPIRVSerialize LiteralSpecConstantOpInteger where
  spirvSerialize _ = undefined
instance SPIRVInfo LiteralSpecConstantOpInteger where
newtype PairLiteralIntegerIdRef = PairLiteralIntegerIdRef (LiteralInteger, IdRef)
  deriving (Show, Eq, Ord)
instance SPIRVInfo PairLiteralIntegerIdRef where
  infoCapabilities (PairLiteralIntegerIdRef (x0, x1)) = infoCapabilities x0 `union` infoCapabilities x1
  infoExtensions (PairLiteralIntegerIdRef (x0, x1)) = infoExtensions x0 `union` infoExtensions x1
  infoVersionRange (PairLiteralIntegerIdRef (x0, x1)) = infoVersionRange x0 <> infoVersionRange x1
instance SPIRVSerialize PairLiteralIntegerIdRef where
  spirvSerialize (PairLiteralIntegerIdRef (x0, x1)) = concat [spirvSerialize x0, spirvSerialize x1]
newtype PairIdRefLiteralInteger = PairIdRefLiteralInteger (IdRef, LiteralInteger)
  deriving (Show, Eq, Ord)
instance SPIRVInfo PairIdRefLiteralInteger where
  infoCapabilities (PairIdRefLiteralInteger (x0, x1)) = infoCapabilities x0 `union` infoCapabilities x1
  infoExtensions (PairIdRefLiteralInteger (x0, x1)) = infoExtensions x0 `union` infoExtensions x1
  infoVersionRange (PairIdRefLiteralInteger (x0, x1)) = infoVersionRange x0 <> infoVersionRange x1
instance SPIRVSerialize PairIdRefLiteralInteger where
  spirvSerialize (PairIdRefLiteralInteger (x0, x1)) = concat [spirvSerialize x0, spirvSerialize x1]
newtype PairIdRefIdRef = PairIdRefIdRef (IdRef, IdRef)
  deriving (Show, Eq, Ord)
instance SPIRVInfo PairIdRefIdRef where
  infoCapabilities (PairIdRefIdRef (x0, x1)) = infoCapabilities x0 `union` infoCapabilities x1
  infoExtensions (PairIdRefIdRef (x0, x1)) = infoExtensions x0 `union` infoExtensions x1
  infoVersionRange (PairIdRefIdRef (x0, x1)) = infoVersionRange x0 <> infoVersionRange x1
instance SPIRVSerialize PairIdRefIdRef where
  spirvSerialize (PairIdRefIdRef (x0, x1)) = concat [spirvSerialize x0, spirvSerialize x1]
data DebugInfoFlagsBit = DebugInfoFlagsFlagIsProtected | DebugInfoFlagsFlagIsPrivate | DebugInfoFlagsFlagIsPublic | DebugInfoFlagsFlagIsLocal | DebugInfoFlagsFlagIsDefinition | DebugInfoFlagsFlagFwdDecl | DebugInfoFlagsFlagArtificial | DebugInfoFlagsFlagExplicit | DebugInfoFlagsFlagPrototyped | DebugInfoFlagsFlagObjectPointer | DebugInfoFlagsFlagStaticMember | DebugInfoFlagsFlagIndirectVariable | DebugInfoFlagsFlagLValueReference | DebugInfoFlagsFlagRValueReference | DebugInfoFlagsFlagIsOptimized
  deriving (Show, Eq, Ord)
newtype DebugInfoFlags = DebugInfoFlags [DebugInfoFlagsBit]
  deriving (Show, Eq, Ord)
instance SPIRVInfo DebugInfoFlagsBit where
  infoCapabilities (DebugInfoFlagsFlagIsProtected) = []
  infoCapabilities (DebugInfoFlagsFlagIsPrivate) = []
  infoCapabilities (DebugInfoFlagsFlagIsPublic) = []
  infoCapabilities (DebugInfoFlagsFlagIsLocal) = []
  infoCapabilities (DebugInfoFlagsFlagIsDefinition) = []
  infoCapabilities (DebugInfoFlagsFlagFwdDecl) = []
  infoCapabilities (DebugInfoFlagsFlagArtificial) = []
  infoCapabilities (DebugInfoFlagsFlagExplicit) = []
  infoCapabilities (DebugInfoFlagsFlagPrototyped) = []
  infoCapabilities (DebugInfoFlagsFlagObjectPointer) = []
  infoCapabilities (DebugInfoFlagsFlagStaticMember) = []
  infoCapabilities (DebugInfoFlagsFlagIndirectVariable) = []
  infoCapabilities (DebugInfoFlagsFlagLValueReference) = []
  infoCapabilities (DebugInfoFlagsFlagRValueReference) = []
  infoCapabilities (DebugInfoFlagsFlagIsOptimized) = []
  infoExtensions (DebugInfoFlagsFlagIsProtected) = []
  infoExtensions (DebugInfoFlagsFlagIsPrivate) = []
  infoExtensions (DebugInfoFlagsFlagIsPublic) = []
  infoExtensions (DebugInfoFlagsFlagIsLocal) = []
  infoExtensions (DebugInfoFlagsFlagIsDefinition) = []
  infoExtensions (DebugInfoFlagsFlagFwdDecl) = []
  infoExtensions (DebugInfoFlagsFlagArtificial) = []
  infoExtensions (DebugInfoFlagsFlagExplicit) = []
  infoExtensions (DebugInfoFlagsFlagPrototyped) = []
  infoExtensions (DebugInfoFlagsFlagObjectPointer) = []
  infoExtensions (DebugInfoFlagsFlagStaticMember) = []
  infoExtensions (DebugInfoFlagsFlagIndirectVariable) = []
  infoExtensions (DebugInfoFlagsFlagLValueReference) = []
  infoExtensions (DebugInfoFlagsFlagRValueReference) = []
  infoExtensions (DebugInfoFlagsFlagIsOptimized) = []
  infoVersionRange (DebugInfoFlagsFlagIsProtected) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagIsPrivate) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagIsPublic) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagIsLocal) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagIsDefinition) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagFwdDecl) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagArtificial) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagExplicit) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagPrototyped) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagObjectPointer) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagStaticMember) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagIndirectVariable) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagLValueReference) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagRValueReference) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugInfoFlagsFlagIsOptimized) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVInfo DebugInfoFlags where
  infoCapabilities (DebugInfoFlags l) = infoCapabilities l
  infoExtensions (DebugInfoFlags l) = infoExtensions l
  infoVersionRange (DebugInfoFlags l) = infoVersionRange l
instance SPIRVSerialize DebugInfoFlags where
  spirvSerialize (DebugInfoFlags bits) =
    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits
    in foldl (.|.) 0 bs : concat os
    where
      serializeBit :: DebugInfoFlagsBit -> (Word32, [Word32])
      serializeBit (DebugInfoFlagsFlagIsProtected) = (1, concat [])
      serializeBit (DebugInfoFlagsFlagIsPrivate) = (2, concat [])
      serializeBit (DebugInfoFlagsFlagIsPublic) = (3, concat [])
      serializeBit (DebugInfoFlagsFlagIsLocal) = (4, concat [])
      serializeBit (DebugInfoFlagsFlagIsDefinition) = (8, concat [])
      serializeBit (DebugInfoFlagsFlagFwdDecl) = (16, concat [])
      serializeBit (DebugInfoFlagsFlagArtificial) = (32, concat [])
      serializeBit (DebugInfoFlagsFlagExplicit) = (64, concat [])
      serializeBit (DebugInfoFlagsFlagPrototyped) = (128, concat [])
      serializeBit (DebugInfoFlagsFlagObjectPointer) = (256, concat [])
      serializeBit (DebugInfoFlagsFlagStaticMember) = (512, concat [])
      serializeBit (DebugInfoFlagsFlagIndirectVariable) = (1024, concat [])
      serializeBit (DebugInfoFlagsFlagLValueReference) = (2048, concat [])
      serializeBit (DebugInfoFlagsFlagRValueReference) = (4096, concat [])
      serializeBit (DebugInfoFlagsFlagIsOptimized) = (8192, concat [])
data DebugBaseTypeAttributeEncoding = DebugBaseTypeAttributeEncodingUnspecified | DebugBaseTypeAttributeEncodingAddress | DebugBaseTypeAttributeEncodingBoolean | DebugBaseTypeAttributeEncodingFloat | DebugBaseTypeAttributeEncodingSigned | DebugBaseTypeAttributeEncodingSignedChar | DebugBaseTypeAttributeEncodingUnsigned | DebugBaseTypeAttributeEncodingUnsignedChar
  deriving (Show, Eq, Ord)
instance SPIRVInfo DebugBaseTypeAttributeEncoding where
  infoCapabilities (DebugBaseTypeAttributeEncodingUnspecified) = []
  infoCapabilities (DebugBaseTypeAttributeEncodingAddress) = []
  infoCapabilities (DebugBaseTypeAttributeEncodingBoolean) = []
  infoCapabilities (DebugBaseTypeAttributeEncodingFloat) = []
  infoCapabilities (DebugBaseTypeAttributeEncodingSigned) = []
  infoCapabilities (DebugBaseTypeAttributeEncodingSignedChar) = []
  infoCapabilities (DebugBaseTypeAttributeEncodingUnsigned) = []
  infoCapabilities (DebugBaseTypeAttributeEncodingUnsignedChar) = []
  infoExtensions (DebugBaseTypeAttributeEncodingUnspecified) = []
  infoExtensions (DebugBaseTypeAttributeEncodingAddress) = []
  infoExtensions (DebugBaseTypeAttributeEncodingBoolean) = []
  infoExtensions (DebugBaseTypeAttributeEncodingFloat) = []
  infoExtensions (DebugBaseTypeAttributeEncodingSigned) = []
  infoExtensions (DebugBaseTypeAttributeEncodingSignedChar) = []
  infoExtensions (DebugBaseTypeAttributeEncodingUnsigned) = []
  infoExtensions (DebugBaseTypeAttributeEncodingUnsignedChar) = []
  infoVersionRange (DebugBaseTypeAttributeEncodingUnspecified) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugBaseTypeAttributeEncodingAddress) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugBaseTypeAttributeEncodingBoolean) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugBaseTypeAttributeEncodingFloat) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugBaseTypeAttributeEncodingSigned) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugBaseTypeAttributeEncodingSignedChar) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugBaseTypeAttributeEncodingUnsigned) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugBaseTypeAttributeEncodingUnsignedChar) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize DebugBaseTypeAttributeEncoding where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (DebugBaseTypeAttributeEncodingUnspecified) = (0, concat [])
      ser (DebugBaseTypeAttributeEncodingAddress) = (1, concat [])
      ser (DebugBaseTypeAttributeEncodingBoolean) = (2, concat [])
      ser (DebugBaseTypeAttributeEncodingFloat) = (4, concat [])
      ser (DebugBaseTypeAttributeEncodingSigned) = (5, concat [])
      ser (DebugBaseTypeAttributeEncodingSignedChar) = (6, concat [])
      ser (DebugBaseTypeAttributeEncodingUnsigned) = (7, concat [])
      ser (DebugBaseTypeAttributeEncodingUnsignedChar) = (8, concat [])
data DebugCompositeType = DebugCompositeTypeClass | DebugCompositeTypeStructure | DebugCompositeTypeUnion
  deriving (Show, Eq, Ord)
instance SPIRVInfo DebugCompositeType where
  infoCapabilities (DebugCompositeTypeClass) = []
  infoCapabilities (DebugCompositeTypeStructure) = []
  infoCapabilities (DebugCompositeTypeUnion) = []
  infoExtensions (DebugCompositeTypeClass) = []
  infoExtensions (DebugCompositeTypeStructure) = []
  infoExtensions (DebugCompositeTypeUnion) = []
  infoVersionRange (DebugCompositeTypeClass) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugCompositeTypeStructure) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugCompositeTypeUnion) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize DebugCompositeType where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (DebugCompositeTypeClass) = (0, concat [])
      ser (DebugCompositeTypeStructure) = (1, concat [])
      ser (DebugCompositeTypeUnion) = (2, concat [])
data DebugTypeQualifier = DebugTypeQualifierConstType | DebugTypeQualifierVolatileType | DebugTypeQualifierRestrictType
  deriving (Show, Eq, Ord)
instance SPIRVInfo DebugTypeQualifier where
  infoCapabilities (DebugTypeQualifierConstType) = []
  infoCapabilities (DebugTypeQualifierVolatileType) = []
  infoCapabilities (DebugTypeQualifierRestrictType) = []
  infoExtensions (DebugTypeQualifierConstType) = []
  infoExtensions (DebugTypeQualifierVolatileType) = []
  infoExtensions (DebugTypeQualifierRestrictType) = []
  infoVersionRange (DebugTypeQualifierConstType) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugTypeQualifierVolatileType) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugTypeQualifierRestrictType) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize DebugTypeQualifier where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (DebugTypeQualifierConstType) = (0, concat [])
      ser (DebugTypeQualifierVolatileType) = (1, concat [])
      ser (DebugTypeQualifierRestrictType) = (2, concat [])
data DebugOperation = DebugOperationDeref | DebugOperationPlus | DebugOperationMinus | DebugOperationPlusUconst LiteralInteger | DebugOperationBitPiece LiteralInteger LiteralInteger | DebugOperationSwap | DebugOperationXderef | DebugOperationStackValue | DebugOperationConstu LiteralInteger
  deriving (Show, Eq, Ord)
instance SPIRVInfo DebugOperation where
  infoCapabilities (DebugOperationDeref) = []
  infoCapabilities (DebugOperationPlus) = []
  infoCapabilities (DebugOperationMinus) = []
  infoCapabilities (DebugOperationPlusUconst x0) = []
  infoCapabilities (DebugOperationBitPiece x0 x1) = []
  infoCapabilities (DebugOperationSwap) = []
  infoCapabilities (DebugOperationXderef) = []
  infoCapabilities (DebugOperationStackValue) = []
  infoCapabilities (DebugOperationConstu x0) = []
  infoExtensions (DebugOperationDeref) = []
  infoExtensions (DebugOperationPlus) = []
  infoExtensions (DebugOperationMinus) = []
  infoExtensions (DebugOperationPlusUconst x0) = []
  infoExtensions (DebugOperationBitPiece x0 x1) = []
  infoExtensions (DebugOperationSwap) = []
  infoExtensions (DebugOperationXderef) = []
  infoExtensions (DebugOperationStackValue) = []
  infoExtensions (DebugOperationConstu x0) = []
  infoVersionRange (DebugOperationDeref) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugOperationPlus) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugOperationMinus) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugOperationPlusUconst x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugOperationBitPiece x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugOperationSwap) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugOperationXderef) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugOperationStackValue) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DebugOperationConstu x0) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize DebugOperation where
  spirvSerialize v = uncurry (:) $ ser v
    where
      ser (DebugOperationDeref) = (0, concat [])
      ser (DebugOperationPlus) = (1, concat [])
      ser (DebugOperationMinus) = (2, concat [])
      ser (DebugOperationPlusUconst x0) = (3, concat [spirvSerialize x0])
      ser (DebugOperationBitPiece x0 x1) = (4, concat [spirvSerialize x0, spirvSerialize x1])
      ser (DebugOperationSwap) = (5, concat [])
      ser (DebugOperationXderef) = (6, concat [])
      ser (DebugOperationStackValue) = (7, concat [])
      ser (DebugOperationConstu x0) = (8, concat [spirvSerialize x0])
data Instruction = OpNop | OpUndef IdResultType IdResult | OpSourceContinued LiteralString | OpSource SourceLanguage LiteralInteger (Maybe IdRef) (Maybe LiteralString) | OpSourceExtension LiteralString | OpName IdRef LiteralString | OpMemberName IdRef LiteralInteger LiteralString | OpString LiteralString IdResult | OpLine IdRef LiteralInteger LiteralInteger | OpExtension LiteralString | OpExtInstImport LiteralString IdResult | OpExtInst IdRef ExtInstruction IdResultType IdResult | OpMemoryModel AddressingModel MemoryModel | OpEntryPoint ExecutionModel IdRef LiteralString [IdRef] | OpExecutionMode IdRef ExecutionMode | OpCapability Capability | OpTypeVoid IdResult | OpTypeBool IdResult | OpTypeInt LiteralInteger LiteralInteger IdResult | OpTypeFloat LiteralInteger IdResult | OpTypeVector IdRef LiteralInteger IdResult | OpTypeMatrix IdRef LiteralInteger IdResult | OpTypeImage IdRef Dim LiteralInteger LiteralInteger LiteralInteger LiteralInteger ImageFormat (Maybe AccessQualifier) IdResult | OpTypeSampler IdResult | OpTypeSampledImage IdRef IdResult | OpTypeArray IdRef IdRef IdResult | OpTypeRuntimeArray IdRef IdResult | OpTypeStruct [IdRef] IdResult | OpTypeOpaque LiteralString IdResult | OpTypePointer StorageClass IdRef IdResult | OpTypeFunction IdRef [IdRef] IdResult | OpTypeEvent IdResult | OpTypeDeviceEvent IdResult | OpTypeReserveId IdResult | OpTypeQueue IdResult | OpTypePipe AccessQualifier IdResult | OpTypeForwardPointer IdRef StorageClass | OpConstantTrue IdResultType IdResult | OpConstantFalse IdResultType IdResult | OpConstant LiteralContextDependentNumber IdResultType IdResult | OpConstantComposite [IdRef] IdResultType IdResult | OpConstantSampler SamplerAddressingMode LiteralInteger SamplerFilterMode IdResultType IdResult | OpConstantNull IdResultType IdResult | OpSpecConstantTrue IdResultType IdResult | OpSpecConstantFalse IdResultType IdResult | OpSpecConstant LiteralContextDependentNumber IdResultType IdResult | OpSpecConstantComposite [IdRef] IdResultType IdResult | OpSpecConstantOp LiteralSpecConstantOpInteger IdResultType IdResult | OpFunction FunctionControl IdRef IdResultType IdResult | OpFunctionParameter IdResultType IdResult | OpFunctionEnd | OpFunctionCall IdRef [IdRef] IdResultType IdResult | OpVariable StorageClass (Maybe IdRef) IdResultType IdResult | OpImageTexelPointer IdRef IdRef IdRef IdResultType IdResult | OpLoad IdRef (Maybe MemoryAccess) IdResultType IdResult | OpStore IdRef IdRef (Maybe MemoryAccess) | OpCopyMemory IdRef IdRef (Maybe MemoryAccess) (Maybe MemoryAccess) | OpCopyMemorySized IdRef IdRef IdRef (Maybe MemoryAccess) (Maybe MemoryAccess) | OpAccessChain IdRef [IdRef] IdResultType IdResult | OpInBoundsAccessChain IdRef [IdRef] IdResultType IdResult | OpPtrAccessChain IdRef IdRef [IdRef] IdResultType IdResult | OpArrayLength IdRef LiteralInteger IdResultType IdResult | OpGenericPtrMemSemantics IdRef IdResultType IdResult | OpInBoundsPtrAccessChain IdRef IdRef [IdRef] IdResultType IdResult | OpDecorate IdRef Decoration | OpMemberDecorate IdRef LiteralInteger Decoration | OpDecorationGroup IdResult | OpGroupDecorate IdRef [IdRef] | OpGroupMemberDecorate IdRef [PairIdRefLiteralInteger] | OpVectorExtractDynamic IdRef IdRef IdResultType IdResult | OpVectorInsertDynamic IdRef IdRef IdRef IdResultType IdResult | OpVectorShuffle IdRef IdRef [LiteralInteger] IdResultType IdResult | OpCompositeConstruct [IdRef] IdResultType IdResult | OpCompositeExtract IdRef [LiteralInteger] IdResultType IdResult | OpCompositeInsert IdRef IdRef [LiteralInteger] IdResultType IdResult | OpCopyObject IdRef IdResultType IdResult | OpTranspose IdRef IdResultType IdResult | OpSampledImage IdRef IdRef IdResultType IdResult | OpImageSampleImplicitLod IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSampleExplicitLod IdRef IdRef ImageOperands IdResultType IdResult | OpImageSampleDrefImplicitLod IdRef IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSampleDrefExplicitLod IdRef IdRef IdRef ImageOperands IdResultType IdResult | OpImageSampleProjImplicitLod IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSampleProjExplicitLod IdRef IdRef ImageOperands IdResultType IdResult | OpImageSampleProjDrefImplicitLod IdRef IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSampleProjDrefExplicitLod IdRef IdRef IdRef ImageOperands IdResultType IdResult | OpImageFetch IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageGather IdRef IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageDrefGather IdRef IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageRead IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageWrite IdRef IdRef IdRef (Maybe ImageOperands) | OpImage IdRef IdResultType IdResult | OpImageQueryFormat IdRef IdResultType IdResult | OpImageQueryOrder IdRef IdResultType IdResult | OpImageQuerySizeLod IdRef IdRef IdResultType IdResult | OpImageQuerySize IdRef IdResultType IdResult | OpImageQueryLod IdRef IdRef IdResultType IdResult | OpImageQueryLevels IdRef IdResultType IdResult | OpImageQuerySamples IdRef IdResultType IdResult | OpConvertFToU IdRef IdResultType IdResult | OpConvertFToS IdRef IdResultType IdResult | OpConvertSToF IdRef IdResultType IdResult | OpConvertUToF IdRef IdResultType IdResult | OpUConvert IdRef IdResultType IdResult | OpSConvert IdRef IdResultType IdResult | OpFConvert IdRef IdResultType IdResult | OpQuantizeToF16 IdRef IdResultType IdResult | OpConvertPtrToU IdRef IdResultType IdResult | OpSatConvertSToU IdRef IdResultType IdResult | OpSatConvertUToS IdRef IdResultType IdResult | OpConvertUToPtr IdRef IdResultType IdResult | OpPtrCastToGeneric IdRef IdResultType IdResult | OpGenericCastToPtr IdRef IdResultType IdResult | OpGenericCastToPtrExplicit IdRef StorageClass IdResultType IdResult | OpBitcast IdRef IdResultType IdResult | OpSNegate IdRef IdResultType IdResult | OpFNegate IdRef IdResultType IdResult | OpIAdd IdRef IdRef IdResultType IdResult | OpFAdd IdRef IdRef IdResultType IdResult | OpISub IdRef IdRef IdResultType IdResult | OpFSub IdRef IdRef IdResultType IdResult | OpIMul IdRef IdRef IdResultType IdResult | OpFMul IdRef IdRef IdResultType IdResult | OpUDiv IdRef IdRef IdResultType IdResult | OpSDiv IdRef IdRef IdResultType IdResult | OpFDiv IdRef IdRef IdResultType IdResult | OpUMod IdRef IdRef IdResultType IdResult | OpSRem IdRef IdRef IdResultType IdResult | OpSMod IdRef IdRef IdResultType IdResult | OpFRem IdRef IdRef IdResultType IdResult | OpFMod IdRef IdRef IdResultType IdResult | OpVectorTimesScalar IdRef IdRef IdResultType IdResult | OpMatrixTimesScalar IdRef IdRef IdResultType IdResult | OpVectorTimesMatrix IdRef IdRef IdResultType IdResult | OpMatrixTimesVector IdRef IdRef IdResultType IdResult | OpMatrixTimesMatrix IdRef IdRef IdResultType IdResult | OpOuterProduct IdRef IdRef IdResultType IdResult | OpDot IdRef IdRef IdResultType IdResult | OpIAddCarry IdRef IdRef IdResultType IdResult | OpISubBorrow IdRef IdRef IdResultType IdResult | OpUMulExtended IdRef IdRef IdResultType IdResult | OpSMulExtended IdRef IdRef IdResultType IdResult | OpAny IdRef IdResultType IdResult | OpAll IdRef IdResultType IdResult | OpIsNan IdRef IdResultType IdResult | OpIsInf IdRef IdResultType IdResult | OpIsFinite IdRef IdResultType IdResult | OpIsNormal IdRef IdResultType IdResult | OpSignBitSet IdRef IdResultType IdResult | OpLessOrGreater IdRef IdRef IdResultType IdResult | OpOrdered IdRef IdRef IdResultType IdResult | OpUnordered IdRef IdRef IdResultType IdResult | OpLogicalEqual IdRef IdRef IdResultType IdResult | OpLogicalNotEqual IdRef IdRef IdResultType IdResult | OpLogicalOr IdRef IdRef IdResultType IdResult | OpLogicalAnd IdRef IdRef IdResultType IdResult | OpLogicalNot IdRef IdResultType IdResult | OpSelect IdRef IdRef IdRef IdResultType IdResult | OpIEqual IdRef IdRef IdResultType IdResult | OpINotEqual IdRef IdRef IdResultType IdResult | OpUGreaterThan IdRef IdRef IdResultType IdResult | OpSGreaterThan IdRef IdRef IdResultType IdResult | OpUGreaterThanEqual IdRef IdRef IdResultType IdResult | OpSGreaterThanEqual IdRef IdRef IdResultType IdResult | OpULessThan IdRef IdRef IdResultType IdResult | OpSLessThan IdRef IdRef IdResultType IdResult | OpULessThanEqual IdRef IdRef IdResultType IdResult | OpSLessThanEqual IdRef IdRef IdResultType IdResult | OpFOrdEqual IdRef IdRef IdResultType IdResult | OpFUnordEqual IdRef IdRef IdResultType IdResult | OpFOrdNotEqual IdRef IdRef IdResultType IdResult | OpFUnordNotEqual IdRef IdRef IdResultType IdResult | OpFOrdLessThan IdRef IdRef IdResultType IdResult | OpFUnordLessThan IdRef IdRef IdResultType IdResult | OpFOrdGreaterThan IdRef IdRef IdResultType IdResult | OpFUnordGreaterThan IdRef IdRef IdResultType IdResult | OpFOrdLessThanEqual IdRef IdRef IdResultType IdResult | OpFUnordLessThanEqual IdRef IdRef IdResultType IdResult | OpFOrdGreaterThanEqual IdRef IdRef IdResultType IdResult | OpFUnordGreaterThanEqual IdRef IdRef IdResultType IdResult | OpShiftRightLogical IdRef IdRef IdResultType IdResult | OpShiftRightArithmetic IdRef IdRef IdResultType IdResult | OpShiftLeftLogical IdRef IdRef IdResultType IdResult | OpBitwiseOr IdRef IdRef IdResultType IdResult | OpBitwiseXor IdRef IdRef IdResultType IdResult | OpBitwiseAnd IdRef IdRef IdResultType IdResult | OpNot IdRef IdResultType IdResult | OpBitFieldInsert IdRef IdRef IdRef IdRef IdResultType IdResult | OpBitFieldSExtract IdRef IdRef IdRef IdResultType IdResult | OpBitFieldUExtract IdRef IdRef IdRef IdResultType IdResult | OpBitReverse IdRef IdResultType IdResult | OpBitCount IdRef IdResultType IdResult | OpDPdx IdRef IdResultType IdResult | OpDPdy IdRef IdResultType IdResult | OpFwidth IdRef IdResultType IdResult | OpDPdxFine IdRef IdResultType IdResult | OpDPdyFine IdRef IdResultType IdResult | OpFwidthFine IdRef IdResultType IdResult | OpDPdxCoarse IdRef IdResultType IdResult | OpDPdyCoarse IdRef IdResultType IdResult | OpFwidthCoarse IdRef IdResultType IdResult | OpEmitVertex | OpEndPrimitive | OpEmitStreamVertex IdRef | OpEndStreamPrimitive IdRef | OpControlBarrier IdScope IdScope IdMemorySemantics | OpMemoryBarrier IdScope IdMemorySemantics | OpAtomicLoad IdRef IdScope IdMemorySemantics IdResultType IdResult | OpAtomicStore IdRef IdScope IdMemorySemantics IdRef | OpAtomicExchange IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicCompareExchange IdRef IdScope IdMemorySemantics IdMemorySemantics IdRef IdRef IdResultType IdResult | OpAtomicCompareExchangeWeak IdRef IdScope IdMemorySemantics IdMemorySemantics IdRef IdRef IdResultType IdResult | OpAtomicIIncrement IdRef IdScope IdMemorySemantics IdResultType IdResult | OpAtomicIDecrement IdRef IdScope IdMemorySemantics IdResultType IdResult | OpAtomicIAdd IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicISub IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicSMin IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicUMin IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicSMax IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicUMax IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicAnd IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicOr IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpAtomicXor IdRef IdScope IdMemorySemantics IdRef IdResultType IdResult | OpPhi [PairIdRefIdRef] IdResultType IdResult | OpLoopMerge IdRef IdRef LoopControl | OpSelectionMerge IdRef SelectionControl | OpLabel IdResult | OpBranch IdRef | OpBranchConditional IdRef IdRef IdRef [LiteralInteger] | OpSwitch IdRef IdRef [PairLiteralIntegerIdRef] | OpKill | OpReturn | OpReturnValue IdRef | OpUnreachable | OpLifetimeStart IdRef LiteralInteger | OpLifetimeStop IdRef LiteralInteger | OpGroupAsyncCopy IdScope IdRef IdRef IdRef IdRef IdRef IdResultType IdResult | OpGroupWaitEvents IdScope IdRef IdRef | OpGroupAll IdScope IdRef IdResultType IdResult | OpGroupAny IdScope IdRef IdResultType IdResult | OpGroupBroadcast IdScope IdRef IdRef IdResultType IdResult | OpGroupIAdd IdScope GroupOperation IdRef IdResultType IdResult | OpGroupFAdd IdScope GroupOperation IdRef IdResultType IdResult | OpGroupFMin IdScope GroupOperation IdRef IdResultType IdResult | OpGroupUMin IdScope GroupOperation IdRef IdResultType IdResult | OpGroupSMin IdScope GroupOperation IdRef IdResultType IdResult | OpGroupFMax IdScope GroupOperation IdRef IdResultType IdResult | OpGroupUMax IdScope GroupOperation IdRef IdResultType IdResult | OpGroupSMax IdScope GroupOperation IdRef IdResultType IdResult | OpReadPipe IdRef IdRef IdRef IdRef IdResultType IdResult | OpWritePipe IdRef IdRef IdRef IdRef IdResultType IdResult | OpReservedReadPipe IdRef IdRef IdRef IdRef IdRef IdRef IdResultType IdResult | OpReservedWritePipe IdRef IdRef IdRef IdRef IdRef IdRef IdResultType IdResult | OpReserveReadPipePackets IdRef IdRef IdRef IdRef IdResultType IdResult | OpReserveWritePipePackets IdRef IdRef IdRef IdRef IdResultType IdResult | OpCommitReadPipe IdRef IdRef IdRef IdRef | OpCommitWritePipe IdRef IdRef IdRef IdRef | OpIsValidReserveId IdRef IdResultType IdResult | OpGetNumPipePackets IdRef IdRef IdRef IdResultType IdResult | OpGetMaxPipePackets IdRef IdRef IdRef IdResultType IdResult | OpGroupReserveReadPipePackets IdScope IdRef IdRef IdRef IdRef IdResultType IdResult | OpGroupReserveWritePipePackets IdScope IdRef IdRef IdRef IdRef IdResultType IdResult | OpGroupCommitReadPipe IdScope IdRef IdRef IdRef IdRef | OpGroupCommitWritePipe IdScope IdRef IdRef IdRef IdRef | OpEnqueueMarker IdRef IdRef IdRef IdRef IdResultType IdResult | OpEnqueueKernel IdRef IdRef IdRef IdRef IdRef IdRef IdRef IdRef IdRef IdRef [IdRef] IdResultType IdResult | OpGetKernelNDrangeSubGroupCount IdRef IdRef IdRef IdRef IdRef IdResultType IdResult | OpGetKernelNDrangeMaxSubGroupSize IdRef IdRef IdRef IdRef IdRef IdResultType IdResult | OpGetKernelWorkGroupSize IdRef IdRef IdRef IdRef IdResultType IdResult | OpGetKernelPreferredWorkGroupSizeMultiple IdRef IdRef IdRef IdRef IdResultType IdResult | OpRetainEvent IdRef | OpReleaseEvent IdRef | OpCreateUserEvent IdResultType IdResult | OpIsValidEvent IdRef IdResultType IdResult | OpSetUserEventStatus IdRef IdRef | OpCaptureEventProfilingInfo IdRef IdRef IdRef | OpGetDefaultQueue IdResultType IdResult | OpBuildNDRange IdRef IdRef IdRef IdResultType IdResult | OpImageSparseSampleImplicitLod IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSparseSampleExplicitLod IdRef IdRef ImageOperands IdResultType IdResult | OpImageSparseSampleDrefImplicitLod IdRef IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSparseSampleDrefExplicitLod IdRef IdRef IdRef ImageOperands IdResultType IdResult | OpImageSparseFetch IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSparseGather IdRef IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSparseDrefGather IdRef IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpImageSparseTexelsResident IdRef IdResultType IdResult | OpNoLine | OpAtomicFlagTestAndSet IdRef IdScope IdMemorySemantics IdResultType IdResult | OpAtomicFlagClear IdRef IdScope IdMemorySemantics | OpImageSparseRead IdRef IdRef (Maybe ImageOperands) IdResultType IdResult | OpSizeOf IdRef IdResultType IdResult | OpTypePipeStorage IdResult | OpConstantPipeStorage LiteralInteger LiteralInteger LiteralInteger IdResultType IdResult | OpCreatePipeFromPipeStorage IdRef IdResultType IdResult | OpGetKernelLocalSizeForSubgroupCount IdRef IdRef IdRef IdRef IdRef IdResultType IdResult | OpGetKernelMaxNumSubgroups IdRef IdRef IdRef IdRef IdResultType IdResult | OpTypeNamedBarrier IdResult | OpNamedBarrierInitialize IdRef IdResultType IdResult | OpMemoryNamedBarrier IdRef IdScope IdMemorySemantics | OpModuleProcessed LiteralString | OpExecutionModeId IdRef ExecutionMode | OpDecorateId IdRef Decoration | OpGroupNonUniformElect IdScope IdResultType IdResult | OpGroupNonUniformAll IdScope IdRef IdResultType IdResult | OpGroupNonUniformAny IdScope IdRef IdResultType IdResult | OpGroupNonUniformAllEqual IdScope IdRef IdResultType IdResult | OpGroupNonUniformBroadcast IdScope IdRef IdRef IdResultType IdResult | OpGroupNonUniformBroadcastFirst IdScope IdRef IdResultType IdResult | OpGroupNonUniformBallot IdScope IdRef IdResultType IdResult | OpGroupNonUniformInverseBallot IdScope IdRef IdResultType IdResult | OpGroupNonUniformBallotBitExtract IdScope IdRef IdRef IdResultType IdResult | OpGroupNonUniformBallotBitCount IdScope GroupOperation IdRef IdResultType IdResult | OpGroupNonUniformBallotFindLSB IdScope IdRef IdResultType IdResult | OpGroupNonUniformBallotFindMSB IdScope IdRef IdResultType IdResult | OpGroupNonUniformShuffle IdScope IdRef IdRef IdResultType IdResult | OpGroupNonUniformShuffleXor IdScope IdRef IdRef IdResultType IdResult | OpGroupNonUniformShuffleUp IdScope IdRef IdRef IdResultType IdResult | OpGroupNonUniformShuffleDown IdScope IdRef IdRef IdResultType IdResult | OpGroupNonUniformIAdd IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformFAdd IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformIMul IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformFMul IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformSMin IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformUMin IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformFMin IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformSMax IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformUMax IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformFMax IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformBitwiseAnd IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformBitwiseOr IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformBitwiseXor IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformLogicalAnd IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformLogicalOr IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformLogicalXor IdScope GroupOperation IdRef (Maybe IdRef) IdResultType IdResult | OpGroupNonUniformQuadBroadcast IdScope IdRef IdRef IdResultType IdResult | OpGroupNonUniformQuadSwap IdScope IdRef IdRef IdResultType IdResult | OpCopyLogical IdRef IdResultType IdResult | OpPtrEqual IdRef IdRef IdResultType IdResult | OpPtrNotEqual IdRef IdRef IdResultType IdResult | OpPtrDiff IdRef IdRef IdResultType IdResult | OpDecorateString IdRef Decoration | OpDecorateStringGOOGLE IdRef Decoration | OpMemberDecorateString IdRef LiteralInteger Decoration | OpMemberDecorateStringGOOGLE IdRef LiteralInteger Decoration
  deriving (Show, Eq, Ord)
instance SPIRVInfo Instruction where
  infoCapabilities (OpNop) = []
  infoCapabilities (OpUndef x0 x1) = []
  infoCapabilities (OpSourceContinued x0) = []
  infoCapabilities (OpSource x0 x1 x2 x3) = []
  infoCapabilities (OpSourceExtension x0) = []
  infoCapabilities (OpName x0 x1) = []
  infoCapabilities (OpMemberName x0 x1 x2) = []
  infoCapabilities (OpString x1 x0) = []
  infoCapabilities (OpLine x0 x1 x2) = []
  infoCapabilities (OpExtension x0) = []
  infoCapabilities (OpExtInstImport x1 x0) = []
  infoCapabilities (OpExtInst x2 x3 x0 x1) = []
  infoCapabilities (OpMemoryModel x0 x1) = []
  infoCapabilities (OpEntryPoint x0 x1 x2 x3) = []
  infoCapabilities (OpExecutionMode x0 x1) = []
  infoCapabilities (OpCapability x0) = []
  infoCapabilities (OpTypeVoid x0) = []
  infoCapabilities (OpTypeBool x0) = []
  infoCapabilities (OpTypeInt x1 x2 x0) = []
  infoCapabilities (OpTypeFloat x1 x0) = []
  infoCapabilities (OpTypeVector x1 x2 x0) = []
  infoCapabilities (OpTypeMatrix x1 x2 x0) = [CapabilityMatrix]
  infoCapabilities (OpTypeImage x1 x2 x3 x4 x5 x6 x7 x8 x0) = []
  infoCapabilities (OpTypeSampler x0) = []
  infoCapabilities (OpTypeSampledImage x1 x0) = []
  infoCapabilities (OpTypeArray x1 x2 x0) = []
  infoCapabilities (OpTypeRuntimeArray x1 x0) = [CapabilityShader]
  infoCapabilities (OpTypeStruct x1 x0) = []
  infoCapabilities (OpTypeOpaque x1 x0) = [CapabilityKernel]
  infoCapabilities (OpTypePointer x1 x2 x0) = []
  infoCapabilities (OpTypeFunction x1 x2 x0) = []
  infoCapabilities (OpTypeEvent x0) = [CapabilityKernel]
  infoCapabilities (OpTypeDeviceEvent x0) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpTypeReserveId x0) = [CapabilityPipes]
  infoCapabilities (OpTypeQueue x0) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpTypePipe x1 x0) = [CapabilityPipes]
  infoCapabilities (OpTypeForwardPointer x0 x1) = [CapabilityAddresses, CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (OpConstantTrue x0 x1) = []
  infoCapabilities (OpConstantFalse x0 x1) = []
  infoCapabilities (OpConstant x2 x0 x1) = []
  infoCapabilities (OpConstantComposite x2 x0 x1) = []
  infoCapabilities (OpConstantSampler x2 x3 x4 x0 x1) = [CapabilityLiteralSampler]
  infoCapabilities (OpConstantNull x0 x1) = []
  infoCapabilities (OpSpecConstantTrue x0 x1) = []
  infoCapabilities (OpSpecConstantFalse x0 x1) = []
  infoCapabilities (OpSpecConstant x2 x0 x1) = []
  infoCapabilities (OpSpecConstantComposite x2 x0 x1) = []
  infoCapabilities (OpSpecConstantOp x2 x0 x1) = []
  infoCapabilities (OpFunction x2 x3 x0 x1) = []
  infoCapabilities (OpFunctionParameter x0 x1) = []
  infoCapabilities (OpFunctionEnd) = []
  infoCapabilities (OpFunctionCall x2 x3 x0 x1) = []
  infoCapabilities (OpVariable x2 x3 x0 x1) = []
  infoCapabilities (OpImageTexelPointer x2 x3 x4 x0 x1) = []
  infoCapabilities (OpLoad x2 x3 x0 x1) = []
  infoCapabilities (OpStore x0 x1 x2) = []
  infoCapabilities (OpCopyMemory x0 x1 x2 x3) = []
  infoCapabilities (OpCopyMemorySized x0 x1 x2 x3 x4) = [CapabilityAddresses]
  infoCapabilities (OpAccessChain x2 x3 x0 x1) = []
  infoCapabilities (OpInBoundsAccessChain x2 x3 x0 x1) = []
  infoCapabilities (OpPtrAccessChain x2 x3 x4 x0 x1) = [CapabilityAddresses, CapabilityVariablePointers, CapabilityVariablePointersStorageBuffer, CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (OpArrayLength x2 x3 x0 x1) = [CapabilityShader]
  infoCapabilities (OpGenericPtrMemSemantics x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpInBoundsPtrAccessChain x2 x3 x4 x0 x1) = [CapabilityAddresses]
  infoCapabilities (OpDecorate x0 x1) = []
  infoCapabilities (OpMemberDecorate x0 x1 x2) = []
  infoCapabilities (OpDecorationGroup x0) = []
  infoCapabilities (OpGroupDecorate x0 x1) = []
  infoCapabilities (OpGroupMemberDecorate x0 x1) = []
  infoCapabilities (OpVectorExtractDynamic x2 x3 x0 x1) = []
  infoCapabilities (OpVectorInsertDynamic x2 x3 x4 x0 x1) = []
  infoCapabilities (OpVectorShuffle x2 x3 x4 x0 x1) = []
  infoCapabilities (OpCompositeConstruct x2 x0 x1) = []
  infoCapabilities (OpCompositeExtract x2 x3 x0 x1) = []
  infoCapabilities (OpCompositeInsert x2 x3 x4 x0 x1) = []
  infoCapabilities (OpCopyObject x2 x0 x1) = []
  infoCapabilities (OpTranspose x2 x0 x1) = [CapabilityMatrix]
  infoCapabilities (OpSampledImage x2 x3 x0 x1) = []
  infoCapabilities (OpImageSampleImplicitLod x2 x3 x4 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageSampleExplicitLod x2 x3 x4 x0 x1) = []
  infoCapabilities (OpImageSampleDrefImplicitLod x2 x3 x4 x5 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageSampleDrefExplicitLod x2 x3 x4 x5 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageSampleProjImplicitLod x2 x3 x4 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageSampleProjExplicitLod x2 x3 x4 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageSampleProjDrefImplicitLod x2 x3 x4 x5 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageSampleProjDrefExplicitLod x2 x3 x4 x5 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageFetch x2 x3 x4 x0 x1) = []
  infoCapabilities (OpImageGather x2 x3 x4 x5 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageDrefGather x2 x3 x4 x5 x0 x1) = [CapabilityShader]
  infoCapabilities (OpImageRead x2 x3 x4 x0 x1) = []
  infoCapabilities (OpImageWrite x0 x1 x2 x3) = []
  infoCapabilities (OpImage x2 x0 x1) = []
  infoCapabilities (OpImageQueryFormat x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpImageQueryOrder x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpImageQuerySizeLod x2 x3 x0 x1) = [CapabilityKernel, CapabilityImageQuery]
  infoCapabilities (OpImageQuerySize x2 x0 x1) = [CapabilityKernel, CapabilityImageQuery]
  infoCapabilities (OpImageQueryLod x2 x3 x0 x1) = [CapabilityImageQuery]
  infoCapabilities (OpImageQueryLevels x2 x0 x1) = [CapabilityKernel, CapabilityImageQuery]
  infoCapabilities (OpImageQuerySamples x2 x0 x1) = [CapabilityKernel, CapabilityImageQuery]
  infoCapabilities (OpConvertFToU x2 x0 x1) = []
  infoCapabilities (OpConvertFToS x2 x0 x1) = []
  infoCapabilities (OpConvertSToF x2 x0 x1) = []
  infoCapabilities (OpConvertUToF x2 x0 x1) = []
  infoCapabilities (OpUConvert x2 x0 x1) = []
  infoCapabilities (OpSConvert x2 x0 x1) = []
  infoCapabilities (OpFConvert x2 x0 x1) = []
  infoCapabilities (OpQuantizeToF16 x2 x0 x1) = []
  infoCapabilities (OpConvertPtrToU x2 x0 x1) = [CapabilityAddresses, CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (OpSatConvertSToU x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpSatConvertUToS x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpConvertUToPtr x2 x0 x1) = [CapabilityAddresses, CapabilityPhysicalStorageBufferAddresses]
  infoCapabilities (OpPtrCastToGeneric x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpGenericCastToPtr x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpGenericCastToPtrExplicit x2 x3 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpBitcast x2 x0 x1) = []
  infoCapabilities (OpSNegate x2 x0 x1) = []
  infoCapabilities (OpFNegate x2 x0 x1) = []
  infoCapabilities (OpIAdd x2 x3 x0 x1) = []
  infoCapabilities (OpFAdd x2 x3 x0 x1) = []
  infoCapabilities (OpISub x2 x3 x0 x1) = []
  infoCapabilities (OpFSub x2 x3 x0 x1) = []
  infoCapabilities (OpIMul x2 x3 x0 x1) = []
  infoCapabilities (OpFMul x2 x3 x0 x1) = []
  infoCapabilities (OpUDiv x2 x3 x0 x1) = []
  infoCapabilities (OpSDiv x2 x3 x0 x1) = []
  infoCapabilities (OpFDiv x2 x3 x0 x1) = []
  infoCapabilities (OpUMod x2 x3 x0 x1) = []
  infoCapabilities (OpSRem x2 x3 x0 x1) = []
  infoCapabilities (OpSMod x2 x3 x0 x1) = []
  infoCapabilities (OpFRem x2 x3 x0 x1) = []
  infoCapabilities (OpFMod x2 x3 x0 x1) = []
  infoCapabilities (OpVectorTimesScalar x2 x3 x0 x1) = []
  infoCapabilities (OpMatrixTimesScalar x2 x3 x0 x1) = [CapabilityMatrix]
  infoCapabilities (OpVectorTimesMatrix x2 x3 x0 x1) = [CapabilityMatrix]
  infoCapabilities (OpMatrixTimesVector x2 x3 x0 x1) = [CapabilityMatrix]
  infoCapabilities (OpMatrixTimesMatrix x2 x3 x0 x1) = [CapabilityMatrix]
  infoCapabilities (OpOuterProduct x2 x3 x0 x1) = [CapabilityMatrix]
  infoCapabilities (OpDot x2 x3 x0 x1) = []
  infoCapabilities (OpIAddCarry x2 x3 x0 x1) = []
  infoCapabilities (OpISubBorrow x2 x3 x0 x1) = []
  infoCapabilities (OpUMulExtended x2 x3 x0 x1) = []
  infoCapabilities (OpSMulExtended x2 x3 x0 x1) = []
  infoCapabilities (OpAny x2 x0 x1) = []
  infoCapabilities (OpAll x2 x0 x1) = []
  infoCapabilities (OpIsNan x2 x0 x1) = []
  infoCapabilities (OpIsInf x2 x0 x1) = []
  infoCapabilities (OpIsFinite x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpIsNormal x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpSignBitSet x2 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpLessOrGreater x2 x3 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpOrdered x2 x3 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpUnordered x2 x3 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpLogicalEqual x2 x3 x0 x1) = []
  infoCapabilities (OpLogicalNotEqual x2 x3 x0 x1) = []
  infoCapabilities (OpLogicalOr x2 x3 x0 x1) = []
  infoCapabilities (OpLogicalAnd x2 x3 x0 x1) = []
  infoCapabilities (OpLogicalNot x2 x0 x1) = []
  infoCapabilities (OpSelect x2 x3 x4 x0 x1) = []
  infoCapabilities (OpIEqual x2 x3 x0 x1) = []
  infoCapabilities (OpINotEqual x2 x3 x0 x1) = []
  infoCapabilities (OpUGreaterThan x2 x3 x0 x1) = []
  infoCapabilities (OpSGreaterThan x2 x3 x0 x1) = []
  infoCapabilities (OpUGreaterThanEqual x2 x3 x0 x1) = []
  infoCapabilities (OpSGreaterThanEqual x2 x3 x0 x1) = []
  infoCapabilities (OpULessThan x2 x3 x0 x1) = []
  infoCapabilities (OpSLessThan x2 x3 x0 x1) = []
  infoCapabilities (OpULessThanEqual x2 x3 x0 x1) = []
  infoCapabilities (OpSLessThanEqual x2 x3 x0 x1) = []
  infoCapabilities (OpFOrdEqual x2 x3 x0 x1) = []
  infoCapabilities (OpFUnordEqual x2 x3 x0 x1) = []
  infoCapabilities (OpFOrdNotEqual x2 x3 x0 x1) = []
  infoCapabilities (OpFUnordNotEqual x2 x3 x0 x1) = []
  infoCapabilities (OpFOrdLessThan x2 x3 x0 x1) = []
  infoCapabilities (OpFUnordLessThan x2 x3 x0 x1) = []
  infoCapabilities (OpFOrdGreaterThan x2 x3 x0 x1) = []
  infoCapabilities (OpFUnordGreaterThan x2 x3 x0 x1) = []
  infoCapabilities (OpFOrdLessThanEqual x2 x3 x0 x1) = []
  infoCapabilities (OpFUnordLessThanEqual x2 x3 x0 x1) = []
  infoCapabilities (OpFOrdGreaterThanEqual x2 x3 x0 x1) = []
  infoCapabilities (OpFUnordGreaterThanEqual x2 x3 x0 x1) = []
  infoCapabilities (OpShiftRightLogical x2 x3 x0 x1) = []
  infoCapabilities (OpShiftRightArithmetic x2 x3 x0 x1) = []
  infoCapabilities (OpShiftLeftLogical x2 x3 x0 x1) = []
  infoCapabilities (OpBitwiseOr x2 x3 x0 x1) = []
  infoCapabilities (OpBitwiseXor x2 x3 x0 x1) = []
  infoCapabilities (OpBitwiseAnd x2 x3 x0 x1) = []
  infoCapabilities (OpNot x2 x0 x1) = []
  infoCapabilities (OpBitFieldInsert x2 x3 x4 x5 x0 x1) = [CapabilityShader]
  infoCapabilities (OpBitFieldSExtract x2 x3 x4 x0 x1) = [CapabilityShader]
  infoCapabilities (OpBitFieldUExtract x2 x3 x4 x0 x1) = [CapabilityShader]
  infoCapabilities (OpBitReverse x2 x0 x1) = [CapabilityShader]
  infoCapabilities (OpBitCount x2 x0 x1) = []
  infoCapabilities (OpDPdx x2 x0 x1) = [CapabilityShader]
  infoCapabilities (OpDPdy x2 x0 x1) = [CapabilityShader]
  infoCapabilities (OpFwidth x2 x0 x1) = [CapabilityShader]
  infoCapabilities (OpDPdxFine x2 x0 x1) = [CapabilityDerivativeControl]
  infoCapabilities (OpDPdyFine x2 x0 x1) = [CapabilityDerivativeControl]
  infoCapabilities (OpFwidthFine x2 x0 x1) = [CapabilityDerivativeControl]
  infoCapabilities (OpDPdxCoarse x2 x0 x1) = [CapabilityDerivativeControl]
  infoCapabilities (OpDPdyCoarse x2 x0 x1) = [CapabilityDerivativeControl]
  infoCapabilities (OpFwidthCoarse x2 x0 x1) = [CapabilityDerivativeControl]
  infoCapabilities (OpEmitVertex) = [CapabilityGeometry]
  infoCapabilities (OpEndPrimitive) = [CapabilityGeometry]
  infoCapabilities (OpEmitStreamVertex x0) = [CapabilityGeometryStreams]
  infoCapabilities (OpEndStreamPrimitive x0) = [CapabilityGeometryStreams]
  infoCapabilities (OpControlBarrier x0 x1 x2) = []
  infoCapabilities (OpMemoryBarrier x0 x1) = []
  infoCapabilities (OpAtomicLoad x2 x3 x4 x0 x1) = []
  infoCapabilities (OpAtomicStore x0 x1 x2 x3) = []
  infoCapabilities (OpAtomicExchange x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicCompareExchange x2 x3 x4 x5 x6 x7 x0 x1) = []
  infoCapabilities (OpAtomicCompareExchangeWeak x2 x3 x4 x5 x6 x7 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpAtomicIIncrement x2 x3 x4 x0 x1) = []
  infoCapabilities (OpAtomicIDecrement x2 x3 x4 x0 x1) = []
  infoCapabilities (OpAtomicIAdd x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicISub x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicSMin x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicUMin x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicSMax x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicUMax x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicAnd x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicOr x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpAtomicXor x2 x3 x4 x5 x0 x1) = []
  infoCapabilities (OpPhi x2 x0 x1) = []
  infoCapabilities (OpLoopMerge x0 x1 x2) = []
  infoCapabilities (OpSelectionMerge x0 x1) = []
  infoCapabilities (OpLabel x0) = []
  infoCapabilities (OpBranch x0) = []
  infoCapabilities (OpBranchConditional x0 x1 x2 x3) = []
  infoCapabilities (OpSwitch x0 x1 x2) = []
  infoCapabilities (OpKill) = [CapabilityShader]
  infoCapabilities (OpReturn) = []
  infoCapabilities (OpReturnValue x0) = []
  infoCapabilities (OpUnreachable) = []
  infoCapabilities (OpLifetimeStart x0 x1) = [CapabilityKernel]
  infoCapabilities (OpLifetimeStop x0 x1) = [CapabilityKernel]
  infoCapabilities (OpGroupAsyncCopy x2 x3 x4 x5 x6 x7 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpGroupWaitEvents x0 x1 x2) = [CapabilityKernel]
  infoCapabilities (OpGroupAll x2 x3 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupAny x2 x3 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupBroadcast x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupIAdd x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupFAdd x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupFMin x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupUMin x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupSMin x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupFMax x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupUMax x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpGroupSMax x2 x3 x4 x0 x1) = [CapabilityGroups]
  infoCapabilities (OpReadPipe x2 x3 x4 x5 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpWritePipe x2 x3 x4 x5 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpReservedReadPipe x2 x3 x4 x5 x6 x7 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpReservedWritePipe x2 x3 x4 x5 x6 x7 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpReserveReadPipePackets x2 x3 x4 x5 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpReserveWritePipePackets x2 x3 x4 x5 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpCommitReadPipe x0 x1 x2 x3) = [CapabilityPipes]
  infoCapabilities (OpCommitWritePipe x0 x1 x2 x3) = [CapabilityPipes]
  infoCapabilities (OpIsValidReserveId x2 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpGetNumPipePackets x2 x3 x4 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpGetMaxPipePackets x2 x3 x4 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpGroupReserveReadPipePackets x2 x3 x4 x5 x6 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpGroupReserveWritePipePackets x2 x3 x4 x5 x6 x0 x1) = [CapabilityPipes]
  infoCapabilities (OpGroupCommitReadPipe x0 x1 x2 x3 x4) = [CapabilityPipes]
  infoCapabilities (OpGroupCommitWritePipe x0 x1 x2 x3 x4) = [CapabilityPipes]
  infoCapabilities (OpEnqueueMarker x2 x3 x4 x5 x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpEnqueueKernel x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpGetKernelNDrangeSubGroupCount x2 x3 x4 x5 x6 x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpGetKernelNDrangeMaxSubGroupSize x2 x3 x4 x5 x6 x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpGetKernelWorkGroupSize x2 x3 x4 x5 x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpGetKernelPreferredWorkGroupSizeMultiple x2 x3 x4 x5 x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpRetainEvent x0) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpReleaseEvent x0) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpCreateUserEvent x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpIsValidEvent x2 x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpSetUserEventStatus x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpCaptureEventProfilingInfo x0 x1 x2) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpGetDefaultQueue x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpBuildNDRange x2 x3 x4 x0 x1) = [CapabilityDeviceEnqueue]
  infoCapabilities (OpImageSparseSampleImplicitLod x2 x3 x4 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpImageSparseSampleExplicitLod x2 x3 x4 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpImageSparseSampleDrefImplicitLod x2 x3 x4 x5 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpImageSparseSampleDrefExplicitLod x2 x3 x4 x5 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpImageSparseFetch x2 x3 x4 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpImageSparseGather x2 x3 x4 x5 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpImageSparseDrefGather x2 x3 x4 x5 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpImageSparseTexelsResident x2 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpNoLine) = []
  infoCapabilities (OpAtomicFlagTestAndSet x2 x3 x4 x0 x1) = [CapabilityKernel]
  infoCapabilities (OpAtomicFlagClear x0 x1 x2) = [CapabilityKernel]
  infoCapabilities (OpImageSparseRead x2 x3 x4 x0 x1) = [CapabilitySparseResidency]
  infoCapabilities (OpSizeOf x2 x0 x1) = [CapabilityAddresses]
  infoCapabilities (OpTypePipeStorage x0) = [CapabilityPipeStorage]
  infoCapabilities (OpConstantPipeStorage x2 x3 x4 x0 x1) = [CapabilityPipeStorage]
  infoCapabilities (OpCreatePipeFromPipeStorage x2 x0 x1) = [CapabilityPipeStorage]
  infoCapabilities (OpGetKernelLocalSizeForSubgroupCount x2 x3 x4 x5 x6 x0 x1) = [CapabilitySubgroupDispatch]
  infoCapabilities (OpGetKernelMaxNumSubgroups x2 x3 x4 x5 x0 x1) = [CapabilitySubgroupDispatch]
  infoCapabilities (OpTypeNamedBarrier x0) = [CapabilityNamedBarrier]
  infoCapabilities (OpNamedBarrierInitialize x2 x0 x1) = [CapabilityNamedBarrier]
  infoCapabilities (OpMemoryNamedBarrier x0 x1 x2) = [CapabilityNamedBarrier]
  infoCapabilities (OpModuleProcessed x0) = []
  infoCapabilities (OpExecutionModeId x0 x1) = []
  infoCapabilities (OpDecorateId x0 x1) = []
  infoCapabilities (OpGroupNonUniformElect x2 x0 x1) = [CapabilityGroupNonUniform]
  infoCapabilities (OpGroupNonUniformAll x2 x3 x0 x1) = [CapabilityGroupNonUniformVote]
  infoCapabilities (OpGroupNonUniformAny x2 x3 x0 x1) = [CapabilityGroupNonUniformVote]
  infoCapabilities (OpGroupNonUniformAllEqual x2 x3 x0 x1) = [CapabilityGroupNonUniformVote]
  infoCapabilities (OpGroupNonUniformBroadcast x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (OpGroupNonUniformBroadcastFirst x2 x3 x0 x1) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (OpGroupNonUniformBallot x2 x3 x0 x1) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (OpGroupNonUniformInverseBallot x2 x3 x0 x1) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (OpGroupNonUniformBallotBitExtract x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (OpGroupNonUniformBallotBitCount x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (OpGroupNonUniformBallotFindLSB x2 x3 x0 x1) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (OpGroupNonUniformBallotFindMSB x2 x3 x0 x1) = [CapabilityGroupNonUniformBallot]
  infoCapabilities (OpGroupNonUniformShuffle x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformShuffle]
  infoCapabilities (OpGroupNonUniformShuffleXor x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformShuffle]
  infoCapabilities (OpGroupNonUniformShuffleUp x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformShuffleRelative]
  infoCapabilities (OpGroupNonUniformShuffleDown x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformShuffleRelative]
  infoCapabilities (OpGroupNonUniformIAdd x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformFAdd x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformIMul x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformFMul x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformSMin x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformUMin x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformFMin x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformSMax x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformUMax x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformFMax x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformBitwiseAnd x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformBitwiseOr x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformBitwiseXor x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformLogicalAnd x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformLogicalOr x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformLogicalXor x2 x3 x4 x5 x0 x1) = [CapabilityGroupNonUniformArithmetic, CapabilityGroupNonUniformClustered]
  infoCapabilities (OpGroupNonUniformQuadBroadcast x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformQuad]
  infoCapabilities (OpGroupNonUniformQuadSwap x2 x3 x4 x0 x1) = [CapabilityGroupNonUniformQuad]
  infoCapabilities (OpCopyLogical x2 x0 x1) = []
  infoCapabilities (OpPtrEqual x2 x3 x0 x1) = []
  infoCapabilities (OpPtrNotEqual x2 x3 x0 x1) = []
  infoCapabilities (OpPtrDiff x2 x3 x0 x1) = [CapabilityAddresses, CapabilityVariablePointers, CapabilityVariablePointersStorageBuffer]
  infoCapabilities (OpDecorateString x0 x1) = []
  infoCapabilities (OpDecorateStringGOOGLE x0 x1) = []
  infoCapabilities (OpMemberDecorateString x0 x1 x2) = []
  infoCapabilities (OpMemberDecorateStringGOOGLE x0 x1 x2) = []
  infoExtensions (OpNop) = []
  infoExtensions (OpUndef x0 x1) = []
  infoExtensions (OpSourceContinued x0) = []
  infoExtensions (OpSource x0 x1 x2 x3) = []
  infoExtensions (OpSourceExtension x0) = []
  infoExtensions (OpName x0 x1) = []
  infoExtensions (OpMemberName x0 x1 x2) = []
  infoExtensions (OpString x1 x0) = []
  infoExtensions (OpLine x0 x1 x2) = []
  infoExtensions (OpExtension x0) = []
  infoExtensions (OpExtInstImport x1 x0) = []
  infoExtensions (OpExtInst x2 x3 x0 x1) = []
  infoExtensions (OpMemoryModel x0 x1) = []
  infoExtensions (OpEntryPoint x0 x1 x2 x3) = []
  infoExtensions (OpExecutionMode x0 x1) = []
  infoExtensions (OpCapability x0) = []
  infoExtensions (OpTypeVoid x0) = []
  infoExtensions (OpTypeBool x0) = []
  infoExtensions (OpTypeInt x1 x2 x0) = []
  infoExtensions (OpTypeFloat x1 x0) = []
  infoExtensions (OpTypeVector x1 x2 x0) = []
  infoExtensions (OpTypeMatrix x1 x2 x0) = []
  infoExtensions (OpTypeImage x1 x2 x3 x4 x5 x6 x7 x8 x0) = []
  infoExtensions (OpTypeSampler x0) = []
  infoExtensions (OpTypeSampledImage x1 x0) = []
  infoExtensions (OpTypeArray x1 x2 x0) = []
  infoExtensions (OpTypeRuntimeArray x1 x0) = []
  infoExtensions (OpTypeStruct x1 x0) = []
  infoExtensions (OpTypeOpaque x1 x0) = []
  infoExtensions (OpTypePointer x1 x2 x0) = []
  infoExtensions (OpTypeFunction x1 x2 x0) = []
  infoExtensions (OpTypeEvent x0) = []
  infoExtensions (OpTypeDeviceEvent x0) = []
  infoExtensions (OpTypeReserveId x0) = []
  infoExtensions (OpTypeQueue x0) = []
  infoExtensions (OpTypePipe x1 x0) = []
  infoExtensions (OpTypeForwardPointer x0 x1) = []
  infoExtensions (OpConstantTrue x0 x1) = []
  infoExtensions (OpConstantFalse x0 x1) = []
  infoExtensions (OpConstant x2 x0 x1) = []
  infoExtensions (OpConstantComposite x2 x0 x1) = []
  infoExtensions (OpConstantSampler x2 x3 x4 x0 x1) = []
  infoExtensions (OpConstantNull x0 x1) = []
  infoExtensions (OpSpecConstantTrue x0 x1) = []
  infoExtensions (OpSpecConstantFalse x0 x1) = []
  infoExtensions (OpSpecConstant x2 x0 x1) = []
  infoExtensions (OpSpecConstantComposite x2 x0 x1) = []
  infoExtensions (OpSpecConstantOp x2 x0 x1) = []
  infoExtensions (OpFunction x2 x3 x0 x1) = []
  infoExtensions (OpFunctionParameter x0 x1) = []
  infoExtensions (OpFunctionEnd) = []
  infoExtensions (OpFunctionCall x2 x3 x0 x1) = []
  infoExtensions (OpVariable x2 x3 x0 x1) = []
  infoExtensions (OpImageTexelPointer x2 x3 x4 x0 x1) = []
  infoExtensions (OpLoad x2 x3 x0 x1) = []
  infoExtensions (OpStore x0 x1 x2) = []
  infoExtensions (OpCopyMemory x0 x1 x2 x3) = []
  infoExtensions (OpCopyMemorySized x0 x1 x2 x3 x4) = []
  infoExtensions (OpAccessChain x2 x3 x0 x1) = []
  infoExtensions (OpInBoundsAccessChain x2 x3 x0 x1) = []
  infoExtensions (OpPtrAccessChain x2 x3 x4 x0 x1) = []
  infoExtensions (OpArrayLength x2 x3 x0 x1) = []
  infoExtensions (OpGenericPtrMemSemantics x2 x0 x1) = []
  infoExtensions (OpInBoundsPtrAccessChain x2 x3 x4 x0 x1) = []
  infoExtensions (OpDecorate x0 x1) = []
  infoExtensions (OpMemberDecorate x0 x1 x2) = []
  infoExtensions (OpDecorationGroup x0) = []
  infoExtensions (OpGroupDecorate x0 x1) = []
  infoExtensions (OpGroupMemberDecorate x0 x1) = []
  infoExtensions (OpVectorExtractDynamic x2 x3 x0 x1) = []
  infoExtensions (OpVectorInsertDynamic x2 x3 x4 x0 x1) = []
  infoExtensions (OpVectorShuffle x2 x3 x4 x0 x1) = []
  infoExtensions (OpCompositeConstruct x2 x0 x1) = []
  infoExtensions (OpCompositeExtract x2 x3 x0 x1) = []
  infoExtensions (OpCompositeInsert x2 x3 x4 x0 x1) = []
  infoExtensions (OpCopyObject x2 x0 x1) = []
  infoExtensions (OpTranspose x2 x0 x1) = []
  infoExtensions (OpSampledImage x2 x3 x0 x1) = []
  infoExtensions (OpImageSampleImplicitLod x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageSampleExplicitLod x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageSampleDrefImplicitLod x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageSampleDrefExplicitLod x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageSampleProjImplicitLod x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageSampleProjExplicitLod x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageSampleProjDrefImplicitLod x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageSampleProjDrefExplicitLod x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageFetch x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageGather x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageDrefGather x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageRead x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageWrite x0 x1 x2 x3) = []
  infoExtensions (OpImage x2 x0 x1) = []
  infoExtensions (OpImageQueryFormat x2 x0 x1) = []
  infoExtensions (OpImageQueryOrder x2 x0 x1) = []
  infoExtensions (OpImageQuerySizeLod x2 x3 x0 x1) = []
  infoExtensions (OpImageQuerySize x2 x0 x1) = []
  infoExtensions (OpImageQueryLod x2 x3 x0 x1) = []
  infoExtensions (OpImageQueryLevels x2 x0 x1) = []
  infoExtensions (OpImageQuerySamples x2 x0 x1) = []
  infoExtensions (OpConvertFToU x2 x0 x1) = []
  infoExtensions (OpConvertFToS x2 x0 x1) = []
  infoExtensions (OpConvertSToF x2 x0 x1) = []
  infoExtensions (OpConvertUToF x2 x0 x1) = []
  infoExtensions (OpUConvert x2 x0 x1) = []
  infoExtensions (OpSConvert x2 x0 x1) = []
  infoExtensions (OpFConvert x2 x0 x1) = []
  infoExtensions (OpQuantizeToF16 x2 x0 x1) = []
  infoExtensions (OpConvertPtrToU x2 x0 x1) = []
  infoExtensions (OpSatConvertSToU x2 x0 x1) = []
  infoExtensions (OpSatConvertUToS x2 x0 x1) = []
  infoExtensions (OpConvertUToPtr x2 x0 x1) = []
  infoExtensions (OpPtrCastToGeneric x2 x0 x1) = []
  infoExtensions (OpGenericCastToPtr x2 x0 x1) = []
  infoExtensions (OpGenericCastToPtrExplicit x2 x3 x0 x1) = []
  infoExtensions (OpBitcast x2 x0 x1) = []
  infoExtensions (OpSNegate x2 x0 x1) = []
  infoExtensions (OpFNegate x2 x0 x1) = []
  infoExtensions (OpIAdd x2 x3 x0 x1) = []
  infoExtensions (OpFAdd x2 x3 x0 x1) = []
  infoExtensions (OpISub x2 x3 x0 x1) = []
  infoExtensions (OpFSub x2 x3 x0 x1) = []
  infoExtensions (OpIMul x2 x3 x0 x1) = []
  infoExtensions (OpFMul x2 x3 x0 x1) = []
  infoExtensions (OpUDiv x2 x3 x0 x1) = []
  infoExtensions (OpSDiv x2 x3 x0 x1) = []
  infoExtensions (OpFDiv x2 x3 x0 x1) = []
  infoExtensions (OpUMod x2 x3 x0 x1) = []
  infoExtensions (OpSRem x2 x3 x0 x1) = []
  infoExtensions (OpSMod x2 x3 x0 x1) = []
  infoExtensions (OpFRem x2 x3 x0 x1) = []
  infoExtensions (OpFMod x2 x3 x0 x1) = []
  infoExtensions (OpVectorTimesScalar x2 x3 x0 x1) = []
  infoExtensions (OpMatrixTimesScalar x2 x3 x0 x1) = []
  infoExtensions (OpVectorTimesMatrix x2 x3 x0 x1) = []
  infoExtensions (OpMatrixTimesVector x2 x3 x0 x1) = []
  infoExtensions (OpMatrixTimesMatrix x2 x3 x0 x1) = []
  infoExtensions (OpOuterProduct x2 x3 x0 x1) = []
  infoExtensions (OpDot x2 x3 x0 x1) = []
  infoExtensions (OpIAddCarry x2 x3 x0 x1) = []
  infoExtensions (OpISubBorrow x2 x3 x0 x1) = []
  infoExtensions (OpUMulExtended x2 x3 x0 x1) = []
  infoExtensions (OpSMulExtended x2 x3 x0 x1) = []
  infoExtensions (OpAny x2 x0 x1) = []
  infoExtensions (OpAll x2 x0 x1) = []
  infoExtensions (OpIsNan x2 x0 x1) = []
  infoExtensions (OpIsInf x2 x0 x1) = []
  infoExtensions (OpIsFinite x2 x0 x1) = []
  infoExtensions (OpIsNormal x2 x0 x1) = []
  infoExtensions (OpSignBitSet x2 x0 x1) = []
  infoExtensions (OpLessOrGreater x2 x3 x0 x1) = []
  infoExtensions (OpOrdered x2 x3 x0 x1) = []
  infoExtensions (OpUnordered x2 x3 x0 x1) = []
  infoExtensions (OpLogicalEqual x2 x3 x0 x1) = []
  infoExtensions (OpLogicalNotEqual x2 x3 x0 x1) = []
  infoExtensions (OpLogicalOr x2 x3 x0 x1) = []
  infoExtensions (OpLogicalAnd x2 x3 x0 x1) = []
  infoExtensions (OpLogicalNot x2 x0 x1) = []
  infoExtensions (OpSelect x2 x3 x4 x0 x1) = []
  infoExtensions (OpIEqual x2 x3 x0 x1) = []
  infoExtensions (OpINotEqual x2 x3 x0 x1) = []
  infoExtensions (OpUGreaterThan x2 x3 x0 x1) = []
  infoExtensions (OpSGreaterThan x2 x3 x0 x1) = []
  infoExtensions (OpUGreaterThanEqual x2 x3 x0 x1) = []
  infoExtensions (OpSGreaterThanEqual x2 x3 x0 x1) = []
  infoExtensions (OpULessThan x2 x3 x0 x1) = []
  infoExtensions (OpSLessThan x2 x3 x0 x1) = []
  infoExtensions (OpULessThanEqual x2 x3 x0 x1) = []
  infoExtensions (OpSLessThanEqual x2 x3 x0 x1) = []
  infoExtensions (OpFOrdEqual x2 x3 x0 x1) = []
  infoExtensions (OpFUnordEqual x2 x3 x0 x1) = []
  infoExtensions (OpFOrdNotEqual x2 x3 x0 x1) = []
  infoExtensions (OpFUnordNotEqual x2 x3 x0 x1) = []
  infoExtensions (OpFOrdLessThan x2 x3 x0 x1) = []
  infoExtensions (OpFUnordLessThan x2 x3 x0 x1) = []
  infoExtensions (OpFOrdGreaterThan x2 x3 x0 x1) = []
  infoExtensions (OpFUnordGreaterThan x2 x3 x0 x1) = []
  infoExtensions (OpFOrdLessThanEqual x2 x3 x0 x1) = []
  infoExtensions (OpFUnordLessThanEqual x2 x3 x0 x1) = []
  infoExtensions (OpFOrdGreaterThanEqual x2 x3 x0 x1) = []
  infoExtensions (OpFUnordGreaterThanEqual x2 x3 x0 x1) = []
  infoExtensions (OpShiftRightLogical x2 x3 x0 x1) = []
  infoExtensions (OpShiftRightArithmetic x2 x3 x0 x1) = []
  infoExtensions (OpShiftLeftLogical x2 x3 x0 x1) = []
  infoExtensions (OpBitwiseOr x2 x3 x0 x1) = []
  infoExtensions (OpBitwiseXor x2 x3 x0 x1) = []
  infoExtensions (OpBitwiseAnd x2 x3 x0 x1) = []
  infoExtensions (OpNot x2 x0 x1) = []
  infoExtensions (OpBitFieldInsert x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpBitFieldSExtract x2 x3 x4 x0 x1) = []
  infoExtensions (OpBitFieldUExtract x2 x3 x4 x0 x1) = []
  infoExtensions (OpBitReverse x2 x0 x1) = []
  infoExtensions (OpBitCount x2 x0 x1) = []
  infoExtensions (OpDPdx x2 x0 x1) = []
  infoExtensions (OpDPdy x2 x0 x1) = []
  infoExtensions (OpFwidth x2 x0 x1) = []
  infoExtensions (OpDPdxFine x2 x0 x1) = []
  infoExtensions (OpDPdyFine x2 x0 x1) = []
  infoExtensions (OpFwidthFine x2 x0 x1) = []
  infoExtensions (OpDPdxCoarse x2 x0 x1) = []
  infoExtensions (OpDPdyCoarse x2 x0 x1) = []
  infoExtensions (OpFwidthCoarse x2 x0 x1) = []
  infoExtensions (OpEmitVertex) = []
  infoExtensions (OpEndPrimitive) = []
  infoExtensions (OpEmitStreamVertex x0) = []
  infoExtensions (OpEndStreamPrimitive x0) = []
  infoExtensions (OpControlBarrier x0 x1 x2) = []
  infoExtensions (OpMemoryBarrier x0 x1) = []
  infoExtensions (OpAtomicLoad x2 x3 x4 x0 x1) = []
  infoExtensions (OpAtomicStore x0 x1 x2 x3) = []
  infoExtensions (OpAtomicExchange x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicCompareExchange x2 x3 x4 x5 x6 x7 x0 x1) = []
  infoExtensions (OpAtomicCompareExchangeWeak x2 x3 x4 x5 x6 x7 x0 x1) = []
  infoExtensions (OpAtomicIIncrement x2 x3 x4 x0 x1) = []
  infoExtensions (OpAtomicIDecrement x2 x3 x4 x0 x1) = []
  infoExtensions (OpAtomicIAdd x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicISub x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicSMin x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicUMin x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicSMax x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicUMax x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicAnd x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicOr x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpAtomicXor x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpPhi x2 x0 x1) = []
  infoExtensions (OpLoopMerge x0 x1 x2) = []
  infoExtensions (OpSelectionMerge x0 x1) = []
  infoExtensions (OpLabel x0) = []
  infoExtensions (OpBranch x0) = []
  infoExtensions (OpBranchConditional x0 x1 x2 x3) = []
  infoExtensions (OpSwitch x0 x1 x2) = []
  infoExtensions (OpKill) = []
  infoExtensions (OpReturn) = []
  infoExtensions (OpReturnValue x0) = []
  infoExtensions (OpUnreachable) = []
  infoExtensions (OpLifetimeStart x0 x1) = []
  infoExtensions (OpLifetimeStop x0 x1) = []
  infoExtensions (OpGroupAsyncCopy x2 x3 x4 x5 x6 x7 x0 x1) = []
  infoExtensions (OpGroupWaitEvents x0 x1 x2) = []
  infoExtensions (OpGroupAll x2 x3 x0 x1) = []
  infoExtensions (OpGroupAny x2 x3 x0 x1) = []
  infoExtensions (OpGroupBroadcast x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupIAdd x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupFAdd x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupFMin x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupUMin x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupSMin x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupFMax x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupUMax x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupSMax x2 x3 x4 x0 x1) = []
  infoExtensions (OpReadPipe x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpWritePipe x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpReservedReadPipe x2 x3 x4 x5 x6 x7 x0 x1) = []
  infoExtensions (OpReservedWritePipe x2 x3 x4 x5 x6 x7 x0 x1) = []
  infoExtensions (OpReserveReadPipePackets x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpReserveWritePipePackets x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpCommitReadPipe x0 x1 x2 x3) = []
  infoExtensions (OpCommitWritePipe x0 x1 x2 x3) = []
  infoExtensions (OpIsValidReserveId x2 x0 x1) = []
  infoExtensions (OpGetNumPipePackets x2 x3 x4 x0 x1) = []
  infoExtensions (OpGetMaxPipePackets x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupReserveReadPipePackets x2 x3 x4 x5 x6 x0 x1) = []
  infoExtensions (OpGroupReserveWritePipePackets x2 x3 x4 x5 x6 x0 x1) = []
  infoExtensions (OpGroupCommitReadPipe x0 x1 x2 x3 x4) = []
  infoExtensions (OpGroupCommitWritePipe x0 x1 x2 x3 x4) = []
  infoExtensions (OpEnqueueMarker x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpEnqueueKernel x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x0 x1) = []
  infoExtensions (OpGetKernelNDrangeSubGroupCount x2 x3 x4 x5 x6 x0 x1) = []
  infoExtensions (OpGetKernelNDrangeMaxSubGroupSize x2 x3 x4 x5 x6 x0 x1) = []
  infoExtensions (OpGetKernelWorkGroupSize x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGetKernelPreferredWorkGroupSizeMultiple x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpRetainEvent x0) = []
  infoExtensions (OpReleaseEvent x0) = []
  infoExtensions (OpCreateUserEvent x0 x1) = []
  infoExtensions (OpIsValidEvent x2 x0 x1) = []
  infoExtensions (OpSetUserEventStatus x0 x1) = []
  infoExtensions (OpCaptureEventProfilingInfo x0 x1 x2) = []
  infoExtensions (OpGetDefaultQueue x0 x1) = []
  infoExtensions (OpBuildNDRange x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageSparseSampleImplicitLod x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageSparseSampleExplicitLod x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageSparseSampleDrefImplicitLod x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageSparseSampleDrefExplicitLod x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageSparseFetch x2 x3 x4 x0 x1) = []
  infoExtensions (OpImageSparseGather x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageSparseDrefGather x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpImageSparseTexelsResident x2 x0 x1) = []
  infoExtensions (OpNoLine) = []
  infoExtensions (OpAtomicFlagTestAndSet x2 x3 x4 x0 x1) = []
  infoExtensions (OpAtomicFlagClear x0 x1 x2) = []
  infoExtensions (OpImageSparseRead x2 x3 x4 x0 x1) = []
  infoExtensions (OpSizeOf x2 x0 x1) = []
  infoExtensions (OpTypePipeStorage x0) = []
  infoExtensions (OpConstantPipeStorage x2 x3 x4 x0 x1) = []
  infoExtensions (OpCreatePipeFromPipeStorage x2 x0 x1) = []
  infoExtensions (OpGetKernelLocalSizeForSubgroupCount x2 x3 x4 x5 x6 x0 x1) = []
  infoExtensions (OpGetKernelMaxNumSubgroups x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpTypeNamedBarrier x0) = []
  infoExtensions (OpNamedBarrierInitialize x2 x0 x1) = []
  infoExtensions (OpMemoryNamedBarrier x0 x1 x2) = []
  infoExtensions (OpModuleProcessed x0) = []
  infoExtensions (OpExecutionModeId x0 x1) = []
  infoExtensions (OpDecorateId x0 x1) = ["SPV_GOOGLE_hlsl_functionality1"]
  infoExtensions (OpGroupNonUniformElect x2 x0 x1) = []
  infoExtensions (OpGroupNonUniformAll x2 x3 x0 x1) = []
  infoExtensions (OpGroupNonUniformAny x2 x3 x0 x1) = []
  infoExtensions (OpGroupNonUniformAllEqual x2 x3 x0 x1) = []
  infoExtensions (OpGroupNonUniformBroadcast x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupNonUniformBroadcastFirst x2 x3 x0 x1) = []
  infoExtensions (OpGroupNonUniformBallot x2 x3 x0 x1) = []
  infoExtensions (OpGroupNonUniformInverseBallot x2 x3 x0 x1) = []
  infoExtensions (OpGroupNonUniformBallotBitExtract x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupNonUniformBallotBitCount x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupNonUniformBallotFindLSB x2 x3 x0 x1) = []
  infoExtensions (OpGroupNonUniformBallotFindMSB x2 x3 x0 x1) = []
  infoExtensions (OpGroupNonUniformShuffle x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupNonUniformShuffleXor x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupNonUniformShuffleUp x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupNonUniformShuffleDown x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupNonUniformIAdd x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformFAdd x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformIMul x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformFMul x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformSMin x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformUMin x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformFMin x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformSMax x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformUMax x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformFMax x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformBitwiseAnd x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformBitwiseOr x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformBitwiseXor x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformLogicalAnd x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformLogicalOr x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformLogicalXor x2 x3 x4 x5 x0 x1) = []
  infoExtensions (OpGroupNonUniformQuadBroadcast x2 x3 x4 x0 x1) = []
  infoExtensions (OpGroupNonUniformQuadSwap x2 x3 x4 x0 x1) = []
  infoExtensions (OpCopyLogical x2 x0 x1) = []
  infoExtensions (OpPtrEqual x2 x3 x0 x1) = []
  infoExtensions (OpPtrNotEqual x2 x3 x0 x1) = []
  infoExtensions (OpPtrDiff x2 x3 x0 x1) = []
  infoExtensions (OpDecorateString x0 x1) = ["SPV_GOOGLE_decorate_string", "SPV_GOOGLE_hlsl_functionality1"]
  infoExtensions (OpDecorateStringGOOGLE x0 x1) = ["SPV_GOOGLE_decorate_string", "SPV_GOOGLE_hlsl_functionality1"]
  infoExtensions (OpMemberDecorateString x0 x1 x2) = ["SPV_GOOGLE_decorate_string", "SPV_GOOGLE_hlsl_functionality1"]
  infoExtensions (OpMemberDecorateStringGOOGLE x0 x1 x2) = ["SPV_GOOGLE_decorate_string", "SPV_GOOGLE_hlsl_functionality1"]
  infoVersionRange (OpNop) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUndef x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSourceContinued x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSource x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSourceExtension x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpName x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpMemberName x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpString x1 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLine x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpExtension x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpExtInstImport x1 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpExtInst x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpMemoryModel x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpEntryPoint x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpExecutionMode x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCapability x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeVoid x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeBool x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeInt x1 x2 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeFloat x1 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeVector x1 x2 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeMatrix x1 x2 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeImage x1 x2 x3 x4 x5 x6 x7 x8 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeSampler x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeSampledImage x1 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeArray x1 x2 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeRuntimeArray x1 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeStruct x1 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeOpaque x1 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypePointer x1 x2 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeFunction x1 x2 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeEvent x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeDeviceEvent x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeReserveId x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeQueue x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypePipe x1 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTypeForwardPointer x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConstantTrue x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConstantFalse x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConstant x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConstantComposite x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConstantSampler x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConstantNull x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSpecConstantTrue x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSpecConstantFalse x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSpecConstant x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSpecConstantComposite x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSpecConstantOp x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFunction x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFunctionParameter x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFunctionEnd) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFunctionCall x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpVariable x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageTexelPointer x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLoad x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpStore x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCopyMemory x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCopyMemorySized x0 x1 x2 x3 x4) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAccessChain x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpInBoundsAccessChain x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpPtrAccessChain x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpArrayLength x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGenericPtrMemSemantics x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpInBoundsPtrAccessChain x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDecorate x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpMemberDecorate x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDecorationGroup x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupDecorate x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupMemberDecorate x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpVectorExtractDynamic x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpVectorInsertDynamic x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpVectorShuffle x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCompositeConstruct x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCompositeExtract x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCompositeInsert x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCopyObject x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpTranspose x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSampledImage x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSampleImplicitLod x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSampleExplicitLod x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSampleDrefImplicitLod x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSampleDrefExplicitLod x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSampleProjImplicitLod x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSampleProjExplicitLod x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSampleProjDrefImplicitLod x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSampleProjDrefExplicitLod x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageFetch x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageGather x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageDrefGather x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageRead x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageWrite x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImage x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageQueryFormat x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageQueryOrder x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageQuerySizeLod x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageQuerySize x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageQueryLod x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageQueryLevels x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageQuerySamples x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConvertFToU x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConvertFToS x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConvertSToF x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConvertUToF x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUConvert x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSConvert x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFConvert x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpQuantizeToF16 x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConvertPtrToU x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSatConvertSToU x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSatConvertUToS x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpConvertUToPtr x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpPtrCastToGeneric x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGenericCastToPtr x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGenericCastToPtrExplicit x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitcast x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSNegate x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFNegate x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIAdd x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFAdd x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpISub x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFSub x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIMul x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFMul x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUDiv x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSDiv x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFDiv x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUMod x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSRem x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSMod x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFRem x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFMod x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpVectorTimesScalar x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpMatrixTimesScalar x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpVectorTimesMatrix x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpMatrixTimesVector x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpMatrixTimesMatrix x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpOuterProduct x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDot x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIAddCarry x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpISubBorrow x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUMulExtended x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSMulExtended x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAny x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAll x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIsNan x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIsInf x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIsFinite x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIsNormal x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSignBitSet x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLessOrGreater x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpOrdered x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUnordered x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLogicalEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLogicalNotEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLogicalOr x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLogicalAnd x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLogicalNot x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSelect x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpINotEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUGreaterThan x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSGreaterThan x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUGreaterThanEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSGreaterThanEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpULessThan x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSLessThan x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpULessThanEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSLessThanEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFOrdEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFUnordEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFOrdNotEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFUnordNotEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFOrdLessThan x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFUnordLessThan x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFOrdGreaterThan x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFUnordGreaterThan x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFOrdLessThanEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFUnordLessThanEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFOrdGreaterThanEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFUnordGreaterThanEqual x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpShiftRightLogical x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpShiftRightArithmetic x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpShiftLeftLogical x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitwiseOr x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitwiseXor x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitwiseAnd x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpNot x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitFieldInsert x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitFieldSExtract x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitFieldUExtract x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitReverse x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBitCount x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDPdx x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDPdy x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFwidth x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDPdxFine x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDPdyFine x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFwidthFine x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDPdxCoarse x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpDPdyCoarse x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpFwidthCoarse x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpEmitVertex) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpEndPrimitive) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpEmitStreamVertex x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpEndStreamPrimitive x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpControlBarrier x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpMemoryBarrier x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicLoad x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicStore x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicExchange x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicCompareExchange x2 x3 x4 x5 x6 x7 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicCompareExchangeWeak x2 x3 x4 x5 x6 x7 x0 x1) = SPIRVVersionRange (Nothing, Just (SPIRVVersion 1 3))
  infoVersionRange (OpAtomicIIncrement x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicIDecrement x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicIAdd x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicISub x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicSMin x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicUMin x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicSMax x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicUMax x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicAnd x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicOr x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicXor x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpPhi x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLoopMerge x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSelectionMerge x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLabel x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBranch x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBranchConditional x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSwitch x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpKill) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpReturn) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpReturnValue x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpUnreachable) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLifetimeStart x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpLifetimeStop x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupAsyncCopy x2 x3 x4 x5 x6 x7 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupWaitEvents x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupAll x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupAny x2 x3 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupBroadcast x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupIAdd x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupFAdd x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupFMin x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupUMin x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupSMin x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupFMax x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupUMax x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupSMax x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpReadPipe x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpWritePipe x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpReservedReadPipe x2 x3 x4 x5 x6 x7 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpReservedWritePipe x2 x3 x4 x5 x6 x7 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpReserveReadPipePackets x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpReserveWritePipePackets x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCommitReadPipe x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCommitWritePipe x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIsValidReserveId x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGetNumPipePackets x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGetMaxPipePackets x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupReserveReadPipePackets x2 x3 x4 x5 x6 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupReserveWritePipePackets x2 x3 x4 x5 x6 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupCommitReadPipe x0 x1 x2 x3 x4) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGroupCommitWritePipe x0 x1 x2 x3 x4) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpEnqueueMarker x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpEnqueueKernel x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGetKernelNDrangeSubGroupCount x2 x3 x4 x5 x6 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGetKernelNDrangeMaxSubGroupSize x2 x3 x4 x5 x6 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGetKernelWorkGroupSize x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGetKernelPreferredWorkGroupSizeMultiple x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpRetainEvent x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpReleaseEvent x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCreateUserEvent x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpIsValidEvent x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSetUserEventStatus x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpCaptureEventProfilingInfo x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpGetDefaultQueue x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpBuildNDRange x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseSampleImplicitLod x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseSampleExplicitLod x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseSampleDrefImplicitLod x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseSampleDrefExplicitLod x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseFetch x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseGather x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseDrefGather x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseTexelsResident x2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpNoLine) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicFlagTestAndSet x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpAtomicFlagClear x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpImageSparseRead x2 x3 x4 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (OpSizeOf x2 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpTypePipeStorage x0) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpConstantPipeStorage x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpCreatePipeFromPipeStorage x2 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpGetKernelLocalSizeForSubgroupCount x2 x3 x4 x5 x6 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpGetKernelMaxNumSubgroups x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpTypeNamedBarrier x0) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpNamedBarrierInitialize x2 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpMemoryNamedBarrier x0 x1 x2) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpModuleProcessed x0) = SPIRVVersionRange (Just (SPIRVVersion 1 1), Nothing)
  infoVersionRange (OpExecutionModeId x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 2), Nothing)
  infoVersionRange (OpDecorateId x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 2), Nothing)
  infoVersionRange (OpGroupNonUniformElect x2 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformAll x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformAny x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformAllEqual x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBroadcast x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBroadcastFirst x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBallot x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformInverseBallot x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBallotBitExtract x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBallotBitCount x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBallotFindLSB x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBallotFindMSB x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformShuffle x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformShuffleXor x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformShuffleUp x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformShuffleDown x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformIAdd x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformFAdd x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformIMul x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformFMul x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformSMin x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformUMin x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformFMin x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformSMax x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformUMax x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformFMax x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBitwiseAnd x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBitwiseOr x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformBitwiseXor x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformLogicalAnd x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformLogicalOr x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformLogicalXor x2 x3 x4 x5 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformQuadBroadcast x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpGroupNonUniformQuadSwap x2 x3 x4 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 3), Nothing)
  infoVersionRange (OpCopyLogical x2 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (OpPtrEqual x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (OpPtrNotEqual x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (OpPtrDiff x2 x3 x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (OpDecorateString x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (OpDecorateStringGOOGLE x0 x1) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (OpMemberDecorateString x0 x1 x2) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
  infoVersionRange (OpMemberDecorateStringGOOGLE x0 x1 x2) = SPIRVVersionRange (Just (SPIRVVersion 1 4), Nothing)
instance SPIRVSerialize Instruction where
  spirvSerialize (OpNop) = let opsSer = concat [] in makeInstructionHeader 0 (1 + length opsSer) : opsSer
  spirvSerialize (OpUndef x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 1 (1 + length opsSer) : opsSer
  spirvSerialize (OpSourceContinued x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 2 (1 + length opsSer) : opsSer
  spirvSerialize (OpSource x0 x1 x2 x3) = if trueThenFalse [isJust x2, isJust x3] then let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 3 (1 + length opsSer) : opsSer else error "Nothing-optional followed by Just-optional"
  spirvSerialize (OpSourceExtension x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 4 (1 + length opsSer) : opsSer
  spirvSerialize (OpName x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 5 (1 + length opsSer) : opsSer
  spirvSerialize (OpMemberName x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 6 (1 + length opsSer) : opsSer
  spirvSerialize (OpString x1 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 7 (1 + length opsSer) : opsSer
  spirvSerialize (OpLine x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 8 (1 + length opsSer) : opsSer
  spirvSerialize (OpExtension x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 10 (1 + length opsSer) : opsSer
  spirvSerialize (OpExtInstImport x1 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 11 (1 + length opsSer) : opsSer
  spirvSerialize (OpExtInst x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 12 (1 + length opsSer) : opsSer
  spirvSerialize (OpMemoryModel x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 14 (1 + length opsSer) : opsSer
  spirvSerialize (OpEntryPoint x0 x1 x2 x3) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 15 (1 + length opsSer) : opsSer
  spirvSerialize (OpExecutionMode x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 16 (1 + length opsSer) : opsSer
  spirvSerialize (OpCapability x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 17 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeVoid x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 19 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeBool x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 20 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeInt x1 x2 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 21 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeFloat x1 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 22 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeVector x1 x2 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 23 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeMatrix x1 x2 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 24 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeImage x1 x2 x3 x4 x5 x6 x7 x8 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7, spirvSerialize x8] in makeInstructionHeader 25 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeSampler x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 26 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeSampledImage x1 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 27 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeArray x1 x2 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 28 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeRuntimeArray x1 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 29 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeStruct x1 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 30 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeOpaque x1 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 31 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypePointer x1 x2 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 32 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeFunction x1 x2 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 33 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeEvent x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 34 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeDeviceEvent x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 35 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeReserveId x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 36 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeQueue x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 37 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypePipe x1 x0) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 38 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeForwardPointer x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 39 (1 + length opsSer) : opsSer
  spirvSerialize (OpConstantTrue x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 41 (1 + length opsSer) : opsSer
  spirvSerialize (OpConstantFalse x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 42 (1 + length opsSer) : opsSer
  spirvSerialize (OpConstant x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 43 (1 + length opsSer) : opsSer
  spirvSerialize (OpConstantComposite x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 44 (1 + length opsSer) : opsSer
  spirvSerialize (OpConstantSampler x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 45 (1 + length opsSer) : opsSer
  spirvSerialize (OpConstantNull x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 46 (1 + length opsSer) : opsSer
  spirvSerialize (OpSpecConstantTrue x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 48 (1 + length opsSer) : opsSer
  spirvSerialize (OpSpecConstantFalse x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 49 (1 + length opsSer) : opsSer
  spirvSerialize (OpSpecConstant x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 50 (1 + length opsSer) : opsSer
  spirvSerialize (OpSpecConstantComposite x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 51 (1 + length opsSer) : opsSer
  spirvSerialize (OpSpecConstantOp x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 52 (1 + length opsSer) : opsSer
  spirvSerialize (OpFunction x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 54 (1 + length opsSer) : opsSer
  spirvSerialize (OpFunctionParameter x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 55 (1 + length opsSer) : opsSer
  spirvSerialize (OpFunctionEnd) = let opsSer = concat [] in makeInstructionHeader 56 (1 + length opsSer) : opsSer
  spirvSerialize (OpFunctionCall x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 57 (1 + length opsSer) : opsSer
  spirvSerialize (OpVariable x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 59 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageTexelPointer x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 60 (1 + length opsSer) : opsSer
  spirvSerialize (OpLoad x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 61 (1 + length opsSer) : opsSer
  spirvSerialize (OpStore x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 62 (1 + length opsSer) : opsSer
  spirvSerialize (OpCopyMemory x0 x1 x2 x3) = if trueThenFalse [isJust x2, isJust x3] then let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 63 (1 + length opsSer) : opsSer else error "Nothing-optional followed by Just-optional"
  spirvSerialize (OpCopyMemorySized x0 x1 x2 x3 x4) = if trueThenFalse [isJust x3, isJust x4] then let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 64 (1 + length opsSer) : opsSer else error "Nothing-optional followed by Just-optional"
  spirvSerialize (OpAccessChain x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 65 (1 + length opsSer) : opsSer
  spirvSerialize (OpInBoundsAccessChain x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 66 (1 + length opsSer) : opsSer
  spirvSerialize (OpPtrAccessChain x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 67 (1 + length opsSer) : opsSer
  spirvSerialize (OpArrayLength x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 68 (1 + length opsSer) : opsSer
  spirvSerialize (OpGenericPtrMemSemantics x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 69 (1 + length opsSer) : opsSer
  spirvSerialize (OpInBoundsPtrAccessChain x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 70 (1 + length opsSer) : opsSer
  spirvSerialize (OpDecorate x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 71 (1 + length opsSer) : opsSer
  spirvSerialize (OpMemberDecorate x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 72 (1 + length opsSer) : opsSer
  spirvSerialize (OpDecorationGroup x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 73 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupDecorate x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 74 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupMemberDecorate x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 75 (1 + length opsSer) : opsSer
  spirvSerialize (OpVectorExtractDynamic x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 77 (1 + length opsSer) : opsSer
  spirvSerialize (OpVectorInsertDynamic x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 78 (1 + length opsSer) : opsSer
  spirvSerialize (OpVectorShuffle x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 79 (1 + length opsSer) : opsSer
  spirvSerialize (OpCompositeConstruct x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 80 (1 + length opsSer) : opsSer
  spirvSerialize (OpCompositeExtract x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 81 (1 + length opsSer) : opsSer
  spirvSerialize (OpCompositeInsert x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 82 (1 + length opsSer) : opsSer
  spirvSerialize (OpCopyObject x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 83 (1 + length opsSer) : opsSer
  spirvSerialize (OpTranspose x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 84 (1 + length opsSer) : opsSer
  spirvSerialize (OpSampledImage x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 86 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSampleImplicitLod x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 87 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSampleExplicitLod x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 88 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSampleDrefImplicitLod x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 89 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSampleDrefExplicitLod x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 90 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSampleProjImplicitLod x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 91 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSampleProjExplicitLod x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 92 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSampleProjDrefImplicitLod x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 93 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSampleProjDrefExplicitLod x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 94 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageFetch x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 95 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageGather x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 96 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageDrefGather x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 97 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageRead x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 98 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageWrite x0 x1 x2 x3) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 99 (1 + length opsSer) : opsSer
  spirvSerialize (OpImage x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 100 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageQueryFormat x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 101 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageQueryOrder x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 102 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageQuerySizeLod x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 103 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageQuerySize x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 104 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageQueryLod x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 105 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageQueryLevels x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 106 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageQuerySamples x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 107 (1 + length opsSer) : opsSer
  spirvSerialize (OpConvertFToU x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 109 (1 + length opsSer) : opsSer
  spirvSerialize (OpConvertFToS x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 110 (1 + length opsSer) : opsSer
  spirvSerialize (OpConvertSToF x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 111 (1 + length opsSer) : opsSer
  spirvSerialize (OpConvertUToF x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 112 (1 + length opsSer) : opsSer
  spirvSerialize (OpUConvert x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 113 (1 + length opsSer) : opsSer
  spirvSerialize (OpSConvert x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 114 (1 + length opsSer) : opsSer
  spirvSerialize (OpFConvert x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 115 (1 + length opsSer) : opsSer
  spirvSerialize (OpQuantizeToF16 x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 116 (1 + length opsSer) : opsSer
  spirvSerialize (OpConvertPtrToU x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 117 (1 + length opsSer) : opsSer
  spirvSerialize (OpSatConvertSToU x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 118 (1 + length opsSer) : opsSer
  spirvSerialize (OpSatConvertUToS x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 119 (1 + length opsSer) : opsSer
  spirvSerialize (OpConvertUToPtr x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 120 (1 + length opsSer) : opsSer
  spirvSerialize (OpPtrCastToGeneric x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 121 (1 + length opsSer) : opsSer
  spirvSerialize (OpGenericCastToPtr x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 122 (1 + length opsSer) : opsSer
  spirvSerialize (OpGenericCastToPtrExplicit x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 123 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitcast x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 124 (1 + length opsSer) : opsSer
  spirvSerialize (OpSNegate x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 126 (1 + length opsSer) : opsSer
  spirvSerialize (OpFNegate x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 127 (1 + length opsSer) : opsSer
  spirvSerialize (OpIAdd x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 128 (1 + length opsSer) : opsSer
  spirvSerialize (OpFAdd x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 129 (1 + length opsSer) : opsSer
  spirvSerialize (OpISub x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 130 (1 + length opsSer) : opsSer
  spirvSerialize (OpFSub x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 131 (1 + length opsSer) : opsSer
  spirvSerialize (OpIMul x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 132 (1 + length opsSer) : opsSer
  spirvSerialize (OpFMul x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 133 (1 + length opsSer) : opsSer
  spirvSerialize (OpUDiv x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 134 (1 + length opsSer) : opsSer
  spirvSerialize (OpSDiv x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 135 (1 + length opsSer) : opsSer
  spirvSerialize (OpFDiv x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 136 (1 + length opsSer) : opsSer
  spirvSerialize (OpUMod x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 137 (1 + length opsSer) : opsSer
  spirvSerialize (OpSRem x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 138 (1 + length opsSer) : opsSer
  spirvSerialize (OpSMod x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 139 (1 + length opsSer) : opsSer
  spirvSerialize (OpFRem x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 140 (1 + length opsSer) : opsSer
  spirvSerialize (OpFMod x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 141 (1 + length opsSer) : opsSer
  spirvSerialize (OpVectorTimesScalar x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 142 (1 + length opsSer) : opsSer
  spirvSerialize (OpMatrixTimesScalar x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 143 (1 + length opsSer) : opsSer
  spirvSerialize (OpVectorTimesMatrix x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 144 (1 + length opsSer) : opsSer
  spirvSerialize (OpMatrixTimesVector x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 145 (1 + length opsSer) : opsSer
  spirvSerialize (OpMatrixTimesMatrix x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 146 (1 + length opsSer) : opsSer
  spirvSerialize (OpOuterProduct x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 147 (1 + length opsSer) : opsSer
  spirvSerialize (OpDot x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 148 (1 + length opsSer) : opsSer
  spirvSerialize (OpIAddCarry x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 149 (1 + length opsSer) : opsSer
  spirvSerialize (OpISubBorrow x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 150 (1 + length opsSer) : opsSer
  spirvSerialize (OpUMulExtended x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 151 (1 + length opsSer) : opsSer
  spirvSerialize (OpSMulExtended x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 152 (1 + length opsSer) : opsSer
  spirvSerialize (OpAny x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 154 (1 + length opsSer) : opsSer
  spirvSerialize (OpAll x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 155 (1 + length opsSer) : opsSer
  spirvSerialize (OpIsNan x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 156 (1 + length opsSer) : opsSer
  spirvSerialize (OpIsInf x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 157 (1 + length opsSer) : opsSer
  spirvSerialize (OpIsFinite x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 158 (1 + length opsSer) : opsSer
  spirvSerialize (OpIsNormal x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 159 (1 + length opsSer) : opsSer
  spirvSerialize (OpSignBitSet x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 160 (1 + length opsSer) : opsSer
  spirvSerialize (OpLessOrGreater x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 161 (1 + length opsSer) : opsSer
  spirvSerialize (OpOrdered x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 162 (1 + length opsSer) : opsSer
  spirvSerialize (OpUnordered x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 163 (1 + length opsSer) : opsSer
  spirvSerialize (OpLogicalEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 164 (1 + length opsSer) : opsSer
  spirvSerialize (OpLogicalNotEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 165 (1 + length opsSer) : opsSer
  spirvSerialize (OpLogicalOr x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 166 (1 + length opsSer) : opsSer
  spirvSerialize (OpLogicalAnd x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 167 (1 + length opsSer) : opsSer
  spirvSerialize (OpLogicalNot x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 168 (1 + length opsSer) : opsSer
  spirvSerialize (OpSelect x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 169 (1 + length opsSer) : opsSer
  spirvSerialize (OpIEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 170 (1 + length opsSer) : opsSer
  spirvSerialize (OpINotEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 171 (1 + length opsSer) : opsSer
  spirvSerialize (OpUGreaterThan x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 172 (1 + length opsSer) : opsSer
  spirvSerialize (OpSGreaterThan x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 173 (1 + length opsSer) : opsSer
  spirvSerialize (OpUGreaterThanEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 174 (1 + length opsSer) : opsSer
  spirvSerialize (OpSGreaterThanEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 175 (1 + length opsSer) : opsSer
  spirvSerialize (OpULessThan x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 176 (1 + length opsSer) : opsSer
  spirvSerialize (OpSLessThan x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 177 (1 + length opsSer) : opsSer
  spirvSerialize (OpULessThanEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 178 (1 + length opsSer) : opsSer
  spirvSerialize (OpSLessThanEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 179 (1 + length opsSer) : opsSer
  spirvSerialize (OpFOrdEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 180 (1 + length opsSer) : opsSer
  spirvSerialize (OpFUnordEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 181 (1 + length opsSer) : opsSer
  spirvSerialize (OpFOrdNotEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 182 (1 + length opsSer) : opsSer
  spirvSerialize (OpFUnordNotEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 183 (1 + length opsSer) : opsSer
  spirvSerialize (OpFOrdLessThan x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 184 (1 + length opsSer) : opsSer
  spirvSerialize (OpFUnordLessThan x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 185 (1 + length opsSer) : opsSer
  spirvSerialize (OpFOrdGreaterThan x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 186 (1 + length opsSer) : opsSer
  spirvSerialize (OpFUnordGreaterThan x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 187 (1 + length opsSer) : opsSer
  spirvSerialize (OpFOrdLessThanEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 188 (1 + length opsSer) : opsSer
  spirvSerialize (OpFUnordLessThanEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 189 (1 + length opsSer) : opsSer
  spirvSerialize (OpFOrdGreaterThanEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 190 (1 + length opsSer) : opsSer
  spirvSerialize (OpFUnordGreaterThanEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 191 (1 + length opsSer) : opsSer
  spirvSerialize (OpShiftRightLogical x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 194 (1 + length opsSer) : opsSer
  spirvSerialize (OpShiftRightArithmetic x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 195 (1 + length opsSer) : opsSer
  spirvSerialize (OpShiftLeftLogical x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 196 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitwiseOr x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 197 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitwiseXor x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 198 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitwiseAnd x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 199 (1 + length opsSer) : opsSer
  spirvSerialize (OpNot x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 200 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitFieldInsert x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 201 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitFieldSExtract x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 202 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitFieldUExtract x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 203 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitReverse x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 204 (1 + length opsSer) : opsSer
  spirvSerialize (OpBitCount x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 205 (1 + length opsSer) : opsSer
  spirvSerialize (OpDPdx x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 207 (1 + length opsSer) : opsSer
  spirvSerialize (OpDPdy x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 208 (1 + length opsSer) : opsSer
  spirvSerialize (OpFwidth x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 209 (1 + length opsSer) : opsSer
  spirvSerialize (OpDPdxFine x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 210 (1 + length opsSer) : opsSer
  spirvSerialize (OpDPdyFine x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 211 (1 + length opsSer) : opsSer
  spirvSerialize (OpFwidthFine x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 212 (1 + length opsSer) : opsSer
  spirvSerialize (OpDPdxCoarse x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 213 (1 + length opsSer) : opsSer
  spirvSerialize (OpDPdyCoarse x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 214 (1 + length opsSer) : opsSer
  spirvSerialize (OpFwidthCoarse x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 215 (1 + length opsSer) : opsSer
  spirvSerialize (OpEmitVertex) = let opsSer = concat [] in makeInstructionHeader 218 (1 + length opsSer) : opsSer
  spirvSerialize (OpEndPrimitive) = let opsSer = concat [] in makeInstructionHeader 219 (1 + length opsSer) : opsSer
  spirvSerialize (OpEmitStreamVertex x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 220 (1 + length opsSer) : opsSer
  spirvSerialize (OpEndStreamPrimitive x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 221 (1 + length opsSer) : opsSer
  spirvSerialize (OpControlBarrier x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 224 (1 + length opsSer) : opsSer
  spirvSerialize (OpMemoryBarrier x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 225 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicLoad x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 227 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicStore x0 x1 x2 x3) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 228 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicExchange x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 229 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicCompareExchange x2 x3 x4 x5 x6 x7 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7] in makeInstructionHeader 230 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicCompareExchangeWeak x2 x3 x4 x5 x6 x7 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7] in makeInstructionHeader 231 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicIIncrement x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 232 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicIDecrement x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 233 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicIAdd x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 234 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicISub x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 235 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicSMin x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 236 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicUMin x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 237 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicSMax x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 238 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicUMax x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 239 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicAnd x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 240 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicOr x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 241 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicXor x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 242 (1 + length opsSer) : opsSer
  spirvSerialize (OpPhi x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 245 (1 + length opsSer) : opsSer
  spirvSerialize (OpLoopMerge x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 246 (1 + length opsSer) : opsSer
  spirvSerialize (OpSelectionMerge x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 247 (1 + length opsSer) : opsSer
  spirvSerialize (OpLabel x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 248 (1 + length opsSer) : opsSer
  spirvSerialize (OpBranch x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 249 (1 + length opsSer) : opsSer
  spirvSerialize (OpBranchConditional x0 x1 x2 x3) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 250 (1 + length opsSer) : opsSer
  spirvSerialize (OpSwitch x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 251 (1 + length opsSer) : opsSer
  spirvSerialize (OpKill) = let opsSer = concat [] in makeInstructionHeader 252 (1 + length opsSer) : opsSer
  spirvSerialize (OpReturn) = let opsSer = concat [] in makeInstructionHeader 253 (1 + length opsSer) : opsSer
  spirvSerialize (OpReturnValue x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 254 (1 + length opsSer) : opsSer
  spirvSerialize (OpUnreachable) = let opsSer = concat [] in makeInstructionHeader 255 (1 + length opsSer) : opsSer
  spirvSerialize (OpLifetimeStart x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 256 (1 + length opsSer) : opsSer
  spirvSerialize (OpLifetimeStop x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 257 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupAsyncCopy x2 x3 x4 x5 x6 x7 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7] in makeInstructionHeader 259 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupWaitEvents x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 260 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupAll x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 261 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupAny x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 262 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupBroadcast x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 263 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupIAdd x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 264 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupFAdd x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 265 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupFMin x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 266 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupUMin x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 267 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupSMin x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 268 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupFMax x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 269 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupUMax x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 270 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupSMax x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 271 (1 + length opsSer) : opsSer
  spirvSerialize (OpReadPipe x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 274 (1 + length opsSer) : opsSer
  spirvSerialize (OpWritePipe x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 275 (1 + length opsSer) : opsSer
  spirvSerialize (OpReservedReadPipe x2 x3 x4 x5 x6 x7 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7] in makeInstructionHeader 276 (1 + length opsSer) : opsSer
  spirvSerialize (OpReservedWritePipe x2 x3 x4 x5 x6 x7 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7] in makeInstructionHeader 277 (1 + length opsSer) : opsSer
  spirvSerialize (OpReserveReadPipePackets x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 278 (1 + length opsSer) : opsSer
  spirvSerialize (OpReserveWritePipePackets x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 279 (1 + length opsSer) : opsSer
  spirvSerialize (OpCommitReadPipe x0 x1 x2 x3) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 280 (1 + length opsSer) : opsSer
  spirvSerialize (OpCommitWritePipe x0 x1 x2 x3) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 281 (1 + length opsSer) : opsSer
  spirvSerialize (OpIsValidReserveId x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 282 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetNumPipePackets x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 283 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetMaxPipePackets x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 284 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupReserveReadPipePackets x2 x3 x4 x5 x6 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6] in makeInstructionHeader 285 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupReserveWritePipePackets x2 x3 x4 x5 x6 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6] in makeInstructionHeader 286 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupCommitReadPipe x0 x1 x2 x3 x4) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 287 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupCommitWritePipe x0 x1 x2 x3 x4) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 288 (1 + length opsSer) : opsSer
  spirvSerialize (OpEnqueueMarker x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 291 (1 + length opsSer) : opsSer
  spirvSerialize (OpEnqueueKernel x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7, spirvSerialize x8, spirvSerialize x9, spirvSerialize x10, spirvSerialize x11, spirvSerialize x12] in makeInstructionHeader 292 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetKernelNDrangeSubGroupCount x2 x3 x4 x5 x6 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6] in makeInstructionHeader 293 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetKernelNDrangeMaxSubGroupSize x2 x3 x4 x5 x6 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6] in makeInstructionHeader 294 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetKernelWorkGroupSize x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 295 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetKernelPreferredWorkGroupSizeMultiple x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 296 (1 + length opsSer) : opsSer
  spirvSerialize (OpRetainEvent x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 297 (1 + length opsSer) : opsSer
  spirvSerialize (OpReleaseEvent x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 298 (1 + length opsSer) : opsSer
  spirvSerialize (OpCreateUserEvent x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 299 (1 + length opsSer) : opsSer
  spirvSerialize (OpIsValidEvent x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 300 (1 + length opsSer) : opsSer
  spirvSerialize (OpSetUserEventStatus x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 301 (1 + length opsSer) : opsSer
  spirvSerialize (OpCaptureEventProfilingInfo x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 302 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetDefaultQueue x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 303 (1 + length opsSer) : opsSer
  spirvSerialize (OpBuildNDRange x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 304 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseSampleImplicitLod x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 305 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseSampleExplicitLod x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 306 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseSampleDrefImplicitLod x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 307 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseSampleDrefExplicitLod x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 308 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseFetch x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 313 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseGather x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 314 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseDrefGather x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 315 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseTexelsResident x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 316 (1 + length opsSer) : opsSer
  spirvSerialize (OpNoLine) = let opsSer = concat [] in makeInstructionHeader 317 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicFlagTestAndSet x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 318 (1 + length opsSer) : opsSer
  spirvSerialize (OpAtomicFlagClear x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 319 (1 + length opsSer) : opsSer
  spirvSerialize (OpImageSparseRead x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 320 (1 + length opsSer) : opsSer
  spirvSerialize (OpSizeOf x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 321 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypePipeStorage x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 322 (1 + length opsSer) : opsSer
  spirvSerialize (OpConstantPipeStorage x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 323 (1 + length opsSer) : opsSer
  spirvSerialize (OpCreatePipeFromPipeStorage x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 324 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetKernelLocalSizeForSubgroupCount x2 x3 x4 x5 x6 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6] in makeInstructionHeader 325 (1 + length opsSer) : opsSer
  spirvSerialize (OpGetKernelMaxNumSubgroups x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 326 (1 + length opsSer) : opsSer
  spirvSerialize (OpTypeNamedBarrier x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 327 (1 + length opsSer) : opsSer
  spirvSerialize (OpNamedBarrierInitialize x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 328 (1 + length opsSer) : opsSer
  spirvSerialize (OpMemoryNamedBarrier x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 329 (1 + length opsSer) : opsSer
  spirvSerialize (OpModuleProcessed x0) = let opsSer = concat [spirvSerialize x0] in makeInstructionHeader 330 (1 + length opsSer) : opsSer
  spirvSerialize (OpExecutionModeId x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 331 (1 + length opsSer) : opsSer
  spirvSerialize (OpDecorateId x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 332 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformElect x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 333 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformAll x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 334 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformAny x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 335 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformAllEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 336 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBroadcast x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 337 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBroadcastFirst x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 338 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBallot x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 339 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformInverseBallot x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 340 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBallotBitExtract x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 341 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBallotBitCount x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 342 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBallotFindLSB x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 343 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBallotFindMSB x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 344 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformShuffle x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 345 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformShuffleXor x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 346 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformShuffleUp x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 347 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformShuffleDown x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 348 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformIAdd x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 349 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformFAdd x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 350 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformIMul x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 351 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformFMul x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 352 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformSMin x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 353 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformUMin x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 354 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformFMin x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 355 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformSMax x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 356 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformUMax x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 357 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformFMax x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 358 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBitwiseAnd x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 359 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBitwiseOr x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 360 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformBitwiseXor x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 361 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformLogicalAnd x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 362 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformLogicalOr x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 363 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformLogicalXor x2 x3 x4 x5 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in makeInstructionHeader 364 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformQuadBroadcast x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 365 (1 + length opsSer) : opsSer
  spirvSerialize (OpGroupNonUniformQuadSwap x2 x3 x4 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in makeInstructionHeader 366 (1 + length opsSer) : opsSer
  spirvSerialize (OpCopyLogical x2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 400 (1 + length opsSer) : opsSer
  spirvSerialize (OpPtrEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 401 (1 + length opsSer) : opsSer
  spirvSerialize (OpPtrNotEqual x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 402 (1 + length opsSer) : opsSer
  spirvSerialize (OpPtrDiff x2 x3 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in makeInstructionHeader 403 (1 + length opsSer) : opsSer
  spirvSerialize (OpDecorateString x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 5632 (1 + length opsSer) : opsSer
  spirvSerialize (OpDecorateStringGOOGLE x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in makeInstructionHeader 5632 (1 + length opsSer) : opsSer
  spirvSerialize (OpMemberDecorateString x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 5633 (1 + length opsSer) : opsSer
  spirvSerialize (OpMemberDecorateStringGOOGLE x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in makeInstructionHeader 5633 (1 + length opsSer) : opsSer
data GlslInstruction = GlslRound IdRef | GlslRoundEven IdRef | GlslTrunc IdRef | GlslFAbs IdRef | GlslSAbs IdRef | GlslFSign IdRef | GlslSSign IdRef | GlslFloor IdRef | GlslCeil IdRef | GlslFract IdRef | GlslRadians IdRef | GlslDegrees IdRef | GlslSin IdRef | GlslCos IdRef | GlslTan IdRef | GlslAsin IdRef | GlslAcos IdRef | GlslAtan IdRef | GlslSinh IdRef | GlslCosh IdRef | GlslTanh IdRef | GlslAsinh IdRef | GlslAcosh IdRef | GlslAtanh IdRef | GlslAtan2 IdRef IdRef | GlslPow IdRef IdRef | GlslExp IdRef | GlslLog IdRef | GlslExp2 IdRef | GlslLog2 IdRef | GlslSqrt IdRef | GlslInverseSqrt IdRef | GlslDeterminant IdRef | GlslMatrixInverse IdRef | GlslModf IdRef IdRef | GlslModfStruct IdRef | GlslFMin IdRef IdRef | GlslUMin IdRef IdRef | GlslSMin IdRef IdRef | GlslFMax IdRef IdRef | GlslUMax IdRef IdRef | GlslSMax IdRef IdRef | GlslFClamp IdRef IdRef IdRef | GlslUClamp IdRef IdRef IdRef | GlslSClamp IdRef IdRef IdRef | GlslFMix IdRef IdRef IdRef | GlslIMix IdRef IdRef IdRef | GlslStep IdRef IdRef | GlslSmoothStep IdRef IdRef IdRef | GlslFma IdRef IdRef IdRef | GlslFrexp IdRef IdRef | GlslFrexpStruct IdRef | GlslLdexp IdRef IdRef | GlslPackSnorm4x8 IdRef | GlslPackUnorm4x8 IdRef | GlslPackSnorm2x16 IdRef | GlslPackUnorm2x16 IdRef | GlslPackHalf2x16 IdRef | GlslPackDouble2x32 IdRef | GlslUnpackSnorm2x16 IdRef | GlslUnpackUnorm2x16 IdRef | GlslUnpackHalf2x16 IdRef | GlslUnpackSnorm4x8 IdRef | GlslUnpackUnorm4x8 IdRef | GlslUnpackDouble2x32 IdRef | GlslLength IdRef | GlslDistance IdRef IdRef | GlslCross IdRef IdRef | GlslNormalize IdRef | GlslFaceForward IdRef IdRef IdRef | GlslReflect IdRef IdRef | GlslRefract IdRef IdRef IdRef | GlslFindILsb IdRef | GlslFindSMsb IdRef | GlslFindUMsb IdRef | GlslInterpolateAtCentroid IdRef | GlslInterpolateAtSample IdRef IdRef | GlslInterpolateAtOffset IdRef IdRef | GlslNMin IdRef IdRef | GlslNMax IdRef IdRef | GlslNClamp IdRef IdRef IdRef
  deriving (Show, Eq, Ord)
instance SPIRVInfo GlslInstruction where
  infoCapabilities (GlslRound x0) = []
  infoCapabilities (GlslRoundEven x0) = []
  infoCapabilities (GlslTrunc x0) = []
  infoCapabilities (GlslFAbs x0) = []
  infoCapabilities (GlslSAbs x0) = []
  infoCapabilities (GlslFSign x0) = []
  infoCapabilities (GlslSSign x0) = []
  infoCapabilities (GlslFloor x0) = []
  infoCapabilities (GlslCeil x0) = []
  infoCapabilities (GlslFract x0) = []
  infoCapabilities (GlslRadians x0) = []
  infoCapabilities (GlslDegrees x0) = []
  infoCapabilities (GlslSin x0) = []
  infoCapabilities (GlslCos x0) = []
  infoCapabilities (GlslTan x0) = []
  infoCapabilities (GlslAsin x0) = []
  infoCapabilities (GlslAcos x0) = []
  infoCapabilities (GlslAtan x0) = []
  infoCapabilities (GlslSinh x0) = []
  infoCapabilities (GlslCosh x0) = []
  infoCapabilities (GlslTanh x0) = []
  infoCapabilities (GlslAsinh x0) = []
  infoCapabilities (GlslAcosh x0) = []
  infoCapabilities (GlslAtanh x0) = []
  infoCapabilities (GlslAtan2 x0 x1) = []
  infoCapabilities (GlslPow x0 x1) = []
  infoCapabilities (GlslExp x0) = []
  infoCapabilities (GlslLog x0) = []
  infoCapabilities (GlslExp2 x0) = []
  infoCapabilities (GlslLog2 x0) = []
  infoCapabilities (GlslSqrt x0) = []
  infoCapabilities (GlslInverseSqrt x0) = []
  infoCapabilities (GlslDeterminant x0) = []
  infoCapabilities (GlslMatrixInverse x0) = []
  infoCapabilities (GlslModf x0 x1) = []
  infoCapabilities (GlslModfStruct x0) = []
  infoCapabilities (GlslFMin x0 x1) = []
  infoCapabilities (GlslUMin x0 x1) = []
  infoCapabilities (GlslSMin x0 x1) = []
  infoCapabilities (GlslFMax x0 x1) = []
  infoCapabilities (GlslUMax x0 x1) = []
  infoCapabilities (GlslSMax x0 x1) = []
  infoCapabilities (GlslFClamp x0 x1 x2) = []
  infoCapabilities (GlslUClamp x0 x1 x2) = []
  infoCapabilities (GlslSClamp x0 x1 x2) = []
  infoCapabilities (GlslFMix x0 x1 x2) = []
  infoCapabilities (GlslIMix x0 x1 x2) = []
  infoCapabilities (GlslStep x0 x1) = []
  infoCapabilities (GlslSmoothStep x0 x1 x2) = []
  infoCapabilities (GlslFma x0 x1 x2) = []
  infoCapabilities (GlslFrexp x0 x1) = []
  infoCapabilities (GlslFrexpStruct x0) = []
  infoCapabilities (GlslLdexp x0 x1) = []
  infoCapabilities (GlslPackSnorm4x8 x0) = []
  infoCapabilities (GlslPackUnorm4x8 x0) = []
  infoCapabilities (GlslPackSnorm2x16 x0) = []
  infoCapabilities (GlslPackUnorm2x16 x0) = []
  infoCapabilities (GlslPackHalf2x16 x0) = []
  infoCapabilities (GlslPackDouble2x32 x0) = [CapabilityFloat64]
  infoCapabilities (GlslUnpackSnorm2x16 x0) = []
  infoCapabilities (GlslUnpackUnorm2x16 x0) = []
  infoCapabilities (GlslUnpackHalf2x16 x0) = []
  infoCapabilities (GlslUnpackSnorm4x8 x0) = []
  infoCapabilities (GlslUnpackUnorm4x8 x0) = []
  infoCapabilities (GlslUnpackDouble2x32 x0) = [CapabilityFloat64]
  infoCapabilities (GlslLength x0) = []
  infoCapabilities (GlslDistance x0 x1) = []
  infoCapabilities (GlslCross x0 x1) = []
  infoCapabilities (GlslNormalize x0) = []
  infoCapabilities (GlslFaceForward x0 x1 x2) = []
  infoCapabilities (GlslReflect x0 x1) = []
  infoCapabilities (GlslRefract x0 x1 x2) = []
  infoCapabilities (GlslFindILsb x0) = []
  infoCapabilities (GlslFindSMsb x0) = []
  infoCapabilities (GlslFindUMsb x0) = []
  infoCapabilities (GlslInterpolateAtCentroid x0) = [CapabilityInterpolationFunction]
  infoCapabilities (GlslInterpolateAtSample x0 x1) = [CapabilityInterpolationFunction]
  infoCapabilities (GlslInterpolateAtOffset x0 x1) = [CapabilityInterpolationFunction]
  infoCapabilities (GlslNMin x0 x1) = []
  infoCapabilities (GlslNMax x0 x1) = []
  infoCapabilities (GlslNClamp x0 x1 x2) = []
  infoExtensions (GlslRound x0) = []
  infoExtensions (GlslRoundEven x0) = []
  infoExtensions (GlslTrunc x0) = []
  infoExtensions (GlslFAbs x0) = []
  infoExtensions (GlslSAbs x0) = []
  infoExtensions (GlslFSign x0) = []
  infoExtensions (GlslSSign x0) = []
  infoExtensions (GlslFloor x0) = []
  infoExtensions (GlslCeil x0) = []
  infoExtensions (GlslFract x0) = []
  infoExtensions (GlslRadians x0) = []
  infoExtensions (GlslDegrees x0) = []
  infoExtensions (GlslSin x0) = []
  infoExtensions (GlslCos x0) = []
  infoExtensions (GlslTan x0) = []
  infoExtensions (GlslAsin x0) = []
  infoExtensions (GlslAcos x0) = []
  infoExtensions (GlslAtan x0) = []
  infoExtensions (GlslSinh x0) = []
  infoExtensions (GlslCosh x0) = []
  infoExtensions (GlslTanh x0) = []
  infoExtensions (GlslAsinh x0) = []
  infoExtensions (GlslAcosh x0) = []
  infoExtensions (GlslAtanh x0) = []
  infoExtensions (GlslAtan2 x0 x1) = []
  infoExtensions (GlslPow x0 x1) = []
  infoExtensions (GlslExp x0) = []
  infoExtensions (GlslLog x0) = []
  infoExtensions (GlslExp2 x0) = []
  infoExtensions (GlslLog2 x0) = []
  infoExtensions (GlslSqrt x0) = []
  infoExtensions (GlslInverseSqrt x0) = []
  infoExtensions (GlslDeterminant x0) = []
  infoExtensions (GlslMatrixInverse x0) = []
  infoExtensions (GlslModf x0 x1) = []
  infoExtensions (GlslModfStruct x0) = []
  infoExtensions (GlslFMin x0 x1) = []
  infoExtensions (GlslUMin x0 x1) = []
  infoExtensions (GlslSMin x0 x1) = []
  infoExtensions (GlslFMax x0 x1) = []
  infoExtensions (GlslUMax x0 x1) = []
  infoExtensions (GlslSMax x0 x1) = []
  infoExtensions (GlslFClamp x0 x1 x2) = []
  infoExtensions (GlslUClamp x0 x1 x2) = []
  infoExtensions (GlslSClamp x0 x1 x2) = []
  infoExtensions (GlslFMix x0 x1 x2) = []
  infoExtensions (GlslIMix x0 x1 x2) = []
  infoExtensions (GlslStep x0 x1) = []
  infoExtensions (GlslSmoothStep x0 x1 x2) = []
  infoExtensions (GlslFma x0 x1 x2) = []
  infoExtensions (GlslFrexp x0 x1) = []
  infoExtensions (GlslFrexpStruct x0) = []
  infoExtensions (GlslLdexp x0 x1) = []
  infoExtensions (GlslPackSnorm4x8 x0) = []
  infoExtensions (GlslPackUnorm4x8 x0) = []
  infoExtensions (GlslPackSnorm2x16 x0) = []
  infoExtensions (GlslPackUnorm2x16 x0) = []
  infoExtensions (GlslPackHalf2x16 x0) = []
  infoExtensions (GlslPackDouble2x32 x0) = []
  infoExtensions (GlslUnpackSnorm2x16 x0) = []
  infoExtensions (GlslUnpackUnorm2x16 x0) = []
  infoExtensions (GlslUnpackHalf2x16 x0) = []
  infoExtensions (GlslUnpackSnorm4x8 x0) = []
  infoExtensions (GlslUnpackUnorm4x8 x0) = []
  infoExtensions (GlslUnpackDouble2x32 x0) = []
  infoExtensions (GlslLength x0) = []
  infoExtensions (GlslDistance x0 x1) = []
  infoExtensions (GlslCross x0 x1) = []
  infoExtensions (GlslNormalize x0) = []
  infoExtensions (GlslFaceForward x0 x1 x2) = []
  infoExtensions (GlslReflect x0 x1) = []
  infoExtensions (GlslRefract x0 x1 x2) = []
  infoExtensions (GlslFindILsb x0) = []
  infoExtensions (GlslFindSMsb x0) = []
  infoExtensions (GlslFindUMsb x0) = []
  infoExtensions (GlslInterpolateAtCentroid x0) = []
  infoExtensions (GlslInterpolateAtSample x0 x1) = []
  infoExtensions (GlslInterpolateAtOffset x0 x1) = []
  infoExtensions (GlslNMin x0 x1) = []
  infoExtensions (GlslNMax x0 x1) = []
  infoExtensions (GlslNClamp x0 x1 x2) = []
  infoVersionRange (GlslRound x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslRoundEven x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslTrunc x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFAbs x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSAbs x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFSign x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSSign x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFloor x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslCeil x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFract x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslRadians x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslDegrees x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSin x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslCos x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslTan x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslAsin x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslAcos x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslAtan x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSinh x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslCosh x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslTanh x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslAsinh x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslAcosh x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslAtanh x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslAtan2 x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslPow x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslExp x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslLog x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslExp2 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslLog2 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSqrt x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslInverseSqrt x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslDeterminant x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslMatrixInverse x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslModf x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslModfStruct x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFMin x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUMin x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSMin x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFMax x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUMax x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSMax x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFClamp x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUClamp x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSClamp x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFMix x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslIMix x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslStep x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslSmoothStep x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFma x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFrexp x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFrexpStruct x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslLdexp x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslPackSnorm4x8 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslPackUnorm4x8 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslPackSnorm2x16 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslPackUnorm2x16 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslPackHalf2x16 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslPackDouble2x32 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUnpackSnorm2x16 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUnpackUnorm2x16 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUnpackHalf2x16 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUnpackSnorm4x8 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUnpackUnorm4x8 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslUnpackDouble2x32 x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslLength x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslDistance x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslCross x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslNormalize x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFaceForward x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslReflect x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslRefract x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFindILsb x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFindSMsb x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslFindUMsb x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslInterpolateAtCentroid x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslInterpolateAtSample x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslInterpolateAtOffset x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslNMin x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslNMax x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (GlslNClamp x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize GlslInstruction where
  spirvSerialize (GlslRound x0) = let opsSer = concat [spirvSerialize x0] in 1 : opsSer
  spirvSerialize (GlslRoundEven x0) = let opsSer = concat [spirvSerialize x0] in 2 : opsSer
  spirvSerialize (GlslTrunc x0) = let opsSer = concat [spirvSerialize x0] in 3 : opsSer
  spirvSerialize (GlslFAbs x0) = let opsSer = concat [spirvSerialize x0] in 4 : opsSer
  spirvSerialize (GlslSAbs x0) = let opsSer = concat [spirvSerialize x0] in 5 : opsSer
  spirvSerialize (GlslFSign x0) = let opsSer = concat [spirvSerialize x0] in 6 : opsSer
  spirvSerialize (GlslSSign x0) = let opsSer = concat [spirvSerialize x0] in 7 : opsSer
  spirvSerialize (GlslFloor x0) = let opsSer = concat [spirvSerialize x0] in 8 : opsSer
  spirvSerialize (GlslCeil x0) = let opsSer = concat [spirvSerialize x0] in 9 : opsSer
  spirvSerialize (GlslFract x0) = let opsSer = concat [spirvSerialize x0] in 10 : opsSer
  spirvSerialize (GlslRadians x0) = let opsSer = concat [spirvSerialize x0] in 11 : opsSer
  spirvSerialize (GlslDegrees x0) = let opsSer = concat [spirvSerialize x0] in 12 : opsSer
  spirvSerialize (GlslSin x0) = let opsSer = concat [spirvSerialize x0] in 13 : opsSer
  spirvSerialize (GlslCos x0) = let opsSer = concat [spirvSerialize x0] in 14 : opsSer
  spirvSerialize (GlslTan x0) = let opsSer = concat [spirvSerialize x0] in 15 : opsSer
  spirvSerialize (GlslAsin x0) = let opsSer = concat [spirvSerialize x0] in 16 : opsSer
  spirvSerialize (GlslAcos x0) = let opsSer = concat [spirvSerialize x0] in 17 : opsSer
  spirvSerialize (GlslAtan x0) = let opsSer = concat [spirvSerialize x0] in 18 : opsSer
  spirvSerialize (GlslSinh x0) = let opsSer = concat [spirvSerialize x0] in 19 : opsSer
  spirvSerialize (GlslCosh x0) = let opsSer = concat [spirvSerialize x0] in 20 : opsSer
  spirvSerialize (GlslTanh x0) = let opsSer = concat [spirvSerialize x0] in 21 : opsSer
  spirvSerialize (GlslAsinh x0) = let opsSer = concat [spirvSerialize x0] in 22 : opsSer
  spirvSerialize (GlslAcosh x0) = let opsSer = concat [spirvSerialize x0] in 23 : opsSer
  spirvSerialize (GlslAtanh x0) = let opsSer = concat [spirvSerialize x0] in 24 : opsSer
  spirvSerialize (GlslAtan2 x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 25 : opsSer
  spirvSerialize (GlslPow x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 26 : opsSer
  spirvSerialize (GlslExp x0) = let opsSer = concat [spirvSerialize x0] in 27 : opsSer
  spirvSerialize (GlslLog x0) = let opsSer = concat [spirvSerialize x0] in 28 : opsSer
  spirvSerialize (GlslExp2 x0) = let opsSer = concat [spirvSerialize x0] in 29 : opsSer
  spirvSerialize (GlslLog2 x0) = let opsSer = concat [spirvSerialize x0] in 30 : opsSer
  spirvSerialize (GlslSqrt x0) = let opsSer = concat [spirvSerialize x0] in 31 : opsSer
  spirvSerialize (GlslInverseSqrt x0) = let opsSer = concat [spirvSerialize x0] in 32 : opsSer
  spirvSerialize (GlslDeterminant x0) = let opsSer = concat [spirvSerialize x0] in 33 : opsSer
  spirvSerialize (GlslMatrixInverse x0) = let opsSer = concat [spirvSerialize x0] in 34 : opsSer
  spirvSerialize (GlslModf x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 35 : opsSer
  spirvSerialize (GlslModfStruct x0) = let opsSer = concat [spirvSerialize x0] in 36 : opsSer
  spirvSerialize (GlslFMin x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 37 : opsSer
  spirvSerialize (GlslUMin x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 38 : opsSer
  spirvSerialize (GlslSMin x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 39 : opsSer
  spirvSerialize (GlslFMax x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 40 : opsSer
  spirvSerialize (GlslUMax x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 41 : opsSer
  spirvSerialize (GlslSMax x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 42 : opsSer
  spirvSerialize (GlslFClamp x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 43 : opsSer
  spirvSerialize (GlslUClamp x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 44 : opsSer
  spirvSerialize (GlslSClamp x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 45 : opsSer
  spirvSerialize (GlslFMix x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 46 : opsSer
  spirvSerialize (GlslIMix x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 47 : opsSer
  spirvSerialize (GlslStep x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 48 : opsSer
  spirvSerialize (GlslSmoothStep x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 49 : opsSer
  spirvSerialize (GlslFma x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 50 : opsSer
  spirvSerialize (GlslFrexp x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 51 : opsSer
  spirvSerialize (GlslFrexpStruct x0) = let opsSer = concat [spirvSerialize x0] in 52 : opsSer
  spirvSerialize (GlslLdexp x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 53 : opsSer
  spirvSerialize (GlslPackSnorm4x8 x0) = let opsSer = concat [spirvSerialize x0] in 54 : opsSer
  spirvSerialize (GlslPackUnorm4x8 x0) = let opsSer = concat [spirvSerialize x0] in 55 : opsSer
  spirvSerialize (GlslPackSnorm2x16 x0) = let opsSer = concat [spirvSerialize x0] in 56 : opsSer
  spirvSerialize (GlslPackUnorm2x16 x0) = let opsSer = concat [spirvSerialize x0] in 57 : opsSer
  spirvSerialize (GlslPackHalf2x16 x0) = let opsSer = concat [spirvSerialize x0] in 58 : opsSer
  spirvSerialize (GlslPackDouble2x32 x0) = let opsSer = concat [spirvSerialize x0] in 59 : opsSer
  spirvSerialize (GlslUnpackSnorm2x16 x0) = let opsSer = concat [spirvSerialize x0] in 60 : opsSer
  spirvSerialize (GlslUnpackUnorm2x16 x0) = let opsSer = concat [spirvSerialize x0] in 61 : opsSer
  spirvSerialize (GlslUnpackHalf2x16 x0) = let opsSer = concat [spirvSerialize x0] in 62 : opsSer
  spirvSerialize (GlslUnpackSnorm4x8 x0) = let opsSer = concat [spirvSerialize x0] in 63 : opsSer
  spirvSerialize (GlslUnpackUnorm4x8 x0) = let opsSer = concat [spirvSerialize x0] in 64 : opsSer
  spirvSerialize (GlslUnpackDouble2x32 x0) = let opsSer = concat [spirvSerialize x0] in 65 : opsSer
  spirvSerialize (GlslLength x0) = let opsSer = concat [spirvSerialize x0] in 66 : opsSer
  spirvSerialize (GlslDistance x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 67 : opsSer
  spirvSerialize (GlslCross x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 68 : opsSer
  spirvSerialize (GlslNormalize x0) = let opsSer = concat [spirvSerialize x0] in 69 : opsSer
  spirvSerialize (GlslFaceForward x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 70 : opsSer
  spirvSerialize (GlslReflect x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 71 : opsSer
  spirvSerialize (GlslRefract x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 72 : opsSer
  spirvSerialize (GlslFindILsb x0) = let opsSer = concat [spirvSerialize x0] in 73 : opsSer
  spirvSerialize (GlslFindSMsb x0) = let opsSer = concat [spirvSerialize x0] in 74 : opsSer
  spirvSerialize (GlslFindUMsb x0) = let opsSer = concat [spirvSerialize x0] in 75 : opsSer
  spirvSerialize (GlslInterpolateAtCentroid x0) = let opsSer = concat [spirvSerialize x0] in 76 : opsSer
  spirvSerialize (GlslInterpolateAtSample x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 77 : opsSer
  spirvSerialize (GlslInterpolateAtOffset x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 78 : opsSer
  spirvSerialize (GlslNMin x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 79 : opsSer
  spirvSerialize (GlslNMax x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 80 : opsSer
  spirvSerialize (GlslNClamp x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 81 : opsSer
data DbgInfoInstruction = DbgInfoDebugInfoNone | DbgInfoDebugCompilationUnit IdRef LiteralInteger LiteralInteger | DbgInfoDebugTypeBasic IdRef IdRef DebugBaseTypeAttributeEncoding | DbgInfoDebugTypePointer IdRef StorageClass DebugInfoFlags | DbgInfoDebugTypeQualifier IdRef DebugTypeQualifier | DbgInfoDebugTypeArray IdRef [IdRef] | DbgInfoDebugTypeVector IdRef LiteralInteger | DbgInfoDebugTypedef IdRef IdRef IdRef LiteralInteger LiteralInteger IdRef | DbgInfoDebugTypeFunction IdRef [IdRef] | DbgInfoDebugTypeEnum IdRef IdRef IdRef LiteralInteger LiteralInteger IdRef IdRef DebugInfoFlags [PairIdRefIdRef] | DbgInfoDebugTypeComposite IdRef DebugCompositeType IdRef LiteralInteger LiteralInteger IdRef IdRef DebugInfoFlags [IdRef] | DbgInfoDebugTypeMember IdRef IdRef IdRef LiteralInteger LiteralInteger IdRef IdRef IdRef DebugInfoFlags (Maybe IdRef) | DbgInfoDebugTypeInheritance IdRef IdRef IdRef IdRef DebugInfoFlags | DbgInfoDebugTypePtrToMember IdRef IdRef | DbgInfoDebugTypeTemplate IdRef [IdRef] | DbgInfoDebugTypeTemplateParameter IdRef IdRef IdRef IdRef LiteralInteger LiteralInteger | DbgInfoDebugTypeTemplateTemplateParameter IdRef IdRef IdRef LiteralInteger LiteralInteger | DbgInfoDebugTypeTemplateParameterPack IdRef IdRef LiteralInteger LiteralInteger [IdRef] | DbgInfoDebugGlobalVariable IdRef IdRef IdRef LiteralInteger LiteralInteger IdRef IdRef IdRef DebugInfoFlags (Maybe IdRef) | DbgInfoDebugFunctionDeclaration IdRef IdRef IdRef LiteralInteger LiteralInteger IdRef IdRef DebugInfoFlags | DbgInfoDebugFunction IdRef IdRef IdRef LiteralInteger LiteralInteger IdRef IdRef DebugInfoFlags LiteralInteger IdRef (Maybe IdRef) | DbgInfoDebugLexicalBlock IdRef LiteralInteger LiteralInteger IdRef (Maybe IdRef) | DbgInfoDebugLexicalBlockDiscriminator IdRef LiteralInteger IdRef | DbgInfoDebugScope IdRef (Maybe IdRef) | DbgInfoDebugNoScope | DbgInfoDebugInlinedAt LiteralInteger IdRef (Maybe IdRef) | DbgInfoDebugLocalVariable IdRef IdRef IdRef LiteralInteger LiteralInteger IdRef (Maybe LiteralInteger) | DbgInfoDebugInlinedVariable IdRef IdRef | DbgInfoDebugDeclare IdRef IdRef IdRef | DbgInfoDebugValue IdRef IdRef [IdRef] | DbgInfoDebugOperation DebugOperation [LiteralInteger] | DbgInfoDebugExpression [IdRef] | DbgInfoDebugMacroDef IdRef LiteralInteger IdRef (Maybe IdRef) | DbgInfoDebugMacroUndef IdRef LiteralInteger IdRef
  deriving (Show, Eq, Ord)
instance SPIRVInfo DbgInfoInstruction where
  infoCapabilities (DbgInfoDebugInfoNone) = []
  infoCapabilities (DbgInfoDebugCompilationUnit x0 x1 x2) = []
  infoCapabilities (DbgInfoDebugTypeBasic x0 x1 x2) = []
  infoCapabilities (DbgInfoDebugTypePointer x0 x1 x2) = []
  infoCapabilities (DbgInfoDebugTypeQualifier x0 x1) = []
  infoCapabilities (DbgInfoDebugTypeArray x0 x1) = []
  infoCapabilities (DbgInfoDebugTypeVector x0 x1) = []
  infoCapabilities (DbgInfoDebugTypedef x0 x1 x2 x3 x4 x5) = []
  infoCapabilities (DbgInfoDebugTypeFunction x0 x1) = []
  infoCapabilities (DbgInfoDebugTypeEnum x0 x1 x2 x3 x4 x5 x6 x7 x8) = []
  infoCapabilities (DbgInfoDebugTypeComposite x0 x1 x2 x3 x4 x5 x6 x7 x8) = []
  infoCapabilities (DbgInfoDebugTypeMember x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = []
  infoCapabilities (DbgInfoDebugTypeInheritance x0 x1 x2 x3 x4) = []
  infoCapabilities (DbgInfoDebugTypePtrToMember x0 x1) = []
  infoCapabilities (DbgInfoDebugTypeTemplate x0 x1) = []
  infoCapabilities (DbgInfoDebugTypeTemplateParameter x0 x1 x2 x3 x4 x5) = []
  infoCapabilities (DbgInfoDebugTypeTemplateTemplateParameter x0 x1 x2 x3 x4) = []
  infoCapabilities (DbgInfoDebugTypeTemplateParameterPack x0 x1 x2 x3 x4) = []
  infoCapabilities (DbgInfoDebugGlobalVariable x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = []
  infoCapabilities (DbgInfoDebugFunctionDeclaration x0 x1 x2 x3 x4 x5 x6 x7) = []
  infoCapabilities (DbgInfoDebugFunction x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = []
  infoCapabilities (DbgInfoDebugLexicalBlock x0 x1 x2 x3 x4) = []
  infoCapabilities (DbgInfoDebugLexicalBlockDiscriminator x0 x1 x2) = []
  infoCapabilities (DbgInfoDebugScope x0 x1) = []
  infoCapabilities (DbgInfoDebugNoScope) = []
  infoCapabilities (DbgInfoDebugInlinedAt x0 x1 x2) = []
  infoCapabilities (DbgInfoDebugLocalVariable x0 x1 x2 x3 x4 x5 x6) = []
  infoCapabilities (DbgInfoDebugInlinedVariable x0 x1) = []
  infoCapabilities (DbgInfoDebugDeclare x0 x1 x2) = []
  infoCapabilities (DbgInfoDebugValue x0 x1 x2) = []
  infoCapabilities (DbgInfoDebugOperation x0 x1) = []
  infoCapabilities (DbgInfoDebugExpression x0) = []
  infoCapabilities (DbgInfoDebugMacroDef x0 x1 x2 x3) = []
  infoCapabilities (DbgInfoDebugMacroUndef x0 x1 x2) = []
  infoExtensions (DbgInfoDebugInfoNone) = []
  infoExtensions (DbgInfoDebugCompilationUnit x0 x1 x2) = []
  infoExtensions (DbgInfoDebugTypeBasic x0 x1 x2) = []
  infoExtensions (DbgInfoDebugTypePointer x0 x1 x2) = []
  infoExtensions (DbgInfoDebugTypeQualifier x0 x1) = []
  infoExtensions (DbgInfoDebugTypeArray x0 x1) = []
  infoExtensions (DbgInfoDebugTypeVector x0 x1) = []
  infoExtensions (DbgInfoDebugTypedef x0 x1 x2 x3 x4 x5) = []
  infoExtensions (DbgInfoDebugTypeFunction x0 x1) = []
  infoExtensions (DbgInfoDebugTypeEnum x0 x1 x2 x3 x4 x5 x6 x7 x8) = []
  infoExtensions (DbgInfoDebugTypeComposite x0 x1 x2 x3 x4 x5 x6 x7 x8) = []
  infoExtensions (DbgInfoDebugTypeMember x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = []
  infoExtensions (DbgInfoDebugTypeInheritance x0 x1 x2 x3 x4) = []
  infoExtensions (DbgInfoDebugTypePtrToMember x0 x1) = []
  infoExtensions (DbgInfoDebugTypeTemplate x0 x1) = []
  infoExtensions (DbgInfoDebugTypeTemplateParameter x0 x1 x2 x3 x4 x5) = []
  infoExtensions (DbgInfoDebugTypeTemplateTemplateParameter x0 x1 x2 x3 x4) = []
  infoExtensions (DbgInfoDebugTypeTemplateParameterPack x0 x1 x2 x3 x4) = []
  infoExtensions (DbgInfoDebugGlobalVariable x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = []
  infoExtensions (DbgInfoDebugFunctionDeclaration x0 x1 x2 x3 x4 x5 x6 x7) = []
  infoExtensions (DbgInfoDebugFunction x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = []
  infoExtensions (DbgInfoDebugLexicalBlock x0 x1 x2 x3 x4) = []
  infoExtensions (DbgInfoDebugLexicalBlockDiscriminator x0 x1 x2) = []
  infoExtensions (DbgInfoDebugScope x0 x1) = []
  infoExtensions (DbgInfoDebugNoScope) = []
  infoExtensions (DbgInfoDebugInlinedAt x0 x1 x2) = []
  infoExtensions (DbgInfoDebugLocalVariable x0 x1 x2 x3 x4 x5 x6) = []
  infoExtensions (DbgInfoDebugInlinedVariable x0 x1) = []
  infoExtensions (DbgInfoDebugDeclare x0 x1 x2) = []
  infoExtensions (DbgInfoDebugValue x0 x1 x2) = []
  infoExtensions (DbgInfoDebugOperation x0 x1) = []
  infoExtensions (DbgInfoDebugExpression x0) = []
  infoExtensions (DbgInfoDebugMacroDef x0 x1 x2 x3) = []
  infoExtensions (DbgInfoDebugMacroUndef x0 x1 x2) = []
  infoVersionRange (DbgInfoDebugInfoNone) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugCompilationUnit x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeBasic x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypePointer x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeQualifier x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeArray x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeVector x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypedef x0 x1 x2 x3 x4 x5) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeFunction x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeEnum x0 x1 x2 x3 x4 x5 x6 x7 x8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeComposite x0 x1 x2 x3 x4 x5 x6 x7 x8) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeMember x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeInheritance x0 x1 x2 x3 x4) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypePtrToMember x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeTemplate x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeTemplateParameter x0 x1 x2 x3 x4 x5) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeTemplateTemplateParameter x0 x1 x2 x3 x4) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugTypeTemplateParameterPack x0 x1 x2 x3 x4) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugGlobalVariable x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugFunctionDeclaration x0 x1 x2 x3 x4 x5 x6 x7) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugFunction x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugLexicalBlock x0 x1 x2 x3 x4) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugLexicalBlockDiscriminator x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugScope x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugNoScope) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugInlinedAt x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugLocalVariable x0 x1 x2 x3 x4 x5 x6) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugInlinedVariable x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugDeclare x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugValue x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugOperation x0 x1) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugExpression x0) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugMacroDef x0 x1 x2 x3) = SPIRVVersionRange (Nothing, Nothing)
  infoVersionRange (DbgInfoDebugMacroUndef x0 x1 x2) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize DbgInfoInstruction where
  spirvSerialize (DbgInfoDebugInfoNone) = let opsSer = concat [] in 0 : opsSer
  spirvSerialize (DbgInfoDebugCompilationUnit x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 1 : opsSer
  spirvSerialize (DbgInfoDebugTypeBasic x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 2 : opsSer
  spirvSerialize (DbgInfoDebugTypePointer x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 3 : opsSer
  spirvSerialize (DbgInfoDebugTypeQualifier x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 4 : opsSer
  spirvSerialize (DbgInfoDebugTypeArray x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 5 : opsSer
  spirvSerialize (DbgInfoDebugTypeVector x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 6 : opsSer
  spirvSerialize (DbgInfoDebugTypedef x0 x1 x2 x3 x4 x5) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in 7 : opsSer
  spirvSerialize (DbgInfoDebugTypeFunction x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 8 : opsSer
  spirvSerialize (DbgInfoDebugTypeEnum x0 x1 x2 x3 x4 x5 x6 x7 x8) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7, spirvSerialize x8] in 9 : opsSer
  spirvSerialize (DbgInfoDebugTypeComposite x0 x1 x2 x3 x4 x5 x6 x7 x8) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7, spirvSerialize x8] in 10 : opsSer
  spirvSerialize (DbgInfoDebugTypeMember x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7, spirvSerialize x8, spirvSerialize x9] in 11 : opsSer
  spirvSerialize (DbgInfoDebugTypeInheritance x0 x1 x2 x3 x4) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in 12 : opsSer
  spirvSerialize (DbgInfoDebugTypePtrToMember x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 13 : opsSer
  spirvSerialize (DbgInfoDebugTypeTemplate x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 14 : opsSer
  spirvSerialize (DbgInfoDebugTypeTemplateParameter x0 x1 x2 x3 x4 x5) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5] in 15 : opsSer
  spirvSerialize (DbgInfoDebugTypeTemplateTemplateParameter x0 x1 x2 x3 x4) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in 16 : opsSer
  spirvSerialize (DbgInfoDebugTypeTemplateParameterPack x0 x1 x2 x3 x4) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in 17 : opsSer
  spirvSerialize (DbgInfoDebugGlobalVariable x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7, spirvSerialize x8, spirvSerialize x9] in 18 : opsSer
  spirvSerialize (DbgInfoDebugFunctionDeclaration x0 x1 x2 x3 x4 x5 x6 x7) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7] in 19 : opsSer
  spirvSerialize (DbgInfoDebugFunction x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6, spirvSerialize x7, spirvSerialize x8, spirvSerialize x9, spirvSerialize x10] in 20 : opsSer
  spirvSerialize (DbgInfoDebugLexicalBlock x0 x1 x2 x3 x4) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4] in 21 : opsSer
  spirvSerialize (DbgInfoDebugLexicalBlockDiscriminator x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 22 : opsSer
  spirvSerialize (DbgInfoDebugScope x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 23 : opsSer
  spirvSerialize (DbgInfoDebugNoScope) = let opsSer = concat [] in 24 : opsSer
  spirvSerialize (DbgInfoDebugInlinedAt x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 25 : opsSer
  spirvSerialize (DbgInfoDebugLocalVariable x0 x1 x2 x3 x4 x5 x6) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3, spirvSerialize x4, spirvSerialize x5, spirvSerialize x6] in 26 : opsSer
  spirvSerialize (DbgInfoDebugInlinedVariable x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 27 : opsSer
  spirvSerialize (DbgInfoDebugDeclare x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 28 : opsSer
  spirvSerialize (DbgInfoDebugValue x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 29 : opsSer
  spirvSerialize (DbgInfoDebugOperation x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 30 : opsSer
  spirvSerialize (DbgInfoDebugExpression x0) = let opsSer = concat [spirvSerialize x0] in 31 : opsSer
  spirvSerialize (DbgInfoDebugMacroDef x0 x1 x2 x3) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2, spirvSerialize x3] in 32 : opsSer
  spirvSerialize (DbgInfoDebugMacroUndef x0 x1 x2) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1, spirvSerialize x2] in 33 : opsSer
data DbgPrintInstruction = DbgPrintDebugPrintf IdRef [IdRef]
  deriving (Show, Eq, Ord)
instance SPIRVInfo DbgPrintInstruction where
  infoCapabilities (DbgPrintDebugPrintf x0 x1) = []
  infoExtensions (DbgPrintDebugPrintf x0 x1) = []
  infoVersionRange (DbgPrintDebugPrintf x0 x1) = SPIRVVersionRange (Nothing, Nothing)
instance SPIRVSerialize DbgPrintInstruction where
  spirvSerialize (DbgPrintDebugPrintf x0 x1) = let opsSer = concat [spirvSerialize x0, spirvSerialize x1] in 1 : opsSer
data ExtInstruction = GlslInstruction GlslInstruction | DbgInfoInstruction DbgInfoInstruction | DbgPrintInstruction DbgPrintInstruction
  deriving (Show, Eq, Ord)
instance SPIRVSerialize ExtInstruction where
  spirvSerialize (GlslInstruction x) = spirvSerialize x
  spirvSerialize (DbgInfoInstruction x) = spirvSerialize x
  spirvSerialize (DbgPrintInstruction x) = spirvSerialize x
instance SPIRVInfo ExtInstruction where
  infoCapabilities (GlslInstruction x) = infoCapabilities x
  infoCapabilities (DbgInfoInstruction x) = infoCapabilities x
  infoCapabilities (DbgPrintInstruction x) = infoCapabilities x
  infoExtensions (GlslInstruction x) = infoExtensions x
  infoExtensions (DbgInfoInstruction x) = infoExtensions x
  infoExtensions (DbgPrintInstruction x) = infoExtensions x
  infoVersionRange (GlslInstruction x) = infoVersionRange x
  infoVersionRange (DbgInfoInstruction x) = infoVersionRange x
  infoVersionRange (DbgPrintInstruction x) = infoVersionRange x
