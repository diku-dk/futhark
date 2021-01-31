module Futhark.CodeGen.Backends.SPIRV.Builder
  ( module Futhark.CodeGen.Backends.SPIRV.Spec,
    fromBindingId,
    fromSpecId,
    Builder,
    SpecId,
    BindingId,
    runBuilder,
    newId,
    newSpecId,
    getUserState,
    getUserStateS,
    modifyUserState,
    SPIRVConstant(..),
    SPIRVType(..),
    getTypeId,
    getConstId,
    instrRT,
    instrRTId,
    instrR,
    instr,
    glslInstr,
    getBuiltIn,
    makeInterfaceVar,
    makeFunctionVar,
    makeLocalMemArray,
    makeLocalMemArrayConst,
    makeArrayAccess,
    getType,
    assertType,
  )
where
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Word
import Futhark.CodeGen.Backends.SPIRV.Spec
import Futhark.IR.Primitive
import Futhark.IR.Prop.Types (int32)

newtype SpecId = SpecId Id
  deriving (Eq, Show, Ord)

fromSpecId :: SpecId -> Word32
fromSpecId (SpecId (Id w)) = w

newtype BindingId = BindingId Id
  deriving (Eq, Show, Ord)

fromBindingId :: BindingId -> Word32
fromBindingId (BindingId (Id w)) = w

data TypeOrConst
  = SPIRVType SPIRVType
  | SPIRVConstant SPIRVConstant
  deriving (Eq, Show, Ord)

data SPIRVState s = SPIRVState
  { spvRunningId :: Word32
  , spvRunningSpecId :: Word32
  , spvRunningBindingId :: Word32
  , -- For *every* instruction with an IdResult and an IdResultType, this map
    -- holds an entry mapping the IdResult to the SPIRVType representing the
    -- associated IdResultType
    spvIdTypes :: M.Map IdResult SPIRVType
  , -- All types and constants and the Ids that define them.
    -- XXX: Can be turned into an ordered map now that TypeOrConst is Ord
    spvTypesAndConsts :: [(TypeOrConst, Id)]
  , -- OpDecorate and OpMemberDecorate instructions
    spvDecorationInstructions :: [Instruction]
  , -- OpVariable instructions with anything but StorageBufferFunction storage class
    spvGlobalDeclarations :: [Instruction]
  , -- OpVariable instructions with StorageBufferFunction storage class
    spvFunVarDecls :: [Instruction]
  , -- Access chains used to conveniently access interface variables
    spvInterfaceAccessChains :: [Instruction]
  , -- Instructions that make up the body of the function
    spvBodyInstructions :: [Instruction]
  , -- For OpEntryPoint
    spvInterfaceIds :: [Id]
  , spvGlslExtId :: Id
  , spvBuiltIns :: M.Map BuiltIn Id
  , spvWorkgroupSizeSpecIds :: (SpecId, SpecId, SpecId)
  , spvUserState :: s
  }

newState :: s -> SPIRVState s
newState s = SPIRVState
  {
  -- Reserve id 1 for GLSL extended instruction set
    spvRunningId = 2
  , spvRunningSpecId = 0
  , spvRunningBindingId = 0
  , spvIdTypes = M.empty
  , spvTypesAndConsts = []
  , spvDecorationInstructions = []
  , spvGlobalDeclarations = []
  , spvFunVarDecls = []
  , spvInterfaceAccessChains = []
  , spvBodyInstructions = []
  , spvInterfaceIds = []
  , spvGlslExtId = Id 1
  , spvBuiltIns = M.empty
  , spvWorkgroupSizeSpecIds = (undefined, undefined, undefined)
  , spvUserState = s
  }

type Builder s = State (SPIRVState s)

runBuilder :: String -> Builder s () -> s -> (s, [Word32], (SpecId, SpecId, SpecId))
runBuilder shaderName m userState =
  let (instructions, s) = runState (setup >> m >> finalize) (newState userState)
      header = makeHeader (SPIRVVersion 1 5) (spvRunningId s)
      shader = spirvSerialize header ++ spirvSerialize instructions
      (specX, specY, specZ) = spvWorkgroupSizeSpecIds s
  in (spvUserState s, shader, (specX, specY, specZ))
  where
    setup = do
      specXYZ <- replicateM 3 newSpecId
      let vec3u32 = TVec (TScalar (IntType Int32)) (Word32 3)
          num1 = IntValue $ intValue Int32 (1 :: Integer)
          specConsts = map (SpecConst num1) specXYZ
          c = SpecConstComposite vec3u32 specConsts
      i <- getConstId c
      let dec = OpDecorate i $ DecorationBuiltIn BuiltInWorkgroupSize
          [specX, specY, specZ] = specXYZ
      modify $ \s ->
        s { spvDecorationInstructions = dec : spvDecorationInstructions s
          , spvBuiltIns = M.insert BuiltInWorkgroupSize i $ spvBuiltIns s
          , spvWorkgroupSizeSpecIds = (specX, specY, specZ)
          }
    capabilities = map OpCapability
      [ CapabilityShader
      --, CapabilityFloat16 -- Futhark does not have a Float16 FloatType
      , CapabilityFloat64
      , CapabilityInt16
      , CapabilityInt64
      , CapabilityInt64Atomics
      , CapabilityStorageBuffer8BitAccess
      , CapabilityStorageBuffer16BitAccess
      , CapabilityVariablePointers
      ]
    extensions = map OpExtension []
    finalize = do
      entryId <- newId
      entryType <- getTypeId (TFunction TVoid [])
      voidType <- getTypeId TVoid
      entryStartLabelId <- newId
      interfaceIds <- gets $ reverse . spvInterfaceIds
      let memoryModelInstr = OpMemoryModel AddressingModelLogical MemoryModelSimple
          entryInstr = OpEntryPoint ExecutionModelGLCompute entryId (String shaderName) interfaceIds
          execModeInstr = OpExecutionMode entryId
                            (ExecutionModeLocalSize
                                (Word32 1) (Word32 1) (Word32 1))
          fnstart = [OpFunction (FunctionControl []) entryType voidType entryId,
                        OpLabel entryStartLabelId]
          fnend = [OpReturn, OpFunctionEnd]
      glslImportInstr <- OpExtInstImport (String "GLSL.std.450") <$> gets spvGlslExtId
      decorations <- reverse <$> gets spvDecorationInstructions
      typesAndConsts <- genTypeAndConstInstructions
      globalDecs <- reverse <$> gets spvGlobalDeclarations
      funVarDecls <- reverse <$> gets spvFunVarDecls
      ifcAccessChains <- reverse <$> gets spvInterfaceAccessChains
      body <- reverse <$> gets spvBodyInstructions
      return . concat $
        [ capabilities
        , extensions
        , [glslImportInstr, memoryModelInstr, entryInstr, execModeInstr]
        , decorations
        , typesAndConsts
        , globalDecs
        , fnstart
        , funVarDecls
        , ifcAccessChains
        , body
        , fnend
        ]

newId :: Builder s Id
newId = state $
  \s -> (Id $ spvRunningId s, s { spvRunningId = spvRunningId s + 1 })

newSpecId :: Builder s SpecId
newSpecId = state $
  \s -> (SpecId . Id $ spvRunningSpecId s,
         s { spvRunningSpecId = spvRunningSpecId s + 1 })

newBindingId :: Builder s BindingId
newBindingId = state $
  \s -> (BindingId . Id $ spvRunningBindingId s,
         s { spvRunningBindingId = spvRunningBindingId s + 1 })

getUserState :: Builder s s
getUserState = gets spvUserState

getUserStateS :: (s -> a) -> Builder s a
getUserStateS f = f <$> gets spvUserState

modifyUserState :: (s -> s) -> Builder s ()
modifyUserState f = modify $ \s -> s { spvUserState = f $ spvUserState s }

-- PrimValue does not carry a signedness flag, so at the moment we only support
-- unsigned integers.
data SPIRVConstant
  = Const PrimValue
  | ConstComposite SPIRVType [SPIRVConstant]
  | SpecConst PrimValue SpecId
  | SpecConstComposite SPIRVType [SPIRVConstant]
  deriving (Eq, Show, Ord)

-- SPIR-V types needed by the Futhark compiler.
data SPIRVType
  = TVoid
  | -- We use Futhark's PrimType to represent scalars.
    TScalar PrimType
  | TArray SPIRVType (Maybe SPIRVConstant)
  | TVec SPIRVType SPIRVWord
  | -- NB: For simplicity, we only support structs with one member.
    -- Additionally, all struct types are assumed to be used for
    -- shader-interfacing, and are as such decorated with Block.
    TStruct SPIRVType
  | TFunction SPIRVType [SPIRVType]
  | TPointer SPIRVType StorageClass
  deriving (Eq, Show, Ord)

spirvConstToType :: SPIRVConstant -> SPIRVType
spirvConstToType (Const n) = TScalar $ primValueType n
spirvConstToType (ConstComposite t _) = t
spirvConstToType (SpecConst n _) = TScalar $ primValueType n
spirvConstToType (SpecConstComposite t _) = t

numPrimValueToSPIRVNumber :: PrimValue -> SPIRVNumber
numPrimValueToSPIRVNumber (IntValue (Int8Value i8)) = SPIRVInt False $ Word8 $ fromIntegral i8
numPrimValueToSPIRVNumber (IntValue (Int16Value i16)) = SPIRVInt False $ Word16 $ fromIntegral i16
numPrimValueToSPIRVNumber (IntValue (Int32Value i32)) = SPIRVInt False $ Word32 $ fromIntegral i32
numPrimValueToSPIRVNumber (IntValue (Int64Value i64)) = SPIRVInt False $ Word64 $ fromIntegral i64
numPrimValueToSPIRVNumber (FloatValue (Float32Value f32)) = SPIRVFloat32 f32
numPrimValueToSPIRVNumber (FloatValue (Float64Value f64)) = SPIRVFloat64 f64
numPrimValueToSPIRVNumber _ = error "Non-numeric PrimValue"

getTypeInners :: SPIRVType -> ([SPIRVType], [SPIRVConstant])
getTypeInners TVoid = ([], [])
getTypeInners (TScalar _) = ([], [])
getTypeInners (TArray elemType lenConst) = ([elemType], maybeToList lenConst)
getTypeInners (TVec elemType _) = ([elemType], [])
getTypeInners (TStruct memberType) = ([memberType], [])
getTypeInners (TFunction retType paramTypes) = (retType : paramTypes, [])
getTypeInners (TPointer pointeeType _) = ([pointeeType], [])

getConstInners :: SPIRVConstant -> ([SPIRVType], [SPIRVConstant])
getConstInners c@(ConstComposite _ memberConsts) =
  ([spirvConstToType c], memberConsts)
getConstInners c@(SpecConstComposite _ memberConsts) =
  ([spirvConstToType c], memberConsts)
getConstInners c = ([spirvConstToType c], [])

makeType, makeType_ :: SPIRVType -> Builder s Id
makeType t@(TStruct _) = do
  i <- makeType_ t
  let -- We assume all structs are used for shader-interfacing
      dec1 = OpDecorate i $ DecorationBlock
      -- We only allow 1 struct member; it must be at offset 0 in the struct
      dec2 = OpMemberDecorate i (Word32 0) $ DecorationOffset (Word32 0)
  modify (\s -> s { spvDecorationInstructions =
                      dec2 : dec1 : spvDecorationInstructions s })
  return i
makeType t@(TArray (TScalar elemPrimType) _) = do
  i <- makeType_ t
  let dec = OpDecorate i $ DecorationArrayStride $ Word32 $ primByteSize elemPrimType
  modify (\s -> s { spvDecorationInstructions =
                      dec : spvDecorationInstructions s })
  return i
makeType (TArray elemType _) =
  error $ "Non-scalar array element type: " ++ show elemType
makeType t = makeType_ t
makeType_ t = do
  let (ts, cs) = getTypeInners t
  mapM_ getTypeId ts
  mapM_ getConstId cs
  typeId <- newId
  modify (\s -> s { spvTypesAndConsts = (SPIRVType t, typeId) : spvTypesAndConsts s })
  return typeId

getTypeId, getTypeIdA :: SPIRVType -> Builder s Id
getTypeId t =
  gets spvTypesAndConsts
    >>= maybe (makeType t) return . lookup (SPIRVType t)
getTypeIdA t = fromJust . lookup (SPIRVType t) <$> gets spvTypesAndConsts

makeConst, makeConst_ :: SPIRVConstant -> Builder s Id
makeConst c@(SpecConst _ (SpecId (Id specId))) = do
  i <- makeConst_ c
  let dec = OpDecorate i $ DecorationSpecId (Word32 specId)
  modify (\s -> s { spvDecorationInstructions =
                      dec : spvDecorationInstructions s })
  return i
makeConst c = makeConst_ c
makeConst_ c = do
  let (ts, cs) = getConstInners c
  mapM_ getTypeId ts
  mapM_ getConstId cs
  constId <- newId
  modify (\s -> s { spvTypesAndConsts = (SPIRVConstant c, constId) : spvTypesAndConsts s })
  return constId

getConstId, getConstIdA :: SPIRVConstant -> Builder s Id
getConstId c =
  gets spvTypesAndConsts
    >>= maybe (makeConst c) return . lookup (SPIRVConstant c)
getConstIdA t = fromJust . lookup (SPIRVConstant t) <$> gets spvTypesAndConsts

genTypeAndConstInstructions :: Builder s [Instruction]
genTypeAndConstInstructions =
  reverse <$> gets spvTypesAndConsts >>= mapM onTypeOrConst
  where
    onTypeOrConst (SPIRVType t, i) = toInstruction (t, i)
    onTypeOrConst (SPIRVConstant c, i) = toConstant (c, i)
    toInstruction (TVoid, resId) = return $ OpTypeVoid resId
    toInstruction (TScalar primType@(IntType _), resId) =
      return $ OpTypeInt (Word32 $ fromIntegral $ primBitSize primType) (Word32 0) resId
    toInstruction (TScalar primType@(FloatType _), resId) =
      return $ OpTypeFloat (Word32 $ fromIntegral $ primBitSize primType) resId
    toInstruction (TScalar Bool, resId) =
      return $ OpTypeBool resId
    toInstruction (TScalar Cert, _) = error "Cert not supported for TScalar"
    toInstruction (TArray elemType (Just lenConst), resId) =
      OpTypeArray <$> getTypeIdA elemType <*> getConstIdA lenConst <*> return resId
    toInstruction (TArray elemType Nothing, resId) =
      OpTypeRuntimeArray <$> getTypeIdA elemType <*> return resId
    toInstruction (TVec memType numElems, resId) =
      OpTypeVector <$> getTypeIdA memType <*> return numElems <*> return resId
    toInstruction (TStruct memType, resId) =
      OpTypeStruct <$> ((:[]) <$> getTypeIdA memType) <*> return resId
    toInstruction (TFunction retType paramTypes, resId) =
      OpTypeFunction <$> getTypeIdA retType
                     <*> mapM getTypeIdA paramTypes
                     <*> return resId
    toInstruction (TPointer pointeeType storageClass, resId) =
      OpTypePointer storageClass <$> getTypeIdA pointeeType
                                 <*> return resId

    toConstant (Const n@(IntValue _), resId) =
      OpConstant (numPrimValueToSPIRVNumber n)
        <$> getTypeIdA (TScalar $ primValueType n)
        <*> return resId
    toConstant (Const n@(FloatValue _), resId) =
      OpConstant (numPrimValueToSPIRVNumber n)
        <$> getTypeIdA (TScalar $ primValueType n)
        <*> return resId
    toConstant (Const (BoolValue True), resId) =
      OpConstantTrue <$> getTypeIdA (TScalar Bool) <*> return resId
    toConstant (Const (BoolValue False), resId) =
      OpConstantFalse <$> getTypeIdA (TScalar Bool) <*> return resId
    toConstant (Const _, _) = error "Invalid PrimType"
    toConstant (ConstComposite t constants, resId) =
      OpConstantComposite <$> mapM getConstIdA constants
                          <*> getTypeIdA t
                          <*> return resId
    toConstant (SpecConst n@(IntValue _) _, resId) =
      OpSpecConstant (numPrimValueToSPIRVNumber n)
        <$> getTypeIdA (TScalar $ primValueType n)
        <*> return resId
    toConstant (SpecConst n@(FloatValue _) _, resId) =
      OpSpecConstant (numPrimValueToSPIRVNumber n)
        <$> getTypeIdA (TScalar $ primValueType n)
        <*> return resId
    toConstant (SpecConst (BoolValue True) _, resId) =
      OpSpecConstantTrue <$> getTypeIdA (TScalar Bool) <*> return resId
    toConstant (SpecConst (BoolValue False) _, resId) =
      OpSpecConstantFalse <$> getTypeIdA (TScalar Bool) <*> return resId
    toConstant (SpecConst _ _, _) = error "Invalid PrimType"
    toConstant (SpecConstComposite t constants, resId) =
      OpSpecConstantComposite <$> mapM getConstIdA constants
                              <*> getTypeIdA t
                              <*> return resId

instrRT_ :: Bool -> SPIRVType -> Id -> RTInstruction -> Builder s Instruction
instrRT_ isBodyInstr t resId rti = do
  typeId <- getTypeId t
  let i = rti typeId resId
      bodyPrepend b = if isBodyInstr then i:b else b
  modify $ \s ->
    s { spvBodyInstructions = bodyPrepend (spvBodyInstructions s),
        spvIdTypes = M.insert resId t $ spvIdTypes s
      }
  return i

-- Give an RTInstruction an IdResult and an IdResultType (based on t), insert
-- it into the body of the function being generated, and return the IdResult.
-- Importantly, the internal spvIdTypes map is also updated.
instrRT :: SPIRVType -> RTInstruction -> Builder s Id
instrRT t rti = do
  resId <- newId
  void $ instrRT_ True t resId rti
  return resId

-- Same as instrRT but takes the Id as input instead of generating a new one
instrRTId :: SPIRVType -> Id -> RTInstruction -> Builder s ()
instrRTId t resId rti = void $ instrRT_ True t resId rti

-- Same as instrRT but doesn't insert into the body of the function. Instead,
-- the generated instruction is returned.
instrRTInternal :: SPIRVType -> RTInstruction -> Builder s (Id, Instruction)
instrRTInternal t rti = do
  resId <- newId
  ins <- instrRT_ False t resId rti
  return (resId, ins)

-- Give an RInstruction an IdResult, insert it into the bod of the function
-- being generated, and return the IdResult
instrR :: RInstruction -> Builder s Id
instrR ri = do
  resId <- newId
  let i = ri resId
  modify (\s -> s { spvBodyInstructions = i : spvBodyInstructions s })
  return resId

instr :: Instruction -> Builder s ()
instr i = modify (\s -> s { spvBodyInstructions = i : spvBodyInstructions s })

glslInstr :: SPIRVType -> GlslInstruction -> Builder s Id
glslInstr t gi = do
  glslId <- gets spvGlslExtId
  instrRT t $ OpExtInst glslId (GlslInstruction gi)

makeBuiltIn :: BuiltIn -> Builder s Id
makeBuiltIn b
  | b `elem`
      [BuiltInGlobalInvocationId, BuiltInLocalInvocationId,
        BuiltInWorkgroupId, BuiltInNumWorkgroups] = do
      let vec3u32 = TVec (TScalar (IntType Int32)) (Word32 3)
          declType = TPointer vec3u32 StorageClassInput
      (declId, decl) <-
        instrRTInternal declType $ OpVariable StorageClassInput Nothing
      let dec = OpDecorate declId $ DecorationBuiltIn b
      modify $ \s ->
        s { spvDecorationInstructions = dec : spvDecorationInstructions s
          , spvGlobalDeclarations = decl : spvGlobalDeclarations s
          , spvBuiltIns = M.insert b declId $ spvBuiltIns s
          , spvInterfaceIds = declId : spvInterfaceIds s
          }
      return declId
  | otherwise = error "Unsupported BuiltIn"

getBuiltIn :: BuiltIn -> Int -> Builder s Id
getBuiltIn BuiltInWorkgroupSize idx = do
  vec <- gets $ fromMaybe (error "WorkgroupSize not initialized") .
                  M.lookup BuiltInWorkgroupSize . spvBuiltIns
  instrRT (TScalar int32) $ OpCompositeExtract vec [Word32 $ fromIntegral idx]
getBuiltIn b idx = do
  lookupRes <- gets $ M.lookup b . spvBuiltIns
  vec <- maybe (makeBuiltIn b) return lookupRes
  idxConst <- getConstId (Const $ IntValue $ intValue Int32 idx)
  let tChain = TPointer (TScalar int32) StorageClassInput
  access <- instrRT tChain $ OpAccessChain vec [idxConst]
  instrRT (TScalar int32) $ OpLoad access Nothing

makeInterfaceVar :: SPIRVType -> Builder s (Id, BindingId)
makeInterfaceVar innerType = do
  -- Must be wrapped in a struct:
  --  https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources-descset
  let tFull = TPointer (TStruct innerType) StorageClassStorageBuffer
      tChain = TPointer innerType StorageClassStorageBuffer
  (declId, decl) <-
    instrRTInternal tFull $ OpVariable StorageClassStorageBuffer Nothing
  u32_0 <- getConstId (Const (IntValue $ intValue Int32 (0 :: Integer)))
  (accessChainId, accessChain) <-
    instrRTInternal tChain $ OpAccessChain declId [u32_0]

  bindingId@(BindingId (Id b)) <- newBindingId
  let dec1 = OpDecorate declId $ DecorationDescriptorSet $ Word32 0
      dec2 = OpDecorate declId $ DecorationBinding $ Word32 b
  modify $ \s ->
    s { spvDecorationInstructions = dec2 : dec1 : spvDecorationInstructions s
      , spvGlobalDeclarations = decl : spvGlobalDeclarations s
      , spvInterfaceAccessChains = accessChain : spvInterfaceAccessChains s
      , spvInterfaceIds = declId : spvInterfaceIds s
      }
  return (accessChainId, bindingId)

makeFunctionVar :: SPIRVType -> Maybe SPIRVConstant -> Builder s Id
makeFunctionVar t mcInit = do
  let declType = TPointer t StorageClassFunction
  maybeInitId <- maybe (return Nothing) (fmap Just . getConstId) mcInit
  (declId, decl) <-
    instrRTInternal declType $ OpVariable StorageClassFunction maybeInitId
  modify $ \s -> s { spvFunVarDecls = decl : spvFunVarDecls s }
  return declId

makeLocalMemArray :: SPIRVType -> Id -> Builder s Id
makeLocalMemArray elemType cId = do
  specConst <- getConstFromDefiningId cId
  let declType = TPointer (TArray elemType (Just specConst)) StorageClassWorkgroup
  (declId, decl) <- instrRTInternal declType $ OpVariable StorageClassWorkgroup Nothing
  modify $ \s ->
    s { spvGlobalDeclarations = decl : spvGlobalDeclarations s
      , spvInterfaceIds = declId : spvInterfaceIds s
      }
  return declId

makeLocalMemArrayConst :: SPIRVType -> PrimValue -> Builder s Id
makeLocalMemArrayConst elemType numElems = do
  let declType = TPointer (TArray elemType (Just $ Const numElems)) StorageClassWorkgroup
  (declId, decl) <- instrRTInternal declType $ OpVariable StorageClassWorkgroup Nothing
  modify $ \s ->
    s { spvGlobalDeclarations = decl : spvGlobalDeclarations s
      , spvInterfaceIds = declId : spvInterfaceIds s
      }
  return declId

makeArrayAccess :: Id -> Id -> Builder s (Id, StorageClass)
makeArrayAccess arr idx = do
  t <- getType arr
  let (storageClass, (elemType, constIndices)) = checkPtr t
      indConsts = map (\i -> Const (IntValue $ intValue Int32 (i :: Integer))) constIndices
      accessType = TPointer elemType storageClass
  indices <- mapM getConstId indConsts
  accessId <- instrRT accessType $ OpAccessChain arr $ indices ++ [idx]
  return (accessId, storageClass)
  where
    checkPtr (TPointer t storageClass) = (storageClass, getConstIndices t)
    checkPtr _ = error "Unexpected type"
    getConstIndices (TStruct t) =
      -- Recall that we only allow a single member in the struct type
      -- representation. We don't need more atm
      let (et, inds) = getConstIndices t in (et, 0 : inds)
    getConstIndices (TArray et _) = (et, [])
    getConstIndices _ = error "Unexpected type"

-- Given an Id returned from instrRT or instrRTInternal, get the associated type
getType :: Id -> Builder s SPIRVType
getType i = do
  typesAndConsts <- gets spvTypesAndConsts
  valTypes <- gets spvIdTypes
  case (M.lookup i valTypes, findConst typesAndConsts) of
    (Just t, _) -> return t
    (_, Just c) -> return $ spirvConstToType c
    _ -> error $ "Bad Id: " ++ show i
  where
    findConst ((SPIRVConstant c, i'):xs) =
      if i' == i then Just c else findConst xs
    findConst (_:xs) = findConst xs
    findConst [] = Nothing

-- Given an Id defining a SPIRVConstant, get the associated SPIRVConstant
getConstFromDefiningId :: Id -> Builder s SPIRVConstant
getConstFromDefiningId cId = do
  gets $ findConst . spvTypesAndConsts
  where
    findConst ((SPIRVConstant (c@(SpecConst _ _)), cId'):xs) =
      if cId' == cId then c else findConst xs
    findConst (_:xs) = findConst xs
    findConst [] = error $ "Id " ++ show cId ++ " does not define a constant"

assertType :: SPIRVType -> Id -> Builder s ()
assertType t i = do
  t' <- getType i
  unless (t == t') $ error $
    "Type assertion failed: Got " ++ show t' ++ ", expected " ++ show t
