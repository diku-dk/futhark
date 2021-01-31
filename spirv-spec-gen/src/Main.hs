import Spec
import Control.Arrow ((***))
import Data.Char
import Data.List
import Data.Maybe

import Debug.Trace

getReservedCapabilities :: CoreSpec -> [String]
getReservedCapabilities coreSpec =
  case find onEnum (operandTypes coreSpec) of
    Just (ValueEnum _ enus) ->
      map enumerantName . filter ((== Just SPIRVNoVersion) . enumerantFirstVersion) $ enus
    _ -> error "Missing Capability ValueEnum"
  where
    onEnum (ValueEnum "Capability" _) = True
    onEnum _ = False

fixEnumerant :: [String] -> Enumerant -> Maybe Enumerant
fixEnumerant reservedCaps enu =
  if enumerantFirstVersion enu == Just SPIRVNoVersion
      || enumerantLastVersion enu == Just SPIRVNoVersion
      || (enumerantCapabilities enu /= []
          && (enumerantCapabilities enu \\ reservedCaps) == [])
  then trace ("Removing " ++ show enu ++ "; all enabling capabilities are reserved") Nothing
  else Just enu { enumerantCapabilities =
                      enumerantCapabilities enu \\ reservedCaps }

fixOperandType :: [String] -> OperandType -> Maybe OperandType
fixOperandType reservedCaps t@(BitEnum name enus) =
  case mapMaybe (fixEnumerant reservedCaps) enus of
    [] -> trace ("Removing " ++ show t ++ "; no enumerants") Nothing
    es -> Just $ BitEnum name es
fixOperandType reservedCaps t@(ValueEnum name enus) =
  case mapMaybe (fixEnumerant reservedCaps) enus of
    [] -> trace ("Removing " ++ show t ++ "; no enumerants") Nothing
    es -> Just $ ValueEnum name es
fixOperandType _ t = Just t

fixInstruction :: [String] -> Instruction -> Maybe Instruction
fixInstruction reservedCaps instr =
  if maybe False isExclude (instrClass instr)
      || instrFirstVersion instr == Just SPIRVNoVersion
      || instrLastVersion instr == Just SPIRVNoVersion
      || (instrCapabilities instr /= []
          && (instrCapabilities instr \\ reservedCaps) == [])
  then trace ("Removing " ++ show instr ++ "; all enabling capabilities are reserved") Nothing
  else Just instr { instrCapabilities =
                        instrCapabilities instr \\ reservedCaps }

fixCoreSpec :: CoreSpec -> CoreSpec
fixCoreSpec coreSpec =
  coreSpec
    { instructions = mapMaybe (fixInstruction rc) $ instructions coreSpec
    , operandTypes = mapMaybe (fixOperandType rc) $ operandTypes coreSpec
    }
  where
    rc = getReservedCapabilities coreSpec

fixExtSpec :: ExtSpec -> ExtSpec
fixExtSpec extSpec =
  extSpec
    { extInstructions = mapMaybe (fixInstruction []) $ extInstructions extSpec
    , extOperandTypes = mapMaybe (fixOperandType []) $ extOperandTypes extSpec
    }

main :: IO ()
main = do
  coreSpec <- fixCoreSpec <$> parseSpec "../res/spirv.core.grammar.json"
  glslSpec <- fixExtSpec <$> parseSpec "../res/extinst.glsl.std.450.grammar.json"
  dbgInfoSpec <- fixExtSpec <$> parseSpec "../res/extinst.debuginfo.grammar.json"
  dbgPrintSpec <- fixExtSpec <$> parseSpec "../res/extinst.nonsemantic.debugprintf.grammar.json"
  readFile "../res/header.hs.txt" >>= putStrLn
  putStrLn "magic :: Word32"
  putStrLn $ "magic = " ++ show (magicNumber coreSpec)
  mapM_ (putStrLn . onOperandType)
        (operandTypes coreSpec ++ concatMap extOperandTypes [glslSpec, dbgInfoSpec, dbgPrintSpec])
  putStrLn . onCoreInstructionSet $ instructions coreSpec
  putStrLn . onExtendedInstructionSets $
      [("Glsl", extInstructions glslSpec),
        ("DbgInfo", extInstructions dbgInfoSpec),
        ("DbgPrint", extInstructions dbgPrintSpec)]
  return ()

dataDecl :: String -> String -> String
dataDecl name ctors =
  intercalate "\n"
    [ "data " ++ name ++ " = " ++ ctors
    , "  deriving (Show, Eq, Ord)" ]

newtypeDecl :: String -> String -> String
newtypeDecl name innertype =
  intercalate "\n"
    [ "newtype " ++ name ++ " = " ++ name ++ " " ++ innertype
    , "  deriving (Show, Eq, Ord)" ]

data SPIRVInfoCode = SPIRVInfoCode
  { capabilitiesCode :: String
  , extensionsCode :: String
  , versionRangeCode :: String
  }
  deriving (Show)

makeSpirvInfo :: String -> [(String, SPIRVInfoCode)] -> String
makeSpirvInfo t infos = intercalate "\n" $
  [ "instance SPIRVInfo " ++ t ++ " where" ]
  ++ makeFnLines "infoCapabilities" capabilitiesCode
  ++ makeFnLines "infoExtensions" extensionsCode
  ++ makeFnLines "infoVersionRange" versionRangeCode
  where
    fnLine s pat body = "  " ++ s ++ " (" ++ pat ++ ") = " ++ body
    makeFnLines s proj = map (\(pat, i) -> fnLine s pat (proj i)) infos

makeDummySpirvInfo :: String -> String
makeDummySpirvInfo t = "instance SPIRVInfo " ++ t ++ " where"

makeSpirvSerialize :: String -> [(String, String)] -> String
makeSpirvSerialize t xs = intercalate "\n" $
  [ "instance SPIRVSerialize " ++ t ++ " where" ]
  ++ map (\(pat, body) -> "  spirvSerialize (" ++ pat ++ ") = " ++ body) xs

onOperandType :: OperandType -> String
onOperandType (BitEnum name enus) =
  let enusName = name ++ "Bit"
      (ctorCode, patCode, infoCode, serCode) = unzip4 $ map (onEnumerant name) enus
  in intercalate "\n" $
      [ dataDecl enusName $ intercalate " | " ctorCode
      , newtypeDecl name $ "[" ++ enusName ++ "]"
      , makeSpirvInfo enusName $ zip patCode infoCode
      , makeSpirvInfo name
          [(name ++ " l",
            SPIRVInfoCode
              "infoCapabilities l"
              "infoExtensions l"
              "infoVersionRange l"
            )]
      , "instance SPIRVSerialize " ++ name ++ " where"
      , "  spirvSerialize (" ++ name ++ " bits) ="
      , "    let (bs, os) = unzip $ sortBy (compare `on` fst) $ map serializeBit bits"
      , "    in foldl (.|.) 0 bs : concat os"
      , "    where"
      , "      serializeBit :: " ++  enusName ++ " -> (Word32, [Word32])"
      ] ++ (zipWith (\pc sc -> "      serializeBit (" ++ pc ++ ") = " ++ sc) patCode serCode)
onOperandType (ValueEnum name enus) =
  let (ctorCode, patCode, infoCode, serCode) = unzip4 $ map (onEnumerant name) enus
  in intercalate "\n" $
      [ dataDecl name $ intercalate " | " ctorCode
      , makeSpirvInfo name $ zip patCode infoCode
      , "instance SPIRVSerialize " ++ name ++ " where"
      , "  spirvSerialize v = uncurry (:) $ ser v"
      , "    where"
      ] ++ (zipWith (\pc sc -> "      ser (" ++ pc ++ ") = " ++ sc) patCode serCode)
onOperandType (Id name _) = "type " ++ name ++ " = Id"
onOperandType (Literal "LiteralInteger" _) = "type LiteralInteger = SPIRVWord"
onOperandType (Literal "LiteralString" _) = "type LiteralString = SPIRVString"
onOperandType (Literal "LiteralContextDependentNumber" _) = "type LiteralContextDependentNumber = SPIRVNumber"
onOperandType (Literal "LiteralExtInstInteger" _) = "type LiteralExtInstInteger = SPIRVWord"
onOperandType (Literal "LiteralSpecConstantOpInteger" _) =
  -- XXX: Serialization of this type (used only by OpSpecConstantOp as of 1.5
  -- rev4) is unsupported atm, since the inner instruction needs to be
  -- serialized in a way that removes its IdResult and IdResultType
  intercalate "\n" $
    [ newtypeDecl "LiteralSpecConstantOpInteger" "Instruction"
    , "instance SPIRVSerialize LiteralSpecConstantOpInteger where"
    , "  spirvSerialize _ = undefined"
    , makeDummySpirvInfo "LiteralSpecConstantOpInteger"
    ]
onOperandType (Literal _ _) = error "Unrecognized Literal type"
onOperandType (Composite name ks) =
  intercalate "\n"
    [ newtypeDecl name $ "(" ++ intercalate ", " ks ++ ")"
    , makeSpirvInfo name
        [(pat,
          SPIRVInfoCode
            (intercalate " `union` " $ map (\p -> "infoCapabilities " ++ p) params)
            (intercalate " `union` " $ map (\p -> "infoExtensions " ++ p) params)
            (intercalate " <> " $ map (\p -> "infoVersionRange " ++ p) params)
          )]
    , -- makeSpirvSerialize name [("_", "error \"Not implemented\"")] -- XXX
      "instance SPIRVSerialize " ++ name ++ " where"
    , "  spirvSerialize (" ++ pat ++ ") = concat [" ++
      intercalate ", " (map ("spirvSerialize " ++) params) ++ "]"
    ]
  where
    params = zipWith (\n _ -> 'x' : show n) [0..] ks
    pat = name ++ " (" ++ intercalate ", " params ++ ")"

data OperandsInfo
  = OperandsNormal
  | OperandsWithMaybes Int Int
  | OperandsWithMany

getOperandsInfo :: [Operand] -> OperandsInfo
getOperandsInfo os =
  case findIndex isJust quants of
    Just i ->
      case snd $ splitAt i quants of
        [Just QuantMany] -> OperandsWithMany
        (Just QuantOptional):xs ->
          if all (==(Just QuantOptional)) xs
          then OperandsWithMaybes i $ length os - i
          else error "Mixing QuantOptional and QuantOptional"
        _ -> error "Non-last QuantMany"
    Nothing -> OperandsNormal
  where
    quants = map operandQuantifier os

onEnumerant :: String -> Enumerant -> (String, String, SPIRVInfoCode, String)
onEnumerant typeName e =
  (intercalate " " $ name : paramTypes,
   intercalate " " $ name : paramVars,
   SPIRVInfoCode
    { capabilitiesCode = "[" ++ intercalate ", " capabilities ++ "]"
    , extensionsCode = "[" ++ intercalate ", " extensions ++ "]"
    , versionRangeCode = "SPIRVVersionRange " ++ versionRange
    },
   -- This assumes that each Enumerant in each OperandType in params *cannot*
   -- itself take parameters, which is the case for the current unified spec.
   -- We could add a check to make sure this assumption isn't violated if the
   -- spec is updated in the future
   case getOperandsInfo params of
    OperandsNormal ->
      "(" ++ show (enumerantValue e)
      ++ ", concat [" ++ intercalate ", " (map (\p -> "spirvSerialize " ++ p) paramVars)
      ++ "])"
    OperandsWithMaybes _ _ -> error "Encountered Enumerant with QuantOptional"
    OperandsWithMany -> error "Encountered Enumerant with QuantMany"
   )
  where
    params = enumerantParameters e
    paramVars = zipWith (\n _ -> 'x' : show n) [0..] params
    paramTypes = map onOperand params
    name = typeName ++ enumerantName e
    capabilities = map ("Capability" ++) $ enumerantCapabilities e
    extensions = map (\s -> "\"" ++ s ++ "\"") $ enumerantExtensions e
    versionRange =
      "(" ++ versionToString (enumerantFirstVersion e) ++ ", "
        ++ versionToString (enumerantLastVersion e) ++ ")"
    versionToString (Just SPIRVNoVersion) =
      -- Reserved enumerants should have been removed by here
      error "Unexpected SPIRVNoVersion"
    versionToString (Just (SPIRVVersion ma mi)) =
      "Just (SPIRVVersion " ++ show ma ++ " " ++ show mi ++ ")"
    versionToString Nothing = "Nothing"

onOperand :: Operand -> String
onOperand p = wrap . operandTypeName $ p
  where
    wrap s =
      case operandQuantifier p of
        Just QuantOptional -> "(Maybe " ++ s ++ ")"
        Just QuantMany -> "[" ++ s ++ "]"
        Nothing -> s



data InsnCode = InsnCode
  { icCtor :: String
  , icPat :: String
  , icInfo :: SPIRVInfoCode
  , icSer :: String
  }

operandsPermuteFun :: [Operand] -> ([a] -> [a])
operandsPermuteFun [] = id
operandsPermuteFun os =
  case (resInds, resTypeInds) of
    ([], []) -> id
    ([a], []) -> \l -> let (l1, l2) = popAt l [a] in l2 ++ l1
    ([a], [b]) -> \l -> let (l1, l2) = popAt l [a, b] in l2 ++ l1
    ([], [_]) -> error "Result type without result Id"
    _ -> error "More than one result or result type"
  where
    resInds = findIndices ((=="IdResult") . operandTypeName) os
    resTypeInds = findIndices ((=="IdResultType") . operandTypeName) os
    popAt :: [a] -> [Int] -> ([a], [a])
    popAt xs is =
      (fst . unzip *** fst . unzip) $
        partition ((`elem` is) . snd) (zip xs [0..])

onCoreInstruction i =
  if instrName i == "OpExtInst"
  then
    InsnCode
      { icCtor = "OpExtInst IdRef ExtInstruction IdResultType IdResult"
      , icPat = "OpExtInst x2 x3 x0 x1"
      , icInfo = SPIRVInfoCode
          { capabilitiesCode = "[]"
          , extensionsCode = "[]"
          , versionRangeCode = "SPIRVVersionRange (Nothing, Nothing)"
          }
      , icSer =
          "let opsSer = concat ["
          ++ intercalate ", "
                  (map ("spirvSerialize " ++) ["x0", "x1", "x2", "x3"])
          ++ "] in makeInstructionHeader " ++ show (instrCode i)
          ++ " (1 + length opsSer) : opsSer"
      }
  else onInstruction_ True "" i
onExtInstruction = onInstruction_ False

onInstruction_ :: Bool -> String -> Instruction -> InsnCode
onInstruction_ isCore pfx i = InsnCode
  { icCtor = intercalate " " $ name : (permute $ map onOperand operands)
  , icPat = intercalate " " $ name : (permute operandVars)
  , icInfo =
      SPIRVInfoCode
        { capabilitiesCode = "[" ++ intercalate ", " capabilities ++ "]"
        , extensionsCode = "[" ++ intercalate ", " extensions ++ "]"
        , versionRangeCode = "SPIRVVersionRange " ++ versionRange
        }
  , icSer =
      handleMaybes $
        "let opsSer = concat ["
        ++ intercalate ", " (map ("spirvSerialize "++) operandVars)
        ++ "] in " ++
        if isCore
        then
          "makeInstructionHeader " ++ show (instrCode i)
          ++ " (1 + length opsSer) : opsSer"
        else show (instrCode i) ++ " : opsSer"
  }
  where
    name = pfx ++ instrName i
    operands = instrOperands i
    operandVars = zipWith (\n _ -> 'x' : show n) [0..] operands
    permute = operandsPermuteFun operands
    capabilities = map ("Capability" ++) $ instrCapabilities i
    extensions = map (\s -> "\"" ++ s ++ "\"") $ instrExtensions i
    versionRange =
      "(" ++ versionToString (instrFirstVersion i) ++ ", "
        ++ versionToString (instrLastVersion i) ++ ")"
    versionToString (Just SPIRVNoVersion) =
      -- Reserved instructions should have been removed by here
      error "Unexpected SPIRVNoVersion"
    versionToString (Just (SPIRVVersion ma mi)) =
      "Just (SPIRVVersion " ++ show ma ++ " " ++ show mi ++ ")"
    versionToString Nothing = "Nothing"
    handleMaybes serCode =
      case getOperandsInfo operands of
        OperandsWithMaybes numNormals numMaybes | numMaybes > 1 ->
          "if trueThenFalse ["
            ++ (intercalate ", " . map ("isJust " ++) . drop numNormals $ operandVars)
            ++ "] then " ++ serCode
            ++ " else error \"Nothing-optional followed by Just-optional\""
        _ -> serCode

onCoreInstructionSet :: [Instruction] -> String
onCoreInstructionSet is =
  let ic = map onCoreInstruction is
      ctorCode = map icCtor ic
      patCode = map icPat ic
      infoCode = map icInfo ic
      serCode = map icSer ic
  in intercalate "\n" $
      [ dataDecl "Instruction" $ intercalate " | " ctorCode
      , makeSpirvInfo "Instruction" $ zip patCode infoCode
      , makeSpirvSerialize "Instruction" (zip patCode serCode)
      ]

onExtendedInstructionSet :: (String, [Instruction]) -> (String, String)
onExtendedInstructionSet (pfx, is) =
  let ic = map (onExtInstruction pfx) is
      ctorCode = map icCtor ic
      patCode = map icPat ic
      infoCode = map icInfo ic
      serCode = map icSer ic
      name = pfx ++ "Instruction"
  in (intercalate "\n" $
       [ dataDecl name $ intercalate " | " ctorCode
       , makeSpirvInfo name $ zip patCode infoCode
       , "instance SPIRVSerialize " ++ name ++ " where"
       ] ++ zipWith (\pc sc -> "  spirvSerialize (" ++ pc ++ ") = " ++ sc) patCode serCode
     , name)


onExtendedInstructionSets :: [(String, [Instruction])] -> String
onExtendedInstructionSets [] =
   intercalate "\n"
     [ dataDecl "ExtInstruction" "ExtInstructionDummy"
     , makeDummySpirvInfo "ExtInstruction"
     , makeSpirvSerialize "ExtInstruction"
         [("_", "error \"No extensionded instruction sets\"")]
     ]
onExtendedInstructionSets sets =
  intercalate "\n" $ setsCode ++
    [ dataDecl "ExtInstruction" $ intercalate " | " $
        map (\s -> s ++ " " ++ s) names
    , makeSpirvSerialize "ExtInstruction" $
        zip pats (repeat "spirvSerialize x")
    , makeSpirvInfo "ExtInstruction" $ zip pats $ repeat $
          SPIRVInfoCode
            "infoCapabilities x"
            "infoExtensions x"
            "infoVersionRange x"
    ]
    where
      (setsCode, names) = unzip $ map onExtendedInstructionSet sets
      pats = map (++ " x") names
