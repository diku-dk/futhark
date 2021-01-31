{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Spec
  ( CoreSpec (..)
  , ExtSpec (..)
  , InstructionClass
  , isExclude
  , InstructionClasses (..)
  , SPIRVVersion (..)
  , Instruction (..)
  , Operand (..)
  , Quantifier (..)
  , OperandTypeName
  , OperandType (..)
  , Enumerant (..)
  , parseSpec
  )
where
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.List (sort, intercalate)
import Data.Word
import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import System.Exit (exitFailure)

-- Tested with SPIR-V 1.5 revision 4
-- Grammars can be found here:
--  https://github.com/KhronosGroup/SPIRV-Headers/blob/1.5.4/include/spirv/unified1/spirv.core.grammar.json
--  https://github.com/KhronosGroup/SPIRV-Headers/blob/1.5.4/include/spirv/unified1/extinst.glsl.std.450.grammar.json
--  https://github.com/KhronosGroup/SPIRV-Headers/blob/1.5.4/include/spirv/unified1/extinst.debuginfo.grammar.json
--  https://github.com/KhronosGroup/SPIRV-Headers/blob/1.5.4/include/spirv/unified1/extinst.nonsemantic.debugprintf.grammar.json
--

readM :: (MonadFail m, Read a) => String -> String -> m a
readM err s =
  case reads s of
    [(x, "")] -> return x
    _ -> fail err

data CoreSpec = CoreSpec
  { copyright :: [String]
  , magicNumber :: Word32
  , magicNumberAsString :: String -- Only useful for ToJSON
  , majorVersion :: Int
  , minorVersion :: Int
  , revision :: Int
  , instructionClasses :: InstructionClasses
  , instructions :: [Instruction]
  , operandTypes :: [OperandType]
  } deriving (Show)

instance FromJSON CoreSpec where
  parseJSON = withObject "CoreSpec" $ \o ->
    CoreSpec <$> o .: "copyright"
              <*> (o .: "magic_number" >>= readM "Bad magic number")
              <*> o .: "magic_number"
              <*> o .: "major_version"
              <*> o .: "minor_version"
              <*> o .: "revision"
              <*> o .: "instruction_printing_class"
              <*> o .: "instructions"
              <*> o .: "operand_kinds"

instance ToJSON CoreSpec where
  toJSON spec = object
    [ "copyright" .= (Array . V.fromList . map toJSON $ copyright spec)
    , "magic_number" .= magicNumberAsString spec
    , "major_version" .= majorVersion spec
    , "minor_version" .= minorVersion spec
    , "revision" .= revision spec
    , "instruction_printing_class" .= instructionClasses spec
    , "instructions" .= instructions spec
    , "operand_kinds" .= operandTypes spec
    ]


data ExtSpec = ExtSpec
  { extCopyright :: [String]
  , extVersion :: Maybe Int
  , extRevision :: Int
  , extInstructions :: [Instruction]
  , extOperandTypes :: [OperandType]
  } deriving  (Show)

instance FromJSON ExtSpec where
  parseJSON = withObject "ExtSpec" $ \o ->
    ExtSpec <$> o .:? "copyright" .!= []
            <*> o .:? "version"
            <*> o .: "revision"
            <*> o .: "instructions"
            <*> o .:? "operand_kinds" .!= []

instance ToJSON ExtSpec where
  toJSON extspec = object
    [ "copyright" .= extCopyright extspec
    , "version" .= extVersion extspec
    , "revision" .= extRevision extspec
    , "instructions" .= extInstructions extspec
    , "operand_kinds" .= extOperandTypes extspec
    ]


type InstructionClass = String

isExclude :: InstructionClass -> Bool
isExclude "@exclude" = True
isExclude "Reserved" = True
isExclude _ = False

newtype InstructionClasses = InstructionClasses
  { unClasses :: M.Map InstructionClass (Maybe String) }
  deriving (Show)

instance FromJSON InstructionClasses where
  parseJSON = withArray "InstructionClasses" $
    fmap (InstructionClasses . M.fromList . V.toList) . mapM parseInstrClass
    where
      parseInstrClass = withObject "InstructionClass" $ \o ->
        (,) <$> o .: "tag" <*> o .:? "heading"

instance ToJSON InstructionClasses where
  toJSON (InstructionClasses m) =
    Array . V.fromList . map instrClassToJSON . M.toList $ m
    where
      instrClassToJSON (name,  Nothing) = object ["tag" .= name]
      instrClassToJSON (name, Just desc) =
        object ["tag" .= name, "heading" .= desc]


data SPIRVVersion
  = SPIRVVersion Int Int
  | -- Reserved Enumerants and Instructions
    SPIRVNoVersion
  deriving (Show)

instance FromJSON SPIRVVersion where
  parseJSON = withText "SPIRVVersion" $ \t ->
    case T.findIndex (== '.') t of
      Just i ->
        let (ma, dotmi) = T.splitAt i t
            mi = T.tail dotmi
        in SPIRVVersion <$> readM "Bad major version" (T.unpack ma)
                        <*> readM "Bad minor version" (T.unpack mi)
      Nothing | t == "None" -> return SPIRVNoVersion
      Nothing -> fail "Bad version string"

instance ToJSON SPIRVVersion where
  toJSON (SPIRVVersion major minor) = String . T.pack $ sma ++ "." ++ smi
    where
      sma = show major
      smi = show minor
  toJSON SPIRVNoVersion = String . T.pack $ "None"

instance Eq SPIRVVersion where
  (SPIRVVersion ma1 mi1) == (SPIRVVersion ma2 mi2) = ma1 == ma2 && mi1 == mi2
  SPIRVNoVersion == SPIRVNoVersion = True
  _ == _ = False

data Instruction = Instruction
  { instrName :: String
  , instrCode :: Word32
  , instrClass :: Maybe InstructionClass -- Extension instructions have no class
  , instrOperands :: [Operand]
  , instrCapabilities :: [String]
  , instrExtensions :: [String]
  , instrFirstVersion :: Maybe SPIRVVersion
  , instrLastVersion :: Maybe SPIRVVersion
  } deriving (Show)

instance FromJSON Instruction where
  parseJSON = withObject "Instruction" $ \o ->
    Instruction <$> o .: "opname"
                <*> o .: "opcode"
                <*> o .:? "class"
                <*> o .:? "operands" .!= []
                <*> o .:? "capabilities" .!= []
                <*> o .:? "extensions" .!= []
                <*> o .:? "version"
                <*> o .:? "lastVersion"

instance ToJSON Instruction where
  toJSON instr = object
    [ "opname" .= instrName instr
    , "class" .= instrClass instr
    , "opcode" .= instrCode instr
    , "operands" .= instrOperands instr
    , "capabilities" .= instrCapabilities instr
    , "extensions" .= instrExtensions instr
    , "version" .= instrFirstVersion instr
    , "lastVersion" .= instrLastVersion instr
    ]

data Operand = Operand
  { operandTypeName :: OperandTypeName
  , operandDesc :: Maybe String
  , operandQuantifier :: Maybe Quantifier
  } deriving (Show)

instance FromJSON Operand where
  parseJSON = withObject "Param" $ \o ->
    Operand <$> o .: "kind"
            <*> o .:? "name"
            <*> o .:? "quantifier"

instance ToJSON Operand where
  toJSON p = object
    [ "kind" .= operandTypeName p
    , "name" .= operandDesc p
    , "quantifier" .= operandQuantifier p
    ]


data Quantifier = QuantOptional | QuantMany
  deriving (Show, Eq)

instance FromJSON Quantifier where
  parseJSON = withText "Quantifier" $ \t ->
    case t of
      "?" -> return QuantOptional
      "*" -> return QuantMany
      _ -> fail "Invalid instruction quantifier"

instance ToJSON Quantifier where
  toJSON QuantOptional = String "?"
  toJSON QuantMany = String "*"


type OperandTypeName = String

data OperandType
  = BitEnum OperandTypeName [Enumerant]
  | ValueEnum OperandTypeName [Enumerant]
  | Id OperandTypeName String
  | Literal OperandTypeName String
  | Composite OperandTypeName [OperandTypeName]
  deriving (Show)

instance FromJSON OperandType where
  parseJSON  = withObject "OperandType" $ \o -> do
    cat <- o .: "category"
    case cat of
      String "BitEnum" -> BitEnum <$> o .: "kind" <*> o .: "enumerants"
      String "ValueEnum" -> ValueEnum <$> o .: "kind" <*> o .: "enumerants"
      String "Id" -> Id <$> o .: "kind" <*> o .: "doc"
      String "Literal" -> Literal <$> o .: "kind" <*> o .: "doc"
      String "Composite" -> Composite <$> o .: "kind" <*> o .: "bases"

instance ToJSON OperandType where
  toJSON (BitEnum k es) = object
    ["category" .= String "BitEnum", "kind" .= k, "enumerants" .= map toJSON es]
  toJSON (ValueEnum k es) = object
    ["category" .= String "ValueEnum", "kind" .= k, "enumerants" .= map toJSON es]
  toJSON (Id k doc) = object
    ["category" .= String "Id", "kind" .= k, "doc" .= doc]
  toJSON (Literal k doc) = object
    ["category" .= String "Literal", "kind" .= k, "doc" .= doc]
  toJSON (Composite k bases) = object
    ["category" .= String "Composite", "kind" .= k, "bases" .= map toJSON bases]

data Enumerant = Enumerant
  { enumerantName :: String
  , enumerantValue :: Word32
  , enumerantValueAsString :: Maybe String -- Only useful for ToJSON
  , enumerantCapabilities :: [String]
  , enumerantExtensions :: [String]
  , enumerantFirstVersion :: Maybe SPIRVVersion
  , enumerantLastVersion :: Maybe SPIRVVersion
  , enumerantParameters :: [Operand]
  } deriving (Show)

instance FromJSON Enumerant where
  parseJSON = withObject "Enumerant" $ \o -> do
    (wasString, val) <- (Nothing,) <$> o .: "value"
                        <|> (o .: "value" >>= \v -> (Just v,) <$> readM "Invalid value" v)
    Enumerant <$> o .: "enumerant"
              <*> return val
              <*> return wasString
              <*> o .:? "capabilities" .!= []
              <*> o .:? "extensions" .!= []
              <*> o .:? "version"
              <*> o .:? "lastVersion"
              <*> o .:? "parameters" .!= []

instance ToJSON Enumerant where
  toJSON e = object
    [ "enumerant" .= enumerantName e
    , "value" .=
      case enumerantValueAsString e of
        Just s -> toJSON s
        Nothing -> toJSON (enumerantValue e)
    , "capabilities" .= enumerantCapabilities e
    , "extensions" .= enumerantExtensions e
    , "version" .= enumerantFirstVersion e
    , "lastVersion" .= enumerantLastVersion e
    , "parameters" .= enumerantParameters e
    ]


notNull :: Value -> Bool
notNull Null = False
notNull _ = True

removeNulls :: Value -> Value
removeNulls (Object hm) = Object . HM.map removeNulls . HM.filter notNull $ hm
removeNulls (Array vec) = Array . V.map removeNulls . V.filter notNull $ vec
removeNulls v = v

isEmptyArray (Array vec) = V.length vec == 0
isEmptyArray _ = True

removeEmptyArrays :: Value -> Value
removeEmptyArrays (Object hm) =
  Object . HM.map removeEmptyArrays . HM.filter (not . isEmptyArray) $ hm
removeEmptyArrays (Array vec) =
  Array . V.map removeEmptyArrays . V.filter (not . isEmptyArray) $ vec
removeEmptyArrays v = v

sortArrays :: Value -> Value
sortArrays (Object hm) = Object . HM.map sortArrays $ hm
sortArrays (Array vec) = Array . V.fromList . sort . map sortArrays . V.toList $ vec
sortArrays v = v

-- This could be a lot shorter, but we add a sanity check to make sure our
-- parsed json representations aren't missing anythting
parseSpec :: (FromJSON a, ToJSON a) => FilePath -> IO a
parseSpec path = do
  decodeRes <- decode . B.fromString <$> readFile path
  jsonValue <- maybe (fail "Invalid JSON") return decodeRes
  let parseRes = resToEither $ fromJSON jsonValue
  a <- either fail return parseRes
  let val = sortArrays . removeEmptyArrays . removeNulls . toJSON $ a
      origVal = sortArrays . removeEmptyArrays . removeNulls $ jsonValue
  when (val /= origVal) (putStrLn "Mismatch!" >> exitFailure)
  return a
  where
    resToEither (Success x) = Right x
    resToEither (Error x) = Left x
