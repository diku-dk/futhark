{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Futhark.SyntaxTests (tests) where

import Control.Applicative hiding (many, some)
import Data.Bifunctor
import Data.Char (isAlpha)
import Data.Functor
import Data.Map qualified as M
import Data.String
import Data.Text qualified as T
import Data.Void
import Language.Futhark
import Language.Futhark.Parser
import Language.Futhark.Primitive.Parse (constituent, keyword, lexeme)
import Language.Futhark.PrimitiveTests ()
import Test.QuickCheck
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude

tests :: TestTree
tests = testGroup "Source SyntaxTests" []

instance Arbitrary BinOp where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Uniqueness where
  arbitrary = elements [Unique, Nonunique]

instance Arbitrary PrimType where
  arbitrary =
    oneof
      [ Signed <$> arbitrary,
        Unsigned <$> arbitrary,
        FloatType <$> arbitrary,
        pure Bool
      ]

instance Arbitrary PrimValue where
  arbitrary =
    oneof
      [ SignedValue <$> arbitrary,
        UnsignedValue <$> arbitrary,
        FloatValue <$> arbitrary,
        BoolValue <$> arbitrary
      ]

-- The following dirty instances make it slightly nicer to write unit tests.

instance IsString VName where
  fromString s =
    let (tag, s') = bimap reverse (reverse . tail) $ span (/= '_') $ reverse s
     in VName (fromString s') (read tag)

instance (IsString v) => IsString (QualName v) where
  fromString = QualName [] . fromString

instance IsString UncheckedTypeExp where
  fromString =
    either (error . T.unpack . syntaxErrorMsg) id
      . parseType "IsString UncheckedTypeExp"
      . fromString

type Parser = Parsec Void T.Text

braces, brackets, parens :: Parser a -> Parser a
braces = between (lexeme "{") (lexeme "}")
brackets = between (lexeme "[") (lexeme "]")
parens = between (lexeme "(") (lexeme ")")

pName :: Parser Name
pName =
  lexeme . fmap nameFromString $
    (:) <$> satisfy isAlpha <*> many (satisfy constituent)

pVName :: Parser VName
pVName = lexeme $ do
  (s, tag) <-
    satisfy constituent
      `manyTill_` try pTag
      <?> "variable name"
  pure $ VName (nameFromString s) tag
  where
    pTag =
      "_" *> L.decimal <* notFollowedBy (satisfy constituent)

pQualName :: Parser (QualName VName)
pQualName = QualName [] <$> pVName

pPrimType :: Parser PrimType
pPrimType =
  choice $
    map
      f
      [ Bool,
        Signed Int8,
        Signed Int16,
        Signed Int32,
        Signed Int64,
        Unsigned Int8,
        Unsigned Int16,
        Unsigned Int32,
        Unsigned Int64,
        FloatType Float32,
        FloatType Float64
      ]
  where
    f t = keyword (prettyText t) $> t

pUniqueness :: Parser Uniqueness
pUniqueness = choice [lexeme "*" $> Unique, pure Nonunique]

pDim :: Parser d -> Parser d
pDim = brackets

pSize :: Parser Size
pSize =
  choice
    [ flip sizeFromInteger mempty <$> lexeme L.decimal,
      flip sizeFromName mempty <$> pQualName
    ]

pScalarNonFun :: Parser d -> Parser (ScalarTypeBase d Uniqueness)
pScalarNonFun pd =
  choice
    [ Prim <$> pPrimType,
      pTypeVar,
      tupleRecord <$> parens (pType pd `sepBy` lexeme ","),
      Record . M.fromList <$> braces (pField `sepBy1` lexeme ",")
    ]
  where
    pField = (,) <$> pName <* lexeme ":" <*> pType pd
    pTypeVar = TypeVar <$> pUniqueness <*> pQualName <*> many pTypeArg
    pTypeArg =
      choice
        [ TypeArgDim <$> pDim pd,
          TypeArgType . second (const NoUniqueness) <$> pTypeArgType
        ]
    pTypeArgType =
      choice
        [ Scalar . Prim <$> pPrimType,
          parens $ pType pd
        ]

pArrayType :: Parser d -> Parser (TypeBase d Uniqueness)
pArrayType pd =
  Array
    <$> pUniqueness
    <*> (Shape <$> some (pDim pd))
    <*> (second (const NoUniqueness) <$> pScalarNonFun pd)

pNonFunType :: Parser d -> Parser (TypeBase d Uniqueness)
pNonFunType pd =
  choice
    [ try $ pArrayType pd,
      try $ parens $ pType pd,
      Scalar <$> pScalarNonFun pd
    ]

uniquenessToDiet :: Uniqueness -> Diet
uniquenessToDiet Unique = Consume
uniquenessToDiet Nonunique = Observe

pScalarType :: Parser d -> Parser (ScalarTypeBase d Uniqueness)
pScalarType pd = choice [try pFun, pScalarNonFun pd]
  where
    pFun =
      pParam <* lexeme "->" <*> pRetType pd
    pParam =
      choice
        [ try pNamedParam,
          do
            t <- pNonFunType pd
            pure $ Arrow Nonunique Unnamed (diet $ second uniquenessToDiet t) (toStruct t)
        ]
    pNamedParam = parens $ do
      v <- pVName <* lexeme ":"
      t <- pType pd
      pure $ Arrow Nonunique (Named v) (diet $ second uniquenessToDiet t) (toStruct t)

pRetType :: Parser d -> Parser (RetTypeBase d Uniqueness)
pRetType pd =
  choice
    [ lexeme "?" *> (RetType <$> some (brackets pVName) <* lexeme "." <*> pType pd),
      RetType [] <$> pType pd
    ]

pType :: Parser d -> Parser (TypeBase d Uniqueness)
pType pd =
  choice [try $ Scalar <$> pScalarType pd, pArrayType pd, parens (pType pd)]

fromStringParse :: Parser a -> String -> String -> a
fromStringParse p what s =
  either onError id $ parse (p <* eof) "" (T.pack s)
  where
    onError e =
      error $ "not a " <> what <> ": " <> s <> "\n" <> errorBundlePretty e

instance IsString (ScalarTypeBase Size NoUniqueness) where
  fromString =
    fromStringParse
      (second (const NoUniqueness) <$> pScalarType pSize)
      "ScalarType"

instance IsString (ScalarTypeBase () NoUniqueness) where
  fromString =
    fromStringParse
      (second (const NoUniqueness) <$> pScalarType (pure ()))
      "ScalarType"

instance IsString (TypeBase () NoUniqueness) where
  fromString =
    fromStringParse (second (const NoUniqueness) <$> pType (pure ())) "Type"

instance IsString StructType where
  fromString =
    fromStringParse (second (const NoUniqueness) <$> pType pSize) "StructType"

instance IsString StructRetType where
  fromString =
    fromStringParse (second (pure NoUniqueness) <$> pRetType pSize) "StructRetType"

instance IsString ResRetType where
  fromString = fromStringParse (pRetType pSize) "ResRetType"
