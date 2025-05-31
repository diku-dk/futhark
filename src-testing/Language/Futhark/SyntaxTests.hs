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
import Language.Futhark.Parser (SyntaxError (syntaxErrorMsg), parseExp, parseType)
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
    let (s', '_' : tag) = span (/= '_') s
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

pSize :: Parser Size
pSize =
  brackets $
    choice
      [ flip sizeFromInteger mempty <$> lexeme L.decimal,
        flip sizeFromName mempty <$> pQualName
      ]

pScalarNonFun :: Parser (ScalarTypeBase Size Uniqueness)
pScalarNonFun =
  choice
    [ Prim <$> pPrimType,
      pTypeVar,
      tupleRecord <$> parens (pType `sepBy` lexeme ","),
      Record . M.fromList <$> braces (pField `sepBy1` lexeme ",")
    ]
  where
    pField = (,) <$> pName <* lexeme ":" <*> pType
    pTypeVar = TypeVar <$> pUniqueness <*> pQualName <*> many pTypeArg
    pTypeArg =
      choice
        [ TypeArgDim <$> pSize,
          TypeArgType . second (const NoUniqueness) <$> pTypeArgType
        ]
    pTypeArgType =
      choice
        [ Scalar . Prim <$> pPrimType,
          parens pType
        ]

pArrayType :: Parser ResType
pArrayType =
  Array
    <$> pUniqueness
    <*> (Shape <$> some pSize)
    <*> (second (const NoUniqueness) <$> pScalarNonFun)

pNonFunType :: Parser ResType
pNonFunType =
  choice
    [ try pArrayType,
      try $ parens pType,
      Scalar <$> pScalarNonFun
    ]

pScalarType :: Parser (ScalarTypeBase Size Uniqueness)
pScalarType = choice [try pFun, pScalarNonFun]
  where
    pFun =
      pParam <* lexeme "->" <*> pRetType
    pParam =
      choice
        [ try pNamedParam,
          do
            t <- pNonFunType
            pure $ Arrow Nonunique Unnamed (diet $ resToParam t) (toStruct t)
        ]
    pNamedParam = parens $ do
      v <- pVName <* lexeme ":"
      t <- pType
      pure $ Arrow Nonunique (Named v) (diet $ resToParam t) (toStruct t)

pRetType :: Parser ResRetType
pRetType =
  choice
    [ lexeme "?" *> (RetType <$> some (brackets pVName) <* lexeme "." <*> pType),
      RetType [] <$> pType
    ]

pType :: Parser ResType
pType =
  choice [try $ Scalar <$> pScalarType, pArrayType, parens pType]

fromStringParse :: Parser a -> String -> String -> a
fromStringParse p what s =
  either onError id $ parse (p <* eof) "" (T.pack s)
  where
    onError e =
      error $ "not a " <> what <> ": " <> s <> "\n" <> errorBundlePretty e

instance IsString (ScalarTypeBase Size NoUniqueness) where
  fromString =
    fromStringParse (second (const NoUniqueness) <$> pScalarType) "ScalarType"

instance IsString StructType where
  fromString =
    fromStringParse (second (const NoUniqueness) <$> pType) "StructType"

instance IsString StructRetType where
  fromString =
    fromStringParse (second (pure NoUniqueness) <$> pRetType) "StructRetType"

instance IsString ResRetType where
  fromString = fromStringParse pRetType "ResRetType"

instance IsString UncheckedExp where
  fromString = either (error . T.unpack . syntaxErrorMsg) id . parseExp "string literal" . T.pack
