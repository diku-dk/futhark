{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Futhark.SyntaxTests (tests) where

import Control.Applicative hiding (many, some)
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
    let (s', '_' : tag) = span (/= '_') s
     in VName (fromString s') (read tag)

instance IsString v => IsString (QualName v) where
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
      [ ConstSize <$> lexeme L.decimal,
        NamedSize <$> pQualName
      ]

pScalarNonFun :: Parser (ScalarTypeBase Size ())
pScalarNonFun =
  choice
    [ Prim <$> pPrimType,
      pTypeVar,
      tupleRecord <$> parens (pStructType `sepBy1` lexeme ","),
      Record . M.fromList <$> braces (pField `sepBy1` lexeme ",")
    ]
  where
    pField = (,) <$> pName <* lexeme ":" <*> pStructType
    pTypeVar = TypeVar () <$> pUniqueness <*> pQualName <*> many pTypeArg
    pTypeArg =
      choice
        [ TypeArgDim <$> pSize <*> pure mempty,
          TypeArgType <$> pTypeArgType <*> pure mempty
        ]
    pTypeArgType =
      choice
        [ Scalar . Prim <$> pPrimType,
          parens pStructType
        ]

pArrayType :: Parser StructType
pArrayType =
  Array () <$> pUniqueness <*> (Shape <$> some pSize) <*> pScalarNonFun

pNonFunType :: Parser StructType
pNonFunType =
  choice [try pArrayType, try $ parens pStructType, Scalar <$> pScalarNonFun]

pScalarType :: Parser (ScalarTypeBase Size ())
pScalarType = choice [try pFun, pScalarNonFun]
  where
    pFun =
      pParam <* lexeme "->" <*> pStructRetType
    pParam =
      choice
        [ try pNamedParam,
          do
            t <- pNonFunType
            pure $ Arrow () Unnamed (diet t) t
        ]
    pNamedParam = parens $ do
      v <- pVName <* lexeme ":"
      t <- pStructType
      pure $ Arrow () (Named v) (diet t) t

pStructRetType :: Parser StructRetType
pStructRetType =
  choice
    [ lexeme "?" *> (RetType <$> some (brackets pVName) <* lexeme "." <*> pStructType),
      RetType [] <$> pStructType
    ]

pStructType :: Parser StructType
pStructType =
  choice [try $ Scalar <$> pScalarType, pArrayType, parens pStructType]

fromStringParse :: Parser a -> String -> String -> a
fromStringParse p what s =
  either onError id $ parse (p <* eof) "" (T.pack s)
  where
    onError e =
      error $ "not a " <> what <> ": " <> s <> "\n" <> errorBundlePretty e

instance IsString (ScalarTypeBase Size ()) where
  fromString = fromStringParse pScalarType "ScalarType"

instance IsString StructType where
  fromString = fromStringParse pStructType "StructType"

instance IsString StructRetType where
  fromString = fromStringParse pStructRetType "StructRetType"
