{-# LANGUAGE OverloadedStrings #-}

-- | Building blocks for parsing prim primexpressions.  *Not* an infix
-- representation.
module Futhark.Analysis.PrimExp.Parse
  ( pPrimExp,
    pPrimValue,

    -- * Module reexport
    module Futhark.Analysis.PrimExp,
  )
where

import Data.Functor
import qualified Data.Text as T
import Data.Void
import Futhark.Analysis.PrimExp
import Futhark.IR.Primitive.Parse
import Futhark.Util.Pretty (prettyText)
import Text.Megaparsec

type Parser = Parsec Void T.Text

pBinOp :: Parser BinOp
pBinOp = choice $ map p allBinOps
  where
    p op = keyword (prettyText op) $> op

pCmpOp :: Parser CmpOp
pCmpOp = choice $ map p allCmpOps
  where
    p op = keyword (prettyText op) $> op

pUnOp :: Parser UnOp
pUnOp = choice $ map p allUnOps
  where
    p op = keyword (prettyText op) $> op

pConvOp :: Parser ConvOp
pConvOp = choice $ map p allConvOps
  where
    p op = keyword (prettyText op) $> op

parens :: Parser a -> Parser a
parens = between (lexeme "(") (lexeme ")")

-- | Parse a 'PrimExp' given a leaf parser.
pPrimExp :: Parser (v, PrimType) -> Parser (PrimExp v)
pPrimExp pLeaf =
  choice
    [ uncurry LeafExp <$> pLeaf,
      ValueExp <$> pPrimValue,
      BinOpExp <$> pBinOp <*> pPrimExp pLeaf <*> pPrimExp pLeaf,
      CmpOpExp <$> pCmpOp <*> pPrimExp pLeaf <*> pPrimExp pLeaf,
      ConvOpExp <$> pConvOp <*> pPrimExp pLeaf,
      UnOpExp <$> pUnOp <*> pPrimExp pLeaf,
      parens $ pPrimExp pLeaf
    ]
