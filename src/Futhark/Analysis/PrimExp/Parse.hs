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
import Data.Text qualified as T
import Data.Void
import Futhark.Analysis.PrimExp
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Primitive.Parse
import Text.Megaparsec

pBinOp :: Parsec Void T.Text BinOp
pBinOp = choice $ map p allBinOps
  where
    p op = keyword (prettyText op) $> op

pCmpOp :: Parsec Void T.Text CmpOp
pCmpOp = choice $ map p allCmpOps
  where
    p op = keyword (prettyText op) $> op

pUnOp :: Parsec Void T.Text UnOp
pUnOp = choice $ map p allUnOps
  where
    p op = keyword (prettyText op) $> op

pConvOp :: Parsec Void T.Text ConvOp
pConvOp = choice $ map p allConvOps
  where
    p op = keyword (prettyText op) $> op

parens :: Parsec Void T.Text a -> Parsec Void T.Text a
parens = between (lexeme "(") (lexeme ")")

-- | Parse a 'PrimExp' given a leaf parser.
pPrimExp :: PrimType -> Parsec Void T.Text v -> Parsec Void T.Text (PrimExp v)
pPrimExp t pLeaf =
  choice
    [ flip LeafExp t <$> pLeaf,
      ValueExp <$> pPrimValue,
      pBinOp >>= binOpExp,
      pCmpOp >>= cmpOpExp,
      pConvOp >>= convOpExp,
      pUnOp >>= unOpExp,
      parens $ pPrimExp t pLeaf
    ]
  where
    binOpExp op =
      BinOpExp op
        <$> pPrimExp (binOpType op) pLeaf
        <*> pPrimExp (binOpType op) pLeaf
    cmpOpExp op =
      CmpOpExp op
        <$> pPrimExp (cmpOpType op) pLeaf
        <*> pPrimExp (cmpOpType op) pLeaf
    convOpExp op =
      ConvOpExp op <$> pPrimExp (fst (convOpType op)) pLeaf
    unOpExp op =
      UnOpExp op <$> pPrimExp (unOpType op) pLeaf
