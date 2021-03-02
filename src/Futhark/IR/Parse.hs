{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Parser for the Futhark core language.
module Futhark.IR.Parse (parseSOACS) where

import Data.Char
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Void
import Futhark.IR
import Futhark.IR.SOACS (SOACS)
import qualified Futhark.IR.SOACS.SOAC as SOAC
import Futhark.Util.Pretty (prettyText)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

pStringLiteral :: Parser String
pStringLiteral = char '"' >> manyTill L.charLiteral (char '"')

constituent :: Char -> Bool
constituent c = isAlphaNum c || (c `elem` ("_/'+-=!&^.<>*|" :: String))

whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = try . L.lexeme whitespace

keyword :: T.Text -> Parser ()
keyword s = lexeme (chunk s *> notFollowedBy (satisfy constituent))

pName :: Parser Name
pName = lexeme $ fmap nameFromString $ (:) <$> satisfy isAlpha <*> many (satisfy constituent)

pVName :: Parser VName
pVName = lexeme $ do
  (s, tag) <-
    satisfy constituent `manyTill_` try pTag
      <?> "variable name"
  pure $ VName (nameFromString s) tag
  where
    pTag =
      "_" *> L.decimal <* notFollowedBy (satisfy constituent)

pInt :: Parser Int
pInt = lexeme L.decimal

braces, brackets, parens :: Parser a -> Parser a
braces = between (lexeme "{") (lexeme "}")
brackets = between (lexeme "[") (lexeme "]")
parens = between (lexeme "(") (lexeme ")")

pComma, pColon, pSemi, pEqual :: Parser ()
pComma = void $ lexeme ","
pColon = void $ lexeme ":"
pSemi = void $ lexeme ";"
pEqual = void $ lexeme "="

pFloatType :: Parser FloatType
pFloatType = choice $ map p allFloatTypes
  where
    p t = keyword (prettyText t) $> t

pIntType :: Parser IntType
pIntType = choice $ map p allIntTypes
  where
    p t = keyword (prettyText t) $> t

pPrimType :: Parser PrimType
pPrimType =
  choice [p Bool, p Cert, FloatType <$> pFloatType, IntType <$> pIntType]
  where
    p t = keyword (prettyText t) $> t

pNonArray :: Parser (TypeBase shape u)
pNonArray = Prim <$> pPrimType

pTypeBase ::
  ArrayShape shape =>
  Parser shape ->
  Parser u ->
  Parser (TypeBase shape u)
pTypeBase ps pu = do
  u <- pu
  shape <- ps
  arrayOf <$> pNonArray <*> pure shape <*> pure u

pShape :: Parser Shape
pShape = Shape <$> many (brackets pSubExp)

pExtSize :: Parser ExtSize
pExtSize =
  choice
    [ lexeme $ "?" $> Ext <*> L.decimal,
      Free <$> pSubExp
    ]

pExtShape :: Parser ExtShape
pExtShape = Shape <$> many (brackets pExtSize)

pType :: Parser Type
pType = pTypeBase pShape (pure NoUniqueness)

pExtType :: Parser ExtType
pExtType = pTypeBase pExtShape (pure NoUniqueness)

pDeclBase ::
  Parser (TypeBase shape NoUniqueness) ->
  Parser (TypeBase shape Uniqueness)
pDeclBase p =
  choice
    [ lexeme "*" $> (`toDecl` Unique) <*> p,
      (`toDecl` Nonunique) <$> p
    ]

pDeclType :: Parser DeclType
pDeclType = pDeclBase pType

pDeclExtType :: Parser DeclExtType
pDeclExtType = pDeclBase pExtType

pRetType :: Parser (RetType SOACS)
pRetType = pDeclExtType

pRetTypes :: Parser [RetType SOACS]
pRetTypes = braces $ pRetType `sepBy` pComma

pBranchType :: Parser (BranchType SOACS)
pBranchType = pExtType

pBranchTypes :: Parser [BranchType SOACS]
pBranchTypes = braces $ pBranchType `sepBy` pComma

pFParam :: Parser (FParam SOACS)
pFParam = Param <$> pVName <*> (pColon *> pDeclType)

pFParams :: Parser [FParam SOACS]
pFParams = parens $ pFParam `sepBy` pComma

pLParam :: Parser (LParam SOACS)
pLParam = Param <$> pVName <*> (pColon *> pType)

pLParams :: Parser [LParam SOACS]
pLParams = parens $ pLParam `sepBy` pComma

pIntValue :: Parser IntValue
pIntValue = try $ do
  x <- L.signed (pure ()) L.decimal
  t <- pIntType
  pure $ intValue t (x :: Integer)

pFloatValue :: Parser FloatValue
pFloatValue =
  choice
    [ pNum,
      keyword "f32.nan" $> Float32Value (0 / 0),
      keyword "f32.inf" $> Float32Value (1 / 0),
      keyword "f64.nan" $> Float64Value (0 / 0),
      keyword "f64.inf" $> Float64Value (1 / 0)
    ]
  where
    pNum = try $ do
      x <- L.signed (pure ()) L.float
      t <- pFloatType
      pure $ floatValue t (x :: Double)

pBoolValue :: Parser Bool
pBoolValue =
  choice
    [ keyword "true" $> True,
      keyword "false" $> False
    ]

pPrimValue :: Parser PrimValue
pPrimValue =
  choice
    [ FloatValue <$> pFloatValue,
      IntValue <$> pIntValue,
      BoolValue <$> pBoolValue
    ]
    <?> "primitive value"

pSubExp :: Parser SubExp
pSubExp = Var <$> pVName <|> Constant <$> pPrimValue

pPatElem :: Parser (PatElemT Type)
pPatElem =
  (PatElem <$> pVName <*> (pColon *> pType))
    <?> "pattern element"

pPatternLike :: Parser a -> Parser ([a], [a])
pPatternLike p = braces $ do
  xs <- p `sepBy` pComma
  choice
    [ pSemi *> ((xs,) <$> (p `sepBy` pComma)),
      pure (mempty, xs)
    ]

pPattern :: Parser (Pattern SOACS)
pPattern = uncurry Pattern <$> pPatternLike pPatElem

pConvOp ::
  T.Text -> (t1 -> t2 -> ConvOp) -> Parser t1 -> Parser t2 -> Parser BasicOp
pConvOp s op t1 t2 =
  keyword s $> op' <*> t1 <*> pSubExp <*> (keyword "to" *> t2)
  where
    op' f se t = ConvOp (op f t) se

pBinOp :: Parser BasicOp
pBinOp = choice (map p allBinOps) <?> "binary op"
  where
    p bop =
      keyword (prettyText bop)
        *> parens (BinOp bop <$> pSubExp <* pComma <*> pSubExp)

pCmpOp :: Parser BasicOp
pCmpOp = choice (map p allCmpOps) <?> "comparison op"
  where
    p op =
      keyword (prettyText op)
        *> parens (CmpOp op <$> pSubExp <* pComma <*> pSubExp)

pUnOp :: Parser BasicOp
pUnOp = choice (map p allUnOps) <?> "unary op"
  where
    p bop = keyword (prettyText bop) $> UnOp bop <*> pSubExp

pDimIndex :: Parser (DimIndex SubExp)
pDimIndex =
  choice
    [ try $
        DimSlice <$> pSubExp <* lexeme ":+"
          <*> pSubExp <* lexeme "*"
          <*> pSubExp,
      DimFix <$> pSubExp
    ]

pSlice :: Parser (Slice SubExp)
pSlice = brackets $ pDimIndex `sepBy` pComma

pIndex :: Parser BasicOp
pIndex = try $ Index <$> pVName <*> pSlice

pErrorMsgPart :: Parser (ErrorMsgPart SubExp)
pErrorMsgPart =
  choice
    [ ErrorString <$> pStringLiteral,
      flip ($) <$> (pSubExp <* pColon)
        <*> choice
          [ keyword "i32" $> ErrorInt32,
            keyword "i64" $> ErrorInt64
          ]
    ]

pErrorMsg :: Parser (ErrorMsg SubExp)
pErrorMsg = ErrorMsg <$> braces (pErrorMsgPart `sepBy` pComma)

pSrcLoc :: Parser SrcLoc
pSrcLoc = pStringLiteral $> mempty -- FIXME

pErrorLoc :: Parser (SrcLoc, [SrcLoc])
pErrorLoc = (,mempty) <$> pSrcLoc

pShapeChange :: Parser (ShapeChange SubExp)
pShapeChange = parens $ pDimChange `sepBy` pComma
  where
    pDimChange =
      choice
        [ "~" $> DimCoercion <*> pSubExp,
          DimNew <$> pSubExp
        ]

pIota :: Parser BasicOp
pIota =
  choice $ map p allIntTypes
  where
    p t =
      keyword ("iota" <> prettyText (primBitSize (IntType t)))
        *> parens
          ( Iota
              <$> pSubExp <* pComma
              <*> pSubExp <* pComma
              <*> pSubExp
              <*> pure t
          )

pBasicOp :: Parser BasicOp
pBasicOp =
  choice
    [ keyword "opaque" $> Opaque <*> parens pSubExp,
      keyword "copy" $> Copy <*> parens pVName,
      keyword "assert"
        *> parens
          ( Assert <$> pSubExp <* pComma
              <*> pErrorMsg <* pComma
              <*> pErrorLoc
          ),
      keyword "rotate"
        *> parens
          (Rotate <$> parens (pSubExp `sepBy` pComma) <* pComma <*> pVName),
      keyword "replicate"
        *> parens (Replicate <$> pShape <* pComma <*> pSubExp),
      keyword "reshape"
        *> parens (Reshape <$> pShapeChange <* pComma <*> pVName),
      keyword "scratch"
        *> parens (Scratch <$> pPrimType <*> many (pComma *> pSubExp)),
      keyword "rearrange"
        *> parens
          ( Rearrange
              <$> parens (pInt `sepBy` pComma) <* pComma
              <*> pVName
          ),
      keyword "concat" *> do
        d <- "@" *> L.decimal
        parens $ do
          w <- pSubExp <* pComma
          Concat d <$> pVName <*> many (pComma *> pVName) <*> pure w,
      pIota,
      try $
        Update
          <$> pVName <* keyword "with"
          <*> pSlice <* lexeme "="
          <*> pSubExp,
      ArrayLit
        <$> brackets (pSubExp `sepBy` pComma)
        <*> (lexeme ":" *> "[]" *> pType),
      --
      pConvOp "sext" SExt pIntType pIntType,
      pConvOp "zext" ZExt pIntType pIntType,
      pConvOp "fpconv" FPConv pFloatType pFloatType,
      pConvOp "fptoui" FPToUI pFloatType pIntType,
      pConvOp "fptosi" FPToSI pFloatType pIntType,
      pConvOp "uitofp" UIToFP pIntType pFloatType,
      pConvOp "sitofp" SIToFP pIntType pFloatType,
      pConvOp "itob" (const . IToB) pIntType (keyword "bool"),
      pConvOp "btoi" (const BToI) (keyword "bool") pIntType,
      --
      pIndex,
      pBinOp,
      pCmpOp,
      pUnOp,
      SubExp <$> pSubExp
    ]

pIf :: Parser (Exp SOACS)
pIf =
  keyword "if" $> f <*> pSort <*> pSubExp
    <*> (keyword "then" *> pBranchBody)
    <*> (keyword "else" *> pBranchBody)
    <*> (lexeme ":" *> pBranchTypes)
  where
    pSort =
      choice
        [ lexeme "<fallback>" $> IfFallback,
          lexeme "<equiv>" $> IfEquiv,
          pure IfNormal
        ]
    f sort cond tbranch fbranch t =
      If cond tbranch fbranch $ IfDec t sort
    pBranchBody =
      choice
        [ try $ braces $ Body () mempty <$> pSubExp `sepBy` pComma,
          braces pBody
        ]

pApply :: Parser (Exp SOACS)
pApply = do
  keyword "apply"
  fname <- pName
  parens $ case M.lookup fname builtInFunctions of
    Just (_, ret) -> do
      args <- pArg `sepBy` pComma
      pure $ Apply fname args (map Prim ret) (Safe, mempty, mempty)
    Nothing ->
      fail $ "Unknown function: " ++ pretty fname
  where
    pArg =
      choice
        [ lexeme "*" $> (,Consume) <*> pSubExp,
          (,Observe) <$> pSubExp
        ]

pLoop :: Parser (Exp SOACS)
pLoop =
  keyword "loop" $> uncurry DoLoop
    <*> pLoopParams
    <*> pLoopForm <* keyword "do"
    <*> braces pBody
  where
    pLoopParams :: Parser ([(FParam SOACS, SubExp)], [(FParam SOACS, SubExp)])
    pLoopParams = do
      (ctx, val) <- pPatternLike pFParam
      void $ lexeme "="
      (ctx_init, val_init) <-
        splitAt (length ctx) <$> braces (pSubExp `sepBy` pComma)
      pure (zip ctx ctx_init, zip val val_init)

    pLoopForm :: Parser (LoopForm SOACS)
    pLoopForm =
      choice
        [ keyword "for" $> ForLoop
            <*> pVName <* lexeme ":"
            <*> pIntType <* lexeme "<"
            <*> pSubExp
            <*> many ((,) <$> pLParam <* lexeme "in" <*> pVName),
          keyword "while" $> WhileLoop <*> pVName
        ]

pLambda :: Parser (Lambda SOACS)
pLambda =
  choice
    [ keyword "fn"
        $> lam
        <*> braces (pType `sepBy` pComma)
        <*> pLParams <* lexeme "=>"
        <*> pBody,
      keyword "nilFn" $> Lambda mempty (Body () mempty []) []
    ]
  where
    lam ret params body = Lambda params body ret

pComm :: Parser Commutativity
pComm =
  choice
    [ keyword "commutative" $> Commutative,
      pure Noncommutative
    ]

pReduce :: Parser (SOAC.Reduce SOACS)
pReduce =
  SOAC.Reduce
    <$> pComm
    <*> pLambda <* pComma
    <*> braces (pSubExp `sepBy` pComma)

pScan :: Parser (SOAC.Scan SOACS)
pScan =
  SOAC.Scan
    <$> pLambda <* pComma
    <*> braces (pSubExp `sepBy` pComma)

pSOAC :: Parser (SOAC.SOAC SOACS)
pSOAC =
  choice
    [ keyword "map" *> pScrema pMapForm,
      keyword "redomap" *> pScrema pRedomapForm,
      keyword "scanomap" *> pScrema pScanomapForm,
      keyword "screma" *> pScrema pScremaForm,
      pScatter,
      pHist,
      pStream
    ]
  where
    pScrema p =
      parens $
        SOAC.Screma
          <$> pSubExp <* pComma
          <*> p <* pComma
          <*> (pVName `sepBy` pComma)
    pScremaForm =
      SOAC.ScremaForm
        <$> braces (pScan `sepBy` pComma) <* pComma
        <*> braces (pReduce `sepBy` pComma) <* pComma
        <*> pLambda
    pRedomapForm =
      SOAC.ScremaForm mempty
        <$> braces (pReduce `sepBy` pComma) <* pComma
        <*> pLambda
    pScanomapForm =
      SOAC.ScremaForm
        <$> braces (pScan `sepBy` pComma) <* pComma
        <*> pure mempty
        <*> pLambda
    pMapForm =
      SOAC.ScremaForm mempty mempty <$> pLambda
    pScatter =
      keyword "scatter"
        *> parens
          ( SOAC.Scatter <$> pSubExp <* pComma
              <*> pLambda <* pComma
              <*> braces (pVName `sepBy` pComma)
              <*> many (pComma *> pDest)
          )
      where
        pDest =
          parens $ (,,) <$> pShape <* pComma <*> pInt <* pComma <*> pVName
    pHist =
      keyword "hist"
        *> parens
          ( SOAC.Hist
              <$> pSubExp <* pComma
              <*> braces (pHistOp `sepBy` pComma) <* pComma
              <*> pLambda
              <*> many (pComma *> pVName)
          )
      where
        pHistOp =
          SOAC.HistOp
            <$> pSubExp <* pComma
            <*> pSubExp <* pComma
            <*> braces (pVName `sepBy` pComma) <* pComma
            <*> braces (pSubExp `sepBy` pComma) <* pComma
            <*> pLambda
    pStream =
      choice
        [ keyword "streamParComm" *> pStreamPar SOAC.InOrder Commutative,
          keyword "streamPar" *> pStreamPar SOAC.InOrder Noncommutative,
          keyword "streamParPerComm" *> pStreamPar SOAC.Disorder Commutative,
          keyword "streamParPer" *> pStreamPar SOAC.Disorder Noncommutative,
          keyword "streamSeq" *> pStreamSeq
        ]
    pStreamPar order comm =
      parens $
        SOAC.Stream
          <$> pSubExp <* pComma
          <*> pParForm order comm <* pComma
          <*> pLambda <* pComma
          <*> braces (pSubExp `sepBy` pComma)
          <*> many (pComma *> pVName)
    pParForm order comm =
      SOAC.Parallel order comm <$> pLambda
    pStreamSeq =
      parens $
        SOAC.Stream
          <$> pSubExp <* pComma
          <*> pure SOAC.Sequential
          <*> pLambda <* pComma
          <*> braces (pSubExp `sepBy` pComma)
          <*> many (pComma *> pVName)

pExp :: Parser (Exp SOACS)
pExp =
  choice
    [ pIf,
      pApply,
      pLoop,
      BasicOp <$> pBasicOp,
      Op <$> pSOAC
    ]

pAttr :: Parser Attr
pAttr = do
  v <- pName
  choice
    [ AttrComp v <$> parens (pAttr `sepBy` pComma),
      pure $ AttrAtom v
    ]

pAttrs :: Parser Attrs
pAttrs = Attrs . S.fromList <$> many pAttr'
  where
    pAttr' = lexeme "#[" *> pAttr <* lexeme "]"

pStm :: Parser (Stm SOACS)
pStm = do
  aux <- flip StmAux <$> pAttrs <*> pCerts <*> pure ()
  keyword "let" $> Let <*> pPattern <* pEqual <*> pure aux <*> pExp
  where
    pCerts =
      choice
        [ lexeme "#" *> braces (Certificates <$> pVName `sepBy` pComma)
            <?> "certificates",
          pure mempty
        ]

pStms :: Parser (Stms SOACS)
pStms = stmsFromList <$> many pStm

pResult :: Parser Result
pResult = braces $ pSubExp `sepBy` pComma

pBody :: Parser (Body SOACS)
pBody =
  choice
    [ Body () <$> pStms <* keyword "in" <*> pResult,
      Body () mempty <$> pResult
    ]

pFunDef :: Parser (FunDef SOACS)
pFunDef = do
  attrs <- pAttrs
  entry <- (keyword "entry" <|> keyword "fun") $> Nothing
  ret <- pRetTypes
  FunDef entry attrs
    <$> pName
    <*> pure ret
    <*> pFParams
    <*> (pEqual *> braces pBody)

pProg :: Parser (Prog SOACS)
pProg = Prog <$> pStms <*> many pFunDef

parseSOACS :: FilePath -> T.Text -> Either T.Text (Prog SOACS)
parseSOACS fname s =
  either (Left . T.pack . errorBundlePretty) Right $
    parse (whitespace *> pProg <* eof) fname s
