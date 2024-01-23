-- | Parser for the Futhark core language.
module Futhark.IR.Parse
  ( -- * Programs
    parseSOACS,
    parseGPU,
    parseGPUMem,
    parseMC,
    parseMCMem,
    parseSeq,
    parseSeqMem,

    -- * Fragments
    parseType,
    parseDeclExtType,
    parseDeclType,
    parseVName,
    parseSubExp,
    parseSubExpRes,
    parseBodyGPU,
    parseBodyMC,
    parseStmGPU,
    parseStmMC,
  )
where

import Data.Char (isAlpha)
import Data.Functor
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Futhark.Analysis.PrimExp.Parse
import Futhark.IR
import Futhark.IR.GPU (GPU)
import Futhark.IR.GPU.Op qualified as GPU
import Futhark.IR.GPUMem (GPUMem)
import Futhark.IR.MC (MC)
import Futhark.IR.MC.Op qualified as MC
import Futhark.IR.MCMem (MCMem)
import Futhark.IR.Mem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.SOACS (SOACS)
import Futhark.IR.SOACS.SOAC qualified as SOAC
import Futhark.IR.SegOp qualified as SegOp
import Futhark.IR.Seq (Seq)
import Futhark.IR.SeqMem (SeqMem)
import Language.Futhark.Primitive.Parse
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void T.Text

pStringLiteral :: Parser T.Text
pStringLiteral =
  lexeme . fmap T.pack $ char '"' >> manyTill L.charLiteral (char '"')

pName :: Parser Name
pName =
  lexeme . fmap nameFromString $
    (:) <$> satisfy leading <*> many (satisfy constituent)
  where
    leading c = isAlpha c || c `elem` ("_+-*/%=!<>|&^.#" :: String)

pVName :: Parser VName
pVName = lexeme $ do
  (s, tag) <-
    choice [exprBox, singleton <$> satisfy constituent]
      `manyTill_` try pTag
      <?> "variable name"
  pure $ VName (nameFromString $ concat s) tag
  where
    pTag = "_" *> L.decimal <* notFollowedBy (satisfy constituent)
    exprBox = ("<{" <>) . (<> "}>") <$> (chunk "<{" *> manyTill anySingle (chunk "}>"))

pInt :: Parser Int
pInt = lexeme L.decimal

pInt64 :: Parser Int64
pInt64 = lexeme L.decimal

braces, brackets, parens :: Parser a -> Parser a
braces = between (lexeme "{") (lexeme "}")
brackets = between (lexeme "[") (lexeme "]")
parens = between (lexeme "(") (lexeme ")")

pComma, pColon, pSemi, pEqual, pSlash, pAsterisk, pArrow :: Parser ()
pComma = void $ lexeme ","
pColon = void $ lexeme ":"
pSemi = void $ lexeme ";"
pEqual = void $ lexeme "="
pSlash = void $ lexeme "/"
pAsterisk = void $ lexeme "*"
pArrow = void $ lexeme "->"

pNonArray :: Parser (TypeBase shape NoUniqueness)
pNonArray =
  choice
    [ Prim <$> pPrimType,
      "acc"
        *> parens
          ( Acc
              <$> pVName
              <* pComma
              <*> pShape
              <* pComma
              <*> pTypes
              <*> pure NoUniqueness
          )
    ]

pTypeBase ::
  (ArrayShape shape) =>
  Parser shape ->
  Parser u ->
  Parser (TypeBase shape u)
pTypeBase ps pu = do
  u <- pu
  shape <- ps
  arrayOf <$> pNonArray <*> pure shape <*> pure u

pShape :: Parser Shape
pShape = Shape <$> many (brackets pSubExp)

pExt :: Parser a -> Parser (Ext a)
pExt p =
  choice
    [ lexeme $ "?" $> Ext <*> L.decimal,
      Free <$> p
    ]

pExtSize :: Parser ExtSize
pExtSize = pExt pSubExp

pExtShape :: Parser ExtShape
pExtShape = Shape <$> many (brackets pExtSize)

pType :: Parser Type
pType = pTypeBase pShape (pure NoUniqueness)

pTypes :: Parser [Type]
pTypes = braces $ pType `sepBy` pComma

pExtType :: Parser ExtType
pExtType = pTypeBase pExtShape (pure NoUniqueness)

pRank :: Parser Rank
pRank = Rank . length <$> many "[]"

pUniqueness :: Parser Uniqueness
pUniqueness = choice [pAsterisk $> Unique, pure Nonunique]

pDeclBase ::
  Parser (TypeBase shape NoUniqueness) ->
  Parser (TypeBase shape Uniqueness)
pDeclBase p = flip toDecl <$> pUniqueness <*> p

pDeclType :: Parser DeclType
pDeclType = pDeclBase pType

pDeclExtType :: Parser DeclExtType
pDeclExtType = pDeclBase pExtType

pSubExp :: Parser SubExp
pSubExp = Var <$> pVName <|> Constant <$> pPrimValue

pSubExps :: Parser [SubExp]
pSubExps = braces (pSubExp `sepBy` pComma)

pVNames :: Parser [VName]
pVNames = braces (pVName `sepBy` pComma)

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
        DimSlice
          <$> pSubExp
          <* lexeme ":+"
          <*> pSubExp
          <* lexeme "*"
          <*> pSubExp,
      DimFix <$> pSubExp
    ]

pSlice :: Parser (Slice SubExp)
pSlice = Slice <$> brackets (pDimIndex `sepBy` pComma)

pIndex :: Parser BasicOp
pIndex = try $ Index <$> pVName <*> pSlice

pFlatDimIndex :: Parser (FlatDimIndex SubExp)
pFlatDimIndex =
  FlatDimIndex <$> pSubExp <* lexeme ":" <*> pSubExp

pFlatSlice :: Parser (FlatSlice SubExp)
pFlatSlice =
  brackets $ FlatSlice <$> pSubExp <* pSemi <*> (pFlatDimIndex `sepBy` pComma)

pFlatIndex :: Parser BasicOp
pFlatIndex = try $ FlatIndex <$> pVName <*> pFlatSlice

pErrorMsgPart :: Parser (ErrorMsgPart SubExp)
pErrorMsgPart =
  choice
    [ ErrorString <$> pStringLiteral,
      flip ErrorVal <$> (pSubExp <* pColon) <*> pPrimType
    ]

pErrorMsg :: Parser (ErrorMsg SubExp)
pErrorMsg = ErrorMsg <$> braces (pErrorMsgPart `sepBy` pComma)

pSrcLoc :: Parser SrcLoc
pSrcLoc = pStringLiteral $> mempty -- FIXME

pErrorLoc :: Parser (SrcLoc, [SrcLoc])
pErrorLoc = (,mempty) <$> pSrcLoc

pIota :: Parser BasicOp
pIota =
  choice $ map p allIntTypes
  where
    p t =
      keyword ("iota" <> prettyText (primBitSize (IntType t)))
        *> parens
          ( Iota
              <$> pSubExp
              <* pComma
              <*> pSubExp
              <* pComma
              <*> pSubExp
              <*> pure t
          )

pBasicOp :: Parser BasicOp
pBasicOp =
  choice
    [ keyword "opaque" $> Opaque OpaqueNil <*> parens pSubExp,
      keyword "trace"
        $> uncurry (Opaque . OpaqueTrace)
        <*> parens ((,) <$> pStringLiteral <* pComma <*> pSubExp),
      keyword "copy" $> Replicate mempty . Var <*> parens pVName,
      keyword "assert"
        *> parens
          ( Assert
              <$> pSubExp
              <* pComma
              <*> pErrorMsg
              <* pComma
              <*> pErrorLoc
          ),
      keyword "replicate"
        *> parens (Replicate <$> pShape <* pComma <*> pSubExp),
      keyword "reshape"
        *> parens (Reshape ReshapeArbitrary <$> pShape <* pComma <*> pVName),
      keyword "coerce"
        *> parens (Reshape ReshapeCoerce <$> pShape <* pComma <*> pVName),
      keyword "scratch"
        *> parens (Scratch <$> pPrimType <*> many (pComma *> pSubExp)),
      keyword "rearrange"
        *> parens
          (Rearrange <$> parens (pInt `sepBy` pComma) <* pComma <*> pVName),
      keyword "manifest"
        *> parens
          (Manifest <$> parens (pInt `sepBy` pComma) <* pComma <*> pVName),
      keyword "concat" *> do
        d <- "@" *> L.decimal
        parens $ do
          w <- pSubExp <* pComma
          x <- pVName
          ys <- many (pComma *> pVName)
          pure $ Concat d (x :| ys) w,
      pIota,
      try $
        flip Update
          <$> pVName
          <* keyword "with"
          <*> choice [lexeme "?" $> Safe, pure Unsafe]
          <*> pSlice
          <* lexeme "="
          <*> pSubExp,
      try $
        FlatUpdate
          <$> pVName
          <* keyword "with"
          <*> pFlatSlice
          <* lexeme "="
          <*> pVName,
      ArrayLit
        <$> brackets (pSubExp `sepBy` pComma)
        <*> (lexeme ":" *> "[]" *> pType),
      keyword "update_acc"
        *> parens
          (UpdateAcc <$> pVName <* pComma <*> pSubExps <* pComma <*> pSubExps),
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
      pConvOp "ftob" (const . FToB) pFloatType (keyword "bool"),
      pConvOp "btof" (const BToF) (keyword "bool") pFloatType,
      --
      pIndex,
      pFlatIndex,
      pBinOp,
      pCmpOp,
      pUnOp,
      SubExp <$> pSubExp
    ]

pAttr :: Parser Attr
pAttr =
  choice
    [ AttrInt . toInteger <$> pInt,
      do
        v <- pName
        choice
          [ AttrComp v <$> parens (pAttr `sepBy` pComma),
            pure $ AttrName v
          ]
    ]

pAttrs :: Parser Attrs
pAttrs = Attrs . S.fromList <$> many pAttr'
  where
    pAttr' = lexeme "#[" *> pAttr <* lexeme "]"

pComm :: Parser Commutativity
pComm =
  choice
    [ keyword "commutative" $> Commutative,
      pure Noncommutative
    ]

-- | This record contains parser for all the representation-specific
-- bits.  Essentially a manually passed-around type class dictionary,
-- because ambiguities make it impossible to write this with actual
-- type classes.
data PR rep = PR
  { pRetType :: Parser (RetType rep),
    pBranchType :: Parser (BranchType rep),
    pFParamInfo :: Parser (FParamInfo rep),
    pLParamInfo :: Parser (LParamInfo rep),
    pLetDec :: Parser (LetDec rep),
    pOp :: Parser (Op rep),
    pBodyDec :: BodyDec rep,
    pExpDec :: ExpDec rep
  }

pRetAls :: Parser RetAls
pRetAls = fromMaybe (RetAls mempty mempty) <$> optional p
  where
    p = lexeme "#" *> parens (RetAls <$> pInts <* pComma <*> pInts)
    pInts = brackets $ pInt `sepBy` pComma

pRetTypes :: PR rep -> Parser [(RetType rep, RetAls)]
pRetTypes pr = braces $ ((,) <$> pRetType pr <*> pRetAls) `sepBy` pComma

pBranchTypes :: PR rep -> Parser [BranchType rep]
pBranchTypes pr = braces $ pBranchType pr `sepBy` pComma

pParam :: Parser t -> Parser (Param t)
pParam p = Param <$> pAttrs <*> pVName <*> (pColon *> p)

pFParam :: PR rep -> Parser (FParam rep)
pFParam = pParam . pFParamInfo

pFParams :: PR rep -> Parser [FParam rep]
pFParams pr = parens $ pFParam pr `sepBy` pComma

pLParam :: PR rep -> Parser (LParam rep)
pLParam = pParam . pLParamInfo

pLParams :: PR rep -> Parser [LParam rep]
pLParams pr = braces $ pLParam pr `sepBy` pComma

pPatElem :: PR rep -> Parser (PatElem (LetDec rep))
pPatElem pr =
  (PatElem <$> pVName <*> (pColon *> pLetDec pr)) <?> "pattern element"

pPat :: PR rep -> Parser (Pat (LetDec rep))
pPat pr = Pat <$> braces (pPatElem pr `sepBy` pComma)

pResult :: Parser Result
pResult = braces $ pSubExpRes `sepBy` pComma

pMatchSort :: Parser MatchSort
pMatchSort =
  choice
    [ lexeme "<fallback>" $> MatchFallback,
      lexeme "<equiv>" $> MatchEquiv,
      pure MatchNormal
    ]

pBranchBody :: PR rep -> Parser (Body rep)
pBranchBody pr =
  choice
    [ try $ Body (pBodyDec pr) mempty <$> pResult,
      braces (pBody pr)
    ]

pIf :: PR rep -> Parser (Exp rep)
pIf pr =
  keyword "if"
    $> f
    <*> pMatchSort
    <*> pSubExp
    <*> (keyword "then" *> pBranchBody pr)
    <*> (keyword "else" *> pBranchBody pr)
    <*> (lexeme ":" *> pBranchTypes pr)
  where
    f sort cond tbranch fbranch t =
      Match [cond] [Case [Just $ BoolValue True] tbranch] fbranch $ MatchDec t sort

pMatch :: PR rep -> Parser (Exp rep)
pMatch pr =
  keyword "match"
    $> f
    <*> pMatchSort
    <*> braces (pSubExp `sepBy` pComma)
    <*> many pCase
    <*> (keyword "default" *> lexeme "->" *> pBranchBody pr)
    <*> (lexeme ":" *> pBranchTypes pr)
  where
    f sort cond cases defbody t =
      Match cond cases defbody $ MatchDec t sort
    pCase =
      keyword "case"
        $> Case
        <*> braces (pMaybeValue `sepBy` pComma)
        <* lexeme "->"
        <*> pBranchBody pr
    pMaybeValue =
      choice [lexeme "_" $> Nothing, Just <$> pPrimValue]

pApply :: PR rep -> Parser (Exp rep)
pApply pr =
  keyword "apply" *> (p =<< choice [lexeme "<unsafe>" $> Unsafe, pure Safe])
  where
    p safety =
      Apply
        <$> pName
        <*> parens (pArg `sepBy` pComma)
        <* pColon
        <*> pRetTypes pr
        <*> pure (safety, mempty, mempty)

    pArg =
      choice
        [ lexeme "*" $> (,Consume) <*> pSubExp,
          (,Observe) <$> pSubExp
        ]

pLoop :: PR rep -> Parser (Exp rep)
pLoop pr =
  keyword "loop"
    $> Loop
    <*> pLoopParams
    <*> pLoopForm
    <* keyword "do"
    <*> braces (pBody pr)
  where
    pLoopParams = do
      params <- braces $ pFParam pr `sepBy` pComma
      void $ lexeme "="
      args <- braces (pSubExp `sepBy` pComma)
      pure (zip params args)

    pLoopForm =
      choice
        [ keyword "for"
            $> ForLoop
            <*> pVName
            <* lexeme ":"
            <*> pIntType
            <* lexeme "<"
            <*> pSubExp,
          keyword "while" $> WhileLoop <*> pVName
        ]

pLambda :: PR rep -> Parser (Lambda rep)
pLambda pr =
  choice
    [ lexeme "\\"
        $> Lambda
        <*> pLParams pr
        <* pColon
        <*> pTypes
        <* pArrow
        <*> pBody pr,
      keyword "nilFn" $> Lambda mempty [] (Body (pBodyDec pr) mempty [])
    ]

pReduce :: PR rep -> Parser (SOAC.Reduce rep)
pReduce pr =
  SOAC.Reduce
    <$> pComm
    <*> pLambda pr
    <* pComma
    <*> braces (pSubExp `sepBy` pComma)

pScan :: PR rep -> Parser (SOAC.Scan rep)
pScan pr =
  SOAC.Scan
    <$> pLambda pr
    <* pComma
    <*> braces (pSubExp `sepBy` pComma)

pWithAcc :: PR rep -> Parser (Exp rep)
pWithAcc pr =
  keyword "with_acc"
    *> parens (WithAcc <$> braces (pInput `sepBy` pComma) <* pComma <*> pLambda pr)
  where
    pInput =
      parens
        ( (,,)
            <$> pShape
            <* pComma
            <*> pVNames
            <*> optional (pComma *> pCombFun)
        )
    pCombFun = parens ((,) <$> pLambda pr <* pComma <*> pSubExps)

pExp :: PR rep -> Parser (Exp rep)
pExp pr =
  choice
    [ pIf pr,
      pMatch pr,
      pApply pr,
      pLoop pr,
      pWithAcc pr,
      Op <$> pOp pr,
      BasicOp <$> pBasicOp
    ]

pCerts :: Parser Certs
pCerts =
  choice
    [ lexeme "#"
        *> braces (Certs <$> pVName `sepBy` pComma)
        <?> "certificates",
      pure mempty
    ]

pSubExpRes :: Parser SubExpRes
pSubExpRes = SubExpRes <$> pCerts <*> pSubExp

pStm :: PR rep -> Parser (Stm rep)
pStm pr =
  keyword "let" $> Let <*> pPat pr <* pEqual <*> pStmAux <*> pExp pr
  where
    pStmAux = flip StmAux <$> pAttrs <*> pCerts <*> pure (pExpDec pr)

pStms :: PR rep -> Parser (Stms rep)
pStms pr = stmsFromList <$> many (pStm pr)

pBody :: PR rep -> Parser (Body rep)
pBody pr =
  choice
    [ Body (pBodyDec pr) <$> pStms pr <* keyword "in" <*> pResult,
      Body (pBodyDec pr) mempty <$> pResult
    ]

pValueType :: Parser ValueType
pValueType = comb <$> pRank <*> pSignedType
  where
    comb r (s, t) = ValueType s r t
    pSignedType =
      choice
        [ keyword "u8" $> (Unsigned, IntType Int8),
          keyword "u16" $> (Unsigned, IntType Int16),
          keyword "u32" $> (Unsigned, IntType Int32),
          keyword "u64" $> (Unsigned, IntType Int64),
          (Signed,) <$> pPrimType
        ]

pEntryPointType :: Parser EntryPointType
pEntryPointType =
  choice
    [ keyword "opaque" $> TypeOpaque . nameFromText <*> pStringLiteral,
      TypeTransparent <$> pValueType
    ]

pEntry :: Parser EntryPoint
pEntry =
  parens $
    (,,)
      <$> (nameFromText <$> pStringLiteral)
      <* pComma
      <*> pEntryPointInputs
      <* pComma
      <*> pEntryPointResults
  where
    pEntryPointInputs = braces (pEntryPointInput `sepBy` pComma)
    pEntryPointResults = braces (pEntryPointResult `sepBy` pComma)
    pEntryPointInput =
      EntryParam <$> pName <* pColon <*> pUniqueness <*> pEntryPointType
    pEntryPointResult =
      EntryResult <$> pUniqueness <*> pEntryPointType

pFunDef :: PR rep -> Parser (FunDef rep)
pFunDef pr = do
  attrs <- pAttrs
  entry <-
    choice
      [ keyword "entry" $> Just <*> pEntry,
        keyword "fun" $> Nothing
      ]
  fname <- pName
  fparams <- pFParams pr <* pColon
  ret <- pRetTypes pr
  FunDef entry attrs fname ret fparams
    <$> (pEqual *> braces (pBody pr))

pOpaqueType :: Parser (Name, OpaqueType)
pOpaqueType =
  (,)
    <$> (keyword "type" *> (nameFromText <$> pStringLiteral) <* pEqual)
    <*> choice [pRecord, pSum, pOpaque]
  where
    pFieldName = choice [pName, nameFromString . show <$> pInt]
    pField = (,) <$> pFieldName <* pColon <*> pEntryPointType
    pRecord = keyword "record" $> OpaqueRecord <*> braces (many pField)

    pConstructor = "#" *> pName
    pPayload =
      parens $
        (,)
          <$> (pEntryPointType <* pComma)
          <*> brackets (pInt `sepBy` pComma)
    pVariant = (,) <$> pConstructor <*> many pPayload
    pSum =
      keyword "sum"
        *> braces
          ( OpaqueSum
              <$> brackets (pValueType `sepBy` pComma)
              <*> many pVariant
          )

    pOpaque = keyword "opaque" $> OpaqueType <*> braces (many pValueType)

pOpaqueTypes :: Parser OpaqueTypes
pOpaqueTypes = keyword "types" $> OpaqueTypes <*> braces (many pOpaqueType)

pProg :: PR rep -> Parser (Prog rep)
pProg pr =
  Prog
    <$> (fromMaybe noTypes <$> optional pOpaqueTypes)
    <*> pStms pr
    <*> many (pFunDef pr)
  where
    noTypes = OpaqueTypes mempty

pSOAC :: PR rep -> Parser (SOAC.SOAC rep)
pSOAC pr =
  choice
    [ keyword "map" *> pScrema pMapForm,
      keyword "redomap" *> pScrema pRedomapForm,
      keyword "scanomap" *> pScrema pScanomapForm,
      keyword "screma" *> pScrema pScremaForm,
      keyword "vjp" *> pVJP,
      keyword "jvp" *> pJVP,
      pScatter,
      pHist,
      pStream
    ]
  where
    pScrema p =
      parens $
        SOAC.Screma
          <$> pSubExp
          <* pComma
          <*> braces (pVName `sepBy` pComma)
          <* pComma
          <*> p
    pScremaForm =
      SOAC.ScremaForm
        <$> braces (pScan pr `sepBy` pComma)
        <* pComma
        <*> braces (pReduce pr `sepBy` pComma)
        <* pComma
        <*> pLambda pr
    pRedomapForm =
      SOAC.ScremaForm mempty
        <$> braces (pReduce pr `sepBy` pComma)
        <* pComma
        <*> pLambda pr
    pScanomapForm =
      SOAC.ScremaForm
        <$> braces (pScan pr `sepBy` pComma)
        <* pComma
        <*> pure mempty
        <*> pLambda pr
    pMapForm =
      SOAC.ScremaForm mempty mempty <$> pLambda pr
    pScatter =
      keyword "scatter"
        *> parens
          ( SOAC.Scatter
              <$> pSubExp
              <* pComma
              <*> braces (pVName `sepBy` pComma)
              <* pComma
              <*> pLambda pr
              <*> many (pComma *> pDest)
          )
      where
        pDest =
          parens $ (,,) <$> pShape <* pComma <*> pInt <* pComma <*> pVName
    pHist =
      keyword "hist"
        *> parens
          ( SOAC.Hist
              <$> pSubExp
              <* pComma
              <*> braces (pVName `sepBy` pComma)
              <* pComma
              <*> braces (pHistOp `sepBy` pComma)
              <* pComma
              <*> pLambda pr
          )
      where
        pHistOp =
          SOAC.HistOp
            <$> pShape
            <* pComma
            <*> pSubExp
            <* pComma
            <*> braces (pVName `sepBy` pComma)
            <* pComma
            <*> braces (pSubExp `sepBy` pComma)
            <* pComma
            <*> pLambda pr
    pStream = keyword "streamSeq" *> pStreamSeq
    pStreamSeq =
      parens $
        SOAC.Stream
          <$> pSubExp
          <* pComma
          <*> braces (pVName `sepBy` pComma)
          <* pComma
          <*> braces (pSubExp `sepBy` pComma)
          <* pComma
          <*> pLambda pr
    pVJP =
      parens $
        SOAC.VJP
          <$> pLambda pr
          <* pComma
          <*> braces (pSubExp `sepBy` pComma)
          <* pComma
          <*> braces (pSubExp `sepBy` pComma)
    pJVP =
      parens $
        SOAC.JVP
          <$> pLambda pr
          <* pComma
          <*> braces (pSubExp `sepBy` pComma)
          <* pComma
          <*> braces (pSubExp `sepBy` pComma)

pSizeClass :: Parser GPU.SizeClass
pSizeClass =
  choice
    [ keyword "thread_block_size" $> GPU.SizeThreadBlock,
      keyword "grid_size" $> GPU.SizeGrid,
      keyword "tile_size" $> GPU.SizeTile,
      keyword "reg_tile_size" $> GPU.SizeRegTile,
      keyword "shared_memory" $> GPU.SizeSharedMemory,
      keyword "threshold"
        *> parens
          ( flip GPU.SizeThreshold
              <$> choice [Just <$> pInt64, "def" $> Nothing]
              <* pComma
              <*> pKernelPath
          ),
      keyword "bespoke"
        *> parens (GPU.SizeBespoke <$> pName <* pComma <*> pInt64)
    ]
  where
    pKernelPath = many pStep
    pStep =
      choice
        [ lexeme "!" $> (,) <*> pName <*> pure False,
          (,) <$> pName <*> pure True
        ]

pSizeOp :: Parser GPU.SizeOp
pSizeOp =
  choice
    [ keyword "get_size"
        *> parens (GPU.GetSize <$> pName <* pComma <*> pSizeClass),
      keyword "get_size_max"
        *> parens (GPU.GetSizeMax <$> pSizeClass),
      keyword "cmp_size"
        *> ( parens (GPU.CmpSizeLe <$> pName <* pComma <*> pSizeClass)
               <*> (lexeme "<=" *> pSubExp)
           ),
      keyword "calc_num_tblocks"
        *> parens
          ( GPU.CalcNumBlocks
              <$> pSubExp
              <* pComma
              <*> pName
              <* pComma
              <*> pSubExp
          )
    ]

pSegSpace :: Parser SegOp.SegSpace
pSegSpace =
  flip SegOp.SegSpace
    <$> parens (pDim `sepBy` pComma)
    <*> parens (lexeme "~" *> pVName)
  where
    pDim = (,) <$> pVName <* lexeme "<" <*> pSubExp

pKernelResult :: Parser SegOp.KernelResult
pKernelResult = do
  cs <- pCerts
  choice
    [ keyword "returns"
        $> SegOp.Returns
        <*> choice
          [ keyword "(manifest)" $> SegOp.ResultNoSimplify,
            keyword "(private)" $> SegOp.ResultPrivate,
            pure SegOp.ResultMaySimplify
          ]
        <*> pure cs
        <*> pSubExp,
      try $
        SegOp.WriteReturns cs
          <$> pVName
          <* keyword "with"
          <*> parens (pWrite `sepBy` pComma),
      try "tile"
        *> parens (SegOp.TileReturns cs <$> (pTile `sepBy` pComma))
        <*> pVName,
      try "blkreg_tile"
        *> parens (SegOp.RegTileReturns cs <$> (pRegTile `sepBy` pComma))
        <*> pVName
    ]
  where
    pTile = (,) <$> pSubExp <* pSlash <*> pSubExp
    pRegTile = do
      dim <- pSubExp <* pSlash
      parens $ do
        blk_tile <- pSubExp <* pAsterisk
        reg_tile <- pSubExp
        pure (dim, blk_tile, reg_tile)
    pWrite = (,) <$> pSlice <* pEqual <*> pSubExp

pKernelBody :: PR rep -> Parser (SegOp.KernelBody rep)
pKernelBody pr =
  SegOp.KernelBody (pBodyDec pr)
    <$> pStms pr
    <* keyword "return"
    <*> braces (pKernelResult `sepBy` pComma)

pSegOp :: PR rep -> Parser lvl -> Parser (SegOp.SegOp lvl rep)
pSegOp pr pLvl =
  choice
    [ keyword "segmap" *> pSegMap,
      keyword "segred" *> pSegRed,
      keyword "segscan" *> pSegScan,
      keyword "seghist" *> pSegHist
    ]
  where
    pSegMap =
      SegOp.SegMap
        <$> pLvl
        <*> pSegSpace
        <* pColon
        <*> pTypes
        <*> braces (pKernelBody pr)
    pSegOp' f p =
      f
        <$> pLvl
        <*> pSegSpace
        <*> parens (p `sepBy` pComma)
        <* pColon
        <*> pTypes
        <*> braces (pKernelBody pr)
    pSegBinOp = do
      nes <- braces (pSubExp `sepBy` pComma) <* pComma
      shape <- pShape <* pComma
      comm <- pComm
      lam <- pLambda pr
      pure $ SegOp.SegBinOp comm lam nes shape
    pHistOp =
      SegOp.HistOp
        <$> pShape
        <* pComma
        <*> pSubExp
        <* pComma
        <*> braces (pVName `sepBy` pComma)
        <* pComma
        <*> braces (pSubExp `sepBy` pComma)
        <* pComma
        <*> pShape
        <* pComma
        <*> pLambda pr
    pSegRed = pSegOp' SegOp.SegRed pSegBinOp
    pSegScan = pSegOp' SegOp.SegScan pSegBinOp
    pSegHist = pSegOp' SegOp.SegHist pHistOp

pSegLevel :: Parser GPU.SegLevel
pSegLevel =
  parens . choice $
    [ "thread"
        $> GPU.SegThread
        <* pSemi
        <*> pSegVirt
        <* pSemi
        <*> optional pKernelGrid,
      "block"
        $> GPU.SegBlock
        <* pSemi
        <*> pSegVirt
        <* pSemi
        <*> optional pKernelGrid,
      "inblock" $> GPU.SegThreadInBlock <* pSemi <*> pSegVirt
    ]
  where
    pSegVirt =
      choice
        [ choice
            [ keyword "full"
                $> GPU.SegNoVirtFull
                <*> (GPU.SegSeqDims <$> brackets (pInt `sepBy` pComma)),
              keyword "virtualise" $> GPU.SegVirt
            ],
          pure GPU.SegNoVirt
        ]
    pKernelGrid =
      GPU.KernelGrid
        <$> (lexeme "grid=" $> GPU.Count <*> pSubExp <* pSemi)
        <*> (lexeme "blocksize=" $> GPU.Count <*> pSubExp)

pHostOp :: PR rep -> Parser (op rep) -> Parser (GPU.HostOp op rep)
pHostOp pr pOther =
  choice
    [ GPU.SegOp <$> pSegOp pr pSegLevel,
      GPU.SizeOp <$> pSizeOp,
      GPU.OtherOp <$> pOther,
      keyword "gpu" $> GPU.GPUBody <*> (pColon *> pTypes) <*> braces (pBody pr)
    ]

pMCOp :: PR rep -> Parser (op rep) -> Parser (MC.MCOp op rep)
pMCOp pr pOther =
  choice
    [ MC.ParOp . Just
        <$> (keyword "par" *> braces pMCSegOp)
        <*> (keyword "seq" *> braces pMCSegOp),
      MC.ParOp Nothing <$> pMCSegOp,
      MC.OtherOp <$> pOther
    ]
  where
    pMCSegOp = pSegOp pr (void $ lexeme "()")

pLMADBase :: Parser a -> Parser (LMAD.LMAD a)
pLMADBase pNum = braces $ do
  offset <- pLab "offset" pNum <* pSemi
  strides <- pLab "strides" $ brackets (pNum `sepBy` pComma) <* pSemi
  shape <- pLab "shape" $ brackets (pNum `sepBy` pComma)
  pure $ LMAD.LMAD offset $ zipWith LMAD.LMADDim strides shape
  where
    pLab s m = keyword s *> pColon *> m

pPrimExpLeaf :: Parser VName
pPrimExpLeaf = pVName

pExtPrimExpLeaf :: Parser (Ext VName)
pExtPrimExpLeaf = pExt pVName

pLMAD :: Parser LMAD
pLMAD = pLMADBase $ isInt64 <$> pPrimExp int64 pPrimExpLeaf

pExtLMAD :: Parser ExtLMAD
pExtLMAD = pLMADBase $ isInt64 <$> pPrimExp int64 pExtPrimExpLeaf

pMemInfo :: Parser d -> Parser u -> Parser ret -> Parser (MemInfo d u ret)
pMemInfo pd pu pret =
  choice
    [ MemPrim <$> pPrimType,
      keyword "mem" $> MemMem <*> choice [pSpace, pure DefaultSpace],
      pArrayOrAcc
    ]
  where
    pArrayOrAcc = do
      u <- pu
      shape <- Shape <$> many (brackets pd)
      choice [pArray u shape, pAcc u]
    pArray u shape = do
      pt <- pPrimType
      MemArray pt shape u <$> (lexeme "@" *> pret)
    pAcc u =
      keyword "acc"
        *> parens
          ( MemAcc
              <$> pVName
              <* pComma
              <*> pShape
              <* pComma
              <*> pTypes
              <*> pure u
          )

pSpace :: Parser Space
pSpace =
  lexeme "@"
    *> choice
      [ Space . nameToString <$> pName,
        ScalarSpace <$> (shapeDims <$> pShape) <*> pPrimType
      ]

pMemBind :: Parser MemBind
pMemBind = ArrayIn <$> pVName <* lexeme "->" <*> pLMAD

pMemReturn :: Parser MemReturn
pMemReturn =
  choice
    [ ReturnsInBlock <$> pVName <* lexeme "->" <*> pExtLMAD,
      do
        i <- "?" *> pInt
        space <- choice [pSpace, pure DefaultSpace] <* lexeme "->"
        ReturnsNewBlock space i <$> pExtLMAD
    ]

pRetTypeMem :: Parser RetTypeMem
pRetTypeMem = pMemInfo pExtSize pUniqueness pMemReturn

pBranchTypeMem :: Parser BranchTypeMem
pBranchTypeMem = pMemInfo pExtSize (pure NoUniqueness) pMemReturn

pFParamMem :: Parser FParamMem
pFParamMem = pMemInfo pSubExp pUniqueness pMemBind

pLParamMem :: Parser LParamMem
pLParamMem = pMemInfo pSubExp (pure NoUniqueness) pMemBind

pLetDecMem :: Parser LetDecMem
pLetDecMem = pMemInfo pSubExp (pure NoUniqueness) pMemBind

pMemOp :: Parser (inner rep) -> Parser (MemOp inner rep)
pMemOp pInner =
  choice
    [ keyword "alloc"
        *> parens
          (Alloc <$> pSubExp <*> choice [pComma *> pSpace, pure DefaultSpace]),
      Inner <$> pInner
    ]

prSOACS :: PR SOACS
prSOACS =
  PR pDeclExtType pExtType pDeclType pType pType (pSOAC prSOACS) () ()

prSeq :: PR Seq
prSeq =
  PR pDeclExtType pExtType pDeclType pType pType empty () ()

prSeqMem :: PR SeqMem
prSeqMem =
  PR pRetTypeMem pBranchTypeMem pFParamMem pLParamMem pLetDecMem op () ()
  where
    op = pMemOp empty

prGPU :: PR GPU
prGPU =
  PR pDeclExtType pExtType pDeclType pType pType op () ()
  where
    op = pHostOp prGPU (pSOAC prGPU)

prGPUMem :: PR GPUMem
prGPUMem =
  PR pRetTypeMem pBranchTypeMem pFParamMem pLParamMem pLetDecMem op () ()
  where
    op = pMemOp $ pHostOp prGPUMem empty

prMC :: PR MC
prMC =
  PR pDeclExtType pExtType pDeclType pType pType op () ()
  where
    op = pMCOp prMC (pSOAC prMC)

prMCMem :: PR MCMem
prMCMem =
  PR pRetTypeMem pBranchTypeMem pFParamMem pLParamMem pLetDecMem op () ()
  where
    op = pMemOp $ pMCOp prMCMem empty

parseFull :: Parser a -> FilePath -> T.Text -> Either T.Text a
parseFull p fname s =
  either (Left . T.pack . errorBundlePretty) Right $
    parse (whitespace *> p <* eof) fname s

parseRep :: PR rep -> FilePath -> T.Text -> Either T.Text (Prog rep)
parseRep = parseFull . pProg

parseSOACS :: FilePath -> T.Text -> Either T.Text (Prog SOACS)
parseSOACS = parseRep prSOACS

parseSeq :: FilePath -> T.Text -> Either T.Text (Prog Seq)
parseSeq = parseRep prSeq

parseSeqMem :: FilePath -> T.Text -> Either T.Text (Prog SeqMem)
parseSeqMem = parseRep prSeqMem

parseGPU :: FilePath -> T.Text -> Either T.Text (Prog GPU)
parseGPU = parseRep prGPU

parseGPUMem :: FilePath -> T.Text -> Either T.Text (Prog GPUMem)
parseGPUMem = parseRep prGPUMem

parseMC :: FilePath -> T.Text -> Either T.Text (Prog MC)
parseMC = parseRep prMC

parseMCMem :: FilePath -> T.Text -> Either T.Text (Prog MCMem)
parseMCMem = parseRep prMCMem

--- Fragment parsers

parseType :: FilePath -> T.Text -> Either T.Text Type
parseType = parseFull pType

parseDeclExtType :: FilePath -> T.Text -> Either T.Text DeclExtType
parseDeclExtType = parseFull pDeclExtType

parseDeclType :: FilePath -> T.Text -> Either T.Text DeclType
parseDeclType = parseFull pDeclType

parseVName :: FilePath -> T.Text -> Either T.Text VName
parseVName = parseFull pVName

parseSubExp :: FilePath -> T.Text -> Either T.Text SubExp
parseSubExp = parseFull pSubExp

parseSubExpRes :: FilePath -> T.Text -> Either T.Text SubExpRes
parseSubExpRes = parseFull pSubExpRes

parseBodyGPU :: FilePath -> T.Text -> Either T.Text (Body GPU)
parseBodyGPU = parseFull $ pBody prGPU

parseStmGPU :: FilePath -> T.Text -> Either T.Text (Stm GPU)
parseStmGPU = parseFull $ pStm prGPU

parseBodyMC :: FilePath -> T.Text -> Either T.Text (Body MC)
parseBodyMC = parseFull $ pBody prMC

parseStmMC :: FilePath -> T.Text -> Either T.Text (Stm MC)
parseStmMC = parseFull $ pStm prMC
