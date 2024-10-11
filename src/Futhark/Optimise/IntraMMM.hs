module Futhark.Optimise.IntraMMM (intraMMM, intraMMMMemFixup) where

-- TODO: add specific imports, clean up

import Control.Lens.Lens
import Control.Monad (liftM, (>=>))
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Foldable (find, fold, toList)
import Data.Maybe (catMaybes)
import Debug.Trace
import Futhark.CodeGen.Backends.GenericC (funName)
import Futhark.IR.GPU
import Futhark.IR.Syntax.Core
import Futhark.IR.GPU.Op
import Futhark.IR.SegOp
import Futhark.IR.GPU.Simplify (simplifyGPU)
import Futhark.IR.GPUMem
--import Futhark.IR.Mem
import Futhark.IR.Pretty
import Futhark.IR.Prop.Scope (Scope)
import Futhark.IR.Syntax
import Futhark.IR.Traversals
--import Futhark.IR.Mem.LMAD as LMAD
import Futhark.Optimise.Simplify.Rep
import Futhark.Builder
import qualified Futhark.Analysis.SymbolTable as ST
import Data.List (partition, elem, intersect, lookup)
import Data.Set (fromList, member, intersection, difference)
import Control.Monad
import Futhark.Optimise.TileLoops.Shared
import Futhark.Construct
import Futhark.Builder.Class
import Prelude hiding (lookup)
import Futhark.Pass
  ( Pass (..),
    PassM,
    intraproceduralTransformation,
    intraproceduralTransformationWithConsts,
  )
import Data.Loc (Loc(NoLoc), SrcLoc (SrcLoc))

traceHelper :: (Show a) => a -> a
traceHelper x = trace (show x ++ "\n") x

intraMMM :: Pass GPU GPU
intraMMM =
  Pass
    "tensor-mma"
    "Extracts NVIDIA tensor core MMA operations"
    transformProg

gemmName :: Name
gemmName = "gemm_123456"


-- TODO: emit in monad when needed?
--gemmFun :: FunDef GPU
-- TODO: make more flexible in sizes and types
mkGemmFun :: FParamInfo GPU -> FParamInfo GPU -> FParamInfo GPU -> [(RetType GPU, RetAls)] -> PassM (FunDef GPU)
mkGemmFun typeA typeB typeCin typeCout = do
  aParam <- newParam "A" typeA
  bParam <- newParam "B" typeB
  cParam <- newParam "C" typeCin
  pure $
    FunDef Nothing mempty gemmName
    typeCout
    [aParam, bParam, cParam]
    $ resultBody $ [Var $ paramName cParam]


transformProg :: Prog GPU -> PassM (Prog GPU)
transformProg (Prog opaqueTypes consts funs) = do
  transformedFuns <- mapM transformFunDef funs

  let typeA = Array (FloatType Float16) (Shape [mkInt64Const 16, mkInt64Const 16]) Nonunique
  let typeB = Array (FloatType Float16) (Shape [mkInt64Const 16, mkInt64Const 16]) Nonunique
  let typeCin = Array (FloatType Float16) (Shape [mkInt64Const 8]) Unique
  let typeCout = [(Array (FloatType Float16) (Shape [Free $ mkInt64Const 8]) Unique, RetAls [] [])]

  gemmFun <- mkGemmFun typeA typeB typeCin typeCout
  pure $ Prog opaqueTypes consts (gemmFun : transformedFuns)

transformFunDef :: FunDef GPU -> PassM (FunDef GPU)
transformFunDef (FunDef entry attrs name retType params body) = do
  newBody <- fmap fst $ modifyNameSource $ runState $ runWriterT $ transformBody body
  pure $ FunDef entry attrs name retType params newBody

--type IntraMMMMonad = WriterT [Int] (Builder GPU)
--newtype IntraMMMMonad a = IntraMMMMonad (WriterT [Int] PassM a)
--type IntraMMMMonad = WriterT [Int] PassM
type IntraMMMMonad = WriterT [Int] (State VNameSource)


data MMAMatch = MMAMatch
  {
    kernelBodyMatch :: KernelBodyMatch,
    ne :: SubExp,
    sizeM :: Int,
    sizeN :: Int,
    sizeK :: Int
  }
  deriving (Show, Eq, Ord)

data KernelBodyMatch = KernelBodyMatch
  {
--  TODO: add types
    innerDimsA :: [VName],
    innerDimsB :: [VName],
    outerDimsA :: [VName],
    outerDimsB :: [VName],
    arrA :: VName,
    arrB :: VName,
    m :: VName,
    n :: VName,
    k :: VName,
    typeA :: PrimType,
    typeB :: PrimType,
    typeC :: PrimType
  }
  deriving (Show, Eq, Ord)


-- TODO: use PassM below?
transformStms :: Stms GPU -> IntraMMMMonad (Stms GPU)
transformStms stms = do
  join <$> mapM transformStm stms

transformStm :: Stm GPU -> IntraMMMMonad (Stms GPU)
-- TODO: pass name of let to builder
transformStm stm@(Let (Pat [PatElem vName _]) aux e)
--    TODO: allow more sizes?
  | Just match@(MMAMatch kernelBodyMatch ne 16 16 16) <- expMatch e = do
-- TODO: how to determine the block size?
    let blockSize = 32
    tell [blockSize]
--    TODO: build MMA
    lift $ runBuilderT_ (buildMMM vName blockSize match) mempty
  | otherwise = transformStmDefault stm
transformStm stm = transformStmDefault stm


transformStmDefault :: Stm GPU -> IntraMMMMonad (Stms GPU)
transformStmDefault (Let pat aux e) = do
  e' <- transformExp e
  pure $ oneStm $ Let pat aux e'

stmToPat :: Stm GPU -> Pat (LetDec GPU)
stmToPat (Let pat _ _) = pat

--type MMMBuilder a = BuilderT GPU (IntraMMMMonad) a


buildMMM :: VName -> Int -> MMAMatch -> Builder GPU ()
buildMMM vName blockSize (MMAMatch kernelBodyMatch ne sizeM sizeN sizeK) = do
  let cValsPerThread = mkInt64Const $ sizeM * sizeN `div` blockSize
--  TODO: get as input instead?
  let blockSizeSubExp = mkInt64Const blockSize
--  TODO: do we need to init regs when used only once?
-- TODO: use SegNoVirtFull instead of loop and avoid setting the block size?
  cRegs_list <- segMap1D "cRegs" (SegThreadInBlock SegNoVirt) ResultPrivate blockSizeSubExp $ \_ -> do
    cScratch <- letExp "cScratch" $ BasicOp $ Scratch (typeC kernelBodyMatch) [cValsPerThread]
    cLoop <- forLoop cValsPerThread [cScratch] $ \i [cMerge] -> do
      cZeroed <- update "cZeroed" cMerge [i] ne
--      TODO: use pure instead of resultBodyM?
      resultBodyM [Var cZeroed]
    pure [varRes cLoop]
  let [cRegs] = cRegs_list
--  TODO: check if need to transpose, ensure not hoisted out of block segmap
  aScratch <- letExp "aScratch" $ BasicOp $ Scratch (typeA kernelBodyMatch) [mkInt64Const sizeM, mkInt64Const sizeK]
  bScratch <- letExp "bScratch" $ BasicOp $ Scratch (typeB kernelBodyMatch) [mkInt64Const sizeK, mkInt64Const sizeN]
--  TODO: copy to shared
--  TODO: is ObservePrim correct?
  blockMMAres_list <- segMap1D "blockMMAres" (SegThreadInBlock SegNoVirt) ResultPrivate blockSizeSubExp $ \thread_idx -> do
    threadCregs <- index "threadCregs" cRegs [thread_idx]
    threadMMAres <- letExp "threadMMAres" $ Apply gemmName [(Var aScratch, ObservePrim), (Var bScratch, ObservePrim), (Var threadCregs, Consume)] [(Array (typeC kernelBodyMatch) (Shape [Free cValsPerThread]) Unique, RetAls [] [])] (Safe, SrcLoc NoLoc, [])
    pure [varRes threadMMAres]
  let [blockMMAres] = blockMMAres_list
  --  TODO: copy res to cScratch?
--  cScratch <- letExp "cScratch" $ BasicOp $ Scratch (typeC kernelBodyMatch) [mkInt64Const sizeM, mkInt64Const sizeN]
  cReshaped <- letExp "cReshaped" $ BasicOp $ Reshape ReshapeArbitrary (Shape [mkInt64Const sizeM, mkInt64Const sizeN]) blockMMAres
  letBindNames [vName] $ BasicOp $ SubExp $ Var cReshaped
--  letBindNames [vName] $


expMatch :: Exp GPU -> Maybe MMAMatch
expMatch (Op (SegOp sOp)) = segOpMatch sOp
expMatch _ = Nothing

transformExp :: Exp GPU -> IntraMMMMonad (Exp GPU)
-- transformExp (BasicOp op) = BasicOp op
-- transformExp (Apply name args rets info) = Apply name args rets info
transformExp (Match subExps cases body matchDec) = Match subExps <$> mapM transformCase cases <*> transformBody body <*> pure matchDec
transformExp (Loop params form body) = Loop params form <$> transformBody body
transformExp (Op op) = Op <$> transformOp op
transformExp e = pure e

transformCase :: Case (Body GPU) -> IntraMMMMonad (Case (Body GPU))
transformCase (Case pat body) = Case pat <$> transformBody body

transformBody :: Body GPU -> IntraMMMMonad (Body GPU)
transformBody (Body dec stms res) = Body dec <$> transformStms stms <*> pure res

transformOp :: Op GPU -> IntraMMMMonad (Op GPU)
transformOp (SegOp sOp) = SegOp <$> transformSegOp sOp
-- TODO: handle these separately?
transformOp op = pure op

transformSegOp :: SegOp SegLevel GPU -> IntraMMMMonad (SegOp SegLevel GPU)
-- TODO: avoid changing the block size, fix other seg inblock, or avoid transformation in these cases
-- TODO: match others?
transformSegOp s@(SegMap level@(SegBlock virt (Just (KernelGrid (Count numBlocks) (Count blockSize)))) space ts body) = lift $ do
  (newBody, newBlockSizes) <- runWriterT $ transformKernelBody body
  case newBlockSizes of
--  TODO: handle more block sizes?
    [newBlockSize] ->
      pure $ SegMap (SegBlock virt (Just (KernelGrid (Count numBlocks) (Count $ mkInt64Const newBlockSize)))) space ts newBody
    _ -> pure $ SegMap level space ts newBody
transformSegOp s = transformSegOpDefault s

mkInt64Const :: Int -> SubExp
mkInt64Const = Constant . IntValue . intValue Int64

segOpMatch :: SegOp SegLevel GPU -> Maybe MMAMatch
segOpMatch s@(SegRed (SegThreadInBlock _) space segBinOps _ body) | Just ne <- segBinOpsMatch segBinOps = do
  let (dimVars, segDims) = unzip $ unSegSpace space
  let freeVars = freeIn s
  bodyMatch <- matchesKernelBody dimVars freeVars body
  constSegDims <- mapM getConstantValue segDims
  case constSegDims of
--  TODO: also match bodyMatch? allow more sizes?
    [16, 16, 16] ->
--    TODO: pass ts? allow more segDims
      Just (MMAMatch bodyMatch ne 16 16 16)
    _ -> Nothing
-- TODO: extract A, B, types, shape, layout, lookup m, n, k shapes
segOpMatch _ = Nothing


getConstantValue :: SubExp -> Maybe Int
getConstantValue (Constant (IntValue v)) = Just $ valueIntegral v
getConstantValue _ = Nothing

-- TODO: return maybe something?
matchesKernelBody :: [VName] -> Names -> KernelBody GPU -> Maybe KernelBodyMatch
-- TODO: support more than 3 dimensions?
matchesKernelBody dimVars@[dimVar1, dimVar2, dimVar3] freeVars (KernelBody _ stms [Returns _ _ (Var res)]) = do
  let sTable = ST.insertStms (informStms stms) mempty
--  TODO: rename to use A, B, C?
  (resExp, _) <- ST.lookupExp res sTable
  (mulArg1, mulArg2) <- matchesMul $ removeExpWisdom resExp
  (mulArg1Exp, _) <- ST.lookupExp mulArg1 sTable
  (mulArg2Exp, _) <- ST.lookupExp mulArg2 sTable
  (arr1, slice1) <- matchesMulArg $ removeExpWisdom mulArg1Exp
  (arr2, slice2) <- matchesMulArg $ removeExpWisdom mulArg2Exp
--  arr1Type <- ST.lookupType arr1 sTable
--  arr2Type <- ST.lookupType arr2 sTable
  resType <- ST.lookupType res sTable
  slice1' <- mapM getIndexVar $ unSlice slice1
  slice2' <- mapM getIndexVar $ unSlice slice2
--  TODO: check that all outerDims are free variables?
  let (innerDims1, outerDims1) = partition (`elem` dimVars) slice1'
  let (innerDims2, outerDims2) = partition (`elem` dimVars) slice2'
  let commonDims = innerDims1 `intersect` innerDims2
  let separateDims1 = toList $ fromList innerDims1 `difference` fromList innerDims2
  let separateDims2 = toList $ fromList innerDims2 `difference` fromList innerDims1
--  case (separateDims1, separateDims2, commonDims, arr1Type, arr2Type, resType) of
--    ([m], [n], [k], Array type1 _ _, Array type2 _ _, Prim typeRes) | k == dimVar3 && all (`nameIn` freeVars) (outerDims1<>outerDims2) ->
--      Just KernelBodyMatch{innerDims1, innerDims2, outerDims1, outerDims2, arr1, arr2, m, n, k, type1, type2, typeRes}
  case (separateDims1, separateDims2, commonDims, resType) of
--  TODO: check which is A and which is B?
    ([m], [n], [k], Prim resTypePrim) | k == dimVar3 && all (`nameIn` freeVars) (outerDims1<>outerDims2) ->
      Just (KernelBodyMatch innerDims1 innerDims2 outerDims1 outerDims2 arr1 arr2 m n k resTypePrim resTypePrim resTypePrim)
    _ -> Nothing
matchesKernelBody _ _ _ = Nothing

getIndexVar :: DimIndex SubExp -> Maybe VName
getIndexVar (DimFix (Var v)) = Just v
getIndexVar _ = Nothing

matchesMul :: Exp GPU -> Maybe (VName, VName)
matchesMul (BasicOp (BinOp (FMul _) (Var arg1) (Var arg2))) = Just (arg1, arg2)
matchesMul _ = Nothing

matchesMulArg :: Exp GPU -> Maybe (VName, Slice SubExp)
matchesMulArg (BasicOp (Index v s)) = Just (v, s)
matchesMulArg _ = Nothing

-- TODO: also return binop?
segBinOpsMatch :: [SegBinOp GPU] -> Maybe SubExp
segBinOpsMatch [SegBinOp Commutative lambda nes s] | lambdaMatch lambda = nesMatch nes
segBinOpsMatch _ = Nothing

lambdaMatch :: Lambda GPU -> Bool
lambdaMatch (Lambda [Param _ arg1 _, Param _ arg2 _] _ body) = lambdaBodyMatch arg1 arg2 body
lambdaMatch _ = False

lambdaBodyMatch :: VName -> VName -> Body GPU -> Bool
lambdaBodyMatch arg1 arg2 (Body _ stms [SubExpRes _ (Var v)]) = any (lambdaStmMatch arg1 arg2  v) stms
lambdaBodyMatch _ _ _ = False

lambdaStmMatch :: VName -> VName -> VName -> Stm GPU -> Bool
lambdaStmMatch arg1 arg2 v (Let (Pat [PatElem v' _]) _ (BasicOp (BinOp (FAdd _) (Var arg1') (Var arg2')))) =
  v == v' && arg1 == arg1' && arg2 == arg2'
lambdaStmMatch arg1 arg2 v (Let (Pat [PatElem v' _]) _ (BasicOp (BinOp (Add _ _) (Var arg1') (Var arg2')))) =
  v == v' && arg1 == arg1' && arg2 == arg2'
lambdaStmMatch _ _ _ _ = False

nesMatch :: [SubExp] -> Maybe SubExp
nesMatch [s@(Constant v)] | zeroIsh v = Just s
nesMatch _ = Nothing

transformSegOpDefault :: SegOp SegLevel GPU -> IntraMMMMonad (SegOp SegLevel GPU)
transformSegOpDefault (SegMap level space ts body) = SegMap level space ts <$> transformKernelBody body
transformSegOpDefault (SegRed level space ops ts body) = SegRed level space ops ts <$> transformKernelBody body
transformSegOpDefault (SegScan level space ops ts body) = SegScan level space ops ts <$> transformKernelBody body
transformSegOpDefault (SegHist level space ops hist body) = SegHist level space ops hist <$> transformKernelBody body

transformKernelBody :: KernelBody GPU -> IntraMMMMonad (KernelBody GPU)
transformKernelBody (KernelBody desc stms res) = KernelBody desc <$> transformStms stms <*> pure res








-- TODO: avoid code duplication with above?
intraMMMMemFixup :: Pass GPUMem GPUMem
intraMMMMemFixup =
  -- TODO: don't use intraproceduralTransformation
  Pass
    "mma-fixup"
    "Extracts NVIDIA tensor core MMA operations"
    $ intraproceduralTransformationWithConsts pure fixFuns

-- \$ intraproceduralTransformation fixStmtsWithScope

-- TODO: use PassM below?
type FixEnv = Scope GPUMem

-- TODO: use map?
type FixState = [(VName, VName)]

-- type FixMonad a = RWST FixEnv () FixState PassM a
type FixMonad a = RWST FixEnv () FixState PassM a

-- TODO: With LMAD we could do swizzle

fixFuns :: Stms GPUMem -> FunDef GPUMem -> PassM (FunDef GPUMem)
fixFuns consts fun
  | funDefName fun == gemmName = pure $ fixGemmFun fun
  | otherwise = do
      let initScope = scopeOf consts
      let body = funDefBody fun
      stms' <- fixStmtsWithScope initScope . bodyStms $ body
      pure $ fun {funDefBody = body {bodyStms = stms'}}

fixGemmFun :: FunDef GPUMem -> FunDef GPUMem
fixGemmFun gemm =
  gemm
    {
      funDefParams = fixParams $ funDefParams gemm,
      funDefRetType = fixRetType $ funDefRetType gemm
    }

fixParams :: [FParam GPUMem] -> [FParam GPUMem]
fixParams [
    Param attrs1 vName1 (MemMem (Space "device")),
    Param attrs2 vName2 (MemMem (Space "device")),
    Param attrs3 vName3 (MemMem (Space "device")),
    p4,
    p5,
    Param attrs6 vName6 (MemArray t6 shp6 u6 (ArrayIn mName6 lmad6))
  ] =
  let space = ScalarSpace (shapeDims shp6) t6 in
  [
      Param attrs1 vName1 (MemMem (Space "shared")),
      Param attrs2 vName2 (MemMem (Space "shared")),
      Param attrs3 vName3 (MemMem space),
      p4,
      p5,
      Param attrs6 vName6 (MemArray t6 shp6 u6 (ArrayIn mName6 lmad6))
    ]
fixParams params = params

fixRetType :: [(RetType GPUMem, RetAls)] -> [(RetType GPUMem, RetAls)]
fixRetType [(MemMem (Space "device"), als1), (MemArray t shp u (ReturnsNewBlock (Space "device") n lmad), als2)] =
  let space = ScalarSpace (fmap extToSubExp (shapeDims shp)) t in
--  TODO: check if ReturnsInBlock is preferred
  [(MemMem space, als1), (MemArray t shp u (ReturnsNewBlock space n lmad), als2)]
fixRetType rets = rets

extToSubExp :: ExtSize -> SubExp
extToSubExp (Ext n) = mkInt64Const n
extToSubExp (Free se) = se

-- TODO: use scope, genearate scope?
fixStmtsWithScope :: Scope GPUMem -> Stms GPUMem -> PassM (Stms GPUMem)
fixStmtsWithScope scope stms = do
  (res, _, _) <- runRWST (fixStmts stms) scope []
  pure res

fixStmts :: Stms GPUMem -> FixMonad (Stms GPUMem)
-- fixStmts stms =
--    mapM fixStmt stms
--    TODO: use fold?
fixStmts stms =
  case stmsHead stms of
    Nothing -> pure mempty
    Just (stm, stms') -> do
      stm' <- fixStmt stm
      stms'' <- inScopeOf stm $ fixStmts stms'
      pure $ oneStm stm' <> stms''

fixStmt :: Stm GPUMem -> FixMonad (Stm GPUMem)
fixStmt stm@(Let (Pat [PatElem resName (MemArray _ _ _ (ArrayIn resMem _))]) _ (BasicOp (Manifest _ inputName))) = do
  info <- lookupInfo inputName
  case info of
    --    TODO: match more cases
    LetName (MemArray _ _ _ (ArrayIn inputMem _)) -> do
      modify ([(resName, inputName), (resMem, inputMem)] <>)
--      TODO: remove manifests?
      defaultFixStm stm
    _ -> defaultFixStm stm
fixStmt (
  Let (Pat [
    PatElem vName1 (MemMem (Space "device")),
    PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
  ])
  aux
  (Apply "gemm_123456" args rets info)) = do
  let space = ScalarSpace (shapeDims shp2) t2
  let newRets = fixRetType rets
  newArgs <- mapM replaceArg args
  pure $
    Let (Pat [
      PatElem vName1 (MemMem space),
      PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
    ])
    aux
    (Apply "gemm_123456" newArgs newRets info)



--  defaultFixStm (Let (Pat patElems) aux (Apply "gemm_123456" args rets info)) = do
--    -- TODO: Should be based on the return value of the function
--    -- For now they match because the return value is hard coded
--    let rets' = map retInRegs rets
--    let patElems' = map letInRegs patElems
--    args' <- mapM replaceArg args
--    pure $ Let (Pat patElems') aux $ Apply "gemm_123456" args' rets' info
--    where
--      -- Put the let bound results in registers
--      letInRegs :: PatElem (LetDec GPUMem)-> PatElem (LetDec GPUMem)
--      letInRegs (PatElem name (MemMem (Space "device"))) =
--        PatElem name $ MemMem . defScalarSpace $ FloatType Float32
--      letInRegs patElem = patElem

fixStmt stm = defaultFixStm stm

defaultFixStm :: Stm GPUMem -> FixMonad (Stm GPUMem)
defaultFixStm (Let pat aux e) = Let pat aux <$> fixExp e

-- TODO: add stuff to scope in other places
fixExp :: Exp GPUMem -> FixMonad (Exp GPUMem)
--fixExp (Apply "gemm_123456" args rets info) = do
--  let rets' = map retInRegs rets
--  newArgs <- mapM replaceArg args
--  pure $ Apply "gemm_123456" newArgs rets' info
fixExp (Match subExps cases body matchDec) = Match subExps <$> mapM fixCase cases <*> fixBody body <*> pure matchDec
fixExp (Loop params form body) = Loop params form <$> fixBody body
fixExp (Op op) = Op <$> fixOp op
-- TODO: match more?
-- fixExp (BasicOp op) = BasicOp op
-- fixExp (Apply name args rets info) = Apply name args rets info
fixExp e = pure e

replaceArg :: (SubExp, Diet) -> FixMonad (SubExp, Diet)
replaceArg (Var v, d) = do
  manifestMap <- get
  case lookup v manifestMap of
    Just v' ->
      pure (Var v', d)
    Nothing ->
      pure (Var v, d)
replaceArg a = pure a

fixCase :: Case (Body GPUMem) -> FixMonad (Case (Body GPUMem))
fixCase (Case pat body) = Case pat <$> fixBody body

fixBody :: Body GPUMem -> FixMonad (Body GPUMem)
fixBody (Body dec stms res) = Body dec <$> fixStmts stms <*> pure res

fixOp :: Op GPUMem -> FixMonad (Op GPUMem)
fixOp (Inner hostOp) = Inner <$> fixHostOp hostOp
-- TODO: recurse here?
fixOp op = pure op

fixHostOp :: HostOp NoOp GPUMem -> FixMonad (HostOp NoOp GPUMem)
fixHostOp (SegOp op) = SegOp <$> fixSegOp op
-- TODO: run recurisvely?
fixHostOp op = pure op

fixSegOp :: SegOp SegLevel GPUMem -> FixMonad (SegOp SegLevel GPUMem)
-- TODO: only look in inblock?
-- fixSegOp (SegMap (SegThreadInBlock virt) space ts body) = SegMap (SegThreadInBlock virt) space ts (fixKernelBody body)
fixSegOp (SegMap level space ts body) = SegMap level space ts <$> fixKernelBody body
fixSegOp (SegRed level space ops ts body) = SegRed level space ops ts <$> fixKernelBody body
fixSegOp (SegScan level space ops ts body) = SegScan level space ops ts <$> fixKernelBody body
fixSegOp (SegHist level space ops hist body) = SegHist level space ops hist <$> fixKernelBody body

fixKernelBody :: KernelBody GPUMem -> FixMonad (KernelBody GPUMem)
fixKernelBody (KernelBody desc stms res) = KernelBody desc <$> fixStmts stms <*> pure res
