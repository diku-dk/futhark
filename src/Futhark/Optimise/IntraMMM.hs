module Futhark.Optimise.IntraMMM (intraMMM, intraMMMMemFixup) where

-- TODO: add specific imports, clean up

import Control.Lens.Lens
import Control.Monad.Identity
import Control.Monad.RWS.Strict
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
import Futhark.Optimise.IntraMMM.Utils
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


--type IntraMMMMonadEnv = Maybe (VName, SubExp)
-- TODO: avoid blockinfo?
data IntraMMMMonadEnv = IntraMMMMonadEnv {scope :: Scope GPU, blockInfo :: Maybe (VName, SubExp)}

type IntraMMMMonad = RWS IntraMMMMonadEnv [Int] VNameSource

-- TODO: use newtype above to fix warning
instance
  HasScope GPU IntraMMMMonad
  where
  askScope = asks scope


askBlockInfo :: IntraMMMMonad (Maybe (VName, SubExp))
askBlockInfo = asks blockInfo

localBlockInfo :: (Maybe (VName, SubExp) -> Maybe (VName, SubExp)) -> IntraMMMMonad a -> IntraMMMMonad a
localBlockInfo f = local $ \env -> env {blockInfo = f $ blockInfo env }


runBuilderMMM_ :: Builder GPU () -> Scope GPU -> IntraMMMMonad (Stms GPU)
runBuilderMMM_ m s =
  modifyNameSource $ runState $ runBuilderT_ m s


intraMMM :: Pass GPU GPU
intraMMM =
  Pass
    "tensor-mma"
    "Extracts NVIDIA tensor core MMA operations"
    transformProg

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
    $ resultBody [Var $ paramName cParam]


-- TODO: make more flexible in sizes and types
-- TODO: entire global as input or only slize, if entire add index
mkCopyGlobalShared :: PassM (FunDef GPU)
mkCopyGlobalShared = do
  globalOuterDim <- newParam "globalOuterDim" $ Prim int64
  globalParam <- newParam "global" $ Array (FloatType Float16) (Shape [Var $ paramName globalOuterDim, mkInt64Const 16, mkInt64Const 16]) Nonunique
  sharedParam <- newParam "shared" $  Array (FloatType Float16) (Shape [mkInt64Const 16, mkInt64Const 16]) Unique
  let sharedOut = [(Array (FloatType Float16) (Shape [Free $ mkInt64Const 16, Free $ mkInt64Const 16]) Unique, RetAls [] [])]
  pure $
    FunDef Nothing mempty copyGlobalSharedName
        sharedOut
--        TODO: this assumes only 1 globalOuterDim
        [globalOuterDim, globalParam, sharedParam]
        $ resultBody [Var $ paramName sharedParam]


-- TODO: make more flexible in sizes and types
-- TODO: entire global as input or only slize, if entire add index, maybe dont even take as input?
-- TODO: copy to global instead of shared
mkCopyRegistersGlobal :: PassM (FunDef GPU)
mkCopyRegistersGlobal = do
  registersParam <- newParam "registers" $  Array (FloatType Float16) (Shape [mkInt64Const 32, mkInt64Const 8]) Nonunique
  globalParam <- newParam "global" $ Array (FloatType Float16) (Shape [mkInt64Const 16, mkInt64Const 16]) Unique
  let globalOut = [(Array (FloatType Float16) (Shape [Free $ mkInt64Const 16, Free $ mkInt64Const 16]) Unique, RetAls [] [])]
  pure $
    FunDef Nothing mempty copyRegistersGlobalName
        globalOut
        [registersParam, globalParam]
        $ resultBody [Var $ paramName globalParam]


transformProg :: Prog GPU -> PassM (Prog GPU)
transformProg (Prog opaqueTypes consts funs) = do
  transformedFuns <- mapM transformFunDef funs

-- TODO: gen these as needed
  let typeA = Array (FloatType Float16) (Shape [mkInt64Const 16, mkInt64Const 16]) Nonunique
  let typeB = Array (FloatType Float16) (Shape [mkInt64Const 16, mkInt64Const 16]) Nonunique
  let typeCin = Array (FloatType Float16) (Shape [mkInt64Const 8]) Unique
  let typeCout = [(Array (FloatType Float16) (Shape [Free $ mkInt64Const 8]) Unique, RetAls [] [])]

  gemmFun <- mkGemmFun typeA typeB typeCin typeCout
  copyGlobalSharedFun <- mkCopyGlobalShared
  copyRegistersGlobalFun <- mkCopyRegistersGlobal
  pure $ Prog opaqueTypes consts (gemmFun : copyGlobalSharedFun : copyRegistersGlobalFun : transformedFuns)

transformFunDef :: FunDef GPU -> PassM (FunDef GPU)
transformFunDef funDef@(FunDef entry attrs name retType params body) = do
  newBody <- modifyNameSource $ (\(a, b, c) -> (a, b)) . runRWS (transformBody body) (IntraMMMMonadEnv (scopeOf funDef) Nothing)
  pure $ FunDef entry attrs name retType params newBody


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
    innerIndecesA :: [VName],
    innerIndecesB :: [VName],
    outerIndecesA :: [VName],
    outerIndecesB :: [VName],
    outerDimsA :: [SubExp],
    outerDimsB :: [SubExp],
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


-- TODO: maintain scope below?
transformStms :: Stms GPU -> IntraMMMMonad (Stms GPU)
transformStms stms = join <$> mapM transformStm stms

transformStm :: Stm GPU -> IntraMMMMonad (Stms GPU)
-- TODO: pass name of let to builder
transformStm stm@(Let (Pat [PatElem resName _]) aux e) = do
    scope <- askScope
    maybeBlockInfo <- askBlockInfo
    case maybeBlockInfo of
    --    TODO: allow more sizes?
      Just blockInfo | Just match@(MMAMatch kernelBodyMatch ne 16 16 16) <- expMatch e scope -> do
        let blockSize = 32
        tell [blockSize]
        runBuilderMMM_ (buildMMM resName blockSize blockInfo match) mempty
      _ -> transformStmDefault stm
transformStm stm = transformStmDefault stm


transformStmDefault :: Stm GPU -> IntraMMMMonad (Stms GPU)
transformStmDefault (Let pat aux e) = do
  e' <- transformExp e
  pure $ oneStm $ Let pat aux e'

stmToPat :: Stm GPU -> Pat (LetDec GPU)
stmToPat (Let pat _ _) = pat


buildMMM :: VName -> Int -> (VName, SubExp) -> MMAMatch -> Builder GPU ()
buildMMM resName blockSize (blockIdx, blockDim) (MMAMatch kernelBodyMatch ne sizeM sizeN sizeK) = do
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
  aCopied <- letExp "aCopied" $ Apply copyGlobalSharedName (fmap (,ObservePrim) (outerDimsA kernelBodyMatch) <> [(Var $ arrA kernelBodyMatch, ObservePrim), (Var aScratch, Consume)]) [(Array (typeA kernelBodyMatch) (Shape [Free $ mkInt64Const sizeM, Free $ mkInt64Const sizeK]) Unique, RetAls [] [])] (Safe, SrcLoc NoLoc, [])
  bCopied <- letExp "bCopied" $ Apply copyGlobalSharedName (fmap (,ObservePrim) (outerDimsB kernelBodyMatch) <> [(Var $ arrB kernelBodyMatch, ObservePrim), (Var bScratch, Consume)]) [(Array (typeB kernelBodyMatch) (Shape [Free $ mkInt64Const sizeK, Free $ mkInt64Const sizeN]) Unique, RetAls [] [])] (Safe, SrcLoc NoLoc, [])
--  TODO: copy to shared
--  TODO: is ObservePrim correct?
  blockMMAres_list <- segMap1D "blockMMAres" (SegThreadInBlock SegNoVirt) ResultPrivate blockSizeSubExp $ \thread_idx -> do
    threadCregs <- index "threadCregs" cRegs [thread_idx]
    threadMMAres <- letExp "threadMMAres" $ Apply gemmName [(Var aCopied, ObservePrim), (Var bCopied, ObservePrim), (Var threadCregs, Consume)] [(Array (typeC kernelBodyMatch) (Shape [Free cValsPerThread]) Unique, RetAls [] [])] (Safe, SrcLoc NoLoc, [])
    pure [varRes threadMMAres]
  let [blockMMAres] = blockMMAres_list
  cScratch <- letExp "cScratch" $ BasicOp $ Scratch (typeC kernelBodyMatch) [mkInt64Const sizeM, mkInt64Const sizeN]
  letBindNames [resName] $ Apply copyRegistersGlobalName [(Var blockMMAres, ObservePrim), (Var cScratch, Consume)] [(Array (typeC kernelBodyMatch) (Shape [Free $ mkInt64Const sizeM, Free $ mkInt64Const sizeN]) Unique, RetAls [] [])] (Safe, SrcLoc NoLoc, [])

-- TODO: use reader for scope?
expMatch :: Exp GPU -> Scope GPU -> Maybe MMAMatch
expMatch (Op (SegOp sOp)) = segOpMatch sOp
expMatch _ = const Nothing

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
transformSegOp s@(SegMap level@(SegBlock virt (Just (KernelGrid (Count numBlocks) (Count blockSize)))) space@(SegSpace _ [blockInfo]) ts body) = do
-- TODO: call match functions her, assume perfectly nested and avoid writer
  (newBody, newBlockSizes) <- listen $ localBlockInfo (const $ Just blockInfo) $ transformKernelBody body
  case newBlockSizes of
--  TODO: handle more block sizes?
    [newBlockSize] ->
      pure $ SegMap (SegBlock virt (Just (KernelGrid (Count numBlocks) (Count $ mkInt64Const newBlockSize)))) space ts newBody
    _ -> pure $ SegMap level space ts newBody
transformSegOp s = transformSegOpDefault s

mkInt64Const :: Int -> SubExp
mkInt64Const = Constant . IntValue . intValue Int64

segOpMatch :: SegOp SegLevel GPU -> Scope GPU -> Maybe MMAMatch
segOpMatch s@(SegRed (SegThreadInBlock _) space segBinOps _ body) scope | Just ne <- segBinOpsMatch segBinOps = do
  let (dimVars, segDims) = unzip $ unSegSpace space
  let freeVars = freeIn s
  bodyMatch <- matchesKernelBody dimVars freeVars body scope
  constSegDims <- mapM getConstantValue segDims
  case constSegDims of
--  TODO: also match bodyMatch? allow more sizes?
    [16, 16, 16] ->
--    TODO: pass ts? allow more segDims
      Just (MMAMatch bodyMatch ne 16 16 16)
    _ -> Nothing
-- TODO: extract A, B, types, shape, layout, lookup m, n, k shapes
segOpMatch _ _ = Nothing


getConstantValue :: SubExp -> Maybe Int
getConstantValue (Constant (IntValue v)) = Just $ valueIntegral v
getConstantValue _ = Nothing

-- TODO: return maybe something?
matchesKernelBody :: [VName] -> Names -> KernelBody GPU -> Scope GPU -> Maybe KernelBodyMatch
-- TODO: support more than 3 dimensions?
matchesKernelBody indexVars@[indexVar1, indexVar2, indexVar3] freeVars (KernelBody _ stms [Returns _ _ (Var res)]) scope = do
  let sTable = ST.insertStms (informStms stms) $ ST.fromScope $ addScopeWisdom scope
--  TODO: rename to use A, B, C?
  (resExp, _) <- ST.lookupExp res sTable
  (mulArg1, mulArg2) <- matchesMul $ removeExpWisdom resExp
  (mulArg1Exp, _) <- ST.lookupExp mulArg1 sTable
  (mulArg2Exp, _) <- ST.lookupExp mulArg2 sTable
  (arr1, slice1) <- matchesMulArg $ removeExpWisdom mulArg1Exp
  (arr2, slice2) <- matchesMulArg $ removeExpWisdom mulArg2Exp
  arr1Type <- ST.lookupType arr1 sTable
  arr2Type <- ST.lookupType arr2 sTable
  resType <- ST.lookupType res sTable
  slice1' <- mapM getIndexVar $ unSlice slice1
  slice2' <- mapM getIndexVar $ unSlice slice2
  let (innerIndeces1, outerIndeces1) = partition (`elem` indexVars) slice1'
  let (innerIndeces2, outerIndeces2) = partition (`elem` indexVars) slice2'
  let commonIndeces = innerIndeces1 `intersect` innerIndeces2
  let separateIndeces1 = toList $ fromList innerIndeces1 `difference` fromList innerIndeces2
  let separateIndeces2 = toList $ fromList innerIndeces2 `difference` fromList innerIndeces1
  case (separateIndeces1, separateIndeces2, commonIndeces, arr1Type, arr2Type, resType) of
--  TODO: check which is A and which is B?
    ([m], [n], [k], Array type1 (Shape arr1Dims) _, Array type2 (Shape arr2Dims) _, Prim resTypePrim) | k == indexVar3 && all (`nameIn` freeVars) (outerIndeces1<>outerIndeces2) ->
      let arr1OuterDims = take (length arr1Dims - 2) arr1Dims in
      let arr2OuterDims = take (length arr2Dims - 2) arr2Dims in
      Just (KernelBodyMatch innerIndeces1 innerIndeces2 outerIndeces1 outerIndeces2 arr1OuterDims arr2OuterDims arr1 arr2 m n k type1 type2 resTypePrim)
    _ -> Nothing
matchesKernelBody _ _ _ _ = Nothing

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

data SpaceType = Device | Shared | Scalar

-- TODO: use map?
-- TODO: add mapping like ("gemm_123456", ["shared", "shared", "regs"]) to make general?
type FixState = [(VName, VName)]

-- type FixMonad a = RWST FixEnv () FixState PassM a
type FixMonad a = RWST FixEnv () FixState PassM a

-- TODO: With LMAD we could do swizzle

fixFuns :: Stms GPUMem -> FunDef GPUMem -> PassM (FunDef GPUMem)
fixFuns consts fun
  | funDefName fun == gemmName = pure $ fun {
                                              funDefParams = fixParamsGemmFun $ funDefParams fun,
                                              funDefRetType = fixRetType Scalar $ funDefRetType fun
                                            }
  | funDefName fun == copyGlobalSharedName = pure $ fun {
                                              funDefParams = fixParamsCopyGlobalShared $ funDefParams fun,
                                              funDefRetType = fixRetType Shared $ funDefRetType fun
                                            }
  | funDefName fun == copyRegistersGlobalName = pure $ fun {
                                                  funDefParams = fixParamsCopyRegistersGlobal $ funDefParams fun,
                                                  funDefRetType = fixRetType Shared $ funDefRetType fun
                                                }
  | otherwise = do
      let initScope = scopeOf consts
      let body = funDefBody fun
      stms' <- fixStmtsWithScope initScope . bodyStms $ body
      pure $ fun {funDefBody = body {bodyStms = stms'}}


fixParamsCopyGlobalShared :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsCopyGlobalShared [
    Param attrs1 vName1 (MemMem (Space "device")),
    Param attrs2 vName2 (MemMem (Space "device")),
    p3,
    p4,
    p5
  ] =
  [
    Param attrs1 vName1 (MemMem (Space "device")),
    Param attrs2 vName2 (MemMem (Space "shared")),
    p3,
    p4,
    p5
  ]
fixParamsCopyGlobalShared params = params

fixParamsCopyRegistersGlobal :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsCopyRegistersGlobal [
    Param attrs1 vName1 (MemMem (Space "device")),
    Param attrs2 vName2 (MemMem (Space "device")),
    Param attrs3 vName3 (MemArray t3 shp3 u3 (ArrayIn mName3 lmad3)),
    Param attrs4 vName4 (MemArray t4 shp4 u4 (ArrayIn mName4 lmad4))
  ] =
  let space = ScalarSpace (drop 1 $ shapeDims shp3) t3 in
  [
    Param attrs1 vName1 (MemMem space),
    Param attrs2 vName2 (MemMem (Space "shared")),
    Param attrs3 vName3 (MemArray t3 shp3 u3 (ArrayIn mName3 lmad3)),
    Param attrs4 vName4 (MemArray t4 shp4 u4 (ArrayIn mName4 lmad4))
  ]
fixParamsCopyRegistersGlobal params = params

fixParamsGemmFun :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsGemmFun [
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
fixParamsGemmFun params = params

fixRetType :: SpaceType -> [(RetType GPUMem, RetAls)] -> [(RetType GPUMem, RetAls)]
fixRetType spaceType [(MemMem (Space "device"), als1), (MemArray t shp u (ReturnsNewBlock (Space "device") n lmad), als2)] =
  let space = case spaceType of
              Device -> Space "device"
              Shared -> Space "shared"
              Scalar -> ScalarSpace (fmap extToSubExp (shapeDims shp)) t
  --  TODO: check if ReturnsInBlock is preferred
  in [(MemMem space, als1), (MemArray t shp u (ReturnsNewBlock space n lmad), als2)]
fixRetType _ rets = rets

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
  (Apply fName args rets info)) | fName == gemmName = do
  let space = ScalarSpace (shapeDims shp2) t2
  let newRets = fixRetType Scalar rets
  newArgs <- mapM replaceArg args
  pure $
    Let (Pat [
      PatElem vName1 (MemMem space),
      PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
    ])
    aux
    (Apply fName newArgs newRets info)
fixStmt (
  Let (Pat [
    PatElem vName1 (MemMem (Space "device")),
    PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
  ])
  aux
  (Apply fName args rets info)) | fName == copyGlobalSharedName = do
  let space = Space "shared"
--  TODO: check if need to handle uniqueness/consumption
  let newRets = fixRetType Shared rets
  newArgs <- mapM replaceArg args
  pure $
    Let (Pat [
      PatElem vName1 (MemMem space),
      PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
    ])
    aux
    (Apply fName newArgs newRets info)
-- TODO: check this
fixStmt (
  Let (Pat [
    PatElem vName1 (MemMem (Space "device")),
    PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
  ])
  aux
  (Apply fName args rets info)) | fName == copyRegistersGlobalName = do
  let space = Space "shared"
  let newRets = fixRetType Shared rets
  newArgs <- mapM replaceArg args
  pure $
    Let (Pat [
      PatElem vName1 (MemMem space),
      PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
    ])
    aux
    (Apply fName newArgs newRets info)
fixStmt stm = defaultFixStm stm

defaultFixStm :: Stm GPUMem -> FixMonad (Stm GPUMem)
defaultFixStm (Let pat aux e) = Let pat aux <$> fixExp e


replaceArg :: (SubExp, Diet) -> FixMonad (SubExp, Diet)
replaceArg (Var v, d) = do
  manifestMap <- get
  case lookup v manifestMap of
    Just v' ->
      pure (Var v', d)
    Nothing ->
      pure (Var v, d)
replaceArg a = pure a


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
