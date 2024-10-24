module Futhark.Optimise.IntraMMM (intraMMM, intraMMMMemFixup) where

-- TODO: add specific imports, clean up, reorganize to multiple files

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
import Data.Map.Strict qualified as M
import Futhark.Builder.Class
import Futhark.Error
import Futhark.Util.Pretty (prettyString)
import Data.Bits
import Prelude hiding (lookup)
import Futhark.Pass
  ( Pass (..),
    PassM,
    intraproceduralTransformation,
    intraproceduralTransformationWithConsts,
  )
import Data.Loc (Loc(NoLoc), SrcLoc (SrcLoc))
import Colog.Core (duplicate)

traceHelper :: (Show a) => a -> a
traceHelper x = trace (show x ++ "\n") x

divUp :: Int -> Int -> Int
divUp x y = (x + y - 1) `div` y

intraMMM :: Pass GPU GPU
intraMMM =
  Pass
    "tensor-mma"
    "Extracts NVIDIA tensor core MMA operations"
    transformProg


type MMMFunDef = (MMMSignature, FunDef GPU)
type MMMFuns = [MMMFunDef]
-- TODO: implement Eq to ensure no duplicate defs


-- TODO: dont use RWS?
type IntraMMMMonad = RWS (Scope GPU) MMMFuns VNameSource


runBuilderMMM :: Builder GPU a -> Scope GPU -> IntraMMMMonad (a, Stms GPU)
runBuilderMMM m s =
  modifyNameSource $ runState $ runBuilderT m s


mapStmsWithScope :: (Monoid a, LocalScope rep f) => (Stm rep -> f a) -> Stms rep -> f a
mapStmsWithScope f stms =
  case stmsHead stms of
    Nothing -> pure mempty
    Just (stm, stms') -> do
      stm' <- f stm
      stms'' <- inScopeOf stm $ mapStmsWithScope f stms'
      pure $ stm' <> stms''

mkInt64Const :: Int -> SubExp
mkInt64Const = Constant . IntValue . intValue Int64



-- IR generation

-- TODO: pass sizes in args or encoded in name? todo also pass types in args?
mkGemmFun :: (MonadFreshNames m) => PrimType -> PrimType -> PrimType -> Int -> Int -> Int -> Int -> m MMMFunDef
-- TODO: use record?
mkGemmFun elmTypeA elmTypeB elmTypeC sizeM sizeN sizeK sizeRegs = do
  let typeA = Array elmTypeA (Shape [mkInt64Const sizeM, mkInt64Const sizeK]) Nonunique
  let typeB = Array elmTypeB (Shape [mkInt64Const sizeK, mkInt64Const sizeN]) Nonunique
  let typeCin = Array elmTypeC (Shape [mkInt64Const sizeRegs]) Unique
--  TODO: use Free or Ext?
  let typeCout = [(Array elmTypeC (Shape [Free $ mkInt64Const sizeRegs]) Unique, RetAls [] [])]

  aParam <- newParam "A" typeA
  bParam <- newParam "B" typeB
  cParam <- newParam "C" typeCin
  aElmTypeParam <- newParam "elmTypeA" $ Prim elmTypeA
  bElmTypeParam <- newParam "elmTypeB" $ Prim elmTypeB
  mParam <- newParam "M" $ Prim int64
  nParam <- newParam "N" $ Prim int64
  kParam <- newParam "K" $ Prim int64
  blockSizeParam <- newParam "blockSize" $ Prim int64

  fName <- fmap (nameFromString . prettyString) $ newName $ VName gemmName 0
  pure (GemmSignature elmTypeA elmTypeB elmTypeC sizeM sizeN sizeK sizeRegs,
      FunDef Nothing mempty fName
      typeCout
      [aParam, bParam, cParam, aElmTypeParam, bElmTypeParam, mParam, nParam, kParam, blockSizeParam]
      $ resultBody [Var $ paramName cParam]
    )

-- TODO: entire global as input or only slize, if entire add index as argument
mkCopyGlobalShared :: (MonadFreshNames m) => PrimType -> Int -> Int -> m MMMFunDef
mkCopyGlobalShared elmType sizeY sizeX = do
-- TODO: take (number of) outer globalOuterDims as input, also get outer indices?
  globalOuterDim <- newParam "globalOuterDim" $ Prim int64
  globalParam <- newParam "global" $ Array elmType (Shape [Var $ paramName globalOuterDim, mkInt64Const sizeY, mkInt64Const sizeX]) Nonunique
  sharedParam <- newParam "shared" $  Array elmType (Shape [mkInt64Const sizeY, mkInt64Const sizeX]) Unique
  elmTypeParam <- newParam "elmTypeA" $ Prim elmType
  yParam <- newParam "Y" $ Prim int64
  xParam <- newParam "X" $ Prim int64
  blockSizeParam <- newParam "blockSize" $ Prim int64

  fName <- fmap (nameFromString . prettyString) $ newName $ VName copyGlobalSharedName 0

--  TODO: use Free or Ext?
  let sharedOut = [(Array elmType (Shape [Free $ mkInt64Const sizeY, Free $ mkInt64Const sizeX]) Unique, RetAls [] [])]
  pure (CopyGlobalSharedSignature elmType sizeY sizeX,
      FunDef Nothing mempty fName
          sharedOut
          [globalOuterDim, globalParam, sharedParam, elmTypeParam, yParam, xParam, blockSizeParam]
          $ resultBody [Var $ paramName sharedParam]
    )

-- TODO: copy to global instead of shared?
mkCopyRegistersShared :: (MonadFreshNames m) => PrimType -> PrimType -> PrimType -> Int -> Int -> Int -> Int -> m MMMFunDef
mkCopyRegistersShared elmTypeA elmTypeB elmTypeC sizeM sizeN sizeRegs blockSize = do
  registersParam <- newParam "registers" $  Array elmTypeC (Shape [mkInt64Const blockSize, mkInt64Const sizeRegs]) Nonunique
  sharedParam <- newParam "shared" $ Array elmTypeC (Shape [mkInt64Const sizeM, mkInt64Const sizeN]) Unique
  aElmTypeParam <- newParam "elmTypeA" $ Prim elmTypeA
  bElmTypeParam <- newParam "elmTypeB" $ Prim elmTypeB
  mParam <- newParam "M" $ Prim int64
  nParam <- newParam "N" $ Prim int64
  blockSizeParam <- newParam "blockSize" $ Prim int64

  fName <- fmap (nameFromString . prettyString) $ newName $ VName copyRegistersSharedName 0

--  TODO: use Free or Ext?
  let sharedOut = [(Array elmTypeC (Shape [Free $ mkInt64Const sizeM, Free $ mkInt64Const sizeN]) Unique, RetAls [] [])]
  pure (CopyRegistersSharedSignature elmTypeC sizeM sizeN sizeRegs blockSize,
      FunDef Nothing mempty fName
          sharedOut
          [registersParam, sharedParam, aElmTypeParam, bElmTypeParam, mParam, nParam, blockSizeParam]
          $ resultBody [Var $ paramName sharedParam]
    )

buildMMM :: VName -> OuterMMAMatch -> Builder GPU MMMFuns
-- TODO: fix args
buildMMM resName outerMatch = do
  let InnerMMAMatch kernelBodyMatch ne sizeM sizeN sizeK = innerMatch outerMatch

--  TODO: handle this?
  unless ([fst $ outerBlockInfo outerMatch] == outerIndecesA kernelBodyMatch && outerIndecesA kernelBodyMatch == outerIndecesB kernelBodyMatch) $
    compilerLimitationS "Not implemented"

--  let cValsPerThread = min 64 (sizeM * sizeN `div` 32)
--  let blockSize = sizeM * sizeN `div` cValsPerThread
-- TODO: should depend on elm size, ensure integers
  let warpsM = sizeM `divUp` 64
  let warpsN = sizeN `divUp` 64

  let blockSize = warpsM * warpsN * 32
  let cValsPerThread = sizeM * sizeN `div` blockSize

  let numBlocks = outerNumBlocks outerMatch
--  TODO: get as input instead?
  let blockSizeSubExp = mkInt64Const blockSize
-- TODO: do we need to init regs when used only once?
-- TODO: use SegNoVirtFull instead of loop and avoid setting the block size?

-- TODO: A and B should probably have same type

  gemmFun <- mkGemmFun (typeA kernelBodyMatch) (typeB kernelBodyMatch) (typeC kernelBodyMatch) sizeM sizeN sizeK cValsPerThread
  copyGlobalSharedFunA <- mkCopyGlobalShared (typeA kernelBodyMatch) sizeM sizeK
  copyGlobalSharedFunB <- mkCopyGlobalShared (typeB kernelBodyMatch) sizeK sizeN
  copyRegistersSharedFun <- mkCopyRegistersShared (typeA kernelBodyMatch) (typeB kernelBodyMatch) (typeC kernelBodyMatch) sizeM sizeN cValsPerThread blockSize
  let addedFuns = [gemmFun, copyGlobalSharedFunA, copyGlobalSharedFunB, copyRegistersSharedFun]

  blockMMAres_list <- segMap1D "blockMMAres" (SegBlock SegNoVirt $ Just $ KernelGrid (Count numBlocks) (Count $ mkInt64Const (blockSize + 32))) ResultMaySimplify numBlocks $ \blockIndex -> do
-- TODO: make version without regs for dynamic arguments, gemm outputs shared mem no regs as input
    cRegs_list <- segMap1D "cRegs" (SegThreadInBlock SegNoVirt) ResultPrivate blockSizeSubExp $ \_ -> do
      cScratch <- letExp "cScratch" $ BasicOp $ Scratch (typeC kernelBodyMatch) [mkInt64Const cValsPerThread]
      cLoop <- forLoop (mkInt64Const cValsPerThread) [cScratch] $ \i [cMerge] -> do
        cZeroed <- update "cZeroed" cMerge [i] ne
  --      TODO: use pure instead of resultBodyM?
        resultBodyM [Var cZeroed]
      pure [varRes cLoop]
    let [cRegs] = cRegs_list
    aScratch <- letExp "aScratch" $ BasicOp $ Scratch (typeA kernelBodyMatch) [mkInt64Const sizeM, mkInt64Const sizeK]
    bScratch <- letExp "bScratch" $ BasicOp $ Scratch (typeB kernelBodyMatch) [mkInt64Const sizeK, mkInt64Const sizeN]

--    TODO: check if should be outerDimsA?
    let copyArgsA = [(numBlocks, ObservePrim), (Var $ arrA kernelBodyMatch, ObservePrim), (Var aScratch, Consume), (Constant $ blankPrimValue $ typeA kernelBodyMatch, ObservePrim), (mkInt64Const sizeM, ObservePrim), (mkInt64Const sizeK, ObservePrim), (mkInt64Const blockSize, ObservePrim)]
    let copyRetsA = [(Array (typeA kernelBodyMatch) (Shape [Free $ mkInt64Const sizeM, Free $ mkInt64Const sizeK]) Unique, RetAls [] [])]
    aCopied <- letExp "aCopied" $ Apply (funDefName $ snd copyGlobalSharedFunA) copyArgsA copyRetsA (Safe, SrcLoc NoLoc, [])
    let copyArgsB = [(numBlocks, ObservePrim), (Var $ arrB kernelBodyMatch, ObservePrim), (Var bScratch, Consume), (Constant $ blankPrimValue $ typeB kernelBodyMatch, ObservePrim), (mkInt64Const sizeK, ObservePrim), (mkInt64Const sizeN, ObservePrim), (mkInt64Const blockSize, ObservePrim)]
    let copyRetsB = [(Array (typeB kernelBodyMatch) (Shape [Free $ mkInt64Const sizeK, Free $ mkInt64Const sizeN]) Unique, RetAls [] [])]
    bCopied <- letExp "bCopied" $ Apply (funDefName $ snd copyGlobalSharedFunB) copyArgsB copyRetsB (Safe, SrcLoc NoLoc, [])

    inBlockMMAres_list <- segMap1D "inBlockMMAres" (SegThreadInBlock SegNoVirt) ResultPrivate blockSizeSubExp $ \thread_idx -> do
      threadCregs <- index "threadCregs" cRegs [thread_idx]
  --  TODO: is ObservePrim correct?
      let mmmArgs = [(Var aCopied, ObservePrim), (Var bCopied, ObservePrim), (Var threadCregs, Consume), (Constant $ blankPrimValue $ typeA kernelBodyMatch, ObservePrim), (Constant $ blankPrimValue $ typeB kernelBodyMatch, ObservePrim), (mkInt64Const sizeM, ObservePrim), (mkInt64Const sizeN, ObservePrim), (mkInt64Const sizeK, ObservePrim), (mkInt64Const blockSize, ObservePrim)]
      let mmmRets = [(Array (typeC kernelBodyMatch) (Shape [Free $ mkInt64Const cValsPerThread]) Unique, RetAls [] [])]
      threadMMAres <- letExp "threadMMAres" $ Apply (funDefName $ snd gemmFun) mmmArgs mmmRets (Safe, SrcLoc NoLoc, [])
      pure [varRes threadMMAres]
    let [inBlockMMAres] = inBlockMMAres_list

    cScratch <- letExp "cScratch" $ BasicOp $ Scratch (typeC kernelBodyMatch) [mkInt64Const sizeM, mkInt64Const sizeN]
    let copyArgsC = [(Var inBlockMMAres, ObservePrim), (Var cScratch, Consume), (Constant $ blankPrimValue $ typeA kernelBodyMatch, ObservePrim), (Constant $ blankPrimValue $ typeB kernelBodyMatch, ObservePrim), (mkInt64Const sizeM, ObservePrim), (mkInt64Const sizeN, ObservePrim), (mkInt64Const blockSize, ObservePrim)]
    let copyRetsC = [(Array (typeC kernelBodyMatch) (Shape [Free $ mkInt64Const sizeM, Free $ mkInt64Const sizeN]) Unique, RetAls [] [])]
    cCopied <- letExp "cCopied" $ Apply (funDefName $ snd copyRegistersSharedFun) copyArgsC copyRetsC (Safe, SrcLoc NoLoc, [])
    pure [varRes cCopied]

  let [blockMMAres] = blockMMAres_list
  letBindNames [resName] $ BasicOp $ SubExp $ Var blockMMAres
  pure addedFuns



-- Traverse and transform

transformProg :: Prog GPU -> PassM (Prog GPU)
transformProg (Prog opaqueTypes consts funs) = do
  (transformedFuns, mmmFuns) <- modifyNameSource $ (\(a, s, w) -> ((a, w), s)) . runRWS (mapM transformFunDef funs) (scopeOf consts)
  let (_, addedFuns) = unzip mmmFuns
  pure $ Prog opaqueTypes consts (addedFuns <> transformedFuns)

transformFunDef :: FunDef GPU -> IntraMMMMonad (FunDef GPU)
transformFunDef funDef@(FunDef entry attrs name retType params body) =
  FunDef entry attrs name retType params <$> inScopeOf funDef (transformBody body)

transformStms :: Stms GPU -> IntraMMMMonad (Stms GPU)
transformStms = mapStmsWithScope transformStm

transformStm :: Stm GPU -> IntraMMMMonad (Stms GPU)
transformStm stm@(Let (Pat [PatElem resName _]) aux e) = do
    scope <- askScope
    case outerSegOpExpMatch scope e of
      Just match -> do
        (mmmFuns, stms) <- runBuilderMMM (buildMMM resName match) scope
        tell mmmFuns
        pure stms
      _ -> transformStmDefault stm
transformStm stm = transformStmDefault stm

transformStmDefault :: Stm GPU -> IntraMMMMonad (Stms GPU)
transformStmDefault (Let pat aux e) = do
  e' <- transformExp e
  pure $ oneStm $ Let pat aux e'

-- TODO: match WithAcc?
transformExp :: Exp GPU -> IntraMMMMonad (Exp GPU)
transformExp (Match subExps cases body matchDec) = Match subExps <$> mapM transformCase cases <*> transformBody body <*> pure matchDec
transformExp (Loop params form body) = localScope (scopeOfFParams (map fst params) <> scopeOfLoopForm form) $ do
  newBody <- transformBody body
  pure $ Loop params form newBody
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
transformSegOp (SegMap level space ts body) = SegMap level space ts <$> transformKernelBody body
transformSegOp (SegRed level space ops ts body) = SegRed level space ops ts <$> transformKernelBody body
transformSegOp (SegScan level space ops ts body) = SegScan level space ops ts <$> transformKernelBody body
transformSegOp (SegHist level space ops hist body) = SegHist level space ops hist <$> transformKernelBody body

transformKernelBody :: KernelBody GPU -> IntraMMMMonad (KernelBody GPU)
transformKernelBody (KernelBody desc stms res) = KernelBody desc <$> transformStms stms <*> pure res


-- Pattern matching

data OuterMMAMatch = OuterMMAMatch
  {
    innerMatch :: InnerMMAMatch,
    outerVirt :: SegVirt,
    outerNumBlocks :: SubExp,
    outerBlockSize :: SubExp,
    outerFlatIndex :: VName,
    outerBlockInfo :: (VName, SubExp),
    outerTypes :: [Type]
  }
  deriving (Show, Eq, Ord)

data InnerMMAMatch = InnerMMAMatch
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

-- TODO: use monad or scope? or monad in scope? maintain scope?
outerSegOpExpMatch :: Scope GPU -> Exp GPU -> Maybe OuterMMAMatch
outerSegOpExpMatch scope
  (Op (SegOp (
      SegMap
        level@(SegBlock SegNoVirt (Just (KernelGrid (Count numBlocks) (Count blockSize))))
        space@(SegSpace flatIndex [blockInfo])
        ts
        body
  ))) = do
  innerMatch <- blockKernelBodyMatch scope body
  pure $ OuterMMAMatch innerMatch SegNoVirt numBlocks blockSize flatIndex blockInfo ts
-- TODO: also match SegThread in similar way
outerSegOpExpMatch _ _ = Nothing

-- TODO: allow stms before and after seg(inblock), allow multiple inner matches?
blockKernelBodyMatch :: Scope GPU -> KernelBody GPU -> Maybe InnerMMAMatch
blockKernelBodyMatch scope (KernelBody desc stms res) | [Let p aux e] <- stmsToList stms =
  innerSegOpExpMatch scope e
blockKernelBodyMatch _ _ = Nothing

innerSegOpExpMatch :: Scope GPU -> Exp GPU -> Maybe InnerMMAMatch
innerSegOpExpMatch scope
-- TODO: check if better to match segmap with inner reduction
  (Op (SegOp segRed@(
      SegRed (SegThreadInBlock _) space segBinOps ts body
  )))
  | Just ne <- segBinOpsMatch segBinOps
  = do
  let (dimVars, segDims) = unzip $ unSegSpace space
--  TODO: check this, probably not correct when not maintaining scope
  let freeVars = freeIn segRed
  bodyMatch <- inBlockKernelBodyMatch dimVars freeVars body scope
  constSegDims <- mapM constantValueMatch segDims
  case constSegDims of
--  TODO: also match bodyMatch? allow more sizes?
    [m, n, k] | all sizeMatches constSegDims ->
--    TODO: pass ts? allow more segDims
      Just (InnerMMAMatch bodyMatch ne m n k)
    _ -> Nothing
-- TODO: extract A, B, types, shape, layout, lookup m, n, k shapes
innerSegOpExpMatch _ _ = Nothing

sizeMatches :: Int -> Bool
sizeMatches x =
  x `mod` 16 == 0
  && 0 < x
--  TODO: should depend on elm size
  && x <= 128
--  Check if x is power of 2
  && popCount x == 1

constantValueMatch :: SubExp -> Maybe Int
constantValueMatch (Constant (IntValue v)) = Just $ valueIntegral v
constantValueMatch _ = Nothing

inBlockKernelBodyMatch :: [VName] -> Names -> KernelBody GPU -> Scope GPU -> Maybe KernelBodyMatch
-- TODO: support more than 3 dimensions?
inBlockKernelBodyMatch indexVars@[indexVar1, indexVar2, indexVar3] freeVars (KernelBody _ stms [Returns _ _ (Var res)]) scope = do
  let sTable = ST.insertStms (informStms stms) $ ST.fromScope $ addScopeWisdom scope
--  TODO: rename to use A, B, C?
  (resExp, _) <- ST.lookupExp res sTable
  resWithoutConversion <- case resExp of
                              BasicOp (ConvOp _ (Var converted)) -> do
                                (convertedExp, _) <- ST.lookupExp converted sTable
                                pure convertedExp
                              notConvertedExp ->
                                pure notConvertedExp
  (mulArg1, mulArg2) <- matchesMul $ removeExpWisdom resWithoutConversion
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
inBlockKernelBodyMatch _ _ _ _ = Nothing

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



-- Memory fixup


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
  | gemmName `isPrefixOfName` funDefName fun = pure $ fun {
                                              funDefParams = fixParamsGemmFun $ funDefParams fun,
                                              funDefRetType = fixRetType Scalar $ funDefRetType fun
                                            }
  | copyGlobalSharedName `isPrefixOfName` funDefName fun = pure $ fun {
                                              funDefParams = fixParamsCopyGlobalShared $ funDefParams fun,
                                              funDefRetType = fixRetType Shared $ funDefRetType fun
                                            }
  | copyRegistersSharedName `isPrefixOfName` funDefName fun = pure $ fun {
                                                  funDefParams = fixParamsCopyRegistersShared $ funDefParams fun,
                                                  funDefRetType = fixRetType Shared $ funDefRetType fun
                                                }
  | otherwise = do
      let initScope = scopeOf consts <> scopeOfFParams (funDefParams fun)
      let body = funDefBody fun
      stms' <- fixStmtsWithScope initScope . bodyStms $ body
      pure $ fun {funDefBody = body {bodyStms = stms'}}


fixParamsCopyGlobalShared :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsCopyGlobalShared [
    Param attrs1 vName1 (MemMem (Space "device")),
    Param attrs2 vName2 (MemMem (Space "device")),
    p3, p4, p5, p6, p7, p8, p9
  ] =
  [
    Param attrs1 vName1 (MemMem (Space "device")),
    Param attrs2 vName2 (MemMem (Space "shared")),
    p3, p4, p5, p6, p7, p8, p9
  ]
fixParamsCopyGlobalShared params = params

fixParamsCopyRegistersShared :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsCopyRegistersShared [
    Param attrs1 vName1 (MemMem (Space "device")),
    Param attrs2 vName2 (MemMem (Space "device")),
    p3@(Param _ _ (MemArray t shp _ (ArrayIn _ _))), p4, p5, p6, p7, p8, p9
  ] =
  let space = ScalarSpace (drop 1 $ shapeDims shp) t in
  [
    Param attrs1 vName1 (MemMem space),
    Param attrs2 vName2 (MemMem (Space "shared")),
    p3, p4, p5, p6, p7, p8, p9
  ]
fixParamsCopyRegistersShared params = params

fixParamsGemmFun :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsGemmFun [
    Param attrs1 vName1 (MemMem (Space "device")),
    Param attrs2 vName2 (MemMem (Space "device")),
    Param attrs3 vName3 (MemMem (Space "device")),
    p4, p5, p6@(Param _ _ (MemArray t shp _ (ArrayIn _ _))), p7, p8, p9, p10, p11, p12
  ] =
  let space = ScalarSpace (shapeDims shp) t in
  [
      Param attrs1 vName1 (MemMem (Space "shared")),
      Param attrs2 vName2 (MemMem (Space "shared")),
      Param attrs3 vName3 (MemMem space),
      p4, p5, p6, p7, p8, p9, p10, p11, p12
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
fixStmts = mapStmsWithScope (fmap oneStm . fixStmt)

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
  (Apply fName args rets info)) | gemmName `isPrefixOfName` fName = do
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
  (Apply fName args rets info)) | copyGlobalSharedName `isPrefixOfName` fName = do
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
  (Apply fName args rets info)) | copyRegistersSharedName `isPrefixOfName` fName = do
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


-- TODO: this may be too aggressive
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
fixExp (Match subExps cases body matchDec) = Match subExps <$> mapM fixCase cases <*> fixBody body <*> pure matchDec
fixExp (Loop params form body) = localScope (scopeOfFParams (map fst params) <> scopeOfLoopForm form) $ do
  newBody <- fixBody body
  pure $ Loop params form newBody
fixExp (Op op) = Op <$> fixOp op
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
fixSegOp (SegMap level space ts body) = SegMap level space ts <$> fixKernelBody body
fixSegOp (SegRed level space ops ts body) = SegRed level space ops ts <$> fixKernelBody body
fixSegOp (SegScan level space ops ts body) = SegScan level space ops ts <$> fixKernelBody body
fixSegOp (SegHist level space ops hist body) = SegHist level space ops hist <$> fixKernelBody body

fixKernelBody :: KernelBody GPUMem -> FixMonad (KernelBody GPUMem)
fixKernelBody (KernelBody desc stms res) = KernelBody desc <$> fixStmts stms <*> pure res
