module Futhark.Optimise.IntraMMM (intraMMM, intraMMMMemFixup) where

-- TODO: add specific imports, clean up, reorganize to multiple files

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Foldable (toList)
import Futhark.IR.GPU

import Futhark.IR.GPUMem
--import Futhark.IR.Mem
--import Futhark.IR.Mem.LMAD as LMAD
import Futhark.Optimise.Simplify.Rep
import Futhark.Builder
import qualified Futhark.Analysis.SymbolTable as ST
import Data.List (partition, intersect, lookup)
import Data.Set (fromList, difference)
import Futhark.Optimise.TileLoops.Shared
import Futhark.Construct
import Futhark.Optimise.IntraMMM.Utils
import Data.Map.Strict qualified as M
import Data.Bits
import Data.Semigroup
import Futhark.Pass.Simplify
import Prelude hiding (lookup)
import Futhark.Pass
  ( Pass (..),
    PassM,
    intraproceduralTransformationWithConsts,
  )
import Data.Loc (Loc(NoLoc), SrcLoc (SrcLoc))

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

data IntraMMMMonadEnv = IntraMMMMonadEnv {envScope :: Scope GPU, envBlockSize :: Maybe Int}

type IntraMMMMonad = RWS IntraMMMMonadEnv MMMFuns VNameSource

-- TODO: use newtype above to fix warning
instance HasScope GPU IntraMMMMonad where
  askScope = asks envScope

instance LocalScope GPU IntraMMMMonad where
  localScope extension = local $ \env -> env {envScope = M.union extension $ envScope env}

askBlockSize :: IntraMMMMonad (Maybe Int)
askBlockSize = asks envBlockSize

localBlockSize :: (Maybe Int -> Maybe Int) -> IntraMMMMonad a -> IntraMMMMonad a
localBlockSize f = local $ \env -> env {envBlockSize = f $ envBlockSize env}


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
mkGemmFun ::
  (MonadFreshNames m) =>
  PrimType ->
  PrimType ->
  PrimType ->
  Int ->
  Int ->
  Int ->
  Int ->
  m MMMFunDef
-- TODO: use record?
mkGemmFun elmTypeA elmTypeB elmTypeC sizeM sizeN sizeK sizeRegs = do
  let typeA = Array elmTypeA (Shape [mkInt64Const sizeM, mkInt64Const sizeK]) Nonunique
      typeB = Array elmTypeB (Shape [mkInt64Const sizeK, mkInt64Const sizeN]) Nonunique
      typeCin = Array elmTypeC (Shape [mkInt64Const sizeRegs]) Unique
      --  TODO: use Free or Ext?
      typeCout = [(Array elmTypeC (Shape [Free $ mkInt64Const sizeRegs]) Unique, RetAls [] [])]

  aParam <- newParam "A" typeA
  bParam <- newParam "B" typeB
  cParam <- newParam "C" typeCin
  aElmTypeParam <- newParam "elmTypeA" $ Prim elmTypeA
  bElmTypeParam <- newParam "elmTypeB" $ Prim elmTypeB
  mParam <- newParam "M" $ Prim int64
  nParam <- newParam "N" $ Prim int64
  kParam <- newParam "K" $ Prim int64
  mWarpsParam <- newParam "mWarps" $ Prim int64
  nWarpsParam <- newParam "nWarps" $ Prim int64

  fName <- fmap (nameFromString . prettyString) $ newName $ VName gemmName 0
  let funParams =
        [ aParam,
          bParam,
          cParam,
          aElmTypeParam,
          bElmTypeParam,
          mParam,
          nParam,
          kParam,
          mWarpsParam,
          nWarpsParam
        ]
  pure
    ( GemmSignature elmTypeA elmTypeB elmTypeC sizeM sizeN sizeK sizeRegs,
      FunDef Nothing mempty fName typeCout funParams $
        resultBody [Var $ paramName cParam]
    )

-- TODO: entire global as input or only slize, if entire add index as argument
mkCopyGlobalShared :: (MonadFreshNames m) => PrimType -> Int -> Int -> m MMMFunDef
mkCopyGlobalShared elmType sizeY sizeX = do
  let arrShape = Shape
        [ mkInt64Const sizeY,
          mkInt64Const sizeX
        ]

  globalParam <- newParam "global" $ Array elmType arrShape Nonunique
  sharedParam <- newParam "shared" $ Array elmType arrShape Unique
  offsetParam <- newParam "offset" $ Prim int64
  elmTypeParam <- newParam "elmTypeA" $ Prim elmType
  yParam <- newParam "Y" $ Prim int64
  xParam <- newParam "X" $ Prim int64
  mWarpsParam <- newParam "mWarps" $ Prim int64
  nWarpsParam <- newParam "nWarps" $ Prim int64

  fName <-
    fmap (nameFromString . prettyString) $ newName $ VName copyGlobalSharedName 0

  --  TODO: use Free or Ext?
  let sharedOut =
        [ ( Array elmType (fmap Free arrShape) Unique,
            RetAls [] []
          )
        ]
  let funParams =
        [ globalParam,
          sharedParam,
          offsetParam,
          elmTypeParam,
          yParam,
          xParam,
          mWarpsParam,
          nWarpsParam
        ]
  pure
    ( CopyGlobalSharedSignature elmType sizeY sizeX,
      FunDef Nothing mempty fName sharedOut funParams $
        resultBody [Var $ paramName sharedParam]
    )

-- TODO: copy to global instead of shared?
mkCopyRegistersShared ::
  (MonadFreshNames m) =>
  PrimType ->
  PrimType ->
  PrimType ->
  Int ->
  Int ->
  Int ->
  Int ->
  m MMMFunDef
mkCopyRegistersShared elmTypeA elmTypeB elmTypeC sizeM sizeN sizeRegs blockSize = do
  registersParam <-
    newParam "registers" $
      Array
        elmTypeC
        (Shape [mkInt64Const blockSize, mkInt64Const sizeRegs])
        Nonunique
  sharedParam <-
    newParam "shared" $
      Array
        elmTypeC
        (Shape [mkInt64Const sizeM, mkInt64Const sizeN])
        Unique
  aElmTypeParam <- newParam "elmTypeA" $ Prim elmTypeA
  bElmTypeParam <- newParam "elmTypeB" $ Prim elmTypeB
  mParam <- newParam "M" $ Prim int64
  nParam <- newParam "N" $ Prim int64
  mWarpsParam <- newParam "mWarps" $ Prim int64
  nWarpsParam <- newParam "nWarps" $ Prim int64

  fName <- fmap (nameFromString . prettyString) $ newName $ VName copyRegistersSharedName 0

  --  TODO: use Free or Ext?
  let sharedOut =
        [ ( Array
              elmTypeC
              (Shape [Free $ mkInt64Const sizeM, Free $ mkInt64Const sizeN])
              Unique,
            RetAls [] []
          )
        ]
  let funParams =
        [ registersParam,
          sharedParam,
          aElmTypeParam,
          bElmTypeParam,
          mParam,
          nParam,
          mWarpsParam,
          nWarpsParam
        ]
  pure
    ( CopyRegistersSharedSignature elmTypeC sizeM sizeN sizeRegs blockSize,
      FunDef Nothing mempty fName sharedOut funParams $
        resultBody [Var $ paramName sharedParam]
    )

-- Expression for creating shared(scratch) memory
scratchMem :: PrimType -> [Int] -> Exp GPU
scratchMem elmType dims = BasicOp $ Scratch elmType $ map mkInt64Const dims

buildMMM :: VName -> Int -> InnerMMAMatch -> Builder GPU MMMFuns
-- TODO: fix args
buildMMM resName actualBlockSize match@(InnerMMAMatch kernelBodyMatch ne sizeM sizeN sizeK) = do
  --  TODO: check this?
  --  unless ([fst $ outerBlockInfo outerMatch] == outerIndecesA kernelBodyMatch && outerIndecesA kernelBodyMatch == outerIndecesB kernelBodyMatch) $
  --    compilerLimitationS "Not implemented"

--    TODO: remove?
--    let optimalWarpTileM = 64 in
--    let optimalWarpTileN = 64 in
--    let (warpsM, warpsN) = (sizeM `divUp` optimalWarpTileM, sizeN `divUp` optimalWarpTileN)

  let (warpsM, warpsN) = getOptimalWarps actualBlockSize match
  let blockSize = warpsM * warpsN * 32
  let cValsPerThread = sizeM * sizeN `div` blockSize

--  traceM $ "blockSize: " ++ show blockSize
--  traceM $ "actualBlockSize: " ++ show actualBlockSize
--  traceM $ "warpsM: " ++ show warpsM
--  traceM $ "warpsN: " ++ show warpsN

--  TODO: would be better to check if copy global shared is needed here?

  -- TODO: do we need to init regs when used only once?
  -- TODO: use SegNoVirtFull instead of loop and avoid setting the block size?

  -- TODO: A and B should probably have same type

  -- TODO: use letSubExp
  let elmTypeC = typeC kernelBodyMatch
      elmTypeB = typeB kernelBodyMatch
      elmTypeA = typeA kernelBodyMatch
  gemmFun <- mkGemmFun elmTypeA elmTypeB elmTypeC sizeM sizeN sizeK cValsPerThread
  copyGlobalSharedFunA <- mkCopyGlobalShared (typeA kernelBodyMatch) sizeM sizeK
  copyGlobalSharedFunB <- mkCopyGlobalShared (typeB kernelBodyMatch) sizeK sizeN
  copyRegistersSharedFun <-
    mkCopyRegistersShared -- Maybe too much formatting
      elmTypeA
      elmTypeB
      elmTypeC
      sizeM
      sizeN
      cValsPerThread
      blockSize
  let addedFuns =
        [ gemmFun,
          copyGlobalSharedFunA,
          copyGlobalSharedFunB,
          copyRegistersSharedFun
        ]

  let thrdInBlock = SegThreadInBlock SegNoVirt
  -- TODO: make version without regs for dynamic arguments, gemm outputs shared mem no regs as input
  cRegs_list <-
    segMap1D "cRegs" thrdInBlock ResultPrivate (mkInt64Const blockSize) $ \_ -> do
      cScratch <- letExp "cScratch" $ scratchMem elmTypeC [cValsPerThread]
      cLoop <- forLoop (mkInt64Const cValsPerThread) [cScratch] $ \i [cMerge] -> do
        cZeroed <- update "cZeroed" cMerge [i] ne
        --        TODO: use pure instead of resultBodyM?
        resultBodyM [Var cZeroed]
      pure [varRes cLoop]
  let [cRegs] = cRegs_list
  aScratch <- letExp "aScratch" $ scratchMem elmTypeA [sizeM, sizeK]
  bScratch <- letExp "bScratch" $ scratchMem elmTypeB [sizeK, sizeN]

  slicedA <- letExp "slicedA" $ BasicOp $ Index (arrA kernelBodyMatch) $ Slice $ fmap (DimFix . Var) (outerIndecesA kernelBodyMatch) <> [DimSlice (mkInt64Const 0) (mkInt64Const sizeM) (mkInt64Const 1), DimSlice (mkInt64Const 0) (mkInt64Const sizeK) (mkInt64Const 1)]
  slicedB <- letExp "slicedB" $ BasicOp $ Index (arrB kernelBodyMatch) $ Slice $ fmap (DimFix . Var) (outerIndecesB kernelBodyMatch) <> [DimSlice (mkInt64Const 0) (mkInt64Const sizeK) (mkInt64Const 1), DimSlice (mkInt64Const 0) (mkInt64Const sizeN) (mkInt64Const 1)]

--  Need to pass this explicitly as LMAD info is lost on function call
  let pe64DimsA = fmap pe64 $ outerDimsA kernelBodyMatch <> [mkInt64Const sizeM, mkInt64Const sizeK]
      pe64IndiciesA = fmap pe64 $ fmap Var (outerIndecesA kernelBodyMatch) <> [mkInt64Const 0, mkInt64Const 0]
      pe64DimsB = fmap pe64 $ outerDimsB kernelBodyMatch <> [mkInt64Const sizeK, mkInt64Const sizeN]
      pe64IndiciesB = fmap pe64 $ fmap Var (outerIndecesB kernelBodyMatch) <> [mkInt64Const 0, mkInt64Const 0]

  flatIndexAExp <- toExp $ flattenIndex pe64DimsA pe64IndiciesA
  offsetA <- letExp "offsetA" flatIndexAExp
  flatIndexBExp <- toExp $ flattenIndex pe64DimsB pe64IndiciesB
  offsetB <- letExp "offsetB" flatIndexBExp

  let copyArgsA =
        [ (Var slicedA, ObservePrim),
          (Var aScratch, Consume),
          (Var offsetA, ObservePrim),
          (Constant $ blankPrimValue $ typeA kernelBodyMatch, ObservePrim),
          (mkInt64Const sizeM, ObservePrim),
          (mkInt64Const sizeK, ObservePrim),
          (mkInt64Const warpsM, ObservePrim),
          (mkInt64Const warpsM, ObservePrim)
        ]
      copyRetsA =
        [ ( Array
              (typeA kernelBodyMatch)
              (Shape [Free $ mkInt64Const sizeM, Free $ mkInt64Const sizeK])
              Unique,
            RetAls [] []
          )
        ]

      copyArgsB =
        [ (Var slicedB, ObservePrim),
          (Var bScratch, Consume),
          (Var offsetB, ObservePrim),
          (Constant $ blankPrimValue $ typeB kernelBodyMatch, ObservePrim),
          (mkInt64Const sizeK, ObservePrim),
          (mkInt64Const sizeN, ObservePrim),
          (mkInt64Const warpsM, ObservePrim),
          (mkInt64Const warpsM, ObservePrim)
        ]
      copyRetsB =
        [ ( Array
              (typeB kernelBodyMatch)
              (Shape [Free $ mkInt64Const sizeK, Free $ mkInt64Const sizeN])
              Unique,
            RetAls [] []
          )
        ]

  aCopied <-
    letExp "aCopied" $
      Apply
        (funDefName $ snd copyGlobalSharedFunA)
        copyArgsA
        copyRetsA
        (Safe, SrcLoc NoLoc, [])

  bCopied <-
    letExp "bCopied" $
      Apply
        (funDefName $ snd copyGlobalSharedFunB)
        copyArgsB
        copyRetsB
        (Safe, SrcLoc NoLoc, [])

  inBlockMMAres_list <-
    segMap1D "inBlockMMAres" thrdInBlock ResultPrivate (mkInt64Const blockSize) $ \thread_idx -> do
      threadCregs <- index "threadCregs" cRegs [thread_idx]
      --  TODO: is ObservePrim correct?
      let mmmArgs =
            [ (Var aCopied, ObservePrim),
              (Var bCopied, ObservePrim),
              (Var threadCregs, Consume),
              (Constant $ blankPrimValue $ typeA kernelBodyMatch, ObservePrim),
              (Constant $ blankPrimValue $ typeB kernelBodyMatch, ObservePrim),
              (mkInt64Const sizeM, ObservePrim),
              (mkInt64Const sizeN, ObservePrim),
              (mkInt64Const sizeK, ObservePrim),
              (mkInt64Const warpsM, ObservePrim),
              (mkInt64Const warpsM, ObservePrim)
            ]
      let mmmRets =
            [ ( Array
                  (typeC kernelBodyMatch)
                  (Shape [Free $ mkInt64Const cValsPerThread])
                  Unique,
                RetAls [] []
              )
            ]
      threadMMAres <-
        letExp "threadMMAres" $
          Apply
            (funDefName $ snd gemmFun)
            mmmArgs
            mmmRets
            (Safe, SrcLoc NoLoc, [])
      pure [varRes threadMMAres]
  let [inBlockMMAres] = inBlockMMAres_list
  -- BasicOp $ Scratch (typeC kernelBodyMatch) [mkInt64Const sizeM, mkInt64Const sizeN]
  cScratch <- letExp "cScratch" $ scratchMem elmTypeC [sizeM, sizeN]
  let copyArgsC =
        [ (Var inBlockMMAres, ObservePrim),
          (Var cScratch, Consume),
          (Constant $ blankPrimValue $ typeA kernelBodyMatch, ObservePrim),
          (Constant $ blankPrimValue $ typeB kernelBodyMatch, ObservePrim),
          (mkInt64Const sizeM, ObservePrim),
          (mkInt64Const sizeN, ObservePrim),
          (mkInt64Const warpsM, ObservePrim),
          (mkInt64Const warpsM, ObservePrim)
        ]
  let copyRetsC =
        [ ( Array
              (typeC kernelBodyMatch)
              (Shape [Free $ mkInt64Const sizeM, Free $ mkInt64Const sizeN])
              Unique,
            RetAls [] []
          )
        ]
  cCopied <-
    letExp "cCopied" $
      Apply
        (funDefName $ snd copyRegistersSharedFun)
        copyArgsC
        copyRetsC
        (Safe, SrcLoc NoLoc, [])

  letBindNames [resName] $ BasicOp $ SubExp $ Var cCopied
  pure addedFuns



-- Traverse and transform

transformProg :: Prog GPU -> PassM (Prog GPU)
transformProg (Prog opaqueTypes consts funs) = do
  (transformedFuns, mmmFuns) <- modifyNameSource $ (\(a, s, w) -> ((a, w), s)) . runRWS (mapM transformFunDef funs) (IntraMMMMonadEnv (scopeOf consts) Nothing)
  let (_, addedFuns) = unzip mmmFuns
  pure $ Prog opaqueTypes consts (addedFuns <> transformedFuns)

transformFunDef :: FunDef GPU -> IntraMMMMonad (FunDef GPU)
transformFunDef funDef@(FunDef entry attrs name retType params body) =
  FunDef entry attrs name retType params <$> inScopeOf funDef (transformBody body)

transformStms :: Stms GPU -> IntraMMMMonad (Stms GPU)
transformStms = mapStmsWithScope transformStm

transformStm :: Stm GPU -> IntraMMMMonad (Stms GPU)
transformStm stm@(Let (Pat [PatElem resName _]) _ e) = do
    scope <- askScope
    maybeBlockSize <- askBlockSize
    case (innerSegOpExpMatch scope e, maybeBlockSize) of
      (Just match, Just blockSize) -> do
        (mmmFuns, stms) <- runBuilderMMM (buildMMM resName blockSize match) scope
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
transformSegOp sOp@(SegMap
    (SegBlock SegNoVirt (Just (KernelGrid (Count numBlocks) (Count _blockSize))))
    space@(SegSpace _ [_blockInfo])
    ts
    body@(KernelBody _ stms _)
  ) = do
  scope <- askScope
  case execWriter $ runReaderT (maxBlockSizeStms stms) scope of
    Known (Max maxBlockSize) -> do
--    TODO: just call transformStms or transformBody?
      transformedBody <- localBlockSize (const $ Just maxBlockSize) $ transformKernelBody body
      pure $ SegMap (SegBlock SegNoVirt (Just (KernelGrid (Count numBlocks) (Count $ mkInt64Const maxBlockSize)))) space ts transformedBody
    Unknown ->
      transformSegOpDefault sOp
transformSegOp sOp = transformSegOpDefault sOp

transformSegOpDefault :: SegOp SegLevel GPU -> IntraMMMMonad (SegOp SegLevel GPU)
transformSegOpDefault (SegMap level space ts body) = SegMap level space ts <$> transformKernelBody body
transformSegOpDefault (SegRed level space ops ts body) = SegRed level space ops ts <$> transformKernelBody body
transformSegOpDefault (SegScan level space ops ts body) = SegScan level space ops ts <$> transformKernelBody body
transformSegOpDefault (SegHist level space ops hist body) = SegHist level space ops hist <$> transformKernelBody body

transformKernelBody :: KernelBody GPU -> IntraMMMMonad (KernelBody GPU)
transformKernelBody (KernelBody desc stms res) = KernelBody desc <$> transformStms stms <*> pure res



-- Block size

data KnownUnknown a = Known a | Unknown
  deriving (Show, Eq, Ord)

instance Monoid a => Monoid (KnownUnknown a) where
  mempty = Known mempty

instance (Semigroup a) => Semigroup (KnownUnknown a) where
  Known a <> Known b = Known $ a <> b
  _ <> _ = Unknown

type MaxBlockSizeMonad = ReaderT (Scope GPU) (Writer (KnownUnknown (Max Int)))

maxBlockSizeWalker :: Walker GPU MaxBlockSizeMonad
maxBlockSizeWalker = (identityWalker @GPU)
                        {
                          walkOnOp = maxBlockSizeOp,
                          walkOnBody = maxBlockSizeBody
                        }

maxBlockSizeStms :: Stms GPU -> MaxBlockSizeMonad ()
maxBlockSizeStms = mapStmsWithScope maxBlockSizeStm

maxBlockSizeStm :: Stm GPU -> MaxBlockSizeMonad ()
maxBlockSizeStm (Let _ _ e) = maxBlockSizeExp e

maxBlockSizeExp :: Exp GPU -> MaxBlockSizeMonad ()
maxBlockSizeExp = walkExpM maxBlockSizeWalker

maxBlockSizeOp :: Op GPU -> MaxBlockSizeMonad ()
maxBlockSizeOp op = do
  scope <- askScope
  case (innerOpMatch scope op, op) of
    (Just match, _) -> do
      tell $ Known $ Max $ getOptimalBlockSize match
    (_, SegOp sOp) | (SegThreadInBlock _) <- segLevel sOp ->
      tell $ foldl prodKnownSegDim (Known 1) $ unSegSpace $ segSpace sOp
    _ -> pure ()

maxBlockSizeBody :: Scope GPU -> Body GPU -> MaxBlockSizeMonad ()
maxBlockSizeBody scope (Body _ stms _) = localScope scope $ maxBlockSizeStms stms

prodKnownSegDim :: KnownUnknown (Max Int) -> (VName, SubExp) -> KnownUnknown (Max Int)
prodKnownSegDim (Known (Max acc)) (_, Constant (IntValue n)) = Known $ Max $ acc * valueIntegral n
-- TODO: should lookup?
prodKnownSegDim _ _ = Unknown

getFactorPairs :: Int -> [(Int, Int)]
getFactorPairs n = [(x, n `div` x) | x <- [1 .. n], n `mod` x == 0]

getRatio :: Int -> Int -> Float
getRatio m n = fromIntegral m / fromIntegral n

getOptimalWarps :: Int -> InnerMMAMatch -> (Int, Int)
getOptimalWarps blockSize (InnerMMAMatch _ _ sizeM sizeN _) =
-- TODO: should depend on type
-- Minimum values per thread used for f16 MMA Atom
    let minValsPerThread = 8 in
    let maxBlockSize = (sizeM * sizeN) `div` minValsPerThread in
    let usedBlockSize = min blockSize maxBlockSize in
    let numWarps = usedBlockSize `divUp` 32 in
    let targetRatio = getRatio sizeM sizeN in
    let factorPairs = getFactorPairs numWarps in
--    TODO: which to use?
--   warp tiles will be as close to square as possible, which should maximize register reuse
    let Arg _ (warpsM, warpsN) = minimum $ map (\(x, y) -> Arg (abs $ targetRatio - getRatio x y) (x, y)) factorPairs in
--   warp tiles grid for thread block will be as close to square as possible
--    let Arg _ (warpsM, warpsN) = minimum $ map (\(x, y) -> Arg (abs $ x - y) (x, y)) factorPairs in
    (warpsM, warpsN)

getOptimalBlockSize :: InnerMMAMatch -> Int
getOptimalBlockSize (InnerMMAMatch _ _ sizeM sizeN _sizeK) =
--  TODO: Should also depend on types, check if this is actually always optimal
    let optimalWarpTileM = 64 in
    let optimalWarpTileN = 64 in
    let warpsM = sizeM `divUp` optimalWarpTileM in
    let warpsN = sizeN `divUp` optimalWarpTileN in
    warpsM * warpsN * 32



-- Pattern matching

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

innerSegOpExpMatch :: Scope GPU -> Exp GPU -> Maybe InnerMMAMatch
innerSegOpExpMatch scope (Op op) = innerOpMatch scope op
innerSegOpExpMatch _ _ = Nothing

innerOpMatch :: Scope GPU -> Op GPU -> Maybe InnerMMAMatch
innerOpMatch scope
-- TODO: check if better to match segmap with inner reduction
  (SegOp segRed@(
      SegRed (SegThreadInBlock _) space segBinOps _ts body
  ))
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
innerOpMatch _ _ = Nothing

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
inBlockKernelBodyMatch indexVars@[_indexVar1, _indexVar2, indexVar3] freeVars (KernelBody _ stms [Returns _ _ (Var res)]) scope = do
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
segBinOpsMatch [SegBinOp Commutative lambda nes _] | lambdaMatch lambda = nesMatch nes
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
    "Extracts NVIDIA tensor core MMA operations" $
    intraproceduralTransformationWithConsts pure fixFuns
    >=> passFunction simplifyGPUMem

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
  | gemmName `isPrefixOfName` funDefName fun =
      pure $
        fun
          { funDefParams = fixParamsGemmFun $ funDefParams fun,
            funDefRetType = fixRetType Scalar $ funDefRetType fun
          }
  | copyGlobalSharedName `isPrefixOfName` funDefName fun =
      pure $
        fun
          { funDefParams = fixParamsCopyGlobalShared $ funDefParams fun,
            funDefRetType = fixRetType Shared $ funDefRetType fun
          }
  | copyRegistersSharedName `isPrefixOfName` funDefName fun =
      pure $
        fun
          { funDefParams = fixParamsCopyRegistersShared $ funDefParams fun,
            funDefRetType = fixRetType Shared $ funDefRetType fun
          }
  | otherwise = do
      let initScope = scopeOf consts <> scopeOfFParams (funDefParams fun)
      let body = funDefBody fun
      stms' <- fixStmtsWithScope initScope . bodyStms $ body
      pure $ fun {funDefBody = body {bodyStms = stms'}}


fixParamsCopyGlobalShared :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsCopyGlobalShared (
    Param attrs1 vName1 (MemMem (Space "device")) :
    Param attrs2 vName2 (MemMem (Space "device")) :
    rest
  ) =
    Param attrs1 vName1 (MemMem (Space "device")) :
    Param attrs2 vName2 (MemMem (Space "shared")) :
    rest
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
fixStmts = mapStmsWithScope fixStmt


-- TODO: fix rets without breaking subsequent code, maybe do in explicit allocations pass instead
-- TODO: pass layout info here?
fixStmt :: Stm GPUMem -> FixMonad (Stms GPUMem)
fixStmt stm@(Let (Pat [PatElem resName (MemArray _ _ _ (ArrayIn resMem _))]) _ (BasicOp (Manifest _ inputName))) = do
  info <- lookupInfo inputName
  case info of
    --    TODO: match more cases
    LetName (MemArray _ _ _ (ArrayIn inputMem _)) -> do
      modify ([(resName, inputName), (resMem, inputMem)] <>)
      defaultFixStm stm
    _ -> defaultFixStm stm
fixStmt (
  Let (Pat [
    PatElem vName1 _,
    PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
  ])
  aux
  (Apply fName args rets info)) | gemmName `isPrefixOfName` fName = do
  let space = ScalarSpace (shapeDims shp2) t2
  let newRets = fixRetType Scalar rets
  newArgs <- mapM replaceArg args
  pure $ oneStm $
    Let (Pat [
      PatElem vName1 (MemMem space),
      PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
    ])
    aux
    (Apply fName newArgs newRets info)
fixStmt (
  Let (Pat [
    PatElem vName1 _,
    PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
  ])
  aux
  (Apply fName args rets info)) | copyGlobalSharedName `isPrefixOfName` fName = do
  let space = Space "shared"
--  TODO: check if need to handle uniqueness/consumption
  let newRets = fixRetType Shared rets
  newArgs <- mapM replaceArg args
--  TODO: check this, maybe use case instead
  let ((Var srcMemMem, _) : _ : (Var srcArray, _) : _restArgs) = newArgs
  srcMemInfo <- lookupInfo srcMemMem
  case srcMemInfo of
    LetName (MemMem srcMemSpace) | srcMemSpace == space ->
      pure $ stmsFromList
        [
          Let (Pat [
                PatElem vName1 (MemMem space)
              ])
              aux
              $ BasicOp $ SubExp $ Var srcMemMem,
          Let (Pat [
                PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
              ])
              aux
              $ BasicOp $ SubExp $ Var srcArray
        ]
    _ ->
      pure $ oneStm $
        Let (Pat [
          PatElem vName1 (MemMem space),
          PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
        ])
        aux
        $ Apply fName newArgs newRets info
-- TODO: check this
fixStmt (
  Let (Pat [
    PatElem vName1 _,
    PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
  ])
  aux
  (Apply fName args rets info)) | copyRegistersSharedName `isPrefixOfName` fName = do
  let space = Space "shared"
  let newRets = fixRetType Shared rets
  newArgs <- mapM replaceArg args
  pure $ oneStm $
    Let (Pat [
      PatElem vName1 (MemMem space),
      PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
    ])
    aux
    (Apply fName newArgs newRets info)
fixStmt stm = defaultFixStm stm

defaultFixStm :: Stm GPUMem -> FixMonad (Stms GPUMem)
defaultFixStm (Let pat aux e) = do
  e' <- fixExp e
  pure $ oneStm $ Let pat aux e'


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
