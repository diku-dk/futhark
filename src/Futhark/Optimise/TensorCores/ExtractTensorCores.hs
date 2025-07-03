module Futhark.Optimise.TensorCores.ExtractTensorCores (transformProg) where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Bits
import Data.Foldable (toList)
import Data.List (elemIndex, intersect, partition)
import Data.Loc (Loc (NoLoc), SrcLoc (SrcLoc))
import Data.Map.Strict qualified as M
import Data.Semigroup
import Data.Set (difference, fromList)
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Builder
import Futhark.Construct
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.TensorCores.Utils
import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass (PassM)
import Prelude hiding (lookup)

-- | Divide and round up.
divUp :: Int -> Int -> Int
divUp x y = (x + y - 1) `div` y

-- Tensor core functions emitted (gemm, copy global shared, copy registers shared)
type TcFunDef = (MMMSignature, FunDef GPU)

type TcFuns = [TcFunDef]

data ExtractTcEnv = ExtractTcEnv
  { envScope :: Scope GPU,
    envBlockSize :: Maybe Int
  }

-- | Monad that the tensor core match and GPU IR code transformations runs within.
type TensorCoreM = RWS ExtractTcEnv TcFuns VNameSource

instance HasScope GPU TensorCoreM where
  askScope = asks envScope

instance LocalScope GPU TensorCoreM where
  localScope extension = local $
    \env -> env {envScope = M.union extension $ envScope env}

askBlockSize :: TensorCoreM (Maybe Int)
askBlockSize = asks envBlockSize

localBlockSize :: (Maybe Int -> Maybe Int) -> TensorCoreM a -> TensorCoreM a
localBlockSize f = local $ \env -> env {envBlockSize = f $ envBlockSize env}

runBuilderMMM :: Builder GPU a -> Scope GPU -> TensorCoreM (a, Stms GPU)
runBuilderMMM m s =
  modifyNameSource $ runState $ runBuilderT m s

-- | Create the gemm function defintion.
mkGemmFun ::
  (MonadFreshNames m) =>
  PrimType ->
  PrimType ->
  PrimType ->
  Int ->
  Int ->
  Int ->
  Int ->
  m TcFunDef
mkGemmFun elmTypeA elmTypeB elmTypeC sizeM sizeN sizeK sizeRegs = do
  let typeA =
        Array
          elmTypeA
          (Shape [mkInt64Const sizeM, mkInt64Const sizeK])
          Nonunique
      typeB =
        Array
          elmTypeB
          (Shape [mkInt64Const sizeK, mkInt64Const sizeN])
          Nonunique
      typeCin =
        Array
          elmTypeC
          (Shape [mkInt64Const sizeRegs])
          Unique
      typeCout =
        [ ( Array
              elmTypeC
              (Shape [Free $ mkInt64Const sizeRegs])
              Unique,
            RetAls [] []
          )
        ]

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
  aSwizzledParam <- newParam "aSwizzledParam" $ Prim int64
  bSwizzledParam <- newParam "bSwizzledParam" $ Prim int64

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
          nWarpsParam,
          aSwizzledParam,
          bSwizzledParam
        ]
  pure
    ( GemmSignature elmTypeA elmTypeB elmTypeC sizeM sizeN sizeK sizeRegs,
      FunDef Nothing mempty fName typeCout funParams $
        resultBody [Var $ paramName cParam]
    )

-- | Create the copy global shared function definition.
mkCopyGlobalShared :: (MonadFreshNames m) => PrimType -> Int -> Int -> m TcFunDef
mkCopyGlobalShared elmType sizeY sizeX = do
  let arrShape =
        Shape
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

-- | Create the copy from registers to shared memory definition.
mkCopyRegistersShared ::
  (MonadFreshNames m) =>
  PrimType ->
  PrimType ->
  PrimType ->
  Int ->
  Int ->
  Int ->
  Int ->
  m TcFunDef
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

  fName <-
    nameFromString . prettyString
      <$> newName (VName copyRegistersSharedName 0)

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

-- | Create some  shared (scratch) memory of the specified type and shape.
scratchMem :: PrimType -> [Int] -> Exp GPU
scratchMem elmType dims = BasicOp $ Scratch elmType $ map mkInt64Const dims

-- | Rebuild the matrix multiplication computation.  The SegRed will be
-- replae by calls to three tensor core related functions.
buildMMM :: VName -> Int -> TensorCoreMatch -> Builder GPU TcFuns
buildMMM
  resName
  actualBlockSize
  match@(TensorCoreMatch kernelBodyMatch ne sizeM sizeN sizeK) = do
    -- Get the best tile of warps given the block size.
    let (warpsM, warpsN) = getOptimalWarps actualBlockSize match
    let blockSize = warpsM * warpsN * 32
    let cValsPerThread = sizeM * sizeN `div` blockSize

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
    cRegs_list <-
      segMap1D "cRegs" thrdInBlock ResultPrivate (mkInt64Const blockSize) $ \_ -> do
        cScratch <- letExp "cScratch" $ scratchMem elmTypeC [cValsPerThread]
        cLoop <- forLoop (mkInt64Const cValsPerThread) [cScratch] $ \i [cMerge] -> do
          cZeroed <- update "cZeroed" cMerge [i] ne
          resultBodyM [Var cZeroed]
        pure [varRes cLoop]
    let [cRegs] = cRegs_list
    aScratch <- letExp "aScratch" $ scratchMem elmTypeA [sizeM, sizeK]
    bScratch <- letExp "bScratch" $ scratchMem elmTypeB [sizeK, sizeN]

    let innerIndecesASlice =
          [ DimSlice (mkInt64Const 0) (mkInt64Const sizeM) (mkInt64Const 1),
            DimSlice (mkInt64Const 0) (mkInt64Const sizeK) (mkInt64Const 1)
          ]
        innerIndecesBSlice =
          [ DimSlice (mkInt64Const 0) (mkInt64Const sizeK) (mkInt64Const 1),
            DimSlice (mkInt64Const 0) (mkInt64Const sizeN) (mkInt64Const 1)
          ]
    slicedA <-
      letExp "slicedA" $
        BasicOp $
          Index (arrA kernelBodyMatch) $
            Slice $
              fmap DimFix (outerIndecesA kernelBodyMatch) <> innerIndecesASlice
    slicedB <-
      letExp "slicedB" $
        BasicOp $
          Index (arrB kernelBodyMatch) $
            Slice $
              fmap DimFix (outerIndecesB kernelBodyMatch) <> innerIndecesBSlice

    --  Need to pass this explicitly as LMAD info is lost on function call
    let pe64DimsA =
          fmap pe64 $
            outerDimsA kernelBodyMatch
              <> [mkInt64Const sizeM, mkInt64Const sizeK]
        pe64IndiciesA =
          fmap pe64 $
            outerIndecesA kernelBodyMatch
              <> [mkInt64Const 0, mkInt64Const 0]
        pe64DimsB =
          fmap pe64 $
            outerDimsB kernelBodyMatch
              <> [mkInt64Const sizeK, mkInt64Const sizeN]
        pe64IndiciesB =
          fmap pe64 $
            outerIndecesB kernelBodyMatch
              <> [mkInt64Const 0, mkInt64Const 0]

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
            (mkInt64Const warpsN, ObservePrim)
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
            (mkInt64Const warpsN, ObservePrim)
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
          Safe

    bCopied <-
      letExp "bCopied" $
        Apply
          (funDefName $ snd copyGlobalSharedFunB)
          copyArgsB
          copyRetsB
          Safe

    let blksize = mkInt64Const blockSize
    inBlockMMAres_list <-
      segMap1D "inBlockMMAres" thrdInBlock ResultPrivate blksize $ \thread_idx -> do
        threadCregs <- index "threadCregs" cRegs [thread_idx]
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
                (mkInt64Const warpsN, ObservePrim),
                (mkInt64Const 1, ObservePrim),
                (mkInt64Const 1, ObservePrim)
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
              Safe
        pure [varRes threadMMAres]
    let [inBlockMMAres] = inBlockMMAres_list

    cScratch <- letExp "cScratch" $ scratchMem elmTypeC [sizeM, sizeN]
    let copyArgsC =
          [ (Var inBlockMMAres, ObservePrim),
            (Var cScratch, Consume),
            (Constant $ blankPrimValue $ typeA kernelBodyMatch, ObservePrim),
            (Constant $ blankPrimValue $ typeB kernelBodyMatch, ObservePrim),
            (mkInt64Const sizeM, ObservePrim),
            (mkInt64Const sizeN, ObservePrim),
            (mkInt64Const warpsM, ObservePrim),
            (mkInt64Const warpsN, ObservePrim)
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
          Safe

    letBindNames [resName] $ BasicOp $ SubExp $ Var cCopied
    pure addedFuns

-- Functions for traversing the input program and transforming the relevant
-- statement to use tensor cores.
transformProg :: Prog GPU -> PassM (Prog GPU)
transformProg (Prog opaqueTypes consts funs) = do
  (transformedFuns, mmmFuns) <-
    modifyNameSource $
      (\(a, s, w) -> ((a, w), s)) . runRWS (mapM transformFunDef funs) init_env
  let (_, addedFuns) = unzip mmmFuns
  pure $ Prog opaqueTypes consts (addedFuns <> transformedFuns)
  where
    init_env = ExtractTcEnv (scopeOf consts) Nothing

transformFunDef :: FunDef GPU -> TensorCoreM (FunDef GPU)
transformFunDef funDef@(FunDef entry attrs name retType params body) =
  FunDef entry attrs name retType params <$> inScopeOf funDef (transformBody body)

transformStms :: Stms GPU -> TensorCoreM (Stms GPU)
transformStms = mapStmsWithScope transformStm

transformStm :: Stm GPU -> TensorCoreM (Stms GPU)
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

transformStmDefault :: Stm GPU -> TensorCoreM (Stms GPU)
transformStmDefault (Let pat aux e) = do
  e' <- transformExp e
  pure $ oneStm $ Let pat aux e'

-- TODO: match WithAcc?
transformExp :: Exp GPU -> TensorCoreM (Exp GPU)
transformExp (Match subExps cases body matchDec) =
  Match subExps
    <$> mapM transformCase cases
    <*> transformBody body
    <*> pure matchDec
transformExp (Loop params form body) =
  localScope (scopeOfFParams (map fst params) <> scopeOfLoopForm form) $ do
    newBody <- transformBody body
    pure $ Loop params form newBody
transformExp (Op op) = Op <$> transformOp op
transformExp e = pure e

transformCase :: Case (Body GPU) -> TensorCoreM (Case (Body GPU))
transformCase (Case pat body) = Case pat <$> transformBody body

transformBody :: Body GPU -> TensorCoreM (Body GPU)
transformBody (Body dec stms res) =
  Body dec
    <$> transformStms stms
    <*> pure res

transformOp :: Op GPU -> TensorCoreM (Op GPU)
transformOp (SegOp sOp) = SegOp <$> transformSegOp sOp
transformOp op = pure op

-- | First we match an out SegMap to get the block size.  Later we
-- try to set the block size dependent on the matrix multiplication dims.
transformSegOp :: SegOp SegLevel GPU -> TensorCoreM (SegOp SegLevel GPU)
transformSegOp
  sOp@( SegMap
          ( SegBlock
              SegNoVirt
              (Just (KernelGrid (Count numBlocks) (Count _blockSize)))
            )
          space@(SegSpace _ _)
          ts
          body@(KernelBody _ stms _)
        ) = do
    scope <- askScope
    case execWriter $ runReaderT (maxBlockSizeStms stms) scope of
      Known (Max maxBlockSize) -> do
        transformedBody <-
          localBlockSize (const $ Just maxBlockSize) $
            transformKernelBody body
        let blocks = Count numBlocks
        let blocksize = Count $ mkInt64Const maxBlockSize
        let grid = KernelGrid blocks blocksize
        pure $ SegMap (SegBlock SegNoVirt (Just grid)) space ts transformedBody
      Unknown ->
        transformSegOpDefault sOp
transformSegOp sOp = transformSegOpDefault sOp

transformSegOpDefault :: SegOp SegLevel GPU -> TensorCoreM (SegOp SegLevel GPU)
transformSegOpDefault (SegMap level space ts body) =
  SegMap level space ts
    <$> transformKernelBody body
transformSegOpDefault (SegRed level space ts body ops) =
  SegRed level space ts <$> transformKernelBody body <*> pure ops
transformSegOpDefault (SegScan level space ts body ops) =
  SegScan level space ts <$> transformKernelBody body <*> pure ops
transformSegOpDefault (SegHist level space ts body histOps) =
  SegHist level space ts <$> transformKernelBody body <*> pure histOps

transformKernelBody :: KernelBody GPU -> TensorCoreM (KernelBody GPU)
transformKernelBody (KernelBody desc stms res) =
  KernelBody desc
    <$> transformStms stms
    <*> pure res

-- | Do we know the block size or not?
data KnownUnknown a = Known a | Unknown
  deriving (Show, Eq, Ord)

instance (Monoid a) => Monoid (KnownUnknown a) where
  mempty = Known mempty

instance (Semigroup a) => Semigroup (KnownUnknown a) where
  Known a <> Known b = Known $ a <> b
  _ <> _ = Unknown

type MaxBlockSizeM = ReaderT (Scope GPU) (Writer (KnownUnknown (Max Int)))

-- | Find the max block size required for the intragroup kernels.
-- This is needed in case more threads are needed than what is ideal for
-- matmul using the tensor cores.
maxBlockSizeWalker :: Walker GPU MaxBlockSizeM
maxBlockSizeWalker =
  (identityWalker @GPU)
    { walkOnOp = maxBlockSizeOp,
      walkOnBody = maxBlockSizeBody
    }

maxBlockSizeStms :: Stms GPU -> MaxBlockSizeM ()
maxBlockSizeStms = mapStmsWithScope maxBlockSizeStm

maxBlockSizeStm :: Stm GPU -> MaxBlockSizeM ()
maxBlockSizeStm (Let _ _ e) = maxBlockSizeExp e

maxBlockSizeExp :: Exp GPU -> MaxBlockSizeM ()
maxBlockSizeExp = walkExpM maxBlockSizeWalker

maxBlockSizeOp :: Op GPU -> MaxBlockSizeM ()
maxBlockSizeOp op = do
  scope <- askScope
  case (innerOpMatch scope op, op) of
    (Just match, _) -> do
      tell $ Known $ Max $ getOptimalBlockSize match
    (_, SegOp sOp)
      | (SegThreadInBlock _) <- segLevel sOp ->
          tell $ foldl prodKnownSegDim (Known 1) $ unSegSpace $ segSpace sOp
    _ -> pure ()

maxBlockSizeBody :: Scope GPU -> Body GPU -> MaxBlockSizeM ()
maxBlockSizeBody scope (Body _ stms _) = localScope scope $ maxBlockSizeStms stms

prodKnownSegDim ::
  KnownUnknown (Max Int) ->
  (VName, SubExp) ->
  KnownUnknown (Max Int)
prodKnownSegDim (Known (Max acc)) (_, Constant (IntValue n)) =
  Known $ Max $ acc * valueIntegral n
-- TODO: should lookup if variable dimension?
-- Currently we do not match if any dimensions are not statically known integers.
-- The only reason a lookup might be relevant is if the variable is bound to a
-- statically known variable. This might already be handled by constant folding.
prodKnownSegDim _ _ = Unknown

getFactorPairs :: Int -> [(Int, Int)]
getFactorPairs n = [(x, n `div` x) | x <- [1 .. n], n `mod` x == 0]

getRatio :: Int -> Int -> Float
getRatio m n = fromIntegral m / fromIntegral n

-- Note: This should in the future depend on the type.
-- F64 might require a different warp layout than f16 for best performance.
getOptimalWarps :: Int -> TensorCoreMatch -> (Int, Int)
getOptimalWarps blockSize (TensorCoreMatch _ _ sizeM sizeN _) =
  -- warp tiles will be as close to square as possible, which should maximize register reuse
  let Arg _ (warpsM, warpsN) = minimum $ map ratioDifference usedfactorPairs
   in (warpsM, warpsN)
  where
    minValsPerThread = 8
    maxBlockSize = (sizeM * sizeN) `div` minValsPerThread
    usedBlockSize = min blockSize maxBlockSize
    targetRatio = getRatio sizeM sizeN
    -- Minimum values per thread used for f16 MMA Atom
    usedfactorPairs = helper $ usedBlockSize `divUp` 32
    ratioDifference (x, y) = Arg (abs $ targetRatio - getRatio x y) (x, y)
    helper 0 = error "Could not find appropriate number of warps"
    helper numwarps =
      let factorPairs = getFactorPairs numwarps
       in if not $ any (\(x, y) -> (sizeM `div` 16) `mod` x == 0 && (sizeN `div` 16) `mod` y == 0) factorPairs
            then helper $ numwarps - 1
            else factorPairs

-- NOTE: In the future the optimal block size should depend on:
--  1. The array element type (f16 or f64)
--  2. Architecture
--  3. Type of program. Sometimes more threads can be good.
-- The current estimate might not be optimal in all cases.
getOptimalBlockSize :: TensorCoreMatch -> Int
getOptimalBlockSize (TensorCoreMatch _ _ sizeM sizeN _sizeK) =
  let optimalElmsPerWarp = 4096
   in ((sizeM * sizeN) `divUp` optimalElmsPerWarp) * 32

-- Pattern matching

data TensorCoreMatch = TensorCoreMatch
  { kernelBodyMatch :: KernelBodyMatch,
    ne :: SubExp,
    sizeM :: Int,
    sizeN :: Int,
    sizeK :: Int
  }
  deriving (Show, Eq, Ord)

-- NOTE: The only type currently supported is f16 for A and B and f16/f32 for C.
-- Future work should also support f64 or maybe even mixed u8/s32.
data KernelBodyMatch = KernelBodyMatch
  { innerIndecesA :: [VName],
    innerIndecesB :: [VName],
    outerIndecesA :: [SubExp],
    outerIndecesB :: [SubExp],
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

innerSegOpExpMatch :: Scope GPU -> Exp GPU -> Maybe TensorCoreMatch
innerSegOpExpMatch scope (Op op) = innerOpMatch scope op
innerSegOpExpMatch _ _ = Nothing

innerOpMatch :: Scope GPU -> Op GPU -> Maybe TensorCoreMatch
innerOpMatch
  scope
  ( SegOp
      segRed@(SegRed (SegThreadInBlock _) space _ts body segBinOps)
    )
    | Just ne <- segBinOpsMatch segBinOps =
        do
          let (dimVars, segDims) = unzip $ unSegSpace space
          let freeVars = freeIn segRed
          bodyMatch <- inBlockKernelBodyMatch dimVars freeVars body scope
          constSegDims <- mapM constantValueMatch segDims
          case constSegDims of
            [m, n, k]
              | all sizeMatches constSegDims ->
                  Just (TensorCoreMatch bodyMatch ne m n k)
            _ -> Nothing
innerOpMatch _ _ = Nothing

-- NOTE: This should definitely depend on the element type when more types are
-- supported. The current bounds fit within the shared memory when using f16
-- on the A100.
sizeMatches :: Int -> Bool
sizeMatches x =
  x `mod` 16 == 0
    && 0 < x
    && x <= 128
    --  Check if x is power of 2
    && popCount x == 1

constantValueMatch :: SubExp -> Maybe Int
constantValueMatch (Constant (IntValue v)) = Just $ valueIntegral v
constantValueMatch _ = Nothing

-- Does the list of indexing variables only have a single dimension?
singleDim :: [d] -> Maybe d
singleDim [v] = Just v
singleDim _ = Nothing

hasCorrectMatMulOperandType :: Type -> PrimType -> Bool
hasCorrectMatMulOperandType (Array typ _ _) pt = typ == pt
hasCorrectMatMulOperandType _ _ = False

-- The indexVars corresponds to the variables from the SegSpace
inBlockKernelBodyMatch ::
  [VName] ->
  Names ->
  KernelBody GPU ->
  Scope GPU ->
  Maybe KernelBodyMatch
inBlockKernelBodyMatch
  indexVars@[_, _, indexVar3]
  freeVars
  (KernelBody _ stms [Returns _ _ (Var res)])
  scope = do
    let f16_type = FloatType Float16
    let sTable = ST.insertStms (informStms stms) $ ST.fromScope $ addScopeWisdom scope
    (resExp, _) <- ST.lookupExp res sTable
    -- Check that the result is optionally converted from f16->f32.
    -- In case it is not, then since the program type checks and the
    -- arrays are asserted to be f16, then all operators must be f16
    resWithoutConversion <- case resExp of
      BasicOp (ConvOp (FPConv Float16 Float32) (Var converted)) -> do
        (convertedExp, _) <- ST.lookupExp converted sTable
        pure convertedExp
      notConvertedExp ->
        pure notConvertedExp
    (mulArg1, mulArg2) <- matchesMul $ removeExpWisdom resWithoutConversion
    (mulArg1Exp, _) <- ST.lookupExp mulArg1 sTable
    (mulArg2Exp, _) <- ST.lookupExp mulArg2 sTable
    (arr1, slice1) <- matchesMulArg $ removeExpWisdom mulArg1Exp
    (arr2, slice2) <- matchesMulArg $ removeExpWisdom mulArg2Exp

    -- For now we only support mixed f16/f32 tensor core operations.
    -- Therefore the array operands A and B in C = A @ B must be f16.
    arr1Type <- ST.lookupType arr1 sTable
    arr2Type <- ST.lookupType arr2 sTable
    guard $ hasCorrectMatMulOperandType arr1Type f16_type
    guard $ hasCorrectMatMulOperandType arr2Type f16_type

    resType <- ST.lookupType res sTable
    slice1' <- mapM dimFix $ unSlice slice1
    slice2' <- mapM dimFix $ unSlice slice2
    let seIndexVars = map Var indexVars
    let (seInnerIndeces1, outerIndeces1) = partition (`elem` seIndexVars) slice1'
    let (seInnerIndeces2, outerIndeces2) = partition (`elem` seIndexVars) slice2'
    let outerIndeces = outerIndeces1 <> outerIndeces2
    innerIndeces1 <- mapM getIndexVar seInnerIndeces1
    innerIndeces2 <- mapM getIndexVar seInnerIndeces2
    -- Check that each array has one unique (n or m) and one commen (k) dimension
    -- as the inner dimensions of the intragroup kernel
    k <- singleDim $ innerIndeces1 `intersect` innerIndeces2
    n <- singleDim $ toList $ fromList innerIndeces1 `difference` fromList innerIndeces2
    m <- singleDim $ toList $ fromList innerIndeces2 `difference` fromList innerIndeces1
    -- TODO: Do we maybe want to allow something that is not matrix multiplication?
    -- It would just require us to "not" transpose B in CuTe
    -- In the meantime, this checks where in the indexing slice k appears.
    -- For B it must be [n, k] and for A it must be [k, n]
    elemIndex k innerIndeces1 >>= guard . (== 1) -- [m, k] matrix
    elemIndex k innerIndeces2 >>= guard . (== 0) -- [k, n] matrix
    case (arr1Type, arr2Type, resType) of
      (Array type1 (Shape arr1Dims) _, Array type2 (Shape arr2Dims) _, Prim resTypePrim)
        | k == indexVar3 && all (`subExpFreeIn` freeVars) outerIndeces ->
            let arr1OuterDims = take (length arr1Dims - 2) arr1Dims
             in let arr2OuterDims = take (length arr2Dims - 2) arr2Dims
                 in Just
                      ( KernelBodyMatch
                          innerIndeces1
                          innerIndeces2
                          outerIndeces1
                          outerIndeces2
                          arr1OuterDims
                          arr2OuterDims
                          arr1
                          arr2
                          m
                          n
                          k
                          type1
                          type2
                          resTypePrim
                      )
      _ -> Nothing
inBlockKernelBodyMatch _ _ _ _ = Nothing

getIndexVar :: SubExp -> Maybe VName
getIndexVar (Var v) = Just v
getIndexVar _ = Nothing

-- A bit weird, but we also count constants as free
subExpFreeIn :: SubExp -> Names -> Bool
subExpFreeIn (Constant _) _ = True
subExpFreeIn (Var v) names = v `nameIn` names

matchesMul :: Exp GPU -> Maybe (VName, VName)
matchesMul (BasicOp (BinOp (FMul _) (Var arg1) (Var arg2))) = Just (arg1, arg2)
matchesMul _ = Nothing

matchesMulArg :: Exp GPU -> Maybe (VName, Slice SubExp)
matchesMulArg (BasicOp (Index v s)) = Just (v, s)
matchesMulArg _ = Nothing

segBinOpsMatch :: [SegBinOp GPU] -> Maybe SubExp
segBinOpsMatch [SegBinOp Commutative lambda nes _]
  | lambdaMatch lambda = nesMatch nes
segBinOpsMatch _ = Nothing

lambdaMatch :: Lambda GPU -> Bool
lambdaMatch (Lambda [Param _ arg1 _, Param _ arg2 _] _ body) =
  lambdaBodyMatch arg1 arg2 body
lambdaMatch _ = False

lambdaBodyMatch :: VName -> VName -> Body GPU -> Bool
lambdaBodyMatch arg1 arg2 (Body _ stms [SubExpRes _ (Var v)]) =
  any (lambdaStmMatch arg1 arg2 v) stms
lambdaBodyMatch _ _ _ = False

lambdaStmMatch :: VName -> VName -> VName -> Stm GPU -> Bool
lambdaStmMatch
  arg1
  arg2
  v
  ( Let
      (Pat [PatElem v' _])
      _
      ( BasicOp
          (BinOp (FAdd _) (Var arg1') (Var arg2'))
        )
    ) =
    v == v' && arg1 == arg1' && arg2 == arg2'
lambdaStmMatch
  arg1
  arg2
  v
  ( Let
      (Pat [PatElem v' _])
      _
      (BasicOp (BinOp (Add _ _) (Var arg1') (Var arg2')))
    ) =
    v == v' && arg1 == arg1' && arg2 == arg2'
lambdaStmMatch _ _ _ _ = False

nesMatch :: [SubExp] -> Maybe SubExp
nesMatch [s@(Constant v)] | zeroIsh v = Just s
nesMatch _ = Nothing
