{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Segmented operations.  These correspond to perfect @map@ nests on
-- top of /something/, except that the @map@s are conceptually only
-- over @iota@s (so there will be explicit indexing inside them).
module Futhark.IR.SegOp
  ( SegOp(..)
  , SegVirt(..)
  , segLevel
  , segSpace
  , typeCheckSegOp
  , SegSpace(..)
  , scopeOfSegSpace
  , segSpaceDims

    -- * Details
  , HistOp(..)
  , histType
  , SegBinOp(..)
  , segBinOpResults
  , segBinOpChunks
  , KernelBody(..)
  , aliasAnalyseKernelBody
  , consumedInKernelBody
  , ResultManifest(..)
  , KernelResult(..)
  , kernelResultSubExp
  , SplitOrdering(..)

    -- ** Generic traversal
  , SegOpMapper(..)
  , identitySegOpMapper
  , mapSegOpM

    -- * Simplification
  , simplifySegOp
  , HasSegOp(..)
  , segOpRules

    -- * Memory
  , segOpReturns
  )
where

import Control.Monad.State.Strict
import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Identity hiding (mapM_)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
  (intersperse, foldl', partition, isPrefixOf, groupBy)

import Futhark.IR
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Util.Pretty as PP
import Futhark.Util.Pretty
  ((</>), (<+>), ppr, commasep, Pretty, parens, text)
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplify.Lore
import Futhark.IR.Prop.Aliases
import Futhark.IR.Aliases
  (Aliases, removeLambdaAliases, removeStmAliases)
import Futhark.IR.Mem
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics
import Futhark.Util (maybeNth, chunks)
import Futhark.Optimise.Simplify.Rule
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Tools

-- | How an array is split into chunks.
data SplitOrdering = SplitContiguous
                   | SplitStrided SubExp
                   deriving (Eq, Ord, Show)

instance FreeIn SplitOrdering where
  freeIn' SplitContiguous = mempty
  freeIn' (SplitStrided stride) = freeIn' stride

instance Substitute SplitOrdering where
  substituteNames _ SplitContiguous =
    SplitContiguous
  substituteNames subst (SplitStrided stride) =
    SplitStrided $ substituteNames subst stride

instance Rename SplitOrdering where
  rename SplitContiguous =
    pure SplitContiguous
  rename (SplitStrided stride) =
    SplitStrided <$> rename stride

-- | An operator for 'SegHist'.
data HistOp lore =
  HistOp { histWidth :: SubExp
         , histRaceFactor :: SubExp
         , histDest :: [VName]
         , histNeutral :: [SubExp]
         , histShape :: Shape
           -- ^ In case this operator is semantically a vectorised
           -- operator (corresponding to a perfect map nest in the
           -- SOACS representation), these are the logical
           -- "dimensions".  This is used to generate more efficient
           -- code.
         , histOp :: Lambda lore
         }
  deriving (Eq, Ord, Show)

-- | The type of a histogram produced by a 'HistOp'.  This can be
-- different from the type of the 'histDest's in case we are
-- dealing with a segmented histogram.
histType :: HistOp lore -> [Type]
histType op = map ((`arrayOfRow` histWidth op) .
                        (`arrayOfShape` histShape op)) $
                   lambdaReturnType $ histOp op

-- | An operator for 'SegScan' and 'SegRed'.
data SegBinOp lore =
  SegBinOp { segBinOpComm :: Commutativity
           , segBinOpLambda :: Lambda lore
           , segBinOpNeutral :: [SubExp]
           , segBinOpShape :: Shape
             -- ^ In case this operator is semantically a vectorised
             -- operator (corresponding to a perfect map nest in the
             -- SOACS representation), these are the logical
             -- "dimensions".  This is used to generate more efficient
             -- code.
           }
  deriving (Eq, Ord, Show)

-- | How many reduction results are produced by these 'SegBinOp's?
segBinOpResults :: [SegBinOp lore] -> Int
segBinOpResults = sum . map (length . segBinOpNeutral)

-- | Split some list into chunks equal to the number of values
-- returned by each 'SegBinOp'
segBinOpChunks :: [SegBinOp lore] -> [a] -> [[a]]
segBinOpChunks = chunks . map (length . segBinOpNeutral)

-- | The body of a 'SegOp'.
data KernelBody lore = KernelBody { kernelBodyLore :: BodyDec lore
                                  , kernelBodyStms :: Stms lore
                                  , kernelBodyResult :: [KernelResult]
                                  }

deriving instance Decorations lore => Ord (KernelBody lore)
deriving instance Decorations lore => Show (KernelBody lore)
deriving instance Decorations lore => Eq (KernelBody lore)

-- | Metadata about whether there is a subtle point to this
-- 'KernelResult'.  This is used to protect things like tiling, which
-- might otherwise be removed by the simplifier because they're
-- semantically redundant.  This has no semantic effect and can be
-- ignored at code generation.
data ResultManifest
  = ResultNoSimplify
    -- ^ Don't simplify this one!
  | ResultMaySimplify
    -- ^ Go nuts.
  | ResultPrivate
    -- ^ The results produced are only used within the
    -- same physical thread later on, and can thus be
    -- kept in registers.
  deriving (Eq, Show, Ord)

-- | A 'KernelBody' does not return an ordinary 'Result'.  Instead, it
-- returns a list of these.
data KernelResult = Returns ResultManifest SubExp
                    -- ^ Each "worker" in the kernel returns this.
                    -- Whether this is a result-per-thread or a
                    -- result-per-group depends on where the 'SegOp' occurs.
                  | WriteReturns
                    [SubExp] -- Size of array.  Must match number of dims.
                    VName -- Which array
                    [(Slice SubExp, SubExp)]
                    -- Arbitrary number of index/value pairs.
                  | ConcatReturns
                    SplitOrdering -- Permuted?
                    SubExp -- The final size.
                    SubExp -- Per-thread/group (max) chunk size.
                    VName -- Chunk by this worker.
                  | TileReturns
                    [(SubExp, SubExp)] -- Total/tile for each dimension
                    VName -- Tile written by this worker.
                    -- The TileReturns must not expect more than one
                    -- result to be written per physical thread.
                  deriving (Eq, Show, Ord)

-- | Get the root t'SubExp' corresponding values for a 'KernelResult'.
kernelResultSubExp :: KernelResult -> SubExp
kernelResultSubExp (Returns _ se) = se
kernelResultSubExp (WriteReturns _ arr _) = Var arr
kernelResultSubExp (ConcatReturns _ _ _ v) = Var v
kernelResultSubExp (TileReturns _ v) = Var v

instance FreeIn KernelResult where
  freeIn' (Returns _ what) = freeIn' what
  freeIn' (WriteReturns rws arr res) = freeIn' rws <> freeIn' arr <> freeIn' res
  freeIn' (ConcatReturns o w per_thread_elems v) =
    freeIn' o <> freeIn' w <> freeIn' per_thread_elems <> freeIn' v
  freeIn' (TileReturns dims v) =
    freeIn' dims <> freeIn' v

instance ASTLore lore => FreeIn (KernelBody lore) where
  freeIn' (KernelBody dec stms res) =
    fvBind bound_in_stms $ freeIn' dec <> freeIn' stms <> freeIn' res
    where bound_in_stms = foldMap boundByStm stms

instance ASTLore lore => Substitute (KernelBody lore) where
  substituteNames subst (KernelBody dec stms res) =
    KernelBody
    (substituteNames subst dec)
    (substituteNames subst stms)
    (substituteNames subst res)

instance Substitute KernelResult where
  substituteNames subst (Returns manifest se) =
    Returns manifest (substituteNames subst se)
  substituteNames subst (WriteReturns rws arr res) =
    WriteReturns
    (substituteNames subst rws) (substituteNames subst arr)
    (substituteNames subst res)
  substituteNames subst (ConcatReturns o w per_thread_elems v) =
    ConcatReturns
    (substituteNames subst o)
    (substituteNames subst w)
    (substituteNames subst per_thread_elems)
    (substituteNames subst v)
  substituteNames subst (TileReturns dims v) =
    TileReturns (substituteNames subst dims) (substituteNames subst v)

instance ASTLore lore => Rename (KernelBody lore) where
  rename (KernelBody dec stms res) = do
    dec' <- rename dec
    renamingStms stms $ \stms' ->
      KernelBody dec' stms' <$> rename res

instance Rename KernelResult where
  rename = substituteRename

-- | Perform alias analysis on a 'KernelBody'.
aliasAnalyseKernelBody :: (ASTLore lore,
                           CanBeAliased (Op lore)) =>
                          KernelBody lore
                       -> KernelBody (Aliases lore)
aliasAnalyseKernelBody (KernelBody dec stms res) =
  let Body dec' stms' _ = Alias.analyseBody mempty $ Body dec stms []
  in KernelBody dec' stms' res

removeKernelBodyAliases :: CanBeAliased (Op lore) =>
                           KernelBody (Aliases lore) -> KernelBody lore
removeKernelBodyAliases (KernelBody (_, dec) stms res) =
  KernelBody dec (fmap removeStmAliases stms) res

removeKernelBodyWisdom :: CanBeWise (Op lore) =>
                          KernelBody (Wise lore) -> KernelBody lore
removeKernelBodyWisdom (KernelBody dec stms res) =
  let Body dec' stms' _ = removeBodyWisdom $ Body dec stms []
  in KernelBody dec' stms' res

-- | The variables consumed in the kernel body.
consumedInKernelBody :: Aliased lore =>
                        KernelBody lore -> Names
consumedInKernelBody (KernelBody dec stms res) =
  consumedInBody (Body dec stms []) <> mconcat (map consumedByReturn res)
  where consumedByReturn (WriteReturns _ a _) = oneName a
        consumedByReturn _                    = mempty

checkKernelBody :: TC.Checkable lore =>
                   [Type] -> KernelBody (Aliases lore) -> TC.TypeM lore ()
checkKernelBody ts (KernelBody (_, dec) stms kres) = do
  TC.checkBodyLore dec
  TC.checkStms stms $ do
    unless (length ts == length kres) $
      TC.bad $ TC.TypeError $ "Kernel return type is " ++ prettyTuple ts ++
      ", but body returns " ++ show (length kres) ++ " values."
    zipWithM_ checkKernelResult kres ts

  where checkKernelResult (Returns _ what) t =
          TC.require [t] what
        checkKernelResult (WriteReturns rws arr res) t = do
          mapM_ (TC.require [Prim int32]) rws
          arr_t <- lookupType arr
          forM_ res $ \(slice, e) -> do
            mapM_ (traverse $ TC.require [Prim int32]) slice
            TC.require [t] e
            unless (arr_t == t `arrayOfShape` Shape rws) $
              TC.bad $ TC.TypeError $ "WriteReturns returning " ++
              pretty e ++ " of type " ++ pretty t ++ ", shape=" ++ pretty rws ++
              ", but destination array has type " ++ pretty arr_t
          TC.consume =<< TC.lookupAliases arr
        checkKernelResult (ConcatReturns o w per_thread_elems v) t = do
          case o of
            SplitContiguous     -> return ()
            SplitStrided stride -> TC.require [Prim int32] stride
          TC.require [Prim int32] w
          TC.require [Prim int32] per_thread_elems
          vt <- lookupType v
          unless (vt == t `arrayOfRow` arraySize 0 vt) $
            TC.bad $ TC.TypeError $ "Invalid type for ConcatReturns " ++ pretty v
        checkKernelResult (TileReturns dims v) t = do
          forM_ dims $ \(dim, tile) -> do
            TC.require [Prim int32] dim
            TC.require [Prim int32] tile
          vt <- lookupType v
          unless (vt == t `arrayOfShape` Shape (map snd dims)) $
            TC.bad $ TC.TypeError $ "Invalid type for TileReturns " ++ pretty v

kernelBodyMetrics :: OpMetrics (Op lore) => KernelBody lore -> MetricsM ()
kernelBodyMetrics = mapM_ stmMetrics . kernelBodyStms

instance PrettyLore lore => Pretty (KernelBody lore) where
  ppr (KernelBody _ stms res) =
    PP.stack (map ppr (stmsToList stms)) </>
    text "return" <+> PP.braces (PP.commasep $ map ppr res)

instance Pretty KernelResult where
  ppr (Returns ResultNoSimplify what) =
    text "returns (manifest)" <+> ppr what
  ppr (Returns ResultPrivate what) =
    text "returns (private)" <+> ppr what
  ppr (Returns ResultMaySimplify what) =
    text "returns" <+> ppr what
  ppr (WriteReturns rws arr res) =
    ppr arr <+> text "with" <+> PP.apply (map ppRes res)
    where ppRes (is, e) =
            PP.brackets (PP.commasep $ zipWith f is rws) <+> text "<-" <+> ppr e
          f i rw = ppr i <+> text "<" <+> ppr rw
  ppr (ConcatReturns o w per_thread_elems v) =
    text "concat" <> suff <>
    parens (commasep [ppr w, ppr per_thread_elems]) <+> ppr v
    where suff = case o of SplitContiguous     -> mempty
                           SplitStrided stride -> text "Strided" <> parens (ppr stride)
  ppr (TileReturns dims v) =
    text "tile" <>
    parens (commasep $ map onDim dims) <+> ppr v
    where onDim (dim, tile) = ppr dim <+> text "/" <+> ppr tile


-- | Do we need group-virtualisation when generating code for the
-- segmented operation?  In most cases, we do, but for some simple
-- kernels, we compute the full number of groups in advance, and then
-- virtualisation is an unnecessary (but generally very small)
-- overhead.  This only really matters for fairly trivial but very
-- wide @map@ kernels where each thread performs constant-time work on
-- scalars.
data SegVirt
  = SegVirt
  | SegNoVirt
  | SegNoVirtFull
    -- ^ Not only do we not need virtualisation, but we _guarantee_
    -- that all physical threads participate in the work.  This can
    -- save some checks in code generation.
  deriving (Eq, Ord, Show)

-- | Index space of a 'SegOp'.
data SegSpace = SegSpace { segFlat :: VName
                         -- ^ Flat physical index corresponding to the
                         -- dimensions (at code generation used for a
                         -- thread ID or similar).
                         , unSegSpace :: [(VName, SubExp)]
                         }
              deriving (Eq, Ord, Show)


-- | The sizes spanned by the indexes of the 'SegSpace'.
segSpaceDims :: SegSpace -> [SubExp]
segSpaceDims (SegSpace _ space) = map snd space

-- | A 'Scope' containing all the identifiers brought into scope by
-- this 'SegSpace'.
scopeOfSegSpace :: SegSpace -> Scope lore
scopeOfSegSpace (SegSpace phys space) =
  M.fromList $ zip (phys : map fst space) $ repeat $ IndexName Int32

checkSegSpace :: TC.Checkable lore => SegSpace -> TC.TypeM lore ()
checkSegSpace (SegSpace _ dims) =
  mapM_ (TC.require [Prim int32] . snd) dims

-- | A 'SegOp' is semantically a perfectly nested stack of maps, on
-- top of some bottommost computation (scalar computation, reduction,
-- scan, or histogram).  The 'SegSpace' encodes the original map
-- structure.
--
-- All 'SegOp's are parameterised by the representation of their body,
-- as well as a *level*.  The *level* is a representation-specific bit
-- of information.  For example, in GPU backends, it is used to
-- indicate whether the 'SegOp' is expected to run at the thread-level
-- or the group-level.
data SegOp lvl lore
  = SegMap lvl SegSpace [Type] (KernelBody lore)
  | SegRed lvl SegSpace [SegBinOp lore] [Type] (KernelBody lore)
    -- ^ The KernelSpace must always have at least two dimensions,
    -- implying that the result of a SegRed is always an array.
  | SegScan lvl SegSpace [SegBinOp lore] [Type] (KernelBody lore)
  | SegHist lvl SegSpace [HistOp lore] [Type] (KernelBody lore)
  deriving (Eq, Ord, Show)

-- | The level of a 'SegOp'.
segLevel :: SegOp lvl lore -> lvl
segLevel (SegMap lvl _ _ _) = lvl
segLevel (SegRed lvl _ _ _ _) = lvl
segLevel (SegScan lvl _ _ _ _) = lvl
segLevel (SegHist lvl _ _ _ _) = lvl

-- | The space of a 'SegOp'.
segSpace :: SegOp lvl lore -> SegSpace
segSpace (SegMap _ lvl _ _) = lvl
segSpace (SegRed _ lvl _ _ _) = lvl
segSpace (SegScan _ lvl _ _ _) = lvl
segSpace (SegHist _ lvl _ _ _) = lvl

segResultShape :: SegSpace -> Type -> KernelResult -> Type
segResultShape _ t (WriteReturns rws _ _) =
  t `arrayOfShape` Shape rws
segResultShape space t (Returns _ _) =
  foldr (flip arrayOfRow) t $ segSpaceDims space
segResultShape _ t (ConcatReturns _ w _ _) =
  t `arrayOfRow` w
segResultShape _ t (TileReturns dims _) =
  t `arrayOfShape` Shape (map fst dims)

-- | The return type of a 'SegOp'.
segOpType :: SegOp lvl lore -> [Type]
segOpType (SegMap _ space ts kbody) =
  zipWith (segResultShape space) ts $ kernelBodyResult kbody
segOpType (SegRed _ space reds ts kbody) =
  red_ts ++
  zipWith (segResultShape space) map_ts
  (drop (length red_ts) $ kernelBodyResult kbody)
  where map_ts = drop (length red_ts) ts
        segment_dims = init $ segSpaceDims space
        red_ts = do
          op <- reds
          let shape = Shape segment_dims <> segBinOpShape op
          map (`arrayOfShape` shape) (lambdaReturnType $ segBinOpLambda op)
segOpType (SegScan _ space scans ts kbody) =
  scan_ts ++
  zipWith (segResultShape space) map_ts
  (drop (length scan_ts) $ kernelBodyResult kbody)
  where map_ts = drop (length scan_ts) ts
        scan_ts = do
          op <- scans
          let shape = Shape (segSpaceDims space) <> segBinOpShape op
          map (`arrayOfShape` shape) (lambdaReturnType $ segBinOpLambda op)
segOpType (SegHist _ space ops _ _) = do
  op <- ops
  let shape = Shape (segment_dims <> [histWidth op]) <> histShape op
  map (`arrayOfShape` shape) (lambdaReturnType $ histOp op)
  where dims = segSpaceDims space
        segment_dims = init dims

instance TypedOp (SegOp lvl lore) where
  opType = pure . staticShapes . segOpType

instance (ASTLore lore, Aliased lore, ASTConstraints lvl) =>
         AliasedOp (SegOp lvl lore) where
  opAliases = map (const mempty) . segOpType

  consumedInOp (SegMap _ _ _ kbody) =
    consumedInKernelBody kbody
  consumedInOp (SegRed _ _ _ _ kbody) =
    consumedInKernelBody kbody
  consumedInOp (SegScan _ _ _ _ kbody) =
    consumedInKernelBody kbody
  consumedInOp (SegHist _ _ ops _ kbody) =
    namesFromList (concatMap histDest ops) <> consumedInKernelBody kbody

-- | Type check a 'SegOp', given a checker for its level.
typeCheckSegOp :: TC.Checkable lore =>
                  (lvl -> TC.TypeM lore ())
               -> SegOp lvl (Aliases lore) -> TC.TypeM lore ()
typeCheckSegOp checkLvl (SegMap lvl space ts kbody) = do
  checkLvl lvl
  checkScanRed space [] ts kbody

typeCheckSegOp checkLvl (SegRed lvl space reds ts body) = do
  checkLvl lvl
  checkScanRed space reds' ts body
  where reds' = zip3
                (map segBinOpLambda reds)
                (map segBinOpNeutral reds)
                (map segBinOpShape reds)

typeCheckSegOp checkLvl (SegScan lvl space scans ts body) = do
  checkLvl lvl
  checkScanRed space scans' ts body
  where scans' = zip3
                 (map segBinOpLambda scans)
                 (map segBinOpNeutral scans)
                 (map segBinOpShape scans)

typeCheckSegOp checkLvl (SegHist lvl space ops ts kbody) = do
  checkLvl lvl
  checkSegSpace space
  mapM_ TC.checkType ts

  TC.binding (scopeOfSegSpace space) $ do
    nes_ts <- forM ops $ \(HistOp dest_w rf dests nes shape op) -> do
      TC.require [Prim int32] dest_w
      TC.require [Prim int32] rf
      nes' <- mapM TC.checkArg nes
      mapM_ (TC.require [Prim int32]) $ shapeDims shape

      -- Operator type must match the type of neutral elements.
      let stripVecDims = stripArray $ shapeRank shape
      TC.checkLambda op $ map (TC.noArgAliases . first stripVecDims) $ nes' ++ nes'
      let nes_t = map TC.argType nes'
      unless (nes_t == lambdaReturnType op) $
        TC.bad $ TC.TypeError $ "SegHist operator has return type " ++
        prettyTuple (lambdaReturnType op) ++ " but neutral element has type " ++
        prettyTuple nes_t

      -- Arrays must have proper type.
      let dest_shape = Shape (segment_dims <> [dest_w]) <> shape
      forM_ (zip nes_t dests) $ \(t, dest) -> do
        TC.requireI [t `arrayOfShape` dest_shape] dest
        TC.consume =<< TC.lookupAliases dest

      return $ map (`arrayOfShape` shape) nes_t

    checkKernelBody ts kbody

    -- Return type of bucket function must be an index for each
    -- operation followed by the values to write.
    let bucket_ret_t = replicate (length ops) (Prim int32) ++ concat nes_ts
    unless (bucket_ret_t == ts) $
      TC.bad $ TC.TypeError $ "SegHist body has return type " ++
      prettyTuple ts ++ " but should have type " ++
      prettyTuple bucket_ret_t

  where segment_dims = init $ segSpaceDims space

checkScanRed :: TC.Checkable lore =>
                SegSpace
             -> [(Lambda (Aliases lore), [SubExp], Shape)]
             -> [Type]
             -> KernelBody (Aliases lore)
             -> TC.TypeM lore ()
checkScanRed space ops ts kbody = do
  checkSegSpace space
  mapM_ TC.checkType ts

  TC.binding (scopeOfSegSpace space) $ do
    ne_ts <- forM ops $ \(lam, nes, shape) -> do
      mapM_ (TC.require [Prim int32]) $ shapeDims shape
      nes' <- mapM TC.checkArg nes

      -- Operator type must match the type of neutral elements.
      TC.checkLambda lam $ map TC.noArgAliases $ nes' ++ nes'
      let nes_t = map TC.argType nes'

      unless (lambdaReturnType lam == nes_t) $
        TC.bad $ TC.TypeError "wrong type for operator or neutral elements."

      return $ map (`arrayOfShape` shape) nes_t

    let expecting = concat ne_ts
        got = take (length expecting) ts
    unless (expecting == got) $
      TC.bad $ TC.TypeError $
      "Wrong return for body (does not match neutral elements; expected " ++
      pretty expecting ++ "; found " ++
      pretty got ++ ")"

    checkKernelBody ts kbody

-- | Like 'Mapper', but just for 'SegOp's.
data SegOpMapper lvl flore tlore m = SegOpMapper {
    mapOnSegOpLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnSegOpBody :: KernelBody flore -> m (KernelBody tlore)
  , mapOnSegOpVName :: VName -> m VName
  , mapOnSegOpLevel :: lvl -> m lvl
  }

mapOnSegOpSubExp :: Monad m => SegOpMapper lvl flore tlore m -> SubExp -> m SubExp
mapOnSegOpSubExp _ (Constant x) = pure $ Constant x
mapOnSegOpSubExp tv (Var v) = Var <$> mapOnSegOpVName tv v

-- | A mapper that simply returns the 'SegOp' verbatim.
identitySegOpMapper :: Monad m => SegOpMapper lvl lore lore m
identitySegOpMapper = SegOpMapper { mapOnSegOpLambda = return
                                  , mapOnSegOpBody = return
                                  , mapOnSegOpVName = return
                                  , mapOnSegOpLevel = return
                                  }

mapOnSegSpace :: Monad f =>
                 SegOpMapper lvl flore tlore f -> SegSpace -> f SegSpace
mapOnSegSpace tv (SegSpace phys dims) =
  SegSpace phys <$> traverse (traverse $ mapOnSegOpSubExp tv) dims

mapSegBinOp :: Monad m =>
               SegOpMapper lvl flore tlore m
            -> SegBinOp flore -> m (SegBinOp tlore)
mapSegBinOp tv (SegBinOp comm red_op nes shape) =
  SegBinOp comm
  <$> mapOnSegOpLambda tv red_op
  <*> mapM (mapOnSegOpSubExp tv) nes
  <*> (Shape <$> mapM (mapOnSegOpSubExp tv) (shapeDims shape))

-- | Apply a 'SegOpMapper' to the given 'SegOp'.
mapSegOpM :: (Applicative m, Monad m) =>
             SegOpMapper lvl flore tlore m
          -> SegOp lvl flore -> m (SegOp lvl tlore)
mapSegOpM tv (SegMap lvl space ts body) =
  SegMap
  <$> mapOnSegOpLevel tv lvl
  <*> mapOnSegSpace tv space
  <*> mapM (mapOnSegOpType tv) ts
  <*> mapOnSegOpBody tv body
mapSegOpM tv (SegRed lvl space reds ts lam) =
  SegRed
  <$> mapOnSegOpLevel tv lvl
  <*> mapOnSegSpace tv space
  <*> mapM (mapSegBinOp tv) reds
  <*> mapM (mapOnType $ mapOnSegOpSubExp tv) ts
  <*> mapOnSegOpBody tv lam
mapSegOpM tv (SegScan lvl space scans ts body) =
  SegScan
  <$> mapOnSegOpLevel tv lvl
  <*> mapOnSegSpace tv space
  <*> mapM (mapSegBinOp tv) scans
  <*> mapM (mapOnType $ mapOnSegOpSubExp tv) ts
  <*> mapOnSegOpBody tv body
mapSegOpM tv (SegHist lvl space ops ts body) =
  SegHist
  <$> mapOnSegOpLevel tv lvl
  <*> mapOnSegSpace tv space
  <*> mapM onHistOp ops
  <*> mapM (mapOnType $ mapOnSegOpSubExp tv) ts
  <*> mapOnSegOpBody tv body
  where onHistOp (HistOp w rf arrs nes shape op) =
          HistOp <$> mapOnSegOpSubExp tv w
          <*> mapOnSegOpSubExp tv rf
          <*> mapM (mapOnSegOpVName tv) arrs
          <*> mapM (mapOnSegOpSubExp tv) nes
          <*> (Shape <$> mapM (mapOnSegOpSubExp tv) (shapeDims shape))
          <*> mapOnSegOpLambda tv op

mapOnSegOpType :: Monad m =>
                  SegOpMapper lvl flore tlore m -> Type -> m Type
mapOnSegOpType _tv (Prim pt) = pure $ Prim pt
mapOnSegOpType tv (Array pt shape u) = Array pt <$> f shape <*> pure u
  where f (Shape dims) = Shape <$> mapM (mapOnSegOpSubExp tv) dims
mapOnSegOpType _tv (Mem s) = pure $ Mem s

instance (ASTLore lore, Substitute lvl) =>
         Substitute (SegOp lvl lore) where
  substituteNames subst = runIdentity . mapSegOpM substitute
    where substitute =
            SegOpMapper { mapOnSegOpLambda = return . substituteNames subst
                        , mapOnSegOpBody = return . substituteNames subst
                        , mapOnSegOpVName = return . substituteNames subst
                        , mapOnSegOpLevel = return . substituteNames subst
                        }

instance (ASTLore lore, ASTConstraints lvl) =>
         Rename (SegOp lvl lore) where
  rename = mapSegOpM renamer
    where renamer = SegOpMapper rename rename rename rename

instance (ASTLore lore, FreeIn (LParamInfo lore), FreeIn lvl) =>
         FreeIn (SegOp lvl lore) where
  freeIn' e = flip execState mempty $ mapSegOpM free e
    where walk f x = modify (<>f x) >> return x
          free = SegOpMapper { mapOnSegOpLambda = walk freeIn'
                             , mapOnSegOpBody = walk freeIn'
                             , mapOnSegOpVName = walk freeIn'
                             , mapOnSegOpLevel = walk freeIn'
                             }

instance OpMetrics (Op lore) => OpMetrics (SegOp lvl lore) where
  opMetrics (SegMap _ _ _ body) =
    inside "SegMap" $ kernelBodyMetrics body
  opMetrics (SegRed _ _ reds _ body) =
    inside "SegRed" $ do mapM_ (lambdaMetrics . segBinOpLambda) reds
                         kernelBodyMetrics body
  opMetrics (SegScan _ _ scans _ body) =
    inside "SegScan" $ do mapM_ (lambdaMetrics . segBinOpLambda) scans
                          kernelBodyMetrics body
  opMetrics (SegHist _ _ ops _ body) =
    inside "SegHist" $ do mapM_ (lambdaMetrics . histOp) ops
                          kernelBodyMetrics body

instance Pretty SegSpace where
  ppr (SegSpace phys dims) = parens (commasep $ do (i,d) <- dims
                                                   return $ ppr i <+> "<" <+> ppr d) <+>
                             parens (text "~" <> ppr phys)

instance PrettyLore lore => Pretty (SegBinOp lore) where
  ppr (SegBinOp comm lam nes shape) =
    PP.braces (PP.commasep $ map ppr nes) <> PP.comma </>
    ppr shape <> PP.comma </>
    comm' <> ppr lam
    where comm' = case comm of Commutative -> text "commutative "
                               Noncommutative -> mempty

instance (PrettyLore lore, PP.Pretty lvl) => PP.Pretty (SegOp lvl lore) where
  ppr (SegMap lvl space ts body) =
    text "segmap" <> ppr lvl </>
    PP.align (ppr space) <+>
    PP.colon <+> ppTuple' ts <+> PP.nestedBlock "{" "}" (ppr body)

  ppr (SegRed lvl space reds ts body) =
    text "segred" <> ppr lvl </>
    PP.parens (PP.braces (mconcat $ intersperse (PP.comma <> PP.line) $ map ppr reds)) </>
    PP.align (ppr space) <+> PP.colon <+> ppTuple' ts <+>
    PP.nestedBlock "{" "}" (ppr body)

  ppr (SegScan lvl space scans ts body) =
    text "segscan" <> ppr lvl </>
    PP.parens (PP.braces (mconcat $ intersperse (PP.comma <> PP.line) $ map ppr scans)) </>
    PP.align (ppr space) <+> PP.colon <+> ppTuple' ts <+>
    PP.nestedBlock "{" "}" (ppr body)

  ppr (SegHist lvl space ops ts body) =
    text "seghist" <> ppr lvl </>
    ppr lvl </>
    PP.parens (PP.braces (mconcat $ intersperse (PP.comma <> PP.line) $ map ppOp ops)) </>
    PP.align (ppr space) <+> PP.colon <+> ppTuple' ts <+>
    PP.nestedBlock "{" "}" (ppr body)
    where ppOp (HistOp w rf dests nes shape op) =
            ppr w <> PP.comma <+> ppr rf <> PP.comma </>
            PP.braces (PP.commasep $ map ppr dests) <> PP.comma </>
            PP.braces (PP.commasep $ map ppr nes) <> PP.comma </>
            ppr shape <> PP.comma </>
            ppr op

instance (ASTLore lore, ASTLore (Aliases lore),
          CanBeAliased (Op lore), ASTConstraints lvl) =>
         CanBeAliased (SegOp lvl lore) where
  type OpWithAliases (SegOp lvl lore) = SegOp lvl (Aliases lore)

  addOpAliases = runIdentity . mapSegOpM alias
    where alias = SegOpMapper (return . Alias.analyseLambda)
                  (return . aliasAnalyseKernelBody) return return

  removeOpAliases = runIdentity . mapSegOpM remove
    where remove = SegOpMapper (return . removeLambdaAliases)
                   (return . removeKernelBodyAliases) return return

instance (CanBeWise (Op lore), ASTLore lore, ASTConstraints lvl) =>
         CanBeWise (SegOp lvl lore) where
  type OpWithWisdom (SegOp lvl lore) = SegOp lvl (Wise lore)

  removeOpWisdom = runIdentity . mapSegOpM remove
    where remove = SegOpMapper
                   (return . removeLambdaWisdom)
                   (return . removeKernelBodyWisdom)
                   return return

instance ASTLore lore => ST.IndexOp (SegOp lvl lore) where
  indexOp vtable k (SegMap _ space _ kbody) is = do
    Returns ResultMaySimplify se <- maybeNth k $ kernelBodyResult kbody
    guard $ length gtids <= length is
    let idx_table = M.fromList $ zip gtids $ map (ST.Indexed mempty) is
        idx_table' = foldl expandIndexedTable idx_table $ kernelBodyStms kbody
    case se of
      Var v -> M.lookup v idx_table'
      _ -> Nothing

    where (gtids, _) = unzip $ unSegSpace space
          -- Indexes in excess of what is used to index through the
          -- segment dimensions.
          excess_is = drop (length gtids) is

          expandIndexedTable table stm
            | [v] <- patternNames $ stmPattern stm,
              Just (pe,cs) <-
                  runWriterT $ primExpFromExp (asPrimExp table) $ stmExp stm =
                M.insert v (ST.Indexed (stmCerts stm <> cs) pe) table

            | [v] <- patternNames $ stmPattern stm,
              BasicOp (Index arr slice) <- stmExp stm,
              length (sliceDims slice) == length excess_is,
              arr `ST.elem` vtable,
              Just (slice', cs) <- asPrimExpSlice table slice =
                let idx = ST.IndexedArray (stmCerts stm <> cs)
                          arr (fixSlice slice' excess_is)
                in M.insert v idx table

            | otherwise =
                table

          asPrimExpSlice table =
            runWriterT . mapM (traverse (primExpFromSubExpM (asPrimExp table)))

          asPrimExp table v
            | Just (ST.Indexed cs e) <- M.lookup v table = tell cs >> return e
            | Just (Prim pt) <- ST.lookupType v vtable =
                return $ LeafExp v pt
            | otherwise = lift Nothing

  indexOp _ _ _ _ = Nothing

instance (ASTLore lore, ASTConstraints lvl) =>
         IsOp (SegOp lvl lore) where
  cheapOp _ = False
  safeOp _ = True

--- Simplification

instance Engine.Simplifiable SplitOrdering where
  simplify SplitContiguous =
    return SplitContiguous
  simplify (SplitStrided stride) =
    SplitStrided <$> Engine.simplify stride

instance Engine.Simplifiable SegSpace where
  simplify (SegSpace phys dims) =
    SegSpace phys <$> mapM (traverse Engine.simplify) dims

instance Engine.Simplifiable KernelResult where
  simplify (Returns manifest what) =
    Returns manifest <$> Engine.simplify what
  simplify (WriteReturns ws a res) =
    WriteReturns <$> Engine.simplify ws <*> Engine.simplify a <*> Engine.simplify res
  simplify (ConcatReturns o w pte what) =
    ConcatReturns
    <$> Engine.simplify o
    <*> Engine.simplify w
    <*> Engine.simplify pte
    <*> Engine.simplify what
  simplify (TileReturns dims what) =
    TileReturns <$> Engine.simplify dims <*> Engine.simplify what

mkWiseKernelBody :: (ASTLore lore, CanBeWise (Op lore)) =>
                    BodyDec lore -> Stms (Wise lore) -> [KernelResult] -> KernelBody (Wise lore)
mkWiseKernelBody dec bnds res =
  let Body dec' _ _ = mkWiseBody dec bnds res_vs
  in KernelBody dec' bnds res
  where res_vs = map kernelResultSubExp res

mkKernelBodyM :: MonadBinder m =>
                 Stms (Lore m) -> [KernelResult]
              -> m (KernelBody (Lore m))
mkKernelBodyM stms kres = do
  Body dec' _ _ <- mkBodyM stms res_ses
  return $ KernelBody dec' stms kres
  where res_ses = map kernelResultSubExp kres

simplifyKernelBody :: (Engine.SimplifiableLore lore, BodyDec lore ~ ()) =>
                      SegSpace -> KernelBody lore
                   -> Engine.SimpleM lore (KernelBody (Wise lore), Stms (Wise lore))
simplifyKernelBody space (KernelBody _ stms res) = do
  par_blocker <- Engine.asksEngineEnv $ Engine.blockHoistPar . Engine.envHoistBlockers

  ((body_stms, body_res), hoisted) <-
    Engine.localVtable (<>scope_vtable) $
    Engine.localVtable (\vtable -> vtable { ST.simplifyMemory = True }) $
    Engine.blockIf (Engine.hasFree bound_here
                    `Engine.orIf` Engine.isOp
                    `Engine.orIf` par_blocker
                    `Engine.orIf` Engine.isConsumed) $
    Engine.simplifyStms stms $ do
    res' <- Engine.localVtable (ST.hideCertified $ namesFromList $ M.keys $ scopeOf stms) $
            mapM Engine.simplify res
    return ((res', UT.usages $ freeIn res'), mempty)

  return (mkWiseKernelBody () body_stms body_res, hoisted)

  where scope_vtable = segSpaceSymbolTable space
        bound_here = namesFromList $ M.keys $ scopeOfSegSpace space

segSpaceSymbolTable :: ASTLore lore => SegSpace -> ST.SymbolTable lore
segSpaceSymbolTable (SegSpace flat gtids_and_dims) =
  foldl' f (ST.fromScope $ M.singleton flat $ IndexName Int32) gtids_and_dims
  where f vtable (gtid, dim) = ST.insertLoopVar gtid Int32 dim vtable

simplifySegBinOp :: Engine.SimplifiableLore lore =>
                    SegBinOp lore
                 -> Engine.SimpleM lore (SegBinOp (Wise lore), Stms (Wise lore))
simplifySegBinOp (SegBinOp comm lam nes shape) = do
  (lam', hoisted) <-
    Engine.localVtable (\vtable -> vtable { ST.simplifyMemory = True }) $
    Engine.simplifyLambda lam
  shape' <- Engine.simplify shape
  nes' <- mapM Engine.simplify nes
  return (SegBinOp comm lam' nes' shape', hoisted)

-- | Simplify the given 'SegOp'.
simplifySegOp :: (Engine.SimplifiableLore lore,
                  BodyDec lore ~ (),
                  Engine.Simplifiable lvl) =>
                 SegOp lvl lore
              -> Engine.SimpleM lore (SegOp lvl (Wise lore), Stms (Wise lore))
simplifySegOp (SegMap lvl space ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (kbody', body_hoisted) <- simplifyKernelBody space kbody
  return (SegMap lvl' space' ts' kbody',
          body_hoisted)

simplifySegOp (SegRed lvl space reds ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (reds', reds_hoisted) <- Engine.localVtable (<>scope_vtable) $
    unzip <$> mapM simplifySegBinOp reds
  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return (SegRed lvl' space' reds' ts' kbody',
          mconcat reds_hoisted <> body_hoisted)
  where scope = scopeOfSegSpace space
        scope_vtable = ST.fromScope scope

simplifySegOp (SegScan lvl space scans ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (scans', scans_hoisted) <- Engine.localVtable (<>scope_vtable) $
    unzip <$> mapM simplifySegBinOp scans
  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return (SegScan lvl' space' scans' ts' kbody',
          mconcat scans_hoisted <> body_hoisted)
  where scope = scopeOfSegSpace space
        scope_vtable = ST.fromScope scope

simplifySegOp (SegHist lvl space ops ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)

  (ops', ops_hoisted) <- fmap unzip $ forM ops $
    \(HistOp w rf arrs nes dims lam) -> do
      w' <- Engine.simplify w
      rf' <- Engine.simplify rf
      arrs' <- Engine.simplify arrs
      nes' <- Engine.simplify nes
      dims' <- Engine.simplify dims
      (lam', op_hoisted) <-
        Engine.localVtable (<>scope_vtable) $
        Engine.localVtable (\vtable -> vtable { ST.simplifyMemory = True }) $
        Engine.simplifyLambda lam
      return (HistOp w' rf' arrs' nes' dims' lam',
              op_hoisted)

  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return (SegHist lvl' space' ops' ts' kbody',
          mconcat ops_hoisted <> body_hoisted)

  where scope = scopeOfSegSpace space
        scope_vtable = ST.fromScope scope

-- | Does this lore contain 'SegOp's in its t'Op's?  A lore must be an
-- instance of this class for the simplification rules to work.
class HasSegOp lore where
  type SegOpLevel lore
  asSegOp :: Op lore -> Maybe (SegOp (SegOpLevel lore) lore)
  segOp :: SegOp (SegOpLevel lore) lore -> Op lore

-- | Simplification rules for simplifying 'SegOp's.
segOpRules :: (HasSegOp lore, BinderOps lore, Bindable lore) =>
              RuleBook lore
segOpRules =
  ruleBook [ RuleOp segOpRuleTopDown ] [ RuleOp segOpRuleBottomUp ]

segOpRuleTopDown :: (HasSegOp lore, BinderOps lore, Bindable lore) =>
                    TopDownRuleOp lore
segOpRuleTopDown vtable pat dec op
  | Just op' <- asSegOp op =
      topDownSegOp vtable pat dec op'
  | otherwise =
      Skip

segOpRuleBottomUp :: (HasSegOp lore, BinderOps lore) =>
                     BottomUpRuleOp lore
segOpRuleBottomUp vtable pat dec op
  | Just op' <- asSegOp op =
      bottomUpSegOp vtable pat dec op'
  | otherwise =
      Skip

topDownSegOp :: (HasSegOp lore, BinderOps lore, Bindable lore) =>
                ST.SymbolTable lore
             -> Pattern lore
             -> StmAux (ExpDec lore)
             -> SegOp (SegOpLevel lore) lore
             -> Rule lore

-- If a SegOp produces something invariant to the SegOp, turn it
-- into a replicate.
topDownSegOp vtable (Pattern [] kpes) dec (SegMap lvl space ts (KernelBody _ kstms kres)) = Simplify $ do
  (ts', kpes', kres') <-
    unzip3 <$> filterM checkForInvarianceResult (zip3 ts kpes kres)

  -- Check if we did anything at all.
  when (kres == kres')
    cannotSimplify

  kbody <- mkKernelBodyM kstms kres'
  addStm $ Let (Pattern [] kpes') dec $ Op $ segOp $
    SegMap lvl space ts' kbody

  where isInvariant Constant{} = True
        isInvariant (Var v) = isJust $ ST.lookup v vtable

        checkForInvarianceResult (_, pe, Returns rm se)
          | rm == ResultMaySimplify,
            isInvariant se = do
              letBindNames [patElemName pe] $
                BasicOp $ Replicate (Shape $ segSpaceDims space) se
              return False
        checkForInvarianceResult _ =
          return True

-- If a SegRed contains two reduction operations that have the same
-- vector shape, merge them together.  This saves on communication
-- overhead, but can in principle lead to more local memory usage.
topDownSegOp _ (Pattern [] pes) _ (SegRed lvl space ops ts kbody)
  | length ops > 1,
    op_groupings <- groupBy sameShape $ zip ops $ chunks (map (length . segBinOpNeutral) ops) $
                    zip3 red_pes red_ts red_res,
    any ((>1) . length) op_groupings = Simplify $ do
      let (ops', aux) = unzip $ mapMaybe combineOps op_groupings
          (red_pes', red_ts', red_res') = unzip3 $ concat aux
          pes' = red_pes' ++ map_pes
          ts' = red_ts' ++ map_ts
          kbody' = kbody { kernelBodyResult = red_res' ++ map_res }
      letBind (Pattern [] pes') $ Op $ segOp $ SegRed lvl space ops' ts' kbody'
  where (red_pes, map_pes) = splitAt (segBinOpResults ops) pes
        (red_ts, map_ts) = splitAt (segBinOpResults ops) ts
        (red_res, map_res) = splitAt (segBinOpResults ops) $ kernelBodyResult kbody

        sameShape (op1, _) (op2, _) = segBinOpShape op1 == segBinOpShape op2

        combineOps [] = Nothing
        combineOps (x:xs) = Just $ foldl' combine x xs

        combine (op1, op1_aux) (op2, op2_aux) =
          let lam1 = segBinOpLambda op1
              lam2 = segBinOpLambda op2
              (op1_xparams, op1_yparams) =
                splitAt (length (segBinOpNeutral op1)) $ lambdaParams lam1
              (op2_xparams, op2_yparams) =
                splitAt (length (segBinOpNeutral op2)) $ lambdaParams lam2
              lam = Lambda { lambdaParams = op1_xparams ++ op2_xparams ++
                                            op1_yparams ++ op2_yparams
                           , lambdaReturnType = lambdaReturnType lam1 ++ lambdaReturnType lam2
                           , lambdaBody =
                               mkBody (bodyStms (lambdaBody lam1) <> bodyStms (lambdaBody lam2)) $
                               bodyResult (lambdaBody lam1) <> bodyResult (lambdaBody lam2)
                           }
          in (SegBinOp { segBinOpComm = segBinOpComm op1 <> segBinOpComm op2
                       , segBinOpLambda = lam
                       , segBinOpNeutral = segBinOpNeutral op1 ++ segBinOpNeutral op2
                       , segBinOpShape = segBinOpShape op1 -- Same as shape of op2 due to the grouping.
                       },
               op1_aux ++ op2_aux)
topDownSegOp _ _ _ _ = Skip

bottomUpSegOp :: (HasSegOp lore, BinderOps lore) =>
                 (ST.SymbolTable lore, UT.UsageTable)
              -> Pattern lore
              -> StmAux (ExpDec lore)
              -> SegOp (SegOpLevel lore) lore
              -> Rule lore

-- Some SegOp results can be moved outside the SegOp, which can
-- simplify further analysis.
bottomUpSegOp (vtable, used) (Pattern [] kpes) dec (SegMap lvl space kts (KernelBody _ kstms kres)) = Simplify $ do

  -- Iterate through the bindings.  For each, we check whether it is
  -- in kres and can be moved outside.  If so, we remove it from kres
  -- and kpes and make it a binding outside.
  (kpes', kts', kres', kstms') <- localScope (scopeOfSegSpace space) $
    foldM distribute (kpes, kts, kres, mempty) kstms

  when (kpes' == kpes)
    cannotSimplify

  kbody <- localScope (scopeOfSegSpace space) $
           mkKernelBodyM kstms' kres'

  addStm $ Let (Pattern [] kpes') dec $ Op $ segOp $
    SegMap lvl space kts' kbody
  where
    free_in_kstms = foldMap freeIn kstms

    sliceWithGtidsFixed stm
      | Let _ _ (BasicOp (Index arr slice)) <- stm,
        space_slice <- map (DimFix . Var . fst) $ unSegSpace space,
        space_slice `isPrefixOf` slice,
        remaining_slice <- drop (length space_slice) slice,
        all (isJust . flip ST.lookup vtable) $ namesToList $
          freeIn arr <> freeIn remaining_slice =
          Just (remaining_slice, arr)

      | otherwise =
          Nothing

    distribute (kpes', kts', kres', kstms') stm
      | Let (Pattern [] [pe]) _ _ <- stm,
        Just (remaining_slice, arr) <- sliceWithGtidsFixed stm,
        Just (kpe, kpes'', kts'', kres'') <- isResult kpes' kts' kres' pe = do
          let outer_slice = map (\d -> DimSlice
                                       (constant (0::Int32))
                                       d
                                       (constant (1::Int32))) $
                            segSpaceDims space
              index kpe' = letBind (Pattern [] [kpe']) $ BasicOp $ Index arr $
                           outer_slice <> remaining_slice
          if patElemName kpe `UT.isConsumed` used
            then do precopy <- newVName $ baseString (patElemName kpe) <> "_precopy"
                    index kpe { patElemName = precopy }
                    letBind (Pattern [] [kpe]) $ BasicOp $ Copy precopy
            else index kpe
          return (kpes'', kts'', kres'',
                  if patElemName pe `nameIn` free_in_kstms
                  then kstms' <> oneStm stm
                  else kstms')

    distribute (kpes', kts', kres', kstms') stm =
      return (kpes', kts', kres', kstms' <> oneStm stm)

    isResult kpes' kts' kres' pe =
      case partition matches $ zip3 kpes' kts' kres' of
        ([(kpe,_,_)], kpes_and_kres)
          | (kpes'', kts'', kres'') <- unzip3 kpes_and_kres ->
              Just (kpe, kpes'', kts'', kres'')
        _ -> Nothing
      where matches (_, _, Returns _ (Var v)) = v == patElemName pe
            matches _ = False
bottomUpSegOp _ _ _ _ = Skip

--- Memory

kernelBodyReturns :: (Mem lore, HasScope lore m, Monad m) =>
                     KernelBody lore -> [ExpReturns] -> m [ExpReturns]
kernelBodyReturns = zipWithM correct . kernelBodyResult
  where correct (WriteReturns _ arr _) _ = varReturns arr
        correct _ ret = return ret

-- | Like 'segOpType', but for memory representations.
segOpReturns :: (Mem lore, Monad m, HasScope lore m) =>
                SegOp lvl lore -> m [ExpReturns]
segOpReturns k@(SegMap _ _ _ kbody) =
  kernelBodyReturns kbody =<< (extReturns <$> opType k)
segOpReturns k@(SegRed _ _ _ _ kbody) =
  kernelBodyReturns kbody =<< (extReturns <$> opType k)
segOpReturns k@(SegScan _ _ _ _ kbody) =
  kernelBodyReturns kbody =<< (extReturns <$> opType k)
segOpReturns (SegHist _ _ ops _ _) =
  concat <$> mapM (mapM varReturns . histDest) ops
