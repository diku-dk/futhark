{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Segmented operations.  These correspond to perfect @map@ nests on
-- top of /something/, except that the @map@s are conceptually only
-- over @iota@s (so there will be explicit indexing inside them).
module Futhark.IR.SegOp
  ( SegOp (..),
    SegVirt (..),
    SegSeqDims (..),
    segLevel,
    segBody,
    segSpace,
    typeCheckSegOp,
    SegSpace (..),
    scopeOfSegSpace,
    segSpaceDims,

    -- * Details
    HistOp (..),
    histType,
    splitHistResults,
    SegBinOp (..),
    segBinOpResults,
    segBinOpChunks,
    KernelBody (..),
    aliasAnalyseKernelBody,
    consumedInKernelBody,
    ResultManifest (..),
    KernelResult (..),
    kernelResultCerts,
    kernelResultSubExp,
    SplitOrdering (..),

    -- ** Generic traversal
    SegOpMapper (..),
    identitySegOpMapper,
    mapSegOpM,
    traverseSegOpStms,

    -- * Simplification
    simplifySegOp,
    HasSegOp (..),
    segOpRules,

    -- * Memory
    segOpReturns,
  )
where

import Control.Category
import Control.Monad.Identity hiding (mapM_)
import Control.Monad.State.Strict
import Control.Monad.Writer hiding (mapM_)
import Data.Bifunctor (first)
import Data.Bitraversable
import Data.Foldable (traverse_)
import Data.List
  ( elemIndex,
    foldl',
    groupBy,
    intersperse,
    isPrefixOf,
    partition,
  )
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Analysis.Metrics
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.IR
import Futhark.IR.Aliases
  ( Aliases,
    removeLambdaAliases,
    removeStmAliases,
  )
import Futhark.IR.Mem
import Futhark.IR.Prop.Aliases
import qualified Futhark.IR.TypeCheck as TC
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (chunks, maybeNth)
import Futhark.Util.Pretty
  ( Pretty,
    commasep,
    parens,
    ppr,
    text,
    (<+>),
    (</>),
  )
import qualified Futhark.Util.Pretty as PP
import Prelude hiding (id, (.))

-- | How an array is split into chunks.
data SplitOrdering
  = SplitContiguous
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
data HistOp rep = HistOp
  { histShape :: Shape,
    histRaceFactor :: SubExp,
    histDest :: [VName],
    histNeutral :: [SubExp],
    -- | In case this operator is semantically a vectorised
    -- operator (corresponding to a perfect map nest in the
    -- SOACS representation), these are the logical
    -- "dimensions".  This is used to generate more efficient
    -- code.
    histOpShape :: Shape,
    histOp :: Lambda rep
  }
  deriving (Eq, Ord, Show)

-- | The type of a histogram produced by a 'HistOp'.  This can be
-- different from the type of the 'histDest's in case we are
-- dealing with a segmented histogram.
histType :: HistOp rep -> [Type]
histType op =
  map (`arrayOfShape` (histShape op <> histOpShape op)) $
    lambdaReturnType $ histOp op

-- | Split reduction results returned by a 'KernelBody' into those
-- that correspond to indexes for the 'HistOps', and those that
-- correspond to value.
splitHistResults :: [HistOp rep] -> [SubExp] -> [([SubExp], [SubExp])]
splitHistResults ops res =
  let ranks = map (shapeRank . histShape) ops
      (idxs, vals) = splitAt (sum ranks) res
   in zip
        (chunks ranks idxs)
        (chunks (map (length . histDest) ops) vals)

-- | An operator for 'SegScan' and 'SegRed'.
data SegBinOp rep = SegBinOp
  { segBinOpComm :: Commutativity,
    segBinOpLambda :: Lambda rep,
    segBinOpNeutral :: [SubExp],
    -- | In case this operator is semantically a vectorised
    -- operator (corresponding to a perfect map nest in the
    -- SOACS representation), these are the logical
    -- "dimensions".  This is used to generate more efficient
    -- code.
    segBinOpShape :: Shape
  }
  deriving (Eq, Ord, Show)

-- | How many reduction results are produced by these 'SegBinOp's?
segBinOpResults :: [SegBinOp rep] -> Int
segBinOpResults = sum . map (length . segBinOpNeutral)

-- | Split some list into chunks equal to the number of values
-- returned by each 'SegBinOp'
segBinOpChunks :: [SegBinOp rep] -> [a] -> [[a]]
segBinOpChunks = chunks . map (length . segBinOpNeutral)

-- | The body of a 'SegOp'.
data KernelBody rep = KernelBody
  { kernelBodyDec :: BodyDec rep,
    kernelBodyStms :: Stms rep,
    kernelBodyResult :: [KernelResult]
  }

deriving instance RepTypes rep => Ord (KernelBody rep)

deriving instance RepTypes rep => Show (KernelBody rep)

deriving instance RepTypes rep => Eq (KernelBody rep)

-- | Metadata about whether there is a subtle point to this
-- 'KernelResult'.  This is used to protect things like tiling, which
-- might otherwise be removed by the simplifier because they're
-- semantically redundant.  This has no semantic effect and can be
-- ignored at code generation.
data ResultManifest
  = -- | Don't simplify this one!
    ResultNoSimplify
  | -- | Go nuts.
    ResultMaySimplify
  | -- | The results produced are only used within the
    -- same physical thread later on, and can thus be
    -- kept in registers.
    ResultPrivate
  deriving (Eq, Show, Ord)

-- | A 'KernelBody' does not return an ordinary 'Result'.  Instead, it
-- returns a list of these.
data KernelResult
  = -- | Each "worker" in the kernel returns this.
    -- Whether this is a result-per-thread or a
    -- result-per-group depends on where the 'SegOp' occurs.
    Returns ResultManifest Certs SubExp
  | WriteReturns
      Certs
      Shape -- Size of array.  Must match number of dims.
      VName -- Which array
      [(Slice SubExp, SubExp)]
  | -- Arbitrary number of index/value pairs.
    ConcatReturns
      Certs
      SplitOrdering -- Permuted?
      SubExp -- The final size.
      SubExp -- Per-thread/group (max) chunk size.
      VName -- Chunk by this worker.
  | TileReturns
      Certs
      [(SubExp, SubExp)] -- Total/tile for each dimension
      VName -- Tile written by this worker.
      -- The TileReturns must not expect more than one
      -- result to be written per physical thread.
  | RegTileReturns
      Certs
      -- For each dim of result:
      [ ( SubExp, -- size of this dim.
          SubExp, -- block tile size for this dim.
          SubExp -- reg tile size for this dim.
        )
      ]
      VName -- Tile returned by this worker/group.
  deriving (Eq, Show, Ord)

-- | Get the certs for this 'KernelResult'.
kernelResultCerts :: KernelResult -> Certs
kernelResultCerts (Returns _ cs _) = cs
kernelResultCerts (WriteReturns cs _ _ _) = cs
kernelResultCerts (ConcatReturns cs _ _ _ _) = cs
kernelResultCerts (TileReturns cs _ _) = cs
kernelResultCerts (RegTileReturns cs _ _) = cs

-- | Get the root t'SubExp' corresponding values for a 'KernelResult'.
kernelResultSubExp :: KernelResult -> SubExp
kernelResultSubExp (Returns _ _ se) = se
kernelResultSubExp (WriteReturns _ _ arr _) = Var arr
kernelResultSubExp (ConcatReturns _ _ _ _ v) = Var v
kernelResultSubExp (TileReturns _ _ v) = Var v
kernelResultSubExp (RegTileReturns _ _ v) = Var v

instance FreeIn KernelResult where
  freeIn' (Returns _ cs what) = freeIn' cs <> freeIn' what
  freeIn' (WriteReturns cs rws arr res) = freeIn' cs <> freeIn' rws <> freeIn' arr <> freeIn' res
  freeIn' (ConcatReturns cs o w per_thread_elems v) =
    freeIn' cs <> freeIn' o <> freeIn' w <> freeIn' per_thread_elems <> freeIn' v
  freeIn' (TileReturns cs dims v) =
    freeIn' cs <> freeIn' dims <> freeIn' v
  freeIn' (RegTileReturns cs dims_n_tiles v) =
    freeIn' cs <> freeIn' dims_n_tiles <> freeIn' v

instance ASTRep rep => FreeIn (KernelBody rep) where
  freeIn' (KernelBody dec stms res) =
    fvBind bound_in_stms $ freeIn' dec <> freeIn' stms <> freeIn' res
    where
      bound_in_stms = foldMap boundByStm stms

instance ASTRep rep => Substitute (KernelBody rep) where
  substituteNames subst (KernelBody dec stms res) =
    KernelBody
      (substituteNames subst dec)
      (substituteNames subst stms)
      (substituteNames subst res)

instance Substitute KernelResult where
  substituteNames subst (Returns manifest cs se) =
    Returns manifest (substituteNames subst cs) (substituteNames subst se)
  substituteNames subst (WriteReturns cs rws arr res) =
    WriteReturns
      (substituteNames subst cs)
      (substituteNames subst rws)
      (substituteNames subst arr)
      (substituteNames subst res)
  substituteNames subst (ConcatReturns cs o w per_thread_elems v) =
    ConcatReturns
      (substituteNames subst cs)
      (substituteNames subst o)
      (substituteNames subst w)
      (substituteNames subst per_thread_elems)
      (substituteNames subst v)
  substituteNames subst (TileReturns cs dims v) =
    TileReturns
      (substituteNames subst cs)
      (substituteNames subst dims)
      (substituteNames subst v)
  substituteNames subst (RegTileReturns cs dims_n_tiles v) =
    RegTileReturns
      (substituteNames subst cs)
      (substituteNames subst dims_n_tiles)
      (substituteNames subst v)

instance ASTRep rep => Rename (KernelBody rep) where
  rename (KernelBody dec stms res) = do
    dec' <- rename dec
    renamingStms stms $ \stms' ->
      KernelBody dec' stms' <$> rename res

instance Rename KernelResult where
  rename = substituteRename

-- | Perform alias analysis on a 'KernelBody'.
aliasAnalyseKernelBody ::
  ( ASTRep rep,
    CanBeAliased (Op rep)
  ) =>
  AliasTable ->
  KernelBody rep ->
  KernelBody (Aliases rep)
aliasAnalyseKernelBody aliases (KernelBody dec stms res) =
  let Body dec' stms' _ = Alias.analyseBody aliases $ Body dec stms []
   in KernelBody dec' stms' res

removeKernelBodyAliases ::
  CanBeAliased (Op rep) =>
  KernelBody (Aliases rep) ->
  KernelBody rep
removeKernelBodyAliases (KernelBody (_, dec) stms res) =
  KernelBody dec (fmap removeStmAliases stms) res

removeKernelBodyWisdom ::
  CanBeWise (Op rep) =>
  KernelBody (Wise rep) ->
  KernelBody rep
removeKernelBodyWisdom (KernelBody dec stms res) =
  let Body dec' stms' _ = removeBodyWisdom $ Body dec stms []
   in KernelBody dec' stms' res

-- | The variables consumed in the kernel body.
consumedInKernelBody ::
  Aliased rep =>
  KernelBody rep ->
  Names
consumedInKernelBody (KernelBody dec stms res) =
  consumedInBody (Body dec stms []) <> mconcat (map consumedByReturn res)
  where
    consumedByReturn (WriteReturns _ _ a _) = oneName a
    consumedByReturn _ = mempty

checkKernelBody ::
  TC.Checkable rep =>
  [Type] ->
  KernelBody (Aliases rep) ->
  TC.TypeM rep ()
checkKernelBody ts (KernelBody (_, dec) stms kres) = do
  TC.checkBodyDec dec
  -- We consume the kernel results (when applicable) before
  -- type-checking the stms, so we will get an error if a statement
  -- uses an array that is written to in a result.
  mapM_ consumeKernelResult kres
  TC.checkStms stms $ do
    unless (length ts == length kres) $
      TC.bad $
        TC.TypeError $
          "Kernel return type is " ++ prettyTuple ts
            ++ ", but body returns "
            ++ show (length kres)
            ++ " values."
    zipWithM_ checkKernelResult kres ts
  where
    consumeKernelResult (WriteReturns _ _ arr _) =
      TC.consume =<< TC.lookupAliases arr
    consumeKernelResult _ =
      pure ()

    checkKernelResult (Returns _ cs what) t = do
      TC.checkCerts cs
      TC.require [t] what
    checkKernelResult (WriteReturns cs shape arr res) t = do
      TC.checkCerts cs
      mapM_ (TC.require [Prim int64]) $ shapeDims shape
      arr_t <- lookupType arr
      forM_ res $ \(slice, e) -> do
        traverse_ (TC.require [Prim int64]) slice
        TC.require [t] e
        unless (arr_t == t `arrayOfShape` shape) $
          TC.bad $
            TC.TypeError $
              "WriteReturns returning "
                ++ pretty e
                ++ " of type "
                ++ pretty t
                ++ ", shape="
                ++ pretty shape
                ++ ", but destination array has type "
                ++ pretty arr_t
    checkKernelResult (ConcatReturns cs o w per_thread_elems v) t = do
      TC.checkCerts cs
      case o of
        SplitContiguous -> return ()
        SplitStrided stride -> TC.require [Prim int64] stride
      TC.require [Prim int64] w
      TC.require [Prim int64] per_thread_elems
      vt <- lookupType v
      unless (vt == t `arrayOfRow` arraySize 0 vt) $
        TC.bad $ TC.TypeError $ "Invalid type for ConcatReturns " ++ pretty v
    checkKernelResult (TileReturns cs dims v) t = do
      TC.checkCerts cs
      forM_ dims $ \(dim, tile) -> do
        TC.require [Prim int64] dim
        TC.require [Prim int64] tile
      vt <- lookupType v
      unless (vt == t `arrayOfShape` Shape (map snd dims)) $
        TC.bad $ TC.TypeError $ "Invalid type for TileReturns " ++ pretty v
    checkKernelResult (RegTileReturns cs dims_n_tiles arr) t = do
      TC.checkCerts cs
      mapM_ (TC.require [Prim int64]) dims
      mapM_ (TC.require [Prim int64]) blk_tiles
      mapM_ (TC.require [Prim int64]) reg_tiles

      -- assert that arr is of element type t and shape (rev outer_tiles ++ reg_tiles)
      arr_t <- lookupType arr
      unless (arr_t == expected) $
        TC.bad . TC.TypeError $
          "Invalid type for TileReturns. Expected:\n  "
            ++ pretty expected
            ++ ",\ngot:\n  "
            ++ pretty arr_t
      where
        (dims, blk_tiles, reg_tiles) = unzip3 dims_n_tiles
        expected = t `arrayOfShape` Shape (blk_tiles ++ reg_tiles)

kernelBodyMetrics :: OpMetrics (Op rep) => KernelBody rep -> MetricsM ()
kernelBodyMetrics = mapM_ stmMetrics . kernelBodyStms

instance PrettyRep rep => Pretty (KernelBody rep) where
  ppr (KernelBody _ stms res) =
    PP.stack (map ppr (stmsToList stms))
      </> text "return" <+> PP.braces (PP.commasep $ map ppr res)

certAnnots :: Certs -> [PP.Doc]
certAnnots cs
  | cs == mempty = []
  | otherwise = [ppr cs]

instance Pretty KernelResult where
  ppr (Returns ResultNoSimplify cs what) =
    PP.spread $ certAnnots cs ++ ["returns (manifest)" <+> ppr what]
  ppr (Returns ResultPrivate cs what) =
    PP.spread $ certAnnots cs ++ ["returns (private)" <+> ppr what]
  ppr (Returns ResultMaySimplify cs what) =
    PP.spread $ certAnnots cs ++ ["returns" <+> ppr what]
  ppr (WriteReturns cs shape arr res) =
    PP.spread $
      certAnnots cs
        ++ [ ppr arr <+> PP.colon <+> ppr shape
               </> "with" <+> PP.apply (map ppRes res)
           ]
    where
      ppRes (slice, e) = ppr slice <+> text "=" <+> ppr e
  ppr (ConcatReturns cs SplitContiguous w per_thread_elems v) =
    PP.spread $
      certAnnots cs
        ++ [ "concat"
               <> parens (commasep [ppr w, ppr per_thread_elems]) <+> ppr v
           ]
  ppr (ConcatReturns cs (SplitStrided stride) w per_thread_elems v) =
    PP.spread $
      certAnnots cs
        ++ [ "concat_strided"
               <> parens (commasep [ppr stride, ppr w, ppr per_thread_elems]) <+> ppr v
           ]
  ppr (TileReturns cs dims v) =
    PP.spread $ certAnnots cs ++ ["tile" <> parens (commasep $ map onDim dims) <+> ppr v]
    where
      onDim (dim, tile) = ppr dim <+> "/" <+> ppr tile
  ppr (RegTileReturns cs dims_n_tiles v) =
    PP.spread $ certAnnots cs ++ ["blkreg_tile" <> parens (commasep $ map onDim dims_n_tiles) <+> ppr v]
    where
      onDim (dim, blk_tile, reg_tile) =
        ppr dim <+> "/" <+> parens (ppr blk_tile <+> "*" <+> ppr reg_tile)

-- | These dimensions (indexed from 0, outermost) of the corresponding
-- 'SegSpace' should not be parallelised, but instead iterated
-- sequentially.  For example, with a 'SegSeqDims' of @[0]@ and a
-- 'SegSpace' with dimensions @[n][m]@, there will be an outer loop
-- with @n@ iterations, while the @m@ dimension will be parallelised.
--
-- Semantically, this has no effect, but it may allow reductions in
-- memory usage or other low-level optimisations.  Operationally, the
-- guarantee is that for a SegSeqDims of e.g. @[i,j,k]@, threads
-- running at any given moment will always have the same indexes along
-- the dimensions specified by @[i,j,k]@.
--
-- At the moment, this is only supported for 'SegNoVirtFull'
-- intra-group parallelism in GPU code, as we have not yet found it
-- useful anywhere else.
newtype SegSeqDims = SegSeqDims {segSeqDims :: [Int]}
  deriving (Eq, Ord, Show)

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
  | -- | Not only do we not need virtualisation, but we _guarantee_
    -- that all physical threads participate in the work.  This can
    -- save some checks in code generation.
    SegNoVirtFull SegSeqDims
  deriving (Eq, Ord, Show)

-- | Index space of a 'SegOp'.
data SegSpace = SegSpace
  { -- | Flat physical index corresponding to the
    -- dimensions (at code generation used for a
    -- thread ID or similar).
    segFlat :: VName,
    unSegSpace :: [(VName, SubExp)]
  }
  deriving (Eq, Ord, Show)

-- | The sizes spanned by the indexes of the 'SegSpace'.
segSpaceDims :: SegSpace -> [SubExp]
segSpaceDims (SegSpace _ space) = map snd space

-- | A 'Scope' containing all the identifiers brought into scope by
-- this 'SegSpace'.
scopeOfSegSpace :: SegSpace -> Scope rep
scopeOfSegSpace (SegSpace phys space) =
  M.fromList $ zip (phys : map fst space) $ repeat $ IndexName Int64

checkSegSpace :: TC.Checkable rep => SegSpace -> TC.TypeM rep ()
checkSegSpace (SegSpace _ dims) =
  mapM_ (TC.require [Prim int64] . snd) dims

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
data SegOp lvl rep
  = SegMap lvl SegSpace [Type] (KernelBody rep)
  | -- | The KernelSpace must always have at least two dimensions,
    -- implying that the result of a SegRed is always an array.
    SegRed lvl SegSpace [SegBinOp rep] [Type] (KernelBody rep)
  | SegScan lvl SegSpace [SegBinOp rep] [Type] (KernelBody rep)
  | SegHist lvl SegSpace [HistOp rep] [Type] (KernelBody rep)
  deriving (Eq, Ord, Show)

-- | The level of a 'SegOp'.
segLevel :: SegOp lvl rep -> lvl
segLevel (SegMap lvl _ _ _) = lvl
segLevel (SegRed lvl _ _ _ _) = lvl
segLevel (SegScan lvl _ _ _ _) = lvl
segLevel (SegHist lvl _ _ _ _) = lvl

-- | The space of a 'SegOp'.
segSpace :: SegOp lvl rep -> SegSpace
segSpace (SegMap _ lvl _ _) = lvl
segSpace (SegRed _ lvl _ _ _) = lvl
segSpace (SegScan _ lvl _ _ _) = lvl
segSpace (SegHist _ lvl _ _ _) = lvl

-- | The body of a 'SegOp'.
segBody :: SegOp lvl rep -> KernelBody rep
segBody segop =
  case segop of
    SegMap _ _ _ body -> body
    SegRed _ _ _ _ body -> body
    SegScan _ _ _ _ body -> body
    SegHist _ _ _ _ body -> body

segResultShape :: SegSpace -> Type -> KernelResult -> Type
segResultShape _ t (WriteReturns _ shape _ _) =
  t `arrayOfShape` shape
segResultShape space t Returns {} =
  foldr (flip arrayOfRow) t $ segSpaceDims space
segResultShape _ t (ConcatReturns _ _ w _ _) =
  t `arrayOfRow` w
segResultShape _ t (TileReturns _ dims _) =
  t `arrayOfShape` Shape (map fst dims)
segResultShape _ t (RegTileReturns _ dims_n_tiles _) =
  t `arrayOfShape` Shape (map (\(dim, _, _) -> dim) dims_n_tiles)

-- | The return type of a 'SegOp'.
segOpType :: SegOp lvl rep -> [Type]
segOpType (SegMap _ space ts kbody) =
  zipWith (segResultShape space) ts $ kernelBodyResult kbody
segOpType (SegRed _ space reds ts kbody) =
  red_ts
    ++ zipWith
      (segResultShape space)
      map_ts
      (drop (length red_ts) $ kernelBodyResult kbody)
  where
    map_ts = drop (length red_ts) ts
    segment_dims = init $ segSpaceDims space
    red_ts = do
      op <- reds
      let shape = Shape segment_dims <> segBinOpShape op
      map (`arrayOfShape` shape) (lambdaReturnType $ segBinOpLambda op)
segOpType (SegScan _ space scans ts kbody) =
  scan_ts
    ++ zipWith
      (segResultShape space)
      map_ts
      (drop (length scan_ts) $ kernelBodyResult kbody)
  where
    map_ts = drop (length scan_ts) ts
    scan_ts = do
      op <- scans
      let shape = Shape (segSpaceDims space) <> segBinOpShape op
      map (`arrayOfShape` shape) (lambdaReturnType $ segBinOpLambda op)
segOpType (SegHist _ space ops _ _) = do
  op <- ops
  let shape = Shape segment_dims <> histShape op <> histOpShape op
  map (`arrayOfShape` shape) (lambdaReturnType $ histOp op)
  where
    dims = segSpaceDims space
    segment_dims = init dims

instance TypedOp (SegOp lvl rep) where
  opType = pure . staticShapes . segOpType

instance
  (ASTRep rep, Aliased rep, ASTConstraints lvl) =>
  AliasedOp (SegOp lvl rep)
  where
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
typeCheckSegOp ::
  TC.Checkable rep =>
  (lvl -> TC.TypeM rep ()) ->
  SegOp lvl (Aliases rep) ->
  TC.TypeM rep ()
typeCheckSegOp checkLvl (SegMap lvl space ts kbody) = do
  checkLvl lvl
  checkScanRed space [] ts kbody
typeCheckSegOp checkLvl (SegRed lvl space reds ts body) = do
  checkLvl lvl
  checkScanRed space reds' ts body
  where
    reds' =
      zip3
        (map segBinOpLambda reds)
        (map segBinOpNeutral reds)
        (map segBinOpShape reds)
typeCheckSegOp checkLvl (SegScan lvl space scans ts body) = do
  checkLvl lvl
  checkScanRed space scans' ts body
  where
    scans' =
      zip3
        (map segBinOpLambda scans)
        (map segBinOpNeutral scans)
        (map segBinOpShape scans)
typeCheckSegOp checkLvl (SegHist lvl space ops ts kbody) = do
  checkLvl lvl
  checkSegSpace space
  mapM_ TC.checkType ts

  TC.binding (scopeOfSegSpace space) $ do
    nes_ts <- forM ops $ \(HistOp dest_shape rf dests nes shape op) -> do
      mapM_ (TC.require [Prim int64]) dest_shape
      TC.require [Prim int64] rf
      nes' <- mapM TC.checkArg nes
      mapM_ (TC.require [Prim int64]) $ shapeDims shape

      -- Operator type must match the type of neutral elements.
      let stripVecDims = stripArray $ shapeRank shape
      TC.checkLambda op $ map (TC.noArgAliases . first stripVecDims) $ nes' ++ nes'
      let nes_t = map TC.argType nes'
      unless (nes_t == lambdaReturnType op) $
        TC.bad $
          TC.TypeError $
            "SegHist operator has return type "
              ++ prettyTuple (lambdaReturnType op)
              ++ " but neutral element has type "
              ++ prettyTuple nes_t

      -- Arrays must have proper type.
      let dest_shape' = Shape segment_dims <> dest_shape <> shape
      forM_ (zip nes_t dests) $ \(t, dest) -> do
        TC.requireI [t `arrayOfShape` dest_shape'] dest
        TC.consume =<< TC.lookupAliases dest

      return $ map (`arrayOfShape` shape) nes_t

    checkKernelBody ts kbody

    -- Return type of bucket function must be an index for each
    -- operation followed by the values to write.
    let bucket_ret_t =
          concatMap ((`replicate` Prim int64) . shapeRank . histShape) ops
            ++ concat nes_ts
    unless (bucket_ret_t == ts) $
      TC.bad $
        TC.TypeError $
          "SegHist body has return type "
            ++ prettyTuple ts
            ++ " but should have type "
            ++ prettyTuple bucket_ret_t
  where
    segment_dims = init $ segSpaceDims space

checkScanRed ::
  TC.Checkable rep =>
  SegSpace ->
  [(Lambda (Aliases rep), [SubExp], Shape)] ->
  [Type] ->
  KernelBody (Aliases rep) ->
  TC.TypeM rep ()
checkScanRed space ops ts kbody = do
  checkSegSpace space
  mapM_ TC.checkType ts

  TC.binding (scopeOfSegSpace space) $ do
    ne_ts <- forM ops $ \(lam, nes, shape) -> do
      mapM_ (TC.require [Prim int64]) $ shapeDims shape
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
      TC.bad $
        TC.TypeError $
          "Wrong return for body (does not match neutral elements; expected "
            ++ pretty expecting
            ++ "; found "
            ++ pretty got
            ++ ")"

    checkKernelBody ts kbody

-- | Like 'Mapper', but just for 'SegOp's.
data SegOpMapper lvl frep trep m = SegOpMapper
  { mapOnSegOpSubExp :: SubExp -> m SubExp,
    mapOnSegOpLambda :: Lambda frep -> m (Lambda trep),
    mapOnSegOpBody :: KernelBody frep -> m (KernelBody trep),
    mapOnSegOpVName :: VName -> m VName,
    mapOnSegOpLevel :: lvl -> m lvl
  }

-- | A mapper that simply returns the 'SegOp' verbatim.
identitySegOpMapper :: Monad m => SegOpMapper lvl rep rep m
identitySegOpMapper =
  SegOpMapper
    { mapOnSegOpSubExp = return,
      mapOnSegOpLambda = return,
      mapOnSegOpBody = return,
      mapOnSegOpVName = return,
      mapOnSegOpLevel = return
    }

mapOnSegSpace ::
  Monad f => SegOpMapper lvl frep trep f -> SegSpace -> f SegSpace
mapOnSegSpace tv (SegSpace phys dims) =
  SegSpace
    <$> mapOnSegOpVName tv phys
    <*> traverse (bitraverse (mapOnSegOpVName tv) (mapOnSegOpSubExp tv)) dims

mapSegBinOp ::
  Monad m =>
  SegOpMapper lvl frep trep m ->
  SegBinOp frep ->
  m (SegBinOp trep)
mapSegBinOp tv (SegBinOp comm red_op nes shape) =
  SegBinOp comm
    <$> mapOnSegOpLambda tv red_op
    <*> mapM (mapOnSegOpSubExp tv) nes
    <*> (Shape <$> mapM (mapOnSegOpSubExp tv) (shapeDims shape))

-- | Apply a 'SegOpMapper' to the given 'SegOp'.
mapSegOpM ::
  (Applicative m, Monad m) =>
  SegOpMapper lvl frep trep m ->
  SegOp lvl frep ->
  m (SegOp lvl trep)
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
  where
    onHistOp (HistOp w rf arrs nes shape op) =
      HistOp <$> mapM (mapOnSegOpSubExp tv) w
        <*> mapOnSegOpSubExp tv rf
        <*> mapM (mapOnSegOpVName tv) arrs
        <*> mapM (mapOnSegOpSubExp tv) nes
        <*> (Shape <$> mapM (mapOnSegOpSubExp tv) (shapeDims shape))
        <*> mapOnSegOpLambda tv op

mapOnSegOpType ::
  Monad m =>
  SegOpMapper lvl frep trep m ->
  Type ->
  m Type
mapOnSegOpType _tv t@Prim {} = pure t
mapOnSegOpType tv (Acc acc ispace ts u) =
  Acc
    <$> mapOnSegOpVName tv acc
    <*> traverse (mapOnSegOpSubExp tv) ispace
    <*> traverse (bitraverse (traverse (mapOnSegOpSubExp tv)) pure) ts
    <*> pure u
mapOnSegOpType tv (Array et shape u) =
  Array et <$> traverse (mapOnSegOpSubExp tv) shape <*> pure u
mapOnSegOpType _tv (Mem s) = pure $ Mem s

-- | A helper for defining 'TraverseOpStms'.
traverseSegOpStms :: Monad m => OpStmsTraverser m (SegOp lvl rep) rep
traverseSegOpStms f segop = mapSegOpM mapper segop
  where
    seg_scope = scopeOfSegSpace (segSpace segop)
    f' scope = f (seg_scope <> scope)
    mapper =
      identitySegOpMapper
        { mapOnSegOpLambda = traverseLambdaStms f',
          mapOnSegOpBody = onBody
        }
    onBody (KernelBody dec stms res) =
      KernelBody dec <$> f seg_scope stms <*> pure res

instance
  (ASTRep rep, Substitute lvl) =>
  Substitute (SegOp lvl rep)
  where
  substituteNames subst = runIdentity . mapSegOpM substitute
    where
      substitute =
        SegOpMapper
          { mapOnSegOpSubExp = return . substituteNames subst,
            mapOnSegOpLambda = return . substituteNames subst,
            mapOnSegOpBody = return . substituteNames subst,
            mapOnSegOpVName = return . substituteNames subst,
            mapOnSegOpLevel = return . substituteNames subst
          }

instance (ASTRep rep, ASTConstraints lvl) => Rename (SegOp lvl rep) where
  rename op =
    renameBound (M.keys (scopeOfSegSpace (segSpace op))) $ mapSegOpM renamer op
    where
      renamer = SegOpMapper rename rename rename rename rename

instance
  (ASTRep rep, FreeIn (LParamInfo rep), FreeIn lvl) =>
  FreeIn (SegOp lvl rep)
  where
  freeIn' e =
    fvBind (namesFromList $ M.keys $ scopeOfSegSpace (segSpace e)) $
      flip execState mempty $ mapSegOpM free e
    where
      walk f x = modify (<> f x) >> return x
      free =
        SegOpMapper
          { mapOnSegOpSubExp = walk freeIn',
            mapOnSegOpLambda = walk freeIn',
            mapOnSegOpBody = walk freeIn',
            mapOnSegOpVName = walk freeIn',
            mapOnSegOpLevel = walk freeIn'
          }

instance OpMetrics (Op rep) => OpMetrics (SegOp lvl rep) where
  opMetrics (SegMap _ _ _ body) =
    inside "SegMap" $ kernelBodyMetrics body
  opMetrics (SegRed _ _ reds _ body) =
    inside "SegRed" $ do
      mapM_ (lambdaMetrics . segBinOpLambda) reds
      kernelBodyMetrics body
  opMetrics (SegScan _ _ scans _ body) =
    inside "SegScan" $ do
      mapM_ (lambdaMetrics . segBinOpLambda) scans
      kernelBodyMetrics body
  opMetrics (SegHist _ _ ops _ body) =
    inside "SegHist" $ do
      mapM_ (lambdaMetrics . histOp) ops
      kernelBodyMetrics body

instance Pretty SegSpace where
  ppr (SegSpace phys dims) =
    parens
      ( commasep $ do
          (i, d) <- dims
          return $ ppr i <+> "<" <+> ppr d
      )
      <+> parens (text "~" <> ppr phys)

instance PrettyRep rep => Pretty (SegBinOp rep) where
  ppr (SegBinOp comm lam nes shape) =
    PP.braces (PP.commasep $ map ppr nes) <> PP.comma
      </> ppr shape <> PP.comma
      </> comm' <> ppr lam
    where
      comm' = case comm of
        Commutative -> text "commutative "
        Noncommutative -> mempty

instance (PrettyRep rep, PP.Pretty lvl) => PP.Pretty (SegOp lvl rep) where
  ppr (SegMap lvl space ts body) =
    text "segmap" <> ppr lvl
      </> PP.align (ppr space)
      <+> PP.colon
      <+> ppTuple' ts
      <+> PP.nestedBlock "{" "}" (ppr body)
  ppr (SegRed lvl space reds ts body) =
    text "segred" <> ppr lvl
      </> PP.align (ppr space)
      </> PP.parens (mconcat $ intersperse (PP.comma <> PP.line) $ map ppr reds)
      </> PP.colon
      <+> ppTuple' ts
      <+> PP.nestedBlock "{" "}" (ppr body)
  ppr (SegScan lvl space scans ts body) =
    text "segscan" <> ppr lvl
      </> PP.align (ppr space)
      </> PP.parens (mconcat $ intersperse (PP.comma <> PP.line) $ map ppr scans)
      </> PP.colon
      <+> ppTuple' ts
      <+> PP.nestedBlock "{" "}" (ppr body)
  ppr (SegHist lvl space ops ts body) =
    text "seghist" <> ppr lvl
      </> PP.align (ppr space)
      </> PP.parens (mconcat $ intersperse (PP.comma <> PP.line) $ map ppOp ops)
      </> PP.colon
      <+> ppTuple' ts
      <+> PP.nestedBlock "{" "}" (ppr body)
    where
      ppOp (HistOp w rf dests nes shape op) =
        ppr w <> PP.comma <+> ppr rf <> PP.comma
          </> PP.braces (PP.commasep $ map ppr dests) <> PP.comma
          </> PP.braces (PP.commasep $ map ppr nes) <> PP.comma
          </> ppr shape <> PP.comma
          </> ppr op

instance
  ( ASTRep rep,
    ASTRep (Aliases rep),
    CanBeAliased (Op rep),
    ASTConstraints lvl
  ) =>
  CanBeAliased (SegOp lvl rep)
  where
  type OpWithAliases (SegOp lvl rep) = SegOp lvl (Aliases rep)

  addOpAliases aliases = runIdentity . mapSegOpM alias
    where
      alias =
        SegOpMapper
          return
          (return . Alias.analyseLambda aliases)
          (return . aliasAnalyseKernelBody aliases)
          return
          return

  removeOpAliases = runIdentity . mapSegOpM remove
    where
      remove =
        SegOpMapper
          return
          (return . removeLambdaAliases)
          (return . removeKernelBodyAliases)
          return
          return

informKernelBody :: Informing rep => KernelBody rep -> KernelBody (Wise rep)
informKernelBody (KernelBody dec stms res) =
  mkWiseKernelBody dec (informStms stms) res

instance
  (CanBeWise (Op rep), ASTRep rep, ASTConstraints lvl) =>
  CanBeWise (SegOp lvl rep)
  where
  type OpWithWisdom (SegOp lvl rep) = SegOp lvl (Wise rep)

  removeOpWisdom = runIdentity . mapSegOpM remove
    where
      remove =
        SegOpMapper
          return
          (return . removeLambdaWisdom)
          (return . removeKernelBodyWisdom)
          return
          return

  addOpWisdom = runIdentity . mapSegOpM add
    where
      add =
        SegOpMapper
          return
          (return . informLambda)
          (return . informKernelBody)
          return
          return

instance ASTRep rep => ST.IndexOp (SegOp lvl rep) where
  indexOp vtable k (SegMap _ space _ kbody) is = do
    Returns ResultMaySimplify _ se <- maybeNth k $ kernelBodyResult kbody
    guard $ length gtids <= length is
    let idx_table = M.fromList $ zip gtids $ map (ST.Indexed mempty . untyped) is
        idx_table' = foldl' expandIndexedTable idx_table $ kernelBodyStms kbody
    case se of
      Var v -> M.lookup v idx_table'
      _ -> Nothing
    where
      (gtids, _) = unzip $ unSegSpace space
      -- Indexes in excess of what is used to index through the
      -- segment dimensions.
      excess_is = drop (length gtids) is

      expandIndexedTable table stm
        | [v] <- patNames $ stmPat stm,
          Just (pe, cs) <-
            runWriterT $ primExpFromExp (asPrimExp table) $ stmExp stm =
          M.insert v (ST.Indexed (stmCerts stm <> cs) pe) table
        | [v] <- patNames $ stmPat stm,
          BasicOp (Index arr slice) <- stmExp stm,
          length (sliceDims slice) == length excess_is,
          arr `ST.available` vtable,
          Just (slice', cs) <- asPrimExpSlice table slice =
          let idx =
                ST.IndexedArray
                  (stmCerts stm <> cs)
                  arr
                  (fixSlice (fmap isInt64 slice') excess_is)
           in M.insert v idx table
        | otherwise =
          table

      asPrimExpSlice table =
        runWriterT . traverse (primExpFromSubExpM (asPrimExp table))

      asPrimExp table v
        | Just (ST.Indexed cs e) <- M.lookup v table = tell cs >> return e
        | Just (Prim pt) <- ST.lookupType v vtable =
          return $ LeafExp v pt
        | otherwise = lift Nothing
  indexOp _ _ _ _ = Nothing

instance
  (ASTRep rep, ASTConstraints lvl) =>
  IsOp (SegOp lvl rep)
  where
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
  simplify (Returns manifest cs what) =
    Returns manifest <$> Engine.simplify cs <*> Engine.simplify what
  simplify (WriteReturns cs ws a res) =
    WriteReturns <$> Engine.simplify cs
      <*> Engine.simplify ws
      <*> Engine.simplify a
      <*> Engine.simplify res
  simplify (ConcatReturns cs o w pte what) =
    ConcatReturns
      <$> Engine.simplify cs
      <*> Engine.simplify o
      <*> Engine.simplify w
      <*> Engine.simplify pte
      <*> Engine.simplify what
  simplify (TileReturns cs dims what) =
    TileReturns <$> Engine.simplify cs <*> Engine.simplify dims <*> Engine.simplify what
  simplify (RegTileReturns cs dims_n_tiles what) =
    RegTileReturns
      <$> Engine.simplify cs
      <*> Engine.simplify dims_n_tiles
      <*> Engine.simplify what

mkWiseKernelBody ::
  (ASTRep rep, CanBeWise (Op rep)) =>
  BodyDec rep ->
  Stms (Wise rep) ->
  [KernelResult] ->
  KernelBody (Wise rep)
mkWiseKernelBody dec stms res =
  let Body dec' _ _ = mkWiseBody dec stms $ subExpsRes res_vs
   in KernelBody dec' stms res
  where
    res_vs = map kernelResultSubExp res

mkKernelBodyM ::
  MonadBuilder m =>
  Stms (Rep m) ->
  [KernelResult] ->
  m (KernelBody (Rep m))
mkKernelBodyM stms kres = do
  Body dec' _ _ <- mkBodyM stms $ subExpsRes res_ses
  return $ KernelBody dec' stms kres
  where
    res_ses = map kernelResultSubExp kres

simplifyKernelBody ::
  (Engine.SimplifiableRep rep, BodyDec rep ~ ()) =>
  SegSpace ->
  KernelBody (Wise rep) ->
  Engine.SimpleM rep (KernelBody (Wise rep), Stms (Wise rep))
simplifyKernelBody space (KernelBody _ stms res) = do
  par_blocker <- Engine.asksEngineEnv $ Engine.blockHoistPar . Engine.envHoistBlockers

  let blocker =
        Engine.hasFree bound_here
          `Engine.orIf` Engine.isOp
          `Engine.orIf` par_blocker
          `Engine.orIf` Engine.isConsumed

  -- Ensure we do not try to use anything that is consumed in the result.
  (body_res, body_stms, hoisted) <-
    Engine.localVtable (flip (foldl' (flip ST.consume)) (foldMap consumedInResult res))
      . Engine.localVtable (<> scope_vtable)
      . Engine.localVtable (\vtable -> vtable {ST.simplifyMemory = True})
      . Engine.enterLoop
      $ Engine.blockIf blocker stms $ do
        res' <-
          Engine.localVtable (ST.hideCertified $ namesFromList $ M.keys $ scopeOf stms) $
            mapM Engine.simplify res
        pure (res', UT.usages $ freeIn res')

  return (mkWiseKernelBody () body_stms body_res, hoisted)
  where
    scope_vtable = segSpaceSymbolTable space
    bound_here = namesFromList $ M.keys $ scopeOfSegSpace space

    consumedInResult (WriteReturns _ _ arr _) =
      [arr]
    consumedInResult _ =
      []

segSpaceSymbolTable :: ASTRep rep => SegSpace -> ST.SymbolTable rep
segSpaceSymbolTable (SegSpace flat gtids_and_dims) =
  foldl' f (ST.fromScope $ M.singleton flat $ IndexName Int64) gtids_and_dims
  where
    f vtable (gtid, dim) = ST.insertLoopVar gtid Int64 dim vtable

simplifySegBinOp ::
  Engine.SimplifiableRep rep =>
  SegBinOp (Wise rep) ->
  Engine.SimpleM rep (SegBinOp (Wise rep), Stms (Wise rep))
simplifySegBinOp (SegBinOp comm lam nes shape) = do
  (lam', hoisted) <-
    Engine.localVtable (\vtable -> vtable {ST.simplifyMemory = True}) $
      Engine.simplifyLambda lam
  shape' <- Engine.simplify shape
  nes' <- mapM Engine.simplify nes
  return (SegBinOp comm lam' nes' shape', hoisted)

-- | Simplify the given 'SegOp'.
simplifySegOp ::
  ( Engine.SimplifiableRep rep,
    BodyDec rep ~ (),
    Engine.Simplifiable lvl
  ) =>
  SegOp lvl (Wise rep) ->
  Engine.SimpleM rep (SegOp lvl (Wise rep), Stms (Wise rep))
simplifySegOp (SegMap lvl space ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (kbody', body_hoisted) <- simplifyKernelBody space kbody
  return
    ( SegMap lvl' space' ts' kbody',
      body_hoisted
    )
simplifySegOp (SegRed lvl space reds ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (reds', reds_hoisted) <-
    Engine.localVtable (<> scope_vtable) $
      unzip <$> mapM simplifySegBinOp reds
  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return
    ( SegRed lvl' space' reds' ts' kbody',
      mconcat reds_hoisted <> body_hoisted
    )
  where
    scope = scopeOfSegSpace space
    scope_vtable = ST.fromScope scope
simplifySegOp (SegScan lvl space scans ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (scans', scans_hoisted) <-
    Engine.localVtable (<> scope_vtable) $
      unzip <$> mapM simplifySegBinOp scans
  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return
    ( SegScan lvl' space' scans' ts' kbody',
      mconcat scans_hoisted <> body_hoisted
    )
  where
    scope = scopeOfSegSpace space
    scope_vtable = ST.fromScope scope
simplifySegOp (SegHist lvl space ops ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)

  (ops', ops_hoisted) <- fmap unzip $
    forM ops $
      \(HistOp w rf arrs nes dims lam) -> do
        w' <- Engine.simplify w
        rf' <- Engine.simplify rf
        arrs' <- Engine.simplify arrs
        nes' <- Engine.simplify nes
        dims' <- Engine.simplify dims
        (lam', op_hoisted) <-
          Engine.localVtable (<> scope_vtable) $
            Engine.localVtable (\vtable -> vtable {ST.simplifyMemory = True}) $
              Engine.simplifyLambda lam
        return
          ( HistOp w' rf' arrs' nes' dims' lam',
            op_hoisted
          )

  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return
    ( SegHist lvl' space' ops' ts' kbody',
      mconcat ops_hoisted <> body_hoisted
    )
  where
    scope = scopeOfSegSpace space
    scope_vtable = ST.fromScope scope

-- | Does this rep contain 'SegOp's in its t'Op's?  A rep must be an
-- instance of this class for the simplification rules to work.
class HasSegOp rep where
  type SegOpLevel rep
  asSegOp :: Op rep -> Maybe (SegOp (SegOpLevel rep) rep)
  segOp :: SegOp (SegOpLevel rep) rep -> Op rep

-- | Simplification rules for simplifying 'SegOp's.
segOpRules ::
  (HasSegOp rep, BuilderOps rep, Buildable rep) =>
  RuleBook rep
segOpRules =
  ruleBook [RuleOp segOpRuleTopDown] [RuleOp segOpRuleBottomUp]

segOpRuleTopDown ::
  (HasSegOp rep, BuilderOps rep, Buildable rep) =>
  TopDownRuleOp rep
segOpRuleTopDown vtable pat dec op
  | Just op' <- asSegOp op =
    topDownSegOp vtable pat dec op'
  | otherwise =
    Skip

segOpRuleBottomUp ::
  (HasSegOp rep, BuilderOps rep) =>
  BottomUpRuleOp rep
segOpRuleBottomUp vtable pat dec op
  | Just op' <- asSegOp op =
    bottomUpSegOp vtable pat dec op'
  | otherwise =
    Skip

topDownSegOp ::
  (HasSegOp rep, BuilderOps rep, Buildable rep) =>
  ST.SymbolTable rep ->
  Pat rep ->
  StmAux (ExpDec rep) ->
  SegOp (SegOpLevel rep) rep ->
  Rule rep
-- If a SegOp produces something invariant to the SegOp, turn it
-- into a replicate.
topDownSegOp vtable (Pat kpes) dec (SegMap lvl space ts (KernelBody _ kstms kres)) = Simplify $ do
  (ts', kpes', kres') <-
    unzip3 <$> filterM checkForInvarianceResult (zip3 ts kpes kres)

  -- Check if we did anything at all.
  when (kres == kres') cannotSimplify

  kbody <- mkKernelBodyM kstms kres'
  addStm $
    Let (Pat kpes') dec $ Op $ segOp $ SegMap lvl space ts' kbody
  where
    isInvariant Constant {} = True
    isInvariant (Var v) = isJust $ ST.lookup v vtable

    checkForInvarianceResult (_, pe, Returns rm cs se)
      | cs == mempty,
        rm == ResultMaySimplify,
        isInvariant se = do
        letBindNames [patElemName pe] $
          BasicOp $ Replicate (Shape $ segSpaceDims space) se
        return False
    checkForInvarianceResult _ =
      return True

-- If a SegRed contains two reduction operations that have the same
-- vector shape, merge them together.  This saves on communication
-- overhead, but can in principle lead to more local memory usage.
topDownSegOp _ (Pat pes) _ (SegRed lvl space ops ts kbody)
  | length ops > 1,
    op_groupings <-
      groupBy sameShape $
        zip ops $
          chunks (map (length . segBinOpNeutral) ops) $
            zip3 red_pes red_ts red_res,
    any ((> 1) . length) op_groupings = Simplify $ do
    let (ops', aux) = unzip $ mapMaybe combineOps op_groupings
        (red_pes', red_ts', red_res') = unzip3 $ concat aux
        pes' = red_pes' ++ map_pes
        ts' = red_ts' ++ map_ts
        kbody' = kbody {kernelBodyResult = red_res' ++ map_res}
    letBind (Pat pes') $ Op $ segOp $ SegRed lvl space ops' ts' kbody'
  where
    (red_pes, map_pes) = splitAt (segBinOpResults ops) pes
    (red_ts, map_ts) = splitAt (segBinOpResults ops) ts
    (red_res, map_res) = splitAt (segBinOpResults ops) $ kernelBodyResult kbody

    sameShape (op1, _) (op2, _) = segBinOpShape op1 == segBinOpShape op2

    combineOps [] = Nothing
    combineOps (x : xs) = Just $ foldl' combine x xs

    combine (op1, op1_aux) (op2, op2_aux) =
      let lam1 = segBinOpLambda op1
          lam2 = segBinOpLambda op2
          (op1_xparams, op1_yparams) =
            splitAt (length (segBinOpNeutral op1)) $ lambdaParams lam1
          (op2_xparams, op2_yparams) =
            splitAt (length (segBinOpNeutral op2)) $ lambdaParams lam2
          lam =
            Lambda
              { lambdaParams =
                  op1_xparams ++ op2_xparams
                    ++ op1_yparams
                    ++ op2_yparams,
                lambdaReturnType = lambdaReturnType lam1 ++ lambdaReturnType lam2,
                lambdaBody =
                  mkBody (bodyStms (lambdaBody lam1) <> bodyStms (lambdaBody lam2)) $
                    bodyResult (lambdaBody lam1) <> bodyResult (lambdaBody lam2)
              }
       in ( SegBinOp
              { segBinOpComm = segBinOpComm op1 <> segBinOpComm op2,
                segBinOpLambda = lam,
                segBinOpNeutral = segBinOpNeutral op1 ++ segBinOpNeutral op2,
                segBinOpShape = segBinOpShape op1 -- Same as shape of op2 due to the grouping.
              },
            op1_aux ++ op2_aux
          )
topDownSegOp _ _ _ _ = Skip

-- A convenient way of operating on the type and body of a SegOp,
-- without worrying about exactly what kind it is.
segOpGuts ::
  SegOp (SegOpLevel rep) rep ->
  ( [Type],
    KernelBody rep,
    Int,
    [Type] -> KernelBody rep -> SegOp (SegOpLevel rep) rep
  )
segOpGuts (SegMap lvl space kts body) =
  (kts, body, 0, SegMap lvl space)
segOpGuts (SegScan lvl space ops kts body) =
  (kts, body, segBinOpResults ops, SegScan lvl space ops)
segOpGuts (SegRed lvl space ops kts body) =
  (kts, body, segBinOpResults ops, SegRed lvl space ops)
segOpGuts (SegHist lvl space ops kts body) =
  (kts, body, sum $ map (length . histDest) ops, SegHist lvl space ops)

bottomUpSegOp ::
  (HasSegOp rep, BuilderOps rep) =>
  (ST.SymbolTable rep, UT.UsageTable) ->
  Pat rep ->
  StmAux (ExpDec rep) ->
  SegOp (SegOpLevel rep) rep ->
  Rule rep
-- Some SegOp results can be moved outside the SegOp, which can
-- simplify further analysis.
bottomUpSegOp (vtable, used) (Pat kpes) dec segop = Simplify $ do
  -- Iterate through the bindings.  For each, we check whether it is
  -- in kres and can be moved outside.  If so, we remove it from kres
  -- and kpes and make it a binding outside.  We have to be careful
  -- not to remove anything that is passed on to a scan/map/histogram
  -- operation.  Fortunately, these are always first in the result
  -- list.
  (kpes', kts', kres', kstms') <-
    localScope (scopeOfSegSpace space) $
      foldM distribute (kpes, kts, kres, mempty) kstms

  when
    (kpes' == kpes)
    cannotSimplify

  kbody <-
    localScope (scopeOfSegSpace space) $
      mkKernelBodyM kstms' kres'

  addStm $ Let (Pat kpes') dec $ Op $ segOp $ mk_segop kts' kbody
  where
    (kts, KernelBody _ kstms kres, num_nonmap_results, mk_segop) =
      segOpGuts segop
    free_in_kstms = foldMap freeIn kstms
    space = segSpace segop

    sliceWithGtidsFixed stm
      | Let _ _ (BasicOp (Index arr slice)) <- stm,
        space_slice <- map (DimFix . Var . fst) $ unSegSpace space,
        space_slice `isPrefixOf` unSlice slice,
        remaining_slice <- Slice $ drop (length space_slice) (unSlice slice),
        all (isJust . flip ST.lookup vtable) $
          namesToList $
            freeIn arr <> freeIn remaining_slice =
        Just (remaining_slice, arr)
      | otherwise =
        Nothing

    distribute (kpes', kts', kres', kstms') stm
      | Let (Pat [pe]) _ _ <- stm,
        Just (Slice remaining_slice, arr) <- sliceWithGtidsFixed stm,
        Just (kpe, kpes'', kts'', kres'') <- isResult kpes' kts' kres' pe = do
        let outer_slice =
              map
                ( \d ->
                    DimSlice
                      (constant (0 :: Int64))
                      d
                      (constant (1 :: Int64))
                )
                $ segSpaceDims space
            index kpe' =
              letBindNames [patElemName kpe'] . BasicOp . Index arr $
                Slice $ outer_slice <> remaining_slice
        if patElemName kpe `UT.isConsumed` used
          then do
            precopy <- newVName $ baseString (patElemName kpe) <> "_precopy"
            index kpe {patElemName = precopy}
            letBindNames [patElemName kpe] $ BasicOp $ Copy precopy
          else index kpe
        return
          ( kpes'',
            kts'',
            kres'',
            if patElemName pe `nameIn` free_in_kstms
              then kstms' <> oneStm stm
              else kstms'
          )
    distribute (kpes', kts', kres', kstms') stm =
      return (kpes', kts', kres', kstms' <> oneStm stm)

    isResult kpes' kts' kres' pe =
      case partition matches $ zip3 kpes' kts' kres' of
        ([(kpe, _, _)], kpes_and_kres)
          | Just i <- elemIndex kpe kpes,
            i >= num_nonmap_results,
            (kpes'', kts'', kres'') <- unzip3 kpes_and_kres ->
            Just (kpe, kpes'', kts'', kres'')
        _ -> Nothing
      where
        matches (_, _, Returns _ _ (Var v)) = v == patElemName pe
        matches _ = False

--- Memory

kernelBodyReturns ::
  (Mem rep inner, HasScope rep m, Monad m) =>
  KernelBody somerep ->
  [ExpReturns] ->
  m [ExpReturns]
kernelBodyReturns = zipWithM correct . kernelBodyResult
  where
    correct (WriteReturns _ _ arr _) _ = varReturns arr
    correct _ ret = return ret

-- | Like 'segOpType', but for memory representations.
segOpReturns ::
  (Mem rep inner, Monad m, HasScope rep m) =>
  SegOp lvl somerep ->
  m [ExpReturns]
segOpReturns k@(SegMap _ _ _ kbody) =
  kernelBodyReturns kbody . extReturns =<< opType k
segOpReturns k@(SegRed _ _ _ _ kbody) =
  kernelBodyReturns kbody . extReturns =<< opType k
segOpReturns k@(SegScan _ _ _ _ kbody) =
  kernelBodyReturns kbody . extReturns =<< opType k
segOpReturns (SegHist _ _ ops _ _) =
  concat <$> mapM (mapM varReturns . histDest) ops
