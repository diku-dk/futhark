{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Segmented operations.  These correspond to perfect @map@ nests on
-- top of /something/, except that the @map@s are conceptually only
-- over @iota@s (so there will be explicit indexing inside them).
module Futhark.IR.SegOp
  ( SegOp (..),
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
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Bifunctor (first)
import Data.Bitraversable
import Data.List
  ( elemIndex,
    foldl',
    groupBy,
    intersperse,
    isPrefixOf,
    partition,
  )
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Analysis.Metrics
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.IR
import Futhark.IR.Aliases
  ( Aliases,
    CanBeAliased (..),
  )
import Futhark.IR.Mem
import Futhark.IR.Prop.Aliases
import Futhark.IR.TypeCheck qualified as TC
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (chunks, maybeNth)
import Futhark.Util.Pretty
  ( Doc,
    apply,
    hsep,
    parens,
    ppTuple',
    pretty,
    (<+>),
    (</>),
  )
import Futhark.Util.Pretty qualified as PP
import Prelude hiding (id, (.))

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
    lambdaReturnType $
      histOp op

-- | Split reduction results returned by a 'KernelBody' into those
-- that correspond to indexes for the 'HistOp's, and those that
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

deriving instance (RepTypes rep) => Ord (KernelBody rep)

deriving instance (RepTypes rep) => Show (KernelBody rep)

deriving instance (RepTypes rep) => Eq (KernelBody rep)

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
    -- result-per-block depends on where the 'SegOp' occurs.
    Returns ResultManifest Certs SubExp
  | WriteReturns
      Certs
      VName -- Destination array
      [(Slice SubExp, SubExp)]
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
      VName -- Tile returned by this thread/block.
  deriving (Eq, Show, Ord)

-- | Get the certs for this 'KernelResult'.
kernelResultCerts :: KernelResult -> Certs
kernelResultCerts (Returns _ cs _) = cs
kernelResultCerts (WriteReturns cs _ _) = cs
kernelResultCerts (TileReturns cs _ _) = cs
kernelResultCerts (RegTileReturns cs _ _) = cs

-- | Get the root t'SubExp' corresponding values for a 'KernelResult'.
kernelResultSubExp :: KernelResult -> SubExp
kernelResultSubExp (Returns _ _ se) = se
kernelResultSubExp (WriteReturns _ arr _) = Var arr
kernelResultSubExp (TileReturns _ _ v) = Var v
kernelResultSubExp (RegTileReturns _ _ v) = Var v

instance FreeIn KernelResult where
  freeIn' (Returns _ cs what) = freeIn' cs <> freeIn' what
  freeIn' (WriteReturns cs arr res) = freeIn' cs <> freeIn' arr <> freeIn' res
  freeIn' (TileReturns cs dims v) =
    freeIn' cs <> freeIn' dims <> freeIn' v
  freeIn' (RegTileReturns cs dims_n_tiles v) =
    freeIn' cs <> freeIn' dims_n_tiles <> freeIn' v

instance (ASTRep rep) => FreeIn (KernelBody rep) where
  freeIn' (KernelBody dec stms res) =
    fvBind bound_in_stms $ freeIn' dec <> freeIn' stms <> freeIn' res
    where
      bound_in_stms = foldMap boundByStm stms

instance (ASTRep rep) => Substitute (KernelBody rep) where
  substituteNames subst (KernelBody dec stms res) =
    KernelBody
      (substituteNames subst dec)
      (substituteNames subst stms)
      (substituteNames subst res)

instance Substitute KernelResult where
  substituteNames subst (Returns manifest cs se) =
    Returns manifest (substituteNames subst cs) (substituteNames subst se)
  substituteNames subst (WriteReturns cs arr res) =
    WriteReturns
      (substituteNames subst cs)
      (substituteNames subst arr)
      (substituteNames subst res)
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

instance (ASTRep rep) => Rename (KernelBody rep) where
  rename (KernelBody dec stms res) = do
    dec' <- rename dec
    renamingStms stms $ \stms' ->
      KernelBody dec' stms' <$> rename res

instance Rename KernelResult where
  rename = substituteRename

-- | Perform alias analysis on a 'KernelBody'.
aliasAnalyseKernelBody ::
  (Alias.AliasableRep rep) =>
  AliasTable ->
  KernelBody rep ->
  KernelBody (Aliases rep)
aliasAnalyseKernelBody aliases (KernelBody dec stms res) =
  let Body dec' stms' _ = Alias.analyseBody aliases $ Body dec stms []
   in KernelBody dec' stms' res

-- | The variables consumed in the kernel body.
consumedInKernelBody ::
  (Aliased rep) =>
  KernelBody rep ->
  Names
consumedInKernelBody (KernelBody dec stms res) =
  consumedInBody (Body dec stms []) <> mconcat (map consumedByReturn res)
  where
    consumedByReturn (WriteReturns _ a _) = oneName a
    consumedByReturn _ = mempty

checkKernelBody ::
  (TC.Checkable rep) =>
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
      TC.bad . TC.TypeError $
        "Kernel return type is "
          <> prettyTuple ts
          <> ", but body returns "
          <> prettyText (length kres)
          <> " values."
    zipWithM_ checkKernelResult kres ts
  where
    consumeKernelResult (WriteReturns _ arr _) =
      TC.consume =<< TC.lookupAliases arr
    consumeKernelResult _ =
      pure ()

    checkKernelResult (Returns _ cs what) t = do
      TC.checkCerts cs
      TC.require [t] what
    checkKernelResult (WriteReturns cs arr res) t = do
      TC.checkCerts cs
      arr_t <- lookupType arr
      unless (arr_t == t) $
        TC.bad . TC.TypeError $
          "WriteReturns result type annotation for "
            <> prettyText arr
            <> " is "
            <> prettyText t
            <> ", but inferred as"
            <> prettyText arr_t
      forM_ res $ \(slice, e) -> do
        TC.checkSlice arr_t slice
        TC.require [t `setArrayShape` sliceShape slice] e
    checkKernelResult (TileReturns cs dims v) t = do
      TC.checkCerts cs
      forM_ dims $ \(dim, tile) -> do
        TC.require [Prim int64] dim
        TC.require [Prim int64] tile
      vt <- lookupType v
      unless (vt == t `arrayOfShape` Shape (map snd dims)) $
        TC.bad $
          TC.TypeError $
            "Invalid type for TileReturns " <> prettyText v
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
            <> prettyText expected
            <> ",\ngot:\n  "
            <> prettyText arr_t
      where
        (dims, blk_tiles, reg_tiles) = unzip3 dims_n_tiles
        expected = t `arrayOfShape` Shape (blk_tiles <> reg_tiles)

kernelBodyMetrics :: (OpMetrics (Op rep)) => KernelBody rep -> MetricsM ()
kernelBodyMetrics = mapM_ stmMetrics . kernelBodyStms

instance (PrettyRep rep) => Pretty (KernelBody rep) where
  pretty (KernelBody _ stms res) =
    PP.stack (map pretty (stmsToList stms))
      </> "return"
      <+> PP.braces (PP.commastack $ map pretty res)

certAnnots :: Certs -> [Doc ann]
certAnnots cs
  | cs == mempty = []
  | otherwise = [pretty cs]

instance Pretty KernelResult where
  pretty (Returns ResultNoSimplify cs what) =
    hsep $ certAnnots cs <> ["returns (manifest)" <+> pretty what]
  pretty (Returns ResultPrivate cs what) =
    hsep $ certAnnots cs <> ["returns (private)" <+> pretty what]
  pretty (Returns ResultMaySimplify cs what) =
    hsep $ certAnnots cs <> ["returns" <+> pretty what]
  pretty (WriteReturns cs arr res) =
    hsep $
      certAnnots cs
        <> [pretty arr </> "with" <+> PP.apply (map ppRes res)]
    where
      ppRes (slice, e) = pretty slice <+> "=" <+> pretty e
  pretty (TileReturns cs dims v) =
    hsep $ certAnnots cs <> ["tile" <> apply (map onDim dims) <+> pretty v]
    where
      onDim (dim, tile) = pretty dim <+> "/" <+> pretty tile
  pretty (RegTileReturns cs dims_n_tiles v) =
    hsep $ certAnnots cs <> ["blkreg_tile" <> apply (map onDim dims_n_tiles) <+> pretty v]
    where
      onDim (dim, blk_tile, reg_tile) =
        pretty dim <+> "/" <+> parens (pretty blk_tile <+> "*" <+> pretty reg_tile)

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
  M.fromList $ map (,IndexName Int64) (phys : map fst space)

checkSegSpace :: (TC.Checkable rep) => SegSpace -> TC.TypeM rep ()
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
-- or the block-level.
--
-- The type list is usually the type of the element returned by a
-- single thread. The result of the SegOp is then an array of that
-- type, with the shape of the 'SegSpace' prepended. One exception is
-- for 'WriteReturns', where the type annotation is the /full/ type of
-- the result.
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
segResultShape _ t (WriteReturns {}) =
  t
segResultShape space t Returns {} =
  foldr (flip arrayOfRow) t $ segSpaceDims space
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

instance (ASTConstraints lvl, Aliased rep) => AliasedOp (SegOp lvl rep) where
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
  (TC.Checkable rep) =>
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
              <> prettyTuple (lambdaReturnType op)
              <> " but neutral element has type "
              <> prettyTuple nes_t

      -- Arrays must have proper type.
      let dest_shape' = Shape segment_dims <> dest_shape <> shape
      forM_ (zip nes_t dests) $ \(t, dest) -> do
        TC.requireI [t `arrayOfShape` dest_shape'] dest
        TC.consume =<< TC.lookupAliases dest

      pure $ map (`arrayOfShape` shape) nes_t

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
            <> prettyTuple ts
            <> " but should have type "
            <> prettyTuple bucket_ret_t
  where
    segment_dims = init $ segSpaceDims space

checkScanRed ::
  (TC.Checkable rep) =>
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
        TC.bad $
          TC.TypeError "wrong type for operator or neutral elements."

      pure $ map (`arrayOfShape` shape) nes_t

    let expecting = concat ne_ts
        got = take (length expecting) ts
    unless (expecting == got) $
      TC.bad $
        TC.TypeError $
          "Wrong return for body (does not match neutral elements; expected "
            <> prettyText expecting
            <> "; found "
            <> prettyText got
            <> ")"

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
identitySegOpMapper :: (Monad m) => SegOpMapper lvl rep rep m
identitySegOpMapper =
  SegOpMapper
    { mapOnSegOpSubExp = pure,
      mapOnSegOpLambda = pure,
      mapOnSegOpBody = pure,
      mapOnSegOpVName = pure,
      mapOnSegOpLevel = pure
    }

mapOnSegSpace ::
  (Monad f) => SegOpMapper lvl frep trep f -> SegSpace -> f SegSpace
mapOnSegSpace tv (SegSpace phys dims) =
  SegSpace
    <$> mapOnSegOpVName tv phys
    <*> traverse (bitraverse (mapOnSegOpVName tv) (mapOnSegOpSubExp tv)) dims

mapSegBinOp ::
  (Monad m) =>
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
  (Monad m) =>
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
      HistOp
        <$> mapM (mapOnSegOpSubExp tv) w
        <*> mapOnSegOpSubExp tv rf
        <*> mapM (mapOnSegOpVName tv) arrs
        <*> mapM (mapOnSegOpSubExp tv) nes
        <*> (Shape <$> mapM (mapOnSegOpSubExp tv) (shapeDims shape))
        <*> mapOnSegOpLambda tv op

mapOnSegOpType ::
  (Monad m) =>
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

rephraseBinOp ::
  (Monad f) =>
  Rephraser f from rep ->
  SegBinOp from ->
  f (SegBinOp rep)
rephraseBinOp r (SegBinOp comm lam nes shape) =
  SegBinOp comm <$> rephraseLambda r lam <*> pure nes <*> pure shape

rephraseKernelBody ::
  (Monad f) =>
  Rephraser f from rep ->
  KernelBody from ->
  f (KernelBody rep)
rephraseKernelBody r (KernelBody dec stms res) =
  KernelBody <$> rephraseBodyDec r dec <*> traverse (rephraseStm r) stms <*> pure res

instance RephraseOp (SegOp lvl) where
  rephraseInOp r (SegMap lvl space ts body) =
    SegMap lvl space ts <$> rephraseKernelBody r body
  rephraseInOp r (SegRed lvl space reds ts body) =
    SegRed lvl space
      <$> mapM (rephraseBinOp r) reds
      <*> pure ts
      <*> rephraseKernelBody r body
  rephraseInOp r (SegScan lvl space scans ts body) =
    SegScan lvl space
      <$> mapM (rephraseBinOp r) scans
      <*> pure ts
      <*> rephraseKernelBody r body
  rephraseInOp r (SegHist lvl space hists ts body) =
    SegHist lvl space
      <$> mapM onOp hists
      <*> pure ts
      <*> rephraseKernelBody r body
    where
      onOp (HistOp w rf arrs nes shape op) =
        HistOp w rf arrs nes shape <$> rephraseLambda r op

-- | A helper for defining 'TraverseOpStms'.
traverseSegOpStms :: (Monad m) => OpStmsTraverser m (SegOp lvl rep) rep
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
          { mapOnSegOpSubExp = pure . substituteNames subst,
            mapOnSegOpLambda = pure . substituteNames subst,
            mapOnSegOpBody = pure . substituteNames subst,
            mapOnSegOpVName = pure . substituteNames subst,
            mapOnSegOpLevel = pure . substituteNames subst
          }

instance (ASTRep rep, ASTConstraints lvl) => Rename (SegOp lvl rep) where
  rename op =
    renameBound (M.keys (scopeOfSegSpace (segSpace op))) $ mapSegOpM renamer op
    where
      renamer = SegOpMapper rename rename rename rename rename

instance (ASTRep rep, FreeIn lvl) => FreeIn (SegOp lvl rep) where
  freeIn' e =
    fvBind (namesFromList $ M.keys $ scopeOfSegSpace (segSpace e)) $
      flip execState mempty $
        mapSegOpM free e
    where
      walk f x = modify (<> f x) >> pure x
      free =
        SegOpMapper
          { mapOnSegOpSubExp = walk freeIn',
            mapOnSegOpLambda = walk freeIn',
            mapOnSegOpBody = walk freeIn',
            mapOnSegOpVName = walk freeIn',
            mapOnSegOpLevel = walk freeIn'
          }

instance (OpMetrics (Op rep)) => OpMetrics (SegOp lvl rep) where
  opMetrics (SegMap _ _ _ body) =
    inside "SegMap" $ kernelBodyMetrics body
  opMetrics (SegRed _ _ reds _ body) =
    inside "SegRed" $ do
      mapM_ (inside "SegBinOp" . lambdaMetrics . segBinOpLambda) reds
      kernelBodyMetrics body
  opMetrics (SegScan _ _ scans _ body) =
    inside "SegScan" $ do
      mapM_ (inside "SegBinOp" . lambdaMetrics . segBinOpLambda) scans
      kernelBodyMetrics body
  opMetrics (SegHist _ _ ops _ body) =
    inside "SegHist" $ do
      mapM_ (lambdaMetrics . histOp) ops
      kernelBodyMetrics body

instance Pretty SegSpace where
  pretty (SegSpace phys dims) =
    apply
      ( do
          (i, d) <- dims
          pure $ pretty i <+> "<" <+> pretty d
      )
      <+> parens ("~" <> pretty phys)

instance (PrettyRep rep) => Pretty (SegBinOp rep) where
  pretty (SegBinOp comm lam nes shape) =
    PP.braces (PP.commasep $ map pretty nes)
      <> PP.comma
        </> pretty shape
      <> PP.comma
        </> comm'
      <> pretty lam
    where
      comm' = case comm of
        Commutative -> "commutative "
        Noncommutative -> mempty

instance (PrettyRep rep, PP.Pretty lvl) => PP.Pretty (SegOp lvl rep) where
  pretty (SegMap lvl space ts body) =
    "segmap"
      <> pretty lvl
        </> PP.align (pretty space)
        <+> PP.colon
        <+> ppTuple' (map pretty ts)
        <+> PP.nestedBlock "{" "}" (pretty body)
  pretty (SegRed lvl space reds ts body) =
    "segred"
      <> pretty lvl
        </> PP.align (pretty space)
        </> PP.parens (mconcat $ intersperse (PP.comma <> PP.line) $ map pretty reds)
        </> PP.colon
        <+> ppTuple' (map pretty ts)
        <+> PP.nestedBlock "{" "}" (pretty body)
  pretty (SegScan lvl space scans ts body) =
    "segscan"
      <> pretty lvl
        </> PP.align (pretty space)
        </> PP.parens (mconcat $ intersperse (PP.comma <> PP.line) $ map pretty scans)
        </> PP.colon
        <+> ppTuple' (map pretty ts)
        <+> PP.nestedBlock "{" "}" (pretty body)
  pretty (SegHist lvl space ops ts body) =
    "seghist"
      <> pretty lvl
        </> PP.align (pretty space)
        </> PP.parens (mconcat $ intersperse (PP.comma <> PP.line) $ map ppOp ops)
        </> PP.colon
        <+> ppTuple' (map pretty ts)
        <+> PP.nestedBlock "{" "}" (pretty body)
    where
      ppOp (HistOp w rf dests nes shape op) =
        pretty w
          <> PP.comma
            <+> pretty rf
          <> PP.comma
            </> PP.braces (PP.commasep $ map pretty dests)
          <> PP.comma
            </> PP.braces (PP.commasep $ map pretty nes)
          <> PP.comma
            </> pretty shape
          <> PP.comma
            </> pretty op

instance CanBeAliased (SegOp lvl) where
  addOpAliases aliases = runIdentity . mapSegOpM alias
    where
      alias =
        SegOpMapper
          pure
          (pure . Alias.analyseLambda aliases)
          (pure . aliasAnalyseKernelBody aliases)
          pure
          pure

informKernelBody :: (Informing rep) => KernelBody rep -> KernelBody (Wise rep)
informKernelBody (KernelBody dec stms res) =
  mkWiseKernelBody dec (informStms stms) res

instance CanBeWise (SegOp lvl) where
  addOpWisdom = runIdentity . mapSegOpM add
    where
      add =
        SegOpMapper
          pure
          (pure . informLambda)
          (pure . informKernelBody)
          pure
          pure

instance (ASTRep rep) => ST.IndexOp (SegOp lvl rep) where
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
        | Just (ST.Indexed cs e) <- M.lookup v table = tell cs >> pure e
        | Just (Prim pt) <- ST.lookupType v vtable =
            pure $ LeafExp v pt
        | otherwise = lift Nothing
  indexOp _ _ _ _ = Nothing

instance
  (ASTRep rep, ASTConstraints lvl) =>
  IsOp (SegOp lvl rep)
  where
  cheapOp _ = False
  safeOp _ = True
  opDependencies op = replicate (length (segOpType op)) (freeIn op)

--- Simplification

instance Engine.Simplifiable SegSpace where
  simplify (SegSpace phys dims) =
    SegSpace phys <$> mapM (traverse Engine.simplify) dims

instance Engine.Simplifiable KernelResult where
  simplify (Returns manifest cs what) =
    Returns manifest <$> Engine.simplify cs <*> Engine.simplify what
  simplify (WriteReturns cs a res) =
    WriteReturns
      <$> Engine.simplify cs
      <*> Engine.simplify a
      <*> Engine.simplify res
  simplify (TileReturns cs dims what) =
    TileReturns <$> Engine.simplify cs <*> Engine.simplify dims <*> Engine.simplify what
  simplify (RegTileReturns cs dims_n_tiles what) =
    RegTileReturns
      <$> Engine.simplify cs
      <*> Engine.simplify dims_n_tiles
      <*> Engine.simplify what

mkWiseKernelBody ::
  (Informing rep) =>
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
  (MonadBuilder m) =>
  Stms (Rep m) ->
  [KernelResult] ->
  m (KernelBody (Rep m))
mkKernelBodyM stms kres = do
  Body dec' _ _ <- mkBodyM stms $ subExpsRes res_ses
  pure $ KernelBody dec' stms kres
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
          `Engine.orIf` Engine.isConsuming
          `Engine.orIf` Engine.isDeviceMigrated

  -- Ensure we do not try to use anything that is consumed in the result.
  (body_res, body_stms, hoisted) <-
    Engine.localVtable (flip (foldl' (flip ST.consume)) (foldMap consumedInResult res))
      . Engine.localVtable (<> scope_vtable)
      . Engine.localVtable (\vtable -> vtable {ST.simplifyMemory = True})
      . Engine.enterLoop
      $ Engine.blockIf blocker stms
      $ do
        res' <-
          Engine.localVtable (ST.hideCertified $ namesFromList $ M.keys $ scopeOf stms) $
            mapM Engine.simplify res
        pure (res', UT.usages $ freeIn res')

  pure (mkWiseKernelBody () body_stms body_res, hoisted)
  where
    scope_vtable = segSpaceSymbolTable space
    bound_here = namesFromList $ M.keys $ scopeOfSegSpace space

    consumedInResult (WriteReturns _ arr _) =
      [arr]
    consumedInResult _ =
      []

simplifyLambda ::
  (Engine.SimplifiableRep rep) =>
  Names ->
  Lambda (Wise rep) ->
  Engine.SimpleM rep (Lambda (Wise rep), Stms (Wise rep))
simplifyLambda bound = Engine.blockMigrated . Engine.simplifyLambda bound

segSpaceSymbolTable :: (ASTRep rep) => SegSpace -> ST.SymbolTable rep
segSpaceSymbolTable (SegSpace flat gtids_and_dims) =
  foldl' f (ST.fromScope $ M.singleton flat $ IndexName Int64) gtids_and_dims
  where
    f vtable (gtid, dim) = ST.insertLoopVar gtid Int64 dim vtable

simplifySegBinOp ::
  (Engine.SimplifiableRep rep) =>
  VName ->
  SegBinOp (Wise rep) ->
  Engine.SimpleM rep (SegBinOp (Wise rep), Stms (Wise rep))
simplifySegBinOp phys_id (SegBinOp comm lam nes shape) = do
  (lam', hoisted) <-
    Engine.localVtable (\vtable -> vtable {ST.simplifyMemory = True}) $
      simplifyLambda (oneName phys_id) lam
  shape' <- Engine.simplify shape
  nes' <- mapM Engine.simplify nes
  pure (SegBinOp comm lam' nes' shape', hoisted)

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
  pure
    ( SegMap lvl' space' ts' kbody',
      body_hoisted
    )
simplifySegOp (SegRed lvl space reds ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (reds', reds_hoisted) <-
    Engine.localVtable (<> scope_vtable) $
      mapAndUnzipM (simplifySegBinOp (segFlat space)) reds
  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  pure
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
      mapAndUnzipM (simplifySegBinOp (segFlat space)) scans
  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  pure
    ( SegScan lvl' space' scans' ts' kbody',
      mconcat scans_hoisted <> body_hoisted
    )
  where
    scope = scopeOfSegSpace space
    scope_vtable = ST.fromScope scope
simplifySegOp (SegHist lvl space ops ts kbody) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)

  Engine.localVtable (flip (foldr ST.consume) $ concatMap histDest ops) $ do
    (ops', ops_hoisted) <- fmap unzip . forM ops $
      \(HistOp w rf arrs nes dims lam) -> do
        w' <- Engine.simplify w
        rf' <- Engine.simplify rf
        arrs' <- Engine.simplify arrs
        nes' <- Engine.simplify nes
        dims' <- Engine.simplify dims
        (lam', op_hoisted) <-
          Engine.localVtable (<> scope_vtable) $
            Engine.localVtable (\vtable -> vtable {ST.simplifyMemory = True}) $
              simplifyLambda (oneName (segFlat space)) lam
        pure
          ( HistOp w' rf' arrs' nes' dims' lam',
            op_hoisted
          )

    (kbody', body_hoisted) <- simplifyKernelBody space kbody

    pure
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
  (HasSegOp rep, BuilderOps rep, Buildable rep, Aliased rep) =>
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
  (HasSegOp rep, BuilderOps rep, Aliased rep) =>
  BottomUpRuleOp rep
segOpRuleBottomUp vtable pat dec op
  | Just op' <- asSegOp op =
      bottomUpSegOp vtable pat dec op'
  | otherwise =
      Skip

topDownSegOp ::
  (HasSegOp rep, BuilderOps rep, Buildable rep) =>
  ST.SymbolTable rep ->
  Pat (LetDec rep) ->
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
    Let (Pat kpes') dec $
      Op $
        segOp $
          SegMap lvl space ts' kbody
  where
    isInvariant Constant {} = True
    isInvariant (Var v) = isJust $ ST.lookup v vtable

    checkForInvarianceResult (_, pe, Returns rm cs se)
      | cs == mempty,
        rm == ResultMaySimplify,
        isInvariant se = do
          letBindNames [patElemName pe] $
            BasicOp $
              Replicate (Shape $ segSpaceDims space) se
          pure False
    checkForInvarianceResult _ =
      pure True

-- If a SegRed contains two reduction operations that have the same
-- vector shape, merge them together.  This saves on communication
-- overhead, but can in principle lead to more shared memory usage.
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

    sameShape (op1, _) (op2, _) =
      segBinOpShape op1 == segBinOpShape op2
        && shapeRank (segBinOpShape op1) > 0

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
                  op1_xparams
                    ++ op2_xparams
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
  (Aliased rep, HasSegOp rep, BuilderOps rep) =>
  (ST.SymbolTable rep, UT.UsageTable) ->
  Pat (LetDec rep) ->
  StmAux (ExpDec rep) ->
  SegOp (SegOpLevel rep) rep ->
  Rule rep
-- Some SegOp results can be moved outside the SegOp, which can
-- simplify further analysis.
bottomUpSegOp (vtable, _used) (Pat kpes) dec segop = Simplify $ do
  -- Iterate through the bindings.  For each, we check whether it is
  -- in kres and can be moved outside.  If so, we remove it from kres
  -- and kpes and make it a binding outside.  We have to be careful
  -- not to remove anything that is passed on to a scan/map/histogram
  -- operation.  Fortunately, these are always first in the result
  -- list.
  (kpes', kts', kres', kstms') <-
    localScope (scopeOfSegSpace space) $
      foldM distribute (kpes, kts, kres, mempty) kstms

  when (kpes' == kpes) cannotSimplify

  kbody' <-
    localScope (scopeOfSegSpace space) $ mkKernelBodyM kstms' kres'

  addStm $ Let (Pat kpes') dec $ Op $ segOp $ mk_segop kts' kbody'
  where
    (kts, KernelBody _ kstms kres, num_nonmap_results, mk_segop) =
      segOpGuts segop
    free_in_kstms = foldMap freeIn kstms
    space = segSpace segop

    sliceWithGtidsFixed stm
      | Let _ aux (BasicOp (Index arr slice)) <- stm,
        space_slice <- map (DimFix . Var . fst) $ unSegSpace space,
        space_slice `isPrefixOf` unSlice slice,
        remaining_slice <- Slice $ drop (length space_slice) (unSlice slice),
        all (isJust . flip ST.lookup vtable) $
          namesToList $
            freeIn arr <> freeIn remaining_slice <> freeIn (stmAuxCerts aux) =
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
                      DimSlice (constant (0 :: Int64)) d (constant (1 :: Int64))
                  )
                  $ segSpaceDims space
              index kpe' =
                letBindNames [patElemName kpe'] . BasicOp . Index arr $
                  Slice $
                    outer_slice <> remaining_slice
          precopy <- newVName $ baseString (patElemName kpe) <> "_precopy"
          index kpe {patElemName = precopy}
          letBindNames [patElemName kpe] $ BasicOp $ Replicate mempty $ Var precopy
          pure
            ( kpes'',
              kts'',
              kres'',
              if patElemName pe `nameIn` free_in_kstms
                then kstms' <> oneStm stm
                else kstms'
            )
    distribute (kpes', kts', kres', kstms') stm =
      pure (kpes', kts', kres', kstms' <> oneStm stm)

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
    correct (WriteReturns _ arr _) _ = varReturns arr
    correct _ ret = pure ret

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
