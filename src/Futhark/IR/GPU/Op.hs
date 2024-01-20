{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Futhark.IR.GPU.Op
  ( -- * Size operations
    SizeOp (..),

    -- * Host operations
    HostOp (..),
    traverseHostOpStms,
    typeCheckHostOp,

    -- * SegOp refinements
    SegLevel (..),
    segVirt,
    SegVirt (..),
    SegSeqDims (..),
    KernelGrid (..),

    -- * Reexports
    module Futhark.IR.GPU.Sizes,
    module Futhark.IR.SegOp,
  )
where

import Control.Monad
import Data.Sequence qualified as SQ
import Data.Text qualified as T
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Analysis.Metrics
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.IR
import Futhark.IR.Aliases (Aliases, CanBeAliased (..))
import Futhark.IR.GPU.Sizes
import Futhark.IR.Prop.Aliases
import Futhark.IR.SegOp
import Futhark.IR.TypeCheck qualified as TC
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.Pretty
  ( commasep,
    parens,
    ppTuple',
    pretty,
    (<+>),
  )
import Futhark.Util.Pretty qualified as PP

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
-- intra-block parallelism in GPU code, as we have not yet found it
-- useful anywhere else.
newtype SegSeqDims = SegSeqDims {segSeqDims :: [Int]}
  deriving (Eq, Ord, Show)

-- | Do we need block-virtualisation when generating code for the
-- segmented operation?  In most cases, we do, but for some simple
-- kernels, we compute the full number of blocks in advance, and then
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

-- | The actual, physical grid dimensions used for the GPU kernel
-- running this 'SegOp'.
data KernelGrid = KernelGrid
  { gridNumBlocks :: Count NumBlocks SubExp,
    gridBlockSize :: Count BlockSize SubExp
  }
  deriving (Eq, Ord, Show)

-- | At which level the *body* of a t'SegOp' executes.
data SegLevel
  = SegThread SegVirt (Maybe KernelGrid)
  | SegBlock SegVirt (Maybe KernelGrid)
  | SegThreadInBlock SegVirt
  deriving (Eq, Ord, Show)

-- | The 'SegVirt' of the 'SegLevel'.
segVirt :: SegLevel -> SegVirt
segVirt (SegThread v _) = v
segVirt (SegBlock v _) = v
segVirt (SegThreadInBlock v) = v

instance PP.Pretty SegVirt where
  pretty SegNoVirt = mempty
  pretty (SegNoVirtFull dims) = "full" <+> pretty (segSeqDims dims)
  pretty SegVirt = "virtualise"

instance PP.Pretty KernelGrid where
  pretty (KernelGrid num_tblocks tblock_size) =
    "grid="
      <> pretty num_tblocks
      <> PP.semi
        <+> "blocksize="
      <> pretty tblock_size

instance PP.Pretty SegLevel where
  pretty (SegThread virt grid) =
    PP.parens ("thread" <> PP.semi <+> pretty virt <> PP.semi <+> pretty grid)
  pretty (SegBlock virt grid) =
    PP.parens ("block" <> PP.semi <+> pretty virt <> PP.semi <+> pretty grid)
  pretty (SegThreadInBlock virt) =
    PP.parens ("inblock" <> PP.semi <+> pretty virt)

instance Engine.Simplifiable KernelGrid where
  simplify (KernelGrid num_tblocks tblock_size) =
    KernelGrid
      <$> traverse Engine.simplify num_tblocks
      <*> traverse Engine.simplify tblock_size

instance Engine.Simplifiable SegLevel where
  simplify (SegThread virt grid) =
    SegThread virt <$> Engine.simplify grid
  simplify (SegBlock virt grid) =
    SegBlock virt <$> Engine.simplify grid
  simplify (SegThreadInBlock virt) =
    pure $ SegThreadInBlock virt

instance Substitute KernelGrid where
  substituteNames substs (KernelGrid num_tblocks tblock_size) =
    KernelGrid
      (substituteNames substs num_tblocks)
      (substituteNames substs tblock_size)

instance Substitute SegLevel where
  substituteNames substs (SegThread virt grid) =
    SegThread virt (substituteNames substs grid)
  substituteNames substs (SegBlock virt grid) =
    SegBlock virt (substituteNames substs grid)
  substituteNames _ (SegThreadInBlock virt) =
    SegThreadInBlock virt

instance Rename SegLevel where
  rename = substituteRename

instance FreeIn KernelGrid where
  freeIn' (KernelGrid num_tblocks tblock_size) =
    freeIn' (num_tblocks, tblock_size)

instance FreeIn SegLevel where
  freeIn' (SegThread _virt grid) = freeIn' grid
  freeIn' (SegBlock _virt grid) = freeIn' grid
  freeIn' (SegThreadInBlock _virt) = mempty

-- | A simple size-level query or computation.
data SizeOp
  = -- | Produce some runtime-configurable size.
    GetSize Name SizeClass
  | -- | The maximum size of some class.
    GetSizeMax SizeClass
  | -- | Compare size (likely a threshold) with some integer value.
    CmpSizeLe Name SizeClass SubExp
  | -- | @CalcNumBlocks w max_num_tblocks tblock_size@ calculates the
    -- number of GPU threadblocks to use for an input of the given size.
    -- The @Name@ is a size name.  Note that @w@ is an i64 to avoid
    -- overflow issues.
    CalcNumBlocks SubExp Name SubExp
  deriving (Eq, Ord, Show)

instance Substitute SizeOp where
  substituteNames substs (CmpSizeLe name sclass x) =
    CmpSizeLe name sclass (substituteNames substs x)
  substituteNames substs (CalcNumBlocks w max_num_tblocks tblock_size) =
    CalcNumBlocks
      (substituteNames substs w)
      max_num_tblocks
      (substituteNames substs tblock_size)
  substituteNames _ op = op

instance Rename SizeOp where
  rename (CmpSizeLe name sclass x) =
    CmpSizeLe name sclass <$> rename x
  rename (CalcNumBlocks w max_num_tblocks tblock_size) =
    CalcNumBlocks <$> rename w <*> pure max_num_tblocks <*> rename tblock_size
  rename x = pure x

instance IsOp SizeOp where
  safeOp _ = True
  cheapOp _ = True
  opDependencies op = [freeIn op]

instance TypedOp SizeOp where
  opType (GetSize _ _) = pure [Prim int64]
  opType (GetSizeMax _) = pure [Prim int64]
  opType CmpSizeLe {} = pure [Prim Bool]
  opType CalcNumBlocks {} = pure [Prim int64]

instance AliasedOp SizeOp where
  opAliases _ = [mempty]
  consumedInOp _ = mempty

instance FreeIn SizeOp where
  freeIn' (CmpSizeLe _ _ x) = freeIn' x
  freeIn' (CalcNumBlocks w _ tblock_size) = freeIn' w <> freeIn' tblock_size
  freeIn' _ = mempty

instance PP.Pretty SizeOp where
  pretty (GetSize name size_class) =
    "get_size" <> parens (commasep [pretty name, pretty size_class])
  pretty (GetSizeMax size_class) =
    "get_size_max" <> parens (commasep [pretty size_class])
  pretty (CmpSizeLe name size_class x) =
    "cmp_size"
      <> parens (commasep [pretty name, pretty size_class])
        <+> "<="
        <+> pretty x
  pretty (CalcNumBlocks w max_num_tblocks tblock_size) =
    "calc_num_tblocks" <> parens (commasep [pretty w, pretty max_num_tblocks, pretty tblock_size])

instance OpMetrics SizeOp where
  opMetrics GetSize {} = seen "GetSize"
  opMetrics GetSizeMax {} = seen "GetSizeMax"
  opMetrics CmpSizeLe {} = seen "CmpSizeLe"
  opMetrics CalcNumBlocks {} = seen "CalcNumBlocks"

typeCheckSizeOp :: (TC.Checkable rep) => SizeOp -> TC.TypeM rep ()
typeCheckSizeOp GetSize {} = pure ()
typeCheckSizeOp GetSizeMax {} = pure ()
typeCheckSizeOp (CmpSizeLe _ _ x) = TC.require [Prim int64] x
typeCheckSizeOp (CalcNumBlocks w _ tblock_size) = do
  TC.require [Prim int64] w
  TC.require [Prim int64] tblock_size

-- | A host-level operation; parameterised by what else it can do.
data HostOp op rep
  = -- | A segmented operation.
    SegOp (SegOp SegLevel rep)
  | SizeOp SizeOp
  | OtherOp (op rep)
  | -- | Code to run sequentially on the GPU,
    -- in a single thread.
    GPUBody [Type] (Body rep)
  deriving (Eq, Ord, Show)

-- | A helper for defining 'TraverseOpStms'.
traverseHostOpStms ::
  (Monad m) =>
  OpStmsTraverser m (op rep) rep ->
  OpStmsTraverser m (HostOp op rep) rep
traverseHostOpStms _ f (SegOp segop) = SegOp <$> traverseSegOpStms f segop
traverseHostOpStms _ _ (SizeOp sizeop) = pure $ SizeOp sizeop
traverseHostOpStms onOtherOp f (OtherOp other) = OtherOp <$> onOtherOp f other
traverseHostOpStms _ f (GPUBody ts body) = do
  stms <- f mempty $ bodyStms body
  pure $ GPUBody ts $ body {bodyStms = stms}

instance (ASTRep rep, Substitute (op rep)) => Substitute (HostOp op rep) where
  substituteNames substs (SegOp op) =
    SegOp $ substituteNames substs op
  substituteNames substs (OtherOp op) =
    OtherOp $ substituteNames substs op
  substituteNames substs (SizeOp op) =
    SizeOp $ substituteNames substs op
  substituteNames substs (GPUBody ts body) =
    GPUBody (substituteNames substs ts) (substituteNames substs body)

instance (ASTRep rep, Rename (op rep)) => Rename (HostOp op rep) where
  rename (SegOp op) = SegOp <$> rename op
  rename (OtherOp op) = OtherOp <$> rename op
  rename (SizeOp op) = SizeOp <$> rename op
  rename (GPUBody ts body) = GPUBody <$> rename ts <*> rename body

instance (ASTRep rep, IsOp (op rep)) => IsOp (HostOp op rep) where
  safeOp (SegOp op) = safeOp op
  safeOp (OtherOp op) = safeOp op
  safeOp (SizeOp op) = safeOp op
  safeOp (GPUBody _ body) = all (safeExp . stmExp) $ bodyStms body

  cheapOp (SegOp op) = cheapOp op
  cheapOp (OtherOp op) = cheapOp op
  cheapOp (SizeOp op) = cheapOp op
  cheapOp (GPUBody types body) =
    -- Current GPUBody usage only benefits from hoisting kernels that
    -- transfer scalars to device.
    SQ.null (bodyStms body) && all ((== 0) . arrayRank) types

  opDependencies (SegOp op) = opDependencies op
  opDependencies (OtherOp op) = opDependencies op
  opDependencies op@(SizeOp {}) = [freeIn op]
  opDependencies (GPUBody _ body) =
    replicate (length . bodyResult $ body) (freeIn body)

instance (TypedOp (op rep)) => TypedOp (HostOp op rep) where
  opType (SegOp op) = opType op
  opType (OtherOp op) = opType op
  opType (SizeOp op) = opType op
  opType (GPUBody ts _) =
    pure $ staticShapes $ map (`arrayOfRow` intConst Int64 1) ts

instance (Aliased rep, AliasedOp (op rep)) => AliasedOp (HostOp op rep) where
  opAliases (SegOp op) = opAliases op
  opAliases (OtherOp op) = opAliases op
  opAliases (SizeOp op) = opAliases op
  opAliases (GPUBody ts _) = map (const mempty) ts

  consumedInOp (SegOp op) = consumedInOp op
  consumedInOp (OtherOp op) = consumedInOp op
  consumedInOp (SizeOp op) = consumedInOp op
  consumedInOp (GPUBody _ body) = consumedInBody body

instance (ASTRep rep, FreeIn (op rep)) => FreeIn (HostOp op rep) where
  freeIn' (SegOp op) = freeIn' op
  freeIn' (OtherOp op) = freeIn' op
  freeIn' (SizeOp op) = freeIn' op
  freeIn' (GPUBody ts body) = freeIn' ts <> freeIn' body

instance (CanBeAliased op) => CanBeAliased (HostOp op) where
  addOpAliases aliases (SegOp op) = SegOp $ addOpAliases aliases op
  addOpAliases aliases (GPUBody ts body) = GPUBody ts $ Alias.analyseBody aliases body
  addOpAliases aliases (OtherOp op) = OtherOp $ addOpAliases aliases op
  addOpAliases _ (SizeOp op) = SizeOp op

instance (CanBeWise op) => CanBeWise (HostOp op) where
  addOpWisdom (SegOp op) = SegOp $ addOpWisdom op
  addOpWisdom (OtherOp op) = OtherOp $ addOpWisdom op
  addOpWisdom (SizeOp op) = SizeOp op
  addOpWisdom (GPUBody ts body) = GPUBody ts $ informBody body

instance (ASTRep rep, ST.IndexOp (op rep)) => ST.IndexOp (HostOp op rep) where
  indexOp vtable k (SegOp op) is = ST.indexOp vtable k op is
  indexOp vtable k (OtherOp op) is = ST.indexOp vtable k op is
  indexOp _ _ _ _ = Nothing

instance (PrettyRep rep, PP.Pretty (op rep)) => PP.Pretty (HostOp op rep) where
  pretty (SegOp op) = pretty op
  pretty (OtherOp op) = pretty op
  pretty (SizeOp op) = pretty op
  pretty (GPUBody ts body) =
    "gpu" <+> PP.colon <+> ppTuple' (map pretty ts) <+> PP.nestedBlock "{" "}" (pretty body)

instance (OpMetrics (Op rep), OpMetrics (op rep)) => OpMetrics (HostOp op rep) where
  opMetrics (SegOp op) = opMetrics op
  opMetrics (OtherOp op) = opMetrics op
  opMetrics (SizeOp op) = opMetrics op
  opMetrics (GPUBody _ body) = inside "GPUBody" $ bodyMetrics body

instance (RephraseOp op) => RephraseOp (HostOp op) where
  rephraseInOp r (SegOp op) = SegOp <$> rephraseInOp r op
  rephraseInOp r (OtherOp op) = OtherOp <$> rephraseInOp r op
  rephraseInOp _ (SizeOp op) = pure $ SizeOp op
  rephraseInOp r (GPUBody ts body) = GPUBody ts <$> rephraseBody r body

checkGrid :: (TC.Checkable rep) => KernelGrid -> TC.TypeM rep ()
checkGrid grid = do
  TC.require [Prim int64] $ unCount $ gridNumBlocks grid
  TC.require [Prim int64] $ unCount $ gridBlockSize grid

checkSegLevel ::
  (TC.Checkable rep) =>
  Maybe SegLevel ->
  SegLevel ->
  TC.TypeM rep ()
checkSegLevel (Just SegBlock {}) (SegThreadInBlock _virt) =
  pure ()
checkSegLevel _ (SegThreadInBlock _virt) =
  TC.bad $ TC.TypeError "inblock SegOp not in block SegOp."
checkSegLevel (Just SegThread {}) _ =
  TC.bad $ TC.TypeError "SegOps cannot occur when already at thread level."
checkSegLevel (Just SegThreadInBlock {}) _ =
  TC.bad $ TC.TypeError "SegOps cannot occur when already at inblock level."
checkSegLevel _ (SegThread _virt Nothing) =
  pure ()
checkSegLevel (Just _) SegThread {} =
  TC.bad $ TC.TypeError "thread-level SegOp cannot be nested"
checkSegLevel Nothing (SegThread _virt grid) =
  mapM_ checkGrid grid
checkSegLevel (Just _) SegBlock {} =
  TC.bad $ TC.TypeError "block-level SegOp cannot be nested"
checkSegLevel Nothing (SegBlock _virt grid) =
  mapM_ checkGrid grid

typeCheckHostOp ::
  (TC.Checkable rep) =>
  (SegLevel -> Op (Aliases rep) -> TC.TypeM rep ()) ->
  Maybe SegLevel ->
  (op (Aliases rep) -> TC.TypeM rep ()) ->
  HostOp op (Aliases rep) ->
  TC.TypeM rep ()
typeCheckHostOp checker lvl _ (SegOp op) =
  TC.checkOpWith (checker $ segLevel op) $
    typeCheckSegOp (checkSegLevel lvl) op
typeCheckHostOp _ Just {} _ GPUBody {} =
  TC.bad $ TC.TypeError "GPUBody may not be nested in SegOps."
typeCheckHostOp _ _ f (OtherOp op) = f op
typeCheckHostOp _ _ _ (SizeOp op) = typeCheckSizeOp op
typeCheckHostOp _ Nothing _ (GPUBody ts body) = do
  mapM_ TC.checkType ts
  void $ TC.checkBody body
  body_ts <-
    extendedScope
      (traverse subExpResType (bodyResult body))
      (scopeOf (bodyStms body))
  unless (body_ts == ts) . TC.bad . TC.TypeError . T.unlines $
    [ "Expected type: " <> prettyTuple ts,
      "Got body type: " <> prettyTuple body_ts
    ]
