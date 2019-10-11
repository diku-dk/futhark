{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.Representation.Kernels.Kernel
       ( HistOp(..)
       , histType
       , SegRedOp(..)
       , segRedResults
       , KernelBody(..)
       , KernelResult(..)
       , kernelResultSubExp
       , SplitOrdering(..)

       -- * Segmented operations
       , SegOp(..)
       , SegLevel(..)
       , SegVirt(..)
       , segLevel
       , segSpace
       , typeCheckSegOp
       , SegSpace(..)
       , scopeOfSegSpace
       , segSpaceDims

       -- ** Generic traversal
       , SegOpMapper(..)
       , identitySegOpMapper
       , mapSegOpM
       , SegOpWalker(..)
       , identitySegOpWalker
       , walkSegOpM

       -- * Size operations
       , SizeOp(..)

       -- * Host operations
       , HostOp(..)
       , typeCheckHostOp

       -- * Reexports
       , module Futhark.Representation.Kernels.Sizes
       )
       where

import Control.Arrow (first)
import Control.Monad.State.Strict
import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Identity hiding (mapM_)
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.List

import Futhark.Representation.AST
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.ScalExp as SE
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Util.Pretty as PP
import Futhark.Util.Pretty
  ((</>), (<+>), ppr, commasep, Pretty, parens, text)
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplify.Lore
import Futhark.Representation.Ranges
  (Ranges, removeLambdaRanges, removeStmRanges, mkBodyRanges)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases
  (Aliases, removeLambdaAliases, removeStmAliases)
import Futhark.Representation.Kernels.Sizes
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics
import qualified Futhark.Analysis.Range as Range
import Futhark.Util (maybeNth)

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
-- different from the type of the 'HistDest's in case we are
-- dealing with a segmented histogram.
histType :: HistOp lore -> [Type]
histType op = map ((`arrayOfRow` histWidth op) .
                        (`arrayOfShape` histShape op)) $
                   lambdaReturnType $ histOp op

data SegRedOp lore =
  SegRedOp { segRedComm :: Commutativity
           , segRedLambda :: Lambda lore
           , segRedNeutral :: [SubExp]
           , segRedShape :: Shape
             -- ^ In case this operator is semantically a vectorised
             -- operator (corresponding to a perfect map nest in the
             -- SOACS representation), these are the logical
             -- "dimensions".  This is used to generate more efficient
             -- code.
           }
  deriving (Eq, Ord, Show)

-- | How many reduction results are produced by these 'SegRedOp's?
segRedResults :: [SegRedOp lore] -> Int
segRedResults = sum . map (length . segRedNeutral)
-- | The body of a 'Kernel'.
data KernelBody lore = KernelBody { kernelBodyLore :: BodyAttr lore
                                  , kernelBodyStms :: Stms lore
                                  , kernelBodyResult :: [KernelResult]
                                  }

deriving instance Annotations lore => Ord (KernelBody lore)
deriving instance Annotations lore => Show (KernelBody lore)
deriving instance Annotations lore => Eq (KernelBody lore)

data KernelResult = Returns SubExp
                    -- ^ Each "worker" in the kernel returns this.
                    -- Whether this is a result-per-thread or a
                    -- result-per-group depends on the 'SegLevel'.
                  | WriteReturns
                    [SubExp] -- Size of array.  Must match number of dims.
                    VName -- Which array
                    [([SubExp], SubExp)]
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

kernelResultSubExp :: KernelResult -> SubExp
kernelResultSubExp (Returns se) = se
kernelResultSubExp (WriteReturns _ arr _) = Var arr
kernelResultSubExp (ConcatReturns _ _ _ v) = Var v
kernelResultSubExp (TileReturns _ v) = Var v

instance FreeIn KernelResult where
  freeIn' (Returns what) = freeIn' what
  freeIn' (WriteReturns rws arr res) = freeIn' rws <> freeIn' arr <> freeIn' res
  freeIn' (ConcatReturns o w per_thread_elems v) =
    freeIn' o <> freeIn' w <> freeIn' per_thread_elems <> freeIn' v
  freeIn' (TileReturns dims v) =
    freeIn' dims <> freeIn' v

instance Attributes lore => FreeIn (KernelBody lore) where
  freeIn' (KernelBody attr stms res) =
    fvBind bound_in_stms $ freeIn' attr <> freeIn' stms <> freeIn' res
    where bound_in_stms = fold $ fmap boundByStm stms

instance Attributes lore => Substitute (KernelBody lore) where
  substituteNames subst (KernelBody attr stms res) =
    KernelBody
    (substituteNames subst attr)
    (substituteNames subst stms)
    (substituteNames subst res)

instance Substitute KernelResult where
  substituteNames subst (Returns se) =
    Returns $ substituteNames subst se
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

instance Attributes lore => Rename (KernelBody lore) where
  rename (KernelBody attr stms res) = do
    attr' <- rename attr
    renamingStms stms $ \stms' ->
      KernelBody attr' stms' <$> rename res

instance Rename KernelResult where
  rename = substituteRename

aliasAnalyseKernelBody :: (Attributes lore,
                           CanBeAliased (Op lore)) =>
                          KernelBody lore
                       -> KernelBody (Aliases lore)
aliasAnalyseKernelBody (KernelBody attr stms res) =
  let Body attr' stms' _ = Alias.analyseBody $ Body attr stms []
  in KernelBody attr' stms' res

removeKernelBodyAliases :: CanBeAliased (Op lore) =>
                           KernelBody (Aliases lore) -> KernelBody lore
removeKernelBodyAliases (KernelBody (_, attr) stms res) =
  KernelBody attr (fmap removeStmAliases stms) res

addKernelBodyRanges :: (Attributes lore, CanBeRanged (Op lore)) =>
                       KernelBody lore -> Range.RangeM (KernelBody (Ranges lore))
addKernelBodyRanges (KernelBody attr stms res) =
  Range.analyseStms stms $ \stms' -> do
  let attr' = (mkBodyRanges stms $ map kernelResultSubExp res, attr)
  return $ KernelBody attr' stms' res

removeKernelBodyRanges :: CanBeRanged (Op lore) =>
                          KernelBody (Ranges lore) -> KernelBody lore
removeKernelBodyRanges (KernelBody (_, attr) stms res) =
  KernelBody attr (fmap removeStmRanges stms) res

removeKernelBodyWisdom :: CanBeWise (Op lore) =>
                          KernelBody (Wise lore) -> KernelBody lore
removeKernelBodyWisdom (KernelBody attr stms res) =
  let Body attr' stms' _ = removeBodyWisdom $ Body attr stms []
  in KernelBody attr' stms' res

consumedInKernelBody :: Aliased lore =>
                        KernelBody lore -> Names
consumedInKernelBody (KernelBody attr stms res) =
  consumedInBody (Body attr stms []) <> mconcat (map consumedByReturn res)
  where consumedByReturn (WriteReturns _ a _) = oneName a
        consumedByReturn _                    = mempty

checkKernelBody :: TC.Checkable lore =>
                   [Type] -> KernelBody (Aliases lore) -> TC.TypeM lore ()
checkKernelBody ts (KernelBody (_, attr) stms kres) = do
  TC.checkBodyLore attr
  TC.checkStms stms $ do
    unless (length ts == length kres) $
      TC.bad $ TC.TypeError $ "Kernel return type is " ++ prettyTuple ts ++
      ", but body returns " ++ show (length kres) ++ " values."
    zipWithM_ checkKernelResult kres ts

  where checkKernelResult (Returns what) t =
          TC.require [t] what
        checkKernelResult (WriteReturns rws arr res) t = do
          mapM_ (TC.require [Prim int32]) rws
          arr_t <- lookupType arr
          forM_ res $ \(is, e) -> do
            mapM_ (TC.require [Prim int32]) is
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
kernelBodyMetrics = mapM_ bindingMetrics . kernelBodyStms

instance PrettyLore lore => Pretty (KernelBody lore) where
  ppr (KernelBody _ stms res) =
    PP.stack (map ppr (stmsToList stms)) </>
    text "return" <+> PP.braces (PP.commasep $ map ppr res)

instance Pretty KernelResult where
  ppr (Returns what) =
    text "thread returns" <+> ppr what
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

--- Segmented operations

-- | Do we need group-virtualisation when generating code for the
-- segmented operation?  In most cases, we do, but for some simple
-- kernels, we compute the full number of groups in advance, and then
-- virtualisation is an unnecessary (but generally very small)
-- overhead.  This only really matters for fairly trivial but very
-- wide @map@ kernels where each thread performs constant-time work on
-- scalars.
data SegVirt = SegVirt | SegNoVirt
             deriving (Eq, Ord, Show)

-- | At which level the *body* of a 'SegOp' executes.
data SegLevel = SegThread { segNumGroups :: Count NumGroups SubExp
                          , segGroupSize :: Count GroupSize SubExp
                          , segVirt :: SegVirt }
              | SegGroup { segNumGroups :: Count NumGroups SubExp
                         , segGroupSize :: Count GroupSize SubExp
                         , segVirt :: SegVirt }
              | SegThreadScalar { segNumGroups :: Count NumGroups SubExp
                                , segGroupSize :: Count GroupSize SubExp
                                , segVirt :: SegVirt }
                -- ^ Like 'SegThread', but with the invariant that the
                -- results produced are only used within the same
                -- physical thread later on, and can thus be kept in
                -- registers.  May only occur within an enclosing
                -- 'SegGroup' construct.
              deriving (Eq, Ord, Show)

-- | Index space of a 'SegOp'.
data SegSpace = SegSpace { segFlat :: VName
                         -- ^ Flat physical index corresponding to the
                         -- dimensions (at code generation used for a
                         -- thread ID or similar).
                         , unSegSpace :: [(VName, SubExp)]
                         }
              deriving (Eq, Ord, Show)


segSpaceDims :: SegSpace -> [SubExp]
segSpaceDims (SegSpace _ space) = map snd space

scopeOfSegSpace :: SegSpace -> Scope lore
scopeOfSegSpace (SegSpace phys space) =
  M.fromList $ zip (phys : map fst space) $ repeat $ IndexInfo Int32

checkSegSpace :: TC.Checkable lore => SegSpace -> TC.TypeM lore ()
checkSegSpace (SegSpace _ dims) =
  mapM_ (TC.require [Prim int32] . snd) dims

data SegOp lore = SegMap SegLevel SegSpace [Type] (KernelBody lore)
                | SegRed SegLevel SegSpace [SegRedOp lore] [Type] (KernelBody lore)
                  -- ^ The KernelSpace must always have at least two dimensions,
                  -- implying that the result of a SegRed is always an array.
                | SegScan SegLevel SegSpace (Lambda lore) [SubExp] [Type] (KernelBody lore)
                | SegHist SegLevel SegSpace [HistOp lore] [Type] (KernelBody lore)
                deriving (Eq, Ord, Show)

segLevel :: SegOp lore -> SegLevel
segLevel (SegMap lvl _ _ _) = lvl
segLevel (SegRed lvl _ _ _ _) = lvl
segLevel (SegScan lvl _ _ _ _ _) = lvl
segLevel (SegHist lvl _ _ _ _) = lvl

segSpace :: SegOp lore -> SegSpace
segSpace (SegMap _ lvl _ _) = lvl
segSpace (SegRed _ lvl _ _ _) = lvl
segSpace (SegScan _ lvl _ _ _ _) = lvl
segSpace (SegHist _ lvl _ _ _) = lvl

segResultShape :: SegSpace -> Type -> KernelResult -> Type
segResultShape _ t (WriteReturns rws _ _) =
  t `arrayOfShape` Shape rws
segResultShape space t (Returns _) =
  foldr (flip arrayOfRow) t $ segSpaceDims space
segResultShape _ t (ConcatReturns _ w _ _) =
  t `arrayOfRow` w
segResultShape _ t (TileReturns dims _) =
  t `arrayOfShape` Shape (map fst dims)

segOpType :: SegOp lore -> [Type]
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
          let shape = Shape segment_dims <> segRedShape op
          map (`arrayOfShape` shape) (lambdaReturnType $ segRedLambda op)
segOpType (SegScan _ space _ nes ts kbody) =
  map (`arrayOfShape` Shape dims) scan_ts ++
  zipWith (segResultShape space) map_ts
  (drop (length scan_ts) $ kernelBodyResult kbody)
  where dims = segSpaceDims space
        (scan_ts, map_ts) = splitAt (length nes) ts
segOpType (SegHist _ space ops _ _) = do
  op <- ops
  let shape = Shape (segment_dims <> [histWidth op]) <> histShape op
  map (`arrayOfShape` shape) (lambdaReturnType $ histOp op)
  where dims = segSpaceDims space
        segment_dims = init dims

instance TypedOp (SegOp lore) where
  opType = pure . staticShapes . segOpType

instance (Attributes lore, Aliased lore) => AliasedOp (SegOp lore) where
  opAliases = map (const mempty) . segOpType

  consumedInOp (SegMap _ _ _ kbody) =
    consumedInKernelBody kbody
  consumedInOp (SegRed _ _ _ _ kbody) =
    consumedInKernelBody kbody
  consumedInOp (SegScan _ _ _ _ _ kbody) =
    consumedInKernelBody kbody
  consumedInOp (SegHist _ _ ops _ kbody) =
    namesFromList (concatMap histDest ops) <> consumedInKernelBody kbody

checkSegLevel :: Maybe SegLevel -> SegLevel -> TC.TypeM lore ()
checkSegLevel Nothing SegThreadScalar{} =
  TC.bad $ TC.TypeError "SegThreadScalar at top level."
checkSegLevel Nothing _ =
  return ()
checkSegLevel (Just SegThread{}) _ =
  TC.bad $ TC.TypeError "SegOps cannot occur when already at thread level."
checkSegLevel (Just x) y
  | x == y = TC.bad $ TC.TypeError $ "Already at at level " ++ pretty x
  | segNumGroups x /= segNumGroups y || segGroupSize x /= segGroupSize y =
      TC.bad $ TC.TypeError "Physical layout for SegLevel does not match parent SegLevel."
  | otherwise =
      return ()

checkSegBasics :: TC.Checkable lore =>
                  Maybe SegLevel -> SegLevel -> SegSpace -> [Type] -> TC.TypeM lore ()
checkSegBasics cur_lvl lvl space ts = do
  checkSegLevel cur_lvl lvl
  checkSegSpace space
  mapM_ TC.checkType ts

typeCheckSegOp :: TC.Checkable lore =>
                  Maybe SegLevel -> SegOp (Aliases lore) -> TC.TypeM lore ()
typeCheckSegOp cur_lvl (SegMap lvl space ts kbody) =
  checkScanRed cur_lvl lvl space [] ts kbody

typeCheckSegOp cur_lvl (SegRed lvl space reds ts body) =
  checkScanRed cur_lvl lvl space reds' ts body
  where reds' = zip3
                (map segRedLambda reds)
                (map segRedNeutral reds)
                (map segRedShape reds)

typeCheckSegOp cur_lvl (SegScan lvl space scan_op nes ts body) =
  checkScanRed cur_lvl lvl space [(scan_op, nes, mempty)] ts body

typeCheckSegOp cur_lvl (SegHist lvl space ops ts kbody) = do
  checkSegBasics cur_lvl lvl space ts

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
                Maybe SegLevel -> SegLevel
             -> SegSpace
             -> [(Lambda (Aliases lore), [SubExp], Shape)]
             -> [Type]
             -> KernelBody (Aliases lore)
             -> TC.TypeM lore ()
checkScanRed cur_lvl lvl space ops ts kbody = do
  checkSegBasics cur_lvl lvl space ts

  TC.binding (scopeOfSegSpace space) $ do
    ne_ts <- forM ops $ \(lam, nes, shape) -> do
      mapM_ (TC.require [Prim int32]) $ shapeDims shape
      nes' <- mapM TC.checkArg nes

      -- Operator type must match the type of neutral elements.
      let stripVecDims = stripArray $ shapeRank shape
      TC.checkLambda lam $ map (TC.noArgAliases . first stripVecDims) $ nes' ++ nes'
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
data SegOpMapper flore tlore m = SegOpMapper {
    mapOnSegOpSubExp :: SubExp -> m SubExp
  , mapOnSegOpLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnSegOpBody :: KernelBody flore -> m (KernelBody tlore)
  , mapOnSegOpVName :: VName -> m VName
  }

-- | A mapper that simply returns the 'SegOp' verbatim.
identitySegOpMapper :: Monad m => SegOpMapper lore lore m
identitySegOpMapper = SegOpMapper { mapOnSegOpSubExp = return
                                  , mapOnSegOpLambda = return
                                  , mapOnSegOpBody = return
                                  , mapOnSegOpVName = return
                                  }

mapOnSegSpace :: Monad f =>
                 SegOpMapper flore tlore f -> SegSpace -> f SegSpace
mapOnSegSpace tv (SegSpace phys dims) =
  SegSpace phys <$> traverse (traverse $ mapOnSegOpSubExp tv) dims

mapSegOpM :: (Applicative m, Monad m) =>
              SegOpMapper flore tlore m -> SegOp flore -> m (SegOp tlore)
mapSegOpM tv (SegMap lvl space ts body) =
  SegMap
  <$> mapOnSegLevel tv lvl
  <*> mapOnSegSpace tv space
  <*> mapM (mapOnSegOpType tv) ts
  <*> mapOnSegOpBody tv body
mapSegOpM tv (SegRed lvl space reds ts lam) =
  SegRed
  <$> mapOnSegLevel tv lvl
  <*> mapOnSegSpace tv space
  <*> mapM onSegOp reds
  <*> mapM (mapOnType $ mapOnSegOpSubExp tv) ts
  <*> mapOnSegOpBody tv lam
  where onSegOp (SegRedOp comm red_op nes shape) =
          SegRedOp comm
          <$> mapOnSegOpLambda tv red_op
          <*> mapM (mapOnSegOpSubExp tv) nes
          <*> (Shape <$> mapM (mapOnSegOpSubExp tv) (shapeDims shape))
mapSegOpM tv (SegScan lvl space scan_op nes ts body) =
  SegScan
  <$> mapOnSegLevel tv lvl
  <*> mapOnSegSpace tv space
  <*> mapOnSegOpLambda tv scan_op
  <*> mapM (mapOnSegOpSubExp tv) nes
  <*> mapM (mapOnType $ mapOnSegOpSubExp tv) ts
  <*> mapOnSegOpBody tv body
mapSegOpM tv (SegHist lvl space ops ts body) =
  SegHist
  <$> mapOnSegLevel tv lvl
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

mapOnSegLevel :: Monad m =>
                 SegOpMapper flore tlore m -> SegLevel -> m SegLevel
mapOnSegLevel tv (SegThread num_groups group_size virt) =
  SegThread
  <$> traverse (mapOnSegOpSubExp tv) num_groups
  <*> traverse (mapOnSegOpSubExp tv) group_size
  <*> pure virt
mapOnSegLevel tv (SegGroup num_groups group_size virt) =
  SegGroup
  <$> traverse (mapOnSegOpSubExp tv) num_groups
  <*> traverse (mapOnSegOpSubExp tv) group_size
  <*> pure virt
mapOnSegLevel tv (SegThreadScalar num_groups group_size virt) =
  SegThreadScalar
  <$> traverse (mapOnSegOpSubExp tv) num_groups
  <*> traverse (mapOnSegOpSubExp tv) group_size
  <*> pure virt

mapOnSegOpType :: Monad m =>
                  SegOpMapper flore tlore m -> Type -> m Type
mapOnSegOpType _tv (Prim pt) = pure $ Prim pt
mapOnSegOpType tv (Array pt shape u) = Array pt <$> f shape <*> pure u
  where f (Shape dims) = Shape <$> mapM (mapOnSegOpSubExp tv) dims
mapOnSegOpType _tv (Mem s) = pure $ Mem s

-- | Like 'Walker', but just for 'SegOp's.
data SegOpWalker lore m = SegOpWalker {
    walkOnSegOpSubExp :: SubExp -> m ()
  , walkOnSegOpLambda :: Lambda lore -> m ()
  , walkOnSegOpBody :: KernelBody lore -> m ()
  , walkOnSegOpVName :: VName -> m ()
  }

-- | A no-op traversal.
identitySegOpWalker :: Monad m => SegOpWalker lore m
identitySegOpWalker = SegOpWalker {
    walkOnSegOpSubExp = const $ return ()
  , walkOnSegOpLambda = const $ return ()
  , walkOnSegOpBody = const $ return ()
  , walkOnSegOpVName = const $ return ()
  }

walkSegOpMapper :: forall lore m. Monad m =>
                   SegOpWalker lore m -> SegOpMapper lore lore m
walkSegOpMapper f = SegOpMapper {
    mapOnSegOpSubExp = wrap walkOnSegOpSubExp
  , mapOnSegOpLambda = wrap walkOnSegOpLambda
  , mapOnSegOpBody = wrap walkOnSegOpBody
  , mapOnSegOpVName = wrap walkOnSegOpVName
  }
  where wrap :: (SegOpWalker lore m -> a -> m ()) -> a -> m a
        wrap op k = op f k >> return k

-- | As 'mapSegOpM', but ignoring the results.
walkSegOpM :: Monad m => SegOpWalker lore m -> SegOp lore -> m ()
walkSegOpM f = void . mapSegOpM m
  where m = walkSegOpMapper f

instance Attributes lore => Substitute (SegOp lore) where
  substituteNames subst = runIdentity . mapSegOpM substitute
    where substitute =
            SegOpMapper { mapOnSegOpSubExp = return . substituteNames subst
                        , mapOnSegOpLambda = return . substituteNames subst
                        , mapOnSegOpBody = return . substituteNames subst
                        , mapOnSegOpVName = return . substituteNames subst
                        }

instance Attributes lore => Rename (SegOp lore) where
  rename = mapSegOpM renamer
    where renamer = SegOpMapper rename rename rename rename

instance (Attributes lore, FreeIn (LParamAttr lore)) =>
         FreeIn (SegOp lore) where
  freeIn' e = flip execState mempty $ mapSegOpM free e
    where walk f x = modify (<>f x) >> return x
          free = SegOpMapper { mapOnSegOpSubExp = walk freeIn'
                             , mapOnSegOpLambda = walk freeIn'
                             , mapOnSegOpBody = walk freeIn'
                             , mapOnSegOpVName = walk freeIn'
                             }

instance OpMetrics (Op lore) => OpMetrics (SegOp lore) where
  opMetrics (SegMap _ _ _ body) =
    inside "SegMap" $ kernelBodyMetrics body
  opMetrics (SegRed _ _ reds _ body) =
    inside "SegRed" $ do mapM_ (lambdaMetrics . segRedLambda) reds
                         kernelBodyMetrics body
  opMetrics (SegScan _ _ scan_op _ _ body) =
    inside "SegScan" $ lambdaMetrics scan_op >> kernelBodyMetrics body
  opMetrics (SegHist _ _ ops _ body) =
    inside "SegHist" $ do mapM_ (lambdaMetrics . histOp) ops
                          kernelBodyMetrics body

instance Pretty SegSpace where
  ppr (SegSpace phys dims) = parens (commasep $ do (i,d) <- dims
                                                   return $ ppr i <+> "<" <+> ppr d) <+>
                             parens (text "~" <> ppr phys)

instance PP.Pretty SegLevel where
  ppr SegThread{} = "thread"
  ppr SegThreadScalar{} = "scalar"
  ppr SegGroup{} = "group"

ppSegLevel :: SegLevel -> PP.Doc
ppSegLevel lvl =
  PP.parens $
  text "#groups=" <> ppr (segNumGroups lvl) <> PP.semi <+>
  text "groupsize=" <> ppr (segGroupSize lvl) <>
  case segVirt lvl of
    SegNoVirt -> mempty
    SegVirt -> PP.semi <+> text "virtualise"

instance PrettyLore lore => PP.Pretty (SegOp lore) where
  ppr (SegMap lvl space ts body) =
    text "segmap_" <> ppr lvl </>
    ppSegLevel lvl </>
    PP.align (ppr space) <+>
    PP.colon <+> ppTuple' ts <+> PP.nestedBlock "{" "}" (ppr body)

  ppr (SegRed lvl space reds ts body) =
    text "segred_" <> ppr lvl </>
    ppSegLevel lvl </>
    PP.parens (PP.braces (mconcat $ intersperse (PP.comma <> PP.line) $ map ppOp reds)) </>
    PP.align (ppr space) <+> PP.colon <+> ppTuple' ts <+>
    PP.nestedBlock "{" "}" (ppr body)
    where ppOp (SegRedOp comm lam nes shape) =
            PP.braces (PP.commasep $ map ppr nes) <> PP.comma </>
            ppr shape <> PP.comma </>
            comm' <> ppr lam
            where comm' = case comm of Commutative -> text "commutative "
                                       Noncommutative -> mempty

  ppr (SegScan lvl space scan_op nes ts body) =
    text "segscan_" <> ppr lvl </>
    ppSegLevel lvl </>
    PP.parens (ppr scan_op <> PP.comma </>
               PP.braces (PP.commasep $ map ppr nes)) </>
    PP.align (ppr space) <+> PP.colon <+> ppTuple' ts <+>
    PP.nestedBlock "{" "}" (ppr body)

  ppr (SegHist lvl space ops ts body) =
    text "seghist_" <> ppr lvl </>
    ppSegLevel lvl </>
    PP.parens (PP.braces (mconcat $ intersperse (PP.comma <> PP.line) $ map ppOp ops)) </>
    PP.align (ppr space) <+> PP.colon <+> ppTuple' ts <+>
    PP.nestedBlock "{" "}" (ppr body)
    where ppOp (HistOp w rf dests nes shape op) =
            ppr w <> PP.comma <+> ppr rf <> PP.comma </>
            PP.braces (PP.commasep $ map ppr dests) <> PP.comma </>
            PP.braces (PP.commasep $ map ppr nes) <> PP.comma </>
            ppr shape <> PP.comma </>
            ppr op

instance Attributes inner => RangedOp (SegOp inner) where
  opRanges op = replicate (length $ segOpType op) unknownRange

instance (Attributes lore, CanBeRanged (Op lore)) => CanBeRanged (SegOp lore) where
  type OpWithRanges (SegOp lore) = SegOp (Ranges lore)

  removeOpRanges = runIdentity . mapSegOpM remove
    where remove = SegOpMapper return (return . removeLambdaRanges)
                   (return . removeKernelBodyRanges) return
  addOpRanges = Range.runRangeM . mapSegOpM add
    where add = SegOpMapper return Range.analyseLambda
                addKernelBodyRanges return

instance (Attributes lore,
          Attributes (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (SegOp lore) where
  type OpWithAliases (SegOp lore) = SegOp (Aliases lore)

  addOpAliases = runIdentity . mapSegOpM alias
    where alias = SegOpMapper return (return . Alias.analyseLambda)
                  (return . aliasAnalyseKernelBody) return

  removeOpAliases = runIdentity . mapSegOpM remove
    where remove = SegOpMapper return (return . removeLambdaAliases)
                   (return . removeKernelBodyAliases) return

instance (CanBeWise (Op lore), Attributes lore) => CanBeWise (SegOp lore) where
  type OpWithWisdom (SegOp lore) = SegOp (Wise lore)

  removeOpWisdom = runIdentity . mapSegOpM remove
    where remove = SegOpMapper return
                   (return . removeLambdaWisdom)
                   (return . removeKernelBodyWisdom)
                   return

instance Attributes lore => ST.IndexOp (SegOp lore) where
  indexOp vtable k (SegMap _ space _ kbody) is = do
    Returns se <- maybeNth k $ kernelBodyResult kbody
    let (gtids, _) = unzip $ unSegSpace space
    guard $ length gtids == length is
    let prim_table = M.fromList $ zip gtids $ zip is $ repeat mempty
        prim_table' = foldl expandPrimExpTable prim_table $ kernelBodyStms kbody
    case se of
      Var v -> M.lookup v prim_table'
      _ -> Nothing
    where expandPrimExpTable table stm
            | [v] <- patternNames $ stmPattern stm,
              Just (pe,cs) <-
                  runWriterT $ primExpFromExp (asPrimExp table) $ stmExp stm =
                M.insert v (pe, stmCerts stm <> cs) table
            | otherwise =
                table

          asPrimExp table v
            | Just (e,cs) <- M.lookup v table = tell cs >> return e
            | Just (Prim pt) <- ST.lookupType v vtable =
                return $ LeafExp v pt
            | otherwise = lift Nothing
  indexOp _ _ _ _ = Nothing

instance Attributes lore => IsOp (SegOp lore) where
  cheapOp _ = False
  safeOp _ = True

--- Host operations

-- | A simple size-level query or computation.
data SizeOp
  = SplitSpace SplitOrdering SubExp SubExp SubExp
    -- ^ @SplitSpace o w i elems_per_thread@.
    --
    -- Computes how to divide array elements to
    -- threads in a kernel.  Returns the number of
    -- elements in the chunk that the current thread
    -- should take.
    --
    -- @w@ is the length of the outer dimension in
    -- the array. @i@ is the current thread
    -- index. Each thread takes at most
    -- @elems_per_thread@ elements.
    --
    -- If the order @o@ is 'SplitContiguous', thread with index @i@
    -- should receive elements
    -- @i*elems_per_tread, i*elems_per_thread + 1,
    -- ..., i*elems_per_thread + (elems_per_thread-1)@.
    --
    -- If the order @o@ is @'SplitStrided' stride@,
    -- the thread will receive elements @i,
    -- i+stride, i+2*stride, ...,
    -- i+(elems_per_thread-1)*stride@.
  | GetSize Name SizeClass
    -- ^ Produce some runtime-configurable size.
  | GetSizeMax SizeClass
    -- ^ The maximum size of some class.
  | CmpSizeLe Name SizeClass SubExp
    -- ^ Compare size (likely a threshold) with some integer value.
  | CalcNumGroups SubExp Name SubExp
    -- ^ @CalcNumGroups w max_num_groups group_size@ calculates the
    -- number of GPU workgroups to use for an input of the given size.
    -- The @Name@ is a size name.  Note that @w@ is an i64 to avoid
    -- overflow issues.
  deriving (Eq, Ord, Show)

instance Substitute SizeOp where
  substituteNames subst (SplitSpace o w i elems_per_thread) =
    SplitSpace
    (substituteNames subst o)
    (substituteNames subst w)
    (substituteNames subst i)
    (substituteNames subst elems_per_thread)
  substituteNames substs (CmpSizeLe name sclass x) =
    CmpSizeLe name sclass (substituteNames substs x)
  substituteNames substs (CalcNumGroups w max_num_groups group_size) =
    CalcNumGroups
    (substituteNames substs w)
    max_num_groups
    (substituteNames substs group_size)
  substituteNames _ op = op

instance Rename SizeOp where
  rename (SplitSpace o w i elems_per_thread) =
    SplitSpace
    <$> rename o
    <*> rename w
    <*> rename i
    <*> rename elems_per_thread
  rename (CmpSizeLe name sclass x) =
    CmpSizeLe name sclass <$> rename x
  rename (CalcNumGroups w max_num_groups group_size) =
    CalcNumGroups <$> rename w <*> pure max_num_groups <*> rename group_size
  rename x = pure x

instance IsOp SizeOp where
  safeOp _ = True
  cheapOp _ = True

instance TypedOp SizeOp where
  opType SplitSpace{} = pure [Prim int32]
  opType (GetSize _ _) = pure [Prim int32]
  opType (GetSizeMax _) = pure [Prim int32]
  opType CmpSizeLe{} = pure [Prim Bool]
  opType CalcNumGroups{} = pure [Prim int32]

instance AliasedOp SizeOp where
  opAliases _ = [mempty]
  consumedInOp _ = mempty

instance RangedOp SizeOp where
  opRanges (SplitSpace _ _ _ elems_per_thread) =
    [(Just (ScalarBound 0),
      Just (ScalarBound (SE.subExpToScalExp elems_per_thread int32)))]
  opRanges _ = [unknownRange]

instance FreeIn SizeOp where
  freeIn' (SplitSpace o w i elems_per_thread) =
    freeIn' o <> freeIn' [w, i, elems_per_thread]
  freeIn' (CmpSizeLe _ _ x) = freeIn' x
  freeIn' (CalcNumGroups w _ group_size) = freeIn' w <> freeIn' group_size
  freeIn' _ = mempty

instance PP.Pretty SizeOp where
  ppr (SplitSpace o w i elems_per_thread) =
    text "splitSpace" <> suff <>
    parens (commasep [ppr w, ppr i, ppr elems_per_thread])
    where suff = case o of SplitContiguous     -> mempty
                           SplitStrided stride -> text "Strided" <> parens (ppr stride)

  ppr (GetSize name size_class) =
    text "get_size" <> parens (commasep [ppr name, ppr size_class])

  ppr (GetSizeMax size_class) =
    text "get_size_max" <> parens (commasep [ppr size_class])

  ppr (CmpSizeLe name size_class x) =
    text "get_size" <> parens (commasep [ppr name, ppr size_class]) <+>
    text "<=" <+> ppr x

  ppr (CalcNumGroups w max_num_groups group_size) =
    text "calc_num_groups" <> parens (commasep [ppr w, ppr max_num_groups, ppr group_size])

instance OpMetrics SizeOp where
  opMetrics SplitSpace{} = seen "SplitSpace"
  opMetrics GetSize{} = seen "GetSize"
  opMetrics GetSizeMax{} = seen "GetSizeMax"
  opMetrics CmpSizeLe{} = seen "CmpSizeLe"
  opMetrics CalcNumGroups{} = seen "CalcNumGroups"

typeCheckSizeOp :: TC.Checkable lore => SizeOp -> TC.TypeM lore ()
typeCheckSizeOp (SplitSpace o w i elems_per_thread) = do
  case o of
    SplitContiguous     -> return ()
    SplitStrided stride -> TC.require [Prim int32] stride
  mapM_ (TC.require [Prim int32]) [w, i, elems_per_thread]
typeCheckSizeOp GetSize{} = return ()
typeCheckSizeOp GetSizeMax{} = return ()
typeCheckSizeOp (CmpSizeLe _ _ x) = TC.require [Prim int32] x
typeCheckSizeOp (CalcNumGroups w _ group_size) = do TC.require [Prim int64] w
                                                    TC.require [Prim int32] group_size

-- | A host-level operation; parameterised by what else it can do.
data HostOp lore op
  = SegOp (SegOp lore)
    -- ^ A segmented operation.
  | SizeOp SizeOp
  | OtherOp op
  deriving (Eq, Ord, Show)

instance (Attributes lore, Substitute op) => Substitute (HostOp lore op) where
  substituteNames substs (SegOp op) =
    SegOp $ substituteNames substs op
  substituteNames substs (OtherOp op) =
    OtherOp $ substituteNames substs op
  substituteNames substs (SizeOp op) =
    SizeOp $ substituteNames substs op

instance (Attributes lore, Rename op) => Rename (HostOp lore op) where
  rename (SegOp op) = SegOp <$> rename op
  rename (OtherOp op) = OtherOp <$> rename op
  rename (SizeOp op) = SizeOp <$> rename op

instance (Attributes lore, IsOp op) => IsOp (HostOp lore op) where
  safeOp (SegOp op) = safeOp op
  safeOp (OtherOp op) = safeOp op
  safeOp (SizeOp op) = safeOp op

  cheapOp (SegOp op) = cheapOp op
  cheapOp (OtherOp op) = cheapOp op
  cheapOp (SizeOp op) = cheapOp op

instance TypedOp op => TypedOp (HostOp lore op) where
  opType (SegOp op) = opType op
  opType (OtherOp op) = opType op
  opType (SizeOp op) = opType op

instance (Aliased lore, AliasedOp op, Attributes lore) => AliasedOp (HostOp lore op) where
  opAliases (SegOp op) = opAliases op
  opAliases (OtherOp op) = opAliases op
  opAliases (SizeOp op) = opAliases op

  consumedInOp (SegOp op) = consumedInOp op
  consumedInOp (OtherOp op) = consumedInOp op
  consumedInOp (SizeOp op) = consumedInOp op

instance (Attributes lore, RangedOp op) => RangedOp (HostOp lore op) where
  opRanges (SegOp op) = opRanges op
  opRanges (OtherOp op) = opRanges op
  opRanges (SizeOp op) = opRanges op

instance (Attributes lore, FreeIn op) => FreeIn (HostOp lore op) where
  freeIn' (SegOp op) = freeIn' op
  freeIn' (OtherOp op) = freeIn' op
  freeIn' (SizeOp op) = freeIn' op

instance (CanBeAliased (Op lore), CanBeAliased op, Attributes lore) => CanBeAliased (HostOp lore op) where
  type OpWithAliases (HostOp lore op) = HostOp (Aliases lore) (OpWithAliases op)

  addOpAliases (SegOp op) = SegOp $ addOpAliases op
  addOpAliases (OtherOp op) = OtherOp $ addOpAliases op
  addOpAliases (SizeOp op) = SizeOp op

  removeOpAliases (SegOp op) = SegOp $ removeOpAliases op
  removeOpAliases (OtherOp op) = OtherOp $ removeOpAliases op
  removeOpAliases (SizeOp op) = SizeOp op

instance (CanBeRanged (Op lore), CanBeRanged op, Attributes lore) => CanBeRanged (HostOp lore op) where
  type OpWithRanges (HostOp lore op) = HostOp (Ranges lore) (OpWithRanges op)

  addOpRanges (SegOp op) = SegOp $ addOpRanges op
  addOpRanges (OtherOp op) = OtherOp $ addOpRanges op
  addOpRanges (SizeOp op) = SizeOp op

  removeOpRanges (SegOp op) = SegOp $ removeOpRanges op
  removeOpRanges (OtherOp op) = OtherOp $ removeOpRanges op
  removeOpRanges (SizeOp op) = SizeOp op

instance (CanBeWise (Op lore), CanBeWise op, Attributes lore) => CanBeWise (HostOp lore op) where
  type OpWithWisdom (HostOp lore op) = HostOp (Wise lore) (OpWithWisdom op)

  removeOpWisdom (SegOp op) = SegOp $ removeOpWisdom op
  removeOpWisdom (OtherOp op) = OtherOp $ removeOpWisdom op
  removeOpWisdom (SizeOp op) = SizeOp op

instance (Attributes lore, ST.IndexOp op) => ST.IndexOp (HostOp lore op) where
  indexOp vtable k (SegOp op) is = ST.indexOp vtable k op is
  indexOp vtable k (OtherOp op) is = ST.indexOp vtable k op is
  indexOp _ _ _ _ = Nothing

instance (PrettyLore lore, PP.Pretty op) => PP.Pretty (HostOp lore op) where
  ppr (SegOp op) = ppr op
  ppr (OtherOp op) = ppr op
  ppr (SizeOp op) = ppr op

instance (OpMetrics (Op lore), OpMetrics op) => OpMetrics (HostOp lore op) where
  opMetrics (SegOp op) = opMetrics op
  opMetrics (OtherOp op) = opMetrics op
  opMetrics (SizeOp op) = opMetrics op

typeCheckHostOp :: TC.Checkable lore =>
                   (SegLevel -> OpWithAliases (Op lore) -> TC.TypeM lore ())
                -> Maybe SegLevel
                -> (op -> TC.TypeM lore ())
                -> HostOp (Aliases lore) op
                -> TC.TypeM lore ()
typeCheckHostOp checker lvl _ (SegOp op) =
  TC.checkOpWith (checker $ segLevel op) $
  typeCheckSegOp lvl op
typeCheckHostOp _ _ f (OtherOp op) = f op
typeCheckHostOp _ _ _ (SizeOp op) = typeCheckSizeOp op
