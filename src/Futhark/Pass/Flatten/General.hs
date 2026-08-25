-- | Various general utilities used in flattening.
module Futhark.Pass.Flatten.General
  ( -- * Reading inputs
    readInputVar,
    readInputs,
    readInput,
    readNeutral,
    readTypeDims,

    -- * Building blocks
    ensureDenseIrregular,
    liftResult,
    liftDistResultRep,
    liftSubExp,
    liftSubExpPreserveRep,
    liftSubExpRegular,
    liftVarRegular,
    isRegularInputArr,
    liftParam,
    liftRegularParam,
    liftRegResult,
    needsIrregularRetType,
    mkIrregFromReg,
    flattenIrregularRep,
    distCerts,
    dataArr,
    getIrregRep,
    scatterIrregular,
    scatterRegular,
    module Futhark.Pass.Flatten.Monad,
    module Futhark.Pass.Flatten.Builtins,

    -- * Various
    scopeOfDistInputs,
    lookupInputType,
    subExpInputType,
    localiseInputs,
    replicateForDims,
    liftBodyWithDistResults,
    distResultsToResReps,
    resultToResReps,
    resultToResRepsByDistResult,
    irregularRepToFlatArrs,
    distributeAndFlattenBody,
    splitInput,
    isVariant,
    segmentDims,
    flattenDistStm,
    flattenScalarStm,
    distributeBodyWith,
    distributeMapWith,
    atSegLevel,
  )
where

import Control.Monad
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS as SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Monad
import Futhark.Tools
import Futhark.Transform.Rename (renameExp)
import Futhark.Util (mapAccumLM)
import Futhark.Util.IntegralExp
import Prelude hiding (div, rem)

-- | Write back the irregular results of a branch to a (partially) blank space
-- The `offsets` variable is the offsets of the final result, whereas `irregRep`
-- is the irregular representation of the result.
scatterIrregular ::
  SegLevel ->
  VName ->
  VName ->
  (VName, IrregularRep) ->
  FlattenM VName
scatterIrregular lvl offsets space (is, irregRep) = do
  let IrregularRep {irregularS = segs, irregularD = elems, irregularO = off, irregularK = kind} = irregRep
  (_, _, ii1) <- doRepIota lvl segs
  (_, _, ii2) <- doSegIota lvl segs
  m <- arraySize 0 <$> lookupType ii1
  letExp "irregular_scatter" <=< genScatter lvl space m $ \gtid -> do
    segment <- letSubExp "segment" =<< eIndex ii1 [eSubExp gtid]
    intra_segment <- letSubExp "segment" =<< eIndex ii2 [eSubExp gtid]
    x <- case kind of
      Dense -> letSubExp "x" =<< eIndex elems [eSubExp gtid]
      Replicated -> do
        o <- letSubExp "rep_O" =<< eIndex off [eSubExp segment]
        letSubExp "x" =<< eIndex elems [toExp $ pe64 o + pe64 intra_segment]
    offset <- letExp "offset" =<< eIndex offsets [eIndex is [eSubExp segment]]
    i <- letExp "i" =<< eBinOp (Add Int64 OverflowUndef) (toExp offset) (eSubExp intra_segment)
    pure (i, x)

-- | Write back the regular results to a (partially) blank space
scatterRegular ::
  SegLevel ->
  VName ->
  (VName, VName) ->
  FlattenM VName
scatterRegular lvl space (is, xs) = do
  dims <- arrayDims <$> lookupType xs
  letExp "regular_scatter" <=< genScatterND lvl space dims $ \(gtid : rest) -> do
    x <- letSubExp "x" =<< eIndex xs (map eSubExp (gtid : rest))
    i <- letSubExp "i" =<< eIndex is [eSubExp gtid]
    pure (i : rest, x)

ensureDenseIrregular :: SegLevel -> Name -> IrregularRep -> FlattenM IrregularRep
ensureDenseIrregular _ _ rep@IrregularRep {irregularK = Dense} =
  pure rep
ensureDenseIrregular lvl desc rep@IrregularRep {} = do
  (new_F, new_O, ii1) <- doRepIota lvl (irregularS rep)
  m <- arraySize 0 <$> lookupType ii1
  new_D <- letExp (desc <> "_dense_D") <=< segMap lvl (MkSolo m) $ \(MkSolo i) -> do
    seg <- letSubExp "seg" =<< eIndex ii1 [eSubExp i]
    old_off <- letSubExp "old_off" =<< eIndex (irregularO rep) [eSubExp seg]
    new_off <- letSubExp "new_off" =<< eIndex new_O [eSubExp seg]
    j <- letSubExp "j" <=< toExp $ pe64 i - pe64 new_off
    x <- letSubExp "x" =<< eIndex (irregularD rep) [toExp $ pe64 old_off + pe64 j]
    pure [subExpRes x]
  pure $
    IrregularRep
      { irregularS = irregularS rep,
        irregularF = new_F,
        irregularO = new_O,
        irregularD = new_D,
        irregularK = Dense
      }

-- Lift a result of a function.
liftResult :: SegLevel -> Segments -> DistInputs -> DistEnv -> SubExpRes -> FlattenM Result
liftResult lvl segments inps env res = map (SubExpRes mempty . Var) <$> vs
  where
    vs = do
      (_, rep) <- liftSubExp lvl segments inps env (resSubExp res)
      case rep of
        Regular v -> pure [v]
        Irregular irreg -> mkIrrep irreg
    mkIrrep
      ( IrregularRep
          { irregularS = segs,
            irregularF = flags,
            irregularO = offsets,
            irregularD = elems
          }
        ) = do
        flags_t <- lookupType flags
        t <- lookupType elems
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        pure [num_data, segs, flags', offsets, elems']

needsIrregularRetType :: DistInputs -> RetType SOACS -> Bool
needsIrregularRetType inps = any needsIrregularDim . arrayDims
  where
    needsIrregularDim Ext {} = True
    needsIrregularDim (Free se) = isVariant inps se

liftRegResult :: SegLevel -> Segments -> SubExp -> DistInputs -> DistEnv -> RetType SOACS -> SubExpRes -> FlattenM Result
liftRegResult lvl segments num_segments inps env rettype res
  | needsIrregularRetType inps rettype = case resSubExp res of
      Var v -> do
        irreg <- getIrregRep lvl segments env inps v
        varsRes <$> irregularRepToFlatArrs num_segments irreg
      Constant {} ->
        error "liftRegResult: irregular result is not a variable"
  | otherwise = do
      let res_se = resSubExp res
      res_t <- subExpInputType inps res_se
      let expectedShape = segmentsShape segments <> arrayShape res_t
      lifted_res <- liftSubExpRegular lvl segments inps env expectedShape res_se
      pure [SubExpRes mempty (Var lifted_res)]

mkIrregFromReg ::
  SegLevel ->
  Segments ->
  VName ->
  FlattenM IrregularRep
mkIrregFromReg lvl segments arr = do
  arr_t <- lookupType arr
  num_segments <-
    letSubExp "reg_num_segments" <=< toExp $ product $ segmentDims segments
  segment_size <-
    letSubExp "reg_seg_size" <=< toExp . product . map pe64 $
      drop (segmentsRank segments) (arrayDims arr_t)
  arr_S <-
    letExp "reg_segments" . BasicOp $
      Replicate (Shape [num_segments]) segment_size
  num_elems <-
    letSubExp "reg_num_elems" <=< toExp $ product $ map pe64 $ arrayDims arr_t
  arr_D <-
    letExp "reg_D" . BasicOp $
      Reshape arr (reshapeAll (arrayShape arr_t) (Shape [num_elems]))
  arr_F <- letExp "reg_F" <=< segMap lvl (MkSolo num_elems) $ \(MkSolo i) -> do
    flag <- letSubExp "flag" <=< toExp $ (pe64 i `rem` pe64 segment_size) .==. 0
    pure [subExpRes flag]
  arr_O <- letExp "reg_O" <=< segMap lvl (MkSolo num_segments) $ \(MkSolo i) -> do
    offset <- letSubExp "offset" <=< toExp $ pe64 i * pe64 segment_size
    pure [subExpRes offset]
  pure $
    IrregularRep
      { irregularS = arr_S,
        irregularF = arr_F,
        irregularO = arr_O,
        irregularD = arr_D,
        irregularK = Dense
      }

readIrregularInput ::
  Segments ->
  [SubExp] ->
  VName ->
  Type ->
  IrregularRep ->
  FlattenM VName
readIrregularInput segments is v t (IrregularRep _ _ v_O v_D _) = do
  offset <- letSubExp "offset" =<< eIndex v_O [toExp $ flatSegmentIndex segments is]
  case arrayDims t of
    [] -> do
      letExp (baseName v <> "_inp") =<< eIndex v_D [eSubExp offset]
    [num_elems] -> do
      let slice = Slice [DimSlice offset num_elems (intConst Int64 1)]
      letExp (baseName v <> "_inp") $ BasicOp $ Index v_D slice
    _ -> do
      num_elems <-
        letSubExp "num_elems" =<< toExp (product $ map pe64 $ arrayDims t)
      let slice = Slice [DimSlice offset num_elems (intConst Int64 1)]
      v_flat <-
        letExp (baseName v <> "_flat") $ BasicOp $ Index v_D slice
      v_flat_t <- lookupType v_flat
      letExp (baseName v <> "_inp") . BasicOp $
        Reshape v_flat (reshapeAll (arrayShape v_flat_t) (arrayShape t))

readInputVar :: Segments -> DistEnv -> [SubExp] -> DistInputs -> VName -> FlattenM VName
readInputVar segments env is inputs v =
  case lookup v inputs of
    Nothing -> pure v
    Just (DistInputFree arr t)
      | isAcc t -> pure arr
      | otherwise -> letExp (baseName v) =<< eIndex arr (map eSubExp is)
    Just (DistInput rt t) -> do
      case resVar rt env of
        Regular arr
          | isAcc t -> pure arr
          | otherwise -> letExp (baseName v) =<< eIndex arr (map eSubExp is)
        Irregular irreg -> readIrregularInput segments is v t irreg

readInput :: Segments -> DistEnv -> [SubExp] -> DistInputs -> SubExp -> FlattenM SubExp
readInput _ _ _ _ (Constant x) =
  pure $ Constant x
readInput segments env is inputs (Var v) =
  Var <$> readInputVar segments env is inputs v

-- | Read the neutral element of a reduction or scan operator. The neutral
-- element of an operator is unique because we assume uniform operators, so a
-- valid program must have the same value in every segment, and we can read it
-- from the first one. This is the case even if the neutral element is
-- nonuniform, which is useless but allowed. The segment space may however be
-- empty, in which case we produce a blank value instead; it is never used then,
-- but an unconditional read would be out of bounds.
readNeutral :: Segments -> DistEnv -> DistInputs -> SubExp -> FlattenM SubExp
readNeutral segments env inps ne
  | Var v <- ne,
    Just _ <- lookup v inps = do
      ne_t <- subExpInputType inps ne
      n <- letSubExp "num_segments" =<< toExp (segmentCount segments)
      letSubExp (baseName v <> "_ne")
        =<< eIf
          (toExp $ pe64 n .==. 0)
          (eBody [eBlank ne_t])
          (eBody [eSubExp =<< readInput segments env zeros inps ne])
  | otherwise =
      readInput segments env zeros inps ne
  where
    zeros = replicate (segmentsRank segments) (intConst Int64 0)

readTypeDims ::
  Segments ->
  DistEnv ->
  [SubExp] ->
  DistInputs ->
  TypeBase Shape u ->
  FlattenM [SubExp]
readTypeDims segments env is inputs =
  mapM (readInput segments env is inputs) . arrayDims

segmentDims :: Segments -> [TPrimExp Int64 VName]
segmentDims = map pe64 . shapeDims . segmentsShape

flatSegmentIndex :: Segments -> [SubExp] -> TPrimExp Int64 VName
flatSegmentIndex segments = flattenIndex (segmentDims segments) . map pe64

readInputs :: Segments -> DistEnv -> [SubExp] -> DistInputs -> FlattenM ()
readInputs segments env is = mapM_ onInput
  where
    bindInputName v e
      | v `nameIn` freeIn e = do
          v' <- letExp (baseName v <> "_inp") e
          letBindNames [v] $ BasicOp $ SubExp $ Var v'
      | otherwise =
          letBindNames [v] e
    onInput (v, DistInputFree arr t) =
      bindInputName v
        =<< if isAcc t
          then eSubExp (Var arr)
          else eIndex arr (map eSubExp is)
    onInput (v, DistInput rt t) =
      case resVar rt env of
        Regular arr ->
          bindInputName v
            =<< if isAcc t
              then eSubExp $ Var arr
              else eIndex arr (map eSubExp is)
        Irregular irreg ->
          readIrregularInput segments is v t irreg >>= eSubExp . Var >>= bindInputName v

scopeOfDistInputs :: DistInputs -> Scope GPU
scopeOfDistInputs = scopeOfLParams . map f
  where
    f (v, inp) = Param mempty v (distInputType inp)

lookupInputType :: DistInputs -> VName -> FlattenM Type
lookupInputType inps v =
  case lookup v inps of
    Just inp -> pure $ distInputType inp
    Nothing -> lookupType v

subExpInputType :: DistInputs -> SubExp -> FlattenM Type
subExpInputType _ (Constant val) =
  pure $ Prim $ primValueType val
subExpInputType inps (Var v) =
  lookupInputType inps v

isVariant :: DistInputs -> SubExp -> Bool
isVariant _ (Constant _) = False
isVariant inps (Var v) = isJust $ lookup v inps

liftDistResultRep ::
  SegLevel ->
  Segments ->
  DistInputs ->
  DistEnv ->
  DistResult ->
  SubExpRes ->
  FlattenM ResRep
liftDistResultRep lvl segments inps env dist_res res
  | isRegularDistResult dist_res = do
      let (DistType _ _ t) = distResType dist_res
          expectedShape = segmentsShape segments <> arrayShape t
      Regular <$> liftSubExpRegular lvl segments inps env expectedShape (resSubExp res)
  | otherwise =
      case resSubExp res of
        Var v -> do
          rep <- getIrregRep lvl segments env inps v
          pure $ Irregular rep
        _ -> error "liftBranchResultRep: irregular result is not a variable"

liftDistResult :: SegLevel -> Segments -> DistInputs -> DistEnv -> DistResult -> SubExpRes -> FlattenM Result
liftDistResult lvl segments inps env dist_res res =
  if isRegularDistResult dist_res
    then do
      let (DistType _ _ t) = distResType dist_res
      let expectedShape = segmentsShape segments <> arrayShape t
      v <- liftSubExpRegular lvl segments inps env expectedShape (resSubExp res)
      pure [SubExpRes mempty (Var v)]
    else case resSubExp res of
      Var v -> do
        irreg <- getIrregRep lvl segments env inps v
        pure $ map (SubExpRes mempty . Var) [irregularS irreg, irregularF irreg, irregularO irreg, irregularD irreg]
      _ -> error "liftDistResult: irregular result is not a variable"

liftBodyWithDistResults :: FlattenOps -> Segments -> DistInputs -> DistEnv -> DistStms -> [DistResult] -> Result -> FlattenM Result
liftBodyWithDistResults ops segments inputs env dstms dist_res result = do
  env' <- foldM (flattenDistStm ops segments) env dstms
  result' <- zipWithM (liftDistResult (flattenSegLevel ops) segments inputs env') dist_res result
  pure $ concat result'

distResultsToResReps :: [DistResult] -> [VName] -> [ResRep]
distResultsToResReps dist_res results =
  snd $ L.mapAccumL f results dist_res
  where
    f rs dist_res' =
      if isRegularDistResult dist_res'
        then
          let (v : rs') = rs
           in (rs', Regular v)
        else
          let (segs : flags : offsets : elems : rs') = rs
           in (rs', Irregular $ IrregularRep segs flags offsets elems Dense)

-- | Convert an irregular representation to its flat constituents (number of
-- data elements, segments, flags, offsets, elements), with the structure arrays
-- reshaped to be one-dimensional of the given width. This is the form in which
-- irregular values are passed to lifted functions and carried through loops.
irregularRepToFlatArrs :: SubExp -> IrregularRep -> FlattenM [VName]
irregularRepToFlatArrs w (IrregularRep segs flags offsets elems _) = do
  t <- lookupType elems
  t_o <- lookupType offsets
  flags_t <- lookupType flags
  num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
  let shape = Shape [Var num_data]
  flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
  elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
  segs' <- letExp "segs" $ BasicOp $ Reshape segs $ reshapeAll (arrayShape t_o) (Shape [w])
  offsets' <- letExp "offsets" $ BasicOp $ Reshape offsets $ reshapeAll (arrayShape t_o) (Shape [w])
  pure [num_data, segs', flags', offsets', elems']

-- | Distribute a body and lift the distributed statements, giving
-- back representations of the body results.
distributeAndFlattenBody ::
  FlattenOps ->
  Segments ->
  Name ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Body SOACS ->
  FlattenM [ResRep]
distributeAndFlattenBody ops segments desc env inps res body = do
  scope <- askScope
  (inps_local, env_local, _) <- localiseInputs env inps
  let (inps_dist, dstms) = distributeBodyWith ops scope segments inps_local body
  lifted_res <- liftBodyWithDistResults ops segments inps_dist env_local dstms res (bodyResult body)
  lifted_vs <- mapM (letExp desc <=< toExp . resSubExp) lifted_res
  pure $ distResultsToResReps res lifted_vs

-- | Take the elements at index @is@ from an input @v@. The representation of
-- @v@ can be overridden through the provided mapping, which takes precedence
-- over what the environment says.
splitInput ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  M.Map VName ResRep ->
  VName ->
  FlattenM (Type, VName, ResRep)
splitInput lvl segments env inps is acc_reps v = do
  (t, rep0) <- liftSubExpPreserveRep segments inps env (Var v)
  let rep = M.findWithDefault rep0 v acc_reps
  (t,v,) <$> case rep of
    Regular arr -> do
      if isAcc t
        then
          pure $ Regular arr
        else do
          -- In the regular case we just take the elements
          -- of the array given by `is`
          n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
          inner_dims <- drop (segmentsRank segments) . arrayDims <$> lookupType arr
          -- Do the segMap over all dims, so the inner dimensions
          -- are gathered in parallel
          arr' <- letExp "split_arr" <=< segMap lvl (n : inner_dims) $ \(i : js) -> do
            idx <- letSubExp "idx" =<< eIndex is [eSubExp i]
            let arr_is = unflattenIndex (segmentDims segments) (pe64 idx)
            subExpsRes . pure <$> (letSubExp "arr" =<< eIndex arr (map toExp arr_is ++ map eSubExp js))
          pure $ Regular arr'
    Irregular (IrregularRep segs flags offsets elems _) -> do
      -- In the irregular case we take the elements
      -- of the `segs` array given by `is` like in the regular case
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      segs' <- letExp "split_segs" <=< segMap lvl (MkSolo n) $ \(MkSolo i) -> do
        idx <- letExp "idx" =<< eIndex is [eSubExp i]
        subExpsRes . pure <$> (letSubExp "segs" =<< eIndex segs [toExp idx])
      -- From this we calculate the offsets and number of elements
      (_, offsets', num_data) <- exScanAndSum lvl segs'
      (_, _, ii1) <- doRepIota lvl segs'
      (_, _, ii2) <- doSegIota lvl segs'
      -- We then take the elements we need from `elems` and `flags`
      -- For each index `i`, we roughly:
      -- Get the offset of the segment we want to copy by indexing
      -- `offsets` through `is` further through `ii1` i.e.
      -- `offset = offsets[is[ii1[i]]]`
      -- We then add `ii2[i]` to `offset`
      -- and use that to index into `elems` and `flags`.
      ~[flags', elems'] <- letTupExp "split_F_data" <=< segMap lvl (MkSolo num_data) $ \(MkSolo i) -> do
        offset <- letExp "offset" =<< eIndex offsets [eIndex is [eIndex ii1 [eSubExp i]]]
        idx <- letExp "idx" =<< eBinOp (Add Int64 OverflowUndef) (toExp offset) (eIndex ii2 [eSubExp i])
        flags_split <- letSubExp "flags" =<< eIndex flags [toExp idx]
        elems_split <- letSubExp "elems" =<< eIndex elems [toExp idx]
        pure $ subExpsRes [flags_split, elems_split]
      pure $
        Irregular $
          IrregularRep
            { irregularS = segs',
              irregularF = flags',
              irregularO = offsets',
              irregularD = elems',
              irregularK = Dense
            }

-- | Flatten the arrays of an IrregularRep to be entirely one-dimensional.
flattenIrregularRep :: SegLevel -> IrregularRep -> FlattenM IrregularRep
flattenIrregularRep lvl ir@(IrregularRep shape _ offsets elems kind) = do
  elems_t <- lookupType elems
  if arrayRank elems_t == 1
    then pure ir
    else do
      n <- arraySize 0 <$> lookupType shape
      m' <- letSubExp "flat_m" <=< toExp $ product $ map pe64 $ arrayDims elems_t
      elems' <-
        letExp (baseName elems <> "_flat") . BasicOp $
          Reshape elems (reshapeAll (arrayShape elems_t) (Shape [m']))
      let inner_size = product $ map pe64 $ tail $ arrayDims elems_t
      ~[shape', offsets'] <-
        letTupExp (baseName shape <> "_flat_metadata")
          <=< renameExp
          <=< segMap lvl (MkSolo n)
          $ \(MkSolo i) -> do
            old_shape <-
              letSubExp "old_shape" =<< eIndex shape [toExp i]
            old_offset <-
              letSubExp "old_offset" =<< eIndex offsets [toExp i]

            segment_shape <-
              letSubExp "segment_shape" <=< toExp $ (pe64 old_shape * inner_size)
            segment_offset <-
              letSubExp "segment_offset" <=< toExp $ (pe64 old_offset * inner_size)
            pure $ subExpsRes [segment_shape, segment_offset]
      flags' <- genFlags lvl m' offsets'
      pure $ IrregularRep shape' flags' offsets' elems' kind

-- If the sub-expression is a constant, replicate it to match the shape of `segments`
-- If it's a variable, lookup the variable in the dist inputs and dist env,
-- and if it can't be found it is a free variable, so we replicate it to match the shape of `segments`.
liftSubExp :: SegLevel -> Segments -> DistInputs -> DistEnv -> SubExp -> FlattenM (Type, ResRep)
liftSubExp lvl segments inps env se = case se of
  c@(Constant prim) ->
    let t = Prim $ primValueType prim
     in ((t,) . Regular <$> letExp "lifted_const" (BasicOp $ Replicate (segmentsShape segments) c))
  Var v -> case M.lookup v $ inputReps inps env of
    Just (t, Regular v') -> do
      (t,)
        <$> case t of
          Prim {} -> pure $ Regular v'
          Array {} -> Irregular <$> mkIrregFromReg lvl segments v'
          Acc {} -> pure $ Regular v'
          Mem {} -> error "liftSubExp: Mem"
    Just (t, Irregular irreg) -> do
      irreg' <- ensureDenseIrregular lvl "lifted_irreg" irreg
      (t,)
        <$> case t of
          Prim {} -> pure $ Regular $ irregularD irreg'
          Array {} -> pure $ Irregular irreg'
          Acc {} -> error "liftSubExp: Irregular Acc"
          Mem {} -> error "liftSubExp: Mem"
    Nothing -> do
      t <- lookupType v
      v' <- letExp "free_replicated" $ BasicOp $ Replicate (segmentsShape segments) (Var v)
      (t,)
        <$> case t of
          Prim {} -> pure $ Regular v'
          Array {} -> Irregular <$> mkIrregFromReg lvl segments v'
          Acc {} -> pure $ Regular v'
          Mem {} -> error "getRepSubExp: Mem"

liftSubExpPreserveRep :: Segments -> DistInputs -> DistEnv -> SubExp -> FlattenM (Type, ResRep)
liftSubExpPreserveRep segments inps env se = case se of
  c@(Constant prim) ->
    let t = Prim $ primValueType prim
     in do
          v <- letExp "lifted_const" $ BasicOp $ Replicate (segmentsShape segments) c
          pure (t, Regular v)
  Var v -> case M.lookup v $ inputReps inps env of
    Just (t, rep) -> pure (t, rep)
    Nothing -> do
      t <- lookupType v
      v' <- letExp "free_replicated" $ BasicOp $ Replicate (segmentsShape segments) (Var v)
      pure (t, Regular v')

-- | Like 'liftSubExp' but always returns a Regular result with the
-- given expected shape. Reshapes the underlying data if necessary.
liftSubExpRegular ::
  SegLevel ->
  Segments ->
  DistInputs ->
  DistEnv ->
  Shape ->
  SubExp ->
  FlattenM VName
liftSubExpRegular lvl segments inps env expectedShape se = do
  case se of
    c@(Constant _) ->
      letExp "lifted_const" (BasicOp $ Replicate (segmentsShape segments) c)
    Var v -> liftVarRegular lvl segments inps env expectedShape v

liftVarRegular ::
  SegLevel ->
  Segments ->
  DistInputs ->
  DistEnv ->
  Shape ->
  VName ->
  FlattenM VName
liftVarRegular lvl segments inps env expectedShape x = do
  v <- case M.lookup x $ inputReps inps env of
    Just (_, Regular v') -> pure v'
    Just (_, Irregular irreg) -> do
      rep_dense <- ensureDenseIrregular lvl "lifted_irreg" irreg
      pure $ irregularD rep_dense
    Nothing ->
      letExp "free_replicated" $ BasicOp $ Replicate (segmentsShape segments) (Var x)
  v_t <- lookupType v
  if isAcc v_t || arrayShape v_t == expectedShape
    then pure v
    else
      letExp "reg_lifted" . BasicOp $
        Reshape v (reshapeAll (arrayShape v_t) expectedShape)

-- | Can this input array be lifted to a regular array? This holds unless it is
-- represented irregularly. The uniform alternatives lift their inputs regularly
-- (via 'liftSubExpRegular'), which is only valid when the inputs are actually
-- regular.
isRegularInputArr :: DistEnv -> DistInputs -> VName -> Bool
isRegularInputArr env inps arr =
  case lookup arr inps of
    Just (DistInput rt _) ->
      case resVar rt env of
        Regular {} -> True
        Irregular {} -> False
    _ -> True

liftParam :: (MonadFreshNames m) => SubExp -> FParam SOACS -> m ([FParam GPU], ResRep)
liftParam w fparam =
  case declTypeOf fparam of
    Prim pt -> do
      p <-
        newParam
          (desc <> "_lifted")
          (arrayOf (Prim pt) (Shape [w]) Nonunique)
      pure ([p], Regular $ paramName p)
    Array pt _ u -> do
      num_data <-
        newParam (desc <> "_num_data") $ Prim int64
      segments <-
        newParam (desc <> "_S") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      flags <-
        newParam (desc <> "_F") $
          arrayOf (Prim Bool) (Shape [Var (paramName num_data)]) Nonunique
      offsets <-
        newParam (desc <> "_O") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      elems <-
        newParam (desc <> "_D") $
          arrayOf (Prim pt) (Shape [Var (paramName num_data)]) u
      pure
        ( [num_data, segments, flags, offsets, elems],
          Irregular $
            IrregularRep
              { irregularS = paramName segments,
                irregularF = paramName flags,
                irregularO = paramName offsets,
                irregularD = paramName elems,
                irregularK = Dense
              }
        )
    Acc {} ->
      error "liftParam: Acc"
    Mem {} ->
      error "liftParam: Mem"
  where
    desc = baseName (paramName fparam)

liftRegularParam :: (MonadFreshNames m) => SubExp -> FParam SOACS -> m (FParam GPU, ResRep)
liftRegularParam w fparam =
  case declTypeOf fparam of
    Prim pt -> do
      p <-
        newParam
          (desc <> "_lifted")
          (arrayOf (Prim pt) (Shape [w]) Nonunique)
      pure (p, Regular $ paramName p)
    Array pt shape u -> do
      p <-
        newParam (desc <> "_lifted") $
          arrayOf (Prim pt) (Shape [w] <> shape) u
      pure (p, Regular $ paramName p)
    Acc {} ->
      error "liftParam: Acc"
    Mem {} ->
      error "liftParam: Mem"
  where
    desc = baseName (paramName fparam)

distCerts :: DistInputs -> StmAux a -> DistEnv -> Certs
distCerts inps aux env = Certs $ map f $ unCerts $ stmAuxCerts aux
  where
    f v = case lookup v inps of
      Nothing -> v
      Just (DistInputFree vs _) -> vs
      Just (DistInput rt _) ->
        case resVar rt env of
          Regular vs -> vs
          Irregular r -> irregularD r

flattenData :: VName -> FlattenM VName
flattenData vs = do
  t <- lookupType vs
  case arrayDims t of
    [_] -> pure vs
    dims -> do
      n <- toSubExp "num_data" $ product $ map pe64 dims
      letExp (baseName vs <> "_flat") . BasicOp $
        Reshape vs $
          reshapeAll (arrayShape t) (Shape [n])

-- | Only sensible for variables of uniform type.
dataArr :: SegLevel -> Segments -> DistEnv -> DistInputs -> SubExp -> FlattenM VName
dataArr lvl _segments env inps (Var v)
  | Just v_inp <- lookup v inps =
      case v_inp of
        DistInputFree vs _ -> flattenData vs
        DistInput rt _ -> case resVar rt env of
          Irregular r -> do
            rep_dense <- ensureDenseIrregular lvl "dataArr" r
            pure $ irregularD rep_dense
          Regular vs -> flattenData vs
dataArr _ segments _ _ se = do
  -- The result is a one-dimensional array with one element per segment. With no
  -- enclosing segments there is a single implicit segment, so we replicate over
  -- a unit dimension; replicating over the empty shape would instead yield a
  -- scalar.
  let rep_shape = case segmentsShape segments of
        Shape [] -> Shape [intConst Int64 1]
        shape -> shape
  rep <- letExp "rep" $ BasicOp $ Replicate rep_shape se
  rep_t <- lookupType rep
  let dims = arrayDims rep_t
  if length dims == 1
    then pure rep
    else do
      n <- toSubExp "n" $ product $ map pe64 dims
      letExp "reshape" $ BasicOp $ Reshape rep $ reshapeAll (arrayShape rep_t) (Shape [n])

-- | Get the irregular representation of a var.
getIrregRep :: SegLevel -> Segments -> DistEnv -> DistInputs -> VName -> FlattenM IrregularRep
getIrregRep lvl segments env inps v =
  case lookup v inps of
    Just v_inp -> case v_inp of
      DistInputFree arr _ -> mkIrregFromReg lvl segments arr
      DistInput rt _ -> case resVar rt env of
        Irregular r -> pure r
        Regular arr -> mkIrregFromReg lvl segments arr
    Nothing -> do
      v' <-
        letExp (baseName v <> "_rep") . BasicOp $
          Replicate (segmentsShape segments) (Var v)
      mkIrregFromReg lvl segments v'

-- | This function walks through the *unlifted* result types
-- and uses the *lifted* results to construct the corresponding res reps.
--
-- See the 'liftResult' function for the opposite process i.e.
-- turning 'ResRep's into results.
resultToResReps :: [TypeBase s u] -> [VName] -> [ResRep]
resultToResReps types results =
  snd $
    L.mapAccumL
      ( \rs t -> case t of
          Prim {} ->
            let (v : rs') = rs
                rep = Regular v
             in (rs', rep)
          Array {} ->
            let (_ : segs : flags : offsets : elems : rs') = rs
                rep = Irregular $ IrregularRep segs flags offsets elems Dense
             in (rs', rep)
          Acc {} -> error "resultToResReps: Illegal type 'Acc'"
          Mem {} -> error "resultToResReps: Illegal type 'Mem'"
      )
      results
      types

resultToResRepsByDistResult :: [DistResult] -> [VName] -> [ResRep]
resultToResRepsByDistResult dist_res results =
  snd $
    L.mapAccumL
      ( \rs dist_res' ->
          if isRegularDistResult dist_res'
            then
              let (v : rs') = rs
               in (rs', Regular v)
            else
              let (_ : segs : flags : offsets : elems : rs') = rs
               in (rs', Irregular $ IrregularRep segs flags offsets elems Dense)
      )
      results
      dist_res

-- helper to not mess up the tags when generating new ones for the loop parameters
-- probably won't be used in future
localiseInputs :: DistEnv -> DistInputs -> FlattenM (DistInputs, DistEnv, Int)
localiseInputs env_outer inps = do
  let step (i, env_acc) (v, inp) =
        case inp of
          DistInputFree arr t ->
            pure ((i, env_acc), (v, DistInputFree arr t))
          DistInput oldrt t -> do
            let newrt = ResTag i
                rep = resVar oldrt env_outer
            env_acc' <- insertRepM newrt rep env_acc
            pure ((i + 1, env_acc'), (v, DistInput newrt t))

  ((next, env_local), inps_local) <-
    mapAccumLM step (0, mempty) inps
  pure (inps_local, env_local, next)

-- | Replicate an array to insert new inner dimensions  after the
-- existing segment dimensions.
replicateForDims :: Segments -> Shape -> VName -> FlattenM VName
replicateForDims segments dims v = do
  v_t <- lookupType v
  let seg_rank = length segments
      v_rank = arrayRank v_t
      dims_rank = shapeRank dims
      perm = [dims_rank .. dims_rank + seg_rank - 1] ++ [0 .. dims_rank - 1] ++ [seg_rank + dims_rank .. dims_rank + v_rank - 1]
  v_rep <-
    letExp (baseName v <> "_reg_rep") . BasicOp $ Replicate dims (Var v)
  letExp (baseName v <> "_reg_rep_tr") . BasicOp $ Rearrange v_rep perm

-- | Flatten a single 'DistStm', producing an updated environment.
flattenDistStm ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistStm ->
  FlattenM DistEnv
flattenDistStm ops = flattenDistStmWith ops ops

-- | Flatten a single scalar statement, producing an updated environment.
flattenScalarStm ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Stm SOACS ->
  FlattenM DistEnv
flattenScalarStm ops = flattenScalarStmAt ops (flattenSegLevel ops)

-- | 'distributeBody' with the settings in the given 'FlattenOps'.
distributeBodyWith ::
  FlattenOps ->
  Scope rep ->
  Segments ->
  DistInputs ->
  Body SOACS ->
  (DistInputs, DistStms)
distributeBodyWith ops =
  distributeBody (flattenIrregularity ops) (flattenFunHasParallelism ops)

-- | 'distributeMap' with the settings in the given 'FlattenOps'.
distributeMapWith ::
  FlattenOps ->
  Scope rep ->
  Pat Type ->
  Segments ->
  [MapArray t] ->
  Lambda SOACS ->
  (Distributed, M.Map ResTag t)
distributeMapWith ops =
  distributeMap (flattenIrregularity ops) (flattenFunHasParallelism ops)

-- | Continue flattening at the given seg level, adjusting the irregularity
-- handling mode to match. Intrablock code cannot use the machinery for
-- flattening irregular arrays, as it produces SegOps whose sizes are bound
-- inside the enclosing kernel. 'SequentialiseIrregularAll' is requested by the
-- user rather than implied by the level, so we keep it if provided.
atSegLevel :: SegLevel -> FlattenOps -> FlattenOps
atSegLevel lvl ops =
  ops {flattenSegLevel = lvl, flattenIrregularity = irreg}
  where
    irreg = case (flattenIrregularity ops, lvl) of
      (SequentialiseIrregularAll, _) -> SequentialiseIrregularAll
      (_, SegThreadInBlock {}) -> SequentialiseIrregularBasicOps
      _ -> DistributeIrregular
