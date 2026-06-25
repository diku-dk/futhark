{-# LANGUAGE TypeFamilies #-}

-- | General definitions for the flattening transformation.
--
-- Defines not just the core monads that are involved, but also the various
-- representations, except perhaps the ones that are completely local to another
-- module.
module Futhark.Pass.Flatten.Monad
  ( IrregularKind (..),
    IrregularRep (..),
    ResRep (..),
    DistEnv (..),

    -- * Flattening monad
    FlattenM,
    FlattenState (..),
    runFlattenM,

    -- * Reading inputs
    readInputVar,
    readInputs,
    readInput,
    readTypeDims,

    -- * Insertions
    insertRep,
    insertReps,
    insertIrregulars,
    insertIrregular,
    insertRegulars,

    -- * Building blocks
    ensureDenseIrregular,
    liftResult,
    liftDistResultRep,
    liftSubExp,
    liftSubExpPreserveRep,
    liftSubExpRegular,
    liftVarRegular,
    liftParam,
    mkIrregFromReg,
    flattenIrregularRep,
    distCerts,
    dataArr,
    getIrregRep,
    scatterIrregular,
    scatterRegular,

    -- * Various
    segsAndElems,
    inputReps,
    resVar,
    scopeOfDistInputs,
    lookupInputType,
    subExpInputType,
    localiseInputs,
    replicateForDims,
    liftBodyWithDistResults,
    distResultsToResReps,
    resultToResReps,
    isVariant,
    flattenDistStms,
    segmentDims,
    FlattenOps (..),
    flattenDistStm,
  )
where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor (bimap, second)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS (SOACS)
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util.IntegralExp
import Prelude hiding (div, rem)

-- Note [Representation of Flat Arrays]
--
-- This flattening implementation uses largely the nomenclature and
-- structure described by Cosmin Oancea. In particular, consider an
-- irregular array 'A' where
--
--   - A has 'n' segments (outermost dimension).
--
--   - A has element type 't'.
--
--   - A has a total of 'm' elements (where 'm' is divisible by 'n',
--     and may indeed be 'm').
--
-- Then A is represented by the following arrays:
--
--   - A_D : [m]t; the "data array".
--
--   - A_S : [n]i64; the "shape array" giving the number of scalar elements of each segment.
--
--   - A_F : [m]bool; the "flag array", indicating when an element begins a
--     new segment.
--
--   - A_O : [n]i64; the offset array, indicating for each segment
--     where it starts in the data (and flag) array.
--
--   - A_II1 : [m]t; the "segment indices"; a mapping from element
--     index to index of the segment it belongs to.
--
--   - A_II2 : [m]t; the "inner indices"; a mapping from element index
--     to index within its corresponding segment.
--
-- The arrays that are not the data array are collectively called the
-- "structure arrays". All of the structure arrays can be computed
-- from each other, but conceptually they all coexist.
--
-- Note that we only consider the *outer* dimension to be the
-- "segments". Also, 't' may actually be an array itself (although in
-- this case, the shape of 't' must be invariant to all parallel
-- dimensions). The inner structure is preserved through code, not
-- data. (Or in practice, ad-hoc auxiliary arrays produced by code.)
-- In Cosmin's notation, we maintain only the information for the
-- outermost dimension.
--
-- As an example, consider an irregular array
--
--   A = [ [], [ [1,2,3], [4], [], [5,6] ], [ [7], [], [8,9,10] ] ]
--
-- then
--
--   n = 3
--
--   m = 10
--
--   A_D = [1,2,3,4,5,6,7,8,9,10]
--
--   A_S = [0, 6, 4]
--
--   A_F = [T,F,F,F,F,F,T,F,F,F]
--
--   A_O = [0, 0, 6]
--
--   A_II1 = [1,1,1,1,1,1,2,2,2,2]
--
--   A_II2 = [0,0,0,1,3,3,0,2,2,2]

data IrregularKind
  = Dense
  | Replicated
  deriving (Show, Eq)

data IrregularRep = IrregularRep
  { -- | Array of size of each segment, type @[]i64@.
    irregularS :: VName,
    irregularF :: VName,
    irregularO :: VName,
    irregularD :: VName,
    irregularK :: IrregularKind
  }
  deriving (Show)

data ResRep
  = -- | This variable is represented completely straightforwardly- if it is an
    -- array, it is a regular array.
    Regular VName
  | -- | The representation of an irregular array.
    Irregular IrregularRep
  deriving (Show)

newtype DistEnv = DistEnv {distResMap :: M.Map ResTag ResRep}

insertRep :: ResTag -> ResRep -> DistEnv -> DistEnv
insertRep rt rep env = env {distResMap = M.insert rt rep $ distResMap env}

insertReps :: [(ResTag, ResRep)] -> DistEnv -> DistEnv
insertReps = flip $ foldl (flip $ uncurry insertRep)

insertIrregular :: VName -> VName -> VName -> ResTag -> VName -> IrregularKind -> DistEnv -> DistEnv
insertIrregular ns flags offsets rt elems kind env =
  let rep = Irregular $ IrregularRep ns flags offsets elems kind
   in insertRep rt rep env

insertIrregulars :: VName -> VName -> VName -> [(ResTag, VName)] -> IrregularKind -> DistEnv -> DistEnv
insertIrregulars ns flags offsets bnds kind env =
  let (tags, elems) = unzip bnds
      mkRep elem_arr =
        Irregular $
          IrregularRep
            { irregularS = ns,
              irregularF = flags,
              irregularO = offsets,
              irregularD = elem_arr,
              irregularK = kind
            }
   in insertReps (zip tags $ map mkRep elems) env

insertRegulars :: [ResTag] -> [VName] -> DistEnv -> DistEnv
insertRegulars rts xs =
  insertReps (zip rts $ map Regular xs)

instance Monoid DistEnv where
  mempty = DistEnv mempty

instance Semigroup DistEnv where
  DistEnv x <> DistEnv y = DistEnv (x <> y)

resVar :: ResTag -> DistEnv -> ResRep
resVar rt env = fromMaybe bad $ M.lookup rt $ distResMap env
  where
    bad = error $ "resVar: unknown tag: " ++ show rt

segsAndElems :: DistEnv -> [DistInput] -> (Maybe (VName, VName, VName), [VName])
segsAndElems _ [] = (Nothing, [])
segsAndElems env (DistInputFree v _ : vs) =
  second (v :) $ segsAndElems env vs
segsAndElems env (DistInput rt _ : vs) =
  case resVar rt env of
    Regular v' ->
      second (v' :) $ segsAndElems env vs
    Irregular (IrregularRep segments flags offsets elems k) -> do
      case k of
        Dense -> do
          bimap (mplus $ Just (segments, flags, offsets)) (elems :) $ segsAndElems env vs
        Replicated ->
          second (flags :) $ segsAndElems env vs

-- Mapping from original variable names to their distributed resreps
inputReps :: DistInputs -> DistEnv -> M.Map VName (Type, ResRep)
inputReps inputs env = M.fromList $ map (second getRep) inputs
  where
    getRep di = case di of
      DistInput rt t -> (t, resVar rt env)
      DistInputFree v' t -> (t, Regular v')

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

isVariant :: DistInputs -> DistEnv -> SubExp -> Bool
isVariant inps env se = case se of
  Constant _ -> False
  Var v -> isJust $ M.lookup v $ inputReps inps env

ensureDenseIrregular :: SegLevel -> Name -> IrregularRep -> FlattenM IrregularRep
ensureDenseIrregular _ _ rep@IrregularRep {irregularK = Dense} =
  pure rep
ensureDenseIrregular lvl desc rep@IrregularRep {} = subBuilder $ do
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
          Irregular <$> ensureDenseIrregular lvl "liftDistResultRep_dense" rep
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
      _ -> undefined

liftBodyWithDistResults :: FlattenOps -> Segments -> DistInputs -> DistEnv -> [DistStm] -> [DistResult] -> Result -> FlattenM Result
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

-- | Flatten the arrays of an IrregularRep to be entirely one-dimensional.
flattenIrregularRep :: SegLevel -> IrregularRep -> FlattenM IrregularRep
flattenIrregularRep lvl ir@(IrregularRep shape flags offsets elems kind) = do
  elems_t <- lookupType elems
  if arrayRank elems_t == 1
    then pure ir
    else do
      n <- arraySize 0 <$> lookupType shape
      m' <- letSubExp "flat_m" <=< toExp $ product $ map pe64 $ arrayDims elems_t
      elems' <-
        letExp (baseName elems <> "_flat") . BasicOp $
          Reshape elems (reshapeAll (arrayShape elems_t) (Shape [m']))

      shape' <- letExp (baseName shape <> "_flat") <=< renameExp <=< segMap lvl (MkSolo n) $
        \(MkSolo i) -> do
          old_shape <- letSubExp "old_shape" =<< eIndex shape [toExp i]
          segment_shape <-
            letSubExp "segment_shape" <=< toExp $
              pe64 old_shape * product (map pe64 $ tail $ arrayDims elems_t)
          pure [subExpRes segment_shape]

      offsets' <- letExp (baseName offsets <> "_flat") <=< renameExp <=< segMap lvl (MkSolo n) $
        \(MkSolo i) -> do
          old_offsets <- letSubExp "old_offsets" =<< eIndex offsets [toExp i]
          segment_offsets <-
            letSubExp "segment_offsets" <=< toExp $
              pe64 old_offsets * product (map pe64 $ tail $ arrayDims elems_t)
          pure [subExpRes segment_offsets]

      flags' <- letExp (baseName flags <> "_flat") <=< renameExp <=< segMap lvl (MkSolo m') $
        \(MkSolo i) -> do
          let head_i = head $ unflattenIndex (map pe64 $ arrayDims elems_t) (pe64 i)
          flag <- letSubExp "flag" =<< eIndex flags [toExp head_i]
          pure [subExpRes flag]
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
        newParam (desc <> "_segments") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      flags <-
        newParam (desc <> "_F") $
          arrayOf (Prim Bool) (Shape [Var (paramName num_data)]) Nonunique
      offsets <-
        newParam (desc <> "_O") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      elems <-
        newParam (desc <> "_data") $
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

-- | Only sensible for variables of segment-invariant type.
dataArr :: SegLevel -> Segments -> DistEnv -> DistInputs -> SubExp -> FlattenM VName
dataArr lvl segments env inps (Var v)
  | Just v_inp <- lookup v inps =
      case v_inp of
        DistInputFree vs _ -> irregularD <$> mkIrregFromReg lvl segments vs
        DistInput rt _ -> case resVar rt env of
          Irregular r -> do
            rep_dense <- ensureDenseIrregular lvl "dataArr" r
            pure $ irregularD rep_dense
          Regular vs -> irregularD <$> mkIrregFromReg lvl segments vs
dataArr _ segments _ _ se = do
  rep <- letExp "rep" $ BasicOp $ Replicate (segmentsShape segments) se
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
  size <- arraySize 0 <$> lookupType xs
  letExp "regular_scatter" <=< genScatter lvl space size $ \gtid -> do
    x <- letSubExp "x" =<< eIndex xs [eSubExp gtid]
    i <- letExp "i" =<< eIndex is [eSubExp gtid]
    pure (i, x)

-- helper to not mess up the tags when generating new ones for the loop parameters
-- probably won't be used in future
localiseInputs :: DistEnv -> DistInputs -> (DistInputs, DistEnv, Int)
localiseInputs env_outer inps =
  let step (i, env_acc) (v, inp) =
        case inp of
          DistInputFree arr t ->
            ((i, env_acc), (v, DistInputFree arr t))
          DistInput oldrt t ->
            let newrt = ResTag i
                rep = resVar oldrt env_outer
                env_acc' = insertRep newrt rep env_acc
             in ((i + 1, env_acc'), (v, DistInput newrt t))

      ((next, env_local), inps_local) =
        L.mapAccumL step (0, mempty) inps
   in (inps_local, env_local, next)

-- | Replicate an array to insert new inner dimensions  after the
-- existing segment dimensions.
replicateForDims :: Segments -> Shape -> VName -> FlattenM VName
replicateForDims segments dims v = do
  v_t <- lookupType v
  let seg_rank = length (NE.toList segments)
      v_rank = arrayRank v_t
      dims_rank = shapeRank dims
      perm = [dims_rank .. dims_rank + seg_rank - 1] ++ [0 .. dims_rank - 1] ++ [seg_rank + dims_rank .. dims_rank + v_rank - 1]
  v_rep <-
    letExp (baseName v <> "_reg_rep") . BasicOp $ Replicate dims (Var v)
  letExp (baseName v <> "_reg_rep_tr") . BasicOp $ Rearrange v_rep perm

-- | Functions for tying together disparate modules - this is to avoid mutually
-- recursive modules.
data FlattenOps = FlattenOps
  { flattenSegLevel :: SegLevel,
    flattenFunHasParallelism :: FunHasParallelism,
    flattenDistStmAtLevel :: SegLevel -> Segments -> DistEnv -> DistStm -> FlattenM DistEnv,
    flattenScalarStm :: Segments -> DistEnv -> DistInputs -> [DistResult] -> Stm SOACS -> FlattenM DistEnv
  }

flattenDistStm :: FlattenOps -> Segments -> DistEnv -> DistStm -> FlattenM DistEnv
flattenDistStm ops = flattenDistStmAtLevel ops (flattenSegLevel ops)

flattenDistStms ::
  FlattenOps ->
  SubExp ->
  DistInputs ->
  DistEnv ->
  [DistStm] ->
  Result ->
  FlattenM Result
flattenDistStms ops w inputs env dstms result = do
  let segments = NE.singleton w
  env' <- foldM (flattenDistStm ops segments) env dstms
  result' <- mapM (liftResult (flattenSegLevel ops) segments inputs env') result
  pure $ concat result'

data FlattenState = FlattenState
  { -- In order to generate more stable threshold names, we keep track of
    -- the numbers used for thresholds separately from the ordinary name
    -- source,
    stateThresholdCounter :: Int,
    stateNameSource :: VNameSource
  }

newtype FlattenM a = FlattenM (BuilderT GPU (State FlattenState) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      LocalScope GPU,
      HasScope GPU,
      MonadState FlattenState,
      MonadFreshNames
    )

instance MonadBuilder FlattenM where
  type Rep FlattenM = GPU
  mkExpDecM pat e = FlattenM $ mkExpDecM pat e
  mkBodyM stms res = FlattenM $ mkBodyM stms res
  mkLetNamesM pat e = FlattenM $ mkLetNamesM pat e

  addStms = FlattenM . addStms
  collectStms (FlattenM m) = FlattenM $ collectStms m

instance MonadFreshNames (State FlattenState) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s {stateNameSource = src}

-- | Do not nest these - the counter for thresholds will be wrong.
runFlattenM :: (MonadFreshNames m) => Scope GPU -> FlattenM a -> m a
runFlattenM scope (FlattenM m) = modifyNameSource $ \src ->
  second stateNameSource $
    runState (fst <$> runBuilderT m scope) (FlattenState 0 src)
