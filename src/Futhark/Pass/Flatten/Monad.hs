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
    mkIrregFromReg,
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
    resultToResReps,
    isVariant,
    flattenDistStms,
    segmentDims,
    FlattenOps (..),
  )
where

import Control.Monad
import Data.Bifunctor (bimap, second)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Tools
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
  deriving (Show,Eq)

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
  = -- | This variable is represented
    -- completely straightforwardly- if it is
    -- an array, it is a regular array.
    Regular VName
  | -- | The representation of an
    -- irregular array.
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
      mkRep elem =
        Irregular $
          IrregularRep
            { irregularS = ns,
              irregularF = flags,
              irregularO = offsets,
              irregularD = elem,
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
          second ( flags :) $ segsAndElems env vs

-- Mapping from original variable names to their distributed resreps
inputReps :: DistInputs -> DistEnv -> M.Map VName (Type, ResRep)
inputReps inputs env = M.fromList $ map (second getRep) inputs
  where
    getRep di = case di of
      DistInput rt t -> (t, resVar rt env)
      DistInputFree v' t -> (t, Regular v')

readInputVar :: Segments -> DistEnv -> [SubExp] -> DistInputs -> VName -> Builder GPU VName
readInputVar _segments env is inputs v =
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
        Irregular (IrregularRep _ _flags _offsets _elems _) ->
          undefined

readInput :: Segments -> DistEnv -> [SubExp] -> DistInputs -> SubExp -> Builder GPU SubExp
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
  Builder GPU [SubExp]
readTypeDims segments env is inputs =
  mapM (readInput segments env is inputs) . arrayDims

segmentDims :: Segments -> [TPrimExp Int64 VName]
segmentDims = map pe64 . shapeDims . segmentsShape

flatSegmentIndex :: Segments -> [SubExp] -> TPrimExp Int64 VName
flatSegmentIndex segments = flattenIndex (segmentDims segments) . map pe64

readInputs :: Segments -> DistEnv -> [SubExp] -> DistInputs -> Builder GPU ()
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
        Irregular (IrregularRep _ _ v_O v_D _) -> do
          offset <- letSubExp "offset" =<< eIndex v_O [toExp $ flatSegmentIndex segments is]
          case arrayDims t of
            [num_elems] -> do
              let slice = Slice [DimSlice offset num_elems (intConst Int64 1)]
              bindInputName v $ BasicOp $ Index v_D slice
            _ -> do
              num_elems <-
                letSubExp "num_elems" =<< toExp (product $ map pe64 $ arrayDims t)
              let slice = Slice [DimSlice offset num_elems (intConst Int64 1)]
              v_flat <-
                letExp (baseName v <> "_flat") $ BasicOp $ Index v_D slice
              v_flat_t <- lookupType v_flat
              v' <-
                letExp (baseName v <> "_inp") . BasicOp $
                  Reshape v_flat (reshapeAll (arrayShape v_flat_t) (arrayShape t))
              letBindNames [v] $ BasicOp $ SubExp $ Var v'

scopeOfDistInputs :: DistInputs -> Scope GPU
scopeOfDistInputs = scopeOfLParams . map f
  where
    f (v, inp) = Param mempty v (distInputType inp)

isVariant :: DistInputs -> DistEnv -> SubExp -> Bool
isVariant inps env se = case se of
  Constant _ -> False
  Var v -> isJust $ M.lookup v $ inputReps inps env

ensureDenseIrregular :: Name -> IrregularRep -> Builder GPU IrregularRep
ensureDenseIrregular _ rep@IrregularRep {irregularK = Dense} =
  pure rep
ensureDenseIrregular desc rep@IrregularRep {} = do
  (new_F, new_O, ii1) <- doRepIota (irregularS rep)
  m <- arraySize 0 <$> lookupType ii1
  new_D <- letExp (desc <> "_dense_D") <=< segMap (MkSolo m) $ \(MkSolo i) -> do
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
liftResult :: Segments -> DistInputs -> DistEnv -> SubExpRes -> Builder GPU Result
liftResult segments inps env res = map (SubExpRes mempty . Var) <$> vs
  where
    vs = do
      (_, rep) <- liftSubExp segments inps env (resSubExp res)
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
  Segments ->
  DistInputs ->
  DistEnv ->
  DistResult ->
  SubExpRes ->
  Builder GPU ResRep
liftDistResultRep segments inps env dist_res res
  | isRegularDistResult dist_res = do
      let (DistType _ _ t) = distResType dist_res
          expectedShape = segmentsShape segments <> arrayShape t
      Regular <$> liftSubExpRegular segments inps env expectedShape (resSubExp res)
  | otherwise =
      case resSubExp res of
        Var v -> Irregular <$> getIrregRep segments env inps v
        _ -> error "liftBranchResultRep: irregular result is not a variable"

mkIrregFromReg ::
  Segments ->
  VName ->
  Builder GPU IrregularRep
mkIrregFromReg segments arr = do
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
  arr_F <- letExp "reg_F" <=< segMap (MkSolo num_elems) $ \(MkSolo i) -> do
    flag <- letSubExp "flag" <=< toExp $ (pe64 i `rem` pe64 segment_size) .==. 0
    pure [subExpRes flag]
  arr_O <- letExp "reg_O" <=< segMap (MkSolo num_segments) $ \(MkSolo i) -> do
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

-- If the sub-expression is a constant, replicate it to match the shape of `segments`
-- If it's a variable, lookup the variable in the dist inputs and dist env,
-- and if it can't be found it is a free variable, so we replicate it to match the shape of `segments`.
liftSubExp :: Segments -> DistInputs -> DistEnv -> SubExp -> Builder GPU (Type, ResRep)
liftSubExp segments inps env se = case se of
  c@(Constant prim) ->
    let t = Prim $ primValueType prim
     in ((t,) . Regular <$> letExp "lifted_const" (BasicOp $ Replicate (segmentsShape segments) c))
  Var v -> case M.lookup v $ inputReps inps env of
    Just (t, Regular v') -> do
      (t,)
        <$> case t of
          Prim {} -> pure $ Regular v'
          Array {} -> Irregular <$> mkIrregFromReg segments v'
          Acc {} -> pure $ Regular v'
          Mem {} -> error "getRepSubExp: Mem"
    Just (t, Irregular irreg) -> do
      irreg' <- ensureDenseIrregular "lifted_irreg" irreg
      pure (t, Irregular irreg')
    Nothing -> do
      t <- lookupType v
      v' <- letExp "free_replicated" $ BasicOp $ Replicate (segmentsShape segments) (Var v)
      (t,)
        <$> case t of
          Prim {} -> pure $ Regular v'
          Array {} -> Irregular <$> mkIrregFromReg segments v'
          Acc {} -> pure $ Regular v'
          Mem {} -> error "getRepSubExp: Mem"

liftSubExpPreserveRep :: Segments -> DistInputs -> DistEnv -> SubExp -> Builder GPU (Type, ResRep)
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
  Segments ->
  DistInputs ->
  DistEnv ->
  Shape ->
  SubExp ->
  Builder GPU VName
liftSubExpRegular segments inps env expectedShape se = do
  v <- case se of
    c@(Constant _) ->
      letExp "lifted_const" (BasicOp $ Replicate (segmentsShape segments) c)
    Var x -> case M.lookup x $ inputReps inps env of
      Just (_, Regular v') -> pure v'
      Just (_, Irregular irreg) -> do 
        rep_dense <- ensureDenseIrregular "lifted_irreg" irreg
        pure $ irregularD rep_dense
      Nothing ->
        letExp "free_replicated" $ BasicOp $ Replicate (segmentsShape segments) (Var x)
  v_t <- lookupType v
  if arrayShape v_t == expectedShape
    then pure v
    else
      letExp "reg_lifted" . BasicOp $
        Reshape v (reshapeAll (arrayShape v_t) expectedShape)

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
dataArr :: Segments -> DistEnv -> DistInputs -> SubExp -> Builder GPU VName
dataArr segments env inps (Var v)
  | Just v_inp <- lookup v inps =
      case v_inp of
        DistInputFree vs _ -> irregularD <$> mkIrregFromReg segments vs
        DistInput rt _ -> case resVar rt env of
          Irregular r ->  do
            rep_dense <- ensureDenseIrregular "dataArr" r
            pure $ irregularD rep_dense
          Regular vs -> irregularD <$> mkIrregFromReg segments vs
dataArr segments _ _ se = do
  rep <- letExp "rep" $ BasicOp $ Replicate (segmentsShape segments) se
  rep_t <- lookupType rep
  let dims = arrayDims rep_t
  if length dims == 1
    then pure rep
    else do
      n <- toSubExp "n" $ product $ map pe64 dims
      letExp "reshape" $ BasicOp $ Reshape rep $ reshapeAll (arrayShape rep_t) (Shape [n])

-- | Get the irregular representation of a var.
getIrregRep :: Segments -> DistEnv -> DistInputs -> VName -> Builder GPU IrregularRep
getIrregRep segments env inps v =
  case lookup v inps of
    Just v_inp -> case v_inp of
      DistInputFree arr _ -> mkIrregFromReg segments arr
      DistInput rt _ -> case resVar rt env of
        Irregular r -> pure r
        Regular arr -> mkIrregFromReg segments arr
    Nothing -> do
      v' <-
        letExp (baseName v <> "_rep") . BasicOp $
          Replicate (segmentsShape segments) (Var v)
      mkIrregFromReg segments v'

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
  VName ->
  VName ->
  (VName, IrregularRep) ->
  Builder GPU VName
scatterIrregular offsets space (is, irregRep) = do
  dense_irreg <- ensureDenseIrregular "scatter_irreg" irregRep
  let IrregularRep {irregularS = segs, irregularD = elems, irregularK = kind} = dense_irreg
  (_, _, ii1) <- doRepIota segs
  (_, _, ii2) <- doSegIota segs
  ~(Array _ (Shape [size]) _) <- lookupType elems
  letExp "irregular_scatter" <=< genScatter space size $ \gtid -> do
    x <- letSubExp "x" =<< eIndex elems [eSubExp gtid]
    offset <- letExp "offset" =<< eIndex offsets [eIndex is [eIndex ii1 [eSubExp gtid]]]
    i <- letExp "i" =<< eBinOp (Add Int64 OverflowUndef) (toExp offset) (eIndex ii2 [eSubExp gtid])
    pure (i, x)

-- | Write back the regular results to a (partially) blank space
scatterRegular ::
  VName ->
  (VName, VName) ->
  Builder GPU VName
scatterRegular space (is, xs) = do
  size <- arraySize 0 <$> lookupType xs
  letExp "regular_scatter" <=< genScatter space size $ \gtid -> do
    x <- letSubExp "x" =<< eIndex xs [eSubExp gtid]
    i <- letExp "i" =<< eIndex is [eSubExp gtid]
    pure (i, x)

-- | Functions for tying together disparate modules - this is to avoid mutually
-- recursive modules.
newtype FlattenOps = FlattenOps
  { flattenDistStm :: Segments -> DistEnv -> DistStm -> Builder GPU DistEnv
  }

flattenDistStms :: FlattenOps -> SubExp -> DistInputs -> DistEnv -> [DistStm] -> Result -> Builder GPU Result
flattenDistStms ops w inputs env dstms result = do
  let segments = NE.singleton w
  env' <- foldM (flattenDistStm ops segments) env dstms
  result' <- mapM (liftResult segments inputs env') result
  pure $ concat result'
