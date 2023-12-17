{-# LANGUAGE TypeFamilies #-}

-- The idea is to perform distribution on one level at a time, and
-- produce "irregular Maps" that can accept and produce irregular
-- arrays.  These irregular maps will then be transformed into flat
-- parallelism based on their contents.  This is a sensitive detail,
-- but if irregular maps contain only a single Stm, then it is fairly
-- straightforward, as we simply implement flattening rules for every
-- single kind of expression.  Of course that is also somewhat
-- inefficient, so we want to support multiple Stms for things like
-- scalar code.
module Futhark.Pass.Flatten (flattenSOACs) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Tuple.Solo
import Debug.Trace
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Pass.ExtractKernels.BlockedKernel (mkSegSpace, segScan)
import Futhark.Pass.ExtractKernels.ToGPU (scopeForGPU, soacsExpToGPU, soacsLambdaToGPU, soacsStmToGPU)
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (mapEither)
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
--   - A_S : [n]i64; the "shape array" giving the size of each segment.
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
-- Some of these structures can be computed from each other, but
-- conceptually they all coexist.
--
-- Note that we only consider the *outer* dimension to be the
-- "segments". Also, 't' may actually be an array itself (although in
-- this case, the shape must be invariant to all parallel dimensions).
-- The inner structure is preserved through code, not data. (Or in
-- practice, ad-hoc auxiliary arrays produced by code.) In Cosmin's
-- notation, we maintain only the information for the outermost
-- dimension.
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
--   A_S = [0, 4, 3]
--
--   A_F = [T,F,F,F,F,F,T,F,F,F]
--
--   A_O = [0, 0, 6]
--
--   A_II1 = [0,0,0,1,3,3,4,6,6,6]
--
--   A_II2 = [1,1,1,1,1,1,2,2,2,2]

data FlattenEnv = FlattenEnv

newtype FlattenM a = FlattenM (StateT VNameSource (Reader FlattenEnv) a)
  deriving
    ( MonadState VNameSource,
      MonadFreshNames,
      MonadReader FlattenEnv,
      Monad,
      Functor,
      Applicative
    )

data IrregularRep = IrregularRep
  { -- | Array of size of each segment, type @[]i64@.
    irregularS :: VName,
    irregularF :: VName,
    irregularO :: VName,
    irregularD :: VName
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

insertIrregular :: VName -> VName -> VName -> ResTag -> VName -> DistEnv -> DistEnv
insertIrregular ns flags offsets rt elems env =
  let rep = Irregular $ IrregularRep ns flags offsets elems
   in insertRep rt rep env

insertIrregulars :: VName -> VName -> VName -> [(ResTag, VName)] -> DistEnv -> DistEnv
insertIrregulars ns flags offsets bnds env =
  let (tags, elems) = unzip bnds
      mkRep = Irregular . IrregularRep ns flags offsets
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
segsAndElems env [] = (Nothing, [])
segsAndElems env (DistInputFree v _ : vs) =
  second (v :) $ segsAndElems env vs
segsAndElems env (DistInput rt _ : vs) =
  case resVar rt env of
    Regular v' ->
      second (v' :) $ segsAndElems env vs
    Irregular (IrregularRep segments flags offsets elems) ->
      bimap (mplus $ Just (segments, flags, offsets)) (elems :) $ segsAndElems env vs

-- Mapping from original variable names to their distributed resreps
inputReps :: DistInputs -> DistEnv -> M.Map VName (Type, ResRep)
inputReps inputs env = M.fromList $ map (second getRep) inputs
  where
    getRep di = case di of
      DistInput rt t -> (t, resVar rt env)
      DistInputFree v' t -> (t, Regular v')

type Segments = NE.NonEmpty SubExp

segmentsShape :: Segments -> Shape
segmentsShape = Shape . toList

segmentsRank :: Segments -> Int
segmentsRank = shapeRank . segmentsShape

segmentsDims :: Segments -> [SubExp]
segmentsDims = shapeDims . segmentsShape

readInput :: Segments -> DistEnv -> [SubExp] -> DistInputs -> SubExp -> Builder GPU SubExp
readInput _ _ _ _ (Constant x) = pure $ Constant x
readInput segments env is inputs (Var v) =
  case lookup v inputs of
    Nothing -> pure $ Var v
    Just (DistInputFree arr _) ->
      letSubExp (baseString v) =<< eIndex arr (map eSubExp is)
    Just (DistInput rt _) -> do
      case resVar rt env of
        Regular arr ->
          letSubExp (baseString v) =<< eIndex arr (map eSubExp is)
        Irregular (IrregularRep _ flags offsets elems) ->
          undefined

readInputs :: Segments -> DistEnv -> [SubExp] -> DistInputs -> Builder GPU ()
readInputs segments env is = mapM_ onInput
  where
    onInput (v, DistInputFree arr _) =
      letBindNames [v] =<< eIndex arr (map eSubExp is)
    onInput (v, DistInput rt t) =
      case resVar rt env of
        Regular arr ->
          letBindNames [v] =<< eIndex arr (map eSubExp is)
        Irregular (IrregularRep _ _ offsets elems) -> do
          offset <- letSubExp "offset" =<< eIndex offsets (map eSubExp is)
          case arrayDims t of
            [num_elems] -> do
              let slice = Slice [DimSlice offset num_elems (intConst Int64 1)]
              letBindNames [v] $ BasicOp $ Index elems slice
            _ -> do
              num_elems <-
                letSubExp "num_elems" =<< toExp (product $ map pe64 $ arrayDims t)
              let slice = Slice [DimSlice offset num_elems (intConst Int64 1)]
              v_flat <-
                letExp (baseString v <> "_flat") $ BasicOp $ Index elems slice
              letBindNames [v] . BasicOp $
                Reshape ReshapeArbitrary (arrayShape t) v_flat

transformScalarStms ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Stms SOACS ->
  [VName] ->
  Builder GPU DistEnv
transformScalarStms segments env inps distres stms res = do
  vs <- letTupExp "scalar_dist" <=< renameExp <=< segMap segments $ \is -> do
    readInputs segments env (toList is) inps
    addStms $ fmap soacsStmToGPU stms
    pure $ subExpsRes $ map Var res
  pure $ insertReps (zip (map distResTag distres) $ map Regular vs) env

transformScalarStm ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Stm SOACS ->
  Builder GPU DistEnv
transformScalarStm segments env inps res stm =
  transformScalarStms segments env inps res (oneStm stm) (patNames (stmPat stm))

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
elemArr :: Segments -> DistEnv -> DistInputs -> SubExp -> Builder GPU VName
elemArr segments env inps (Var v)
  | Just v_inp <- lookup v inps =
      case v_inp of
        DistInputFree vs _ -> irregularD <$> mkIrregFromReg segments vs
        DistInput rt _ -> case resVar rt env of
          Irregular r -> pure $ irregularD r
          Regular vs -> irregularD <$> mkIrregFromReg segments vs
elemArr segments _ _ se = do
  rep <- letExp "rep" $ BasicOp $ Replicate (segmentsShape segments) se
  dims <- arrayDims <$> lookupType rep
  if length dims == 1
    then pure rep
    else do
      n <- toSubExp "n" $ product $ map pe64 dims
      letExp "reshape" $ BasicOp $ Reshape ReshapeArbitrary (Shape [n]) rep

mkIrregFromReg ::
  Segments ->
  VName ->
  Builder GPU IrregularRep
mkIrregFromReg segments arr = do
  arr_t <- lookupType arr
  segment_size <-
    letSubExp "reg_seg_size" <=< toExp . product . map pe64 $
      drop (segmentsRank segments) (arrayDims arr_t)
  arr_S <-
    letExp "reg_segments" . BasicOp $
      Replicate (segmentsShape segments) segment_size
  num_elems <-
    letSubExp "reg_num_elems" <=< toExp $ product $ map pe64 $ arrayDims arr_t
  arr_D <-
    letExp "reg_D" . BasicOp $
      Reshape ReshapeArbitrary (Shape [num_elems]) arr
  arr_F <- letExp "reg_F" <=< segMap (Solo num_elems) $ \(Solo i) -> do
    flag <- letSubExp "flag" <=< toExp $ (pe64 i `rem` pe64 segment_size) .==. 0
    pure [subExpRes flag]
  arr_O <- letExp "reg_O" <=< segMap (shapeDims (segmentsShape segments)) $ \is -> do
    let flat_seg_i =
          flattenIndex
            (map pe64 (shapeDims (segmentsShape segments)))
            (map pe64 is)
    offset <- letSubExp "offset" <=< toExp $ flat_seg_i * pe64 segment_size
    pure [subExpRes offset]
  pure $
    IrregularRep
      { irregularS = arr_S,
        irregularF = arr_F,
        irregularO = arr_O,
        irregularD = arr_D
      }

-- Get the irregular representation of a var.
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
        letExp (baseString v <> "_rep") . BasicOp $
          Replicate (segmentsShape segments) (Var v)
      mkIrregFromReg segments v'

-- Do 'map2 replicate ns A', where 'A' is an irregular array (and so
-- is the result, obviously).
replicateIrreg ::
  Segments ->
  DistEnv ->
  VName ->
  String ->
  IrregularRep ->
  Builder GPU IrregularRep
replicateIrreg segments env ns desc rep = do
  -- Replication does not change the number of segments - it simply
  -- makes each of them larger.

  num_segments <- arraySize 0 <$> lookupType ns

  -- ns multipled with existing segment sizes.
  ns_full <- letExp (baseString ns <> "_full") <=< segMap (Solo num_segments) $
    \(Solo i) -> do
      n <-
        letSubExp "n" =<< eIndex ns [eSubExp i]
      old_segment <-
        letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      full_segment <-
        letSubExp "new_segment" =<< toExp (pe64 n * pe64 old_segment)
      pure $ subExpsRes [full_segment]

  (ns_full_flags, ns_full_offsets, ns_full_elems) <- doRepIota ns_full
  (_, _, flat_to_segs) <- doSegIota ns_full

  w <- arraySize 0 <$> lookupType ns_full_elems

  elems <- letExp (desc <> "_rep_elems") <=< segMap (Solo w) $ \(Solo i) -> do
    -- Which segment we are in.
    segment_i <-
      letSubExp "segment_i" =<< eIndex ns_full_elems [eSubExp i]
    -- Size of original segment.
    old_segment <-
      letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp segment_i]
    -- Index of value inside *new* segment.
    j_new <-
      letSubExp "j_new" =<< eIndex flat_to_segs [eSubExp i]
    -- Index of value inside *old* segment.
    j_old <-
      letSubExp "j_old" =<< toExp (pe64 j_new `rem` pe64 old_segment)
    -- Offset of values in original segment.
    offset <-
      letSubExp "offset" =<< eIndex (irregularO rep) [eSubExp segment_i]
    v <-
      letSubExp "v"
        =<< eIndex (irregularD rep) [toExp $ pe64 offset + pe64 j_old]
    pure $ subExpsRes [v]

  pure $
    IrregularRep
      { irregularS = ns_full,
        irregularF = ns_full_flags,
        irregularO = ns_full_offsets,
        irregularD = elems
      }

rearrangeFlat :: (IntegralExp num) => [Int] -> [num] -> num -> num
rearrangeFlat perm dims i =
  -- TODO?  Maybe we need to invert one of these permutations.
  flattenIndex dims $
    rearrangeShape perm $
      unflattenIndex (rearrangeShape perm dims) i

rearrangeIrreg ::
  Segments ->
  DistEnv ->
  TypeBase Shape u ->
  [Int] ->
  IrregularRep ->
  Builder GPU IrregularRep
rearrangeIrreg segments env v_t perm (IrregularRep shape flags offsets elems) = do
  m <- arraySize 0 <$> lookupType elems
  (_, _, ii1_vss) <- doRepIota shape
  (_, _, ii2_vss) <- doSegIota shape
  elems' <- letExp "elems_rearrange" <=< renameExp <=< segMap (Solo m) $
    \(Solo i) -> do
      seg_i <- letSubExp "seg_i" =<< eIndex ii1_vss [eSubExp i]
      offset <- letSubExp "offset" =<< eIndex offsets [eSubExp seg_i]
      in_seg_i <- letSubExp "in_seg_i" =<< eIndex ii2_vss [eSubExp i]
      let v_dims = map pe64 $ arrayDims v_t
          in_seg_is_tr = rearrangeFlat perm v_dims $ pe64 in_seg_i
      v' <-
        letSubExp "v"
          =<< eIndex elems [toExp $ pe64 offset + in_seg_is_tr]
      pure [subExpRes v']
  pure $
    IrregularRep
      { irregularS = shape,
        irregularF = flags,
        irregularO = offsets,
        irregularD = elems'
      }

transformDistBasicOp ::
  Segments ->
  DistEnv ->
  ( DistInputs,
    DistResult,
    PatElem Type,
    StmAux (),
    BasicOp
  ) ->
  Builder GPU DistEnv
transformDistBasicOp segments env (inps, res, pe, aux, e) =
  case e of
    BinOp {} ->
      scalarCase
    CmpOp {} ->
      scalarCase
    ConvOp {} ->
      scalarCase
    UnOp {} ->
      scalarCase
    Assert {} ->
      scalarCase
    Opaque op se
      | Var v <- se,
        Just (DistInput rt_in _) <- lookup v inps ->
          -- TODO: actually insert opaques
          pure $ insertRep (distResTag res) (resVar rt_in env) env
      | otherwise ->
          scalarCase
    Reshape _ _ arr
      | Just (DistInput rt_in _) <- lookup arr inps ->
          pure $ insertRep (distResTag res) (resVar rt_in env) env
    Index arr slice
      | null $ sliceDims slice ->
          scalarCase
      | otherwise -> do
          -- Maximally irregular case.
          ns <- letExp "slice_sizes" <=< segMap segments $ \is -> do
            slice_ns <- mapM (readInput segments env (toList is) inps) $ sliceDims slice
            fmap varsRes . letTupExp "n" <=< toExp $ product $ map pe64 slice_ns
          (_n, offsets, m) <- exScanAndSum ns
          (_, _, repiota_elems) <- doRepIota ns
          flags <- genFlags m offsets
          elems <- letExp "elems" <=< renameExp <=< segMap (NE.singleton m) $ \is -> do
            segment <- letSubExp "segment" =<< eIndex repiota_elems (toList $ fmap eSubExp is)
            segment_start <- letSubExp "segment_start" =<< eIndex offsets [eSubExp segment]
            readInputs segments env [segment] inps
            -- TODO: multidimensional segments
            let slice' =
                  fixSlice (fmap pe64 slice) $
                    unflattenIndex (map pe64 (sliceDims slice)) $
                      subtract (pe64 segment_start) . pe64 $
                        NE.head is
            auxing aux $
              fmap (subExpsRes . pure) . letSubExp "v"
                =<< eIndex arr (map toExp slice')
          pure $ insertIrregular ns flags offsets (distResTag res) elems env
    Iota n (Constant x) (Constant s) Int64
      | zeroIsh x,
        oneIsh s -> do
          ns <- elemArr segments env inps n
          (flags, offsets, elems) <- certifying (distCerts inps aux env) $ doSegIota ns
          pure $ insertIrregular ns flags offsets (distResTag res) elems env
    Iota n x s it -> do
      ns <- elemArr segments env inps n
      xs <- elemArr segments env inps x
      ss <- elemArr segments env inps s
      (flags, offsets, elems) <- certifying (distCerts inps aux env) $ doSegIota ns
      (_, _, repiota_elems) <- doRepIota ns
      m <- arraySize 0 <$> lookupType elems
      elems' <- letExp "iota_elems_fixed" <=< segMap (Solo m) $ \(Solo i) -> do
        segment <- letSubExp "segment" =<< eIndex repiota_elems [eSubExp i]
        v' <- letSubExp "v" =<< eIndex elems [eSubExp i]
        x' <- letSubExp "x" =<< eIndex xs [eSubExp segment]
        s' <- letSubExp "s" =<< eIndex ss [eSubExp segment]
        fmap (subExpsRes . pure) . letSubExp "v" <=< toExp $
          primExpFromSubExp (IntType it) x'
            ~+~ sExt it (untyped (pe64 v'))
            ~*~ primExpFromSubExp (IntType it) s'
      pure $ insertIrregular ns flags offsets (distResTag res) elems' env
    Replicate (Shape [n]) (Var v) -> do
      ns <- elemArr segments env inps n
      rep <- getIrregRep segments env inps v
      rep' <- replicateIrreg segments env ns (baseString v) rep
      pure $ insertRep (distResTag res) (Irregular rep') env
    Replicate (Shape [n]) (Constant v) -> do
      ns <- elemArr segments env inps n
      (flags, offsets, elems) <-
        certifying (distCerts inps aux env) $ doSegIota ns
      w <- arraySize 0 <$> lookupType elems
      elems' <- letExp "rep_const" $ BasicOp $ Replicate (Shape [w]) (Constant v)
      pure $ insertIrregular ns flags offsets (distResTag res) elems' env
    Replicate (Shape []) (Var v) ->
      case lookup v inps of
        Just (DistInputFree v' _) -> do
          v'' <-
            letExp (baseString v' <> "_copy") . BasicOp $
              Replicate mempty (Var v')
          pure $ insertRegulars [distResTag res] [v''] env
        Just (DistInput rt _) ->
          case resVar rt env of
            Irregular r -> do
              let name = baseString (irregularD r) <> "_copy"
              elems_copy <-
                letExp name . BasicOp $
                  Replicate mempty (Var $ irregularD r)
              let rep = Irregular $ r {irregularD = elems_copy}
              pure $ insertRep (distResTag res) rep env
            Regular v' -> do
              v'' <-
                letExp (baseString v' <> "_copy") . BasicOp $
                  Replicate mempty (Var v')
              pure $ insertRegulars [distResTag res] [v''] env
        Nothing -> do
          v' <-
            letExp (baseString v <> "_copy_free") . BasicOp $
              Replicate (segmentsShape segments) (Var v)
          pure $ insertRegulars [distResTag res] [v'] env
    Update _ as slice (Var v)
      | Just as_t <- distInputType <$> lookup as inps -> do
          ns <- letExp "slice_sizes"
            <=< renameExp
            <=< segMap (shapeDims (segmentsShape segments))
            $ \is -> do
              readInputs segments env is $
                filter ((`elem` sliceDims slice) . Var . fst) inps
              n <- letSubExp "n" <=< toExp $ product $ map pe64 $ sliceDims slice
              pure [subExpRes n]
          -- Irregular representation of `as`
          IrregularRep shape flags offsets elems <- getIrregRep segments env inps as
          -- Inner indices (1 and 2) of `ns`
          (_, _, ii1_vss) <- doRepIota ns
          (_, _, ii2_vss) <- certifying (distCerts inps aux env) $ doSegIota ns
          -- Number of updates to perform
          m <- arraySize 0 <$> lookupType ii2_vss
          elems' <- letExp "elems_scatter" <=< renameExp <=< genScatter elems m $ \gid -> do
            seg_i <- letSubExp "seg_i" =<< eIndex ii1_vss [eSubExp gid]
            in_seg_i <- letSubExp "in_seg_i" =<< eIndex ii2_vss [eSubExp gid]
            readInputs segments env [seg_i] $ filter ((/= as) . fst) inps
            v_t <- lookupType v
            let in_seg_is =
                  unflattenIndex (map pe64 (arrayDims v_t)) (pe64 in_seg_i)
                slice' = fmap pe64 slice
                flat_i =
                  flattenIndex
                    (map pe64 $ arrayDims as_t)
                    (fixSlice slice' in_seg_is)
            -- Value to write
            v' <- letSubExp "v" =<< eIndex v (map toExp in_seg_is)
            o' <- letSubExp "o" =<< eIndex offsets [eSubExp seg_i]
            -- Index to write `v'` at
            i <- letExp "i" =<< toExp (pe64 o' + flat_i)
            pure (i, v')
          pure $ insertIrregular shape flags offsets (distResTag res) elems' env
      | otherwise ->
          error "Flattening update: destination is not input."
    Rearrange perm v -> do
      case lookup v inps of
        Just (DistInputFree v' _) -> do
          v'' <-
            letExp (baseString v' <> "_tr") . BasicOp $
              Rearrange perm v'
          pure $ insertRegulars [distResTag res] [v''] env
        Just (DistInput rt v_t) -> do
          case resVar rt env of
            Irregular rep -> do
              rep' <-
                certifying (distCerts inps aux env) $
                  rearrangeIrreg segments env v_t perm rep
              pure $ insertRep (distResTag res) (Irregular rep') env
            Regular v' -> do
              let r = segmentsRank segments
              v'' <-
                letExp (baseString v' <> "_tr") . BasicOp $
                  Rearrange ([0 .. r - 1] ++ map (+ r) perm) v'
              pure $ insertRegulars [distResTag res] [v''] env
        Nothing -> do
          let r = segmentsRank segments
          v' <-
            letExp (baseString v <> "_tr") . BasicOp $
              Rearrange ([0 .. r - 1] ++ map (+ r) perm) v
          pure $ insertRegulars [distResTag res] [v'] env
    _ -> error $ "Unhandled BasicOp:\n" ++ prettyString e
  where
    scalarCase =
      transformScalarStm segments env inps [res] $
        Let (Pat [pe]) aux (BasicOp e)

-- Replicates inner dimension for inputs.
onMapFreeVar ::
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  (VName, VName, VName) ->
  VName ->
  Maybe (Builder GPU (VName, MapArray IrregularRep))
onMapFreeVar segments env inps ws (ws_flags, ws_offsets, ws_elems) v = do
  let segments_per_elem = ws_elems
  v_inp <- lookup v inps
  pure $ do
    ws_prod <- arraySize 0 <$> lookupType ws_elems
    fmap (v,) $ case v_inp of
      DistInputFree v' t -> do
        fmap (`MapArray` t)
          . letExp (baseString v <> "_rep_free_free_inp")
          <=< segMap (Solo ws_prod)
          $ \(Solo i) -> do
            segment <- letSubExp "segment" =<< eIndex segments_per_elem [eSubExp i]
            subExpsRes . pure <$> (letSubExp "v" =<< eIndex v' [eSubExp segment])
      DistInput rt t -> case resVar rt env of
        Irregular rep -> do
          offsets <- letExp (baseString v <> "_rep_free_irreg_offsets")
            <=< segMap (Solo ws_prod)
            $ \(Solo i) -> do
              segment <- letSubExp "segment" =<< eIndex ws_elems [eSubExp i]
              subExpsRes . pure <$> (letSubExp "v" =<< eIndex (irregularO rep) [eSubExp segment])
          let rep' =
                IrregularRep
                  { irregularS = ws,
                    irregularF = irregularF rep,
                    irregularO = offsets,
                    irregularD = irregularD rep
                  }
          pure $ MapOther rep' t
        Regular vs ->
          fmap (`MapArray` t)
            . letExp (baseString v <> "_rep_free_reg_inp")
            <=< segMap (Solo ws_prod)
            $ \(Solo i) -> do
              segment <- letSubExp "segment" =<< eIndex segments_per_elem [eSubExp i]
              subExpsRes . pure <$> (letSubExp "v" =<< eIndex vs [eSubExp segment])

onMapInputArr ::
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  Param Type ->
  VName ->
  Builder GPU (MapArray IrregularRep)
onMapInputArr segments env inps ii2 p arr = do
  ws_prod <- arraySize 0 <$> lookupType ii2
  case lookup arr inps of
    Just v_inp ->
      case v_inp of
        DistInputFree vs t ->
          pure $ MapArray vs t
        DistInput rt _ ->
          case resVar rt env of
            Irregular rep -> do
              elems_t <- lookupType $ irregularD rep
              -- If parameter type of the map corresponds to the
              -- element type of the value array, we can map it
              -- directly.
              if stripArray (segmentsRank segments) elems_t == paramType p
                then pure $ MapArray (irregularD rep) elems_t
                else do
                  -- Otherwise we need to perform surgery on the metadata.
                  ~[p_segments, p_offsets] <- letTupExp
                    (baseString (paramName p) <> "_rep_inp_irreg")
                    <=< segMap (Solo ws_prod)
                    $ \(Solo i) -> do
                      segment_i <-
                        letSubExp "segment" =<< eIndex ii2 [eSubExp i]
                      segment <-
                        letSubExp "v" =<< eIndex (irregularS rep) [eSubExp segment_i]
                      offset <-
                        letSubExp "v" =<< eIndex (irregularO rep) [eSubExp segment_i]
                      pure $ subExpsRes [segment, offset]
                  let rep' =
                        IrregularRep
                          { irregularD = irregularD rep,
                            irregularF = irregularF rep,
                            irregularS = p_segments,
                            irregularO = p_offsets
                          }
                  pure $ MapOther rep' elems_t
            Regular vs ->
              undefined
    Nothing -> do
      arr_row_t <- rowType <$> lookupType arr
      arr_rep <-
        letExp (baseString arr <> "_inp_rep") . BasicOp $
          Replicate (segmentsShape segments) (Var arr)
      v <-
        letExp (baseString arr <> "_inp_rep_flat") . BasicOp $
          Reshape ReshapeArbitrary (Shape [ws_prod] <> arrayShape arr_row_t) arr_rep
      pure $ MapArray v arr_row_t

scopeOfDistInputs :: DistInputs -> Scope GPU
scopeOfDistInputs = scopeOfLParams . map f
  where
    f (v, inp) = Param mempty v (distInputType inp)

transformInnerMap ::
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Builder GPU (VName, VName, VName)
transformInnerMap segments env inps pat w arrs map_lam = do
  ws <- elemArr segments env inps w
  (ws_flags, ws_offsets, ws_elems) <- doRepIota ws
  new_segment <- arraySize 0 <$> lookupType ws_elems
  arrs' <-
    zipWithM
      (onMapInputArr segments env inps ws_elems)
      (lambdaParams map_lam)
      arrs
  let free = freeIn map_lam
  free_sizes <-
    localScope (scopeOfDistInputs inps) $
      foldMap freeIn <$> mapM lookupType (namesToList free)
  let free_and_sizes = namesToList $ free <> free_sizes
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe
        (onMapFreeVar segments env inps ws (ws_flags, ws_offsets, ws_elems))
        free_and_sizes
  free_ps <-
    zipWithM
      newParam
      (map ((<> "_free") . baseString) free_and_sizes)
      (map mapArrayRowType replicated)
  scope <- askScope
  let substs = M.fromList $ zip free_replicated $ map paramName free_ps
      map_lam' =
        substituteNames
          substs
          ( map_lam
              { lambdaParams = free_ps <> lambdaParams map_lam
              }
          )
      (distributed, arrmap) =
        distributeMap scope pat new_segment (replicated <> arrs') map_lam'
      m =
        transformDistributed arrmap (NE.singleton new_segment) distributed
  traceM $ unlines ["inner map distributed", prettyString distributed]
  addStms =<< runReaderT (runBuilder_ m) scope
  pure (ws_flags, ws_offsets, ws)

transformDistStm :: Segments -> DistEnv -> DistStm -> Builder GPU DistEnv
transformDistStm segments env (DistStm inps res stm) = do
  case stm of
    Let pat aux (BasicOp e) -> do
      let ~[res'] = res
          ~[pe] = patElems pat
      transformDistBasicOp segments env (inps, res', pe, aux, e)
    Let pat _ (Op (Screma w arrs form))
      | Just reds <- isReduceSOAC form,
        Just arrs' <- mapM (`lookup` inps) arrs,
        (Just (arr_segments, flags, offsets), elems) <- segsAndElems env arrs' -> do
          elems' <- genSegRed arr_segments flags offsets elems $ singleReduce reds
          pure $ insertReps (zip (map distResTag res) (map Regular elems')) env
      | Just (reds, map_lam) <- isRedomapSOAC form -> do
          map_pat <- fmap Pat $ forM (lambdaReturnType map_lam) $ \t ->
            PatElem <$> newVName "map" <*> pure (t `arrayOfRow` w)
          (ws_flags, ws_offsets, ws) <-
            transformInnerMap segments env inps map_pat w arrs map_lam
          let (redout_names, mapout_names) =
                splitAt (redResults reds) (patNames map_pat)
          elems' <-
            genSegRed ws ws_flags ws_offsets redout_names $
              singleReduce reds
          let (red_tags, map_tags) = splitAt (redResults reds) $ map distResTag res
          pure $
            insertRegulars red_tags elems' $
              insertIrregulars ws ws_flags ws_offsets (zip map_tags mapout_names) env
      | Just map_lam <- isMapSOAC form -> do
          (ws_flags, ws_offsets, ws) <- transformInnerMap segments env inps pat w arrs map_lam
          pure $ insertIrregulars ws ws_flags ws_offsets (zip (map distResTag res) $ patNames pat) env
    Let _ _ (Match scrutinees cases defaultCase _) -> do
      let [w] = NE.toList segments

      -- Lift the scrutinees.
      -- If it's a variable, we know it's a scalar and the lifted version will therefore be a regular array.
      lifted_scrutinees <- forM scrutinees $ \scrut -> do
        (_, rep) <- liftSubExp segments inps env scrut
        case rep of
          Regular v' -> pure v'
          Irregular {} ->
            error $
              "transformDistStm: Non-scalar match scrutinee: " ++ prettyString scrut
      -- Cases for tagging values that match the same branch.
      -- The default case is the 0'th equvalence class.
      let equiv_cases =
            zipWith
              ( \(Case pat _) n ->
                  Case pat $ eBody [toExp $ intConst Int64 n]
              )
              cases
              [1 ..]
      let equiv_case_default = eBody [toExp $ intConst Int64 0]
      -- Match the scrutinees againts the branch cases
      equiv_classes <- letExp "equiv_classes" <=< segMap (Solo w) $ \(Solo i) -> do
        scruts <- mapM (letSubExp "scruts" <=< flip eIndex [toExp i]) lifted_scrutinees
        cls <- letSubExp "cls" =<< eMatch scruts equiv_cases equiv_case_default
        pure [subExpRes cls]
      let num_cases = fromIntegral $ length cases + 1
      n_cases <- letExp "n_cases" <=< toExp $ intConst Int64 num_cases
      -- Parition the indices of the scrutinees by their equvalence class such
      -- that (the indices) of the scrutinees belonging to class 0 come first,
      -- then those belonging to class 1 and so on.
      (partition_sizes, partition_offs, partition_inds) <- doPartition n_cases equiv_classes
      inds_t <- lookupType partition_inds
      -- Get the indices of each scrutinee by equivalence class
      inds <- forM [0 .. num_cases - 1] $ \i -> do
        num_elems <-
          letSubExp ("size" ++ show i)
            =<< eIndex partition_sizes [toExp $ intConst Int64 i]
        begin <-
          letSubExp ("idx_begin" ++ show i)
            =<< eIndex partition_offs [toExp $ intConst Int64 i]
        letExp ("inds_branch" ++ show i) $
          BasicOp $
            Index partition_inds $
              fullSlice inds_t [DimSlice begin num_elems (intConst Int64 1)]

      -- Take the elements at index `is` from an input `v`.
      let splitInput is v = do
            (t, rep) <- liftSubExp segments inps env (Var v)
            (t,v,) <$> case rep of
              Regular arr -> do
                -- In the regular case we just take the elements
                -- of the array given by `is`
                n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
                arr' <- letExp "split_arr" <=< segMap (Solo n) $ \(Solo i) -> do
                  idx <- letExp "idx" =<< eIndex is [eSubExp i]
                  subExpsRes . pure <$> (letSubExp "arr" =<< eIndex arr [toExp idx])
                pure $ Regular arr'
              Irregular (IrregularRep segs flags offsets elems) -> do
                -- In the irregular case we take the elements
                -- of the `segs` array given by `is` like in the regular case
                n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
                segs' <- letExp "split_segs" <=< segMap (Solo n) $ \(Solo i) -> do
                  idx <- letExp "idx" =<< eIndex is [eSubExp i]
                  subExpsRes . pure <$> (letSubExp "segs" =<< eIndex segs [toExp idx])
                -- From this we calculate the offsets and number of elements
                (_, offsets', num_elems) <- exScanAndSum segs'
                (_, _, ii1) <- doRepIota segs'
                (_, _, ii2) <- doSegIota segs'
                -- We then take the elements we need from `elems` and `flags`
                -- For each index `i`, we roughly:
                -- Get the offset of the segment we want to copy by indexing
                -- `offsets` through `is` further through `ii1` i.e.
                -- `offset = offsets[is[ii1[i]]]`
                -- We then add `ii2[i]` to `offset`
                -- and use that to index into `elems` and `flags`.
                ~[flags', elems'] <- letTupExp "split_flags_elems" <=< segMap (Solo num_elems) $ \(Solo i) -> do
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
                        irregularD = elems'
                      }
      -- Given the indices for which a branch is taken and its body,
      -- distribute the statements of the body of that branch.
      let distributeBranch is body = do
            (ts, vs, reps) <- unzip3 <$> mapM (splitInput is) (namesToList $ freeIn body)
            let inputs = do
                  (v, t, i) <- zip3 vs ts [0 ..]
                  pure (v, DistInput (ResTag i) t)
            let env' = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
            scope <- askScope
            let (inputs', dstms) = distributeBody scope w inputs body
            pure (inputs', env', dstms)

      -- Distribute and lift the branch bodies.
      -- We put the default case at the start as it's the 0'th equivalence class
      -- and is therefore the first segment after the partition.
      let branch_bodies = defaultCase : map (\(Case _ body) -> body) cases
      (branch_inputs, branch_envs, branch_dstms) <- unzip3 <$> zipWithM distributeBranch inds branch_bodies
      let branch_results = map bodyResult branch_bodies
      lifted_bodies <- forM [0 .. num_cases - 1] $ \i -> do
        size <- letSubExp "size" =<< eIndex partition_sizes [toExp $ intConst Int64 i]
        let inputs = branch_inputs !! fromIntegral i
        let env' = branch_envs !! fromIntegral i
        let dstms = branch_dstms !! fromIntegral i
        let result = branch_results !! fromIntegral i
        res' <- liftBody size inputs env' dstms result
        subExpsRes <$> mapM (\(SubExpRes _ se) -> letSubExp ("result" ++ show i) =<< toExp se) res'

      let result_types = map ((\(DistType _ _ t) -> t) . distResType) res
      branch_reps <-
        mapM
          ( fmap (resultToResReps result_types)
              . mapM (letExp "branch_result" <=< toExp . resSubExp)
          )
          lifted_bodies

      -- Write back the regular results of a branch to a (partially) blank space
      let scatterRegular space (is, xs) = do
            ~(Array _ (Shape [size]) _) <- lookupType xs
            letExp "regular_scatter"
              =<< genScatter
                space
                size
                ( \gtid -> do
                    x <- letSubExp "x" =<< eIndex xs [eSubExp gtid]
                    i <- letExp "i" =<< eIndex is [eSubExp gtid]
                    pure (i, x)
                )
      -- Write back the irregular elements of a branch to a (partially) blank space
      -- The `offsets` variable is the offsets of the final result,
      -- whereas `irregRep` is the irregular representation of the result of a single branch.
      let scatterIrregular offsets space (is, irregRep) = do
            let IrregularRep {irregularS = segs, irregularD = elems} = irregRep
            (_, _, ii1) <- doRepIota segs
            (_, _, ii2) <- doSegIota segs
            ~(Array _ (Shape [size]) _) <- lookupType elems
            letExp "irregular_scatter"
              =<< genScatter
                space
                size
                ( \gtid -> do
                    x <- letSubExp "x" =<< eIndex elems [eSubExp gtid]
                    offset <- letExp "offset" =<< eIndex offsets [eIndex is [eIndex ii1 [eSubExp gtid]]]
                    i <- letExp "i" =<< eBinOp (Add Int64 OverflowUndef) (toExp offset) (eIndex ii2 [eSubExp gtid])
                    pure (i, x)
                )
      -- Given a single result from each branch as well the *unlifted*
      -- result type, merge the results of all branches into a single result.
      let mergeResult iss branchesRep resType =
            case resType of
              -- Regular case
              Prim pt -> do
                let xs = map (\(Regular v) -> v) branchesRep
                let resultType = Array pt (Shape [w]) NoUniqueness
                -- Create the blank space for the result
                resultSpace <- letExp "blank_res" =<< eBlank resultType
                -- Write back the values of each branch to the blank space
                result <- foldM scatterRegular resultSpace $ zip iss xs
                pure $ Regular result
              -- Irregular case
              Array pt _ _ -> do
                let branchesIrregRep = map (\(Irregular irregRep) -> irregRep) branchesRep
                let segsType = Array (IntType Int64) (Shape [w]) NoUniqueness
                -- Create a blank space for the 'segs'
                segsSpace <- letExp "blank_segs" =<< eBlank segsType
                -- Write back the segs of each branch to the blank space
                segs <- foldM scatterRegular segsSpace $ zip iss (irregularS <$> branchesIrregRep)
                (_, offsets, num_elems) <- exScanAndSum segs
                let resultType = Array pt (Shape [num_elems]) NoUniqueness
                -- Create the blank space for the result
                resultSpace <- letExp "blank_res" =<< eBlank resultType
                -- Write back the values of each branch to the blank space
                elems <- foldM (scatterIrregular offsets) resultSpace $ zip iss branchesIrregRep
                flags <- genFlags num_elems offsets
                pure $
                  Irregular $
                    IrregularRep
                      { irregularS = segs,
                        irregularF = flags,
                        irregularO = offsets,
                        irregularD = elems
                      }
              Acc {} -> error "transformDistStm: Acc"
              Mem {} -> error "transformDistStm: Mem"

      -- Merge the results of the branches and insert the resulting res reps
      reps <- zipWithM (mergeResult inds) (L.transpose branch_reps) result_types
      pure $ insertReps (zip (map distResTag res) reps) env
    Let _ _ (Apply name args rettype s) -> do
      let [w] = NE.toList segments
          name' = liftFunName name
      args' <- ((w, Observe) :) . concat <$> mapM (liftArg segments inps env) args
      args_ts <- mapM (subExpType . fst) args'
      let dietToUnique Consume = Unique
          dietToUnique Observe = Nonunique
          dietToUnique ObservePrim = Nonunique
          param_ts = zipWith toDecl args_ts $ map (dietToUnique . snd) args'
          rettype' = addRetAls param_ts $ liftRetType w $ map fst rettype

      result <- letTupExp (nameToString name' <> "_res") $ Apply name' args' rettype' s
      let reps = resultToResReps (map fst rettype) result
      pure $ insertReps (zip (map distResTag res) reps) env
    _ -> error $ "Unhandled Stm:\n" ++ prettyString stm

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
                rep = Irregular $ IrregularRep segs flags offsets elems
             in (rs', rep)
          Acc {} -> error "resultToResReps: Illegal type 'Acc'"
          Mem {} -> error "resultToResReps: Illegal type 'Mem'"
      )
      results
      types

distResCerts :: DistEnv -> [DistInput] -> Certs
distResCerts env = Certs . map f
  where
    f (DistInputFree v _) = v
    f (DistInput rt _) = case resVar rt env of
      Regular v -> v
      Irregular {} -> error "resCerts: irregular"

transformDistributed ::
  M.Map ResTag IrregularRep ->
  Segments ->
  Distributed ->
  Builder GPU ()
transformDistributed irregs segments dist = do
  let Distributed dstms (DistResults resmap reps) = dist
  env <- foldM (transformDistStm segments) env_initial dstms
  forM_ (M.toList resmap) $ \(rt, (cs_inps, v, v_t)) ->
    certifying (distResCerts env cs_inps) $
      -- FIXME: the copies are because we have too liberal aliases on
      -- lifted functions.
      case resVar rt env of
        Regular v' -> letBindNames [v] $ BasicOp $ Replicate mempty $ Var v'
        Irregular irreg -> do
          -- It might have an irregular representation, but we know
          -- that it is actually regular because it is a result.
          let shape = segmentsShape segments <> arrayShape v_t
          v_copy <-
            letExp (baseString v) . BasicOp $
              Replicate mempty (Var $ irregularD irreg)
          letBindNames [v] $
            BasicOp (Reshape ReshapeArbitrary shape v_copy)
  forM_ reps $ \(v, r) ->
    case r of
      Left se ->
        letBindNames [v] $ BasicOp $ Replicate (segmentsShape segments) se
      Right (DistInputFree arr _) ->
        letBindNames [v] $ BasicOp $ SubExp $ Var arr
      Right DistInput {} ->
        error "replication of irregular identity result"
  where
    env_initial = DistEnv {distResMap = M.map Irregular irregs}

transformStm :: Scope SOACS -> Stm SOACS -> PassM (Stms GPU)
transformStm scope (Let pat _ (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form = do
      let arrs' =
            zipWith MapArray arrs $
              map paramType (lambdaParams (scremaLambda form))
          (distributed, _) = distributeMap scope pat w arrs' lam
          m = transformDistributed mempty (NE.singleton w) distributed
      traceM $ prettyString distributed
      runReaderT (runBuilder_ m) scope
transformStm _ stm = pure $ oneStm $ soacsStmToGPU stm

transformStms :: Scope SOACS -> Stms SOACS -> PassM (Stms GPU)
transformStms scope stms =
  fold <$> traverse (transformStm (scope <> scopeOf stms)) stms

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
          Acc {} -> error "getRepSubExp: Acc"
          Mem {} -> error "getRepSubExp: Mem"
    Just (t, Irregular irreg) -> pure (t, Irregular irreg)
    Nothing -> do
      t <- lookupType v
      v' <- letExp "free_replicated" $ BasicOp $ Replicate (segmentsShape segments) (Var v)
      (t,)
        <$> case t of
          Prim {} -> pure $ Regular v'
          Array {} -> Irregular <$> mkIrregFromReg segments v'
          Acc {} -> error "getRepSubExp: Acc"
          Mem {} -> error "getRepSubExp: Mem"

liftParam :: SubExp -> FParam SOACS -> PassM ([FParam GPU], ResRep)
liftParam w fparam =
  case declTypeOf fparam of
    Prim pt -> do
      p <-
        newParam
          (desc <> "_lifted")
          (arrayOf (Prim pt) (Shape [w]) Nonunique)
      pure ([p], Regular $ paramName p)
    Array pt _ u -> do
      num_elems <-
        newParam (desc <> "_num_elems") $ Prim int64
      segments <-
        newParam (desc <> "_segments") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      flags <-
        newParam (desc <> "_flags") $
          arrayOf (Prim Bool) (Shape [Var (paramName num_elems)]) Nonunique
      offsets <-
        newParam (desc <> "_offsets") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      elems <-
        newParam (desc <> "_elems") $
          arrayOf (Prim pt) (Shape [Var (paramName num_elems)]) u
      pure
        ( [num_elems, segments, flags, offsets, elems],
          Irregular $
            IrregularRep
              { irregularS = paramName segments,
                irregularF = paramName flags,
                irregularO = paramName offsets,
                irregularD = paramName elems
              }
        )
    Acc {} ->
      error "liftParam: Acc"
    Mem {} ->
      error "liftParam: Mem"
  where
    desc = baseString (paramName fparam)

liftArg :: Segments -> DistInputs -> DistEnv -> (SubExp, Diet) -> Builder GPU [(SubExp, Diet)]
liftArg segments inps env (se, d) = do
  (_, rep) <- liftSubExp segments inps env se
  case rep of
    Regular v -> pure [(Var v, d)]
    Irregular irreg -> mkIrrep irreg
  where
    mkIrrep
      ( IrregularRep
          { irregularS = segs,
            irregularF = flags,
            irregularO = offsets,
            irregularD = elems
          }
        ) = do
        t <- lookupType elems
        num_elems <- letExp "num_elems" =<< toExp (product $ map pe64 $ arrayDims t)
        flags' <- letExp "flags" $ BasicOp $ Reshape ReshapeArbitrary (Shape [Var num_elems]) flags
        elems' <- letExp "elems" $ BasicOp $ Reshape ReshapeArbitrary (Shape [Var num_elems]) elems
        -- Only apply the original diet to the 'elems' array
        let diets = replicate 4 Observe ++ [d]
        pure $ zipWith (curry (first Var)) [num_elems, segs, flags', offsets, elems'] diets

-- Lifts a functions return type such that it matches the lifted functions return type.
liftRetType :: SubExp -> [RetType SOACS] -> [RetType GPU]
liftRetType w = concat . snd . L.mapAccumL liftType 0
  where
    liftType i rettype =
      let lifted = case rettype of
            Prim pt -> pure $ arrayOf (Prim pt) (Shape [Free w]) Nonunique
            Array pt _ u ->
              let num_elems = Prim int64
                  segs = arrayOf (Prim int64) (Shape [Free w]) Nonunique
                  flags = arrayOf (Prim Bool) (Shape [Ext i]) Nonunique
                  offsets = arrayOf (Prim int64) (Shape [Free w]) Nonunique
                  elems = arrayOf (Prim pt) (Shape [Ext i]) u
               in [num_elems, segs, flags, offsets, elems]
            Acc {} -> error "liftRetType: Acc"
            Mem {} -> error "liftRetType: Mem"
       in (i + length lifted, lifted)

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
        t <- lookupType elems
        num_elems <- letExp "num_elems" =<< toExp (product $ map pe64 $ arrayDims t)
        flags' <- letExp "flags" $ BasicOp $ Reshape ReshapeArbitrary (Shape [Var num_elems]) flags
        elems' <- letExp "elems" $ BasicOp $ Reshape ReshapeArbitrary (Shape [Var num_elems]) elems
        pure [num_elems, segs, flags', offsets, elems']

liftBody :: SubExp -> DistInputs -> DistEnv -> [DistStm] -> Result -> Builder GPU Result
liftBody w inputs env dstms result = do
  let segments = NE.singleton w
  env' <- foldM (transformDistStm segments) env dstms
  result' <- mapM (liftResult segments inputs env') result
  pure $ concat result'

liftFunName :: Name -> Name
liftFunName name = name <> "_lifted"

addRetAls :: [DeclType] -> [RetType GPU] -> [(RetType GPU, RetAls)]
addRetAls params rettype = zip rettype $ map possibleAliases rettype
  where
    aliasable (Array _ _ Nonunique) = True
    aliasable _ = False
    aliasable_params =
      map snd $ filter (aliasable . fst) $ zip params [0 ..]
    aliasable_rets =
      map snd $ filter (aliasable . declExtTypeOf . fst) $ zip rettype [0 ..]
    possibleAliases t
      | aliasable t = RetAls aliasable_params aliasable_rets
      | otherwise = mempty

liftFunDef :: Scope SOACS -> FunDef SOACS -> PassM (FunDef GPU)
liftFunDef const_scope fd = do
  let FunDef
        { funDefBody = body,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  wp <- newParam "w" $ Prim int64
  let w = Var $ paramName wp
  (fparams', reps) <- mapAndUnzipM (liftParam w) fparams
  let fparams'' = wp : concat fparams'
  let inputs = do
        (p, i) <- zip fparams [0 ..]
        pure (paramName p, DistInput (ResTag i) (paramType p))
  let rettype' =
        addRetAls (map paramDeclType fparams'') $
          liftRetType w (map fst rettype)
  let (inputs', dstms) =
        distributeBody const_scope (Var (paramName wp)) inputs body
      env = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
  -- Lift the body of the function and get the results
  (result, stms) <-
    runReaderT
      (runBuilder $ liftBody w inputs' env dstms $ bodyResult body)
      (const_scope <> scopeOfFParams fparams'')
  let name = liftFunName $ funDefName fd
  pure $
    fd
      { funDefName = name,
        funDefBody = Body () stms result,
        funDefParams = fparams'',
        funDefRetType = rettype'
      }

transformFunDef :: Scope SOACS -> FunDef SOACS -> PassM (FunDef GPU)
transformFunDef consts_scope fd = do
  let FunDef
        { funDefBody = Body () stms res,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  stms' <- transformStms (consts_scope <> scopeOfFParams fparams) stms
  pure $
    fd
      { funDefBody = Body () stms' res,
        funDefRetType = rettype,
        funDefParams = fparams
      }

transformProg :: Prog SOACS -> PassM (Prog GPU)
transformProg prog = do
  consts' <- transformStms mempty $ progConsts prog
  funs' <- mapM (transformFunDef $ scopeOf (progConsts prog)) $ progFuns prog
  lifted_funs <-
    mapM (liftFunDef $ scopeOf (progConsts prog)) $
      filter (isNothing . funDefEntryPoint) $
        progFuns prog
  pure $ prog {progConsts = consts', progFuns = flatteningBuiltins <> lifted_funs <> funs'}

-- | Transform a SOACS program to a GPU program, using flattening.
flattenSOACs :: Pass SOACS GPU
flattenSOACs =
  Pass
    { passName = "flatten",
      passDescription = "Perform full flattening",
      passFunction = transformProg
    }
{-# NOINLINE flattenSOACs #-}
