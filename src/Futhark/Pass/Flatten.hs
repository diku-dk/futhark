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

import Control.Monad (mapAndUnzipM)
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
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
import Futhark.Util.IntegralExp
import Prelude hiding (div, rem)

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
    irregularSegments :: VName,
    irregularFlags :: VName,
    irregularOffsets :: VName,
    irregularElems :: VName
  }

data ResRep
  = -- | This variable is represented
    -- completely straightforwardly- if it is
    -- an array, it is a regular array.
    Regular VName
  | -- | The representation of an
    -- irregular array.
    Irregular IrregularRep

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

type Segments = NE.NonEmpty SubExp

segmentsShape :: Segments -> Shape
segmentsShape = Shape . toList

segmentsDims :: Segments -> [SubExp]
segmentsDims = shapeDims . segmentsShape

segMap :: Traversable f => f SubExp -> (f SubExp -> Builder GPU Result) -> Builder GPU (Exp GPU)
segMap segments f = do
  gtids <- traverse (const $ newVName "gtid") segments
  space <- mkSegSpace $ zip (toList gtids) (toList segments)
  ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    res <- f $ fmap Var gtids
    ts <- mapM (subExpType . resSubExp) res
    pure (map mkResult res, ts)
  let kbody = KernelBody () stms res
  pure $ Op $ SegOp $ SegMap (SegThread SegVirt Nothing) space ts kbody
  where
    mkResult (SubExpRes cs se) = Returns ResultMaySimplify cs se

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
      case M.lookup rt $ distResMap env of
        Just (Regular arr) ->
          letBindNames [v] =<< eIndex arr (map eSubExp is)
        Just (Irregular (IrregularRep _ _ offsets elems)) -> do
          offset <- letSubExp "offset" =<< eIndex offsets (map eSubExp is)
          num_elems <- letSubExp "num_elems" =<< toExp (product $ map pe64 $ arrayDims t)
          let slice = Slice [DimSlice offset num_elems (intConst Int64 1)]
          letBindNames [v] $ BasicOp $ Index elems slice
        Nothing -> error $ "readInputs: " <> show rt

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
          Irregular r -> irregularElems r

-- | Only sensible for variables of segment-invariant type.
elemArr :: Segments -> DistEnv -> DistInputs -> SubExp -> Builder GPU VName
elemArr segments env inps (Var v)
  | Just v_inp <- lookup v inps =
      case v_inp of
        DistInputFree vs _ -> irregularElems <$> mkIrregFromReg segments vs
        DistInput rt _ -> case resVar rt env of
          Irregular r -> pure $ irregularElems r
          Regular vs -> irregularElems <$> mkIrregFromReg segments vs
elemArr segments _ _ se = do
  rep <- letExp "rep" $ BasicOp $ Replicate (segmentsShape segments) se
  dims <- arrayDims <$> lookupType rep
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
      drop (shapeRank (segmentsShape segments)) (arrayDims arr_t)
  segments_arr <-
    letExp "reg_segments" . BasicOp $
      Replicate (segmentsShape segments) segment_size
  num_elems <-
    letSubExp "reg_num_elems" <=< toExp $ product $ map pe64 $ arrayDims arr_t
  elems <-
    letExp "reg_elems" . BasicOp $
      Reshape ReshapeArbitrary (Shape [num_elems]) arr
  flags <- letExp "reg_flags" <=< segMap (Solo num_elems) $ \(Solo i) -> do
    flag <- letSubExp "flag" <=< toExp $ (pe64 i `rem` pe64 segment_size) .==. 0
    pure [subExpRes flag]
  offsets <- letExp "reg_offsets" <=< segMap (shapeDims (segmentsShape segments)) $ \is -> do
    let flat_seg_i =
          flattenIndex
            (map pe64 (shapeDims (segmentsShape segments)))
            (map pe64 is)
    offset <- letSubExp "offset" <=< toExp $ flat_seg_i * pe64 segment_size
    pure [subExpRes offset]
  pure $
    IrregularRep
      { irregularSegments = segments_arr,
        irregularFlags = flags,
        irregularOffsets = offsets,
        irregularElems = elems
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
    Copy v ->
      case lookup v inps of
        Just (DistInputFree v' _) -> do
          v'' <- letExp (baseString v' <> "_copy") $ BasicOp $ Copy v'
          pure $ insertRegulars [distResTag res] [v''] env
        Just (DistInput rt _) ->
          case resVar rt env of
            Irregular r -> do
              let name = baseString (irregularElems r) <> "_copy"
              elems_copy <- letExp name $ BasicOp $ Copy $ irregularElems r
              let rep = Irregular $ r {irregularElems = elems_copy}
              pure $ insertRep (distResTag res) rep env
            Regular v' -> do
              v'' <- letExp (baseString v' <> "_copy") $ BasicOp $ Copy v'
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
    _ -> error $ "Unhandled BasicOp:\n" ++ prettyString e
  where
    scalarCase =
      transformScalarStm segments env inps [res] $
        Let (Pat [pe]) aux (BasicOp e)

-- Replicates inner dimension for inputs.
onMapFreeVar :: Segments -> DistEnv -> DistInputs -> SubExp -> VName -> VName -> Maybe (Builder GPU (VName, VName))
onMapFreeVar segments env inps w segments_per_elem v = do
  v_inp <- lookup v inps
  pure . fmap (v,) $ case v_inp of
    DistInputFree v' _ -> do
      letExp (baseString v <> "_rep_free_reg_inp") <=< segMap (Solo w) $ \(Solo i) -> do
        segment <- letSubExp "segment" =<< eIndex segments_per_elem [eSubExp i]
        subExpsRes . pure <$> (letSubExp "v" =<< eIndex v' [eSubExp segment])
    DistInput rt _ -> case resVar rt env of
      Irregular r ->
        letExp (baseString v <> "_rep_free_irreg_inp") <=< segMap (Solo w) $ \(Solo i) -> do
          segment <- letSubExp "segment" =<< eIndex segments_per_elem [eSubExp i]
          subExpsRes . pure <$> (letSubExp "v" =<< eIndex v [eSubExp segment])

onMapInputArr ::
  Segments ->
  DistEnv ->
  DistInputs ->
  SubExp ->
  VName ->
  Builder GPU VName
onMapInputArr segments env inps w arr =
  case lookup arr inps of
    Just v_inp ->
      case v_inp of
        DistInputFree vs _ ->
          letExp (baseString vs <> "_flat") . BasicOp $
            Reshape ReshapeArbitrary (Shape [w]) vs
        DistInput rt _ -> undefined
    Nothing -> do
      arr_row_t <- rowType <$> lookupType arr
      arr_rep <-
        letExp (baseString arr <> "_inp_rep") . BasicOp $
          Replicate (segmentsShape segments) (Var arr)
      letExp (baseString arr <> "_inp_rep_flat") . BasicOp $
        Reshape ReshapeArbitrary (Shape [w] <> arrayShape arr_row_t) arr_rep

transformMap ::
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Builder GPU (VName, VName, VName)
transformMap segments env inps pat w arrs map_lam = do
  ws <- elemArr segments env inps w
  (ws_flags, ws_offsets, ws_elems) <- doRepIota ws
  new_segment <- arraySize 0 <$> lookupType ws_elems
  arrs' <- mapM (onMapInputArr segments env inps new_segment) arrs
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe (onMapFreeVar segments env inps new_segment ws_elems) $
        namesToList (freeIn map_lam)
  free_ps <-
    mapM (newParam "free_p" . rowType <=< lookupType) replicated
  scope <- askScope
  let substs = M.fromList $ zip free_replicated $ map paramName free_ps
      map_lam' =
        (substituteNames substs map_lam)
          { lambdaParams = lambdaParams map_lam <> free_ps
          }
      distributed = distributeMap scope pat new_segment (arrs' <> replicated) map_lam'
      m = transformDistributed (NE.singleton new_segment) distributed
  traceM $ unlines ["inner map", prettyString distributed]
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
            transformMap segments env inps map_pat w arrs map_lam
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
          (ws_flags, ws_offsets, ws) <- transformMap segments env inps pat w arrs map_lam
          pure $ insertIrregulars ws ws_flags ws_offsets (zip (map distResTag res) $ patNames pat) env
    _ -> error $ "Unhandled Stm:\n" ++ prettyString stm

distResCerts :: DistEnv -> [DistInput] -> Certs
distResCerts env = Certs . map f
  where
    f (DistInputFree v _) = v
    f (DistInput rt _) = case resVar rt env of
      Regular v -> v
      Irregular {} -> error "resCerts: irregular"

transformDistributed :: Segments -> Distributed -> Builder GPU ()
transformDistributed segments (Distributed dstms resmap) = do
  env <- foldM (transformDistStm segments) mempty dstms
  forM_ (M.toList resmap) $ \(rt, (cs_inps, v, v_t)) ->
    certifying (distResCerts env cs_inps) $
      case resVar rt env of
        Regular v' -> letBindNames [v] $ BasicOp $ SubExp $ Var v'
        Irregular irreg -> do
          -- It might have an irregular representation, but we know
          -- that it is actually regular because it is a result.
          let shape = segmentsShape segments <> arrayShape v_t
          letBindNames [v] $
            BasicOp (Reshape ReshapeArbitrary shape (irregularElems irreg))

transformStm :: Scope SOACS -> Stm SOACS -> PassM (Stms GPU)
transformStm scope (Let pat _ (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form = do
      let distributed = distributeMap scope pat w arrs lam
          m = transformDistributed (NE.singleton w) distributed
      traceM $ prettyString distributed
      runReaderT (runBuilder_ m) scope
transformStm _ stm = pure $ oneStm $ soacsStmToGPU stm

transformStms :: Scope SOACS -> Stms SOACS -> PassM (Stms GPU)
transformStms scope stms =
  fold <$> traverse (transformStm (scope <> scopeOf stms)) stms

liftParam :: SubExp -> FParam SOACS -> PassM ([FParam GPU], ResRep)
liftParam w fparam =
  case declTypeOf fparam of
    Prim pt -> do
      p <-
        newParam
          (desc <> "_lifted")
          (arrayOf (Prim pt) (Shape [w]) Nonunique)
      pure ([p], Regular $ paramName p)
    Array pt shape u -> do
      num_elems <-
        newParam (desc <> "_num_elems") $ Prim int64
      offsets <-
        newParam (desc <> "_offsets") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      flags <-
        newParam (desc <> "_flags") $
          arrayOf (Prim Bool) (Shape [Var (paramName num_elems)]) Nonunique
      segments <-
        newParam (desc <> "_segments") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      elems <-
        newParam (desc <> "_elems") $
          arrayOf (Prim pt) (Shape [Var (paramName num_elems)]) u
      pure
        ( [num_elems, offsets, flags, segments, elems],
          Irregular $
            IrregularRep
              { irregularSegments = paramName segments,
                irregularFlags = paramName flags,
                irregularOffsets = paramName offsets,
                irregularElems = paramName elems
              }
        )
    Acc {} ->
      error "liftParam: Acc"
    Mem {} ->
      error "liftParam: Mem"
  where
    desc = baseString (paramName fparam)

-- Lifts a function return type.
-- TODO: Handle array (and maybe other) case(s).
liftRetType :: SubExp -> RetType SOACS -> RetType GPU
liftRetType w rettype =
  case rettype of
    Prim pt -> arrayOf (Prim pt) (Shape [Free w]) Nonunique
    _ -> undefined

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
  let inputs = do
        (p, i) <- zip fparams [0 ..]
        pure (paramName p, DistInput (ResTag i) (paramType p))
  let rettype' = map (liftRetType w) rettype
  -- Build a pattern of the body results.
  -- TODO: Include a case for constants.
  let result_pat = Pat $ map (\r -> case resSubExp r of Var v -> PatElem v ()) $ bodyResult body
  let distributed@(Distributed dstms resmap) =
        distributeBody const_scope result_pat (Var (paramName wp)) inputs body
  traceM $ prettyString distributed
  let segments = NE.singleton (Var $ paramName wp)
      env = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
  -- Build the new statements and get the modified DistEnv
  (env', stms) <-
    runReaderT (runBuilder $ foldM (transformDistStm segments) env dstms) $
      (const_scope <> scopeOfFParams (concat fparams'))
  -- Get the distributed results from `env'`
  -- TODO: Handle irregular representations and certs.
  let mkSubExpRes _ (Regular v) = SubExpRes {resCerts = mempty, resSubExp = Var v}
  let resultAssoc =
        M.elems $
          M.intersectionWith
            (\(_, v, _) (Regular vl) -> (v, vl))
            resmap
            (distResMap env')
  let Just result = mapM (\(SubExpRes cs (Var v)) -> SubExpRes cs . Var <$> lookup v resultAssoc) $ bodyResult body
  let name = funDefName fd <> "_lifted"
  pure $
    fd
      { funDefName = name,
        funDefBody = Body () stms result,
        funDefParams = wp : concat fparams',
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
  lifted_funs <- mapM (liftFunDef $ scopeOf (progConsts prog)) $ progFuns prog
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
