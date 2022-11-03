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
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
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
    iregularElems :: VName
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

instance Monoid DistEnv where
  mempty = DistEnv mempty

instance Semigroup DistEnv where
  DistEnv x <> DistEnv y = DistEnv (x <> y)

resVar :: ResTag -> DistEnv -> ResRep
resVar rt env = fromMaybe bad $ M.lookup rt $ distResMap env
  where
    bad = error $ "resVar: unknown tag: " ++ show rt

flagsAndElems :: DistEnv -> [DistInput] -> (Maybe (VName, VName), [VName])
flagsAndElems env [] = (Nothing, [])
flagsAndElems env (DistInputFree v _ : vs) =
  second (v :) $ flagsAndElems env vs
flagsAndElems env (DistInput rt _ : vs) =
  case resVar rt env of
    Regular v' ->
      second (v' :) $ flagsAndElems env vs
    Irregular (IrregularRep _ flags offsets elems) ->
      bimap (mplus $ Just (flags, offsets)) (elems :) $ flagsAndElems env vs

type Segments = NE.NonEmpty SubExp

segMap1 :: Segments -> ([SubExp] -> Builder GPU Result) -> Builder GPU (Exp GPU)
segMap1 segments f = do
  gtids <- replicateM (length segments) (newVName "gtid")
  space <- mkSegSpace $ zip gtids $ toList segments
  ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    res <- f $ map Var gtids
    ts <- mapM (subExpType . resSubExp) res
    let resToRes (SubExpRes cs se) = Returns ResultMaySimplify cs se
    pure (map resToRes res, ts)
  let kbody = KernelBody () stms res
  pure $ Op $ SegOp $ SegMap (SegThread SegNoVirt Nothing) space ts kbody
  where
    mkResult (SubExpRes cs se) = Returns ResultMaySimplify cs se

readInput :: Segments -> DistEnv -> [SubExp] -> [(VName, DistInput)] -> SubExp -> Builder GPU SubExp
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

readInputs :: Segments -> DistEnv -> [SubExp] -> [(VName, DistInput)] -> Builder GPU ()
readInputs segments env is = mapM_ onInput
  where
    onInput (v, DistInputFree arr _) =
      letBindNames [v] =<< eIndex arr (map eSubExp is)
    onInput (v, DistInput rt t) =
      case M.lookup rt $ distResMap env of
        Just (Regular arr) ->
          letBindNames [v] =<< eIndex arr (map eSubExp is)
        Just (Irregular (IrregularRep _ flags offsets elems)) -> do
          offset <- letSubExp "offset" =<< eIndex offsets (map eSubExp is)
          num_elems <- letSubExp "num_elems" =<< toExp (product $ map pe64 $ arrayDims t)
          let slice = Slice [DimSlice offset num_elems (intConst Int64 1)]
          letBindNames [v] $ BasicOp $ Index elems slice
        Nothing -> error $ "readInputs: " <> show rt

transformScalarStms ::
  Segments ->
  DistEnv ->
  [(VName, DistInput)] ->
  [DistResult] ->
  Stms SOACS ->
  [VName] ->
  Builder GPU DistEnv
transformScalarStms segments env inps distres stms res = do
  vs <- letTupExp "scalar_dist" <=< renameExp <=< segMap1 segments $ \is -> do
    readInputs segments env is inps
    addStms $ fmap soacsStmToGPU stms
    pure $ subExpsRes $ map Var res
  pure $ insertReps (zip (map distResTag distres) $ map Regular vs) env

transformScalarStm ::
  Segments ->
  DistEnv ->
  [(VName, DistInput)] ->
  [DistResult] ->
  Stm SOACS ->
  Builder GPU DistEnv
transformScalarStm segments env inps res stm =
  transformScalarStms segments env inps res (oneStm stm) (patNames (stmPat stm))

transformDistBasicOp ::
  Segments ->
  DistEnv ->
  ( [(VName, DistInput)],
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
        Just (DistInput rt_in _) <- lookup v inps -> do
          -- TODO: actually insert opaques
          pure $ insertRep (distResTag res) (resVar rt_in env) env
      | otherwise ->
          scalarCase
    Index arr slice
      | null $ sliceDims slice ->
          scalarCase
      | Just rep <- lookup arr inps ->
          case rep of
            DistInput arr_rt _ ->
              case resVar arr_rt env of
                Irregular (IrregularRep arr_ns arr_flags arr_offsets arr_elems) -> do
                  ns <- letExp "slice_sizes" <=< segMap1 segments $ \is -> do
                    slice_ns <- mapM (readInput segments env is inps) $ sliceDims slice
                    fmap varsRes . letTupExp "n" <=< toExp $ product $ map pe64 slice_ns
                  offsets <- doPrefixSum ns
                  m <- letSubExp "total_elems" =<< eLast offsets
                  flags <- genFlags m offsets
                  elems <- letExp "elems" <=< segMap1 (NE.singleton m) $ \is ->
                    fmap (subExpsRes . pure) . letSubExp "v"
                      =<< eIndex arr_elems (map eSubExp is)
                  let rep = Irregular $ IrregularRep ns flags offsets elems
                  pure $ insertRep (distResTag res) rep env
    Iota (Var n) x s Int64
      | Just (DistInputFree ns _) <- lookup n inps -> do
          (flags, offsets, elems) <- doSegIota ns
          let rep = Irregular $ IrregularRep ns flags offsets elems
          pure $ insertRep (distResTag res) rep env
    _ -> error $ "Unhandled BasicOp:\n" ++ prettyString e
  where
    scalarCase =
      transformScalarStm segments env inps [res] $
        Let (Pat [pe]) aux (BasicOp e)

transformDistStm :: Segments -> DistEnv -> DistStm -> Builder GPU DistEnv
transformDistStm segments env (DistStm inps res stm) = do
  case stm of
    Let pat aux (BasicOp e) -> do
      let ~[res'] = res
          ~[pe] = patElems pat
      transformDistBasicOp segments env (inps, res', pe, stmAux stm, e)
    Let _ _ (Op (Screma _ arrs form))
      | Just reds <- isReduceSOAC form,
        Just arrs' <- mapM (`lookup` inps) arrs,
        (Just (flags, offsets), elems) <- flagsAndElems env arrs' -> do
          elems' <- genSegRed flags offsets elems $ singleReduce reds
          pure $ insertReps (zip (map distResTag res) (map Regular elems')) env
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
  forM_ (M.toList resmap) $ \(rt, (cs_inps, v)) ->
    certifying (distResCerts env cs_inps) $
      case resVar rt env of
        Regular v' -> letBindNames [v] $ BasicOp $ SubExp $ Var v'
        Irregular {} -> error $ "Result is irregular: " ++ prettyString v

transformStm :: Scope SOACS -> Stm SOACS -> PassM (Stms GPU)
transformStm scope (Let pat _ (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form = do
      let distributed = distributeMap scope pat w arrs lam
          m = transformDistributed (NE.singleton w) distributed
      traceM $ prettyString distributed
      runReaderT (runBuilder_ m) scope
transformStm scope stm = pure $ oneStm $ soacsStmToGPU stm

transformStms :: Scope SOACS -> Stms SOACS -> PassM (Stms GPU)
transformStms scope stms =
  fold <$> traverse (transformStm (scope <> scopeOf stms)) stms

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
  pure $ prog {progConsts = consts', progFuns = flatteningBuiltins <> funs'}

-- | Transform a SOACS program to a GPU program, using flattening.
flattenSOACs :: Pass SOACS GPU
flattenSOACs =
  Pass
    { passName = "flatten",
      passDescription = "Perform full flattening",
      passFunction = transformProg
    }
{-# NOINLINE flattenSOACs #-}
