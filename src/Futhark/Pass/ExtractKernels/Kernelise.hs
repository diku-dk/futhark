{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Sequentialise to kernel statements.
module Futhark.Pass.ExtractKernels.Kernelise
       ( transformStm
       , transformStms
       , transformBody
       , transformLambda
       , mapIsh

       , groupStreamMapAccumL
       )
       where

import Control.Monad
import Data.Monoid
import qualified Data.Set as S

import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Representation.SOACS
import qualified Futhark.Representation.Kernels as Out
import Futhark.MonadFreshNames
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Tools

type Transformer m = (MonadBinder m,
                      Lore m ~ Out.InKernel,
                      LocalScope (Lore m) m)

transformStms :: Transformer m => Stms SOACS -> m ()
transformStms = mapM_ transformStm . stmsToList

transformStm :: Transformer m => Stm -> m ()

transformStm (Let pat aux (Op (Redomap w _ _ fold_lam nes arrs)))
  -- No map-out part
  | patternSize pat == length nes = do
  chunk_size <- newVName "chunk_size"
  chunk_offset <- newVName "chunk_offset"
  let arr_idents = drop (length nes) $ patternIdents pat
      (fold_acc_params, fold_elem_params) =
        splitAt (length nes) $ lambdaParams fold_lam
  arr_chunk_params <- mapM (mkArrChunkParam $ Var chunk_size) fold_elem_params

  map_arr_params <- forM arr_idents $ \arr ->
    newParam (baseString (identName arr) <> "_in") $
    setOuterSize (identType arr) (Var chunk_size)

  fold_acc_params' <- forM fold_acc_params $ \p ->
    newParam (baseString $ paramName p) $ paramType p

  let param_scope =
        scopeOfLParams $ fold_acc_params' ++ arr_chunk_params ++ map_arr_params

  redomap_pes <- forM (patternValueElements pat) $ \pe ->
    PatElem <$> newVName (baseString $ patElemName pe) <*> pure (patElemType pe)

  redomap_kstms <- collectStms_ $ localScope param_scope $ do
    fold_lam' <- transformLambda fold_lam
    groupStreamMapAccumL redomap_pes (Var chunk_size) fold_lam'
      (map (Var . paramName) fold_acc_params') (map paramName arr_chunk_params)

  let stream_kbody = Out.Body () redomap_kstms $
                     map (Var . patElemName) redomap_pes
      stream_lam = Out.GroupStreamLambda { Out.groupStreamChunkSize = chunk_size
                                         , Out.groupStreamChunkOffset = chunk_offset
                                         , Out.groupStreamAccParams = fold_acc_params'
                                         , Out.groupStreamArrParams = arr_chunk_params
                                         , Out.groupStreamLambdaBody = stream_kbody
                                         }

  -- Tricky reverse logic: we have to copy all the initial
  -- accumulators that were *not* consumed in the original lambda, as
  -- a GroupStream will write to its accumulators.
  let consumed = consumedByLambda $ Alias.analyseLambda fold_lam
  nes' <- forM (zip fold_acc_params nes) $ \(p,e) ->
    case e of
      Var v | not $ paramName p `S.member` consumed,
              not $ primType $ paramType p ->
                letSubExp "groupstream_mapaccum_copy" $ BasicOp $ Copy v
      _ -> return e

  addStm $ Let pat aux $ Op $ Out.GroupStream w w stream_lam nes' arrs

  where mkArrChunkParam chunk_size arr_param =
          newParam (baseString (paramName arr_param) <> "_chunk") $
            arrayOfRow (paramType arr_param) chunk_size

transformStm (Let pat aux (Op (Stream w (Sequential accs) fold_lam arrs))) = do
  let ret = lambdaReturnType fold_lam
  -- Sequential streams can be transformed easily to a GroupStream.
  -- But we have to create accumulator parameters for mapout.

  chunk_offset <- newVName "streamseq_chunk_offset"

  let (chunk_size_param, fold_acc_params, arr_chunk_params) =
        partitionChunkedFoldParameters (length accs) $ lambdaParams fold_lam
      chunk_size = paramName chunk_size_param
      map_arr_tps = map (`setOuterSize` w) $ drop (length accs) ret

  mapout_arrs <- resultArray map_arr_tps
  outarr_params <- forM map_arr_tps $ \map_arr_t ->
    Param <$> newVName "redomap_outarr" <*> pure map_arr_t

  lam_body <- localScope (castScope (scopeOf fold_lam) <>
                          scopeOfLParams outarr_params) $ insertStmsM $ do
    res <- bodyBind =<< transformBody (lambdaBody fold_lam)
    -- Some results are to be returned; others to be copied into the
    -- map-out arrays.
    let (acc_res, mapout_res) = splitAt (length accs) res

    mapout_res' <- forM (zip outarr_params mapout_res) $ \(p, r) ->
      let slice = fullSlice (paramType p)
                  [DimSlice (Var chunk_offset) (Var chunk_size) (constant (1::Int32))]
      in fmap Var $ letInPlace "mapout_res" (paramName p) slice $ BasicOp $ SubExp r

    return $ resultBody $ acc_res++mapout_res'

  let stream_lam = Out.GroupStreamLambda
                   { Out.groupStreamChunkSize = chunk_size
                   , Out.groupStreamChunkOffset = chunk_offset
                   , Out.groupStreamAccParams = fold_acc_params ++ outarr_params
                   , Out.groupStreamArrParams = arr_chunk_params
                   , Out.groupStreamLambdaBody = lam_body
                   }

  -- Only copy the accs that were not consumed in the original stream.
  let consumed = consumedByLambda $ Alias.analyseLambda fold_lam
  accs' <- forM (zip fold_acc_params accs) $ \(p, acc) ->
    case acc of
      Var v | not $ paramName p `S.member` consumed,
              not $ primType $ paramType p ->
                letSubExp "streamseq_acc_copy" $ BasicOp $ Copy v
      _     -> return acc

  addStm $ Let pat aux $ Op $
    Out.GroupStream w w stream_lam (accs'++map Var mapout_arrs) arrs

transformStm (Let pat aux (DoLoop [] val (ForLoop i Int32 bound []) body)) = do
  dummy_chunk_size <- newVName "dummy_chunk_size"
  body' <- localScope (scopeOfFParams (map fst val)) $ transformBody body
  let lam = Out.GroupStreamLambda { Out.groupStreamChunkSize = dummy_chunk_size
                                  , Out.groupStreamChunkOffset = i
                                  , Out.groupStreamAccParams = map (fmap fromDecl . fst) val
                                  , Out.groupStreamArrParams = []
                                  , Out.groupStreamLambdaBody = body' }

  -- Copy the initial merge parameters that were not unique in the
  -- original stream.
  accs' <- forM val $ \(p, initial) ->
    case initial of
      Var v | not $ unique $ paramDeclType p,
              not $ primType $ paramDeclType p ->
                letSubExp "streamseq_merge_copy" $ BasicOp $ Copy v
      _     -> return initial

  addStm $ Let pat aux $ Op $ Out.GroupStream
    bound (constant (1::Int32)) lam accs' []

transformStm (Let pat aux (If cond tb fb ts)) = do
  tb' <- transformBody tb
  fb' <- transformBody fb
  addStm $ Let pat aux $ If cond tb' fb' ts

transformStm bnd =
  FOT.transformStmRecursively bnd

transformBody :: Transformer m => Body -> m (Out.Body Out.InKernel)
transformBody (Body attr bnds res) = do
  stms <- collectStms_ $ transformStms bnds
  return $ Out.Body attr stms res

transformLambda :: (MonadFreshNames m,
                    HasScope lore m,
                    SameScope lore Out.InKernel) =>
                   Lambda -> m (Out.Lambda Out.InKernel)
transformLambda (Lambda params body rettype) = do
  body' <- runBodyBinder $
           localScope (scopeOfLParams params) $
           transformBody body
  return $ Lambda params body' rettype

groupStreamMapAccumL :: Transformer m =>
                        [Out.PatElem Out.InKernel]
                     -> SubExp
                     -> Out.Lambda Out.InKernel
                     -> [SubExp]
                     -> [VName]
                     -> m ()
groupStreamMapAccumL pes w fold_lam accexps arrexps = do
  let acc_num     = length accexps
      res_tps     = lambdaReturnType fold_lam
      map_arr_tps = drop acc_num res_tps

  let fold_lam' = fold_lam { lambdaParams = take acc_num $ lambdaParams fold_lam }
      fold_lam_aliases = Alias.analyseLambda fold_lam'

  mapout_arrs <- resultArray [ arrayOf t (Shape [w]) NoUniqueness
                             | t <- map_arr_tps ]

  (merge, i, redomap_loop) <-
    FOT.doLoopMapAccumL' w fold_lam_aliases accexps [] mapout_arrs

  -- HACK: we manually inject the indexing here.
  dummy_chunk_size <- newVName "groupstream_mapaccum_dummy_chunk_size"
  let arr_params = drop acc_num $ lambdaParams fold_lam
  arr_params_chunked <- forM arr_params $ \arr_param ->
    newParam (baseString (paramName arr_param) <> "_chunked") $
    paramType arr_param `arrayOfRow` Var dummy_chunk_size
  let index_bnds = do
        (p, arr, arr_t) <- zip3 arr_params (map paramName arr_params_chunked)
                           (map paramType arr_params_chunked)
        return $ mkLet [] [paramIdent p] $
          BasicOp $ Index arr $ fullSlice arr_t [DimFix $ constant (0::Int32)]

  let redomap_kbody = stmsFromList index_bnds `insertStms` redomap_loop
      acc_params = map (fmap fromDecl . fst) merge
      stream_lam = Out.GroupStreamLambda { Out.groupStreamChunkSize = dummy_chunk_size
                                         , Out.groupStreamChunkOffset = i
                                         , Out.groupStreamAccParams = acc_params
                                         , Out.groupStreamArrParams = arr_params_chunked
                                         , Out.groupStreamLambdaBody = redomap_kbody
                                         }

  letBind_ (Pattern [] pes) $ Op $
    Out.GroupStream w (constant (1::Int32)) stream_lam (accexps++map Var mapout_arrs) arrexps

resultArray :: MonadBinder m => [Type] -> m [VName]
resultArray = mapM oneArray
  where oneArray t = letExp "result" $ BasicOp $ Scratch (elemType t) (arrayDims t)

mapIsh :: Transformer m =>
          Pattern
       -> Certificates
       -> SubExp
       -> [LParam]
       -> Out.Body Out.InKernel
       -> [VName]
       -> m ()
mapIsh pat cs w params (Out.Body () kstms kres) arrs = do
  i <- newVName "i"

  outarrs <- resultArray $ patternTypes pat

  outarr_params <- forM (patternElements pat) $ \pe ->
    newParam (baseString (patElemName pe) <> "_out") $
    patElemType pe

  dummy_chunk_size <- newVName "dummy_chunk_size"
  params_chunked <- forM params $ \param ->
    newParam (baseString (paramName param) <> "_chunked") $
    paramType param `arrayOfRow` Var dummy_chunk_size

  (outarr_params_new, write_elems) <-
    fmap unzip $ forM (zip outarr_params kres) $ \(outarr_param, se) -> do
      outarr_param_new <- newParam' (<>"_new") outarr_param
      return (outarr_param_new,
              mkLet [] [paramIdent outarr_param_new] $ BasicOp $
               Update (paramName outarr_param)
               (fullSlice (paramType outarr_param) [DimFix $ Var i]) se)

  let index_stms = do
        (p, arr, arr_t) <- zip3 params (map paramName params_chunked) $
                           map paramType params_chunked
        return $ mkLet [] [paramIdent p] $
          BasicOp $ Index arr $ fullSlice arr_t [DimFix $ constant (0::Int32)]
      kbody' = Out.Body () (stmsFromList index_stms <> kstms <> stmsFromList write_elems) $
               map (Var . paramName) outarr_params_new

  let stream_lam = Out.GroupStreamLambda { Out.groupStreamChunkSize = dummy_chunk_size
                                         , Out.groupStreamChunkOffset = i
                                         , Out.groupStreamAccParams = outarr_params
                                         , Out.groupStreamArrParams = params_chunked
                                         , Out.groupStreamLambdaBody = kbody'
                                         }
  certifying cs $ addStm $ Let pat (StmAux cs ()) $
    Op $ Out.GroupStream w (constant (1::Int32)) stream_lam (map Var outarrs) arrs
