{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Tools
  (
    module Futhark.Construct

  , nonuniqueParams
  , redomapToMapAndReduce
  , scanomapToMapAndReduce
  , sequentialStreamWholeArray
  , singletonChunkRedLikeStreamLambda
  , extLambdaToLambda
  , partitionChunkedFoldParameters
  , partitionChunkedKernelLambdaParameters
  , partitionChunkedKernelFoldParameters

  , intraproceduralTransformation

  -- * Primitive expressions
  , module Futhark.Analysis.PrimExp.Convert
  )
where

import Control.Monad.Identity
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Monoid
import qualified Data.Map.Strict as M

import Futhark.Representation.AST
import Futhark.Representation.SOACS.SOAC
import Futhark.MonadFreshNames
import Futhark.Construct
import Futhark.Analysis.PrimExp.Convert

nonuniqueParams :: (MonadFreshNames m, Bindable lore, BinderOps lore) =>
                   [LParam lore] -> m ([LParam lore], Stms lore)
nonuniqueParams params =
  modifyNameSource $ runState $ fmap fst $ runBinderEmptyEnv $
  collectStms $ forM params $ \param ->
    if not $ primType $ paramType param then do
      param_name <- newVName $ baseString (paramName param) ++ "_nonunique"
      let param' = Param param_name $ paramType param
      localScope (scopeOfLParams [param']) $
        letBindNames_ [(paramName param,BindVar)] $
        BasicOp $ Copy $ paramName param'
      return param'
    else
      return param

-- | Turns a binding of a @redomap@ into two seperate bindings, a
-- @map@ binding and a @reduce@ binding (returned in that order).
--
-- Reuses the original pattern for the @reduce@, and creates a new
-- pattern with new 'Ident's for the result of the @map@.
--
-- Only handles a 'Pattern' with an empty 'patternContextElements'
redomapToMapAndReduce :: (MonadFreshNames m, Bindable lore,
                          ExpAttr lore ~ (), Op lore ~ SOAC lore) =>
                         Pattern lore
                      -> ( SubExp
                         , Commutativity
                         , LambdaT lore, LambdaT lore, [SubExp]
                         , [VName])
                      -> m (Stm lore, Stm lore)
redomapToMapAndReduce (Pattern [] patelems)
                      (w, comm, redlam, redmap_lam, accs, arrs) = do
  ((map_pat, newmap_lam), (red_pat, red_args)) <-
    splitScanOrRedomap patelems w redmap_lam accs
  let map_bnd = mkLet [] map_pat $
                Op $ Map w newmap_lam arrs
      red_bnd = Let red_pat (defAux ()) $
                Op $ Reduce w comm redlam red_args
  return (map_bnd, red_bnd)
redomapToMapAndReduce _ _ =
  error "redomapToMapAndReduce does not handle a non-empty 'patternContextElements'"

-- | Like 'redomapToMapAndReduce', but for 'Scanomap'.
scanomapToMapAndReduce :: (MonadFreshNames m, Bindable lore,
                          ExpAttr lore ~ (), Op lore ~ SOAC lore) =>
                         Pattern lore
                      -> ( SubExp
                         , LambdaT lore, LambdaT lore, [SubExp]
                         , [VName])
                      -> m (Stm lore, Stm lore)
scanomapToMapAndReduce (Pattern [] patelems)
                      (w, scanlam, scanmap_lam, accs, arrs) = do
  ((map_pat, newmap_lam), (scan_pat, scan_args)) <-
    splitScanOrRedomap patelems w scanmap_lam accs
  let map_bnd = mkLet [] map_pat $
                Op $ Map w newmap_lam arrs
      scan_bnd = Let scan_pat (defAux ()) $
                 Op $ Scan w scanlam scan_args
  return (map_bnd, scan_bnd)
scanomapToMapAndReduce _ _ =
  error "scanomapToMapAndReduce does not handle a non-empty 'patternContextElements'"

splitScanOrRedomap :: (Typed attr, MonadFreshNames m,
                       Bindable lore) =>
                      [PatElemT attr]
                   -> SubExp -> LambdaT lore -> [SubExp]
                   -> m (([(Ident, Bindage)], LambdaT lore),
                         (PatternT attr, [(SubExp, VName)]))
splitScanOrRedomap patelems w redmap_lam accs = do
  let (acc_patelems, arr_patelems) = splitAt (length accs) patelems
      (acc_ts, _arr_ts) = splitAt (length accs) $ lambdaReturnType redmap_lam
  map_accpat <- zipWithM accMapPatElem acc_patelems acc_ts
  map_arrpat <- mapM arrMapPatElem arr_patelems
  let map_pat = map_accpat ++ map_arrpat
      red_args = zip accs $ map (identName . fst) map_accpat
  return ((map_pat, newmap_lam),
          (Pattern [] acc_patelems, red_args))
  where
    accMapPatElem pe acc_t = do
      i <- newIdent (baseString (patElemName pe) ++ "_map_acc") $
           acc_t `arrayOfRow` w
      return (i, patElemBindage pe)
    arrMapPatElem pe =
      return (patElemIdent pe, patElemBindage pe)

    newmap_lam =
      let tobnd = take (length accs) $ map paramIdent $ lambdaParams redmap_lam
          params' = drop (length accs) $ lambdaParams redmap_lam
          bndaccs = zipWith (\i acc -> mkLet' []  [i] (BasicOp $ SubExp acc))
                            tobnd accs
          body = lambdaBody redmap_lam
          bnds' = stmsFromList bndaccs <> bodyStms body
          body' = body {bodyStms = bnds'}
      in redmap_lam { lambdaBody = body', lambdaParams = params' }

sequentialStreamWholeArrayStms :: Bindable lore =>
                                  SubExp -> [SubExp]
                               -> ExtLambdaT lore -> [VName]
                               -> (Stms lore, [SubExp])
sequentialStreamWholeArrayStms width accs lam arrs =
  let (chunk_param, acc_params, arr_params) =
        partitionChunkedFoldParameters (length accs) $ extLambdaParams lam
      chunk_bnd = mkLet' [] [paramIdent chunk_param] $ BasicOp $ SubExp width
      acc_bnds = [ mkLet' [] [paramIdent acc_param] $ BasicOp $ SubExp acc
                 | (acc_param, acc) <- zip acc_params accs ]
      arr_bnds = [ mkLet' [] [paramIdent arr_param] $
                   BasicOp $ Reshape (map DimCoercion $ arrayDims $ paramType arr_param) arr
                 | (arr_param, arr) <- zip arr_params arrs ]

  in (oneStm chunk_bnd <>
      stmsFromList acc_bnds <>
      stmsFromList arr_bnds <>
      bodyStms (extLambdaBody lam),

      bodyResult $ extLambdaBody lam)

sequentialStreamWholeArray :: (MonadBinder m, Bindable (Lore m)) =>
                              Pattern (Lore m)
                           -> SubExp -> [SubExp]
                           -> ExtLambdaT (Lore m) -> [VName]
                           -> m ()
sequentialStreamWholeArray pat width nes fun arrs = do
  let (body_bnds,res) = sequentialStreamWholeArrayStms width nes fun arrs
      reshapeRes t (Var v)
        | null (arrayDims t) = BasicOp $ SubExp $ Var v
        | otherwise          = shapeCoerce (arrayDims t) v
      reshapeRes _ se        = BasicOp $ SubExp se
      res_bnds =
        [ mkLet' [] [ident] $ reshapeRes (identType ident) se
        | (ident,se) <- zip (patternValueIdents pat) res]

  addStms body_bnds
  shapemap <- shapeMapping (patternValueTypes pat) <$> mapM subExpType res
  forM_ (M.toList shapemap) $ \(name,se) ->
    when (name `elem` patternContextNames pat) $
      addStm =<< mkLetNames' [name] (BasicOp $ SubExp se)
  addStms $ stmsFromList res_bnds

singletonChunkRedLikeStreamLambda :: (Bindable lore, MonadFreshNames m) =>
                                     [Type] -> ExtLambda lore -> m (Lambda lore)
singletonChunkRedLikeStreamLambda acc_ts lam = do
  -- The accumulator params are OK, but we need array params without
  -- the chunk part.
  let (chunk_param, acc_params, arr_params) =
        partitionChunkedFoldParameters (length acc_ts) $ extLambdaParams lam
  unchunked_arr_params <- forM arr_params $ \arr_param ->
    Param <$>
    newVName (baseString (paramName arr_param) <> "_unchunked") <*>
    pure (rowType $ paramType arr_param)
  let chunk_name = paramName chunk_param
      chunk_bnd = mkLet' [] [paramIdent chunk_param] $
                  BasicOp $ SubExp $ intConst Int32 1
      arr_bnds = stmsFromList
                 [ mkLet' [] [paramIdent arr_param] $
                   BasicOp $ Replicate (Shape [Var chunk_name]) $
                   Var $ paramName unchunked_arr_param |
                   (arr_param, unchunked_arr_param) <-
                     zip arr_params unchunked_arr_params ]
      unchunked_body = chunk_bnd `insertStm` (arr_bnds `insertStms` extLambdaBody lam)
  return Lambda { lambdaBody = unchunked_body
                , lambdaParams = acc_params <> unchunked_arr_params
                , lambdaReturnType = acc_ts
                }

-- | Convert an 'ExtLambda' to a 'Lambda' if the return type is
-- non-existential anyway.
extLambdaToLambda :: ExtLambda lore -> Maybe (Lambda lore)
extLambdaToLambda lam = do
  ret <- hasStaticShapes $ extLambdaReturnType lam
  return Lambda { lambdaReturnType = ret
                , lambdaBody = extLambdaBody lam
                , lambdaParams = extLambdaParams lam
                }

partitionChunkedFoldParameters :: Int -> [Param attr]
                               -> (Param attr, [Param attr], [Param attr])
partitionChunkedFoldParameters _ [] =
  error "partitionChunkedFoldParameters: lambda takes no parameters"
partitionChunkedFoldParameters num_accs (chunk_param : params) =
  let (acc_params, arr_params) = splitAt num_accs params
  in (chunk_param, acc_params, arr_params)

partitionChunkedKernelFoldParameters :: Int -> [Param attr]
                                     -> (VName, Param attr, [Param attr], [Param attr])
partitionChunkedKernelFoldParameters num_accs (i_param : chunk_param : params) =
  let (acc_params, arr_params) = splitAt num_accs params
  in (paramName i_param, chunk_param, acc_params, arr_params)
partitionChunkedKernelFoldParameters _ _ =
  error "partitionChunkedKernelFoldParameters: lambda takes too few parameters"

partitionChunkedKernelLambdaParameters :: [Param attr]
                                       -> (VName, Param attr, [Param attr])
partitionChunkedKernelLambdaParameters (i_param : chunk_param : params) =
  (paramName i_param, chunk_param, params)
partitionChunkedKernelLambdaParameters _ =
  error "partitionChunkedKernelLambdaParameters: lambda takes too few parameters"

intraproceduralTransformation :: MonadFreshNames m =>
                                 (FunDef fromlore -> State VNameSource (FunDef tolore))
                              -> Prog fromlore -> m (Prog tolore)
intraproceduralTransformation ft prog =
  modifyNameSource $ \src ->
  let (funs, srcs) = unzip $ parMap rseq (onFunction src) (progFunctions prog)
  in (Prog funs, mconcat srcs)
  where onFunction src f = runState (ft f) src
