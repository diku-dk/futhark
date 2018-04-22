{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Tools
  (
    module Futhark.Construct

  , nonuniqueParams
  , redomapToMapAndReduce
  , scanomapToMapAndReduce
  , sequentialStreamWholeArray
  , partitionChunkedFoldParameters
  , partitionChunkedKernelLambdaParameters
  , partitionChunkedKernelFoldParameters

  -- * Primitive expressions
  , module Futhark.Analysis.PrimExp.Convert
  )
where

import Control.Monad.Identity
import Data.Semigroup ((<>))
import qualified Data.Map.Strict as M

import Futhark.Representation.AST
import Futhark.Representation.SOACS.SOAC
import Futhark.MonadFreshNames
import Futhark.Construct
import Futhark.Analysis.PrimExp.Convert

nonuniqueParams :: (MonadFreshNames m, Bindable lore, HasScope lore m, BinderOps lore) =>
                   [LParam lore] -> m ([LParam lore], Stms lore)
nonuniqueParams params = runBinder $ forM params $ \param ->
    if not $ primType $ paramType param then do
      param_name <- newVName $ baseString (paramName param) ++ "_nonunique"
      let param' = Param param_name $ paramType param
      localScope (scopeOfLParams [param']) $
        letBindNames_ [paramName param] $ BasicOp $ Copy $ paramName param'
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
                      (w, comm, redlam, map_lam, accs, arrs) = do
  (map_pat, red_pat, red_args) <-
    splitScanOrRedomap patelems w map_lam accs
  let map_bnd = mkLet [] map_pat $ Op $ Map w map_lam arrs
      red_bnd = Let red_pat (defAux ()) $ Op $ Reduce w comm redlam red_args
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
                      (w, scanlam, map_lam, accs, arrs) = do
  (map_pat, scan_pat, scan_args) <-
    splitScanOrRedomap patelems w map_lam accs
  let map_bnd = mkLet [] map_pat $ Op $ Map w map_lam arrs
      scan_bnd = Let scan_pat (defAux ()) $ Op $ Scan w scanlam scan_args
  return (map_bnd, scan_bnd)
scanomapToMapAndReduce _ _ =
  error "scanomapToMapAndReduce does not handle a non-empty 'patternContextElements'"

splitScanOrRedomap :: (Typed attr, MonadFreshNames m,
                       Bindable lore) =>
                      [PatElemT attr]
                   -> SubExp -> LambdaT lore -> [SubExp]
                   -> m ([Ident], PatternT attr, [(SubExp, VName)])
splitScanOrRedomap patelems w map_lam accs = do
  let (acc_patelems, arr_patelems) = splitAt (length accs) patelems
      (acc_ts, _arr_ts) = splitAt (length accs) $ lambdaReturnType map_lam
  map_accpat <- zipWithM accMapPatElem acc_patelems acc_ts
  map_arrpat <- mapM arrMapPatElem arr_patelems
  let map_pat = map_accpat ++ map_arrpat
      red_args = zip accs $ map identName map_accpat
  return (map_pat, Pattern [] acc_patelems, red_args)
  where
    accMapPatElem pe acc_t =
      newIdent (baseString (patElemName pe) ++ "_map_acc") $ acc_t `arrayOfRow` w
    arrMapPatElem = return . patElemIdent

sequentialStreamWholeArrayStms :: Bindable lore =>
                                  SubExp -> [SubExp]
                               -> LambdaT lore -> [VName]
                               -> (Stms lore, [SubExp])
sequentialStreamWholeArrayStms width accs lam arrs =
  let (chunk_param, acc_params, arr_params) =
        partitionChunkedFoldParameters (length accs) $ lambdaParams lam
      chunk_bnd = mkLet [] [paramIdent chunk_param] $ BasicOp $ SubExp width
      acc_bnds = [ mkLet [] [paramIdent acc_param] $ BasicOp $ SubExp acc
                 | (acc_param, acc) <- zip acc_params accs ]
      arr_bnds = [ mkLet [] [paramIdent arr_param] $
                   BasicOp $ Reshape (map DimCoercion $ arrayDims $ paramType arr_param) arr
                 | (arr_param, arr) <- zip arr_params arrs ]

  in (oneStm chunk_bnd <>
      stmsFromList acc_bnds <>
      stmsFromList arr_bnds <>
      bodyStms (lambdaBody lam),

      bodyResult $ lambdaBody lam)

sequentialStreamWholeArray :: (MonadBinder m, Bindable (Lore m)) =>
                              Pattern (Lore m)
                           -> SubExp -> [SubExp]
                           -> LambdaT (Lore m) -> [VName]
                           -> m ()
sequentialStreamWholeArray pat width nes fun arrs = do
  let (body_bnds,res) = sequentialStreamWholeArrayStms width nes fun arrs
      reshapeRes t (Var v)
        | null (arrayDims t) = BasicOp $ SubExp $ Var v
        | otherwise          = shapeCoerce (arrayDims t) v
      reshapeRes _ se        = BasicOp $ SubExp se
      res_bnds =
        [ mkLet [] [ident] $ reshapeRes (identType ident) se
        | (ident,se) <- zip (patternValueIdents pat) res]

  addStms body_bnds
  shapemap <- shapeMapping (patternValueTypes pat) <$> mapM subExpType res
  forM_ (M.toList shapemap) $ \(name,se) ->
    when (name `elem` patternContextNames pat) $
      addStm =<< mkLetNames [name] (BasicOp $ SubExp se)
  addStms $ stmsFromList res_bnds

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
