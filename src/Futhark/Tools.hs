{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Tools
  (
    module Futhark.Construct

  , nonuniqueParams
  , redomapToMapAndReduce
  , scanomapToMapAndScan
  , dissectScrema
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

import Futhark.Representation.AST
import Futhark.Representation.SOACS.SOAC
import Futhark.MonadFreshNames
import Futhark.Construct
import Futhark.Analysis.PrimExp.Convert
import Futhark.Util

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
  let map_bnd = mkLet [] map_pat $ Op $ Screma w (mapSOAC map_lam) arrs
      (nes, red_arrs) = unzip red_args
  red_bnd <- Let red_pat (defAux ()) . Op <$>
             (Screma w <$> reduceSOAC comm redlam nes <*> pure red_arrs)
  return (map_bnd, red_bnd)
redomapToMapAndReduce _ _ =
  error "redomapToMapAndReduce does not handle a non-empty 'patternContextElements'"

-- | Like 'redomapToMapAndReduce', but for 'Scanomap'.
scanomapToMapAndScan :: (MonadFreshNames m, Bindable lore,
                          ExpAttr lore ~ (), Op lore ~ SOAC lore) =>
                        Pattern lore
                     -> ( SubExp
                        , LambdaT lore, LambdaT lore, [SubExp]
                        , [VName])
                     -> m (Stm lore, Stm lore)
scanomapToMapAndScan (Pattern [] patelems) (w, scanlam, map_lam, accs, arrs) = do
  (map_pat, scan_pat, scan_args) <-
    splitScanOrRedomap patelems w map_lam accs
  let map_bnd = mkLet [] map_pat $ Op $ Screma w (mapSOAC map_lam) arrs
      (nes, scan_arrs) = unzip scan_args
  scan_bnd <- Let scan_pat (defAux ()) . Op <$>
              (Screma w <$> scanSOAC scanlam nes <*> pure scan_arrs)
  return (map_bnd, scan_bnd)
scanomapToMapAndScan _ _ =
  error "scanomapToMapAndScan does not handle a non-empty 'patternContextElements'"

splitScanOrRedomap :: (Typed attr, MonadFreshNames m) =>
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

-- | Turn a Screma into simpler Scremas that are all simple scans,
-- reduces, and maps.  This is used to handle Scremas that are so
-- complicated that we cannot directly generate efficient parallel
-- code for them.  In essense, what happens is the opposite of
-- horisontal fusion.
dissectScrema :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m),
                    Bindable (Lore m)) =>
                   Pattern (Lore m) -> SubExp -> ScremaForm (Lore m) -> [VName]
                -> m ()
dissectScrema pat w (ScremaForm (scan_lam, scan_nes)
                                    (comm, red_lam, red_nes)
                                    map_lam) arrs = do
  let (scan_res, red_res, map_res) = splitAt3 (length scan_nes) (length red_nes) $
                                     patternNames pat
  -- First we perform the Map, then we perform the Reduce, and finally
  -- the Scan.
  to_scan <- replicateM (length scan_nes) $ newVName "to_scan"
  to_red <- replicateM (length red_nes) $ newVName "to_red"
  letBindNames_ (to_scan <> to_red <> map_res) $ Op $ Screma w (mapSOAC map_lam) arrs

  reduce <- reduceSOAC comm red_lam red_nes
  letBindNames_ red_res $ Op $ Screma w reduce to_red

  scan <- scanSOAC scan_lam scan_nes
  letBindNames_ scan_res $ Op $ Screma w scan to_scan

sequentialStreamWholeArray :: (MonadBinder m, Bindable (Lore m)) =>
                              Pattern (Lore m)
                           -> SubExp -> [SubExp]
                           -> LambdaT (Lore m) -> [VName]
                           -> m ()
sequentialStreamWholeArray pat w nes lam arrs = do
  -- We just set the chunksize to w and inline the lambda body.  There
  -- is no difference between parallel and sequential streams here.
  let (chunk_size_param, fold_params, arr_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam

  -- The chunk size is the full size of the array.
  letBindNames_ [paramName chunk_size_param] $ BasicOp $ SubExp w

  -- The accumulator parameters are initialised to the neutral element.
  forM_ (zip fold_params nes) $ \(p, ne) ->
    letBindNames [paramName p] $ BasicOp $ SubExp ne

  -- Finally, the array parameters are set to the arrays (but reshaped
  -- to make the types work out; this will be simplified rapidly).
  forM_ (zip arr_params arrs) $ \(p, arr) ->
    letBindNames [paramName p] $ BasicOp $
      Reshape (map DimCoercion $ arrayDims $ paramType p) arr

  -- Then we just inline the lambda body.
  mapM_ addStm $ bodyStms $ lambdaBody lam

  -- The number of results in the body matches exactly the size (and
  -- order) of 'pat', so we bind them up here, again with a reshape to
  -- make the types work out.
  forM_ (zip (patternElements pat) $ bodyResult $ lambdaBody lam) $ \(pe, se) ->
    case (arrayDims $ patElemType pe, se) of
      (dims, Var v)
        | not $ null dims ->
            letBindNames_ [patElemName pe] $ BasicOp $ Reshape (map DimCoercion dims) v
      _ -> letBindNames_ [patElemName pe] $ BasicOp $ SubExp se

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
