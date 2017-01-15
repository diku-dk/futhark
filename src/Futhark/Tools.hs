{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Tools
  (
    module Futhark.Construct

  , nonuniqueParams
  , redomapToMapAndReduce
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

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Monoid
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.AST
import Futhark.Representation.SOACS.SOAC
import Futhark.MonadFreshNames
import Futhark.Construct
import Futhark.Analysis.PrimExp.Convert

nonuniqueParams :: (MonadFreshNames m, Bindable lore) =>
                   [LParam lore] -> m ([LParam lore], [Stm lore])
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
-- pattern with new 'Ident's for the result of the @map@. Does /not/
-- add the new idents to the 'Scope'.
--
-- Only handles a 'Pattern' with an empty 'patternContextElements'
redomapToMapAndReduce :: (MonadFreshNames m, Bindable lore, Op lore ~ SOAC lore) =>
                         Pattern lore -> ExpAttr lore
                      -> ( Certificates, SubExp
                         , Commutativity
                         , LambdaT lore, LambdaT lore, [SubExp]
                         , [VName])
                      -> m (Stm lore, Stm lore)
redomapToMapAndReduce (Pattern [] patelems) lore
                      (certs, outersz, comm, redlam, redmap_lam, accs, arrs) = do
  let (acc_patelems, arr_patelems) = splitAt (length accs) patelems
  map_accpat <- mapM accMapPatElem acc_patelems
  map_arrpat <- mapM arrMapPatElem arr_patelems
  let map_pat = map_accpat ++ map_arrpat
      map_bnd = mkLet [] map_pat $
                Op $ Map certs outersz newmap_lam arrs
      red_args = zip accs $ map (identName . fst) map_accpat
      red_bnd = Let (Pattern [] patelems) lore $
                Op $ Reduce certs outersz comm redlam red_args
  return (map_bnd, red_bnd)
  where
    accMapPatElem pe = do
      let (Ident vn tp) = patElemIdent pe
      let tp' = arrayOfRow tp outersz
      i <- newIdent (baseString vn ++ "_map_acc") tp'
      return (i, patElemBindage pe)
    arrMapPatElem pe =
      return (patElemIdent pe, patElemBindage pe)

    newmap_lam =
      let tobnd = take (length accs) $ map paramIdent $ lambdaParams redmap_lam
          params' = drop (length accs) $ lambdaParams redmap_lam
          bndaccs = zipWith (\i acc -> mkLet' []  [i] (BasicOp $ SubExp acc))
                            tobnd accs
          body = lambdaBody redmap_lam
          bnds' = bndaccs ++ bodyStms body
          body' = body {bodyStms = bnds'}
      in redmap_lam { lambdaBody = body', lambdaParams = params' }
redomapToMapAndReduce _ _ _ =
  error "redomapToMapAndReduce does not handle an empty 'patternContextElements'"

sequentialStreamWholeArrayStms :: Bindable lore =>
                                  SubExp -> [SubExp]
                               -> ExtLambdaT lore -> [VName]
                               -> ([Stm lore], [SubExp])
sequentialStreamWholeArrayStms width accs lam arrs =
  let (chunk_param, acc_params, arr_params) =
        partitionChunkedFoldParameters (length accs) $ extLambdaParams lam
      chunk_bnd = mkLet' [] [paramIdent chunk_param] $ BasicOp $ SubExp width
      acc_bnds = [ mkLet' [] [paramIdent acc_param] $ BasicOp $ SubExp acc
                 | (acc_param, acc) <- zip acc_params accs ]
      arr_bnds = [ mkLet' [] [paramIdent arr_param] $
                   BasicOp $ Reshape [] (map DimCoercion $ arrayDims $ paramType arr_param) arr
                 | (arr_param, arr) <- zip arr_params arrs ]

  in (chunk_bnd :
      acc_bnds ++
      arr_bnds ++
      bodyStms (extLambdaBody lam),

      bodyResult $ extLambdaBody lam)

sequentialStreamWholeArray :: (MonadBinder m, Bindable (Lore m)) =>
                              Pattern (Lore m)
                           -> Certificates
                           -> SubExp -> [SubExp]
                           -> ExtLambdaT (Lore m) -> [VName]
                           -> m ()
sequentialStreamWholeArray pat cs width nes fun arrs = do
  let (body_bnds,res) = sequentialStreamWholeArrayStms width nes fun arrs
      reshapeRes t (Var v)
        | null (arrayDims t) = BasicOp $ SubExp $ Var v
        | otherwise          = shapeCoerce cs (arrayDims t) v
      reshapeRes _ se        = BasicOp $ SubExp se
      res_bnds =
        [ mkLet' [] [ident] $ reshapeRes (identType ident) se
        | (ident,se) <- zip (patternValueIdents pat) res]

  mapM_ addStm body_bnds
  shapemap <- shapeMapping (patternValueTypes pat) <$> mapM subExpType res
  forM_ (HM.toList shapemap) $ \(name,se) ->
    when (name `elem` patternContextNames pat) $
      addStm =<< mkLetNames' [name] (BasicOp $ SubExp se)
  mapM_ addStm res_bnds

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
      arr_bnds = [ mkLet' [] [paramIdent arr_param] $
                   BasicOp $ Replicate (Shape [Var chunk_name]) $
                   Var $ paramName unchunked_arr_param |
                   (arr_param, unchunked_arr_param) <-
                     zip arr_params unchunked_arr_params ]
      unchunked_body = insertStms (chunk_bnd:arr_bnds) $ extLambdaBody lam
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
  modifyNameSource $ runState $ Prog <$> mapM ft (progFunctions prog)
