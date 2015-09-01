{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Futhark.Tools
  (
    module Futhark.Construct

  , nonuniqueParams
  , redomapToMapAndReduce
  , sequentialStreamWholeArray
  , intraproceduralTransformation
  , boundInBody
  )
where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State

import Prelude

import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.MonadFreshNames
import Futhark.Construct

nonuniqueParams :: (MonadFreshNames m, Bindable lore) =>
                   [LParam lore] -> m ([LParam lore], [Binding lore])
nonuniqueParams params =
  modifyNameSource $ runState $ liftM fst $ runBinderEmptyEnv $
  collectBindings $ forM params $ \param ->
    if unique $ paramType param then do
      param' <- Param <$>
                (nonuniqueParam <$> newIdent' (++"_nonunique") (paramIdent param)) <*>
                pure ()
      bindingIdentTypes [paramIdent param'] $
        letBindNames_ [(paramName param,BindVar)] $
        PrimOp $ Copy $ paramName param'
      return param'
    else
      return param
  where nonuniqueParam ident =
          ident { identType = identType ident `setUniqueness` Nonunique }

-- | Turns a binding of a @redomap@ into two seperate bindings, a
-- @map@ binding and a @reduce@ binding (returned in that order).
--
-- Reuses the original pattern for the @reduce@, and creates a new
-- pattern with new 'Ident's for the result of the @map@. Does /not/
-- add the new idents to the 'TypeEnv'.
--
-- Only handles a 'Pattern' with an empty 'patternContextElements'
redomapToMapAndReduce :: (MonadFreshNames m, Bindable lore) =>
                         PatternT lore -> Annotations.Exp lore
                      -> ( Certificates, SubExp
                         , LambdaT lore, LambdaT lore, [SubExp]
                         , [VName])
                      -> m (Binding lore, Binding lore)
redomapToMapAndReduce (Pattern [] patelems) lore
                      (certs, outersz, redlam, redmap_lam, accs, arrs) = do
  let (acc_patelems, arr_patelems) = splitAt (length accs) patelems
  map_accpat <- mapM accMapPatElem acc_patelems
  map_arrpat <- mapM arrMapPatElem arr_patelems
  let map_pat = map_accpat ++ map_arrpat
      map_bnd = mkLet [] map_pat $
                LoopOp $ Map certs outersz newmap_lam arrs
      red_args = zip accs $ map (identName . fst) map_accpat
      red_bnd = Let (Pattern [] patelems) lore $
                LoopOp $ Reduce certs outersz redlam red_args
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
          bndaccs = zipWith (\i acc -> mkLet' []  [i] (PrimOp $ SubExp acc))
                            tobnd accs
          body = lambdaBody redmap_lam
          bnds' = bndaccs ++ bodyBindings body
          body' = body {bodyBindings = bnds'}
      in redmap_lam { lambdaBody = body', lambdaParams = params' }
redomapToMapAndReduce _ _ _ =
  error "redomapToMapAndReduce does not handle an empty 'patternContextElements'"

sequentialStreamWholeArray :: Bindable lore =>
                              SubExp -> [SubExp]
                           -> ExtLambdaT lore -> [VName]
                           -> ([Binding lore], [SubExp])
sequentialStreamWholeArray width accs lam arrs =
  let (chunk_param, acc_params, arr_params) =
        partitionParameters $ extLambdaParams lam
      chunk_bnd = mkLet' [] [paramIdent chunk_param] $ PrimOp $ SubExp width
      acc_bnds = [ mkLet' [] [paramIdent acc_param] $ PrimOp $ SubExp acc
                 | (acc_param, acc) <- zip acc_params accs ]
      arr_bnds = [ mkLet' [] [paramIdent arr_param] $
                   PrimOp $ Reshape [] (map DimCoercion $ arrayDims $ paramType arr_param) arr
                 | (arr_param, arr) <- zip arr_params arrs ]

  in (chunk_bnd :
      acc_bnds ++
      arr_bnds ++
      bodyBindings (extLambdaBody lam),

      bodyResult $ extLambdaBody lam)
  where partitionParameters [] =
          error "sequentialStreamWholeArray: lambda takes no parameters"
        partitionParameters (chunk_param : params) =
          let (acc_params, arr_params) = splitAt (length accs) params
          in (chunk_param, acc_params, arr_params)

intraproceduralTransformation :: MonadFreshNames m =>
                                 (FunDec fromlore -> State VNameSource (FunDec tolore))
                              -> Prog fromlore -> m (Prog tolore)
intraproceduralTransformation ft prog =
  modifyNameSource $ runState $ Prog <$> mapM ft (progFunctions prog)
