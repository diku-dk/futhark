{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.Lambdas
  ( InternaliseLambda
  , internaliseMapLambda
  , internaliseFoldLambda
  , internaliseRedomapInnerLambda
  , internaliseStreamLambda
  , internalisePartitionLambdas
  )
  where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Loc
import qualified Data.HashSet as HS

import Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Representation.SOACS.Simplify (simplifyLambda)
import Futhark.Optimise.DeadVarElim (deadCodeElimLambda)

import Prelude hiding (mapM)

-- | A function for internalising lambdas.
type InternaliseLambda =
  E.Lambda -> [I.Type] -> InternaliseM ([I.LParam], I.Body, [I.ExtType])

internaliseMapLambda :: InternaliseLambda
                     -> (InternaliseM Certificates -> InternaliseM Certificates)
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseMapLambda internaliseLambda asserting lam args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map I.rowType argtypes
  (params, body, rettype) <- internaliseLambda lam rowtypes
  (rettype', inner_shapes) <- instantiateShapes' rettype
  let outer_shape = arraysSize 0 argtypes
  shape_body <- bindingParamTypes params $
                shapeBody (map I.identName inner_shapes) rettype' body
  shapefun <- makeShapeFun params shape_body (length inner_shapes)
  bindMapShapes inner_shapes shapefun args outer_shape
  body' <- bindingParamTypes params $
           ensureResultShape asserting (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'

makeShapeFun :: [I.LParam] -> I.Body -> Int
             -> InternaliseM I.Lambda
makeShapeFun params body n = do
  -- Some of 'params' may be unique, which means that the shape slice
  -- would consume its input.  This is not acceptable - that input is
  -- needed for the value function!  Hence, for all unique parameters,
  -- we create a substitute non-unique parameter, and insert a
  -- copy-binding in the body of the function.
  (params', copybnds) <- nonuniqueParams params
  return $ I.Lambda params' (insertBindings copybnds body) rettype
  where rettype = replicate n $ I.Prim int32

bindMapShapes :: [I.Ident] -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM ()
bindMapShapes inner_shapes sizefun args outer_shape
  | null $ I.lambdaReturnType sizefun = return ()
  | otherwise = do
      let size_args = replicate (length $ lambdaParams sizefun) Nothing
      sizefun' <- deadCodeElimLambda <$> simplifyLambda sizefun Nothing size_args
      let sizefun_safe =
            all (I.safeExp . I.bindingExp) $ I.bodyBindings $ I.lambdaBody sizefun'
          sizefun_arg_invariant =
            not $ any (`HS.member` freeInBody (I.lambdaBody sizefun')) $
            map I.paramName $ lambdaParams sizefun'
      if sizefun_safe && sizefun_arg_invariant
        then do ses <- bodyBind $ lambdaBody sizefun'
                forM_ (zip inner_shapes ses) $ \(v, se) ->
                  letBind_ (basicPattern' [] [v]) $ I.BasicOp $ I.SubExp se
        else letBind_ (basicPattern' [] inner_shapes) =<<
             eIf isempty emptybranch nonemptybranch

  where emptybranch =
          pure $ resultBody (map (const zero) $ I.lambdaReturnType sizefun)
        nonemptybranch = insertBindingsM $
          resultBody <$> (eLambda sizefun =<< mapM index0 args)

        isempty = eCmpOp (I.CmpEq I.int32)
                  (pure $ I.BasicOp $ I.SubExp outer_shape)
                  (pure $ I.BasicOp $ SubExp zero)
        zero = constant (0::I.Int32)
        index0 arg = do
          arg' <- letExp "arg" $ I.BasicOp $ I.SubExp arg
          arg_t <- lookupType arg'
          letSubExp "elem" $ I.BasicOp $ I.Index [] arg' $ fullSlice arg_t [I.DimFix zero]

internaliseFoldLambda :: InternaliseLambda
                      -> (InternaliseM Certificates -> InternaliseM Certificates)
                      -> E.Lambda
                      -> [I.Type] -> [I.Type]
                      -> InternaliseM I.Lambda
internaliseFoldLambda internaliseLambda asserting lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda lam $ acctypes ++ rowtypes
  let rettype' = [ t `I.setArrayShape` arrayShape shape
                   | (t,shape) <- zip rettype acctypes ]
  -- The result of the body must have the exact same shape as the
  -- initial accumulator.  We accomplish this with an assertion and
  -- reshape().
  body' <- bindingParamTypes params $
           ensureResultShape asserting (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'


internaliseRedomapInnerLambda :: InternaliseLambda
                              -> (InternaliseM Certificates -> InternaliseM Certificates)
                              -> E.Lambda
                              -> [I.SubExp]
                              -> [I.SubExp]
                              -> InternaliseM I.Lambda
internaliseRedomapInnerLambda internaliseLambda asserting lam nes arr_args = do
  arrtypes <- mapM I.subExpType arr_args
  acctypes <- mapM I.subExpType nes
  let rowtypes = map I.rowType arrtypes
  --
  (params, body, rettype) <- internaliseLambda lam $ acctypes ++ rowtypes
  -- split rettype into (i) accummulator types && (ii) result-array-elem types
  let acc_len = length acctypes
      (acc_tps, res_el_tps) = (take acc_len rettype, drop acc_len rettype)
  -- For the map part:  for shape computation we build
  -- a map lambda from the inner lambda by dropping from
  -- the result the accumular and binding the accumulating
  -- param to their corresponding neutral-element subexp.
  -- Troels: would this be correct?
  (rettypearr', inner_shapes) <- instantiateShapes' res_el_tps
  let outer_shape = arraysSize 0 arrtypes
      acc_params  = take acc_len params
      map_bodyres = drop acc_len $ I.bodyResult body
      acc_bindings= map (\(ac_var,ac_val) ->
                            mkLet' [] [paramIdent ac_var] (BasicOp $ SubExp ac_val)
                        ) (zip acc_params nes)

      map_bindings= acc_bindings ++ bodyBindings body
      map_lore    = bodyLore body
      map_body = I.Body map_lore map_bindings map_bodyres
  shape_body <- bindingParamTypes params $
                shapeBody (map I.identName inner_shapes) rettypearr' map_body
  shapefun <- makeShapeFun (drop acc_len params) shape_body (length inner_shapes)
  bindMapShapes inner_shapes shapefun arr_args outer_shape
  --
  -- for the reduce part
  let acctype' = [ t `I.setArrayShape` arrayShape shape
                   | (t,shape) <- zip acc_tps acctypes ]
  -- The reduce-part result of the body must have the exact same
  -- shape as the initial accumulator.  We accomplish this with
  -- an assertion and reshape().
  --
  -- finally, place assertions and return result
  body' <- bindingParamTypes params $
           ensureResultShape asserting (srclocOf lam) (acctype'++rettypearr') body
  return $ I.Lambda params body' (acctype'++rettypearr')

internaliseStreamLambda :: InternaliseLambda
                        -> (InternaliseM Certificates -> InternaliseM Certificates)
                        -> E.Lambda
                        -> [I.SubExp]
                        -> [I.Type]
                        -> InternaliseM I.ExtLambda
internaliseStreamLambda internaliseLambda asserting lam accs arrtypes = do
  acctypes <- mapM I.subExpType accs
  (params, body, rettype) <- internaliseLambda lam $ acctypes++arrtypes
  -- split rettype into (i) accummulator types && (ii) result-array-elem types
  let acc_len = length acctypes
      lam_acc_tps = take acc_len rettype
  let lam_arr_tps = drop acc_len rettype
  --
  -- The accumulator result of the body must have the exact same
  -- shape as the initial accumulator.  We accomplish this with
  -- an assertion and reshape().  For the result arrays, we allow
  -- the outermost dimension to be existential, but we require
  -- all inner dimensions to be specified by the user, so we can
  -- check them with an assertion and reshape().
  --
  let assertProperShape t se =
        let name = "result_stream_proper_shape"
        in  ensureShape asserting (srclocOf lam) t name se
  let acctype' = [ t `I.setArrayShape` arrayShape shape
                   | (t,shape) <- zip lam_acc_tps acctypes ]
  body' <- insertBindingsM $ do
                let mkArrType :: (VName, ExtType) -> InternaliseM I.Type
                    mkArrType (x, I.Array btp shp u) = do
                      dsx <- I.shapeDims . I.arrayShape <$> I.lookupType x
                      let dsrtpx =  I.extShapeDims shp
                          resdims= zipWith (\ dx drtpx ->
                                                  case drtpx of
                                                    Ext  _ -> dx
                                                    Free s -> s
                                           ) dsx dsrtpx
                      return $ I.Array btp (I.Shape resdims) u
                    mkArrType (_, I.Prim btp ) =
                      return $ I.Prim btp
                    mkArrType (_, I.Mem se sid) =
                      return $ I.Mem se sid
                lamres <- bodyBind body
                let (lamacc_res, lamarr_res) = (take acc_len lamres, drop acc_len lamres)
                    lamarr_idtps = concatMap (\(y,tp) -> case y of
                                                           I.Var ii -> [(ii,tp)]
                                                           _        -> []
                                             ) (zip lamarr_res lam_arr_tps)
                arrtype' <- mapM mkArrType lamarr_idtps
                reses1 <- zipWithM assertProperShape acctype' lamacc_res
                reses2 <- zipWithM assertProperShape arrtype' lamarr_res
                return $ resultBody $ reses1 ++ reses2
  return $ I.ExtLambda params body' $
            staticShapes acctypes ++ lam_arr_tps

-- Given @n@ lambdas, this will return a lambda that returns an
-- integer in the range @[0,n]@.
internalisePartitionLambdas :: InternaliseLambda
                            -> [E.Lambda]
                            -> [I.SubExp]
                            -> InternaliseM I.Lambda
internalisePartitionLambdas internaliseLambda lams args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map I.rowType argtypes
  lams' <- forM lams $ \lam -> do
    (params, body, _) <- internaliseLambda lam rowtypes
    return (params, body)
  params <- newIdents "partition_param" rowtypes
  let params' = [ I.Param name t
                | I.Ident name t <- params]
  body <- mkCombinedLambdaBody params 0 lams'
  return $ I.Lambda params' body [I.Prim int32]
  where mkCombinedLambdaBody :: [I.Ident]
                             -> Int32
                             -> [([I.LParam], I.Body)]
                             -> InternaliseM I.Body
        mkCombinedLambdaBody _      i [] =
          return $ resultBody [constant i]
        mkCombinedLambdaBody params i ((lam_params,lam_body):lams') =
          case lam_body of
            Body () bodybnds [boolres] -> do
              intres <- newIdent "partition_equivalence_class" $ I.Prim int32
              next_lam_body <-
                mkCombinedLambdaBody (map paramIdent lam_params) (i+1) lams'
              let parambnds =
                    [ mkLet' [] [paramIdent top] $ I.BasicOp $ I.SubExp $ I.Var $ I.identName fromp
                    | (top,fromp) <- zip lam_params params ]
                  branchbnd = mkLet' [] [intres] $ I.If boolres
                              (resultBody [constant i])
                              next_lam_body
                              [I.Prim int32]
              return $ mkBody
                (parambnds++bodybnds++[branchbnd])
                [I.Var $ I.identName intres]
            _ ->
              fail "Partition lambda returns too many values."
