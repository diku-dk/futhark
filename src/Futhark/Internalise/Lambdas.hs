module Futhark.Internalise.Lambdas
  ( InternaliseLambda
  , internaliseMapLambda
  , internaliseConcatMapLambda
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

import Futhark.Representation.External as E
import Futhark.Representation.Basic as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes

import Prelude hiding (mapM)

-- | A function for internalising lambdas.
type InternaliseLambda =
  E.Lambda -> Maybe [I.Type] -> InternaliseM ([I.LParam], I.Body, [I.ExtType])

internaliseMapLambda :: InternaliseLambda
                     -> (InternaliseM Certificates -> InternaliseM Certificates)
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseMapLambda internaliseLambda asserting lam args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map I.rowType argtypes
  (params, body, rettype) <- internaliseLambda lam $ Just rowtypes
  (rettype', inner_shapes) <- instantiateShapes' rettype
  let outer_shape = arraysSize 0 argtypes
  shape_body <- bindingParamTypes params $
                shapeBody (map I.identName inner_shapes) rettype' body
  shapefun <- makeShapeFun params shape_body (length inner_shapes)
  bindMapShapes inner_shapes shapefun args outer_shape
  body' <- bindingParamTypes params $
           ensureResultShape asserting (srclocOf lam) rettype' body
  i <- newVName "i"
  return $ I.Lambda i params body' rettype'

internaliseConcatMapLambda :: InternaliseLambda
                           -> E.Lambda
                           -> InternaliseM I.Lambda
internaliseConcatMapLambda internaliseLambda lam = do
  (params, body, rettype) <- internaliseLambda lam Nothing
  i <- newVName "i"
  case rettype of
    [I.Array bt (ExtShape [_]) _] ->
      return $ I.Lambda i params body [I.Basic bt]
    _ ->
      fail "concatMap lambda does not return a single-dimensional array"

makeShapeFun :: [I.LParam] -> I.Body -> Int
             -> InternaliseM I.Lambda
makeShapeFun params body n = do
  -- Some of 'params' may be unique, which means that the shape slice
  -- would consume its input.  This is not acceptable - that input is
  -- needed for the value function!  Hence, for all unique parameters,
  -- we create a substitute non-unique parameter, and insert a
  -- copy-binding in the body of the function.
  (params', copybnds) <- nonuniqueParams params
  i <- newVName "i"
  return $ I.Lambda i params' (insertBindings copybnds body) rettype
  where rettype = replicate n $ I.Basic Int

bindMapShapes :: [I.Ident] -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM ()
bindMapShapes inner_shapes sizefun args outer_shape
  | null $ I.lambdaReturnType sizefun = return ()
  | otherwise =
    letBind_ (basicPattern' [] inner_shapes) =<<
    eIf isempty emptybranch nonemptybranch
  where zero = intconst 0
        isempty = eBinOp I.Equal
                  (pure $ I.PrimOp $ I.SubExp outer_shape)
                  (pure $ I.PrimOp $ SubExp zero)
                  I.Bool
        emptybranch =
          pure $ resultBody (map (const zero) $ I.lambdaReturnType sizefun)
        nonemptybranch = insertBindingsM $
          resultBody <$> (eLambda sizefun =<< mapM index0 args)
        index0 arg = do
          arg' <- letExp "arg" $ I.PrimOp $ I.SubExp arg
          letSubExp "elem" $ I.PrimOp $ I.Index [] arg' [intconst 0]

internaliseFoldLambda :: InternaliseLambda
                      -> (InternaliseM Certificates -> InternaliseM Certificates)
                      -> E.Lambda
                      -> [I.Type] -> [I.Type]
                      -> InternaliseM I.Lambda
internaliseFoldLambda internaliseLambda asserting lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda lam $ Just $
                             acctypes ++ rowtypes
  let rettype' = [ t `I.setArrayShape` arrayShape shape
                   | (t,shape) <- zip rettype acctypes ]
  -- The result of the body must have the exact same shape as the
  -- initial accumulator.  We accomplish this with an assertion and
  -- reshape().
  body' <- bindingParamTypes params $
           ensureResultShape asserting (srclocOf lam) rettype' body

  i <- newVName "i"

  return $ I.Lambda i params body' rettype'


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
  (params, body, rettype) <- internaliseLambda lam $ Just $
                             acctypes ++ rowtypes
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
                            mkLet' [] [paramIdent ac_var] (PrimOp $ SubExp ac_val)
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
  i <- newVName "i"
  return $ I.Lambda i params body' (acctype'++rettypearr')

internaliseStreamLambda :: InternaliseLambda
                        -> (InternaliseM Certificates -> InternaliseM Certificates)
                        -> E.Lambda
                        -> [I.SubExp]
                        -> [I.Type]
                        -> InternaliseM I.ExtLambda
internaliseStreamLambda internaliseLambda asserting lam accs arrtypes = do
  acctypes <- mapM I.subExpType accs
  (params, body, rettype) <- internaliseLambda lam $ Just $
                             acctypes++arrtypes
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
                      dsx <- I.shapeDims <$> I.arrayShape <$> I.lookupType x
                      let dsrtpx =  I.extShapeDims shp
                          resdims= zipWith (\ dx drtpx ->
                                                  case drtpx of
                                                    Ext  _ -> dx
                                                    Free s -> s
                                           ) dsx dsrtpx
                      return $ I.Array btp (I.Shape resdims) u
                    mkArrType (_, I.Basic btp ) =
                      return $ I.Basic btp
                    mkArrType (_, I.Mem se space) =
                      return $ I.Mem se space
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
  i <- newVName "i"
  return $ I.ExtLambda i params body' $
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
    (params, body, _) <- internaliseLambda lam $ Just rowtypes
    return (params, body)
  params <- newIdents "partition_param" rowtypes
  body <- mkCombinedLambdaBody params 0 lams'
  i <- newVName "i"
  return $ I.Lambda i (map (`Param` ()) params) body [I.Basic Int]
  where mkCombinedLambdaBody :: [I.Ident]
                             -> Int
                             -> [([I.LParam], I.Body)]
                             -> InternaliseM I.Body
        mkCombinedLambdaBody _      i [] =
          return $ resultBody [intconst i]
        mkCombinedLambdaBody params i ((lam_params,lam_body):lams') =
          case lam_body of
            Body () bodybnds [boolres] -> do
              intres <- newIdent "partition_equivalence_class" $ I.Basic Int
              next_lam_body <-
                mkCombinedLambdaBody (map paramIdent lam_params) (i+1) lams'
              let parambnds =
                    [ mkLet' [] [paramIdent top] $ I.PrimOp $ I.SubExp $ I.Var $ I.identName fromp
                    | (top,fromp) <- zip lam_params params ]
                  branchbnd = mkLet' [] [intres] $ I.If boolres
                              (resultBody [intconst i])
                              next_lam_body
                              [I.Basic Int]
              return $ mkBody
                (parambnds++bodybnds++[branchbnd])
                [I.Var $ I.identName intres]
            _ ->
              fail "Partition lambda returns too many values."
