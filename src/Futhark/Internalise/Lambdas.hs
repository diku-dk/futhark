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
import qualified Data.Set as S

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
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseMapLambda internaliseLambda lam args = do
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
           ensureResultShape asserting "not all iterations produce same shape"
           (srclocOf lam) rettype' body
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
  return $ I.Lambda params' (insertStms copybnds body) rettype
  where rettype = replicate n $ I.Prim int32

bindMapShapes :: [I.Ident] -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM ()
bindMapShapes inner_shapes sizefun args outer_shape
  | null $ I.lambdaReturnType sizefun = return ()
  | otherwise = do
      let size_args = replicate (length $ lambdaParams sizefun) Nothing
      sizefun' <- deadCodeElimLambda <$> simplifyLambda sizefun Nothing size_args
      let sizefun_safe =
            all (I.safeExp . I.stmExp) $ I.bodyStms $ I.lambdaBody sizefun'
          sizefun_arg_invariant =
            not $ any (`S.member` freeInBody (I.lambdaBody sizefun')) $
            map I.paramName $ lambdaParams sizefun'
      if sizefun_safe && sizefun_arg_invariant
        then do ses <- bodyBind $ lambdaBody sizefun'
                forM_ (zip inner_shapes ses) $ \(v, se) ->
                  letBind_ (basicPattern' [] [v]) $ I.BasicOp $ I.SubExp se
        else letBind_ (basicPattern' [] inner_shapes) =<<
             eIf' isnonempty nonemptybranch emptybranch IfFallback

  where emptybranch =
          pure $ resultBody (map (const zero) $ I.lambdaReturnType sizefun)
        nonemptybranch = insertStmsM $
          resultBody <$> (eLambda sizefun =<< mapM index0 args)

        isnonempty = eNot $ eCmpOp (I.CmpEq I.int32)
                     (pure $ I.BasicOp $ I.SubExp outer_shape)
                     (pure $ I.BasicOp $ SubExp zero)
        zero = constant (0::I.Int32)
        index0 arg = do
          arg' <- letExp "arg" $ I.BasicOp $ I.SubExp arg
          arg_t <- lookupType arg'
          letSubExp "elem" $ I.BasicOp $ I.Index [] arg' $ fullSlice arg_t [I.DimFix zero]

internaliseFoldLambda :: InternaliseLambda
                      -> E.Lambda
                      -> [I.Type] -> [I.Type]
                      -> InternaliseM I.Lambda
internaliseFoldLambda internaliseLambda lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda lam $ acctypes ++ rowtypes
  let rettype' = [ t `I.setArrayShape` I.arrayShape shape
                   | (t,shape) <- zip rettype acctypes ]
  -- The result of the body must have the exact same shape as the
  -- initial accumulator.  We accomplish this with an assertion and
  -- reshape().
  body' <- bindingParamTypes params $
           ensureResultShape asserting
           "shape of result does not match shape of initial value" (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'


internaliseRedomapInnerLambda :: InternaliseLambda
                              -> E.Lambda
                              -> [I.SubExp]
                              -> [I.SubExp]
                              -> InternaliseM I.Lambda
internaliseRedomapInnerLambda internaliseLambda lam nes arr_args = do
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

      map_bindings= acc_bindings ++ bodyStms body
      map_lore    = bodyAttr body
      map_body = I.Body map_lore map_bindings map_bodyres
  shape_body <- bindingParamTypes params $
                shapeBody (map I.identName inner_shapes) rettypearr' map_body
  shapefun <- makeShapeFun (drop acc_len params) shape_body (length inner_shapes)
  bindMapShapes inner_shapes shapefun arr_args outer_shape
  --
  -- for the reduce part
  let acctype' = [ t `I.setArrayShape` I.arrayShape shape
                   | (t,shape) <- zip acc_tps acctypes ]
  -- The reduce-part result of the body must have the exact same
  -- shape as the initial accumulator.  We accomplish this with
  -- an assertion and reshape().
  --
  -- finally, place assertions and return result
  body' <- bindingParamTypes params $
           ensureResultShape asserting "shape of result does not match shape of initial value"
           (srclocOf lam) (acctype'++rettypearr') body
  return $ I.Lambda params body' (acctype'++rettypearr')

internaliseStreamLambda :: InternaliseLambda
                        -> E.Lambda
                        -> [I.Type]
                        -> InternaliseM I.ExtLambda
internaliseStreamLambda internaliseLambda lam rowts = do
  chunk_size <- newVName "chunk_size"
  let chunk_param = I.Param chunk_size $ I.Prim int32
      chunktypes = map (`arrayOfRow` I.Var chunk_size) rowts
  (params, body, rettype) <- localScope (scopeOfLParams [chunk_param]) $
                             internaliseLambda lam chunktypes


  -- The accumulator result of the body must have the exact same
  -- shape as the initial accumulator.  We accomplish this with
  -- an assertion and reshape().  For the result arrays, we allow
  -- the outermost dimension to be existential, but we require
  -- all inner dimensions to be specified by the user, so we can
  -- check them with an assertion and reshape().
  --
  let assertProperShape t se =
        let name = "result_stream_proper_shape"
        in  ensureShape asserting "shape of result does not match shape of initial value"
            (srclocOf lam) t name se

  body' <- insertStmsM $ do
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
                let lamarr_idtps = concatMap (\(y,tp) -> case y of
                                                           I.Var ii -> [(ii,tp)]
                                                           _        -> []
                                             ) (zip lamres rettype)
                arrtype' <- mapM mkArrType lamarr_idtps
                reses <- zipWithM assertProperShape arrtype' lamres
                return $ resultBody reses
  return $ I.ExtLambda (chunk_param:params) body' rettype

-- Given @k@ lambdas, this will return a lambda that returns an
-- (k+2)-element tuple of integers.  The first element is the
-- equivalence class ID in the range [0,k].  The remaining are all zero
-- except for possibly one element.
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
  return $ I.Lambda params' body rettype
  where k = length lams
        rettype = replicate (k+2) $ I.Prim int32
        result i = resultBody $
                   map constant $ (fromIntegral i :: Int32) :
                   (replicate i 0 ++ [1::Int32] ++ replicate (k-i) 0)
        mkCombinedLambdaBody :: [I.Ident]
                             -> Int
                             -> [([I.LParam], I.Body)]
                             -> InternaliseM I.Body
        mkCombinedLambdaBody _      i [] =
          return $ result i
        mkCombinedLambdaBody params i ((lam_params,lam_body):lams') =
          case lam_body of
            Body () bodybnds [boolres] -> do
              intres <- (:) <$> newIdent "eq_class" (I.Prim int32) <*>
                        replicateM (k+1) (newIdent "partition_incr" $ I.Prim int32)
              next_lam_body <-
                mkCombinedLambdaBody (map paramIdent lam_params) (i+1) lams'
              let parambnds =
                    [ mkLet' [] [paramIdent top] $ I.BasicOp $ I.SubExp $ I.Var $ I.identName fromp
                    | (top,fromp) <- zip lam_params params ]
                  branchbnd = mkLet' [] intres $ I.If boolres
                              (result i)
                              next_lam_body $
                              ifCommon rettype
              return $ mkBody (parambnds++bodybnds++[branchbnd]) $
                map (I.Var . I.identName) intres
            _ ->
              fail "Partition lambda returns too many values."
