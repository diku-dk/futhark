{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.Lambdas
  ( InternaliseLambda
  , internaliseMapLambda
  , internaliseStreamMapLambda
  , internaliseFoldLambda
  , internaliseStreamLambda
  , internalisePartitionLambda
  )
  where

import Control.Monad
import Data.Loc
import qualified Data.Set as S

import Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Representation.SOACS.Simplify (simplifyLambda)

-- | A function for internalising lambdas.
type InternaliseLambda =
  E.Exp -> [I.Type] -> InternaliseM ([I.LParam], I.Body, [I.ExtType])

internaliseMapLambda :: InternaliseLambda
                     -> E.Exp
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseMapLambda internaliseLambda lam args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map I.rowType argtypes
  (params, body, rettype) <- internaliseLambda lam rowtypes
  (rettype', inner_shapes) <- instantiateShapes' rettype
  let outer_shape = arraysSize 0 argtypes
  shapefun <- makeShapeFun params body rettype' inner_shapes
  bindMapShapes index0 [] inner_shapes shapefun args outer_shape
  body' <- localScope (scopeOfLParams params) $
           ensureResultShape asserting "not all iterations produce same shape"
           (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'
  where index0 arg = do
          arg' <- letExp "arg" $ I.BasicOp $ I.SubExp arg
          arg_t <- lookupType arg'
          return $ I.BasicOp $ I.Index arg' $ fullSlice arg_t [I.DimFix zero]
        zero = constant (0::I.Int32)

internaliseStreamMapLambda :: InternaliseLambda
                           -> E.Exp
                           -> [I.SubExp]
                           -> InternaliseM I.Lambda
internaliseStreamMapLambda internaliseLambda lam args = do
  chunk_size <- newVName "chunk_size"
  let chunk_param = I.Param chunk_size (I.Prim int32)
      outer = (`setOuterSize` I.Var chunk_size)
  localScope (scopeOfLParams [chunk_param]) $ do
    argtypes <- mapM I.subExpType args
    (params, body, rettype) <- internaliseLambda lam $ map outer argtypes
    (rettype', inner_shapes) <- instantiateShapes' rettype
    let outer_shape = arraysSize 0 argtypes
    shapefun <- makeShapeFun (chunk_param:params) body rettype' inner_shapes
    bindMapShapes (slice0 chunk_size) [zero] inner_shapes shapefun args outer_shape
    body' <- localScope (scopeOfLParams params) $
             ensureResultShape asserting "not all iterations produce same shape"
             (srclocOf lam) (map outer rettype') body
    return $ I.Lambda (chunk_param:params) body' (map outer rettype')
  where slice0 chunk_size arg = do
          arg' <- letExp "arg" $ I.BasicOp $ I.SubExp arg
          arg_t <- lookupType arg'
          return $ I.BasicOp $ I.Index arg' $
            fullSlice arg_t [I.DimSlice zero (I.Var chunk_size) one]
        zero = constant (0::I.Int32)
        one = constant (1::I.Int32)

makeShapeFun :: [I.LParam] -> I.Body -> [Type] -> [I.Ident]
             -> InternaliseM I.Lambda
makeShapeFun params body val_rettype inner_shapes = do
  -- Some of 'params' may be unique, which means that the shape slice
  -- would consume its input.  This is not acceptable - that input is
  -- needed for the value function!  Hence, for all unique parameters,
  -- we create a substitute non-unique parameter, and insert a
  -- copy-binding in the body of the function.
  (params', copystms) <- nonuniqueParams params
  shape_body <- runBodyBinder $ localScope (scopeOfLParams params') $ do
    mapM_ addStm copystms
    shapeBody (map I.identName inner_shapes) val_rettype body
  return $ I.Lambda params' shape_body rettype
  where rettype = replicate (length inner_shapes) $ I.Prim int32

bindMapShapes :: (SubExp -> InternaliseM I.Exp) -> [SubExp]
              -> [I.Ident] -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM ()
bindMapShapes indexArg extra_args inner_shapes sizefun args outer_shape
  | null $ I.lambdaReturnType sizefun = return ()
  | otherwise = do
      let size_args = replicate (length $ lambdaParams sizefun) Nothing
      sizefun' <- simplifyLambda sizefun size_args
      let sizefun_safe =
            all (I.safeExp . I.stmExp) $ I.bodyStms $ I.lambdaBody sizefun'
          sizefun_arg_invariant =
            not $ any (`S.member` freeInBody (I.lambdaBody sizefun')) $
            map I.paramName $ lambdaParams sizefun'
      if sizefun_safe && sizefun_arg_invariant
        then do ses <- bodyBind $ lambdaBody sizefun'
                forM_ (zip inner_shapes ses) $ \(v, se) ->
                  letBind_ (basicPattern [] [v]) $ I.BasicOp $ I.SubExp se
        else letBind_ (basicPattern [] inner_shapes) =<<
             eIf' isnonempty nonemptybranch emptybranch IfFallback

  where emptybranch =
          pure $ resultBody (map (const zero) $ I.lambdaReturnType sizefun)
        nonemptybranch = insertStmsM $
          resultBody <$> (eLambda sizefun . (map eSubExp extra_args++) $ map indexArg args)

        isnonempty = eNot $ eCmpOp (I.CmpEq I.int32)
                     (pure $ I.BasicOp $ I.SubExp outer_shape)
                     (pure $ I.BasicOp $ SubExp zero)
        zero = constant (0::I.Int32)

internaliseFoldLambda :: InternaliseLambda
                      -> E.Exp
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
  body' <- localScope (scopeOfLParams params) $
           ensureResultShape asserting
           "shape of result does not match shape of initial value" (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'

internaliseStreamLambda :: InternaliseLambda
                        -> E.Exp
                        -> [I.Type]
                        -> InternaliseM ([LParam], Body)
internaliseStreamLambda internaliseLambda lam rowts = do
  chunk_size <- newVName "chunk_size"
  let chunk_param = I.Param chunk_size $ I.Prim int32
      chunktypes = map (`arrayOfRow` I.Var chunk_size) rowts
  (params, body, _) <- localScope (scopeOfLParams [chunk_param]) $
                       internaliseLambda lam chunktypes
  return (chunk_param:params, body)

-- Given @k@ lambdas, this will return a lambda that returns an
-- (k+2)-element tuple of integers.  The first element is the
-- equivalence class ID in the range [0,k].  The remaining are all zero
-- except for possibly one element.
internalisePartitionLambda :: InternaliseLambda
                           -> Int
                           -> E.Exp
                           -> [I.SubExp]
                           -> InternaliseM I.Lambda
internalisePartitionLambda internaliseLambda k lam args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map I.rowType argtypes
  (params, body, _) <- internaliseLambda lam rowtypes
  body' <- localScope (scopeOfLParams params) $
           lambdaWithIncrement body
  return $ I.Lambda params body' rettype
  where rettype = replicate (k+2) $ I.Prim int32
        result i = map constant $ (fromIntegral i :: Int32) :
                   (replicate i 0 ++ [1::Int32] ++ replicate (k-i) 0)

        mkResult _ i | i >= k = return $ result i
        mkResult eq_class i = do
          is_i <- letSubExp "is_i" $ BasicOp $ CmpOp (CmpEq int32) eq_class (constant i)
          fmap (map I.Var) . letTupExp "part_res" =<<
            eIf (eSubExp is_i) (pure $ resultBody $ result i)
                               (resultBody <$> mkResult eq_class (i+1))

        lambdaWithIncrement :: I.Body -> InternaliseM I.Body
        lambdaWithIncrement lam_body = runBodyBinder $ do
          [eq_class] <- bodyBind lam_body
          resultBody <$> mkResult eq_class 0
