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

import Data.Loc

import Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.MonadFreshNames
import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes

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
  (rettype', _) <- instantiateShapes' rettype
  body' <- localScope (scopeOfLParams params) $
           ensureResultShape asserting
           (ErrorMsg [ErrorString "not all iterations produce same shape"])
           (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'

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
    (lam_params, orig_body, rettype) <-
      internaliseLambda lam $ I.Prim int32 : map outer argtypes
    let orig_chunk_param : params = lam_params
    body <- runBodyBinder $ do
      letBindNames_ [paramName orig_chunk_param] $ I.BasicOp $ I.SubExp $ I.Var chunk_size
      return orig_body
    (rettype', _) <- instantiateShapes' rettype
    body' <- localScope (scopeOfLParams params) $ insertStmsM $ do
      letBindNames_ [paramName orig_chunk_param] $ I.BasicOp $ I.SubExp $ I.Var chunk_size
      ensureResultShape asserting
        (ErrorMsg [ErrorString "not all iterations produce same shape"])
        (srclocOf lam) (map outer rettype') body
    return $ I.Lambda (chunk_param:params) body' (map outer rettype')

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
           (ErrorMsg [ErrorString "shape of result does not match shape of initial value"])
           (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'

internaliseStreamLambda :: InternaliseLambda
                        -> E.Exp
                        -> [I.Type]
                        -> InternaliseM ([LParam], Body)
internaliseStreamLambda internaliseLambda lam rowts = do
  chunk_size <- newVName "chunk_size"
  let chunk_param = I.Param chunk_size $ I.Prim int32
      chunktypes = map (`arrayOfRow` I.Var chunk_size) rowts
  localScope (scopeOfLParams [chunk_param]) $ do
    (lam_params, orig_body, _) <-
      internaliseLambda lam $ I.Prim int32 : chunktypes
    let orig_chunk_param : params = lam_params
    body <- runBodyBinder $ do
      letBindNames_ [paramName orig_chunk_param] $ I.BasicOp $ I.SubExp $ I.Var chunk_size
      return orig_body
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
          eq_class <- head <$> bodyBind lam_body
          resultBody <$> mkResult eq_class 0
