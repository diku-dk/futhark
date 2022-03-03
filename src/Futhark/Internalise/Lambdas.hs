{-# LANGUAGE FlexibleContexts #-}

module Futhark.Internalise.Lambdas
  ( InternaliseLambda,
    internaliseMapLambda,
    internaliseStreamMapLambda,
    internaliseFoldLambda,
    internaliseStreamLambda,
    internalisePartitionLambda,
  )
where

import Futhark.IR.SOACS as I
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.Monad
import Language.Futhark as E

-- | A function for internalising lambdas.
type InternaliseLambda =
  E.Exp -> [I.Type] -> InternaliseM ([I.LParam SOACS], I.Body SOACS, [I.Type])

internaliseMapLambda ::
  InternaliseLambda ->
  E.Exp ->
  [I.SubExp] ->
  InternaliseM (I.Lambda SOACS)
internaliseMapLambda internaliseLambda lam args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map I.rowType argtypes
  (params, body, rettype) <- internaliseLambda lam rowtypes
  mkLambda params $
    ensureResultShape
      (ErrorMsg [ErrorString "not all iterations produce same shape"])
      (srclocOf lam)
      rettype
      =<< bodyBind body

internaliseStreamMapLambda ::
  InternaliseLambda ->
  E.Exp ->
  [I.SubExp] ->
  InternaliseM (I.Lambda SOACS)
internaliseStreamMapLambda internaliseLambda lam args = do
  chunk_size <- newVName "chunk_size"
  let chunk_param = I.Param mempty chunk_size (I.Prim int64)
      outer = (`setOuterSize` I.Var chunk_size)
  localScope (scopeOfLParams [chunk_param]) $ do
    argtypes <- mapM I.subExpType args
    (lam_params, orig_body, rettype) <-
      internaliseLambda lam $ I.Prim int64 : map outer argtypes
    let orig_chunk_param : params = lam_params
    body <- runBodyBuilder $ do
      letBindNames [paramName orig_chunk_param] $ I.BasicOp $ I.SubExp $ I.Var chunk_size
      return orig_body
    mkLambda (chunk_param : params) $ do
      letBindNames [paramName orig_chunk_param] $ I.BasicOp $ I.SubExp $ I.Var chunk_size
      ensureResultShape
        (ErrorMsg [ErrorString "not all iterations produce same shape"])
        (srclocOf lam)
        (map outer rettype)
        =<< bodyBind body

internaliseFoldLambda ::
  InternaliseLambda ->
  E.Exp ->
  [I.Type] ->
  [I.Type] ->
  InternaliseM (I.Lambda SOACS)
internaliseFoldLambda internaliseLambda lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda lam $ acctypes ++ rowtypes
  let rettype' =
        [ t `I.setArrayShape` I.arrayShape shape
          | (t, shape) <- zip rettype acctypes
        ]
  -- The result of the body must have the exact same shape as the
  -- initial accumulator.
  mkLambda params $
    ensureResultShape
      (ErrorMsg [ErrorString "shape of result does not match shape of initial value"])
      (srclocOf lam)
      rettype'
      =<< bodyBind body

internaliseStreamLambda ::
  InternaliseLambda ->
  E.Exp ->
  [I.Type] ->
  InternaliseM ([LParam SOACS], Body SOACS)
internaliseStreamLambda internaliseLambda lam rowts = do
  chunk_size <- newVName "chunk_size"
  let chunk_param = I.Param mempty chunk_size $ I.Prim int64
      chunktypes = map (`arrayOfRow` I.Var chunk_size) rowts
  localScope (scopeOfLParams [chunk_param]) $ do
    (lam_params, orig_body, _) <-
      internaliseLambda lam $ I.Prim int64 : chunktypes
    let orig_chunk_param : params = lam_params
    body <- runBodyBuilder $ do
      letBindNames [paramName orig_chunk_param] $ I.BasicOp $ I.SubExp $ I.Var chunk_size
      pure orig_body
    pure (chunk_param : params, body)

-- Given @k@ lambdas, this will return a lambda that returns an
-- (k+2)-element tuple of integers.  The first element is the
-- equivalence class ID in the range [0,k].  The remaining are all zero
-- except for possibly one element.
internalisePartitionLambda ::
  InternaliseLambda ->
  Int ->
  E.Exp ->
  [I.SubExp] ->
  InternaliseM (I.Lambda SOACS)
internalisePartitionLambda internaliseLambda k lam args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map I.rowType argtypes
  (params, body, _) <- internaliseLambda lam rowtypes
  body' <-
    localScope (scopeOfLParams params) $
      lambdaWithIncrement body
  return $ I.Lambda params body' rettype
  where
    rettype = replicate (k + 2) $ I.Prim int64
    result i =
      map constant $
        fromIntegral i :
        (replicate i 0 ++ [1 :: Int64] ++ replicate (k - i) 0)

    mkResult _ i | i >= k = return $ result i
    mkResult eq_class i = do
      is_i <-
        letSubExp "is_i" $
          BasicOp $
            CmpOp (CmpEq int64) eq_class $
              intConst Int64 $ toInteger i
      letTupExp' "part_res"
        =<< eIf
          (eSubExp is_i)
          (pure $ resultBody $ result i)
          (resultBody <$> mkResult eq_class (i + 1))

    lambdaWithIncrement :: I.Body SOACS -> InternaliseM (I.Body SOACS)
    lambdaWithIncrement lam_body = runBodyBuilder $ do
      eq_class <- resSubExp . head <$> bodyBind lam_body
      resultBody <$> mkResult eq_class 0
