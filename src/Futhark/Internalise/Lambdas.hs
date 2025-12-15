module Futhark.Internalise.Lambdas
  ( InternaliseLambda,
    internaliseFoldLambda,
    internalisePartitionLambda,
  )
where

import Data.Maybe (listToMaybe)
import Futhark.IR.SOACS as I
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.Monad
import Language.Futhark as E

-- | A function for internalising lambdas.
type InternaliseLambda =
  E.Exp -> [I.Type] -> InternaliseM ([I.LParam SOACS], I.Body SOACS, [I.Type])

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
      rettype'
      =<< bodyBind body

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
  pure $ I.Lambda params rettype body'
  where
    rettype = replicate (k + 2) $ I.Prim int64
    result i =
      map constant $
        fromIntegral i
          : (replicate i 0 ++ [1 :: Int64] ++ replicate (k - i) 0)

    mkResult _ i | i >= k = pure $ result i
    mkResult eq_class i = do
      is_i <-
        letSubExp "is_i" $
          BasicOp $
            CmpOp (CmpEq int64) eq_class $
              intConst Int64 $
                toInteger i
      letTupExp' "part_res"
        =<< eIf
          (eSubExp is_i)
          (pure $ resultBody $ result i)
          (resultBody <$> mkResult eq_class (i + 1))

    lambdaWithIncrement :: I.Body SOACS -> InternaliseM (I.Body SOACS)
    lambdaWithIncrement lam_body = runBodyBuilder $ do
      eq_class <-
        maybe (intConst Int64 0) resSubExp . listToMaybe <$> bodyBind lam_body
      subExpsRes <$> mkResult eq_class 0
