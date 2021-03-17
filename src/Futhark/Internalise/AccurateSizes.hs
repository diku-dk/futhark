{-# LANGUAGE FlexibleContexts #-}

module Futhark.Internalise.AccurateSizes
  ( argShapes,
    ensureResultShape,
    ensureResultExtShape,
    ensureExtShape,
    ensureShape,
    ensureArgShapes,
  )
where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Construct
import Futhark.IR.SOACS
import Futhark.Internalise.Monad
import Futhark.Util (takeLast)

shapeMapping ::
  (HasScope SOACS m, Monad m) =>
  [FParam] ->
  [Type] ->
  m (M.Map VName SubExp)
shapeMapping all_params value_arg_types =
  mconcat <$> zipWithM f (map paramType value_params) value_arg_types
  where
    value_params = takeLast (length value_arg_types) all_params

    f t1@Array {} t2@Array {} =
      pure $ M.fromList $ mapMaybe match $ zip (arrayDims t1) (arrayDims t2)
    f (Acc acc1 ispace1 arrs1_ts) (Acc acc2 ispace2 arrs2_ts) = do
      let ispace_m =
            M.fromList $ mapMaybe match $ zip (sliceDims ispace1) (sliceDims ispace2)
      arr_sizes_m <- mconcat <$> zipWithM f arrs1_ts arrs2_ts
      pure $ M.singleton acc1 (Var acc2) <> ispace_m <> arr_sizes_m
    f _ _ =
      pure mempty

    match (Var v, se) = Just (v, se)
    match _ = Nothing

argShapes ::
  (HasScope SOACS m, Monad m) =>
  [VName] ->
  [FParam] ->
  [Type] ->
  m [SubExp]
argShapes shapes all_params valargts = do
  mapping <- shapeMapping all_params valargts
  let addShape name =
        case M.lookup name mapping of
          Just se -> se
          _ -> intConst Int64 0 -- FIXME: we only need this because
          -- the defunctionaliser throws away
          -- sizes.
  return $ map addShape shapes

ensureResultShape ::
  ErrorMsg SubExp ->
  SrcLoc ->
  [Type] ->
  Body ->
  InternaliseM Body
ensureResultShape msg loc =
  ensureResultExtShape msg loc . staticShapes

ensureResultExtShape ::
  ErrorMsg SubExp ->
  SrcLoc ->
  [ExtType] ->
  Body ->
  InternaliseM Body
ensureResultExtShape msg loc rettype body =
  insertStmsM $ do
    reses <-
      bodyBind
        =<< ensureResultExtShapeNoCtx msg loc rettype body
    ts <- mapM subExpType reses
    let ctx = extractShapeContext rettype $ map arrayDims ts
    mkBodyM mempty $ ctx ++ reses

ensureResultExtShapeNoCtx ::
  ErrorMsg SubExp ->
  SrcLoc ->
  [ExtType] ->
  Body ->
  InternaliseM Body
ensureResultExtShapeNoCtx msg loc rettype body =
  insertStmsM $ do
    es <- bodyBind body
    es_ts <- mapM subExpType es
    let ext_mapping = shapeExtMapping rettype es_ts
        rettype' = foldr (uncurry fixExt) rettype $ M.toList ext_mapping
        assertProperShape t se =
          let name = "result_proper_shape"
           in ensureExtShape msg loc t name se
    resultBodyM =<< zipWithM assertProperShape rettype' es

ensureExtShape ::
  ErrorMsg SubExp ->
  SrcLoc ->
  ExtType ->
  String ->
  SubExp ->
  InternaliseM SubExp
ensureExtShape msg loc t name orig
  | Array {} <- t,
    Var v <- orig =
    Var <$> ensureShapeVar msg loc t name v
  | otherwise = return orig

ensureShape ::
  ErrorMsg SubExp ->
  SrcLoc ->
  Type ->
  String ->
  SubExp ->
  InternaliseM SubExp
ensureShape msg loc = ensureExtShape msg loc . staticShapes1

-- | Reshape the arguments to a function so that they fit the expected
-- shape declarations.  Not used to change rank of arguments.  Assumes
-- everything is otherwise type-correct.
ensureArgShapes ::
  (Typed (TypeBase Shape u)) =>
  ErrorMsg SubExp ->
  SrcLoc ->
  [VName] ->
  [TypeBase Shape u] ->
  [SubExp] ->
  InternaliseM [SubExp]
ensureArgShapes msg loc shapes paramts args =
  zipWithM ensureArgShape (expectedTypes shapes paramts args) args
  where
    ensureArgShape _ (Constant v) = return $ Constant v
    ensureArgShape t (Var v)
      | arrayRank t < 1 = return $ Var v
      | otherwise =
        ensureShape msg loc t (baseString v) $ Var v

ensureShapeVar ::
  ErrorMsg SubExp ->
  SrcLoc ->
  ExtType ->
  String ->
  VName ->
  InternaliseM VName
ensureShapeVar msg loc t name v
  | Array {} <- t = do
    newdims <- arrayDims . removeExistentials t <$> lookupType v
    olddims <- arrayDims <$> lookupType v
    if newdims == olddims
      then return v
      else do
        matches <- zipWithM checkDim newdims olddims
        all_match <- letSubExp "match" =<< eAll matches
        cs <- assert "empty_or_match_cert" all_match msg loc
        certifying cs $ letExp name $ shapeCoerce newdims v
  | otherwise = return v
  where
    checkDim desired has =
      letSubExp "dim_match" $ BasicOp $ CmpOp (CmpEq int64) desired has
