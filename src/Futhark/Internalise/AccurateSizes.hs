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
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Construct
import Futhark.IR.SOACS
import Futhark.Internalise.Monad
import Futhark.Util (takeLast)

shapeMapping ::
  (HasScope SOACS m, Monad m) =>
  [FParam SOACS] ->
  [Type] ->
  m (M.Map VName SubExp)
shapeMapping all_params value_arg_types =
  mconcat <$> zipWithM f (map paramType value_params) value_arg_types
  where
    value_params = takeLast (length value_arg_types) all_params

    f t1@Array {} t2@Array {} =
      pure $ M.fromList $ mapMaybe match $ zip (arrayDims t1) (arrayDims t2)
    f (Acc acc1 ispace1 ts1 _) (Acc acc2 ispace2 ts2 _) = do
      let ispace_m =
            M.fromList . mapMaybe match $
              zip (shapeDims ispace1) (shapeDims ispace2)
      arr_sizes_m <- mconcat <$> zipWithM f ts1 ts2
      pure $ M.singleton acc1 (Var acc2) <> ispace_m <> arr_sizes_m
    f _ _ =
      pure mempty

    match (Var v, se) = Just (v, se)
    match _ = Nothing

argShapes :: [VName] -> [FParam SOACS] -> [Type] -> InternaliseM [SubExp]
argShapes shapes all_params valargts = do
  mapping <- shapeMapping all_params valargts
  let addShape name =
        case M.lookup name mapping of
          Just se -> se
          _ -> error $ "argShapes: " ++ prettyString name
  pure $ map addShape shapes

ensureResultShape ::
  ErrorMsg SubExp ->
  [Type] ->
  Result ->
  InternaliseM Result
ensureResultShape msg =
  ensureResultExtShape msg . staticShapes

ensureResultExtShape ::
  ErrorMsg SubExp ->
  [ExtType] ->
  Result ->
  InternaliseM Result
ensureResultExtShape msg rettype res = do
  res' <- ensureResultExtShapeNoCtx msg rettype res
  ts <- mapM subExpResType res'
  let ctx = extractShapeContext rettype $ map arrayDims ts
  pure $ subExpsRes ctx ++ res'

ensureResultExtShapeNoCtx ::
  ErrorMsg SubExp ->
  [ExtType] ->
  Result ->
  InternaliseM Result
ensureResultExtShapeNoCtx msg rettype es = do
  es_ts <- mapM subExpResType es
  let ext_mapping = shapeExtMapping rettype es_ts
      rettype' = foldr (uncurry fixExt) rettype $ M.toList ext_mapping
      assertProperShape t (SubExpRes cs se) =
        let name = "result_proper_shape"
         in SubExpRes cs <$> ensureExtShape msg t name se
  zipWithM assertProperShape rettype' es

ensureExtShape ::
  ErrorMsg SubExp ->
  ExtType ->
  String ->
  SubExp ->
  InternaliseM SubExp
ensureExtShape msg t name orig
  | Array {} <- t,
    Var v <- orig =
      Var <$> ensureShapeVar msg t name v
  | otherwise = pure orig

ensureShape ::
  ErrorMsg SubExp ->
  Type ->
  String ->
  SubExp ->
  InternaliseM SubExp
ensureShape msg = ensureExtShape msg . staticShapes1

-- | Reshape the arguments to a function so that they fit the expected
-- shape declarations.  Not used to change rank of arguments.  Assumes
-- everything is otherwise type-correct.
ensureArgShapes ::
  (Typed (TypeBase Shape u)) =>
  ErrorMsg SubExp ->
  [VName] ->
  [TypeBase Shape u] ->
  [SubExp] ->
  InternaliseM [SubExp]
ensureArgShapes msg shapes paramts args =
  zipWithM ensureArgShape (expectedTypes shapes paramts args) args
  where
    ensureArgShape _ (Constant v) = pure $ Constant v
    ensureArgShape t (Var v)
      | arrayRank t < 1 = pure $ Var v
      | otherwise =
          ensureShape msg t (baseString v) $ Var v

ensureShapeVar ::
  ErrorMsg SubExp ->
  ExtType ->
  String ->
  VName ->
  InternaliseM VName
ensureShapeVar msg t name v
  | Array {} <- t = do
      newdims <- arrayDims . removeExistentials t <$> lookupType v
      olddims <- arrayDims <$> lookupType v
      if newdims == olddims
        then pure v
        else do
          matches <- zipWithM checkDim newdims olddims
          all_match <- letSubExp "match" =<< eAll matches
          cs <- assert "empty_or_match_cert" all_match msg
          certifying cs $ letExp name $ shapeCoerce newdims v
  | otherwise = pure v
  where
    checkDim desired has =
      letSubExp "dim_match" $ BasicOp $ CmpOp (CmpEq int64) desired has
