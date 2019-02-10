{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.AccurateSizes
  ( shapeBody
  , annotateArrayShape
  , argShapes
  , ensureResultShape
  , ensureResultExtShape
  , ensureResultExtShapeNoCtx
  , ensureExtShape
  , ensureShape
  , ensureArgShapes
  )
  where

import Control.Monad
import Data.Loc
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Futhark.Construct
import Futhark.Representation.AST

shapeBody :: (HasScope lore m, MonadFreshNames m, BinderOps lore, Bindable lore) =>
             [VName] -> [Type] -> Body lore
          -> m (Body lore)
shapeBody shapenames ts body =
  runBodyBinder $ do
    ses <- bodyBind body
    sets <- mapM subExpType ses
    resultBody <$> argShapes shapenames ts sets

annotateArrayShape :: ArrayShape shape =>
                      TypeBase shape u -> [Int] -> TypeBase Shape u
annotateArrayShape t newshape =
  t `setArrayShape` Shape (take (arrayRank t) $
                           map (intConst Int32 . toInteger) $ newshape ++ repeat 0)

-- Some trickery is needed here to predict sensible values for
-- dimensions that are used exclusively as the inner dimension of an
-- array.  The issue is that the dimension may be inside an empty
-- array.  In this case, the dimension inside the empty array should
-- not count, as it will be zero.  The solution we use is to take the
-- maximum of such sizes; this will effectively disregard the zeroes.
argShapes :: MonadBinder m =>
             [VName] -> [TypeBase Shape u0] -> [TypeBase Shape u1] -> m [SubExp]
argShapes shapes valts valargts =
  mapM addShape shapes
  where mapping = shapeMapping valts valargts
        outer_dims = map (arraySize 0) valts
        addShape name =
          case M.lookup name mapping of
            Just s | x:xs <- S.toList s ->
                       if Var name `elem` outer_dims
                       then return x
                       else letSubExp "size" =<< foldBinOp (SMax Int32) x xs
            _ -> return $ intConst Int32 0

ensureResultShape :: MonadBinder m =>
                     (m Certificates -> m Certificates)
                  -> ErrorMsg SubExp -> SrcLoc -> [Type] -> Body (Lore m)
                  -> m (Body (Lore m))
ensureResultShape asserting msg loc =
  ensureResultExtShape asserting msg loc . staticShapes

ensureResultExtShape :: MonadBinder m =>
                        (m Certificates -> m Certificates)
                     -> ErrorMsg SubExp -> SrcLoc -> [ExtType] -> Body (Lore m)
                     -> m (Body (Lore m))
ensureResultExtShape asserting msg loc rettype body =
  insertStmsM $ do
    reses <- bodyBind =<<
             ensureResultExtShapeNoCtx asserting msg loc rettype body
    ts <- mapM subExpType reses
    let ctx = extractShapeContext rettype $ map arrayDims ts
    mkBodyM mempty $ ctx ++ reses

ensureResultExtShapeNoCtx :: MonadBinder m =>
                             (m Certificates -> m Certificates)
                          -> ErrorMsg SubExp -> SrcLoc -> [ExtType] -> Body (Lore m)
                          -> m (Body (Lore m))
ensureResultExtShapeNoCtx asserting msg loc rettype body =
  insertStmsM $ do
    es <- bodyBind body
    es_ts <- mapM subExpType es
    let ext_mapping = shapeExtMapping rettype es_ts
        rettype' = foldr (uncurry fixExt) rettype $ M.toList ext_mapping
        assertProperShape t se =
          let name = "result_proper_shape"
          in ensureExtShape asserting msg loc t name se
    resultBodyM =<< zipWithM assertProperShape rettype' es

ensureExtShape :: MonadBinder m =>
                  (m Certificates -> m Certificates)
               -> ErrorMsg SubExp -> SrcLoc -> ExtType -> String -> SubExp
               -> m SubExp
ensureExtShape asserting msg loc t name orig
  | Array{} <- t, Var v <- orig =
    Var <$> ensureShapeVar asserting msg loc t name v
  | otherwise = return orig

ensureShape :: MonadBinder m =>
               (m Certificates -> m Certificates)
            -> ErrorMsg SubExp -> SrcLoc -> Type -> String -> SubExp
            -> m SubExp
ensureShape asserting msg loc = ensureExtShape asserting msg loc . staticShapes1

-- | Reshape the arguments to a function so that they fit the expected
-- shape declarations.  Not used to change rank of arguments.  Assumes
-- everything is otherwise type-correct.
ensureArgShapes :: (MonadBinder m, Typed (TypeBase Shape u)) =>
                   (m Certificates -> m Certificates)
                -> ErrorMsg SubExp -> SrcLoc -> [VName] -> [TypeBase Shape u] -> [SubExp]
                -> m [SubExp]
ensureArgShapes asserting msg loc shapes paramts args =
  zipWithM ensureArgShape (expectedTypes shapes paramts args) args
  where ensureArgShape _ (Constant v) = return $ Constant v
        ensureArgShape t (Var v)
          | arrayRank t < 1 = return $ Var v
          | otherwise =
              ensureShape asserting msg loc t (baseString v) $ Var v


ensureShapeVar :: MonadBinder m =>
                  (m Certificates -> m Certificates)
               -> ErrorMsg SubExp -> SrcLoc -> ExtType -> String -> VName
               -> m VName
ensureShapeVar asserting msg loc t name v
  | Array{} <- t = do
    newdims <- arrayDims . removeExistentials t <$> lookupType v
    olddims <- arrayDims <$> lookupType v
    if newdims == olddims
      then return v
      else do
        certs <- asserting $ do
          old_zero <- letSubExp "old_empty" =<< anyZero olddims
          new_zero <- letSubExp "new_empty" =<< anyZero newdims
          both_empty <- letSubExp "both_empty" $ BasicOp $ BinOp LogAnd old_zero new_zero

          matches <- zipWithM checkDim newdims olddims
          all_match <- letSubExp "match" =<< foldBinOp LogAnd (constant True) matches

          empty_or_match <- letSubExp "empty_or_match" $ BasicOp $ BinOp LogOr both_empty all_match
          Certificates . pure <$> letExp "empty_or_match_cert"
            (BasicOp $ Assert empty_or_match msg (loc, []))
        certifying certs $ letExp name $ shapeCoerce newdims v
  | otherwise = return v
  where checkDim desired has =
          letSubExp "dim_match" $ BasicOp $ CmpOp (CmpEq int32) desired has
        anyZero =
          foldBinOp LogOr (constant False) <=<
          mapM (letSubExp "dim_zero" . BasicOp . CmpOp (CmpEq int32) (intConst Int32 0))
