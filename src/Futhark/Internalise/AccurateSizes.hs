module Futhark.Internalise.AccurateSizes
  ( shapeBody
  , annotateArrayShape
  , argShapes
  , ensureResultShape
  , ensureResultExtShape
  , ensureShape
  , ensureShapeVar
  )
  where

import Control.Applicative
import Control.Monad
import Data.Loc
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.AST
import Futhark.Construct
import Futhark.MonadFreshNames

shapeBody :: (HasTypeEnv m, MonadFreshNames m, Bindable lore) =>
             [VName] -> [Type] -> Body lore
          -> m (Body lore)
shapeBody shapenames ts body =
  runBodyBinder $ do
    ses <- bodyBind body
    sets <- mapM subExpType ses
    return $ resultBody $ argShapes shapenames ts sets

annotateArrayShape :: ArrayShape shape =>
                      TypeBase shape -> [Int] -> TypeBase Shape
annotateArrayShape t newshape =
  t `setArrayShape` Shape (take (arrayRank t) (map intconst $ newshape ++ repeat 0))

argShapes :: [VName] -> [Type] -> [Type] -> [SubExp]
argShapes shapes valts valargts =
  map addShape shapes
  where mapping = shapeMapping valts valargts
        addShape name
          | Just se <- HM.lookup name mapping = se
          | otherwise                         = Constant (IntVal 0)

ensureResultShape :: (HasTypeEnv m, MonadFreshNames m, Bindable lore) =>
                     SrcLoc -> [Type] -> Body lore
                  -> m (Body lore)
ensureResultShape loc =
  ensureResultExtShape loc . staticShapes

ensureResultExtShape :: (HasTypeEnv m, MonadFreshNames m, Bindable lore) =>
                        SrcLoc -> [ExtType] -> Body lore
                     -> m (Body lore)
ensureResultExtShape loc rettype body =
  runBodyBinder $ insertBindingsM $ do
    es <- bodyBind body
    let assertProperShape t se =
          let name = "result_proper_shape"
          in ensureExtShape loc t name se
    reses <- zipWithM assertProperShape rettype es
    mkBodyM [] reses

ensureExtShape :: MonadBinder m =>
                  SrcLoc -> ExtType -> String -> SubExp
               -> m SubExp
ensureExtShape loc t name orig
  | Array{} <- t, Var v <- orig =
    Var <$> ensureShapeVar loc t name v
  | otherwise = return orig

ensureShape :: MonadBinder m =>
               SrcLoc -> Type -> String -> SubExp
            -> m SubExp
ensureShape loc = ensureExtShape loc . staticShapes1

ensureShapeVar :: MonadBinder m =>
                    SrcLoc -> ExtType -> String -> VName
                 -> m VName
ensureShapeVar loc t name v
  | Array{} <- t = do
    newshape <- arrayDims <$> removeExistentials t <$> lookupType v
    oldshape <- arrayDims <$> lookupType v
    let checkDim desired has =
          letExp "shape_cert" =<<
          eAssert (pure $ PrimOp $ BinOp Equal desired has Bool) loc
    certs <- zipWithM checkDim newshape oldshape
    letExp name $ PrimOp $ Reshape certs newshape v
  | otherwise = return v

removeExistentials :: ExtType -> Type -> Type
removeExistentials t1 t2 =
  t1 `setArrayDims`
  zipWith nonExistential
  (extShapeDims $ arrayShape t1)
  (arrayDims t2)
  where nonExistential (Ext _)    dim = dim
        nonExistential (Free dim) _   = dim
