{-# LANGUAGE FlexibleContexts #-}
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

shapeBody :: (HasScope lore m, MonadFreshNames m, Bindable lore) =>
             [VName] -> [Type] -> Body lore
          -> m (Body lore)
shapeBody shapenames ts body =
  runBodyBinder $ do
    ses <- bodyBind body
    sets <- mapM subExpType ses
    return $ resultBody $ argShapes shapenames ts sets

annotateArrayShape :: ArrayShape shape =>
                      TypeBase shape u -> [Int] -> TypeBase Shape u
annotateArrayShape t newshape =
  t `setArrayShape` Shape (take (arrayRank t) $
                           map (intConst Int32 . toInteger) $ newshape ++ repeat 0)

argShapes :: [VName] -> [TypeBase Shape u0] -> [TypeBase Shape u1] -> [SubExp]
argShapes shapes valts valargts =
  map addShape shapes
  where mapping = shapeMapping valts valargts
        addShape name
          | Just se <- HM.lookup name mapping = se
          | otherwise                         = intConst Int32 0

ensureResultShape :: MonadBinder m =>
                     (m Certificates -> m Certificates)
                  -> SrcLoc -> [Type] -> Body (Lore m)
                  -> m (Body (Lore m))
ensureResultShape asserting loc =
  ensureResultExtShape asserting loc . staticShapes

ensureResultExtShape :: MonadBinder m =>
                        (m Certificates -> m Certificates)
                     -> SrcLoc -> [ExtType] -> Body (Lore m)
                     -> m (Body (Lore m))
ensureResultExtShape asserting loc rettype body =
  insertBindingsM $ do
    es <- bodyBind body
    let assertProperShape t se =
          let name = "result_proper_shape"
          in ensureExtShape asserting loc t name se
    reses <- zipWithM assertProperShape rettype es
    mkBodyM [] reses

ensureExtShape :: MonadBinder m =>
                  (m Certificates -> m Certificates)
               -> SrcLoc -> ExtType -> String -> SubExp
               -> m SubExp
ensureExtShape asserting loc t name orig
  | Array{} <- t, Var v <- orig =
    Var <$> ensureShapeVar asserting loc t name v
  | otherwise = return orig

ensureShape :: MonadBinder m =>
               (m Certificates -> m Certificates)
            -> SrcLoc -> Type -> String -> SubExp
            -> m SubExp
ensureShape asserting loc = ensureExtShape asserting loc . staticShapes1

ensureShapeVar :: MonadBinder m =>
                  (m Certificates -> m Certificates)
               -> SrcLoc -> ExtType -> String -> VName
               -> m VName
ensureShapeVar asserting loc t name v
  | Array{} <- t = do
    newshape <- arrayDims . removeExistentials t <$> lookupType v
    oldshape <- arrayDims <$> lookupType v
    let checkDim desired has =
          letExp "shape_cert" =<<
          eAssert (pure $ PrimOp $ CmpOp (CmpEq int32) desired has) loc
    certs <- asserting $ zipWithM checkDim newshape oldshape
    letExp name $ shapeCoerce certs newshape v
  | otherwise = return v

removeExistentials :: ExtType -> Type -> Type
removeExistentials t1 t2 =
  t1 `setArrayDims`
  zipWith nonExistential
  (extShapeDims $ arrayShape t1)
  (arrayDims t2)
  where nonExistential (Ext _)    dim = dim
        nonExistential (Free dim) _   = dim
