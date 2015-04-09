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
import Control.Monad.Reader
import Data.Loc

import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.Basic
import Futhark.Tools

shapeBody :: (HasTypeEnv m, Monad m) => [VName] -> [Type] -> Body -> m Body
shapeBody shapenames ts (Body () bnds (Result ses)) = do
  types <- askTypeEnv
  let types' = typeEnvFromBindings bnds `HM.union` types
      sets = runReader (mapM subExpType ses) types'
  return $ Body () bnds $ Result $ argShapes shapenames ts sets

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

ensureResultShape :: MonadBinder m =>
                     SrcLoc -> [Type] -> Body
                  -> m Body
ensureResultShape loc =
  ensureResultExtShape loc . staticShapes

ensureResultExtShape :: MonadBinder m =>
                        SrcLoc -> [ExtType] -> Body
                     -> m Body
ensureResultExtShape loc rettype body = runBinder $ do
  es <- bodyBind body
  let assertProperShape t se =
        let name = "result_proper_shape"
        in ensureExtShape loc t name se
  reses <- zipWithM assertProperShape rettype es
  return $ resultBody reses

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
    newshape <- arrayDims <$> removeExistentials t <$> lookupTypeM v
    oldshape <- arrayDims <$> lookupTypeM v
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
