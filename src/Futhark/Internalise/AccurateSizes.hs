module Futhark.Internalise.AccurateSizes
  ( shapeBody
  , annotateArrayShape
  , argShapes
  , ensureResultShape
  , ensureResultExtShape
  , ensureShape
  , ensureShapeIdent
  )
  where

import Control.Applicative
import Control.Monad
import Data.Loc

import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.Basic
import Futhark.Tools

shapeBody :: [VName] -> [Type] -> Body -> Body
shapeBody shapenames ts (Body () bnds (Result ses)) =
  Body () bnds $ Result shapes
  where shapes = argShapes shapenames ts ses

annotateArrayShape :: ArrayShape shape =>
                      TypeBase shape -> [Int] -> TypeBase Shape
annotateArrayShape t newshape =
  t `setArrayShape` Shape (take (arrayRank t) (map intconst $ newshape ++ repeat 0))

argShapes :: [VName] -> [Type] -> [SubExp] -> [SubExp]
argShapes shapes valts valargs =
  map addShape shapes
  where mapping = shapeMapping valts $ map subExpType valargs
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
    Var <$> ensureShapeIdent loc t name v
  | otherwise = return orig

ensureShape :: MonadBinder m =>
               SrcLoc -> Type -> String -> SubExp
            -> m SubExp
ensureShape loc = ensureExtShape loc . staticShapes1

ensureShapeIdent :: MonadBinder m =>
                    SrcLoc -> ExtType -> String -> Ident
                 -> m Ident
ensureShapeIdent loc t name v
  | Array{} <- t = do
      let checkDim desired has =
            letExp "shape_cert" =<<
            eAssert (pure $ PrimOp $ BinOp Equal desired has Bool) loc
      certs <- zipWithM checkDim newshape oldshape
      letExp name $ PrimOp $ Reshape certs newshape v
  | otherwise = return v
  where newshape = arrayDims $ removeExistentials t $ identType v
        oldshape = arrayDims $ identType v

removeExistentials :: ExtType -> Type -> Type
removeExistentials t1 t2 =
  t1 `setArrayDims`
  zipWith nonExistential
  (extShapeDims $ arrayShape t1)
  (arrayDims t2)
  where nonExistential (Ext _)    dim = dim
        nonExistential (Free dim) _   = dim
