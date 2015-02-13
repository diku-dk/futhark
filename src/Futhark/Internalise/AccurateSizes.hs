module Futhark.Internalise.AccurateSizes
  ( shapeBody
  , annotateArrayShape
  , argShapes
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

ensureShape :: MonadBinder m =>
               SrcLoc -> Type -> String -> SubExp
            -> m SubExp
ensureShape loc t name orig
  | Array{} <- t, Var v <- orig =
    Var <$> ensureShapeIdent loc t name v
  | otherwise = return orig

ensureShapeIdent :: MonadBinder m =>
                    SrcLoc -> Type -> String -> Ident
                 -> m Ident
ensureShapeIdent loc t name v
  | Array{} <- t = do
      let checkDim desired has =
            letExp "shape_cert" =<<
            eAssert (pure $ PrimOp $ BinOp Equal desired has Bool) loc
      certs <- zipWithM checkDim newshape oldshape
      letExp name $ PrimOp $ Reshape certs newshape v
  | otherwise = return v
  where newshape = arrayDims t
        oldshape = arrayDims $ identType v
