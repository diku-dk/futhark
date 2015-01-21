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
shapeBody shapenames ts (Body () bnds (Result ses loc)) =
  Body () bnds $ Result shapes loc
  where shapes = argShapes shapenames ts ses

annotateArrayShape :: ArrayShape shape =>
                      TypeBase shape -> ([Int], SrcLoc) -> TypeBase Shape
annotateArrayShape t (newshape, loc) =
  t `setArrayShape` Shape (take (arrayRank t) (map (`intconst` loc) $ newshape ++ repeat 0))

argShapes :: [VName] -> [Type] -> [SubExp] -> [SubExp]
argShapes shapes valts valargs =
  map addShape shapes
  where mapping = shapeMapping valts $ map subExpType valargs
        addShape name
          | Just se <- HM.lookup name mapping = se
          | otherwise                         = Constant (IntVal 0) noLoc

ensureShape :: MonadBinder m =>
               Type -> String -> SubExp
            -> m SubExp
ensureShape t name orig
  | Array{} <- t, Var v <- orig =
    Var <$> ensureShapeIdent t name v
  | otherwise = return orig

ensureShapeIdent :: MonadBinder m =>
                    Type -> String -> Ident
                 -> m Ident
ensureShapeIdent t name v
  | Array{} <- t = do
      let checkDim desired has =
            letExp "shape_cert" =<<
            eAssert (pure $ PrimOp $ BinOp Equal desired has (Basic Bool) loc)
      certs <- zipWithM checkDim newshape oldshape
      letExp name $ PrimOp $ Reshape certs newshape v loc
  | otherwise = return v
  where newshape = arrayDims t
        oldshape = arrayDims $ identType v
        loc = srclocOf v
