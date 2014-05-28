module Futhark.Internalise.AccurateSizes
  ( subExpShape
  , identShapes
  , typeShapes
  , prefixTypeShapes
  , extShapes
  , shapeBody
  , prefixSubExpShapes
  , allEqual
  , UnsizedLambda(..)
  , annotateArrayShape
  , annotateIdents
  , addTypeShapes
  , addIdentShapes
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Loc

import Futhark.Internalise.Monad
import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.Tools

subExpShape :: SubExp -> [SubExp]
subExpShape = shapeDims . arrayShape . subExpType

prefixSubExpShapes :: [SubExp] -> [SubExp]
prefixSubExpShapes ses = concatMap subExpShape ses ++ ses

identShapes :: (MonadFreshNames m, ArrayShape shape) =>
               IdentBase Names shape -> m (Ident, [Ident])
identShapes v = do
  shape <- replicateM rank $ newIdent (base ++ "_size") (Basic Int) $ srclocOf v
  let vshape = Shape $ map Var shape
  return (v { identType = identType v `setArrayShape` vshape },
          shape)
  where base = textual $ baseName $ identName v
        rank = arrayRank $ identType v

typeShapes :: ArrayShape shape1 =>
              [TypeBase als1 shape1] -> [TypeBase als2 shape2]
typeShapes  = (`replicate` Basic Int) . sum . map arrayRank

prefixTypeShapes :: ArrayShape shape =>
                    [TypeBase als shape] -> [TypeBase als shape]
prefixTypeShapes ts = typeShapes ts ++ ts

extShapes :: ArrayShape shape =>
             [TypeBase als shape] -> [TypeBase als ExtShape]
extShapes ts = evalState (mapM extShapes' ts) 0
  where extShapes' t =
          setArrayShape t <$> ExtShape <$> replicateM (arrayRank t) newExt
        newExt = do
          x <- get
          put $ x + 1
          return $ Ext x

shapeBody :: Body -> Body
shapeBody (Body bnds (Result cs ses loc)) =
  Body bnds $ Result cs shapes loc
  where shapes = concatMap subExpShape ses

allEqual :: Ident -> InternaliseM (Ident, Ident)
allEqual comp_shape = do
  x <- newIdent "x" (Basic Int) loc
  y <- newIdent "y" (Basic Int) loc
  compFun <- makeLambda [toParam x, toParam y] $
             eBody [eAssert $ pure $
                    BinOp Equal (Var x) (Var y) (Basic Bool) loc]
  bacc <- newIdent "bacc" (Basic Cert) loc
  nacc <- newIdent "nacc" (Basic Int) loc
  belm <- newIdent "belm" (Basic Cert) loc
  nelm <- newIdent "nelm" (Basic Int) loc
  checkFun <- makeLambda (map toParam [bacc,nacc,belm,nelm]) $ eBody
              [ pure $ Conjoin [Var bacc, Var belm] loc
              , pure $ SubExp $ Var nelm ]
  comp_shape_rot1 <- letExp "comp_shape_rot1" $ Rotate [] 1 (Var comp_shape) loc
  comp <- letExp "map_size_checks" $
          Map [] compFun [Var comp_shape, Var comp_shape_rot1] loc
  cert <- newIdent "all_equal_cert" (Basic Cert) loc
  shape <- newIdent "all_equal_shape" (Basic Int) loc
  letBind [cert, shape] $
          Reduce [] checkFun [(Constant (BasicVal Checked) loc,Var comp),
                              (intconst 0 loc,Var comp_shape)] loc
  return (cert, shape)
  where loc  = srclocOf comp_shape

data UnsizedLambda = UnsizedLambda {
    unsizedLambdaParams     :: [Param]
  , unsizedLambdaBody       :: Body
  , unsizedLambdaReturnType :: [DeclType]
  , unsizedLambdaSrcLoc     :: SrcLoc
  }
  deriving (Eq, Ord, Show)

annotateArrayShape :: ArrayShape shape =>
                      TypeBase als shape -> ([Int], SrcLoc) -> TypeBase als Shape
annotateArrayShape t (newshape, loc) =
  t `setArrayShape` Shape (take (arrayRank t) (map (`intconst` loc) $ newshape ++ repeat 0))

addTypeShapes :: ArrayShape oldshape =>
                 [TypeBase als oldshape]
              -> [SubExp]
              -> [TypeBase als Shape]
addTypeShapes [] _ = []
addTypeShapes (t:ts) shapes =
  let (shape,shapes') = splitAt (arrayRank t) shapes
      t' = t `setArrayShape` Shape shape
  in t' : addTypeShapes ts shapes'

addIdentShapes :: ArrayShape oldshape =>
                  [IdentBase als oldshape]
               -> [SubExp]
               -> [IdentBase als Shape]
addIdentShapes [] _ = []
addIdentShapes (v:vs) shapes =
  let (shape,shapes') = splitAt (arrayRank $ identType v) shapes
      t' = identType v `setArrayShape` Shape shape
  in v { identType = t' } : addIdentShapes vs shapes'

addShapeAnnotations :: [IdentBase Names Rank] -> [Type] -> [Ident]
addShapeAnnotations = zipWith addShapeAnnotation
  where addShapeAnnotation v t =
          v { identType = identType v `setArrayShape` arrayShape t }

annotateIdents :: [IdentBase Names Rank]
               -> [Type] -> ([Ident], [Type])
annotateIdents vs ts =
  let (ts', rest_ts) = splitAt (length vs) ts
      vs' = addShapeAnnotations vs ts'
  in (vs', rest_ts)
