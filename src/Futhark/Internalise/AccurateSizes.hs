module Futhark.Internalise.AccurateSizes
  ( subExpShape
  , identWithShapes
  , typeShapes
  , prefixTypeShapes
  , extShapes
  , shapeBody
  , prefixSubExpShapes
  , prefixArgShapes
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
import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import Futhark.Tools

subExpShape :: SubExp -> [SubExp]
subExpShape = shapeDims . arrayShape . subExpType

prefixSubExpShapes :: [SubExp] -> [SubExp]
prefixSubExpShapes ses = concatMap subExpShape ses ++ ses

prefixArgShapes :: [(SubExp, Diet)] -> [(SubExp,Diet)]
prefixArgShapes args =
  [ (shape, Observe) |
    shape <- concatMap (subExpShape . fst) args ] ++
  args

identWithShapes :: (MonadFreshNames m, ArrayShape shape) =>
                   IdentBase shape -> m (Ident, [Ident])
identWithShapes v = do
  shape <- replicateM rank $ newIdent (base ++ "_size") (Basic Int) $ srclocOf v
  let vshape = Shape $ map Var shape
  return (v { identType = identType v `setArrayShape` vshape },
          shape)
  where base = textual $ baseName $ identName v
        rank = arrayRank $ identType v

typeShapes :: ArrayShape shape1 =>
              [TypeBase shape1] -> [TypeBase shape2]
typeShapes  = (`replicate` Basic Int) . sum . map arrayRank

prefixTypeShapes :: ArrayShape shape =>
                    [TypeBase shape] -> [TypeBase shape]
prefixTypeShapes ts = typeShapes ts ++ ts

extShapes :: ArrayShape shape =>
             [TypeBase shape] -> [TypeBase ExtShape]
extShapes ts = evalState (mapM extShapes' ts) 0
  where extShapes' t =
          setArrayShape t <$> ExtShape <$> replicateM (arrayRank t) newExt
        newExt = do
          x <- get
          put $ x + 1
          return $ Ext x

shapeBody :: Body -> Body
shapeBody (Body () bnds (Result cs ses loc)) =
  Body () bnds $ Result cs shapes loc
  where shapes = concatMap subExpShape ses

allEqual :: Ident -> InternaliseM (Ident, Ident)
allEqual comp_shape = do
  x <- newIdent "x" (Basic Int) loc
  y <- newIdent "y" (Basic Int) loc
  compFun <- makeLambda [x, y] $
             eBody [eAssert $ pure $
                    PrimOp $ BinOp Equal (Var x) (Var y) (Basic Bool) loc]
  bacc <- newIdent "bacc" (Basic Cert) loc
  nacc <- newIdent "nacc" (Basic Int) loc
  belm <- newIdent "belm" (Basic Cert) loc
  nelm <- newIdent "nelm" (Basic Int) loc
  checkFun <- makeLambda [bacc,nacc,belm,nelm] $ eBody
              [ pure $ PrimOp $ Conjoin [Var bacc, Var belm] loc
              , pure $ PrimOp $ SubExp $ Var nelm ]
  comp_shape_rot1 <- letExp "comp_shape_rot1" $
                     PrimOp $ Rotate [] 1 comp_shape loc
  comp <- letExp "map_size_checks" $
          LoopOp $ Map [] compFun [comp_shape, comp_shape_rot1] loc
  cert <- newIdent "all_equal_cert" (Basic Cert) loc
  shape <- newIdent "all_equal_shape" (Basic Int) loc
  letBindNames_ [identName cert, identName shape] $
    LoopOp $ Reduce [] checkFun [(Constant Checked loc,comp),
                                 (intconst 0 loc,comp_shape)] loc
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
                      TypeBase shape -> ([Int], SrcLoc) -> TypeBase Shape
annotateArrayShape t (newshape, loc) =
  t `setArrayShape` Shape (take (arrayRank t) (map (`intconst` loc) $ newshape ++ repeat 0))

addTypeShapes :: ArrayShape oldshape =>
                 [TypeBase oldshape]
              -> [SubExp]
              -> [TypeBase Shape]
addTypeShapes [] _ = []
addTypeShapes (t:ts) shapes =
  let (shape,shapes') = splitAt (arrayRank t) shapes
      t' = t `setArrayShape` Shape shape
  in t' : addTypeShapes ts shapes'

addIdentShapes :: ArrayShape oldshape =>
                  [IdentBase oldshape]
               -> [SubExp]
               -> [IdentBase Shape]
addIdentShapes [] _ = []
addIdentShapes (v:vs) shapes =
  let (shape,shapes') = splitAt (arrayRank $ identType v) shapes
      t' = identType v `setArrayShape` Shape shape
  in v { identType = t' } : addIdentShapes vs shapes'

addShapeAnnotations :: [IdentBase Rank] -> [Type] -> [Ident]
addShapeAnnotations = zipWith addShapeAnnotation
  where addShapeAnnotation v t =
          v { identType = identType v `setArrayShape` arrayShape t }

annotateIdents :: [IdentBase Rank]
               -> [Type] -> ([Ident], [Type])
annotateIdents vs ts =
  let (ts', rest_ts) = splitAt (length vs) ts
      vs' = addShapeAnnotations vs ts'
  in (vs', rest_ts)
