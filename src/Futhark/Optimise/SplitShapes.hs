-- | For every function with an existential return shape, try to see
-- if we can extract an efficient shape slice.  If so, replace every
-- call of the original function with a function to the shape and
-- value slices.
module Futhark.Optimise.SplitShapes
       (splitShapes)
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.HashMap.Lazy as HM
import Data.Maybe

import Futhark.InternalRep
import Futhark.Tools
import Futhark.MonadFreshNames
import Futhark.Substitute

-- | Perform the transformation on a program.
splitShapes :: Prog -> Prog
splitShapes prog =
  Prog $ progFunctions prog ++ evalState m (newNameSourceForProg prog)
  where m :: State VNameSource [FunDec]
        m = do (fShapes, fValues) <-
                 unzip <$> mapM functionSlices (progFunctions prog)
               return $ fShapes ++ fValues

-- | Returns shape slice and value slice.  The shape slice duplicates
-- the entire value slice - you should try to simplify it, and see if
-- it's "cheap", in some sense.
functionSlices :: MonadFreshNames m => FunDec -> m (FunDec, FunDec)
functionSlices (fname, rettype, params, body@(Body bodybnds bodyres), loc) = do
  -- The shape function should not consume its arguments - if it wants
  -- to do in-place stuff, it needs to copy them first.  In most
  -- cases, these copies will be removed by the simplifier.
  (shapeParams, cpybnds) <- nonuniqueParams params

  -- Give names to the existentially quantified sizes of the return
  -- type.  These will be passed as parameters to the value function.
  (staticRettype, shapeidents) <-
    runWriterT $ map (`setAliases` ()) <$> instantiateShapes instantiate rettype

  valueBody <- substituteExtResultShapes staticRettype body

  let valueRettype = staticShapes staticRettype
      valueParams = map toParam shapeidents ++ params
      shapeBody = Body (cpybnds <> bodybnds) bodyres { resultSubExps = shapes }
      fShape = (shapeFname, shapeRettype, shapeParams, shapeBody, loc)
      fValue = (valueFname, valueRettype, valueParams, valueBody, loc)
  return (fShape, fValue)
  where shapes = subExpShapeContext rettype $ resultSubExps bodyres
        shapeRettype = staticShapes $ map ((`setAliases` ()) . subExpType) shapes
        shapeFname = fname <> nameFromString "_shape"
        valueFname = fname <> nameFromString "_value"

        instantiate = do v <- lift $ newIdent "precomp_shape" (Basic Int) loc
                         tell [v]
                         return $ Var v

substituteExtResultShapes :: MonadFreshNames m => [ConstType] -> Body -> m Body
substituteExtResultShapes rettype (Body bnds res) = do
  bnds' <- mapM substInBnd bnds
  let res' = res { resultSubExps = map (substituteNames subst) $
                                   resultSubExps res
                 }
  return $ Body bnds' res'
  where typesShapes = concatMap (shapeDims . arrayShape)
        compshapes =
          typesShapes $ map subExpType $ resultSubExps res
        subst =
          HM.fromList $ mapMaybe isSubst $ zip compshapes (typesShapes rettype)
        isSubst (Var v1, Var v2) = Just (identName v1, identName v2)
        isSubst _                = Nothing

        substInBnd (Let pat e) =
          Let <$> mapM substInBnd' pat <*> pure (substituteNames subst e)
        substInBnd' v
          | identName v `HM.member` subst = newIdent' (<>"unused") v
          | otherwise                     = return v
