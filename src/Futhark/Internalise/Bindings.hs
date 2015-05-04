{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.Bindings
  (
  -- * Internalising bindings
    bindingParams
  , bindingLambdaParams

  , flattenPattern
  , bindingTupIdent
  , bindingFlatPattern
  )
  where

import Control.Applicative
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Traversable (mapM)

import Futhark.Representation.External as E
import Futhark.Representation.Basic as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.TypesValues

import Prelude hiding (mapM)

internaliseParams :: [E.Parameter]
                  -> InternaliseM (HM.HashMap VName Int,
                                   [[(VName, I.ExtType)]])
internaliseParams params = do
  (param_ts, ctx) <- internaliseDeclTypes $ map E.identType params
  vss <- forM (zip params param_ts) $ \(param, ts) -> do
    let base = nameToString $ baseName $ E.identName param
    forM ts $ \t -> do
      name <- newVName base
      return (name, t)
  return (ctx, vss)

internaliseBindee :: MonadFreshNames m =>
                     E.Ident
                  -> m [(VName, I.ExtType)]
internaliseBindee bindee =
  forM (internaliseType $ E.identType bindee) $ \t -> do
    name <- newVName base
    return (name, t)
  where base = nameToString $ baseName $ E.identName bindee

internaliseFunParams :: [E.Parameter]
                     -> InternaliseM ([I.Ident],
                                      [I.Ident],
                                      VarSubstitutions)
internaliseFunParams params = do
  (shapectx, param_params) <- internaliseParams params

  (shapectx', shapesubst) <- makeShapeIdentsFromContext shapectx
  let declared_shape_params = HM.elems shapectx'

  (implicit_shape_params, value_params) <-
    liftM unzip $ forM param_params $ \params' -> do
    (instantiated_param_types, param_implicit_shapes) <-
      instantiateShapesWithDecls shapectx' $ map snd params'
    let instantiated_params =
          [ I.Ident new_param_name t |
            ((new_param_name, _), t) <- zip params' instantiated_param_types ]
    return (param_implicit_shapes, instantiated_params)
  let subst = HM.fromList $ zip (map E.identName params) (map (map I.identName) value_params)
  return (declared_shape_params ++ concat implicit_shape_params,
          concat value_params,
          subst <> shapesubst)

bindingParams :: [E.Parameter]
              -> ([I.Ident] -> [I.Ident] -> InternaliseM a)
              -> InternaliseM a
bindingParams params m = do
  (shapeparams, valueparams, substs) <- internaliseFunParams params
  let bind env = env { envSubsts = substs `HM.union` envSubsts env }
  local bind $
    bindingIdentTypes (shapeparams++valueparams) $
    m shapeparams valueparams

bindingFlatPattern :: [E.Ident] -> [I.Type]
                   -> ([I.Ident] -> InternaliseM a)
                   -> InternaliseM a
bindingFlatPattern = bindingFlatPattern' []
  where
    bindingFlatPattern' pat []       _  m = do
      let (vs, substs) = unzip pat
          substs' = HM.fromList substs
          idents = concat $ reverse vs
      local (\env -> env { envSubsts = substs' `HM.union` envSubsts env}) $
        m idents

    bindingFlatPattern' pat (p:rest) ts m = do
      (ps, subst, rest_ts) <- handleMapping ts <$> internaliseBindee p
      bindingFlatPattern' ((ps, (E.identName p, map I.identName subst)) : pat) rest rest_ts m

    handleMapping ts [] =
      ([], [], ts)
    handleMapping ts (r:rs) =
        let (ps, reps, ts')    = handleMapping' ts r
            (pss, repss, ts'') = handleMapping ts' rs
        in (ps++pss, reps:repss, ts'')

    handleMapping' (t:ts) (vname,vt) =
      let u = I.uniqueness vt
          v' = I.Ident vname $ t `I.setUniqueness` u
      in ([v'], v', ts)
    handleMapping' [] _ =
      error "bindingFlatPattern: insufficient identifiers in pattern."

flattenPattern :: MonadFreshNames m => E.TupIdent -> m [E.Ident]
flattenPattern (E.Wildcard t loc) = do
  name <- newVName "nameless"
  return [E.Ident name t loc]
flattenPattern (E.Id v) =
  return [v]
flattenPattern (E.TupId pats _) =
  concat <$> mapM flattenPattern pats

bindingTupIdent :: E.TupIdent -> [ExtType] -> (I.Pattern -> InternaliseM a)
                -> InternaliseM a
bindingTupIdent pat ts m = do
  pat' <- flattenPattern pat
  (ts',shapes) <- instantiateShapes' ts
  let addShapeBindings pat'' = m $ I.basicPattern' shapes pat''
  bindingFlatPattern pat' ts' addShapeBindings

bindingLambdaParams :: [E.Parameter] -> [I.Type]
                    -> InternaliseM I.Body
                    -> InternaliseM (I.Body, [I.Ident])
bindingLambdaParams params ts m =
  bindingFlatPattern (map E.fromParam params) ts $ \params' ->
  bindingIdentTypes params' $ do
    body <- m
    return (body, params')

makeShapeIdentsFromContext :: MonadFreshNames m =>
                              HM.HashMap VName Int
                           -> m (HM.HashMap Int I.Ident,
                                 VarSubstitutions)
makeShapeIdentsFromContext ctx = do
  (ctx', substs) <- liftM unzip $ forM (HM.toList ctx) $ \(name, i) -> do
    v <- newIdent (baseString name) $ I.Basic Int
    return ((i, v), (name, [I.identName v]))
  return (HM.fromList ctx', HM.fromList substs)

instantiateShapesWithDecls :: MonadFreshNames m =>
                              HM.HashMap Int I.Ident
                           -> [I.ExtType]
                           -> m ([I.Type], [I.Ident])
instantiateShapesWithDecls ctx ts =
  runWriterT $ instantiateShapes instantiate ts
  where instantiate x
          | Just v <- HM.lookup x ctx =
            return $ I.Var $ I.identName v

          | otherwise = do
            v <- lift $ newIdent "size" (I.Basic Int)
            tell [v]
            return $ I.Var $ I.identName v
