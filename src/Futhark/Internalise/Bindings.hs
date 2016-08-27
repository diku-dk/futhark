{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.Bindings
  (
  -- * Internalising bindings
    bindingParams
  , bindingLambdaParams

  , flattenPattern
  , bindingPattern
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

import Language.Futhark as E
import qualified Futhark.Representation.SOACS as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.TypesValues

import Prelude hiding (mapM)

internaliseParams :: [E.Parameter]
                  -> InternaliseM (HM.HashMap VName Int,
                                   [[(VName, I.DeclExtType)]])
internaliseParams params = do
  (param_ts, ctx) <- internaliseParamTypes $ map E.paramType params
  vss <- forM (zip params param_ts) $ \(param, ts) -> do
    let base = nameToString $ baseName $ E.paramName param
    forM ts $ \t -> do
      name <- newVName base
      return (name, t)
  return (ctx, vss)

internaliseBindee :: MonadFreshNames m =>
                     E.Ident
                  -> m [(VName, I.DeclExtType)]
internaliseBindee bindee =
  forM (internaliseTypeWithUniqueness $ E.unInfo $ E.identType bindee) $ \t -> do
    name <- newVName base
    return (name, t)
  where base = nameToString $ baseName $ E.identName bindee

internaliseFunParams :: [E.Parameter]
                     -> InternaliseM ([I.FParam],
                                      [I.FParam],
                                      VarSubstitutions)
internaliseFunParams params = do
  (shapectx, param_params) <- internaliseParams params

  (shapectx', shapesubst) <- makeShapeIdentsFromContext shapectx
  let declared_shape_params =
        map nonuniqueParamFromIdent $ HM.elems shapectx'

  (implicit_shape_params, value_params) <-
    fmap unzip $ forM param_params $ \params' -> do
    (instantiated_param_types, param_implicit_shapes) <-
      instantiateShapesWithDecls shapectx' $ map snd params'
    let instantiated_params =
          [ I.Param new_param_name t |
            ((new_param_name, _), t) <- zip params' instantiated_param_types ]
    return (param_implicit_shapes, instantiated_params)
  let subst = HM.fromList $ zip
              (map E.paramName params)
              (map (map I.paramName) value_params)
  return (declared_shape_params ++ concat implicit_shape_params,
          concat value_params,
          subst <> shapesubst)

bindingParams :: [E.Parameter]
              -> ([I.FParam] -> [I.FParam] -> InternaliseM a)
              -> InternaliseM a
bindingParams params m = do
  (shapeparams, valueparams, substs) <- internaliseFunParams params
  let bind env = env { envSubsts = substs `HM.union` envSubsts env }
  local bind $
    bindingIdentTypes (map I.paramIdent $ shapeparams++valueparams) $
    m shapeparams valueparams

bindingFlatPattern :: [E.Ident] -> [I.Type]
                   -> ([I.Param I.Type] -> InternaliseM a)
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
      bindingFlatPattern' ((ps, (E.identName p, map I.paramName subst)) : pat) rest rest_ts m

    handleMapping ts [] =
      ([], [], ts)
    handleMapping ts (r:rs) =
        let (ps, reps, ts')    = handleMapping' ts r
            (pss, repss, ts'') = handleMapping ts' rs
        in (ps++pss, reps:repss, ts'')

    handleMapping' (t:ts) (vname,_) =
      let v' = I.Param vname t
      in ([v'], v', ts)
    handleMapping' [] _ =
      error "bindingFlatPattern: insufficient identifiers in pattern."

flattenPattern :: MonadFreshNames m => E.Pattern -> m [E.Ident]
flattenPattern (E.Wildcard t loc) = do
  name <- newVName "nameless"
  return [E.Ident name t loc]
flattenPattern (E.Id v) =
  return [v]
flattenPattern (E.TuplePattern pats _) =
  concat <$> mapM flattenPattern pats
flattenPattern (E.PatternAscription p _) =
  flattenPattern p

bindingPattern :: E.Pattern -> [I.ExtType] -> (I.Pattern -> InternaliseM a)
                -> InternaliseM a
bindingPattern pat ts m = do
  pat' <- flattenPattern pat
  (ts',shapes) <- instantiateShapes' ts
  let addShapeBindings =
        m . I.basicPattern' shapes . map I.paramIdent
  bindingFlatPattern pat' ts' addShapeBindings

bindingLambdaParams :: [E.Parameter] -> [I.Type]
                    -> InternaliseM I.Body
                    -> InternaliseM (I.Body, [I.LParam])
bindingLambdaParams params ts m =
  bindingFlatPattern (map E.fromParam params) ts $ \params' ->
  bindingIdentTypes (map I.paramIdent params') $ do
    body <- m
    return (body, params')

makeShapeIdentsFromContext :: MonadFreshNames m =>
                              HM.HashMap VName Int
                           -> m (HM.HashMap Int I.Ident,
                                 VarSubstitutions)
makeShapeIdentsFromContext ctx = do
  (ctx', substs) <- fmap unzip $ forM (HM.toList ctx) $ \(name, i) -> do
    v <- newIdent (baseString name) $ I.Prim I.int32
    return ((i, v), (name, [I.identName v]))
  return (HM.fromList ctx', HM.fromList substs)

instantiateShapesWithDecls :: MonadFreshNames m =>
                              HM.HashMap Int I.Ident
                           -> [I.DeclExtType]
                           -> m ([I.DeclType], [I.FParam])
instantiateShapesWithDecls ctx ts =
  runWriterT $ instantiateShapes instantiate ts
  where instantiate x
          | Just v <- HM.lookup x ctx =
            return $ I.Var $ I.identName v

          | otherwise = do
            v <- lift $ nonuniqueParamFromIdent <$> newIdent "size" (I.Prim I.int32)
            tell [v]
            return $ I.Var $ I.paramName v

nonuniqueParamFromIdent :: I.Ident -> I.FParam
nonuniqueParamFromIdent (I.Ident name t) =
  I.Param name $ I.toDecl t Nonunique
