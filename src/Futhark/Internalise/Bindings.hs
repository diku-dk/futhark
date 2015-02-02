module Futhark.Internalise.Bindings
  (
  -- * Internalising bindings
    internaliseParam
  , internaliseFunParam
  , bindingParams

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
import Futhark.Tools as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.TypesValues

import Prelude hiding (mapM)

internaliseParam :: MonadFreshNames m => E.Ident
                 -> m [I.IdentBase I.ExtShape]
internaliseParam param =
  forM (internaliseType $ E.identType param) $ \t ->
  newIdent base t
  where base = nameToString $ baseName $ E.identName param

internaliseFunParam :: MonadFreshNames m => E.Parameter
                    -> m ([I.Ident], [I.Ident])
internaliseFunParam param = do
  new_params <- internaliseParam $ E.fromParam param
  (new_param_types, shapes) <-
    I.instantiateShapes' $
    map I.identType new_params
  let new_params' = [ new_param { I.identType = t } |
                      (new_param, t) <- zip new_params new_param_types ]
  return (shapes, new_params')

bindingParams :: [E.Parameter]
              -> ([I.Param] -> [I.Param] -> InternaliseM a)
              -> InternaliseM a
bindingParams params m = do
  ((paramshapes, params'), substs) <-
    runWriterT $ liftM unzip $ forM params $ \param -> do
      (shapes, param') <- lift $ internaliseFunParam param
      tell $ HM.singleton (E.identName param) param'
      return (shapes, param')
  let bind env = env { envSubsts = substs `HM.union` envSubsts env }
  local bind $ m (concat paramshapes) (concat params')

bindingFlatPattern :: [E.Ident] -> [I.Type]
                   -> ([I.Ident] -> InternaliseM a)
                   -> InternaliseM a
bindingFlatPattern = bindingFlatPattern' []
  where
    bindingFlatPattern' pat []       _  m = do
      let (vs, substs) = unzip pat
          substs' = HM.fromList substs
      local (\env -> env { envSubsts = substs' `HM.union` envSubsts env})
              $ m $ concat $ reverse vs

    bindingFlatPattern' pat (p:rest) ts m = do
      (ps, subst, rest_ts) <- handleMapping ts <$> internaliseParam p
      bindingFlatPattern' ((ps, (E.identName p, subst)) : pat) rest rest_ts m

    handleMapping ts [] =
      ([], [], ts)
    handleMapping ts (r:rs) =
        let (ps, reps, ts')    = handleMapping' ts r
            (pss, repss, ts'') = handleMapping ts' rs
        in (ps++pss, reps:repss, ts'')

    handleMapping' (t:ts) v =
      let u = I.uniqueness $ I.identType v
          v' = v { I.identType = t `I.setUniqueness` u }
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
  (ts',shapes) <- I.instantiateShapes' ts
  let addShapeBindings pat'' = m $ I.basicPattern $ shapes ++ pat''
  bindingFlatPattern pat' ts' addShapeBindings
