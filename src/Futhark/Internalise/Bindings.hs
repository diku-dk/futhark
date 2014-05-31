module Futhark.Internalise.Bindings
  (
  -- * Internalising bindings
    internaliseParam
  , bindingParams

  , flattenPattern
  , bindingPattern
  , bindingFlatPatternWithCert
  , bindingFlatPatternOwnCert
  )
  where

import Control.Applicative
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Loc
import Data.Traversable (mapM)

import Futhark.ExternalRep as E
import Futhark.InternalRep as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.TypesValues

import Prelude hiding (mapM)


data InternaliseRes shape = TupleArray I.Ident [I.IdentBase I.Names shape]
                          | Direct (I.IdentBase I.Names shape)
                            deriving (Show)

internaliseParam :: MonadFreshNames m => E.Ident
                 -> m [InternaliseRes I.Rank]
internaliseParam param =
  internalise $ internaliseType $ E.identType param
  where internalise [] = return []
        internalise (I.Basic Cert : ts) = do
          let (ets,restts) = span (/=I.Basic Cert) ts
          ns <- forM ets $ \t' -> newIdent base t' loc
          cert <- newIdent ("zip_cert_" ++ base) (I.Basic Cert) loc
          (TupleArray cert ns:) <$> internalise restts
        internalise ts = do
          let (ets,restts) = span (/=I.Basic Cert) ts
          params <- mapM (\t' -> Direct <$> newIdent base t' loc) ets
          (params++) <$> internalise restts

        loc = srclocOf param
        base = nameToString $ baseName $ E.identName param

bindingParams :: [E.Parameter] -> ([I.Param] -> [I.Param] -> InternaliseM a) -> InternaliseM a
bindingParams params m = do
  ((paramshapes, params'), substs) <-
    runWriterT $ liftM unzip $ forM params $ \param -> do
      internalisations <- lift $ internaliseParam $ E.fromParam param
      (paramshapes, params', substs) <-
        liftM unzip3 $ forM internalisations $ \param' ->
          case param' of
            Direct k -> do
              (k',k_shape) <- lift $ identWithShapes k
              return (k_shape,
                      [k'],
                      DirectSubst k')
            TupleArray c ks -> do
              (ks',ks_shapes) <- unzip <$> lift (mapM identWithShapes ks)
              return (concat ks_shapes,
                      c:ks',
                      ArraySubst (I.Var c) ks')
      tell $ HM.singleton (E.identName param) substs
      return (concat paramshapes, concat params')
  let bind env = env { envSubsts = substs `HM.union` envSubsts env }
      toParams = map I.toParam . concat
  local bind $ m (toParams paramshapes) (toParams params')

bindingFlatPatternAs :: ([I.Type] -> [InternaliseRes Rank] -> ([I.Ident], [Replacement], [I.Type]))
                     -> [E.Ident] -> [I.Type]
                     -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingFlatPatternAs handleMapping = bindingFlatPattern' []
  where
    bindingFlatPattern' pat []       _  m = do
      let (vs, substs) = unzip pat
          substs' = HM.fromList substs
      local (\env -> env { envSubsts = substs' `HM.union` envSubsts env})
              $ m $ concat $ reverse vs

    bindingFlatPattern' pat (p:rest) ts m = do
      (ps, subst, rest_ts) <- handleMapping ts <$> internaliseParam p
      bindingFlatPattern' ((ps, (E.identName p, subst)) : pat) rest rest_ts m

bindingFlatPatternWithCert :: I.SubExp
                           -> [E.Ident] -> [I.Type]
                           -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingFlatPatternWithCert ce = bindingFlatPatternAs handleMapping
  where handleMapping ts [] = ([], [], ts)
        handleMapping ts (r:rs) =
          let (ps, reps, ts') = handleMapping' ts r
              (pss, repss, ts'') = handleMapping ts' rs
          in (ps++pss, reps:repss, ts'')
        handleMapping' ts (Direct v) =
          let ([v'], rest_ts) = annotateIdents [v] ts
          in ([v'], DirectSubst v', rest_ts)
        handleMapping' ts (TupleArray _ vs') =
          let (vs'', rest_ts) = annotateIdents vs' ts
          in (vs'', ArraySubst ce vs'', rest_ts)

bindingFlatPatternOwnCert :: [E.Ident] -> [I.Type]
                          -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingFlatPatternOwnCert = bindingFlatPatternAs handleMapping
  where handleMapping ts [] = ([], [], ts)
        handleMapping ts (r:rs) =
          let (ps, reps, ts')    = handleMapping' ts r
              (pss, repss, ts'') = handleMapping ts' rs
          in (ps++pss, reps:repss, ts'')
        handleMapping' ts (Direct v) =
          let ([v'], rest_ts) = annotateIdents [v] ts
          in ([v'], DirectSubst v', rest_ts)
        handleMapping' ts (TupleArray c vs') =
          let (vs'', rest_ts) = annotateIdents vs' $ drop 1 ts -- Skip the cert.
          in (c:vs'', ArraySubst (I.Var c) vs'', rest_ts)

flattenPattern :: MonadFreshNames m => E.TupIdent -> m [E.Ident]
flattenPattern (E.Wildcard t loc) = do
  name <- newVName "nameless"
  return [E.Ident name t loc]
flattenPattern (E.Id v) =
  return [v]
flattenPattern (E.TupId pats _) =
  concat <$> mapM flattenPattern pats

bindingPattern :: E.TupIdent -> Maybe SubExp -> ResType -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingPattern pat ce ts m = do
  pat' <- flattenPattern pat
  (ts',shapes) <- runWriterT $ instantiateShapes instantiate ts
  let addShapeBindings pat'' = m $ shapes ++ pat''
  case ce of Just ce' -> bindingFlatPatternWithCert ce' pat' ts' addShapeBindings
             Nothing  -> bindingFlatPatternOwnCert pat' ts' addShapeBindings
  where instantiate = do v <- lift $ newIdent "size" (I.Basic I.Int) $ srclocOf pat
                         tell [v]
                         return $ I.Var v
