module L0C.Internalise.Bindings
  (
  -- * Internalising bindings
    internaliseParam
  , bindingParams

  , bindingPattern
  , bindingFlatPatternWithCert
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

import L0C.ExternalRep as E
import L0C.InternalRep as I
import L0C.MonadFreshNames

import L0C.Internalise.Monad
import L0C.Internalise.AccurateSizes
import L0C.Internalise.TypesValues

import Prelude hiding (mapM)


data InternaliseRes shape
  = FlatTuple [I.IdentBase I.Names shape]
  | TupleArray I.Ident [I.IdentBase I.Names shape]
  | Direct (I.IdentBase I.Names shape)

internaliseParam :: MonadFreshNames m => E.Ident
                 -> m (InternaliseRes I.Rank)
internaliseParam param =
  case (internaliseType $ E.identType param, E.identType param) of
    (_:t:ts, E.Array {}) -> do
      ns <- forM (t:ts) $ \t' -> newIdent base t' loc
      cert <- newIdent ("zip_cert_" ++ base) (I.Basic Cert) loc
      return $ TupleArray cert ns
    ([paramt], _) -> return $ Direct $
                     I.Ident (E.identName param)
                             paramt
                             (E.identSrcLoc param)
    (ts, _) ->
      -- We know from internaliseIdent that none of the element
      -- types are themselves tuples.
      FlatTuple <$> mapM (\t' -> newIdent base t' loc) ts
  where loc = srclocOf param
        base = nameToString $ baseName $ E.identName param

bindingParams :: [E.Parameter] -> ([I.Param] -> InternaliseM a) -> InternaliseM a
bindingParams params m = do
  (params', substs) <- runWriterT $ liftM concat . forM params $ \param -> do
    param' <- lift $ internaliseParam $ E.fromParam param
    case param' of
      Direct k -> do
        (k',shape) <- lift $ paramShapes k
        tell $ HM.singleton (E.identName param) $ DirectSubst k'
        return $ k' : shape
      FlatTuple ks -> do
        ks_sizes <- lift $ mapM paramShapes ks
        tell $ HM.singleton (E.identName param) $ TupleSubst $ map fst ks_sizes
        return $ concatMap (uncurry (:)) ks_sizes
      TupleArray c ks -> do
        ks_sizes <- lift $ mapM paramShapes ks
        tell $ HM.singleton (E.identName param) $ ArraySubst c $ map fst ks_sizes
        return $ c:concatMap (uncurry (:)) ks_sizes
  let bind env = env { envSubsts = substs `HM.union` envSubsts env }
  local bind $ m $ map I.toParam params'

bindingFlatPatternAs :: ([I.Type] -> InternaliseRes Rank -> ([I.Ident], Replacement, [I.Type]))
                     -> [E.Ident] -> [I.Type]
                     -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingFlatPatternAs handleMapping = bindingFlatPattern' []
  where
    bindingFlatPattern' pat []       _  m = do
      let (vs, substs) = unzip pat
          substs' = HM.fromList substs
      local (\env -> env { envSubsts = substs' `HM.union` envSubsts env})
              $ m $ concat vs

    bindingFlatPattern' pat (p:rest) ts m = do
      (p', subst, rest_ts) <- handleMapping ts <$> internaliseParam p
      bindingFlatPattern' ((p', (E.identName p, subst)) : pat) rest rest_ts m

bindingFlatPattern :: [E.Ident] -> [I.Type]
                   -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingFlatPattern = bindingFlatPatternAs handleMapping
  where handleMapping ts (Direct v) =
          let ([v'], rest_ts) = annotateIdents [v] ts
          in ([v'], DirectSubst v', rest_ts)
        handleMapping ts (FlatTuple vs') =
          let (vs'', rest_ts) = annotateIdents vs' ts
          in (vs'', TupleSubst vs'', rest_ts)
        handleMapping ts (TupleArray c vs') =
          -- Drop one type, corresponding to the cert-typed 'c'.
          let (vs'', rest_ts) = annotateIdents vs' $ drop 1 ts
          in (c:vs'', ArraySubst c vs'', rest_ts)

bindingFlatPatternWithCert :: I.Ident
                           -> [E.Ident] -> [I.Type]
                           -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingFlatPatternWithCert ce = bindingFlatPatternAs handleMapping
  where handleMapping ts (Direct v) =
          let ([v'], rest_ts) = annotateIdents [v] ts
          in ([v'], DirectSubst v', rest_ts)
        handleMapping ts (FlatTuple vs') =
          let (vs'', rest_ts) = annotateIdents vs' ts
          in (vs'', TupleSubst vs'', rest_ts)
        handleMapping ts (TupleArray _ vs') =
          let (vs'', rest_ts) = annotateIdents vs' ts
          in (vs'', ArraySubst ce vs'', rest_ts)

flattenPattern :: MonadFreshNames m => E.TupIdent -> m [E.Ident]
flattenPattern (E.Wildcard t loc) = do
  name <- newVName "nameless"
  return [E.Ident name t loc]
flattenPattern (E.Id v) =
  return [v]
flattenPattern (E.TupId pats _) =
  concat <$> mapM flattenPattern pats

bindingPattern :: E.TupIdent -> [I.Type] -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingPattern pat ts m = do
  pat' <- flattenPattern pat
  bindingFlatPattern pat' ts m
