{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Futhark.Tools
  ( letSubExp
  , letSubExps
  , letExp
  , letExps
  , letTupExp

  , newVar

  , eIf
  , eBinOp
  , eNegate
  , eNot
  , eIndex
  , eCopy
  , eAssert
  , eDoLoop
  , eSubExps
  , eBody

  , foldBinOp
  , binOpLambda
  , makeLambda

  , copyConsumed

  , module Futhark.Binder
  )
where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Loc
import Control.Applicative
import Control.Monad.Writer

import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.Substitute
import Futhark.Binder

letSubExp :: MonadBinder m =>
             String -> Exp -> m SubExp
letSubExp _ (SubExps [se] _) = return se
letSubExp desc e =
  case typeOf e of
    [_] -> Var <$> letExp desc e
    _   -> fail $ "letSubExp: tuple-typed expression given for " ++ desc ++ ":\n" ++ ppExp e

letExp :: MonadBinder m =>
          String -> Exp -> m Ident
letExp _ (SubExps [Var v] _) = return v
letExp desc e =
  case typeOf e of
    [t] -> do v <- fst <$> newVar (srclocOf e) desc t
              letBind [v] e
              return v
    _   -> fail $ "letExp: tuple-typed expression given:\n" ++ ppExp e

letSubExps :: MonadBinder m =>
              String -> [Exp] -> m [SubExp]
letSubExps desc = mapM $ letSubExp desc

letExps :: MonadBinder m =>
           String -> [Exp] -> m [Ident]
letExps desc = mapM $ letExp desc

varSubExp :: SubExp -> Maybe Ident
varSubExp (Var v) = Just v
varSubExp _       = Nothing

letTupExp :: MonadBinder m => String -> Exp -> m [Ident]
letTupExp _ (SubExps es _)
  | Just vs <- mapM varSubExp es = return vs
letTupExp name e = do
  let ts  = typeOf e
      loc = srclocOf e
  names <- mapM (liftM fst . newVar loc name) ts
  letBind names e
  return names

newVar :: MonadFreshNames m =>
          SrcLoc -> String -> Type -> m (Ident, SubExp)
newVar loc name tp = do
  x <- newVName name
  return (Ident x tp loc, Var $ Ident x tp loc)

eIf :: MonadBinder m =>
       m Exp -> m Body -> m Body -> [Type] -> SrcLoc -> m Exp
eIf ce te fe ts loc = do
  ce' <- letSubExp "cond" =<< ce
  te' <- insertBindingsM te
  fe' <- insertBindingsM fe
  return $ If ce' te' fe' ts loc

eBinOp :: MonadBinder m =>
          BinOp -> m Exp -> m Exp -> Type -> SrcLoc -> m Exp
eBinOp op x y t loc = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  return $ BinOp op x' y' t loc

eNegate :: MonadBinder m =>
           m Exp -> SrcLoc -> m Exp
eNegate e loc = do
  e' <- letSubExp "negate_arg" =<< e
  return $ Negate e' loc

eNot :: MonadBinder m =>
        m Exp -> SrcLoc -> m Exp
eNot e loc = do
  e' <- letSubExp "not_arg" =<< e
  return $ Not e' loc

eIndex :: MonadBinder m =>
          Certificates -> Ident -> [m Exp] -> SrcLoc
       -> m Exp
eIndex cs a idxs loc = do
  idxs' <- letSubExps "i" =<< sequence idxs
  return $ Index cs a idxs' loc

eCopy :: MonadBinder m =>
         m Exp -> m Exp
eCopy e = do e' <- letSubExp "copy_arg" =<< e
             return $ Copy e' $ srclocOf e'

eAssert :: MonadBinder m =>
         m Exp -> m Exp
eAssert e = do e' <- letSubExp "assert_arg" =<< e
               return $ Assert e' $ srclocOf e'

eDoLoop :: MonadBinder m =>
           [(Ident,m Exp)] -> Ident -> m Exp -> m Body -> m Body -> m Body
eDoLoop pat i boundexp loopbody body = do
  mergeexps' <- letSubExps "merge_init" =<< sequence mergeexps
  boundexp' <- letSubExp "bound" =<< boundexp
  loopbody' <- insertBindingsM loopbody
  Body bnds res <- insertBindingsM body
  return $ Body (DoLoop (zip mergepat mergeexps') i boundexp' loopbody':bnds) res
  where (mergepat, mergeexps) = unzip pat

eSubExps :: MonadBinder m =>
           [m Exp] -> SrcLoc -> m Exp
eSubExps es loc = do
  es' <- letSubExps "tuplit_elems" =<< sequence es
  return $ SubExps es' loc

eBody :: MonadBinder m =>
         m Exp -> m Body
eBody e = insertBindingsM $ do
            e' <- e
            x <- letTupExp "x" e'
            return $ resultBody [] (map Var x) $ srclocOf e'

-- | Apply a binary operator to several subexpressions.  A left-fold.
foldBinOp :: MonadBinder m =>
             BinOp -> SubExp -> [SubExp] -> Type -> m Exp
foldBinOp _ ne [] _   = return $ subExp ne
foldBinOp bop ne (e:es) t =
  eBinOp bop (pure $ subExp e) (foldBinOp bop ne es t) t $ srclocOf e

-- | Create a two-parameter lambda whose body applies the given binary
-- operation to its arguments.  It is assumed that both argument and
-- result types are the same.  (This assumption should be fixed at
-- some point.)
binOpLambda :: MonadFreshNames m =>
               BinOp -> Type -> SrcLoc -> m Lambda
binOpLambda bop t loc = do
  x   <- newIdent "x"   t loc
  y   <- newIdent "y"   t loc
  res <- newIdent "res" t loc
  return Lambda {
             lambdaParams     = [toParam x, toParam y]
           , lambdaReturnType = [toConstType t]
           , lambdaSrcLoc     = loc
           , lambdaBody = Body [Let [res] (BinOp bop (Var x) (Var y) t loc)] $
                          Result [] [Var res] loc
           }

makeLambda :: MonadBinder m =>
              [Param] -> m Body -> m Lambda
makeLambda params body = do
  body' <- insertBindingsM body
  return Lambda {
             lambdaParams = params
           , lambdaSrcLoc = srclocOf body'
           , lambdaReturnType = map (`setAliases` ()) $ bodyType body'
           , lambdaBody = body'
           }

copyConsumed :: MonadBinder m => Body -> m Body
copyConsumed e
  | consumed <- HS.toList $ freeUniqueInBody e,
    not (null consumed) = do
      copies <- copyVariables consumed
      let substs = HM.fromList $ zip (map identName consumed)
                                     (map identName copies)
      return $ substituteNames substs e
  | otherwise = return e
  where copyVariables = mapM copyVariable
        copyVariable v =
          letExp (textual (baseName $ identName v) ++ "_copy") $
                 Copy (Var v) loc
          where loc = srclocOf v

        freeUniqueInBody = HS.filter (unique . identType) . freeInBody
