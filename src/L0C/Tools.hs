{-# LANGUAGE FlexibleContexts #-}
module L0C.Tools
  ( asSubExp
  , asSubExps

  , newVar

  , eIf
  , eBinOp
  )
where

import Data.Loc
import Control.Applicative

import L0C.InternalRep
import L0C.InternalRep.MonadFreshNames

asSubExp :: MonadFreshNames VName m => m Exp -> (SubExp -> m Exp) -> m Exp
asSubExp m f = do
  e <- m
  let loc = srclocOf e
  case (e, typeOf e) of
    (SubExp se, _) -> f se
    (_, [t])         -> do v <- fst <$> newVar loc "subexp" t
                           LetPat [v] e <$> f (Var v) <*> pure loc
    _                -> fail "asSubExp: tuple-typed expression given."

asSubExps :: MonadFreshNames VName m => m [Exp] -> ([SubExp] -> m Exp) -> m Exp
asSubExps m f =
  asSubExps' [] =<< m
  where asSubExps' ses []            = f $ reverse ses
        asSubExps' ses (SubExp e:es) = asSubExps' (e : ses) es
        asSubExps' ses (e:es)
          | [t] <- typeOf e = do
            v  <- fst <$> newVar loc "subexp" t
            LetPat [v] e <$> asSubExps' (Var v:ses) es <*> pure loc
          | otherwise = fail "asSubExps: tuple-typed expression given."
          where loc = srclocOf e

newVar :: MonadFreshNames VName m => SrcLoc -> String -> Type -> m (Ident, SubExp)
newVar loc name tp = do
  x <- newVName name
  return (Ident x tp loc, Var $ Ident x tp loc)

eIf :: MonadFreshNames VName m =>
          m Exp -> m Exp -> m Exp -> [Type] -> SrcLoc -> m Exp
eIf ce te fe ts loc =
  asSubExp ce $ \ce' -> do
    te' <- te
    fe' <- fe
    return $ If ce' te' fe' ts loc

eBinOp :: MonadFreshNames VName m =>
             BinOp -> m Exp -> m Exp -> Type -> SrcLoc -> m Exp
eBinOp op x y t loc =
  asSubExp x $ \x' ->
  asSubExp y $ \y' ->
  return $ BinOp op x' y' t loc
