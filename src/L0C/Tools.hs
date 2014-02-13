{-# LANGUAGE FlexibleContexts #-}
module L0C.Tools
  ( letSubExp
  , letSubExps
  , letExp
  , letExps
  , letTupExp

  , newVar

  , eIf
  , eBinOp
  , eIndex
  , eCopy
  , eDoLoop
  , eTupLit
  )
where

import Data.Loc
import Control.Applicative

import L0C.InternalRep
import L0C.InternalRep.MonadFreshNames

letSubExp :: MonadFreshNames VName m =>
             String -> m Exp -> (SubExp -> m Exp) -> m Exp
letSubExp desc m f = do
  e <- m
  case (e, typeOf e) of
    (SubExp se, _) -> f se
    (_, [_])       -> letExp desc m (f . Var)
    _              -> fail $ "letSubExp: tuple-typed expression given for " ++ desc ++ ":\n" ++ ppExp e

letExp :: MonadFreshNames VName m =>
          String -> m Exp -> (Ident -> m Exp) -> m Exp
letExp desc m f = do
  e <- m
  let loc = srclocOf e
  case (e, typeOf e) of
    (SubExp (Var v), _) -> f v
    (_, [t])            -> do v <- fst <$> newVar loc desc t
                              LetPat [v] e <$> f v <*> pure loc
    _                   -> fail $ "letExp: tuple-typed expression given:\n" ++ ppExp e

letSubExps :: MonadFreshNames VName m =>
            String -> m [Exp] -> ([SubExp] -> m Exp) -> m Exp
letSubExps desc m f =
  letSubExps' [] =<< m
  where letSubExps' ses []            = f $ reverse ses
        letSubExps' ses (SubExp e:es) = letSubExps' (e : ses) es
        letSubExps' ses (e:es)
          | [t] <- typeOf e = do
            v  <- fst <$> newVar loc desc t
            LetPat [v] e <$> letSubExps' (Var v:ses) es <*> pure loc
          | otherwise = fail $ "letSubExps: tuple-typed expression given:\n" ++ ppExp e
          where loc = srclocOf e

letExps :: MonadFreshNames VName m =>
           String -> m [Exp] -> ([Ident] -> m Exp) -> m Exp
letExps desc m f =
  letExps' [] =<< m
  where letExps' vs []                  = f $ reverse vs
        letExps' vs (SubExp (Var v):es) = letExps' (v : vs) es
        letExps' vs (e:es)
          | [t] <- typeOf e = do
            v  <- fst <$> newVar loc desc t
            LetPat [v] e <$> letExps' (v:vs) es <*> pure loc
          | otherwise = fail "letExps: tuple-typed expression given."
          where loc = srclocOf e

letTupExp :: MonadFreshNames VName m =>
             String -> m Exp -> ([Ident] -> Exp -> m Exp) -> m Exp
letTupExp name e body = do
  e' <- e
  let ts  = typeOf e'
      loc = srclocOf e'
  (names, namevs) <- unzip <$> mapM (newVar loc name) ts
  let bndOuter inner = LetPat names e' inner loc
  bndOuter <$> body names (TupLit namevs loc)

newVar :: MonadFreshNames VName m => SrcLoc -> String -> Type -> m (Ident, SubExp)
newVar loc name tp = do
  x <- newVName name
  return (Ident x tp loc, Var $ Ident x tp loc)

eIf :: MonadFreshNames VName m =>
          m Exp -> m Exp -> m Exp -> [Type] -> SrcLoc -> m Exp
eIf ce te fe ts loc =
  letSubExp "cond" ce $ \ce' -> do
    te' <- te
    fe' <- fe
    return $ If ce' te' fe' ts loc

eBinOp :: MonadFreshNames VName m =>
          BinOp -> m Exp -> m Exp -> Type -> SrcLoc -> m Exp
eBinOp op x y t loc =
  letSubExp "x" x $ \x' ->
  letSubExp "y" y $ \y' ->
  return $ BinOp op x' y' t loc

eIndex :: MonadFreshNames VName m =>
          Certificates -> Ident -> Maybe Certificates -> [m Exp] -> SrcLoc
       -> m Exp
eIndex cs a idxcs idxs loc =
  letSubExps "i" (sequence idxs) $ \idxs' ->
    return $ Index cs a idxcs idxs' loc

eCopy :: MonadFreshNames VName m =>
         m Exp -> m Exp
eCopy e = letSubExp "copy_arg" e $ \e' -> return $ Copy e' $ srclocOf e'

eDoLoop :: MonadFreshNames VName m =>
           [(Ident,m Exp)] -> Ident -> m Exp -> Exp -> Exp -> SrcLoc -> m Exp
eDoLoop pat i boundexp loopbody body loc =
  letSubExps "merge_init" (sequence mergeexps) $ \mergeexps' ->
  letSubExp "bound" boundexp $ \boundexp' ->
  return $ DoLoop (zip mergepat mergeexps') i boundexp' loopbody body loc
  where (mergepat, mergeexps) = unzip pat

eTupLit :: MonadFreshNames VName m =>
           [m Exp] -> SrcLoc -> m Exp
eTupLit es loc =
  letSubExps "tuplit_elems" (sequence es) $ \es' ->
  return $ TupLit es' loc
