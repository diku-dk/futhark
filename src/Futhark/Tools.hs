{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Futhark.Tools
  ( letSubExp
  , letSubExps
  , letExp
  , letExps
  , letTupExp
  , letTupExp'

  , newVar

  , eIf
  , eBinOp
  , eNegate
  , eNot
  , eIndex
  , eCopy
  , eAssert
  , eDoLoop
  , eBody
  , eLambda

  , foldBinOp
  , binOpLambda
  , makeLambda

  , copyConsumed
  , nonuniqueParams

  , module Futhark.Binder
  )
where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Loc
import Control.Applicative
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Substitute
import Futhark.Binder

letSubExp :: MonadBinder m =>
             String -> Exp (Lore m) -> m SubExp
letSubExp _ (SubExp se) = return se
letSubExp desc e =
  case typeOf e of
    [_] -> Var <$> letExp desc e
    _   -> fail $ "letSubExp: tuple-typed expression given for " ++ desc ++ ":\n" ++ ppExp e

letExp :: MonadBinder m =>
          String -> Exp (Lore m) -> m Ident
letExp _ (SubExp (Var v)) = return v
letExp desc e =
  case hasStaticShape $ typeOf e of
    Just [t] -> do v <- fst <$> newVar (srclocOf e) desc t
                   letBind [v] e
                   return v
    _   -> fail $ "letExp: tuple-typed expression given:\n" ++ ppExp e

letSubExps :: MonadBinder m =>
              String -> [Exp (Lore m)] -> m [SubExp]
letSubExps desc = mapM $ letSubExp desc

letExps :: MonadBinder m =>
           String -> [Exp (Lore m)] -> m [Ident]
letExps desc = mapM $ letExp desc

letShapedExp :: MonadBinder m => String -> Exp (Lore m)
             -> m ([Ident], [Ident])
letShapedExp _ (SubExp (Var v)) = return ([], [v])
letShapedExp name e = do
  (ts, shapes) <- runWriterT $ instantiateShapes instantiate $ typeOf e
  names <- mapM (liftM fst . newVar loc name) ts
  letBind (shapes++names) e
  return (shapes, names)
  where loc = srclocOf e
        instantiate = do v <- lift $ newIdent "size" (Basic Int) loc
                         tell [v]
                         return $ Var v

letTupExp :: MonadBinder m => String -> Exp (Lore m)
          -> m [Ident]
letTupExp name e = snd <$> letShapedExp name e

letTupExp' :: MonadBinder m => String -> Exp (Lore m)
           -> m [SubExp]
letTupExp' _ (SubExp se) = return [se]
letTupExp' name ses = do vs <- letTupExp name ses
                         return $ map Var vs

newVar :: MonadFreshNames m =>
          SrcLoc -> String -> Type -> m (Ident, SubExp)
newVar loc name tp = do
  x <- newVName name
  return (Ident x tp loc, Var $ Ident x tp loc)

eIf :: MonadBinder m =>
       m (Exp (Lore m)) -> m (Body (Lore m)) -> m (Body (Lore m)) -> ResType -> SrcLoc
    -> m (Exp (Lore m))
eIf ce te fe ts loc = do
  ce' <- letSubExp "cond" =<< ce
  te' <- insertBindingsM te
  fe' <- insertBindingsM fe
  return $ If ce' te' fe' ts loc

eBinOp :: MonadBinder m =>
          BinOp -> m (Exp (Lore m)) -> m (Exp (Lore m)) -> Type -> SrcLoc
       -> m (Exp (Lore m))
eBinOp op x y t loc = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  return $ BinOp op x' y' t loc

eNegate :: MonadBinder m =>
           m (Exp (Lore m)) -> SrcLoc -> m (Exp (Lore m))
eNegate e loc = do
  e' <- letSubExp "negate_arg" =<< e
  return $ Negate e' loc

eNot :: MonadBinder m =>
        m (Exp (Lore m)) -> SrcLoc -> m (Exp (Lore m))
eNot e loc = do
  e' <- letSubExp "not_arg" =<< e
  return $ Not e' loc

eIndex :: MonadBinder m =>
          Certificates -> Ident -> [m (Exp (Lore m))] -> SrcLoc
       -> m (Exp (Lore m))
eIndex cs a idxs loc = do
  idxs' <- letSubExps "i" =<< sequence idxs
  return $ Index cs a idxs' loc

eCopy :: MonadBinder m =>
         m (Exp (Lore m)) -> m (Exp (Lore m))
eCopy e = do e' <- letSubExp "copy_arg" =<< e
             return $ Copy e' $ srclocOf e'

eAssert :: MonadBinder m =>
         m (Exp (Lore m)) -> m (Exp (Lore m))
eAssert e = do e' <- letSubExp "assert_arg" =<< e
               return $ Assert e' $ srclocOf e'

eDoLoop :: MonadBinder m =>
           [Ident] -> [(Ident, m (Exp (Lore m)))]
        -> Ident -> m (Exp (Lore m)) -> m (Body (Lore m))
        -> m (Exp (Lore m))
eDoLoop respat merge i boundexp loopbody = do
  mergeexps' <- letSubExps "merge_init" =<< sequence mergeexps
  boundexp' <- letSubExp "bound" =<< boundexp
  loopbody' <- insertBindingsM loopbody
  return $ DoLoop respat (zip mergepat mergeexps') i boundexp' loopbody' loc
  where (mergepat, mergeexps) = unzip merge
        loc = srclocOf i

eBody :: MonadBinder m =>
         [m (Exp (Lore m))]
      -> m (Body (Lore m))
eBody es = insertBindingsM $ do
             es' <- sequence es
             xs <- mapM (letTupExp "x") es'
             let loc = case es' of []  -> noLoc
                                   e:_ -> srclocOf e
             return $ resultBody [] (map Var $ concat xs) loc

eLambda :: MonadBinder m =>
           Lambda (Lore m) -> [SubExp] -> m [SubExp]
eLambda lam args = do zipWithM_ letBind params $ map SubExp args
                      bodyBind $ lambdaBody lam
  where params = map (pure . fromParam) $ lambdaParams lam

-- | Apply a binary operator to several subexpressions.  A left-fold.
foldBinOp :: MonadBinder m =>
             BinOp -> SubExp -> [SubExp] -> Type -> m (Exp (Lore m))
foldBinOp _ ne [] _   = return $ SubExp ne
foldBinOp bop ne (e:es) t =
  eBinOp bop (pure $ SubExp e) (foldBinOp bop ne es t) t $ srclocOf e

-- | Create a two-parameter lambda whose body applies the given binary
-- operation to its arguments.  It is assumed that both argument and
-- result types are the same.  (This assumption should be fixed at
-- some point.)
binOpLambda :: (Bindable lore, MonadFreshNames m) =>
               BinOp -> Type -> SrcLoc -> m (Lambda lore)
binOpLambda bop t loc = do
  x   <- newIdent "x"   t loc
  y   <- newIdent "y"   t loc
  res <- newIdent "res" t loc
  return Lambda {
             lambdaParams     = [toParam x, toParam y]
           , lambdaReturnType = [toConstType t]
           , lambdaSrcLoc     = loc
           , lambdaBody = Body [mkLet [res] (BinOp bop (Var x) (Var y) t loc)] $
                          Result [] [Var res] loc
           }

makeLambda :: MonadBinder m =>
              [Param] -> m (Body (Lore m)) -> m (Lambda (Lore m))
makeLambda params body = do
  body' <- insertBindingsM body
  case hasStaticShape $ bodyType body' of
    Nothing -> fail "Body passed to makeLambda has open type"
    Just ts ->
      return Lambda {
        lambdaParams = params
        , lambdaSrcLoc = srclocOf body'
        , lambdaReturnType = map (`setAliases` ()) ts
        , lambdaBody = body'
        }

copyConsumed :: (Proper (Lore m), MonadBinder m) => Body (Lore m) -> m (Body (Lore m))
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

nonuniqueParams :: (Bindable lore, MonadFreshNames m) =>
                   [Param] -> m ([Param], [Binding lore])
nonuniqueParams params = do
  (params', bnds) <- liftM unzip $ forM params $ \param ->
    if unique $ identType param then do
      param' <- nonuniqueParam <$> newIdent' (++"_nonunique") param
      return (param',
              [mkLet [fromParam param] $
               Copy (Var $ fromParam param') $ srclocOf param'])
    else
      return (param, [])
  return (params', concat bnds)
  where nonuniqueParam param =
          param { identType = identType param `setUniqueness` Nonunique }
