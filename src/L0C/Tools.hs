{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
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
  , eAssert
  , eDoLoop
  , eSubExps
  , eBody

  , binOpLambda
  , makeLambda

  , MonadBinder(..)
  , Binding(..)
  , bodyBindings
  , insertBindingsM
  , letBind
  , letWithBind
  , loopBind
  , bodyBind
  -- * A concrete @Binder@ monad.
  , Binder
  , runBinder
  , runBinder'
  , runBinderWithNameSource
  -- * Writer convenience interface
  , addBindingWriter
  , collectBindingsWriter
  )
where

import qualified Data.DList as DL
import Data.Loc
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

import L0C.InternalRep
import L0C.MonadFreshNames

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
  return $ Body (LoopBind (zip mergepat mergeexps') i boundexp' loopbody':bnds) res
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
           , lambdaBody = Body [LetBind [res] (BinOp bop (Var x) (Var y) t loc)] $
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

class (MonadFreshNames m, Applicative m, Monad m) => MonadBinder m where
  addBinding      :: Binding -> m ()
  collectBindings :: m a -> m (a, [Binding])

letBind :: MonadBinder m => [Ident] -> Exp -> m ()
letBind pat e =
  addBinding $ LetBind pat e

letWithBind :: MonadBinder m =>
               Certificates -> Ident -> Ident -> [SubExp] -> SubExp -> m ()
letWithBind cs dest src idxs ve =
  addBinding $ LetWithBind cs dest src idxs ve

loopBind :: MonadBinder m => [(Ident, SubExp)] -> Ident -> SubExp -> Body -> m ()
loopBind pat i bound loopbody =
  addBinding $ LoopBind pat i bound loopbody

bodyBind :: MonadBinder m => Body -> m [SubExp]
bodyBind (Body bnds (Result _ es _)) = do
  mapM_ addBinding bnds
  return es

-- | Evaluate the action, producing a body, then wrap it in all the
-- bindings it created using 'addBinding'.
insertBindingsM :: MonadBinder m => m Body -> m Body
insertBindingsM m = do
  (e,bnds) <- collectBindings m
  return $ insertBindings bnds e

addBindingWriter :: (MonadFreshNames m, Applicative m, MonadWriter (DL.DList Binding) m) =>
                    Binding -> m ()
addBindingWriter = tell . DL.singleton

collectBindingsWriter :: (MonadFreshNames m, Applicative m, MonadWriter (DL.DList Binding) m) =>
                         m a -> m (a, [Binding])
collectBindingsWriter m = pass $ do
                            (x, bnds) <- listen m
                            return ((x, DL.toList bnds), const DL.empty)

newtype Binder a = TransformM (WriterT (DL.DList Binding) (State VNameSource) a)
  deriving (Functor, Monad, Applicative,
            MonadWriter (DL.DList Binding), MonadState VNameSource)

instance MonadFreshNames Binder where
  getNameSource = get
  putNameSource = put

instance MonadBinder Binder where
  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

runBinder :: MonadFreshNames m => Binder Body -> m Body
runBinder m = do
  (b, f) <- runBinder' m
  return $ f b

runBinder' :: MonadFreshNames m => Binder a -> m (a, Body -> Body)
runBinder' m = do
  src <- getNameSource
  let (x, bnds, src') = runBinderWithNameSource m src
  putNameSource src'
  return (x, insertBindings bnds)

runBinderWithNameSource :: Binder a -> VNameSource -> (a, [Binding], VNameSource)
runBinderWithNameSource (TransformM m) src =
  let ((x,bnds),src') = runState (runWriterT m) src
  in (x, DL.toList bnds, src')
