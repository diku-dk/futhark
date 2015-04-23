{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Futhark.Tools
  ( letSubExp
  , letSubExps
  , letExp
  , letExps
  , letTupExp
  , letTupExp'
  , letInPlace

  , eSubExp
  , eIf
  , eBinOp
  , eNegate
  , eNot
  , eIndex
  , eCopy
  , eAssert
  , eValue
  , eBody
  , eLambda

  , resultBody
  , resultBodyM
  , insertBindingsM
  , mapResultM
  , mapResult

  , foldBinOp
  , binOpLambda
  , makeLambda

  , nonuniqueParams

  , module Futhark.Binder

  -- * Result types
  , instantiateShapes
  , instantiateShapes'
  , instantiateShapesFromIdentList
  , instantiateExtTypes
  , instantiateIdents
  )
where

import qualified Data.Array as A
import qualified Data.HashMap.Lazy as HM
import Data.Loc (SrcLoc)

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Prelude

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Util

letSubExp :: MonadBinder m =>
             String -> Exp (Lore m) -> m SubExp
letSubExp _ (PrimOp (SubExp se)) = return se
letSubExp desc e = do
  n <- length <$> expExtType e
  vs <- replicateM n $ newVName desc
  idents <- letBindNames' vs e
  case idents of
    [ident] -> return $ Var $ identName ident
    _       -> fail $ "letSubExp: tuple-typed expression given:\n" ++ pretty e

letExp :: MonadBinder m =>
          String -> Exp (Lore m) -> m VName
letExp _ (PrimOp (SubExp (Var v))) =
  return v
letExp desc e = do
  n <- length <$> expExtType e
  vs <- replicateM n $ newVName desc
  idents <- letBindNames' vs e
  case idents of
    [ident] -> return $ identName ident
    _       -> fail $ "letExp: tuple-typed expression given:\n" ++ pretty e

letInPlace :: MonadBinder m =>
              String -> Certificates -> VName -> [SubExp] -> Exp (Lore m)
           -> m VName
letInPlace desc cs src is e = do
  v <- newVName desc
  idents <- letBindNames [(v,BindInPlace cs src is)] e
  case idents of
    [ident] -> return $ identName ident
    _       -> fail $ "letExp: tuple-typed expression given:\n" ++ pretty e

letSubExps :: MonadBinder m =>
              String -> [Exp (Lore m)] -> m [SubExp]
letSubExps desc = mapM $ letSubExp desc

letExps :: MonadBinder m =>
           String -> [Exp (Lore m)] -> m [VName]
letExps desc = mapM $ letExp desc

letTupExp :: (MonadBinder m) =>
             String -> Exp (Lore m)
          -> m [VName]
letTupExp _ (PrimOp (SubExp (Var v))) =
  return [v]
letTupExp name e = do
  numValues <- length <$> expExtType e
  names <- replicateM numValues $ newVName name
  map identName <$> letBindNames' names e

letTupExp' :: (MonadBinder m) =>
              String -> Exp (Lore m)
           -> m [SubExp]
letTupExp' _ (PrimOp (SubExp se)) = return [se]
letTupExp' name ses = do vs <- letTupExp name ses
                         return $ map Var vs

eSubExp :: MonadBinder m =>
           SubExp -> m (Exp (Lore m))
eSubExp = pure . PrimOp . SubExp

eIf :: MonadBinder m =>
       m (Exp (Lore m)) -> m (Body (Lore m)) -> m (Body (Lore m))
    -> m (Exp (Lore m))
eIf ce te fe = do
  ce' <- letSubExp "cond" =<< ce
  te' <- insertBindingsM te
  fe' <- insertBindingsM fe
  ts <- generaliseExtTypes <$> bodyExtType te' <*> bodyExtType fe'
  return $ If ce' te' fe' ts

eBinOp :: MonadBinder m =>
          BinOp -> m (Exp (Lore m)) -> m (Exp (Lore m)) -> BasicType
       -> m (Exp (Lore m))
eBinOp op x y t = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  return $ PrimOp $ BinOp op x' y' t

eNegate :: MonadBinder m =>
           m (Exp (Lore m)) -> m (Exp (Lore m))
eNegate e = do
  e' <- letSubExp "negate_arg" =<< e
  return $ PrimOp $ Negate e'

eNot :: MonadBinder m =>
        m (Exp (Lore m)) -> m (Exp (Lore m))
eNot e = do
  e' <- letSubExp "not_arg" =<< e
  return $ PrimOp $ Not e'

eIndex :: MonadBinder m =>
          Certificates -> VName -> [m (Exp (Lore m))]
       -> m (Exp (Lore m))
eIndex cs a idxs = do
  idxs' <- letSubExps "i" =<< sequence idxs
  return $ PrimOp $ Index cs a idxs'

eCopy :: MonadBinder m =>
         m (Exp (Lore m)) -> m (Exp (Lore m))
eCopy e = do e' <- letExp "copy_arg" =<< e
             return $ PrimOp $ Copy e'

eAssert :: MonadBinder m =>
         m (Exp (Lore m)) -> SrcLoc -> m (Exp (Lore m))
eAssert e loc = do e' <- letSubExp "assert_arg" =<< e
                   return $ PrimOp $ Assert e' loc

eValue :: MonadBinder m => Value -> m (Exp (Lore m))
eValue (BasicVal bv) =
  return $ PrimOp $ SubExp $ Constant bv
eValue (ArrayVal a bt [_]) = do
  let ses = map Constant $ A.elems a
  return $ PrimOp $ ArrayLit ses $ Basic bt
eValue (ArrayVal a bt shape) = do
  let rowshape = drop 1 shape
      rowsize  = product rowshape
      rows     = [ ArrayVal (A.listArray (0,rowsize-1) r) bt rowshape
                 | r <- chunk rowsize $ A.elems a ]
      rowtype = Array bt (Shape $ map (Constant . IntVal) rowshape) Nonunique
  ses <- mapM (letSubExp "array_elem" <=< eValue) rows
  return $ PrimOp $ ArrayLit ses rowtype

eBody :: (MonadBinder m) =>
         [m (Exp (Lore m))]
      -> m (Body (Lore m))
eBody es = insertBindingsM $ do
             es' <- sequence es
             xs <- mapM (letTupExp "x") es'
             mkBodyM [] $ map Var $ concat xs

eLambda :: MonadBinder m =>
           Lambda (Lore m) -> [SubExp] -> m [SubExp]
eLambda lam args = do zipWithM_ letBindNames params $
                        map (PrimOp . SubExp) args
                      bodyBind $ lambdaBody lam
  where params = [ [(param, BindVar)] |
                   param <- map identName $ lambdaParams lam ]

-- | Apply a binary operator to several subexpressions.  A left-fold.
foldBinOp :: MonadBinder m =>
             BinOp -> SubExp -> [SubExp] -> BasicType -> m (Exp (Lore m))
foldBinOp _ ne [] _   = return $ PrimOp $ SubExp ne
foldBinOp bop ne (e:es) t =
  eBinOp bop (pure $ PrimOp $ SubExp e) (foldBinOp bop ne es t) t

-- | Create a two-parameter lambda whose body applies the given binary
-- operation to its arguments.  It is assumed that both argument and
-- result types are the same.  (This assumption should be fixed at
-- some point.)
binOpLambda :: (MonadFreshNames m, Bindable lore) =>
               BinOp -> BasicType -> m (Lambda lore)
binOpLambda bop t = do
  x   <- newVName "x"
  y   <- newVName "y"
  (body, _) <- runBinderEmptyEnv $ insertBindingsM $ do
    res <- letSubExp "res" $ PrimOp $ BinOp bop (Var x) (Var y) t
    return $ resultBody [res]
  return Lambda {
             lambdaParams     = [Ident x (Basic t), Ident y (Basic t)]
           , lambdaReturnType = [Basic t]
           , lambdaBody       = body
           }

makeLambda :: (Bindable (Lore m), MonadBinder m) =>
              [Param] -> m (Body (Lore m)) -> m (Lambda (Lore m))
makeLambda params body = do
  body' <- insertBindingsM body
  bodyt <- bodyExtType body'
  case allBasic bodyt of
    Nothing -> fail "Body passed to makeLambda has non-basic type"
    Just ts ->
      return Lambda {
          lambdaParams = params
        , lambdaReturnType = map Basic ts
        , lambdaBody = body'
        }
  where allBasic = mapM isBasic
        isBasic (Basic t) = Just t
        isBasic _         = Nothing

-- | Conveniently construct a body that contains no bindings.
resultBody :: Bindable lore => [SubExp] -> Body lore
resultBody = mkBody []

-- | Conveniently construct a body that contains no bindings - but
-- this time, monadically!
resultBodyM :: MonadBinder m =>
               [SubExp]
            -> m (Body (Lore m))
resultBodyM = mkBodyM []

-- | Evaluate the action, producing a body, then wrap it in all the
-- bindings it created using 'addBinding'.
insertBindingsM :: (MonadBinder m) =>
                   m (Body (Lore m)) -> m (Body (Lore m))
insertBindingsM m = do
  (Body _ bnds res, otherbnds) <- collectBindings m
  mkBodyM (otherbnds <> bnds) res

-- | Change that subexpression where evaluation of the body would
-- stop.
mapResultM :: MonadBinder m =>
              (Result -> m (Body (Lore m))) -> Body (Lore m) -> m (Body (Lore m))
mapResultM f (Body _ bnds res) = do
  Body _ bnds2 res' <- f res
  mkBodyM (bnds++bnds2) res'

-- | Change that result where evaluation of the body would stop.  Also
-- change type annotations at branches.  This a non-monadic variant of
-- @mapResultM@.
mapResult :: Bindable lore =>
             (Result -> Body lore) -> Body lore -> Body lore
mapResult f (Body _ bnds res) =
  let Body _ bnds2 newres = f res
  in mkBody (bnds<>bnds2) newres

nonuniqueParams :: (MonadFreshNames m, Bindable lore) =>
                   [Param] -> m ([Param], [Binding lore])
nonuniqueParams params =
  modifyNameSource $ runState $ liftM fst $ runBinderEmptyEnv $
  collectBindings $ forM params $ \param ->
    if unique $ identType param then do
      param' <- nonuniqueParam <$> newIdent' (++"_nonunique") param
      bindingIdentTypes [param'] $
        letBindNames_ [(identName param,BindVar)] $
        PrimOp $ Copy $ identName param'
      return param'
    else
      return param
  where nonuniqueParam param =
          param { identType = identType param `setUniqueness` Nonunique }

-- | Instantiate all existential parts dimensions of the given
-- 'RetType', using a monadic action to create the necessary
-- 'SubExp's.  You should call this function within some monad that
-- allows you to collect the actions performed (say, 'Writer').
instantiateShapes :: Monad m =>
                     (Int -> m SubExp)
                  -> [TypeBase ExtShape]
                  -> m [TypeBase Shape]
instantiateShapes f ts = evalStateT (mapM instantiate ts) HM.empty
  where instantiate t = do
          shape <- mapM instantiate' $ extShapeDims $ arrayShape t
          return $ t `setArrayShape` Shape shape
        instantiate' (Ext x) = do
          m <- get
          case HM.lookup x m of
            Just se -> return se
            Nothing -> do se <- lift $ f x
                          put $ HM.insert x se m
                          return se
        instantiate' (Free se) = return se

instantiateShapes' :: MonadFreshNames m =>
                      [TypeBase ExtShape]
                   -> m ([TypeBase Shape], [Ident])
instantiateShapes' ts =
  runWriterT $ instantiateShapes instantiate ts
  where instantiate _ = do v <- lift $ newIdent "size" (Basic Int)
                           tell [v]
                           return $ Var $ identName v

instantiateShapesFromIdentList :: [Ident] -> [ExtType] -> [Type]
instantiateShapesFromIdentList idents ts =
  evalState (instantiateShapes instantiate ts) idents
  where instantiate _ = do
          idents' <- get
          case idents' of
            [] -> fail "instantiateShapesFromIdentList: insufficiently sized context"
            ident:idents'' -> do put idents''
                                 return $ Var $ identName ident

instantiateExtTypes :: [VName] -> [ExtType] -> [Ident]
instantiateExtTypes names rt =
  let (shapenames,valnames) = splitAt (shapeContextSize rt) names
      shapes = [ Ident name (Basic Int) | name <- shapenames ]
      valts  = instantiateShapesFromIdentList shapes rt
      vals   = [ Ident name t | (name,t) <- zip valnames valts ]
  in shapes ++ vals

instantiateIdents :: [VName] -> [ExtType]
                  -> Maybe ([Ident], [Ident])
instantiateIdents names ts
  | let n = shapeContextSize ts,
    n + length ts == length names = do
    let (context, vals) = splitAt n names
        nextShape _ = do
          (context', remaining) <- get
          case remaining of []   -> lift Nothing
                            x:xs -> do let ident = Ident x (Basic Int)
                                       put (context'++[ident], xs)
                                       return $ Var x
    (ts', (context', _)) <-
      runStateT (instantiateShapes nextShape ts) ([],context)
    return (context', zipWith Ident vals ts')
  | otherwise = Nothing
