{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Construct
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
  , eCmpOp
  , eNegate
  , eNot
  , eAbs
  , eSignum
  , eCopy
  , eAssert
  , eValue
  , eBody
  , eLambda
  , eDivRoundingUp
  , eRoundToMultipleOf

  , resultBody
  , resultBodyM
  , insertBindingsM
  , mapResultM
  , mapResult

  , foldBinOp
  , binOpLambda
  , fullSlice
  , fullSliceNum

  , module Futhark.Binder

  -- * Result types
  , instantiateShapes
  , instantiateShapes'
  , instantiateShapesFromIdentList
  , instantiateExtTypes
  , instantiateIdents

  -- * Convenience
  , simpleMkLetNames
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
letSubExp _ (BasicOp (SubExp se)) = return se
letSubExp desc e = Var <$> letExp desc e

letExp :: MonadBinder m =>
          String -> Exp (Lore m) -> m VName
letExp _ (BasicOp (SubExp (Var v))) =
  return v
letExp desc e = do
  n <- length <$> expExtType e
  vs <- replicateM n $ newVName desc
  idents <- letBindNames' vs e
  case idents of
    [ident] -> return $ identName ident
    _       -> fail $ "letExp: tuple-typed expression given:\n" ++ pretty e

letInPlace :: MonadBinder m =>
              String -> Certificates -> VName -> Slice SubExp -> Exp (Lore m)
           -> m VName
letInPlace desc cs src slice e = do
  v <- newVName desc
  idents <- letBindNames [(v,BindInPlace cs src slice)] e
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
letTupExp _ (BasicOp (SubExp (Var v))) =
  return [v]
letTupExp name e = do
  numValues <- length <$> expExtType e
  names <- replicateM numValues $ newVName name
  map identName <$> letBindNames' names e

letTupExp' :: (MonadBinder m) =>
              String -> Exp (Lore m)
           -> m [SubExp]
letTupExp' _ (BasicOp (SubExp se)) = return [se]
letTupExp' name ses = do vs <- letTupExp name ses
                         return $ map Var vs

eSubExp :: MonadBinder m =>
           SubExp -> m (Exp (Lore m))
eSubExp = pure . BasicOp . SubExp

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
          BinOp -> m (Exp (Lore m)) -> m (Exp (Lore m))
       -> m (Exp (Lore m))
eBinOp op x y = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  return $ BasicOp $ BinOp op x' y'

eCmpOp :: MonadBinder m =>
          CmpOp -> m (Exp (Lore m)) -> m (Exp (Lore m))
       -> m (Exp (Lore m))
eCmpOp op x y = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  return $ BasicOp $ CmpOp op x' y'

eNegate :: MonadBinder m =>
           m (Exp (Lore m)) -> m (Exp (Lore m))
eNegate em = do
  e <- em
  e' <- letSubExp "negate_arg" e
  t <- subExpType e'
  case t of
    Prim (IntType int_t) ->
      return $ BasicOp $
      BinOp (Sub int_t) (intConst int_t 0) e'
    Prim (FloatType float_t) ->
      return $ BasicOp $
      BinOp (FSub float_t) (floatConst float_t 0) e'
    _ ->
      fail $ "eNegate: operand " ++ pretty e ++ " has invalid type."

eNot :: MonadBinder m =>
        m (Exp (Lore m)) -> m (Exp (Lore m))
eNot e = do
  e' <- letSubExp "not_arg" =<< e
  return $ BasicOp $ UnOp Not e'

eAbs :: MonadBinder m =>
        m (Exp (Lore m)) -> m (Exp (Lore m))
eAbs em = do
  e <- em
  e' <- letSubExp "abs_arg" e
  t <- subExpType e'
  case t of
    Prim (IntType int_t) ->
      return $ BasicOp $ UnOp (Abs int_t) e'
    Prim (FloatType float_t) ->
      return $ BasicOp $ UnOp (FAbs float_t) e'
    _ ->
      fail $ "eAbs: operand " ++ pretty e ++ " has invalid type."

eSignum :: MonadBinder m =>
        m (Exp (Lore m)) -> m (Exp (Lore m))
eSignum em = do
  e <- em
  e' <- letSubExp "signum_arg" e
  t <- subExpType e'
  case t of
    Prim (IntType int_t) ->
      return $ BasicOp $ UnOp (SSignum int_t) e'
    _ ->
      fail $ "eSignum: operand " ++ pretty e ++ " has invalid type."

eCopy :: MonadBinder m =>
         m (Exp (Lore m)) -> m (Exp (Lore m))
eCopy e = do e' <- letExp "copy_arg" =<< e
             return $ BasicOp $ Copy e'

eAssert :: MonadBinder m =>
         m (Exp (Lore m)) -> SrcLoc -> m (Exp (Lore m))
eAssert e loc = do e' <- letSubExp "assert_arg" =<< e
                   return $ BasicOp $ Assert e' loc

eValue :: MonadBinder m => Value -> m (Exp (Lore m))
eValue (PrimVal bv) =
  return $ BasicOp $ SubExp $ Constant bv
eValue (ArrayVal a bt [_]) = do
  let ses = map Constant $ A.elems a
  return $ BasicOp $ ArrayLit ses $ Prim bt
eValue (ArrayVal a bt shape) = do
  let rowshape = drop 1 shape
      rowsize  = product rowshape
      rows     = [ ArrayVal (A.listArray (0,rowsize-1) r) bt rowshape
                 | r <- chunk rowsize $ A.elems a ]
      rowtype = Array bt (Shape $ map (intConst Int32 . toInteger) rowshape)
                NoUniqueness
  ses <- mapM (letSubExp "array_elem" <=< eValue) rows
  return $ BasicOp $ ArrayLit ses rowtype

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
                        map (BasicOp . SubExp) args
                      bodyBind $ lambdaBody lam
  where params = [ [(param, BindVar)] |
                   param <- map paramName $ lambdaParams lam ]

-- | Note: unsigned division.
eDivRoundingUp :: MonadBinder m =>
                  IntType -> m (Exp (Lore m)) -> m (Exp (Lore m)) -> m (Exp (Lore m))
eDivRoundingUp t x y =
  eBinOp (SQuot t) (eBinOp (Add t) x (eBinOp (Sub t) y (eSubExp one))) y
  where one = intConst t 1

eRoundToMultipleOf :: MonadBinder m =>
                      IntType -> m (Exp (Lore m)) -> m (Exp (Lore m)) -> m (Exp (Lore m))
eRoundToMultipleOf t x d =
  ePlus x (eMod (eMinus d (eMod x d)) d)
  where eMod = eBinOp (SMod t)
        eMinus = eBinOp (Sub t)
        ePlus = eBinOp (Add t)

-- | Apply a binary operator to several subexpressions.  A left-fold.
foldBinOp :: MonadBinder m =>
             BinOp -> SubExp -> [SubExp] -> m (Exp (Lore m))
foldBinOp _ ne [] =
  return $ BasicOp $ SubExp ne
foldBinOp bop ne (e:es) =
  eBinOp bop (pure $ BasicOp $ SubExp e) (foldBinOp bop ne es)

-- | Create a two-parameter lambda whose body applies the given binary
-- operation to its arguments.  It is assumed that both argument and
-- result types are the same.  (This assumption should be fixed at
-- some point.)
binOpLambda :: (MonadFreshNames m, Bindable lore) =>
               BinOp -> PrimType -> m (Lambda lore)
binOpLambda bop t = do
  x   <- newVName "x"
  y   <- newVName "y"
  (body, _) <- runBinderEmptyEnv $ insertBindingsM $ do
    res <- letSubExp "res" $ BasicOp $ BinOp bop (Var x) (Var y)
    return $ resultBody [res]
  return Lambda {
             lambdaParams     = [Param x (Prim t),
                                 Param y (Prim t)]
           , lambdaReturnType = [Prim t]
           , lambdaBody       = body
           }

-- | @fullSlice t slice@ returns @slice@, but with 'DimSlice's of
-- entire dimensions appended to the full dimensionality of @t@.  This
-- function is used to turn incomplete indexing complete, as required
-- by 'Index'.
fullSlice :: Type -> [DimIndex SubExp] -> Slice SubExp
fullSlice t slice =
  slice ++
  map (DimSlice (constant (0::Int32)))
  (drop (length slice) $ arrayDims t)

-- | Like 'fullSlice', but the dimensions are simply numeric.
fullSliceNum :: Num d => [d] -> [DimIndex d] -> Slice d
fullSliceNum dims slice =
  slice ++ map (DimSlice 0) (drop (length slice) dims)

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

-- | Instantiate all existential parts dimensions of the given
-- type, using a monadic action to create the necessary 'SubExp's.
-- You should call this function within some monad that allows you to
-- collect the actions performed (say, 'Writer').
instantiateShapes :: Monad m =>
                     (Int -> m SubExp)
                  -> [TypeBase ExtShape u]
                  -> m [TypeBase Shape u]
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
                      [TypeBase ExtShape u]
                   -> m ([TypeBase Shape u], [Ident])
instantiateShapes' ts =
  runWriterT $ instantiateShapes instantiate ts
  where instantiate _ = do v <- lift $ newIdent "size" (Prim $ IntType Int32)
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
      shapes = [ Ident name (Prim $ IntType Int32) | name <- shapenames ]
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
                            x:xs -> do let ident = Ident x (Prim $ IntType Int32)
                                       put (context'++[ident], xs)
                                       return $ Var x
    (ts', (context', _)) <-
      runStateT (instantiateShapes nextShape ts) ([],context)
    return (context', zipWith Ident vals ts')
  | otherwise = Nothing

-- | Can be used as the definition of 'mkLetNames' for a 'Bindable'
-- instance for simple representations.
simpleMkLetNames :: (ExpAttr lore ~ (), LetAttr lore ~ Type,
                     MonadFreshNames m, TypedOp (Op lore), HasScope lore m) =>
                    [(VName, Bindage)] -> Exp lore -> m (Binding lore)
simpleMkLetNames names e = do
  et <- expExtType e
  (ts, shapes) <- instantiateShapes' et
  let shapeElems = [ PatElem shape BindVar shapet
                   | Ident shape shapet <- shapes
                   ]
      mkValElem (name, BindVar) t =
        return $ PatElem name BindVar t
      mkValElem (name, bindage@(BindInPlace _ src _)) _ = do
        srct <- lookupType src
        return $ PatElem name bindage srct
  valElems <- zipWithM mkValElem names ts
  return $ Let (Pattern shapeElems valElems) () e
