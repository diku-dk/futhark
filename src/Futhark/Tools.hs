{-# LANGUAGE FlexibleContexts #-}
module Futhark.Tools
  ( letSubExp
  , letSubExps
  , letExp
  , letExps
  , letTupExp
  , letTupExp'

  , newVar

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

  , copyConsumed
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
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Loc
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Substitute
import Futhark.Binder

letSubExp :: MonadBinder m =>
             String -> Exp (Lore m) -> m SubExp
letSubExp _ (PrimOp (SubExp se)) = return se
letSubExp desc e =
  case expExtType e of
    [_] -> Var <$> letExp desc e
    _   -> fail $ "letSubExp: tuple-typed expression given for " ++ desc ++ ":\n" ++ pretty e

letExp :: MonadBinder m =>
          String -> Exp (Lore m) -> m Ident
letExp _ (PrimOp (SubExp (Var v))) = return v
letExp desc e = do
  v <- newVName desc
  idents <- letBindNames [v] e
  case idents of
    [ident] -> return ident
    _       -> fail $ "letExp: tuple-typed expression given:\n" ++ pretty e

letSubExps :: MonadBinder m =>
              String -> [Exp (Lore m)] -> m [SubExp]
letSubExps desc = mapM $ letSubExp desc

letExps :: MonadBinder m =>
           String -> [Exp (Lore m)] -> m [Ident]
letExps desc = mapM $ letExp desc

letShapedExp :: (MonadBinder m) =>
                String -> Exp (Lore m)
             -> m ([Ident], [Ident])
letShapedExp _ (PrimOp (SubExp (Var v))) = return ([], [v])
letShapedExp name e = do
  names <- replicateM numValues $ newVName name
  idents <- letBindNames names e
  return $ splitAt (length idents - numValues) idents
  where numValues = length $ expExtType e

letTupExp :: (MonadBinder m) =>
             String -> Exp (Lore m)
          -> m [Ident]
letTupExp name e = snd <$> letShapedExp name e

letTupExp' :: (MonadBinder m) =>
              String -> Exp (Lore m)
           -> m [SubExp]
letTupExp' _ (PrimOp (SubExp se)) = return [se]
letTupExp' name ses = do vs <- letTupExp name ses
                         return $ map Var vs

newVar :: MonadFreshNames m =>
          SrcLoc -> String -> Type -> m (Ident, SubExp)
newVar loc name tp = do
  x <- newVName name
  return (Ident x tp loc, Var $ Ident x tp loc)

eSubExp :: MonadBinder m =>
           SubExp -> m (Exp (Lore m))
eSubExp = pure . PrimOp . SubExp

eIf :: MonadBinder m =>
       m (Exp (Lore m)) -> m (Body (Lore m)) -> m (Body (Lore m))
    -> SrcLoc
    -> m (Exp (Lore m))
eIf ce te fe loc = do
  ce' <- letSubExp "cond" =<< ce
  te' <- insertBindingsM te
  fe' <- insertBindingsM fe
  let ts = bodyExtType te' `generaliseExtTypes` bodyExtType fe'
  return $ If ce' te' fe' ts loc

eBinOp :: MonadBinder m =>
          BinOp -> m (Exp (Lore m)) -> m (Exp (Lore m)) -> Type -> SrcLoc
       -> m (Exp (Lore m))
eBinOp op x y t loc = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  return $ PrimOp $ BinOp op x' y' t loc

eNegate :: MonadBinder m =>
           m (Exp (Lore m)) -> SrcLoc -> m (Exp (Lore m))
eNegate e loc = do
  e' <- letSubExp "negate_arg" =<< e
  return $ PrimOp $ Negate e' loc

eNot :: MonadBinder m =>
        m (Exp (Lore m)) -> SrcLoc -> m (Exp (Lore m))
eNot e loc = do
  e' <- letSubExp "not_arg" =<< e
  return $ PrimOp $ Not e' loc

eIndex :: MonadBinder m =>
          Certificates -> Ident -> [m (Exp (Lore m))] -> SrcLoc
       -> m (Exp (Lore m))
eIndex cs a idxs loc = do
  idxs' <- letSubExps "i" =<< sequence idxs
  return $ PrimOp $ Index cs a idxs' loc

eCopy :: MonadBinder m =>
         m (Exp (Lore m)) -> m (Exp (Lore m))
eCopy e = do e' <- letSubExp "copy_arg" =<< e
             return $ PrimOp $ Copy e' $ srclocOf e'

eAssert :: MonadBinder m =>
         m (Exp (Lore m)) -> m (Exp (Lore m))
eAssert e = do e' <- letSubExp "assert_arg" =<< e
               return $ PrimOp $ Assert e' $ srclocOf e'

eValue :: MonadBinder m => Value -> SrcLoc -> m (Exp (Lore m))
eValue (BasicVal bv) loc =
  return $ PrimOp $ SubExp $ Constant bv loc
eValue (ArrayVal a t) loc = do
  let rowshape = [ Constant (IntVal d) loc
                 | d <- drop 1 $ valueShape (ArrayVal a t)
                 ]
  ses <- mapM (letSubExp "array_elem" <=< (`eValue` loc)) $ A.elems a
  return $ PrimOp $ ArrayLit ses (t `setArrayDims` rowshape) loc

eBody :: (MonadBinder m) =>
         [m (Exp (Lore m))]
      -> m (Body (Lore m))
eBody es = insertBindingsM $ do
             es' <- sequence es
             xs <- mapM (letTupExp "x") es'
             let loc = case es' of []  -> noLoc
                                   e:_ -> srclocOf e
             mkBodyM [] $ Result [] (map Var $ concat xs) loc

eLambda :: MonadBinder m =>
           Lambda (Lore m) -> [SubExp] -> m [SubExp]
eLambda lam args = do zipWithM_ letBindNames params $
                        map (PrimOp . SubExp) args
                      bodyBind $ lambdaBody lam
  where params = map (pure . identName) $ lambdaParams lam

-- | Apply a binary operator to several subexpressions.  A left-fold.
foldBinOp :: MonadBinder m =>
             BinOp -> SubExp -> [SubExp] -> Type -> m (Exp (Lore m))
foldBinOp _ ne [] _   = return $ PrimOp $ SubExp ne
foldBinOp bop ne (e:es) t =
  eBinOp bop (pure $ PrimOp $ SubExp e) (foldBinOp bop ne es t) t (srclocOf e)

-- | Create a two-parameter lambda whose body applies the given binary
-- operation to its arguments.  It is assumed that both argument and
-- result types are the same.  (This assumption should be fixed at
-- some point.)
binOpLambda :: (MonadFreshNames m, Bindable lore) =>
               BinOp -> Type -> SrcLoc -> m (Lambda lore)
binOpLambda bop t loc = do
  x   <- newIdent "x"   t loc
  y   <- newIdent "y"   t loc
  res <- newIdent "res" t loc
  body <- runBinder $ do
    bnds <- mkLetNamesM [identName res] $
            PrimOp $ BinOp bop (Var x) (Var y) t loc
    mkBodyM [bnds] $ Result [] [Var res] loc
  return Lambda {
             lambdaParams     = [x, y]
           , lambdaReturnType = [t]
           , lambdaSrcLoc     = loc
           , lambdaBody       = body
           }

makeLambda :: (Bindable (Lore m), MonadBinder m) =>
              [Param] -> m (Body (Lore m)) -> m (Lambda (Lore m))
makeLambda params body = do
  body' <- insertBindingsM body
  case allBasic $ bodyExtType body' of
    Nothing -> fail "Body passed to makeLambda has non-basic type"
    Just ts ->
      return Lambda {
          lambdaParams = params
        , lambdaSrcLoc = srclocOf body'
        , lambdaReturnType = map Basic ts
        , lambdaBody = body'
        }
  where allBasic = mapM isBasic
        isBasic (Basic t) = Just t
        isBasic _         = Nothing

-- | Conveniently construct a body that contains no bindings.
resultBody :: Bindable lore => Certificates -> [SubExp] -> SrcLoc -> Body lore
resultBody cs ses loc = mkBody [] $ Result cs ses loc

-- | Conveniently construct a body that contains no bindings - but
-- this time, monadically!
resultBodyM :: MonadBinder m =>
               Certificates
            -> [SubExp]
            -> SrcLoc
            -> m (Body (Lore m))
resultBodyM cs ses loc =
  mkBodyM [] $ Result cs ses loc

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
                 PrimOp $ Copy (Var v) loc
          where loc = srclocOf v

        freeUniqueInBody = HS.filter (unique . identType) . freeInBody

nonuniqueParams :: (MonadFreshNames m, Bindable lore) =>
                   [Param] -> m ([Param], [Binding lore])
nonuniqueParams params = runBinder'' $ forM params $ \param ->
  if unique $ identType param then do
    param' <- nonuniqueParam <$> newIdent' (++"_nonunique") param
    _ <- mkLetNamesM [identName param] $
         PrimOp $ Copy (Var param') $ srclocOf param'
    return param'
  else
    return param
  where nonuniqueParam param =
          param { identType = identType param `setUniqueness` Nonunique }

-- | Instantiate all existential parts dimensions of the given
-- 'RetType', using a monadic action to create the necessary
-- 'SubExp's.  You should call this function within some monad that
-- allows you to collect the actions performed (say, 'Writer').
instantiateShapes :: Monad m => m SubExp -> [TypeBase ExtShape]
                  -> m [TypeBase Shape]
instantiateShapes f ts = evalStateT (mapM instantiate ts) HM.empty
  where instantiate t = do
          shape <- mapM instantiate' $ extShapeDims $ arrayShape t
          return $ t `setArrayShape` Shape shape
        instantiate' (Ext x) = do
          m <- get
          case HM.lookup x m of
            Just se -> return se
            Nothing -> do se <- lift f
                          put $ HM.insert x se m
                          return se
        instantiate' (Free se) = return se

instantiateShapes' :: MonadFreshNames m =>
                      SrcLoc
                   -> [TypeBase ExtShape]
                   -> m ([TypeBase Shape], [Ident])
instantiateShapes' loc ts =
  runWriterT $ instantiateShapes instantiate ts
  where instantiate = do v <- lift $ newIdent "size" (Basic Int) loc
                         tell [v]
                         return $ Var v

instantiateShapesFromIdentList :: [Ident] -> [ExtType] -> [Type]
instantiateShapesFromIdentList idents ts =
  evalState (instantiateShapes instantiate ts) idents
  where instantiate = do
          idents' <- get
          case idents' of
            [] -> fail "instantiateShapesFromIdentList: insufficiently sized context"
            ident:idents'' -> do put idents''
                                 return $ Var ident

instantiateExtTypes :: SrcLoc -> [VName] -> [ExtType] -> [Ident]
instantiateExtTypes loc names rt =
  let (shapenames,valnames) = splitAt (shapeContextSize rt) names
      shapes = [ Ident name (Basic Int) loc | name <- shapenames ]
      valts  = instantiateShapesFromIdentList shapes rt
      vals   = [ Ident name t loc | (name,t) <- zip valnames valts ]
  in shapes ++ vals

instantiateIdents :: SrcLoc -> [VName] -> [ExtType]
                  -> Maybe ([Ident], [Ident])
instantiateIdents loc names ts
  | let n = shapeContextSize ts,
    n + length ts == length names = do
    let (context, vals) = splitAt n names
        mkIdent name t = Ident name t loc
        nextShape = do
          (context', remaining) <- get
          case remaining of []   -> lift Nothing
                            x:xs -> do let ident = Ident x (Basic Int) loc
                                       put (context'++[ident], xs)
                                       return $ Var ident
    (ts', (context', _)) <-
      runStateT (instantiateShapes nextShape ts) ([],context)
    return (context', zipWith mkIdent vals ts')
  | otherwise = Nothing
