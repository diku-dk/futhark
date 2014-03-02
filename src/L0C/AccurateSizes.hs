{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0C.AccurateSizes
  (
   addSizeInformation
  )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import L0C.InternalRep
import L0C.MonadFreshNames

import qualified Data.HashMap.Lazy as HM

addSizeInformation :: Prog -> Prog
addSizeInformation prog = runSizeM prog $
  Prog <$> mapM functionSizes (progFunctions prog)

data SizeEnv = SizeEnv {
    envVtable :: HM.HashMap VName [SubExp]
  }

newtype SizeM a = SizeM (ReaderT SizeEnv (State VNameSource) a)
  deriving (Applicative, Functor, Monad,
            MonadReader SizeEnv, MonadState VNameSource)

instance MonadFreshNames SizeM where
  getNameSource = get
  putNameSource = put

bindShapes :: HM.HashMap VName [SubExp] -> SizeM a -> SizeM a
bindShapes shapes = local bind
  where bind env = env { envVtable = shapes `HM.union` envVtable env }

sameShapes :: Ident -> Ident -> SizeM a -> SizeM a
sameShapes dest src = local bind
  where bind env =
          case HM.lookup (identName src) $ envVtable env of
            Just srcShape ->
              env { envVtable = HM.insert (identName dest) srcShape $
                                envVtable env }
            Nothing -> error "Shape not found - should never happen."

lookupSize :: Int -> Ident -> SizeM SubExp
lookupSize i v = do
  shape <- asks $ HM.lookup (identName v) . envVtable
  case shape of Nothing     -> error "Unknown variable"
                Just shape' -> return $ shape' !! i

runSizeM :: Prog -> SizeM a -> a
runSizeM prog (SizeM m) = evalState (runReaderT m env) src
  where src = newNameSourceForProg prog
        env = SizeEnv {
                envVtable = HM.empty
              }

functionSizes :: FunDec -> SizeM FunDec
functionSizes (fname, rettype, params, body, loc) = do
  shapes <- liftM HM.fromList $ forM params $ \param -> do
    let rank = arrayRank (identType param)
    names <- replicateM rank $ newNameFromString "param_size"
    return (identName param,
            [ Ident name (Basic Int) loc | name <- names ])
  let params' = concatMap addShape params
      addShape v = case HM.lookup (identName v) shapes of
                     Just shape -> fromParam v : shape
                     Nothing    -> [fromParam v]
      rettype' = concatMap addShapeTypes rettype
      addShapeTypes t = t : replicate (arrayRank t) (Basic Int)
  body' <- bindShapes (HM.map (map Var) shapes) $ bodySizes body
  return (fname, rettype', map toParam params', body', loc)

bodySizes :: Body -> SizeM Body

bodySizes (LetPat pat e body loc) = do
  (shapes,e') <- expSizes pat e
  body' <- bindShapes (HM.map (map shapeBindingIdent) shapes) $ bodySizes body
  let pat' = concatMap addShape pat
      addShape v =
        case HM.lookup (identName v) shapes of
          Just shape -> v : mapMaybe isComputedShape shape
          Nothing    -> [v]
  return $ LetPat pat' e' body' loc

bodySizes (LetWith cs dest src idx iv body loc) =
  LetWith cs dest src idx iv <$>
          sameShapes dest src (bodySizes body) <*> pure loc

bodySizes (DoLoop merge i bound loopbody letbody loc) =
  DoLoop merge i bound <$>
  bodySizes loopbody <*> bodySizes letbody <*> pure loc -- XXX need shape bindings

bodySizes (Result cs es loc) = do
  es' <- concat <$> mapM addShapes es
  return $ Result cs es' loc
  where addShapes (Constant v cloc) =
          return [Constant v cloc]
        addShapes (Var v) = do
          shape <- asks $ HM.lookup (identName v) . envVtable
          case shape of
            Just shape' -> return $ Var v : shape'
            Nothing     -> return [Var v]

expSizes :: [Ident] -> Exp -> SizeM (HM.HashMap VName [ShapeBinding], Exp)

expSizes pat (Map cs fun args loc) = do
  fun' <- sizeLambda fun
  shapes <-
    liftM HM.fromList $ forM (zip pat args) $ \(v, arg) -> do
      names <- replicateM (arrayRank (identType v) - 1) $
               newNameFromString "map_shape"
      let Var argv = arg
      argSize <- lookupSize 0 argv
      return (identName v,
              ExistingShape argSize :
              [ ComputedShape $ Ident name (arrayType 1 (Basic Int) Unique) loc
                  | name <- names ])
  return (shapes, Map cs fun' args loc)

expSizes [v] (Iota e loc) =
  return (HM.singleton (identName v) [ExistingShape e], Iota e loc)

expSizes _ (Size _ i (Var v) _) = do
  se <- lookupSize i v
  return (HM.empty, SubExp se)

expSizes pat e
  | all (basicType . identType) pat =
    return (HM.empty, e)

sizeLambda :: Lambda -> SizeM Lambda
sizeLambda (Lambda params body rettype loc) = do
  body' <- bodySizes body
  return $ Lambda params body' rettype' loc
  where rettype' = concatMap addShapes rettype
        addShapes t =
          t : replicate (arrayRank t) (Basic Int)

data ShapeBinding = ComputedShape Ident
                  | ExistingShape SubExp

isComputedShape :: ShapeBinding -> Maybe Ident
isComputedShape (ComputedShape v) = Just v
isComputedShape (ExistingShape _) = Nothing

shapeBindingIdent :: ShapeBinding -> SubExp
shapeBindingIdent (ComputedShape v) = Var v
shapeBindingIdent (ExistingShape v) = v
