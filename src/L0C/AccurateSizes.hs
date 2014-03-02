{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0C.AccurateSizes
  (
   addSizeInformation
  )
  where

import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State

import L0C.InternalRep
import L0C.MonadFreshNames

import qualified Data.HashMap.Lazy as HM

addSizeInformation :: Prog -> Prog
addSizeInformation prog = runSizeM prog $
  Prog <$> mapM functionSizes (progFunctions prog)

data SizeEnv = SizeEnv {
    envVtable :: HM.HashMap VName [Ident]
  }

newtype SizeM a = SizeM (ReaderT SizeEnv (State VNameSource) a)
  deriving (Applicative, Functor, Monad,
            MonadReader SizeEnv, MonadState VNameSource)

instance MonadFreshNames SizeM where
  getNameSource = get
  putNameSource = put

bindShapes :: HM.HashMap VName [Ident] -> SizeM a -> SizeM a
bindShapes shapes = local bind
  where bind env = env { envVtable = shapes `HM.union` envVtable env }

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
  body' <- bindShapes shapes $ bodySizes body
  return (fname, rettype, map toParam params', body', loc)

bodySizes :: Body -> SizeM Body

bodySizes (LetPat pat e body loc) = do
  (shapes,e') <- expSizes pat e
  body' <- bindShapes shapes $ bodySizes body
  let pat' = concatMap addShape pat
      addShape v = case HM.lookup (identName v) shapes of
                     Just shape -> v : shape
                     Nothing    -> [v]
  return $ LetPat pat' e' body' loc

bodySizes (Result cs es loc) = do
  es' <- concat <$> mapM addShapes es
  return $ Result cs es' loc
  where addShapes (Constant v cloc) =
          return [Constant v cloc]
        addShapes (Var v) = do
          shape <- asks $ HM.lookup (identName v) . envVtable
          case shape of
            Just shape' -> return $ map Var $ v : shape'
            Nothing     -> return [Var v]

expSizes :: [Ident] -> Exp -> SizeM (HM.HashMap VName [Ident], Exp)
expSizes pat (Map cs fun args loc) = do
  fun' <- sizeLambda fun
  shapes <-
    liftM HM.fromList $ forM pat $ \v -> do
      names <- replicateM (arrayRank $ identType v) $
               newNameFromString "map_shape"
      return (identName v,
              [ Ident name (Basic Int) loc | name <- names ])
  return (shapes, Map cs fun' args loc)

expSizes _ (Size _ i (Var v) _) = do
  shape <- asks $ HM.lookup (identName v) . envVtable
  case shape of
    Just shape' -> return (HM.empty, SubExp $ Var $ shape' !! i)
    Nothing     -> error "no"

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
