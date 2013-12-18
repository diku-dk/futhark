{-# LANGUAGE FlexibleContexts #-}
module L0C.BohriumBackend (compileExp) where

import Control.Applicative

import L0C.L0
import L0C.MonadFreshNames

import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.HORepresentation.SOACNest (SOACNest(..))
import qualified L0C.HORepresentation.SOACNest as Nest

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

alt :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
alt x y = do x' <- x
             case x' of
               Just _  -> return x'
               Nothing -> y

compileExp :: (Applicative m, MonadFreshNames VName m) =>
              (VName -> m C.Exp)
           -> C.Exp -> Exp -> m (Maybe [C.BlockItem])
compileExp lookupVar target e
  | Right nest <- Nest.fromExp e =
      let try f = f lookupVar target nest
      in try compileMap `alt`
         try compileReduce `alt`
         try compileMapWithReduce `alt`
         try compileMapWithScan
  | otherwise = return Nothing

type SOACCompiler m = (VName -> m C.Exp) -> C.Exp -> SOACNest -> m (Maybe [C.BlockItem])

compileMap :: (Applicative m, MonadFreshNames VName m) => SOACCompiler m
compileMap _ _ (SOACNest _ (Nest.Map2 _ (Nest.Lambda _) _ _)) =
  return Nothing
compileMap _ _ _ = return Nothing

compileReduce :: (Applicative m, MonadFreshNames VName m) => SOACCompiler m
compileReduce _ _ (SOACNest _ (Nest.Reduce2 _ (Nest.Lambda _) _ _ _)) =
  return Nothing
compileReduce _ _ _ = return Nothing

compileMapWithReduce :: (Applicative m, MonadFreshNames VName m) => SOACCompiler m
compileMapWithReduce _ _
                       (SOACNest _ (Nest.Reduce2 _ (Nest.NewNest _ (Nest.Map2 {})) _ _ _)) =
  return Nothing
compileMapWithReduce _ _ _ = return Nothing

compileMapWithScan :: (Applicative m, MonadFreshNames VName m) => SOACCompiler m
compileMapWithScan _ _
                       (SOACNest _ (Nest.Scan2 _ (Nest.NewNest _ (Nest.Map2 {})) _ _ _)) =
  return Nothing
compileMapWithScan _ _ _ = return Nothing
