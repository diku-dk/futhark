{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Get a mapping from statement name to PrimExp (if the statement has a
-- primitive expression) for all statements.
module Futhark.Optimise.MemoryBlockMerging.PrimExps
  ( findPrimExpsFunDef
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       (ExplicitMemorish, ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel
import Futhark.Tools

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


type CurrentTypes = M.Map VName PrimType
type PrimExps = M.Map VName (PrimExp VName)

newtype FindM lore a = FindM { unFindM :: RWS () PrimExps CurrentTypes a }
  deriving (Monad, Functor, Applicative,
            MonadWriter PrimExps,
            MonadState CurrentTypes)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore)

coerce :: FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- Find/construct all 'PrimExp's in a function definition.
findPrimExpsFunDef :: FunDef ExplicitMemory -> PrimExps
findPrimExpsFunDef fundef =
  let m = unFindM $ do
        lookInFParams $ funDefParams fundef
        lookInBody $ funDefBody fundef
      res = snd $ evalRWS m () M.empty
  in res

lookInFParams :: LoreConstraints lore =>
                 [FParam lore] -> FindM lore ()
lookInFParams params = forM_ params $ \(Param var membound) -> do
  case typeOf membound of
    Prim pt -> modify $ M.insert var pt
    _ -> return ()

  case membound of
    ExpMem.MemArray pt shape _ (ExpMem.ArrayIn mem _) -> do
      let matchingSizeVar (Param mem1 (ExpMem.MemMem (Var mem_size) _))
            | mem1 == mem = Just mem_size
          matchingSizeVar _ = Nothing
      case mapMaybe matchingSizeVar params of
        [mem_size] -> do
          let prod_i32 = product (map (primExpFromSubExp (IntType Int32)) (shapeDims shape))
          let prod_i64 = ConvOpExp (SExt Int32 Int64) prod_i32
          let pe = prod_i64 * primByteSize pt
          tell $ M.singleton mem_size pe
        _ -> return ()
    _ -> return ()

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
  prim_types <- get
  let varUse v = ExpMem.LeafExp v <$> M.lookup v prim_types

  case patvalelems of
    [PatElem dst _] ->
      forM_ (primExpFromExp varUse e) $ tell . M.singleton dst
    _ -> return ()

  forM_ patvalelems $ \(PatElem var membound) ->
    case typeOf membound of
      Prim pt ->
        modify $ M.insert var pt
      _ -> return ()

  -- Recursive body walk.
  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInBody . lambdaBody
          }
