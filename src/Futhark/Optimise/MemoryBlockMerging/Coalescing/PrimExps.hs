{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Optimise.MemoryBlockMerging.Coalescing.PrimExps
  ( findPrimExpsFunDef
  ) where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
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

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

findPrimExpsFunDef :: LoreConstraints lore =>
                      FunDef lore -> PrimExps
findPrimExpsFunDef fundef =
  let m = unFindM $
        -- We do not need to look in the function parameters.  That would likely
        -- find more mappings, but none of them would be able to be transformed
        -- to e.g. BinOps of other variables, so it would not be of much use to
        -- the modules that use this module.
        lookInBody $ funDefBody fundef
      res = snd $ evalRWS m () M.empty
  in res

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
  let varUse (BasicOp (SubExp (Var v))) = do
        pt <- M.lookup v prim_types
        return $ ExpMem.LeafExp v pt
      varUse _ = Nothing

  case patvalelems of
    [PatElem dst _ _] ->
      onJust (primExpFromExp varUse e) $ tell . M.singleton dst
    _ -> return ()

  forM_ patvalelems $ \(PatElem var _ membound) ->
    case typeOf membound of
      Prim pt ->
        modify $ M.insert var pt
      _ -> return ()

  -- RECURSIVE BODY WALK.
  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          }
