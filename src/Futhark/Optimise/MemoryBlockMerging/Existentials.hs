{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find all existential variables.
module Futhark.Optimise.MemoryBlockMerging.Existentials
  ( findExistentials
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


newtype FindM lore a = FindM { unFindM :: Writer Names a }
  deriving (Monad, Functor, Applicative,
            MonadWriter Names)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore)

record :: VName -> FindM lore ()
record = tell . S.singleton

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

findExistentials :: LoreConstraints lore =>
                    FunDef lore -> Names
findExistentials fundef =
  let m = unFindM $ lookInBody $ funDefBody fundef
      existentials = execWriter m
  in existentials

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
lookInStm (Let (Pattern patctxelems patvalelems) _ e) = do
  forM_ patvalelems $ \(PatElem var _ membound) ->
    case membound of
      ExpMem.ArrayMem _ _ _ mem _ ->
        when (mem `L.elem` map patElemName patctxelems)
        $ record var
      _ -> return ()

  case e of
    DoLoop mergectxparams mergevalparams _loopform _body ->
      forM_ mergevalparams $ \(Param var membound, _) ->
        case membound of
          ExpMem.ArrayMem _ _ _ mem _ ->
            when (mem `L.elem` map (paramName . fst) mergectxparams)
            $ record var
          _ -> return ()
    _ -> return ()

  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInBody . lambdaBody
          }
