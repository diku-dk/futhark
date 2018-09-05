{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find all variable-to-memory mappings, so that other modules can lookup the
-- relation.  Maps array names to memory blocks.

module Futhark.Optimise.MemoryBlockMerging.VariableMemory
  ( findVarMemMappings
  ) where

import qualified Data.Map.Strict as M
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       (ExplicitMemorish, ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


newtype FindM lore a = FindM { unFindM :: Writer (VarMemMappings MemorySrc) a }
  deriving (Monad, Functor, Applicative,
            MonadWriter (VarMemMappings MemorySrc))

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore)

recordMapping :: VName -> MemorySrc -> FindM lore ()
recordMapping var memloc = tell $ M.singleton var memloc

coerce :: FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- | Find all variable-memory block mappings in a function definition.
findVarMemMappings :: FunDef ExplicitMemory -> VarMemMappings MemorySrc
findVarMemMappings fundef =
  let m = unFindM $ do
        mapM_ lookInFParam $ funDefParams fundef
        lookInBody $ funDefBody fundef
      var_to_mem = execWriter m
  in var_to_mem

lookInFParam :: LoreConstraints lore =>
                FParam lore -> FindM lore ()
lookInFParam (Param x (ExpMem.MemArray _ shape _ (ExpMem.ArrayIn xmem xixfun))) = do
  let memloc = MemorySrc xmem xixfun shape
  recordMapping x memloc
lookInFParam _ = return ()

lookInLParam :: LoreConstraints lore =>
                LParam lore -> FindM lore ()
lookInLParam (Param x (ExpMem.MemArray _ shape _ (ExpMem.ArrayIn xmem xixfun))) = do
  let memloc = MemorySrc xmem xixfun shape
  recordMapping x memloc
lookInLParam _ = return ()

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
  mapM_ lookInPatValElem patvalelems
  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody
          , walkOnFParam = lookInFParam
          , walkOnLParam = lookInLParam
          }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInLambda
          , walkOnKernelLParam = lookInLParam
          }

lookInPatValElem :: LoreConstraints lore =>
                    PatElem lore -> FindM lore ()
lookInPatValElem (PatElem x (ExpMem.MemArray _ shape _ (ExpMem.ArrayIn xmem xixfun))) = do
  let memloc = MemorySrc xmem xixfun shape
  recordMapping x memloc
lookInPatValElem _ = return ()

lookInLambda :: LoreConstraints lore =>
                Lambda lore -> FindM lore ()
lookInLambda (Lambda params body _) = do
  forM_ params lookInLParam
  lookInBody body
