{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find all variable aliases.  Avoids having to use the Aliases representation
-- in other modules.
module Futhark.Optimise.MemoryBlockMerging.VariableAliases where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.Representation.Aliases (Aliases, unNames)
import Futhark.Representation.ExplicitMemory
       (ExplicitMemorish, ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel
import Futhark.Analysis.Alias (analyseFun)

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


newtype FindM lore a = FindM { unFindM :: Writer [VarAliases] a }
  deriving (Monad, Functor, Applicative,
            MonadWriter [VarAliases])

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalkAliases lore)

recordMapping :: VName -> Names -> FindM lore ()
recordMapping var names = tell [M.singleton var names]

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- | Find all variable aliases in a function definition.
findVarAliases :: FunDef ExplicitMemory -> VarAliases
findVarAliases fundef =
  let fundef' = analyseFun fundef
      m = unFindM $ lookInBody $ funDefBody fundef'
      var_aliases = M.unionsWith S.union $ execWriter m
      var_aliases' = removeEmptyMaps $ expandWithAliases var_aliases var_aliases
  in var_aliases'

lookInBody :: LoreConstraints lore =>
              Body (Aliases lore) -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody (Aliases lore) -> FindM lore ()
lookInKernelBody (KernelBody _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: LoreConstraints lore =>
             Stm (Aliases lore) -> FindM lore ()
lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
  mapM_ lookInPatValElem patvalelems
  fullWalkAliasesExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody
          }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInBody . lambdaBody
          }

lookInPatValElem :: LoreConstraints lore =>
                    PatElem (Aliases lore) -> FindM lore ()
lookInPatValElem (PatElem x (names', ExpMem.MemArray{})) = do
  let aliases = unNames names'
  recordMapping x aliases
lookInPatValElem _ = return ()
