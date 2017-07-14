{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.Optimise.MemoryBlockMerging.VariableMemory where

import qualified Data.Map.Strict as M

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Types


findVarMemMappings :: forall lore. ExplicitMemorish lore
                   => FunDef lore -> VarMemMappings MemorySrc
findVarMemMappings fundef = M.union fromParams fromBody
  where fromParams = M.fromList $ concatMap onParam $ funDefParams fundef

        fromBody = M.fromList $ onBody $ funDefBody fundef

        onBody = concatMap onStm . bodyStms

        onParam (Param x (ExpMem.ArrayMem _ shape _ xmem xixfun)) =
          let memloc = MemorySrc xmem xixfun shape
          in [(x, memloc)]
        onParam _ = []

        onStm (Let (Pattern _patctxelems patvalelems) _ e) =
          let m0 = concatMap onPatValElem patvalelems
              m1 = foldExp folder [] e
          in m0 ++ m1

        onPatValElem (PatElem x _bindage (ExpMem.ArrayMem _ shape _ xmem xixfun)) =
          let memloc = MemorySrc xmem xixfun shape
          in [(x, memloc)]
        onPatValElem _ = []

        folder = identityFolder
          { foldOnBody = \mappings body -> return (mappings ++ onBody body)
          , foldOnFParam = \mappings fparam -> return (mappings ++ onParam fparam)
          , foldOnLParam = \mappings lparam -> return (mappings ++ onParam lparam)
          }
