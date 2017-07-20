{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.Optimise.MemoryBlockMerging.VariableAliases where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Futhark.Representation.AST
import Futhark.Representation.Aliases (Aliases, unNames)
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Analysis.Alias (analyseFun)

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


findVarAliases :: forall lore. ExplicitMemorish lore -- Maybe overkill.
               => FunDef lore -> VarAliases
findVarAliases fundef = cleanupMapping $ expandWithAliases fromBody fromBody
  where fundef' = analyseFun fundef

        fromBody = M.unionsWith S.union $ map (uncurry M.singleton)
                   $ onBody $ funDefBody fundef'

        onBody :: Body (Aliases lore) -> [(VName, Names)]
        onBody body = concatMap onStm $ bodyStms body

        onStm :: Stm (Aliases lore) -> [(VName, Names)]
        onStm (Let (Pattern patctxelems patvalelems) _ e) =
          let m0 = concatMap onPatElem (patctxelems ++ patvalelems)
              m1 = foldExp folder [] e
          in m0 ++ m1

        folder = identityFolder {
          foldOnBody = \mappings body -> return (mappings ++ onBody body)
          }

        onPatElem :: PatElem (Aliases lore) -> [(VName, Names)]
        onPatElem (PatElem x _bindage (names', ExpMem.ArrayMem{})) =
          let aliases = unNames names'
          in [(x, aliases)]

        onPatElem _ = []
