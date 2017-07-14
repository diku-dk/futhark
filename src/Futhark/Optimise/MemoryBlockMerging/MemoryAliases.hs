{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.Optimise.MemoryBlockMerging.MemoryAliases where

import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L

import Futhark.Representation.AST
import Futhark.Representation.Aliases
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Analysis.Alias (analyseFun)

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


findMemAliases :: forall lore. ExplicitMemorish lore -- Maybe overkill.
               => FunDef lore -> VarMemMappings MemorySrc -> MemAliases
findMemAliases fundef var_to_mem = cleanupMapping $ expandWithAliases fromBody fromBody
  where fundef' = analyseFun fundef

        fromBody = M.unionsWith S.union $ map (uncurry M.singleton)
                   $ concatMap onStm $ bodyStms $ funDefBody fundef'

        onStm :: Stm (Aliases lore) -> [(VName, Names)]
        onStm (Let (Pattern patctxelems patvalelems) _ e) =
          let m0 = concatMap onPatElem (patctxelems ++ patvalelems)
              m1 = case e of
                DoLoop mergectxparams mergevalparams _loopform body ->
                  -- There are most likely more body results than
                  -- mergectxparams, but we are only interested in the first
                  -- body results anyway (those that have a matching location
                  -- with the mergectxparams).
                  concat (zipWith onMergeCtxParam mergectxparams (bodyResult body))
                  ++ concatMap (onMergeValParam body) mergevalparams
                  ++ concatMap (fromBodyTuples patctxelems (map snd mergectxparams) (bodyResult body)) patvalelems
                If _ body_then body_else _ ->
                  -- Pretty conservative.  Too restrictive if the If works on
                  -- tuples of arrays.  FIXME (similar to the DoLoop FIXME).
                  let ress = mapMaybe fromVar
                             (bodyResult body_then ++ bodyResult body_else)
                      mems = map memSrcName $ mapMaybe (`M.lookup` var_to_mem) ress
                      patAliases (PatElem var _ (_, ExpMem.MemMem{})) =
                        Just (var, S.fromList mems)
                      patAliases _ = Nothing
                  in mapMaybe patAliases patctxelems
                _ -> []
              m2 = foldExp folder [] e
          in m0 ++ m1 ++ m2

        folder = identityFolder {
          foldOnStm = \mappings stm -> return (mappings ++ onStm stm)
          }

        onMergeCtxParam :: (FParam (Aliases lore), SubExp) -> SubExp -> [(VName, Names)]
        onMergeCtxParam (Param xmem ExpMem.MemMem{}, Var param_mem) (Var body_mem_res) =
          let aliases = S.fromList [param_mem, body_mem_res]
          in [(xmem, aliases)]
        onMergeCtxParam _ _ = []

        onMergeValParam :: Body (Aliases lore) -> (FParam (Aliases lore), SubExp) -> [(VName, Names)]
        onMergeValParam body (Param _ (ExpMem.ArrayMem _ _ _ mem _), _t) =
          -- FIXME: This is probably more conservative than it needs to in case
          -- you have more than one loop array.  Fixing this would require
          -- either changing the Aliases representation, or building something
          -- on top of it.
          let aliases = S.unions $ map (lookupMems . unNames) $ fst $ fst $ bodyAttr body
          in [(mem, aliases)]
        onMergeValParam _ _ = []

        fromBodyTuples :: [PatElem (Aliases lore)]
                       -> [SubExp] -> [SubExp]
                       -> PatElem (Aliases lore)
                       -> [(VName, Names)]
        -- When a parameter refers to a existential memory, we want to find
        -- which return memory in the loop that the existential memory refers
        -- to.
        fromBodyTuples patctxelems body_params body_results
          (PatElem _ _ (_, ExpMem.ArrayMem _ _ _ mem _)) =
          let zipped = zip3 patctxelems body_params body_results
          in case L.find ((== mem) . patElemName . (\(x, _, _) -> x)) zipped of
            Just (_, Var param_mem, Var res_mem) ->
              [(mem, S.fromList [param_mem, res_mem])]
            _ -> []
        fromBodyTuples _ _ _ _ = []

        onPatElem :: PatElem (Aliases lore) -> [(VName, Names)]
        onPatElem (PatElem _ _ (names', ExpMem.ArrayMem _ _ _ xmem _)) =
          let aliases = lookupMems $ unNames names'
          in [(xmem, aliases)]
        onPatElem (PatElem xmem _ (names', ExpMem.MemMem {})) =
          let aliases = lookupMems $ unNames names'
          in [(xmem, aliases)]
        onPatElem _ = []

        lookupMems :: Names -> Names
        lookupMems var_aliases =
          S.fromList $ mapMaybe ((memSrcName <$>) . flip M.lookup var_to_mem)
          $ S.toList var_aliases

-- If the source and destination use the same memory block, those two
-- (identical) memory blocks obviously "alias" "each other".  We would need to
-- do something special if we were analysing variable aliasing, but in this case
-- we can get away with doing nothing.
