{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
-- | Find memory block aliases.  The conceptual difference from variable aliases
-- is that if a variable x has an alias y, it means that x and y use the same
-- memory block, but if a memory block xmem has an alias ymem, it means that
-- xmem and ymem refer to the same *memory*.  This is not commutative.
module Futhark.Optimise.MemoryBlockMerging.MemoryAliases
  ( findMemAliases
  ) where

import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.Aliases
import Futhark.Representation.ExplicitMemory
       (ExplicitMemorish, ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel
import Futhark.Analysis.Alias (analyseFun)

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


newtype FindM lore a = FindM { unFindM :: RWS (VarMemMappings MemorySrc) [MemAliases] () a }
  deriving (Monad, Functor, Applicative,
            MonadReader (VarMemMappings MemorySrc),
            MonadWriter [MemAliases])

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalkAliases lore)

recordMapping :: MName -> MNames -> FindM lore ()
recordMapping mem mems = tell [M.singleton mem (S.delete mem mems)]

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

lookupMems :: Names -> FindM lore MNames
lookupMems var_aliases = do
  var_to_mem <- ask
  return $ S.fromList $ mapMaybe ((memSrcName <$>) . flip M.lookup var_to_mem)
    $ S.toList var_aliases

-- | Find all memory aliases in a function definition.
findMemAliases :: FunDef ExplicitMemory -> VarMemMappings MemorySrc -> MemAliases
findMemAliases fundef var_to_mem =
  let fundef' = analyseFun fundef
      m = unFindM $ lookInBody $ funDefBody fundef'
      mem_aliases = M.unionsWith S.union $ snd $ evalRWS m var_to_mem ()
      mem_aliases' = removeEmptyMaps $ expandWithAliases mem_aliases mem_aliases
  in mem_aliases'

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
lookInStm (Let (Pattern patctxelems patvalelems) _ e) = do
  forM_ (patctxelems ++ patvalelems) lookInPatElem

  case e of
    DoLoop mergectxparams mergevalparams _loopform body -> do
      -- There are most likely more body results than
      -- mergectxparams, but we are only interested in the first
      -- body results anyway (those that have a matching location
      -- with the mergectxparams).
      zipWithM_ lookInMergeCtxParam mergectxparams (bodyResult body)
      zipWithM_ lookInCtx patctxelems mergectxparams
      mapM_ (lookInMergeValParam body) mergevalparams
      mapM_ (lookInBodyTuples patctxelems (map snd mergectxparams) (bodyResult body))
        patvalelems
    If _ body_then body_else _ -> do
      -- Alias everything.  FIXME: This is maybe more conservative than
      -- necessary if the If works on tuples of arrays.
      let ress = mapMaybe fromVar
                 (bodyResult body_then ++ bodyResult body_else)
      var_to_mem <- ask
      let mems = map memSrcName $ mapMaybe (`M.lookup` var_to_mem) ress
      forM_ patctxelems $ \case
          (PatElem patmem (_, ExpMem.MemMem{})) ->
            recordMapping patmem $ S.fromList mems
          _ -> return ()
    _ -> return ()

  fullWalkAliasesExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody
          }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInBody . lambdaBody
          }

lookInCtx :: LoreConstraints lore =>
             PatElem (Aliases lore) -> (FParam (Aliases lore), SubExp)
          -> FindM lore ()
lookInCtx (PatElem patmem (_, ExpMem.MemMem{})) (Param parammem ExpMem.MemMem{}, _) = do
  recordMapping patmem (S.singleton parammem)
  recordMapping parammem (S.singleton patmem)
lookInCtx _ _ = return ()

lookInMergeCtxParam :: LoreConstraints lore =>
                       (FParam (Aliases lore), SubExp) -> SubExp -> FindM lore ()
lookInMergeCtxParam (Param xmem ExpMem.MemMem{}, Var param_mem) (Var body_mem_res) = do
  let aliases = S.fromList [param_mem, body_mem_res]
  recordMapping xmem aliases
lookInMergeCtxParam _ _ = return ()

lookInMergeValParam :: LoreConstraints lore =>
                       Body (Aliases lore) -> (FParam (Aliases lore), SubExp)
                    -> FindM lore ()
lookInMergeValParam body (Param _ (ExpMem.MemArray _ _ _ (ExpMem.ArrayIn mem _)), _t) = do
  -- FIXME: This is maybe more conservative than necessary in case you have more
  -- than one loop array.  Fixing this would require either changing the Aliases
  -- representation, or building something on top of it.
  aliases <- S.unions
             <$> mapM (lookupMems . unNames) (fst $ fst $ bodyAttr body)
  recordMapping mem aliases
lookInMergeValParam _ _ = return ()

lookInBodyTuples :: LoreConstraints lore =>
                    [PatElem (Aliases lore)]
                 -> [SubExp] -> [SubExp]
                 -> PatElem (Aliases lore)
                 -> FindM lore ()
-- When a parameter refers to a existential memory, we want to find
-- which return memory in the loop that the existential memory refers
-- to.
lookInBodyTuples patctxelems body_params body_results
  (PatElem _ (_, ExpMem.MemArray _ _ _ (ExpMem.ArrayIn mem _))) = do
  let zipped = zip3 patctxelems body_params body_results
  case L.find ((== mem) . patElemName . (\(x, _, _) -> x)) zipped of
    Just (_, Var param_mem, Var res_mem) ->
      recordMapping mem (S.fromList [param_mem, res_mem])
    _ -> return ()
lookInBodyTuples _ _ _ _ = return ()

lookInPatElem :: LoreConstraints lore =>
                 PatElem (Aliases lore) -> FindM lore ()
lookInPatElem (PatElem _ (names', ExpMem.MemArray _ _ _ (ExpMem.ArrayIn xmem _))) = do
  aliases <- lookupMems $ unNames names'
  recordMapping xmem aliases
lookInPatElem (PatElem xmem (names', ExpMem.MemMem {})) = do
  aliases <- lookupMems $ unNames names'
  recordMapping xmem aliases
lookInPatElem _ = return ()
