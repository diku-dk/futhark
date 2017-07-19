{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Find first uses for all memory blocks.
--
-- A memory block can have more than one first use.
module Futhark.Optimise.MemoryBlockMerging.Liveness.FirstUse
  (findFirstUses
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


type FirstUsesList = [FirstUses]
-- Nicer approach: Make a new Monoid instance for a M.Map wrapper type.  We need
-- the 'M.unionsWith S.union' functionality; the default `M.unions` is too
-- destructive.

getFirstUsesMap :: FirstUsesList -> FirstUses
getFirstUsesMap = M.unionsWith S.union

data Context = Context (VarMemMappings MemorySrc) MemAliases
  deriving (Show)

newtype FindM a = FindM { unFindM :: RWS Context FirstUsesList () a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter FirstUsesList)

recordMapping :: VName -> VName -> FindM ()
recordMapping stmt_var mem = tell [M.singleton stmt_var (S.singleton mem)]

-- Overkill with the lore?
findFirstUses :: forall lore. (ExplicitMemorish lore, ArrayUtils lore)
              => VarMemMappings MemorySrc -> MemAliases -> FunDef lore -> FirstUses
findFirstUses var_to_mem mem_aliases fundef =
  let context = Context var_to_mem mem_aliases
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFunDefFParam
        lookInBody $ funDefBody fundef
      first_uses = cleanupMapping $ expandWithAliases mem_aliases
                   $ getFirstUsesMap $ snd $ evalRWS m context ()
  in first_uses

  where
    lookInFunDefFParam :: FParam lore -> FindM ()
    lookInFunDefFParam (Param x (ExpMem.ArrayMem _ _ _ xmem _)) =
      recordMapping x xmem
    lookInFunDefFParam _ = return ()

    lookInBody :: Body lore -> FindM ()
    lookInBody (Body _ bnds _res) =
      mapM_ lookInStm bnds

    lookInStm :: Stm lore -> FindM ()
    lookInStm (Let (Pattern patctxelems patvalelems) _ e) = do
      when (createsNewArray e) $ do
        let e_free_vars = freeInExp e
        e_mems <- S.unions <$> mapM varMems (S.toList e_free_vars)
        forM_ patvalelems $ \(PatElem x bindage membound) ->
          case (bindage, membound) of
            (BindVar, ExpMem.ArrayMem _ _ _ xmem _) -> do
              x_mems <- varMems xmem

              -- For the first use to be a proper first use, it must write to
              -- the memory, but not read from it.  We need to check this to
              -- support multiple liveness intervals.  If we don't check this,
              -- the last use analysis and the interference analysis might end
              -- up wrong.
              when (S.null $ S.intersection x_mems e_mems)
                -- We only record the mapping between the statement and the
                -- memory block, not any of its aliased memory blocks.  They
                -- would not be aliased unless they are themselves created at
                -- some point, so they will get their own FirstUses.  Putting
                -- them into first use here would probably also be too
                -- conservative.
                $ recordMapping x xmem
            _ -> return ()

      -- Find first uses of existential memory blocks.  Fairly conservative.
      -- Covers the case where a loop uses multiple arrays by saying every
      -- existential memory block overlaps with every result memory block.  Fine
      -- for now.
      forM_ patctxelems
          $ \p -> forM_ patvalelems
                  $ \el -> lookInPatCtxElem (patElemName el) p
      case e of
        DoLoop mergectxparams _mergevalparams _loopform _body ->
          forM_ mergectxparams
          $ \p -> forM_ patvalelems
                  $ \el -> lookInMergeCtxParam (patElemName el) p
        _ -> return ()

      walkExpM walker e
      where walker = identityWalker { walkOnBody = lookInBody }

    -- Find the memory blocks used or aliased by a variable.
    varMems :: VName -> FindM Names
    varMems var =
      -- Context var_to_mem mem_aliases <- ask
      return $ fromMaybe S.empty $ do
        mem <- memSrcName <$> M.lookup var var_to_mem
        return $ S.union (S.singleton mem) $ lookupEmptyable mem mem_aliases

    lookInPatCtxElem :: VName -> PatElem lore -> FindM ()
    lookInPatCtxElem x (PatElem xmem _bindage ExpMem.MemMem{}) =
      recordMapping x xmem
    lookInPatCtxElem _ _ = return ()

    lookInMergeCtxParam :: VName -> (FParam lore, SubExp) -> FindM ()
    lookInMergeCtxParam x (Param xmem ExpMem.MemMem{}, _) =
      recordMapping x xmem
    lookInMergeCtxParam _ _ = return ()
