{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Find last uses for all memory blocks.
--
-- A memory block can have more than one last use.
module Futhark.Optimise.MemoryBlockMerging.Liveness.LastUse
  ( findLastUses
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


type LastUsesList = [LastUses]

getLastUsesMap :: LastUsesList -> LastUses
getLastUsesMap = M.unionsWith S.union

-- Mapping from a memory block to its currently assumed last use statement
-- variable.
type OptimisticLastUses = M.Map VName VName

data Context = Context (VarMemMappings MemorySrc) MemAliases FirstUses
  deriving (Show)

newtype FindM a = FindM { unFindM :: RWS Context LastUsesList OptimisticLastUses a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter LastUsesList,
            MonadState OptimisticLastUses)

recordMapping :: VName -> VName -> FindM ()
recordMapping stmt_var mem = tell [M.singleton stmt_var (S.singleton mem)]

setOptimistic :: VName -> VName -> FindM ()
setOptimistic mem x_lu = modify (M.insert mem x_lu)

commitOptimistic :: VName -> FindM ()
commitOptimistic mem = do
  res <- M.lookup mem <$> get
  case res of
    Just x_lu -> recordMapping x_lu mem
    Nothing -> return ()

-- Overkill with the lore?
findLastUses :: forall lore. (ExplicitMemorish lore, ArrayUtils lore)
             => VarMemMappings MemorySrc -> MemAliases -> FirstUses -> FunDef lore -> LastUses
findLastUses var_to_mem mem_aliases first_uses fundef =
  let context = Context var_to_mem mem_aliases first_uses
      m = unFindM $ do
        -- We do not need to look in the function paramaters, as they should not
        -- contain last uses -- in that case they would have been simplified away.
        lookInBody $ funDefBody fundef
        optimistics <- get
        forM_ (M.assocs optimistics) $ \(mem, x_lu) ->
          recordMapping x_lu mem

      last_uses = cleanupMapping $ expandWithAliases mem_aliases $ getLastUsesMap
                  $ snd $ evalRWS m context M.empty
  in last_uses

  where
    lookInBody :: Body lore -> FindM ()
    lookInBody (Body _ bnds _res) =
      mapM_ lookInStm bnds

    lookInStm :: Stm lore -> FindM ()
    lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
      forM_ patvalelems $ lookInPatValElem e

      walkExpM walker e
      where walker = identityWalker { walkOnBody = lookInBody }

    -- Find the memory blocks used or aliased by a variable.
    varMems :: VName -> FindM Names
    varMems var =
      -- Context var_to_mem mem_aliases _ <- ask
      return $ fromMaybe S.empty $ do
        mem <- memSrcName <$> M.lookup var var_to_mem
        return $ S.union (S.singleton mem) $ lookupEmptyable mem mem_aliases

    lookInPatValElem :: Exp lore -> PatElem lore -> FindM ()
    lookInPatValElem e (PatElem x _bindage membound) = do
      case membound of
        ExpMem.ArrayMem _ _ _ xmem _ -> do
          -- Context _ _ first_uses <- ask
          let first_uses_x = lookupEmptyable x first_uses
          -- When this is a new first use of a memory block, commit the previous
          -- optimistic last use of it, so that it can be considered unused in
          -- the statements inbetween.  FIXME: Aliasing problems?  Edge cases?
          when (S.member xmem first_uses_x) $ commitOptimistic xmem
        _ -> return ()

      let e_free_vars = freeInExp e
      e_mems <- S.unions <$> mapM varMems (S.toList e_free_vars)

      -- Set all memory blocks being used as optimistic last uses.
      forM_ (S.toList e_mems) $ \mem -> setOptimistic mem x

      let debug = do
            putStrLn $ replicate 70 '~'
            putStrLn "LastUse lookInPatElem:"
            putStrLn ("free vars in expression: " ++ prettySet e_free_vars)
            putStrLn ("memblocks in or aliased: " ++ prettySet e_mems)
            putStrLn $ replicate 70 '~'
      debug `seq` return ()
