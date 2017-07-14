{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Find memory block interferences.
module Futhark.Optimise.MemoryBlockMerging.Liveness.Interference where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


data Context = Context FirstUses LastUses
  deriving (Show)

type InterferencesList = [Interferences]

getInterferencesMap :: InterferencesList -> Interferences
getInterferencesMap = M.unionsWith S.union

type CurrentlyAlive = Names

newtype FindM a = FindM { unFindM :: RWS Context InterferencesList CurrentlyAlive a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter InterferencesList,
            MonadState CurrentlyAlive)

awaken :: VName -> FindM ()
awaken mem =
  modify $ S.insert mem

kill :: VName -> FindM ()
kill mem =
  modify $ S.delete mem

recordCurrentInterferences :: FindM ()
recordCurrentInterferences = do
  current <- get
  -- Interferences are commutative.  Reflect that in the resulting data.
  forM_ (S.toList current) $ \mem ->
    tell [M.singleton mem $ S.delete mem current]

-- Overkill with the lore?
findInterferences :: forall lore. (ExplicitMemorish lore, ArrayUtils lore)
                  => MemAliases -> FirstUses -> LastUses -> FunDef lore -> Interferences
findInterferences mem_aliases first_uses last_uses fundef =
  let context = Context first_uses last_uses
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFunDefFParam
        lookInBody $ funDefBody fundef
      interferences = cleanupMapping $ makeCommutativeMap $ expandWithAliases mem_aliases $ getInterferencesMap $ snd $ evalRWS m context S.empty
  in interferences

  where
    lookInFunDefFParam :: FParam lore -> FindM ()
    lookInFunDefFParam (Param var _) =
      handleDeclaration var False

    lookInBody :: Body lore -> FindM ()
    lookInBody (Body _ bnds _res) =
      mapM_ lookInStm bnds

    lookInStm :: Stm lore -> FindM ()
    lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
      mapM_ (lookInPatValElem e) patvalelems

      walkExpM walker e
      where walker = identityWalker { walkOnBody = lookInBody }

    lookInPatValElem :: Exp lore -> PatElem lore -> FindM ()
    lookInPatValElem e (PatElem var _ _) =
      handleDeclaration var $ canShare e

    -- Can the destination and source memory blocks share the same memory,
    -- i.e. is the reading and writing certain to be structured in a way that
    -- allows in-place use?
    canShare :: Exp lore -> Bool
    canShare e = case e of
      BasicOp ExpMem.Copy{} -> True
      BasicOp Concat{} -> True -- Watch out for this?
      -- More?  This should be enough to make the coalescing optimisation work,
      -- in any case.
      --
      -- There is no need to put expressions here that do not take arrays as
      -- input, such as iota.
      _ -> False

    -- For every declaration, see if any liveness intervals can be started or
    -- ended.
    handleDeclaration :: VName -> Bool -> FindM ()
    handleDeclaration var can_share = do
      Context first_uses_all last_uses_all <- ask
      let first_uses = lookupEmptyable var first_uses_all
          last_uses = lookupEmptyable var last_uses_all

      mapM_ awaken $ S.toList first_uses
      unless can_share
        -- Be conservative.  If a memory block has its last use here, and another
        -- memory block has its first use, they still interfere.  Only kill the
        -- last uses after the interferences have been recorded.
        recordCurrentInterferences
      mapM_ kill $ S.toList last_uses
      when can_share
        -- Be un-conservative.  If a memory block has its last use here, don't
        -- let it interfere with any new firstly used memory blocks.
        recordCurrentInterferences

-- We end up also recording interferences with existential memory blocks.  This
-- is not a problem, since the optimisations using the interference graph will
-- never try to allocate into existential memory anyway.
