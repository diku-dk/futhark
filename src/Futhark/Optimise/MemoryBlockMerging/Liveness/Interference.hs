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
-- import qualified Futhark.Representation.ExplicitMemory as ExpMem

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
    lookInFunDefFParam (Param var _) = do
      let first_uses_var = lookupEmptyable var first_uses
      mapM_ awaken $ S.toList first_uses_var
      recordCurrentInterferences

    lookInBody :: Body lore -> FindM ()
    lookInBody (Body _ bnds _res) =
      mapM_ lookInStm bnds

    lookInStm :: Stm lore -> FindM ()
    lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
      -- Context first_uses last_uses <- ask
      let can_share = canShare e

      forM_ patvalelems $ \(PatElem var _ _) -> do
        let first_uses_var = lookupEmptyable var first_uses
        mapM_ awaken $ S.toList first_uses_var

        cur <- get
        doDebug $ do
          putStrLn $ replicate 70 '~'
          putStrLn ("Interference: awakening " ++ pretty var)
          putStrLn ("current live: " ++ prettySet cur)
          putStrLn $ replicate 70 '~'
      unless can_share
        -- Be conservative.  If a memory block has its last use here, and another
        -- memory block has its first use, they still interfere.  Only kill the
        -- last uses after the interferences have been recorded.
        recordCurrentInterferences

      walkExpM walker e

      forM_ patvalelems $ \(PatElem var _ _) -> do
        let last_uses_var = lookupEmptyable var last_uses
        mapM_ kill $ S.toList last_uses_var

        cur <- get
        doDebug $ do
          putStrLn $ replicate 70 '~'
          putStrLn ("Interference: killing " ++ pretty var)
          putStrLn ("current live: " ++ prettySet cur)
          putStrLn $ replicate 70 '~'
      when can_share
        -- Be un-conservative.  If a memory block has its last use here, don't
        -- let it interfere with any new firstly used memory blocks.
        recordCurrentInterferences

      where walker = identityWalker { walkOnBody = lookInBody }

    -- Can the destination and source memory blocks share the same memory,
    -- i.e. is the reading and writing certain to be structured in a way that
    -- allows in-place use?
    --
    -- Currently disabled, as e.g. it is only true copy if its source has the
    -- same index function as the destination (which is not the case for a
    -- rearrange, for example).
    canShare :: Exp lore -> Bool
    canShare _e = False --case e of
      -- BasicOp ExpMem.Copy{} -> True
      -- BasicOp Concat{} -> True -- Watch out for this?

      -- More?  This should be enough to make the coalescing optimisation work,
      -- in any case.
      --
      -- There is no need to put expressions here that do not take arrays as
      -- input, such as iota.
--      _ -> False

-- We end up also recording interferences with existential memory blocks.  This
-- is not a problem, since the optimisations using the interference graph will
-- never try to allocate into existential memory anyway.
