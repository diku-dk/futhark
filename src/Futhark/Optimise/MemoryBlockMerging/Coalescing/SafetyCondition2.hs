{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find safety condition 2 for all statements.
module Futhark.Optimise.MemoryBlockMerging.Coalescing.SafetyCondition2
  ( findSafetyCondition2FunDef
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem


type CurrentAllocatedBlocks = Names
type AllocatedBlocksBeforeCreation = M.Map VName Names

newtype FindM a = FindM { unFindM :: RWS ()
                          AllocatedBlocksBeforeCreation CurrentAllocatedBlocks a }
  deriving (Monad, Functor, Applicative,
            MonadWriter AllocatedBlocksBeforeCreation,
            MonadState CurrentAllocatedBlocks)

findSafetyCondition2FunDef :: FunDef ExplicitMemory
                           -> AllocatedBlocksBeforeCreation
findSafetyCondition2FunDef fundef =
  let m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
      res = snd $ evalRWS m () S.empty
  in res

lookInFParam :: FParam ExplicitMemory -> FindM ()
lookInFParam (Param _ membound) =
  -- Unique array function parameters also count as "allocations" in which
  -- memory can be coalesced.
  case membound of
    ExpMem.ArrayMem _ _ Unique mem _ ->
      modify $ S.insert mem
    _ -> return ()

lookInBody :: Body ExplicitMemory -> FindM ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm (Let (Pattern patctxelems patvalelems) _ e) = do
  let new_decls0 = map patElemName (patctxelems ++ patvalelems)
      new_decls1 = case e of
        DoLoop _mergectxparams mergevalparams _loopform _body ->
          -- Technically not a declaration for the current expression, but very
          -- close, and hopefully okay to consider it as one.
          map (paramName . fst) mergevalparams
        _ -> []
      new_decls = new_decls0 ++ new_decls1

  cur_allocated_blocks <- get
  forM_ new_decls $ \x ->
    tell $ M.singleton x cur_allocated_blocks

  case (patvalelems, e) of
    ([PatElem mem _ _], Op ExpMem.Alloc{}) ->
      modify $ S.insert mem
    _ -> return ()

  -- RECURSIVE BODY WALK.
  walkExpM walker e
  where walker = identityWalker
          { walkOnBody = lookInBody
          , walkOnFParam = lookInFParam
          }
