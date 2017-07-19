{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Traverse a body to find memory blocks that can be allocated together.
module Futhark.Optimise.MemoryBlockMerging.Reuse.Core
  ( coreReuseFunDef
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types

import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes


data Context = Context { ctxFirstUses :: FirstUses
                       , ctxInterferences :: Interferences
                       , ctxSizes :: Sizes
                       , ctxVarToMem :: VarMemMappings MemorySrc
                       , ctxActualVars :: M.Map VName Names
                       , ctxExistentials :: Names
                       , ctxCurLoopBodyRes :: Result
                       }
  deriving (Show)

data Current = Current { curUses :: M.Map VName Names

                         -- Mostly used as in a writer monad, but not fully.
                       , curResult :: VarMemMappings MemoryLoc
                       }
  deriving (Show)

emptyCurrent :: Current
emptyCurrent = Current M.empty M.empty

newtype FindM a = FindM { unFindM :: RWS Context () Current a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadState Current)

-- Lookup the memory block statically associated with a variable.
lookupVarMem :: VName -> FindM MemorySrc
lookupVarMem var =
  -- This should always be called from a place where it is certain that 'var'
  -- refers to a statement with an array expression.
  (fromJust ("lookup memory block from " ++ pretty var) . M.lookup var)
  <$> asks ctxVarToMem

lookupActualVars :: VName -> FindM Names
lookupActualVars var =
  (fromMaybe (S.singleton var) . M.lookup var) <$> asks ctxActualVars

lookupSize :: VName -> FindM SubExp
lookupSize var =
  (fromJust ("lookup size from " ++ pretty var) . M.lookup var)
  <$> asks ctxSizes

insertUse :: VName -> VName -> FindM ()
insertUse new_mem old_mem =
  modify $ \cur -> cur { curUses = insertOrUpdate new_mem old_mem $ curUses cur }

recordMapping :: VName -> MemoryLoc -> FindM ()
recordMapping x mem =
  modify $ \cur -> cur { curResult = M.insert x mem $ curResult cur }

withLocalUses :: FindM a -> FindM a
withLocalUses m = do
  -- Keep the curResult.
  uses <- gets curUses
  res <- m
  modify $ \cur -> cur { curUses = uses }
  return res

coreReuseFunDef :: FunDef ExplicitMemory
                -> FirstUses -> Interferences -> VarMemMappings MemorySrc
                -> (ActualVariables, Names) -> FunDef ExplicitMemory
coreReuseFunDef fundef first_uses interferences var_to_mem (actual_vars, existentials) =
  let sizes = memBlockSizes fundef
      context = Context first_uses interferences sizes var_to_mem
                actual_vars existentials []
      m = unFindM $ lookInBody $ funDefBody fundef
      var_to_mem_res = curResult $ fst $ execRWS m context emptyCurrent
      fundef' = transformFromVarMemMappings var_to_mem_res fundef

      debug = fundef' `seq` do
        putStrLn $ replicate 70 '='
        putStrLn "coreReuseFunDef reuse results:"
        forM_ (M.assocs var_to_mem_res) $ \(src, dstmem) ->
          putStrLn ("Source " ++ pretty src ++ " reuses "
                    ++ pretty (memLocName dstmem) ++ "; ixfun: "
                    ++ show (memLocIxFun dstmem))
        putStrLn $ pretty fundef'
        putStrLn $ replicate 70 '='

  in withDebug debug fundef'

lookInBody :: Body ExplicitMemory
           -> FindM ()
lookInBody (Body () bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm (Let (Pattern _patctxelems patvalelems) () e) = do
  forM_ patvalelems $ \(PatElem var _ membound) -> do
    first_uses <- lookupEmptyable var <$> asks ctxFirstUses
    forM_ first_uses $ handleNewArray var membound

  let mMod = case e of
        DoLoop _ _ _ loopbody ->
          local (\ctx -> ctx { ctxCurLoopBodyRes = bodyResult loopbody })
        _ -> id
  withLocalUses $ mMod $ walkExpM walker e

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "Statement."
        print patvalelems
        print e
        putStrLn $ replicate 70 '~'

  withDebug debug $ return ()

  where walker = identityWalker { walkOnBody = lookInBody }

-- FIXME: Less conservative, please.  Would require some more state.  Also
-- support using memory blocks of larger sizes?
equalSizeSubExps :: SubExp -> SubExp -> Bool
equalSizeSubExps x y =
  let eq = (x == y)

      debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "Equal sizes?"
        print x
        print y
        putStrLn $ replicate 70 '~'

  in withDebug debug eq

handleNewArray :: VName -> ExpMem.MemBound u -> VName -> FindM ()
handleNewArray x (ExpMem.ArrayMem _ _ _ _ _xixfun) xmem = do
  interferences <- asks ctxInterferences

  -- If the statement is inside a loop body, and the loop result contains this
  -- array, abort!  That is a sign of trouble.
  --
  -- This can be described in more general terms with interference, but the
  -- existing interference analysis is not good enough, since it has to be rerun
  -- in the case of loop memory block mergings.  We *can* do that, but it might
  -- get messy.
  cur_loop_body_res <- asks ctxCurLoopBodyRes
  let loop_disabled = Var x `L.elem` cur_loop_body_res

  let notTheSame :: (VName, Names) -> FindM Bool
      notTheSame (kmem, _) = return (kmem /= xmem)

  let sizesMatch :: (VName, Names) -> FindM Bool
      sizesMatch (kmem, _) =
        equalSizeSubExps <$> lookupSize kmem <*> lookupSize xmem

  let noneInterfere :: (VName, Names) -> FindM Bool
      noneInterfere (_kmem, used_mems) =
        -- A memory block can have already been reused.  For safety's sake, we
        -- also check for interference with any previously merged blocks.  Might
        -- not be necessary?
        return $ all (\used_mem -> not $ S.member xmem
                                   $ lookupEmptyable used_mem interferences)
        $ S.toList used_mems

  let canBeUsed t = and <$> mapM ($ t) [notTheSame, sizesMatch, noneInterfere]
  found_use <- catMaybes <$> (mapM (maybeFromBoolM canBeUsed) =<< (M.assocs <$> gets curUses))

  existentials <- asks ctxExistentials
  case (loop_disabled, found_use) of
    (False, (kmem, _) : _) -> do
      -- There is a previous memory block that we can use.  Record the mapping.
      insertUse kmem xmem
      actual_vars <- lookupActualVars x
      forM_ actual_vars $ \var ->
        unless (S.member var existentials) $ do
          ixfun <- memSrcIxFun <$> lookupVarMem var
          recordMapping var $ MemoryLoc kmem ixfun
    _ ->
      -- There is no previous memory block available for use.  Record that this
      -- memory block is available.
      unless (S.member x existentials) $ insertUse xmem xmem

  let debug = found_use `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "Handle new array."
        putStrLn ("var: " ++ pretty x)
        putStrLn ("mem: " ++ pretty xmem)
        putStrLn ("loop disabled: " ++ show loop_disabled)
        putStrLn ("found use: " ++ show found_use)
        putStrLn $ replicate 70 '~'

  withDebug debug $ return ()

handleNewArray _ _ _ = return ()
