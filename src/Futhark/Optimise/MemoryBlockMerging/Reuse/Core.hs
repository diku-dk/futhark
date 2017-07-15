{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Traverse a body to find memory blocks that can be allocated together.
module Futhark.Optimise.MemoryBlockMerging.Reuse.Core
  ( coreReuseFunDef
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types

import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes
import Futhark.Optimise.MemoryBlockMerging.ActualVariables (findActualVariables)


data Context = Context { ctxFirstUses :: FirstUses
                       , ctxInterferences :: Interferences
                       , ctxSizes :: Sizes
                       , ctxVarToMem :: VarMemMappings MemorySrc
                       , ctxActualVars :: M.Map VName Names
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

insertUse :: VName -> VName -> FindM ()
insertUse new_mem old_mem =
  modify $ \cur -> cur { curUses = M.alter (insertOrNew old_mem) new_mem $ curUses cur }

insertOrNew :: Ord a => a -> Maybe (S.Set a) -> Maybe (S.Set a)
insertOrNew x m = Just $ case m of
  Just s -> S.insert x s
  Nothing -> S.singleton x

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
                -> FunDef ExplicitMemory
coreReuseFunDef fundef first_uses interferences var_to_mem =
  let sizes = memBlockSizes fundef
      (actual_vars, _existentials) = findActualVariables first_uses var_to_mem fundef
      context = Context first_uses interferences sizes var_to_mem actual_vars
      m = unFindM $ lookInBody $ funDefBody fundef
      var_to_mem_res = curResult $ fst $ execRWS m context emptyCurrent
      fundef' = transformFromVarMemMappings var_to_mem_res fundef

      debug = var_to_mem_res `seq` do
        putStrLn $ replicate 70 '='
        putStrLn "coreReuseFunDef reuse results:"
        forM_ (M.assocs var_to_mem_res) $ \(src, dstmem) ->
          putStrLn ("Source " ++ pretty src ++ " reuses " ++ pretty (memLocName dstmem) ++ "; ixfun: " ++ show (memLocIxFun dstmem))
        -- print fundef
        -- putStrLn $ pretty fundef'
        putStrLn $ pretty fundef'
        putStrLn $ replicate 70 '='

  in withDebug debug fundef'

lookInBody :: Body ExplicitMemory
           -> FindM ()
lookInBody (Body () bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm (Let (Pattern _patctxelems patvalelems) () e) = do
  withLocalUses $ walkExpM walker e

  first_uses_all <- asks ctxFirstUses
  forM_ patvalelems $ \(PatElem var _ membound) -> do
    let first_uses = lookupEmptyable var first_uses_all
    mapM_ (handleNewArray var membound) first_uses

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "Statement."
        print patvalelems
        print e
        putStrLn $ replicate 70 '~'

  withDebug debug $ return ()

  where walker = identityWalker { walkOnBody = lookInBody }

getActualVars :: VName -> FindM [VName]
getActualVars src0 = do
  res <- getActualVars' S.empty src0

  let debug = res `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "getActualVars:"
        putStrLn ("src: " ++ pretty src0)
        putStrLn ("result: " ++ prettySet (S.fromList res))
        putStrLn $ replicate 70 '~'

  withDebug debug $ return res
  where getActualVars' visited src
          | S.member src visited = return []
          | otherwise = do
              actual_vars <- asks ctxActualVars
              case M.lookup src actual_vars of
                Nothing -> return [src]
                Just a_srcs -> do
                  more <- mapM (getActualVars' (S.insert src visited))
                          $ S.toList a_srcs
                  return (S.toList a_srcs ++ concat more)

handleNewArray :: VName -> ExpMem.MemBound u -> VName -> FindM ()
handleNewArray x (ExpMem.ArrayMem _ _ _ _ _xixfun) xmem = do
  uses <- gets curUses
  interferences <- asks ctxInterferences
  sizes <- asks ctxSizes

  let notTheSame :: (VName, Names) -> Bool
      notTheSame (kmem, _) = kmem /= xmem

  let sizesMatch :: (VName, Names) -> Bool
      sizesMatch (kmem, _) = equalSizeSubExps (shouldWork ("find size of " ++ pretty kmem) $ M.lookup kmem sizes) (shouldWork ("find size of " ++ pretty xmem) $ M.lookup xmem sizes) -- (sizes M.! kmem) (sizes M.! xmem)

  let noneInterfere :: (VName, Names) -> Bool
      noneInterfere (_kmem, used_mems) =
        -- A memory block can have already been reused.  For safety's sake, we
        -- also check for interference with any previously merged blocks.  Might
        -- not be necessary?
        all (\used_mem -> not $ S.member xmem $ lookupEmptyable used_mem interferences)
        $ S.toList used_mems

  let canBeUsed t = notTheSame t && sizesMatch t && noneInterfere t
      found_use = L.find canBeUsed $ M.assocs uses

  case found_use of
    Just (kmem, _) -> do
      insertUse kmem xmem

      -- hack
      actual_vars <- getActualVars x
      -- specially_handled <- M.lookup x <$> asks ctxActualVars
      -- actual_vars <- case specially_handled of
      --   Just _ -> getActualVars x
      --   Nothing -> return [x]
      forM_ actual_vars $ \var -> do
        varmem <- (shouldWork "it's an array" . M.lookup var) <$> asks ctxVarToMem
        recordMapping var $ MemoryLoc kmem (memSrcIxFun varmem)
    Nothing ->
      insertUse xmem xmem

  let debug = uses `seq` sizes `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "Handle new array."
        putStrLn ("var: " ++ pretty x)
        putStrLn ("mem: " ++ pretty xmem)
        putStrLn ("uses: " ++ show uses)
        putStrLn ("sizes: " ++ show sizes)
        putStrLn ("can be used: " ++ show found_use)
        putStrLn $ replicate 70 '~'

  withDebug debug $ return ()

handleNewArray _ _ _ = return ()

-- FIXME: Less conservative, please.  Would require some more state.
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
