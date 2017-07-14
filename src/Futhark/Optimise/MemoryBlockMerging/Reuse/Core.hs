{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Traverse a body to find memory blocks that can be allocated together.
module Futhark.Optimise.MemoryBlockMerging.Reuse.Core
  ( coreReuseFunDef
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
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
                       }
  deriving (Show)

data Current = Current { curUses :: M.Map VName Names

                         -- FIXME: This seems like duplicating effort from coalescing.
                       , curActualVars :: M.Map VName Names

                         -- Mostly used as in a writer monad, but not fully.
                       , curResult :: VarMemMappings MemoryLoc
                       }
  deriving (Show)

emptyCurrent :: Current
emptyCurrent = Current M.empty M.empty M.empty

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

lookupVarMem :: VName -> FindM (Maybe MemorySrc)
lookupVarMem var = do
  var_to_mem <- asks ctxVarToMem
  return $ M.lookup var var_to_mem

coreReuseFunDef :: FunDef ExplicitMemory
                -> FirstUses -> Interferences -> VarMemMappings MemorySrc
                -> FunDef ExplicitMemory
coreReuseFunDef fundef first_uses interferences var_to_mem =
  let sizes = memBlockSizes fundef
      context = Context first_uses interferences sizes var_to_mem
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
lookInStm (Let (Pattern patctxelems patvalelems) () e) = do
  withLocalUses $ walkExpM walker e



-- FIXME: This is from coalescing.  We need to put this logic in its own module.
  forM_ patvalelems $ \(PatElem var bindage _) ->
    case bindage of
      BindInPlace _ orig _ -> do
        -- Record that when coalescing an in-place update statement, also look
        -- at the original array.
        let actuals = [var, orig]
        modify $ \c -> c { curActualVars = M.insert var (S.fromList actuals)
                                           $ curActualVars c }
      _ -> return ()


  -- Special handling of loops, ifs, etc.
  case e of
    DoLoop _mergectxparams mergevalparams _loopform body ->
      forM_ patvalelems $ \(PatElem var _ membound) ->
        case membound of
          ExpMem.ArrayMem _ _ _ mem _ -> do
            -- If mem is existential, we need to find the return memory that it
            -- refers to.  We cannot just look at its memory aliases, since it
            -- likely aliases both the initial memory and the final memory.
            let zipped = zip patctxelems (bodyResult body)
            (var_actual, mem_search) <- case L.find ((== mem) . patElemName . fst) zipped of
              Just (_, Var res_mem) ->
                return (Nothing, res_mem)
              _ -> return (Just var, mem)
            let body_vars0 = mapMaybe (fromVar . snd) mergevalparams
                body_vars1 = map (paramName . fst) mergevalparams
                body_vars2 = concatMap (map patElemName
                                        . patternValueElements . stmPattern)
                             $ bodyStms body
                body_vars = body_vars0 ++ body_vars1 ++ body_vars2
            -- Find the ones using the same memory as the result of the loop
            -- expression.
            body_vars' <- filterM (\v -> do
                                      m <- lookupVarMem v
                                      return $ Just mem_search == (memSrcName <$> m)
                                  ) body_vars
            -- Not only the result variable needs to change its memory block in case
            -- of a future coalescing with it; also the variables extracted above.
            -- FIXME: This is probably an okay way to do it?  What if the memory is
            -- used, but with a different index function?  Can that happen?
            let actuals = maybeToList var_actual ++ body_vars'
            modify $ \c -> c { curActualVars = M.insert var (S.fromList actuals)
                               $ curActualVars c }

            -- If you extend this loop handling, make sure not to target existential
            -- memory blocks.  We want those to stay.
          _ -> return ()

    If _se body_then body_else _types ->
      -- We don't want to coalesce the existiential memory block of the if.
      -- However, if a branch result has a memory block that is firstly used
      -- inside the branch, it is okay to coalesce that in a future statement.
      forM_ (zip3 patvalelems (bodyResult body_then) (bodyResult body_else))
        $ \(PatElem var _ membound, res_then, res_else) ->
        case membound of
          ExpMem.ArrayMem{} -> do
            var_to_mem <- asks ctxVarToMem
            first_uses_all <- asks ctxFirstUses
            let ifBlocks body se = do
                  res <- fromVar se
                  res_mem <- memSrcName <$> M.lookup res var_to_mem
                  let body_vars = concatMap (map patElemName . patternValueElements . stmPattern)
                                  $ bodyStms body
                      body_first_uses = S.unions $ map (`lookupEmptyable` first_uses_all) body_vars
                  Just $ return (S.member res_mem body_first_uses, res)

            case (ifBlocks body_then res_then,
                  ifBlocks body_else res_else) of
              (Just th, Just el) -> do -- prettify this stuff
                (alloc_inside_then, var_then) <- th
                (alloc_inside_else, var_else) <- el
                when (alloc_inside_then || alloc_inside_else) $ do
                  let actuals = [var, var_then, var_else]
                  modify $ \c -> c { curActualVars = M.insert var (S.fromList actuals)
                                     $ curActualVars c }
              _ -> return ()

            let debug = do
                  putStrLn $ replicate 70 '~'
                  putStrLn "coreCoalesceFunDef lookInStm If special handling:"
                  putStrLn $ replicate 70 '~'
            withDebug debug $ return ()
          _ -> return ()

    BasicOp (Reshape _ _ src0) -> do
      let var = patElemName $ head patvalelems -- probably ok
      -- Also refer to the array that it's reshaping.

      -- This might be redundant as it is also done later on.
      src0_actuals <- lookupEmptyable src0 <$> gets curActualVars
      let actuals = S.union (S.fromList [var, src0]) src0_actuals

      modify $ \c -> c { curActualVars = M.insert var actuals
                                         $ curActualVars c }

    _ -> return ()
-- FIXME end



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

handleNewArray :: VName -> ExpMem.MemBound u -> VName -> FindM ()
handleNewArray x (ExpMem.ArrayMem _ _ _ _ xixfun) xmem = do
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
      actual_vars <- (fromMaybe (S.singleton x) . M.lookup x) <$> gets curActualVars
      forM_ (S.toList actual_vars) $ \var ->
        recordMapping var $ MemoryLoc kmem xixfun -- same ixfun works for now, maybe FIXME
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
