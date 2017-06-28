{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Merge memory blocks where possible.
--
-- Enable by setting the environment variable MEMORY_BLOCK_MERGING=1.
module Futhark.Pass.MemoryBlockMerging
  ( mergeMemoryBlocks
  ) where

import System.IO.Unsafe (unsafePerformIO) -- Just for debugging!

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isJust)

import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Scope()
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Analysis.Alias (analyseFun)

import qualified Futhark.Pass.MemoryBlockMerging.AllocHoisting as AH
import qualified Futhark.Pass.MemoryBlockMerging.DataStructs as DS
import qualified Futhark.Pass.MemoryBlockMerging.ArrayCoalescing as ArrayCoalescing

import Futhark.Util (unixEnvironment)
usesDebugging :: Bool
usesDebugging = isJust $ lookup "FUTHARK_DEBUG" unixEnvironment

mergeMemoryBlocks :: Pass ExpMem.ExplicitMemory ExpMem.ExplicitMemory
mergeMemoryBlocks = simplePass
                    "merge memory blocks"
                    "Transform program to coalesce memory blocks"
                    transformProg


transformProg :: MonadFreshNames m
              => Prog ExpMem.ExplicitMemory
              -> m (Prog ExpMem.ExplicitMemory)
transformProg prog = do
  prog1 <- intraproceduralTransformation AH.hoistAllocsFunDef prog
  prog2 <- intraproceduralTransformation transformFunDef prog1

  let debug = unsafePerformIO $ when usesDebugging $ putStrLn $ pretty prog2

  debug `seq` return prog2


-- Initial transformations from the findings in ArrayCoalescing.

transformFunDef :: MonadFreshNames m
                => FunDef ExpMem.ExplicitMemory
                -> m (FunDef ExpMem.ExplicitMemory)
transformFunDef fundef = do
  let coaltab = ArrayCoalescing.mkCoalsTabFun $ analyseFun fundef

  let debug = unsafePerformIO $ when usesDebugging $ do
        -- Print coalescings.
        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " Coalescings result in " ++ pretty (funDefName fundef) ++ " (" ++ show (M.size coaltab) ++ ") " ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs coaltab) $ \(xmem, entry) -> do
          putStrLn $ "Source memory block: " ++ pretty xmem
          putStrLn $ "Destination memory block: " ++ pretty (DS.dstmem entry)
          putStrLn $ "Aliased destination memory blocks: " ++ L.intercalate "   " (map pretty $ S.toList $ DS.alsmem entry)
          putStrLn "Variables currently using the source memory block:"
          putStrLn $ L.intercalate "   " $ map pretty (M.keys (DS.vartab entry))
          putStrLn $ replicate 70 '-'

  let MergeM m0 = m
      m1 = runReaderT m0 coaltab
      body' = evalState m1 S.empty

  debug `seq` return fundef { funDefBody = body' }
  where m = transformBody $ funDefBody fundef

type Existentials = Names

newtype MergeM a = MergeM (ReaderT DS.CoalsTab (State Existentials) a)
  deriving (Monad, Functor, Applicative,
            MonadReader DS.CoalsTab,
            MonadState Existentials)

transformBody :: Body ExpMem.ExplicitMemory
              -> MergeM (Body ExpMem.ExplicitMemory)
transformBody (Body () bnds res) = do
  bnds' <- mapM transformStm bnds
  res' <- mapM transformResultSubExp res
  return $ Body () bnds' res'

transformStm :: Stm ExpMem.ExplicitMemory
             -> MergeM (Stm ExpMem.ExplicitMemory)
transformStm (Let (Pattern patctxelems patvalelems) () e) = do
  -- General approach: If a memory block has a coalesced mapping, replace it
  -- with that.  However, if it is an existential memory block (e.g. from a loop
  -- context or an if context), keep the existential memory block, and use the
  -- shape and index function of the coalesced memory block.  In that case, also
  -- make sure that the existential memory block is set to refer to its
  -- coalesced counterpart.

  let existentials0 = S.fromList $ map (identName . patElemIdent) patctxelems
      existentials = S.union existentials0 $ case e of
        DoLoop mergectxparams _ _ _ ->
          -- A loop has an extra context.
          S.fromList $ map (identName . paramIdent . fst) mergectxparams
        _ -> S.empty
  modify $ S.union existentials -- Keep track of all existentials.

  e' <- case e of
    DoLoop mergectxparams mergevalparams loopform body -> do
      -- More special loop handling because of its extra pattern-like info.
      mergectxparams' <- mapM transformCtxMergeParam mergectxparams
      mergevalparams' <- mapM transformValMergeParam mergevalparams
      return $ DoLoop mergectxparams' mergevalparams' loopform body
    _ -> return e

  patvalelems' <- mapM transformPatValElemT patvalelems
  let pat' = Pattern patctxelems patvalelems'
  e'' <- mapExpM transform e'
  return $ Let pat' () e''

  where transform = identityMapper { mapOnBody = const transformBody }

-- | Update memory block names in the result values of a body.
transformResultSubExp :: SubExp -> MergeM SubExp
transformResultSubExp s@(Var xmem) = do
  existentals <- get
  if S.member xmem existentals
    then return s
    else do
    coaltab <- ask
    return $ fromMaybe s $ do
      entry <- M.lookup xmem coaltab
      return $ Var $ DS.dstmem entry
transformResultSubExp s = return s

-- | Update the actual memory block referred to by a context memory block in a
-- loop.
transformCtxMergeParam :: (FParam ExpMem.ExplicitMemory, SubExp)
                       -> MergeM (FParam ExpMem.ExplicitMemory, SubExp)
transformCtxMergeParam t@(param, Var xmem) = do
  coaltab <- ask
  return $ fromMaybe t $ do
    entry <- M.lookup xmem coaltab
    return (param, Var $ DS.dstmem entry)
transformCtxMergeParam t = return t

transformValMergeParam :: (FParam ExpMem.ExplicitMemory, SubExp)
                       -> MergeM (FParam ExpMem.ExplicitMemory, SubExp)
transformValMergeParam (Param x membound, se) = do
  membound' <- transformValMem x membound
  return (Param x membound', se)

transformPatValElemT :: PatElemT (LetAttr ExpMem.ExplicitMemory)
                     -> MergeM (PatElemT (LetAttr ExpMem.ExplicitMemory))
transformPatValElemT (PatElem x bindage membound) = do
  membound' <- transformValMem x membound
  return $ PatElem x bindage membound'

transformValMem :: VName -> ExpMem.MemBound u -> MergeM (ExpMem.MemBound u)
transformValMem x membound@(ExpMem.ArrayMem pt shape u xmem _xixfun) = do
  existentials <- get
  meminfo <- findCoalescedMemBound x xmem
  return $ case (S.member xmem existentials, meminfo) of
    (True, Just (_xmem', xixfun')) ->
      -- Keep the existential memory while using the index function from the
      -- memory block to which it is coalesced.  The simplifier will remove the
      -- existential memory if possible -- and even so, it doesn't constitute an
      -- allocation, just another reference in the generated code.
      ExpMem.ArrayMem pt shape u xmem xixfun'
    (False, Just (xmem', xixfun')) ->
      -- Not existential, so replace both memory block and index function.
      ExpMem.ArrayMem pt shape u xmem' xixfun'
    _ -> membound
transformValMem _ m = return m

findCoalescedMemBound :: VName -> VName -> MergeM (Maybe (VName, ExpMem.IxFun))
findCoalescedMemBound x xmem = do
  coaltab <- ask
  return $ do
    entry <- M.lookup xmem coaltab
    DS.Coalesced _ (DS.MemBlock _ _ _ xixfun) _ <- M.lookup x $ DS.vartab entry
    return (DS.dstmem entry, xixfun)
