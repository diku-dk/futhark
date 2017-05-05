{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Merge memory blocks where possible.
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
import Data.Maybe (fromMaybe)

import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.AST
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Pass.ExplicitAllocations()
import Futhark.Analysis.PrimExp.Convert (primExpToExp)
import Futhark.Transform.Substitute
import Futhark.Analysis.Alias (aliasAnalysis)

import qualified Futhark.Pass.MemoryBlockMerging.DataStructs as DS
import qualified Futhark.Pass.MemoryBlockMerging.LastUse as LastUse
import qualified Futhark.Pass.MemoryBlockMerging.ArrayCoalescing as ArrayCoalescing
-- import qualified Futhark.Representation.ExplicitMemory.IndexFunction()
-- import qualified Futhark.Pass.MemoryBlockMerging.Interference as Interference


mergeMemoryBlocks :: Pass ExpMem.ExplicitMemory ExpMem.ExplicitMemory
mergeMemoryBlocks = simplePass
                    "merge memory blocks"
                    "Transform program to reuse non-interfering memory blocks"
                    transformProg


transformProg :: MonadFreshNames m
              => Prog ExpMem.ExplicitMemory
              -> m (Prog ExpMem.ExplicitMemory)
transformProg prog = do
  let lutab_prg = LastUse.lastUsePrg $ aliasAnalysis prog
      -- envtab = Interference.intrfAnPrg lutab prog
      coaltab = ArrayCoalescing.mkCoalsTab $ aliasAnalysis prog

  prog' <- intraproceduralTransformation (transformFunDef coaltab) prog
  prog'' <- intraproceduralTransformation cleanUpContextFunDef prog'

  let debug = coaltab `seq` unsafePerformIO $ do
        putStrLn $ pretty prog''

        -- Print last uses.
        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " Last use result " ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs lutab_prg) $ \(fun_name, lutab_fun) ->
          forM_ (M.assocs lutab_fun) $ \(stmt_name, lu_names) -> do
            putStrLn $ "Last uses in function " ++ pretty fun_name ++ ", statement " ++ pretty stmt_name ++ ":"
            putStrLn $ L.intercalate "   " $ map pretty $ S.toList lu_names
            putStrLn $ replicate 70 '-'

        -- Print coalescings.
        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " Coalescings result " ++ "(" ++ show (M.size coaltab) ++ ") " ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs coaltab) $ \(xmem, entry) -> do
          putStrLn $ "Source memory block: " ++ pretty xmem
          putStrLn $ "Destination memory block: " ++ pretty (DS.dstmem entry)
          -- putStrLn $ "Destination index function: " ++ show (DS.dstind entry)
          putStrLn $ "Aliased destination memory blocks: " ++ L.intercalate "   " (map pretty $ S.toList $ DS.alsmem entry)
          putStrLn "Variables currently using the source memory block:"
          putStrLn $ L.intercalate "   " $ map pretty (M.keys (DS.vartab entry))
          putStrLn $ replicate 70 '-'

  debug `seq` return prog''


-- Initial transformations from the findings in ArrayCoalescing.

transformFunDef :: MonadFreshNames m
                => DS.CoalsTab
                -> FunDef ExpMem.ExplicitMemory
                -> m (FunDef ExpMem.ExplicitMemory)
transformFunDef coaltab fundef = do
  let scope = scopeOfFParams (funDefParams fundef)
  (body', _) <-
    modifyNameSource $ \src ->
      let m1 = runBinderT m scope
          m2 = runReaderT m1 coaltab
          m3 = evalStateT m2 M.empty
          (z,newsrc) = runState m3 src
      in  (z,newsrc)

  return fundef { funDefBody = body' }
  where m = transformBody $ funDefBody fundef

type MergeM = BinderT ExpMem.ExplicitMemory (ReaderT DS.CoalsTab (StateT (M.Map VName VName) (State VNameSource)))

-- Maybe do this nicer.
instance MonadFreshNames (StateT (M.Map VName VName) (State VNameSource)) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

transformBody :: Body ExpMem.ExplicitMemory -> MergeM (Body ExpMem.ExplicitMemory)
transformBody (Body () bnds res) = do
  bnds' <- concat <$> mapM transformStm bnds
  return $ Body () bnds' res

transformStm :: Stm ExpMem.ExplicitMemory -> MergeM [Stm ExpMem.ExplicitMemory]
transformStm (Let (Pattern patCtxElems patValElems) () e) = do
  (e', newstmts1) <-
    collectStms $ mapExpM transform e

  (patValElems', newstmts2) <-
    collectStms $ mapM transformPatValElemT patValElems

  let pat' = Pattern patCtxElems patValElems'

  return (newstmts1 ++ newstmts2 ++ [Let pat' () e'])

  where transform = identityMapper { mapOnBody = const transformBody
                                   , mapOnFParam = transformFParam
                                   }

transformFParam :: FParam ExpMem.ExplicitMemory -> MergeM (FParam ExpMem.ExplicitMemory)
transformFParam (Param x
                 membound_orig@(ExpMem.ArrayMem _pt _shape u xmem _xixfun)) = do
  membound <- newMemBound x xmem u
  return $ Param x $ fromMaybe membound_orig membound
transformFParam fp = return fp

transformPatValElemT :: PatElemT (LetAttr ExpMem.ExplicitMemory)
                     -> MergeM (PatElemT (LetAttr ExpMem.ExplicitMemory))
transformPatValElemT (PatElem x bindage
                      membound_orig@(ExpMem.ArrayMem _pt _shape u xmem _xixfun)) = do
  membound <- newMemBound x xmem u
  return $ PatElem x bindage $ fromMaybe membound_orig membound
transformPatValElemT pe = return pe

checkAlreadyConverted :: VName -> MergeM (Maybe VName)
checkAlreadyConverted x = do
  table <- get
  return $ M.lookup x table

addAlreadyConverted :: VName -> VName -> MergeM ()
addAlreadyConverted from to = modify (M.insert from to)

newMemBound :: VName -> VName -> u -> MergeM (Maybe (ExpMem.MemBound u))
newMemBound x xmem u = do
  coaltab <- ask
  sequence $ do
    entry <- M.lookup xmem coaltab
    DS.Coalesced _ (DS.MemBlock pt shape _ xixfun) fv_substs <-
      M.lookup x $ DS.vartab entry
    return $ do
      substs <- forM (M.assocs fv_substs) $ \(name, pe) -> do
        -- If the variable has already been converted from a PrimExp to an Exp
        -- and been assigned a new name, just use that.  This will have happened
        -- prior to the current area, so it will be in scope.  This also avoids
        -- confusing the type checker.
        status <- checkAlreadyConverted name
        name' <- case status of
          Just name' -> return name'
          Nothing -> do
            name' <- newName name
            e <- primExpToExp (return . BasicOp . SubExp . Var) pe
            letBindNames'_ [name'] e
            addAlreadyConverted name name'
            return name'
        return (name, name')
      let xixfun' = substituteNames (M.fromList substs) xixfun
      return $ ExpMem.ArrayMem pt shape u (DS.dstmem entry) xixfun'


-- Clean up the existential contexts in the parameters.

cleanUpContextFunDef :: MonadFreshNames m
                     => FunDef ExpMem.ExplicitMemory
                     -> m (FunDef ExpMem.ExplicitMemory)
cleanUpContextFunDef fundef = do
  let m = cleanUpContextBody $ funDefBody fundef
      body' = runReader m $ scopeOfFParams (funDefParams fundef)
  return fundef { funDefBody = body' }

type CleanUpM = Reader (Scope ExpMem.ExplicitMemory)

cleanUpContextBody :: Body ExpMem.ExplicitMemory -> CleanUpM (Body ExpMem.ExplicitMemory)
cleanUpContextBody (Body () bnds res) = do
  bnds' <- cleanUpContextStms bnds
  return $ Body () bnds' res

cleanUpContextStms :: [Stm ExpMem.ExplicitMemory] -> CleanUpM [Stm ExpMem.ExplicitMemory]
cleanUpContextStms [] = return []
cleanUpContextStms (bnd : bnds) = do
  bnd' <- cleanUpContextStm bnd
  inScopeOf bnd' $ (bnd' :) <$> cleanUpContextStms bnds

cleanUpContextStm :: Stm ExpMem.ExplicitMemory -> CleanUpM (Stm ExpMem.ExplicitMemory)
cleanUpContextStm (Let pat@(Pattern patCtxElems patValElems) () e) = do
  remaining <- ExpMem.findUnusedPatternPartsInExp pat e
  let patCtxElems' = S.toList $ S.fromList patCtxElems S.\\ S.fromList remaining
  let pat' = Pattern patCtxElems' patValElems
  e' <- mapExpM cleanUpContext e
  return $ Let pat' () e'
  where cleanUpContext = identityMapper { mapOnBody = const cleanUpContextBody }
