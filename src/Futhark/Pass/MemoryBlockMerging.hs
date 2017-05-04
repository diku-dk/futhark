{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
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
import Futhark.Pass.ExplicitAllocations()
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Analysis.Alias (aliasAnalysis)

import qualified Futhark.Pass.MemoryBlockMerging.DataStructs as DS
import qualified Futhark.Pass.MemoryBlockMerging.LastUse as LastUse
import qualified Futhark.Pass.MemoryBlockMerging.ArrayCoalescing as ArrayCoalescing
import Futhark.Transform.Substitute
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

  let debug = coaltab `seq` unsafePerformIO $ do
        putStrLn $ pretty prog'

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

  debug `seq` return prog'

transformFunDef :: MonadFreshNames m
                => DS.CoalsTab
                -> FunDef ExpMem.ExplicitMemory
                -> m (FunDef ExpMem.ExplicitMemory)
transformFunDef coaltab fundef = do
  let scope = scopeOfFParams (funDefParams fundef)
  (body', _) <- 
    modifyNameSource $ \src ->
      let x = runBinderT m scope
          y = runReaderT x coaltab
          (z,newsrc) = runState y src
      in  (z,newsrc)
  
  return fundef { funDefBody = body' }
  where m = transformBody $ funDefBody fundef

type MergeM = BinderT ExpMem.ExplicitMemory (ReaderT DS.CoalsTab (State VNameSource))

transformBody :: Body ExpMem.ExplicitMemory -> MergeM (Body ExpMem.ExplicitMemory)
transformBody (Body () bnds res) = do
  bnds' <- concat <$> mapM transformStm bnds
  return $ Body () bnds' res

transformStm :: Stm ExpMem.ExplicitMemory -> MergeM [Stm ExpMem.ExplicitMemory] --MergeM ()
transformStm (Let (Pattern patCtxElems patValElems) () e) = do
  (e', newstmts1) <- 
    collectStms $ do
      mapExpM transform e

  -- FIXME: Remove any context pattern elements not in use anymore.  It should
  -- not be necessary to add new ones, since we only reuse memory, not introduce
  -- new memory.  Seek inspiration in ExpMem.matchPatternToReturns.
  let patCtxElems' = patCtxElems

  --patValElems' <- mapM transformPatValElemT patValElems
  (patValElems', newstmts2) <-
    collectStms $ do
      mapM transformPatValElemT patValElems

  let pat' = Pattern patCtxElems' patValElems'
  
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

newMemBound :: VName -> VName -> u -> MergeM (Maybe (ExpMem.MemBound u))
newMemBound x xmem u = do
  coaltab <- ask
  case M.lookup xmem coaltab of
    Nothing -> return Nothing
    Just entry ->
      case M.lookup x $ DS.vartab entry of
        Nothing -> return Nothing
        Just (DS.Coalesced _ (DS.MemBlock pt shape _ xixfun) transl) -> do
           let (old_nms, pexps) = unzip $ M.toList transl
           new_nms <- mapM (\n-> newName n) old_nms --(Prim (IntType Int32))
           let xixfun'  = substituteNames (M.fromList (zip old_nms new_nms)) xixfun
           exps <- mapM (\pe -> primExpToExp primeExpCoreFun pe) pexps
           mapM_ (\(n,e) -> do
                    let pel = PatElem n BindVar (ExpMem.Scalar (IntType Int32))
                    let pat = Pattern [] [pel]
                    stmt <- mkLetM pat e
                    addStm stmt
                 ) $ zip new_nms exps
           return $ Just $ ExpMem.ArrayMem pt shape u (DS.dstmem entry) xixfun'
  where
    primeExpCoreFun nm = return $ BasicOp (SubExp (Var nm))
{-
  return $ do
    entry <- M.lookup xmem coaltab
    DS.Coalesced _ (DS.MemBlock pt shape _ xixfun) transl <-
      M.lookup x $ DS.vartab entry
    
    return $ ExpMem.ArrayMem pt shape u (DS.dstmem entry) xixfun'
-}
