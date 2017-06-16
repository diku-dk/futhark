{-# LANGUAGE TypeFamilies #-}
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
import Data.Maybe (fromMaybe, isJust)

import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.AST
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Analysis.Alias (analyseFun)

import qualified Futhark.Pass.MemoryBlockMerging.AllocHoisting as AH
import qualified Futhark.Pass.MemoryBlockMerging.DataStructs as DS
import qualified Futhark.Pass.MemoryBlockMerging.LastUse as LastUse
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
  prog3 <- intraproceduralTransformation cleanUpContextFunDef prog2

  let debug = unsafePerformIO $ when usesDebugging $ putStrLn $ pretty prog3

  debug `seq` return prog3


-- Initial transformations from the findings in ArrayCoalescing.

transformFunDef :: MonadFreshNames m
                => FunDef ExpMem.ExplicitMemory
                -> m (FunDef ExpMem.ExplicitMemory)
transformFunDef fundef = do
  let coaltab = ArrayCoalescing.mkCoalsTabFun $ analyseFun fundef

  let debug = unsafePerformIO $ when usesDebugging $ do
        -- Print last uses.
        let lutab_prg = M.fromList [LastUse.lastUseFun $ analyseFun fundef]
        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " Last use result in "  ++ pretty (funDefName fundef) ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs lutab_prg) $ \(fun_name, lutab_fun) ->
          forM_ (M.assocs lutab_fun) $ \(stmt_name, lu_names) -> do
            putStrLn $ "Last uses in function " ++ pretty fun_name ++ ", statement " ++ pretty stmt_name ++ ":"
            putStrLn $ L.intercalate "   " $ map pretty $ S.toList lu_names
            putStrLn $ replicate 70 '-'

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

  body' <-
    modifyNameSource $ \src ->
      let m1 = runReaderT m coaltab
          (z,newsrc) = runState m1 src
      in  (z,newsrc)

  debug `seq` return fundef { funDefBody = body' }
  where m = transformBody $ funDefBody fundef

type MergeM = ReaderT DS.CoalsTab (State VNameSource)

transformBody :: Body ExpMem.ExplicitMemory -> MergeM (Body ExpMem.ExplicitMemory)
transformBody (Body () bnds res) = do
  bnds' <- concat <$> mapM transformStm bnds
  return $ Body () bnds' res

transformStm :: Stm ExpMem.ExplicitMemory -> MergeM [Stm ExpMem.ExplicitMemory]
transformStm (Let (Pattern patCtxElems patValElems) () e) = do
  e' <- mapExpM transform e
  patValElems' <- mapM transformPatValElemT patValElems

  let pat' = Pattern patCtxElems patValElems'

  return [Let pat' () e']

  where transform = identityMapper { mapOnBody = const transformBody
                                   , mapOnFParam = transformFParam
                                   }

transformFParam :: FParam ExpMem.ExplicitMemory -> MergeM (FParam ExpMem.ExplicitMemory)
transformFParam (Param x
                 membound_orig@(ExpMem.ArrayMem _pt shape u xmem _xixfun)) = do
  membound <- newMemBound x xmem shape u
  return $ Param x $ fromMaybe membound_orig membound
transformFParam fp = return fp

transformPatValElemT :: PatElemT (LetAttr ExpMem.ExplicitMemory)
                     -> MergeM (PatElemT (LetAttr ExpMem.ExplicitMemory))
transformPatValElemT (PatElem x bindage
                      membound_orig@(ExpMem.ArrayMem _pt shape u xmem _xixfun)) = do
  membound <- newMemBound x xmem shape u
  return $ PatElem x bindage $ fromMaybe membound_orig membound
transformPatValElemT pe = return pe

newMemBound :: VName -> VName -> Shape -> u -> MergeM (Maybe (ExpMem.MemBound u))
newMemBound x xmem shape u = do
  coaltab <- ask
  return $ do
    entry <- M.lookup xmem coaltab
    DS.Coalesced _ (DS.MemBlock pt _shape _ xixfun) _ <-
      M.lookup x $ DS.vartab entry
    return $ ExpMem.ArrayMem pt shape u (DS.dstmem entry) xixfun


-- Clean up the existential contexts in the parameters.

cleanUpContextFunDef :: MonadFreshNames m
                     => FunDef ExpMem.ExplicitMemory
                     -> m (FunDef ExpMem.ExplicitMemory)
cleanUpContextFunDef fundef = do
  let m = cleanUpContextBody (scopeOf fundef) $ funDefBody fundef
      body' = runReader m M.empty
  return fundef { funDefBody = body' }

type CleanUpM = Reader (Scope ExpMem.ExplicitMemory)

cleanUpContextBody :: Scope ExpMem.ExplicitMemory
                   -> Body ExpMem.ExplicitMemory
                   -> CleanUpM (Body ExpMem.ExplicitMemory)
cleanUpContextBody scope (Body () bnds res) = do
  bnds' <- localScope scope $ localScope (scopeOf bnds)
           $ mapM cleanUpContextStm bnds
  return $ Body () bnds' res

cleanUpContextStm :: Stm ExpMem.ExplicitMemory
                  -> CleanUpM (Stm ExpMem.ExplicitMemory)
cleanUpContextStm (Let pat@(Pattern patCtxElems patValElems) () e) = do
  remaining <- ExpMem.findUnusedPatternPartsInExp pat e
  let patCtxElems' = S.toList $ S.fromList patCtxElems S.\\ S.fromList remaining
  let pat' = Pattern patCtxElems' patValElems
  e' <- mapExpM cleanUpContext e
  return $ Let pat' () e'
  where cleanUpContext = identityMapper { mapOnBody = cleanUpContextBody }
