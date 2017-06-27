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
import Data.Maybe (fromMaybe, isJust, catMaybes)

import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Scope()
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

  let debug = unsafePerformIO $ when usesDebugging $ putStrLn $ pretty prog2

  debug `seq` return prog2


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

  let MergeM m0 = m
      m1 = runReaderT m0 coaltab
      body' = evalState m1 M.empty

  debug `seq` return fundef { funDefBody = body' }
  where m = transformBody $ funDefBody fundef

--type ForbiddenMemoryBlocks = M.Map VName

data ExistentialMemoryBlockFix = ExistentialMemoryBlockFix Shape ExpMem.IxFun
  deriving (Show)

newtype MergeM a = MergeM (ReaderT DS.CoalsTab (State (M.Map VName ExistentialMemoryBlockFix)) a)
  deriving (Monad, Functor, Applicative,
            MonadReader DS.CoalsTab,
            MonadState (M.Map VName ExistentialMemoryBlockFix))

transformBody :: Body ExpMem.ExplicitMemory
              -> MergeM (Body ExpMem.ExplicitMemory)
transformBody (Body () bnds res) = do
  bnds' <- mapM transformStm bnds
  res' <- mapM transformResultSubExp res
  return $ Body () bnds' res'

transformStm :: Stm ExpMem.ExplicitMemory
             -> MergeM (Stm ExpMem.ExplicitMemory)
transformStm (Let pat () e) = do
  (pat', e') <- case (pat, e) of

    (Pattern [] patvalelems, _) -> do
      -- No context to consider, so just replace any memory block you find.
      patvalelems' <- mapM transformPatValElemT patvalelems
      let pat' = Pattern [] patvalelems'
      e' <- case e of
        DoLoop [] mergevalparams loopform body -> do
          -- Also update the memory names in the merge parameters in the loop.
          mergevalparams' <- mapM transformValMergeParam mergevalparams
          return $ DoLoop [] mergevalparams' loopform body
        _ -> return e
      return (pat', e')

    (Pattern patctxelems patvalelems, DoLoop mergectxparams mergevalparams loopform body) -> do
      -- A loop with a context.  Update the actual memory blocks linked from the
      -- context memory blocks.

      let ctx_names = map (identName . patElemIdent) patctxelems
      ctx_mem_fixes <- catMaybes <$> mapM (getExistentialMemoryBlockFix ctx_names) patvalelems

      let ctx_names1 = map (identName . paramIdent . fst) mergectxparams
      ctx_mem_fixes1 <- catMaybes <$> mapM (getExistentialMemoryBlockFix1 ctx_names1) mergevalparams

      modify $ M.union (M.union (M.fromList ctx_mem_fixes) (M.fromList ctx_mem_fixes1))


      patvalelems' <- mapM transformPatValElemT patvalelems
      --applyExistentialMemoryBlockFix
      let pat' = Pattern patctxelems patvalelems'


      mergectxparams' <- mapM transformCtxMergeParam mergectxparams
      mergevalparams' <- mapM transformValMergeParamLight mergevalparams
      let e' = DoLoop mergectxparams' mergevalparams' loopform body
      return (pat', e')

    (Pattern patctxelems patvalelems, If {}) -> do
      -- An if with a context.  This should be fine, as the context memory block
      -- does not constitute extra memory allocation, but just a reference to
      -- the memory of whatever branch is taken.  However, the current array
      -- coalescing module is very aggressive, so it will say that the context
      -- block needs to be coalesced as well -- but the If expects an existental
      -- memory block!

      -- Make sure the context memory blocks are not updated from the coalescing
      -- table in @transformPatValElemT@ by putting it into the
      -- ForbiddenMemoryBlocks field.

      let ctx_names = map (identName . patElemIdent) patctxelems
      ctx_mem_fixes <- catMaybes <$> mapM (getExistentialMemoryBlockFix ctx_names) patvalelems
      modify $ M.union (M.fromList ctx_mem_fixes)




      let debug = unsafePerformIO $ do
            putStrLn "----------------------------"
            print patctxelems
            print patvalelems
            print ctx_mem_fixes
            putStrLn "----------------------------"

      patvalelems' <- mapM transformPatValElemT patvalelems
      --applyExistentialMemoryBlockFix
      let pat' = Pattern patctxelems patvalelems'

      debug `seq` return (pat', e)

    _ -> notReallySure (show (pat, e))

  e'' <- mapExpM transform e'

  return $ Let pat' () e''

  where transform = identityMapper { mapOnBody = const transformBody }

        notReallySure extra = error ("MemoryBlockMerging: transformStm: Is this '"
                                     ++ extra ++ "' thing a case that can occur?  (It is if you see this error message.)")


transformResultSubExp :: SubExp -> MergeM SubExp
transformResultSubExp s@(Var xmem) = do
  m <- get
  case M.lookup xmem m of
    Just _ -> return s
    Nothing -> do
      coaltab <- ask
      return $ fromMaybe s $ do
        entry <- M.lookup xmem coaltab
        return $ Var $ DS.dstmem entry
transformResultSubExp s = return s

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
transformValMergeParam (Param x membound_orig@(ExpMem.ArrayMem _pt shape u xmem _xixfun), se) = do
  membound <- newMemBound x xmem shape u
  return (Param x $ fromMaybe membound_orig membound, se)
transformValMergeParam t = return t

transformValMergeParamLight :: (FParam ExpMem.ExplicitMemory, SubExp)
                            -> MergeM (FParam ExpMem.ExplicitMemory, SubExp)
transformValMergeParamLight t@(Param x membound_orig@(ExpMem.ArrayMem pt shape u xmem _xixfun), se) = do

  m <- get
  case M.lookup xmem m of
    Just (ExistentialMemoryBlockFix shape' xixfun') ->
      return (Param x (ExpMem.ArrayMem pt shape' u xmem xixfun'), se)
    Nothing ->
      transformValMergeParam t

transformValMergeParamLight t = return t

transformPatValElemT :: PatElemT (LetAttr ExpMem.ExplicitMemory)
                     -> MergeM (PatElemT (LetAttr ExpMem.ExplicitMemory))
transformPatValElemT (PatElem x bindage
                      membound_orig@(ExpMem.ArrayMem pt shape u xmem _xixfun)) = do
  m <- get
  case M.lookup xmem m of
    Just (ExistentialMemoryBlockFix shape' xixfun') ->
      return $ PatElem x bindage $ ExpMem.ArrayMem pt shape' u xmem xixfun'
    Nothing -> do
      membound <- newMemBound x xmem shape u
      return $ PatElem x bindage $ fromMaybe membound_orig membound
transformPatValElemT pe = return pe

getExistentialMemoryBlockFix :: [VName] -> PatElemT (LetAttr ExpMem.ExplicitMemory)
                             -> MergeM (Maybe (VName, ExistentialMemoryBlockFix))
getExistentialMemoryBlockFix ctx_names (PatElem x _bindage
                                        (ExpMem.ArrayMem _pt0 shape0 u0 xmem0 _xixfun0))
  | xmem0 `elem` ctx_names = do
  n <- newMemBound x xmem0 shape0 u0
  return $ do
    ExpMem.ArrayMem _pt1 shape1 _u1 _xmem1 xixfun1 <- n
    return (xmem0, ExistentialMemoryBlockFix shape1 xixfun1)
getExistentialMemoryBlockFix _ _ = return Nothing

getExistentialMemoryBlockFix1 :: [VName] -> (FParam ExpMem.ExplicitMemory, SubExp)
                              -> MergeM (Maybe (VName, ExistentialMemoryBlockFix))
getExistentialMemoryBlockFix1 ctx_names (Param x (ExpMem.ArrayMem _pt0 shape0 u0 xmem0 _xixfun0), _se)
  | xmem0 `elem` ctx_names = do
  n <- newMemBound x xmem0 shape0 u0
  return $ do
    ExpMem.ArrayMem _pt1 shape1 _u1 _xmem1 xixfun1 <- n
    return (xmem0, ExistentialMemoryBlockFix shape1 xixfun1)
getExistentialMemoryBlockFix1 _ _ = return Nothing

newMemBound :: VName -> VName -> Shape -> u -> MergeM (Maybe (ExpMem.MemBound u))
newMemBound x xmem shape u = do
  coaltab <- ask
  return $ do
    entry <- M.lookup xmem coaltab
    DS.Coalesced _ (DS.MemBlock pt _shape _ xixfun) _ <-
      M.lookup x $ DS.vartab entry
    return $ ExpMem.ArrayMem pt shape u (DS.dstmem entry) xixfun
