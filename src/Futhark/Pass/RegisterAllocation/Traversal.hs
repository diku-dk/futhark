{-# LANGUAGE TupleSections #-}
-- | Traverse a body to find memory blocks that can be allocated together.
module Futhark.Pass.RegisterAllocation.Traversal
  ( regAllocFunDef
  , RegAllocResult(..)
  ) where

import System.IO.Unsafe (unsafePerformIO) -- Just for debugging!

import Control.Monad.RWS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe (isJust, fromMaybe, mapMaybe)

import Futhark.Analysis.Alias (analyseFun)
import Futhark.Tools
import Futhark.Representation.AST
--import Futhark.Representation.AST.Traversals
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Pass.ExplicitAllocations()

import qualified Futhark.Pass.MemoryBlockMerging.LastUse as LastUse
import qualified Futhark.Pass.MemoryBlockMerging.Interference as Interference
import qualified Futhark.Pass.MemoryBlockMerging.DataStructs as DS

import Futhark.Util (unixEnvironment)
usesDebugging :: Bool
usesDebugging = isJust $ lookup "FUTHARK_DEBUG" unixEnvironment

data RegAllocResult = RegAllocResult
  { -- | Mapping from variable name to new memory block.  Only includes the
    -- variable names where there is a new mapping.
    regMappings :: M.Map VName VName

    -- | Fixes in body results where a memory block needs to be replaced by
    -- another memory block.  Can occur in loops.
  , regMemFixes :: M.Map VName VName
  }
  deriving (Show)

type Sizes = M.Map VName SubExp

data Context = Context { ctxInterferences :: Interference.IntrfEnv
                       , ctxSizes :: Sizes
                       }
  deriving (Show)

data Current = Current { curUses :: M.Map VName Names

                         -- Mostly used as in a writer monad, but not fully.
                       , curResult :: RegAllocResult
                       }
  deriving (Show)

type TraversalMonad a = RWS Context () Current a

insertUse :: VName -> VName -> TraversalMonad ()
insertUse mem x =
  modify $ \cur -> cur { curUses = M.alter (insertOrNew x) mem $ curUses cur }

insertOrNew :: Ord a => a -> Maybe (S.Set a) -> Maybe (S.Set a)
insertOrNew x m = Just $ case m of
  Just s -> S.insert x s
  Nothing -> S.singleton x

modifyResult :: (RegAllocResult -> RegAllocResult) -> TraversalMonad ()
modifyResult f = modify $ \cur -> cur { curResult = f $ curResult cur }

recordMapping :: VName -> VName -> TraversalMonad ()
recordMapping x mem =
    modifyResult $ \res -> res { regMappings = M.union (M.singleton x mem) $ regMappings res }

recordMemFix :: VName -> VName -> TraversalMonad ()
recordMemFix xmem ymem =
    modifyResult $ \res -> res { regMemFixes = M.union (M.singleton xmem ymem) $ regMemFixes res }

withLocalUses :: TraversalMonad a -> TraversalMonad a
withLocalUses m = do
  -- Keep the curResult.
  uses <- gets curUses
  res <- m
  modify $ \cur -> cur { curUses = uses }
  return res

memBlockSizes :: FunDef ExpMem.ExplicitMemory -> Sizes
memBlockSizes fundef = M.union fromParams fromBody
  where fromParams = M.fromList $ concatMap onParam $ funDefParams fundef
        onParam (Param mem (ExpMem.MemMem size _space)) = [(mem, size)]
        onParam _ = []

        fromBody = M.fromList $ concatMap onStm $ bodyStms $ funDefBody fundef
        onStm (Let (Pattern _ [PatElem mem _ _]) ()
               (Op (ExpMem.Alloc size _))) = [(mem, size)]
        onStm stm = foldExp folder [] $ bindingExp stm
        folder = identityFolder
          { foldOnStm = \sizes stm -> return (sizes ++ onStm stm)

          -- Sizes found from the functions below are scope-local, but that does
          -- not matter; we want all sizes so that we can lookup anything.
          , foldOnFParam = \sizes fparam -> return (sizes ++ onParam fparam)
          , foldOnLParam = \sizes lparam -> return (sizes ++ onParam lparam)
          }

regAllocFunDef :: FunDef ExpMem.ExplicitMemory -> RegAllocResult
regAllocFunDef fundef = do
  let fundef_aliases = analyseFun fundef
      lutab = LastUse.lastUseFun fundef_aliases
      interferences = Interference.intrfAnFun lutab fundef_aliases
      sizes = memBlockSizes fundef
      context = Context interferences sizes
      current_empty = Current M.empty $ RegAllocResult M.empty M.empty

      m = regAllocBody $ funDefBody fundef
      result = curResult $ fst $ execRWS m context current_empty

  let debug = interferences `seq` unsafePerformIO $ when usesDebugging $ do
        -- Print last uses.
        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " Last uses in "  ++ pretty (funDefName fundef) ++ " " ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs lutab) $ \(stmt_name, lu_names) -> do
          putStrLn $ "Last uses for " ++ pretty stmt_name ++ ":"
          putStrLn $ L.intercalate "   " $ map pretty $ S.toList lu_names
          putStrLn $ replicate 70 '-'

        -- Print interferences.
        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " Memory block interferences in "  ++ pretty (funDefName fundef) ++ " " ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs $ Interference.intrf interferences) $ \(stmt_name, interf_names) -> do
          putStrLn $ "Interferences for " ++ pretty stmt_name ++ ":"
          putStrLn $ L.intercalate "   " $ map pretty $ S.toList interf_names
          putStrLn $ replicate 70 '-'

        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " Aliases in "  ++ pretty (funDefName fundef) ++ " " ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs $ Interference.alias interferences) $ \(stmt_name, alias_names) -> do
          putStrLn $ "Aliases for " ++ pretty stmt_name ++ ":"
          putStrLn $ L.intercalate "   " $ map pretty $ S.toList alias_names
          putStrLn $ replicate 70 '-'

        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " v2mem in "  ++ pretty (funDefName fundef) ++ " " ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs $ Interference.v2mem interferences) $ \(stmt_name, DS.MemBlock _ _ var_mem _) -> do
          putStrLn $ "Memory block for var " ++ pretty stmt_name ++ ": " ++ pretty var_mem
          putStrLn $ replicate 70 '-'

        -- Print sizes
        replicateM_ 5 $ putStrLn ""
        putStrLn $ replicate 10 '*' ++ " Sizes in "  ++ pretty (funDefName fundef) ++ " " ++ replicate 10 '*'
        putStrLn $ replicate 70 '-'
        forM_ (M.assocs sizes) $ \(x, size) ->
          putStrLn $ "Sizes for " ++ pretty x ++ ": " ++ show size

        -- Print results.
        putStrLn $ replicate 70 '-'
        putStrLn "Allocation results!"
        print result
        putStrLn $ replicate 70 '-'

  debug `seq` result

regAllocBody :: Body ExpMem.ExplicitMemory
             -> TraversalMonad ()
regAllocBody (Body () bnds _res) =
  mapM_ regAllocStm bnds

regAllocStm :: Stm ExpMem.ExplicitMemory -> TraversalMonad ()
regAllocStm (Let (Pattern patctxelems patvalelems) () e) = do
  withLocalUses $ walkExpM walker e

  let creates_new_array = createsNewArrOK e
  when creates_new_array $ mapM_ handleNewArray patvalelems

  case e of
    DoLoop _mergectxparams mergevalparams _loopform body -> do
      -- In this case we need to record mappings to a memory block for all
      -- existential loop variables whose initial value maps to that memory
      -- block.
      res_so_far <- gets (regMappings . curResult)
      let findMem (_, Var y) = M.lookup y res_so_far
          findMem _ = Nothing
          mem_replaces = map findMem mergevalparams

          updateFromReplace (Just mem) x = do
            insertUse mem x
            recordMapping x mem
          updateFromReplace Nothing _ = return ()

      forM_ (zip mem_replaces mergevalparams) $ \(mem_may, (Param x _, _)) ->
        updateFromReplace mem_may x

      forM_ (zip mem_replaces patvalelems) $ \(mem_may, PatElem x _ _) ->
        updateFromReplace mem_may x

      -- The body of a loop can return a memory block in its results.  This is
      -- the memory block used by a variable which is also part of the results.
      -- If the memory block of that variable is changed, we need a way to
      -- record that the memory block in the body result also needs to change.
      let zipped = zip [(0::Int)..] (patctxelems ++ patvalelems) -- Should be fine?
          findMemLinks (i, PatElem _x _binding (ExpMem.ArrayMem _ _ _ xmem _)) =
            case L.find (\(_, PatElem ymem _ _) -> ymem == xmem) zipped of
              Just (j, _) -> Just (j, i)
              Nothing -> Nothing
          findMemLinks _ = Nothing

          mem_links = mapMaybe findMemLinks zipped

          res = bodyResult body

      res_so_far' <- gets (regMappings . curResult)
      let fixResRecord (i, se)
            | Var mem <- se
            , Just j <- L.lookup i mem_links
            , Var related_var <- res L.!! j
            , Just mem_new <- M.lookup related_var res_so_far' =
                recordMemFix mem mem_new
            | otherwise = return ()

      forM_ (zip [(0::Int)..] res) fixResRecord

      let debug = unsafePerformIO $ when usesDebugging $ do
            putStrLn "Extraordinary loop handling."
            print patctxelems
            print patvalelems
            print mem_links

      debug `seq` return ()
    _ -> return ()

  let debug = unsafePerformIO $ when usesDebugging $ do
        putStrLn $ replicate 70 '-'
        putStrLn "Statement."
        print patvalelems
        print e
        putStrLn $ replicate 70 '-'

  debug `seq` return ()

  where walker = identityWalker { walkOnBody = regAllocBody }

handleNewArray :: PatElem ExpMem.ExplicitMemory -> TraversalMonad ()
handleNewArray (PatElem x _bindage (ExpMem.ArrayMem _ _ _ xmem _)) = do
  uses <- gets curUses
  interferences <- asks ctxInterferences
  sizes <- asks ctxSizes

  let varMem var = (\(DS.MemBlock _ _ var_mem _) -> var_mem)
                   <$> M.lookup var (Interference.v2mem interferences)

      memMems mem = S.union
                    (S.singleton mem)
                    (fromMaybe S.empty $ M.lookup mem $ Interference.alias interferences)

      memInterferences mem = S.foldl S.union S.empty $ S.map memInterferences' $ memMems mem
        where memInterferences' m = S.union (S.singleton m) (fromMaybe S.empty $ M.lookup m $ Interference.intrf interferences)

  let memInterferesWithVar mem var
        | Just var_mem <- varMem var =
            let debug = unsafePerformIO $ when usesDebugging $ do
                  putStrLn $ replicate 70 '-'
                  putStrLn ("Mem " ++ pretty mem ++ " interferes with var " ++ pretty var ++ " (mem " ++ pretty var_mem ++ ")?")
                  print $ memInterferences mem
                  print $ memInterferences var_mem
                  putStrLn $ replicate 70 '-'
            in debug `seq` (S.member mem (memInterferences var_mem) || S.member var_mem (memInterferences mem))
        | otherwise = True

  let notTheSame :: (VName, Names) -> Bool
      notTheSame (kmem, _vars) = kmem /= xmem

  let sizesMatch :: (VName, Names) -> Bool
      sizesMatch (kmem, _vars) = equalSizeSubExps (sizes M.! kmem) (sizes M.! xmem)

  let noneInterfere :: (VName, Names) -> Bool
      noneInterfere (_kmem, vars) = not $ any (memInterferesWithVar xmem) vars

  let debug = unsafePerformIO $ when usesDebugging $ do
        putStrLn $ replicate 70 '-'
        putStrLn "Handle new array."
        print x
        print xmem
        print uses
        print sizes
        putStrLn $ replicate 70 '-'

  let canBeUsed t = notTheSame t && sizesMatch t && noneInterfere t

  case L.find canBeUsed $ M.assocs uses of
    Just (kmem, _vars) -> do
      insertUse kmem x
      recordMapping x kmem
    Nothing ->
      insertUse xmem x

  debug `seq` return ()

handleNewArray _ = return ()

-- FIXME: Less conservative, please.  Would require some more state.
equalSizeSubExps :: SubExp -> SubExp -> Bool
equalSizeSubExps x y =
  let eq = (x == y)

      debug = unsafePerformIO $ when usesDebugging $ do
        putStrLn $ replicate 70 '-'
        putStrLn "Equal sizes?"
        print x
        print y
        putStrLn $ replicate 70 '-'

  in debug `seq` eq

-- FIXME: Generalise the one from DataStructs.
-- watch out for copy and concat
createsNewArrOK :: Exp ExpMem.ExplicitMemory -> Bool
createsNewArrOK (BasicOp Partition{}) = True
createsNewArrOK (BasicOp Replicate{}) = True
createsNewArrOK (BasicOp Iota{}) = True
createsNewArrOK (BasicOp Manifest{}) = True
createsNewArrOK (BasicOp ExpMem.Copy{}) = True
createsNewArrOK (BasicOp Concat{}) = True
createsNewArrOK (BasicOp ArrayLit{}) = True
createsNewArrOK (BasicOp Scratch{}) = True
createsNewArrOK (Op (ExpMem.Inner ExpMem.Kernel{})) = True
createsNewArrOK _ = False
