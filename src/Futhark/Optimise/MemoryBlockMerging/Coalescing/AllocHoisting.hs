-- | Move allocation statements upwards in the bodies of a program to enable
-- more array coalescings.
--
-- This should be run *before* the actual memory block merging, as it enables
-- more optimisations.
--
-- It hoists only allocations used by Copy or Concat, as those are the only ones
-- considered for merging in the remaining parts of the optimisation.
module Futhark.Optimise.MemoryBlockMerging.Coalescing.AllocHoisting
  ( hoistAllocsFunDef
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous

import Control.Monad.State
import Control.Monad.Identity
import Data.Function (on)

import Futhark.Pass.ExplicitAllocations()


type Line = Int
data Origin = FromFParam
            | FromLine Line
  deriving (Eq, Ord, Show)

-- The dependencies and the location.
data PrimBinding = PrimBinding { _pbVars :: [VName]
                               , pbOrigin :: Origin
                               }
  deriving (Show)

type BindingMap = M.Map VName PrimBinding

hoistAllocsFunDef :: FunDef ExplicitMemory
                  -> FunDef ExplicitMemory
hoistAllocsFunDef fundef =
  let scope_new = scopeOf fundef
      bindingmap_cur = M.empty
      body' = hoistAllocsBody scope_new bindingmap_cur (Just (funDefParams fundef)) $ funDefBody fundef
      fundef' = fundef { funDefBody = body' }

      debug = fundef' `seq` do
        putStrLn $ replicate 70 '='
        putStrLn "Result of hoistAllocsFunDef:"
        putStrLn $ pretty fundef'
        putStrLn $ replicate 70 '='
  in withDebug debug fundef'

hoistAllocsBody :: Scope ExplicitMemory
                -> BindingMap
                -> Maybe [FParam ExplicitMemory]
                -> Body ExplicitMemory
                -> Body ExplicitMemory
hoistAllocsBody scope_new bindingmap_old params body =
  let hoistees = findHoistees body params

      -- We use the possibly non-empty scope to extend our BindingMap.
      bindingmap_fromscope = M.fromList $ map scopeBindingMap $ M.toList scope_new
      bindingmap = bindingmap_old <> bindingmap_fromscope <> bodyBindingMap (bodyStms body)

      -- Create a new body where all hoistees have been moved as much upwards in
      -- the statement list as possible.
      (Body () bnds res, bindingmap') =
        foldl (\(body0, bindingmap0) -> hoist bindingmap0 body0)
        (body, bindingmap) hoistees

      -- Touch upon any subbodies.
      bnds' = map (hoistRecursivelyStm bindingmap') bnds
      body' = Body () bnds' res

      debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "Allocations found in body:"
        forM_ hoistees $ \h -> putStrLn ("hoistee: " ++ pretty h)
        putStrLn $ replicate 70 '~'

  in withDebug debug body'

scopeBindingMap :: (VName, NameInfo ExplicitMemory)
                -> (VName, PrimBinding)
scopeBindingMap (x, _) = (x, PrimBinding [] FromFParam)

bodyBindingMap :: [Stm ExplicitMemory] -> BindingMap
bodyBindingMap stms =
  M.fromList $ concatMap createBindingStmt $ zip [0..] stms
  -- We do not need to run this recursively on any sub-bodies, since this will
  -- be run for every call to hoistAllocsBody, which *does* run recursively on
  -- sub-bodies.

  where createBindingStmt :: (Line, Stm ExplicitMemory)
                          -> [(VName, PrimBinding)]
        createBindingStmt (line, stmt@(Let (Pattern _ patelems) () _)) =
          let frees = S.toList $ freeInStm stmt
          in map (\(PatElem x _ _) ->
                     (x, PrimBinding frees (FromLine line))) patelems

hoistRecursivelyStm :: BindingMap
                    -> Stm ExplicitMemory
                    -> Stm ExplicitMemory
hoistRecursivelyStm bindingmap (Let pat () e) =
  runIdentity (Let pat () <$> mapExpM transform e)

  where transform = identityMapper { mapOnBody = mapper }
        mapper scope_new = return . hoistAllocsBody scope_new bindingmap' Nothing
        -- The nested body cannot move to any of its locations of its parent's
        -- body, so we say that all its parent's bindings are parameters.
        bindingmap' = M.map (\(PrimBinding vs _) -> PrimBinding vs FromFParam) bindingmap

findHoistees :: Body ExplicitMemory -> Maybe [FParam ExplicitMemory]
             -> [VName]
findHoistees body params =
  let all_found = mapMaybe findThemStm stms ++ maybe [] (mapMaybe findThemFParam) params
      extras = concatMap snd all_found
      allocs = map fst all_found
      -- We must hoist the alloc expressions in the end.  If we hoist an alloc
      -- before we hoist one of its array creations (in case of in-place
      -- updates), that array creation might in turn hoist something depending
      -- on another memory block mem_y further up than the allocation of memory
      -- block mem_x.  This will become a problem if mem_y can get coalesced
      -- into mem_x.
      --
      -- Maybe there is a nicer way to guarantee that this does not happen, but
      -- this seems to work for now.
      --
      -- We reverse the non-alloc dependencies to ensure (sloppily) that they do
      -- not change positions internally compared to the original program: For
      -- example, if a statement x is located before a statement y, and both x
      -- and y need to be hoisted, then we need to hoist x in the end, so that
      -- it can be hoisted further than y, which might have been hoisted to
      -- before x.  A better solution is welcome!
      in reverse extras ++ reverse allocs

  where stms :: [Stm ExplicitMemory]
        stms = bodyStms body

        findThemStm :: Stm ExplicitMemory -> Maybe (VName, [VName])
        findThemStm (Let (Pattern _ [PatElem xmem _ _])
                     () (Op ExpMem.Alloc{})) = usedByCopyOrConcat xmem
        findThemStm _ = Nothing

        -- A function paramater can be a unique memory block.  While we cannot
        -- hoist that, we may have to hoist an index in an in-place update that
        -- uses the memory.
        findThemFParam :: FParam ExplicitMemory -> Maybe (VName, [VName])
        findThemFParam (Param xmem ExpMem.MemMem{}) = usedByCopyOrConcat xmem
        findThemFParam _ = Nothing

        -- Is the allocated memory used by either Copy or Concat in the function
        -- body?  Those are the only kinds of memory we care about, since those
        -- are the cases handled by coalescing.  Also find the names used by
        -- in-place updates, since those also need to be hoisted (as an example
        -- of this, consider the 'copy/pos1.fut' test where the replicate
        -- expression needs to be hoisted as well as its memory allocation).
        usedByCopyOrConcat :: VName -> Maybe (VName, [VName])
        usedByCopyOrConcat xmem_alloc =
          let vs = mapMaybe checkStm stms
              vs' = if null vs then Nothing else Just (xmem_alloc, concat vs)

              debug = do
                putStrLn $ replicate 70 '~'
                putStrLn "usedByCopyOrConcat:"
                putStrLn ("xmem_alloc: " ++ pretty xmem_alloc)
                putStrLn ("vars: " ++ prettySet (S.fromList vs))
                putStrLn ("vars': " ++ show vs')
                putStrLn $ replicate 70 '~'

          in withDebug debug vs'

          where checkStm :: Stm ExplicitMemory -> Maybe [VName]
                checkStm (Let
                          (Pattern _
                           [PatElem _ bindage
                            (ExpMem.ArrayMem _ _ _ xmem_pat _)])
                           ()
                           (BasicOp bop))
                  | xmem_pat == xmem_alloc =
                      let vs = Just $ case bindage of
                            BindVar -> []

                             -- The source array must also be hoisted so that it
                             -- is initialized before it is used by the
                             -- coalesced party.  Any index variables are also
                             -- hoisted.
                            BindInPlace _ v slice -> v : S.toList (freeIn slice)
                      in case bop of
                        Copy{} -> vs
                        Concat{} -> vs
                        _ -> Nothing
                checkStm _ = Nothing

hoist :: BindingMap
      -> Body ExplicitMemory
      -> VName
      -> (Body ExplicitMemory, BindingMap)
hoist bindingmap_cur body hoistee =
  let bindingmap = bindingmap_cur <> bodyBindingMap (bodyStms body)

      body' = runState (moveLetUpwards hoistee body) bindingmap

      debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "AllocHoisting hoist:"
        putStrLn ("Name: " ++ show hoistee)
        putStrLn $ replicate 70 '~'

  in withDebug debug body'

lookupPrimBinding :: VName -> State BindingMap PrimBinding
lookupPrimBinding vname = do
  m <- M.lookup vname <$> get
  case m of
    Just b -> return b
    Nothing -> error (pretty vname ++ " was not found in BindingMap.  This should not happen!  At least not with --cpu.")

sortByKeyM :: (Ord t, Monad m) => (a -> m t) -> [a] -> m [a]
sortByKeyM f xs = do
  rs <- mapM f xs
  return $ map fst $ L.sortBy (compare `on` snd) $ zip xs rs

-- Move a statement as much up as possible.
moveLetUpwards :: VName -> Body ExplicitMemory
               -> State BindingMap (Body ExplicitMemory)
moveLetUpwards letname body = do
  PrimBinding deps letorig <- lookupPrimBinding letname
  case letorig of
    FromFParam -> return body
    FromLine line_cur -> do
      -- Sort by how close they are to the beginning of the body.  The closest
      -- one should be the first one to hoist, so that the other ones can maybe
      -- exploit it.
      deps' <- sortByKeyM (\t -> pbOrigin <$> lookupPrimBinding t) deps
      body' <- foldM (flip moveLetUpwards) body deps'
      origins <- mapM (\t -> pbOrigin <$> lookupPrimBinding t) deps'
      let line_dest = case foldl max FromFParam origins of
            FromFParam -> 0
            FromLine n -> n + 1
      stms' <- moveLetToLine letname line_cur line_dest $ bodyStms body'

      let debug = line_dest `seq` do
            putStrLn $ replicate 70 '~'
            putStrLn "AllocHoisting moveLetUpwards"
            print letname
            print deps'
            print line_cur
            print line_dest
            putStrLn $ replicate 70 '~'

      withDebug debug $ return body' { bodyStms = stms' }

-- Both move the statement to the new line, and update the BindingMap.
moveLetToLine :: VName -> Line -> Line -> [Stm ExplicitMemory]
              -> State BindingMap [Stm ExplicitMemory]
moveLetToLine stm_cur_name line_cur line_dest stms
  | line_cur == line_dest = return stms
  | otherwise = do

  let stm_cur = stms !! line_cur
      stms1 = take line_cur stms ++ drop (line_cur + 1) stms
      stms2 = take line_dest stms1 ++ [stm_cur] ++ drop line_dest stms1

  modify $ M.map (\pb@(PrimBinding vars orig) ->
                    case orig of
                      FromFParam -> pb
                      FromLine l -> if l >= line_dest && l < line_cur
                                    then PrimBinding vars (FromLine (l + 1))
                                    else pb)

  PrimBinding vars _ <- lookupPrimBinding stm_cur_name
  modify $ M.delete stm_cur_name
  modify $ M.insert stm_cur_name (PrimBinding vars (FromLine line_dest))

  return stms2
