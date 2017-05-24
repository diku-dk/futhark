-- | Move allocation statements upwards in the bodies of a program to enable
-- more array coalescings.
--
-- This should be run *before* the actual memory block merging, as it enables
-- more optimisations.
--
-- It hoists only allocations used by Copy or Concat, as those are the only ones
-- considered for merging in the remaining parts of the optimisation.
module Futhark.Pass.MemoryBlockMerging.AllocHoisting
  ( hoistAllocsFunDef
  ) where

import System.IO.Unsafe (unsafePerformIO) -- Just for debugging!

import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy)
import Data.Maybe (mapMaybe, isJust)
import Data.Function (on)

import Futhark.MonadFreshNames
import Futhark.Tools
import Data.Monoid
import Futhark.Representation.AST
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Pass.ExplicitAllocations()

import Futhark.Util (unixEnvironment)
usesDebugging :: Bool
usesDebugging = isJust $ lookup "FUTHARK_DEBUG" unixEnvironment

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

hoistAllocsFunDef :: MonadFreshNames m
                  => FunDef ExpMem.ExplicitMemory
                  -> m (FunDef ExpMem.ExplicitMemory)
hoistAllocsFunDef fundef = do
  let scope_new = scopeOf fundef
      bindingmap_cur = M.empty
      body' = hoistAllocsBody scope_new bindingmap_cur $ funDefBody fundef
  return fundef { funDefBody = body' }

hoistAllocsBody :: Scope ExpMem.ExplicitMemory
                -> BindingMap
                -> Body ExpMem.ExplicitMemory
                -> Body ExpMem.ExplicitMemory
hoistAllocsBody scope_new bindingmap_old body =
  let hoistees = findHoistees body

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

      debug = unsafePerformIO $ when usesDebugging $ do
        putStrLn $ replicate 10 '*' ++ " Allocations found in body "  ++ replicate 10 '*'
        forM_ hoistees print
        putStrLn $ replicate 70 '-'

  in debug `seq` body'

scopeBindingMap :: (VName, NameInfo ExpMem.ExplicitMemory)
                -> (VName, PrimBinding)
scopeBindingMap (x, _) = (x, PrimBinding [] FromFParam)

bodyBindingMap :: [Stm ExpMem.ExplicitMemory] -> BindingMap
bodyBindingMap stms =
  M.fromList $ concatMap createBindingStmt $ zip [0..] stms

  where createBindingStmt :: (Line, Stm ExpMem.ExplicitMemory)
                          -> [(VName, PrimBinding)]
        createBindingStmt (line, stmt@(Let (Pattern _ patelems) () _)) =
          let frees = S.toList $ freeInStm stmt
          in map (\(PatElem x _ _) ->
                     (x, PrimBinding frees (FromLine line))) patelems

hoistRecursivelyStm :: BindingMap
                    -> Stm ExpMem.ExplicitMemory
                    -> Stm ExpMem.ExplicitMemory
hoistRecursivelyStm bindingmap (Let pat () e) =
  runIdentity (Let pat () <$> mapExpM transform e)

  where transform = identityMapper { mapOnBody = mapper }
        mapper scope_new = return . hoistAllocsBody scope_new bindingmap'
        -- The nested body cannot move to any of its locations of its parent's
        -- body, so we say that all its parent's bindings are parameters.
        bindingmap' = M.map (\(PrimBinding vs _) -> PrimBinding vs FromFParam) bindingmap

findHoistees :: Body ExpMem.ExplicitMemory
             -> [VName]
findHoistees body = concatMap findThem stms

  where stms :: [Stm ExpMem.ExplicitMemory]
        stms = bodyStms body

        findThem :: Stm ExpMem.ExplicitMemory -> [VName]
        findThem (Let (Pattern _ [PatElem xmem _ _])
                  () (Op (ExpMem.Alloc _ _))) = usedByCopyOrConcat xmem
        findThem _ = []

        -- Is the allocated memory used by either Copy or Concat in the function
        -- body?  Those are the only kinds of memory we care about, since those
        -- are the cases handled in ArrayCoalescing.  Also find the names used
        -- by inplace updates, since those also need to be hoisted (as an
        -- example of this, consider the 'copy/pos1.fut' test where the
        -- replicate expression needs to be hoisted as well as its memory
        -- allocation).
        usedByCopyOrConcat :: VName -> [VName]
        usedByCopyOrConcat xmem_alloc =
          let vs = mapMaybe checkStm stms
          in if null vs then [] else xmem_alloc : concat vs

          where checkStm :: Stm ExpMem.ExplicitMemory -> Maybe [VName]
                checkStm (Let
                          (Pattern _
                           [PatElem _ bindage
                            (ExpMem.ArrayMem _ _ _ xmem_pat _)])
                           ()
                           (BasicOp bop))
                  | xmem_pat == xmem_alloc =
                      let vs = Just $ case bindage of
                            BindVar -> []
                            BindInPlace _ v _ -> [v]
                      in case bop of
                        Copy{} -> vs
                        Concat{} -> vs
                        _ -> Nothing
                checkStm _ = Nothing

hoist :: BindingMap
      -> Body ExpMem.ExplicitMemory
      -> VName
      -> (Body ExpMem.ExplicitMemory, BindingMap)
hoist bindingmap_cur body hoistee =
  let bindingmap = bindingmap_cur <> bodyBindingMap (bodyStms body)

      body' = runState (moveLetUpwards hoistee body) bindingmap

      debug = unsafePerformIO $ when usesDebugging $ do
        putStrLn $ replicate 10 '*' ++ " Hoisting "  ++ replicate 10 '*'
        putStrLn $ "Name: " ++ show hoistee
        putStrLn $ replicate 70 '-'

  in debug `seq` body'

lookupPrimBinding :: VName -> State BindingMap PrimBinding
lookupPrimBinding vname = do
  m <- M.lookup vname <$> get
  case m of
    Just b -> return b
    Nothing -> error (pretty vname ++ " was not found in BindingMap.  This should not happen!")

sortByKeyM :: (Ord t, Monad m) => (a -> m t) -> [a] -> m [a]
sortByKeyM f xs = do
  rs <- mapM f xs
  return $ map fst $ sortBy (compare `on` snd) $ zip xs rs

-- Move a statement as much up as possible.
moveLetUpwards :: VName -> Body ExpMem.ExplicitMemory
               -> State BindingMap (Body ExpMem.ExplicitMemory)
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

      let debug = line_dest `seq` unsafePerformIO $ when usesDebugging $ do
            print letname
            print deps'
            print line_cur
            print line_dest
            putStrLn (replicate 70 '-')

      debug `seq` return body' { bodyStms = stms' }

-- Both move the statement to the new line, and update the BindingMap.
moveLetToLine :: VName -> Line -> Line -> [Stm ExpMem.ExplicitMemory]
              -> State BindingMap [Stm ExpMem.ExplicitMemory]
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
