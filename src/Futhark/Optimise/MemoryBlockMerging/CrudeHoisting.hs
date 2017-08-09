-- | Hoist everything asked for as much as possible.
module Futhark.Optimise.MemoryBlockMerging.CrudeHoisting
  ( hoistInFunDef
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous

import Control.Monad.State
import Control.Monad.Identity


type Line = Int
data Origin = FromFParam
            | FromLine Line (Exp ExplicitMemory)
  deriving (Eq, Ord, Show)

-- The dependencies and the location.
data PrimBinding = PrimBinding { _pbVars :: Names
                               , pbOrigin :: Origin
                               }
  deriving (Show)

-- A mapping from names to PrimBinding.  The key is a collection of names, since
-- a statement can have multiple patterns.
type BindingMap = [(Names, PrimBinding)]


hoistInFunDef :: FunDef ExplicitMemory
              -> (Body ExplicitMemory -> Maybe [FParam ExplicitMemory] -> [VName])
              -> FunDef ExplicitMemory
hoistInFunDef fundef findHoistees =
  let scope_new = scopeOf fundef
      bindingmap_cur = []
      body' = hoistInBody scope_new bindingmap_cur
              (Just (funDefParams fundef)) findHoistees (funDefBody fundef)
      fundef' = fundef { funDefBody = body' }

      debug = fundef' `seq` do
        putStrLn $ replicate 70 '='
        putStrLn "Result of hoistInFunDef:"
        putStrLn $ pretty fundef'
        putStrLn $ replicate 70 '='
  in withDebug debug fundef'

lookupPrimBinding :: VName -> State BindingMap PrimBinding
lookupPrimBinding vname = do
  bm <- get
  return $ snd $
    fromJust (pretty vname ++ " was not found in BindingMap."
              ++ "  This should not happen!")
    $ L.find ((vname `S.member`) . fst) bm

scopeBindingMap :: (VName, NameInfo ExplicitMemory)
                -> BindingMap
scopeBindingMap (x, _) = [(S.singleton x, PrimBinding S.empty FromFParam)]

boundInKernelSpace :: ExpMem.KernelSpace -> Names
boundInKernelSpace space =
  -- This might do too much.
  S.fromList ([ ExpMem.spaceGlobalId space
              , ExpMem.spaceLocalId space
              , ExpMem.spaceGroupId space]
              ++ (case ExpMem.spaceStructure space of
                    ExpMem.FlatThreadSpace ts ->
                      map fst ts ++ mapMaybe (fromVar . snd) ts
                    ExpMem.NestedThreadSpace ts ->
                      map (\(x, _, _, _) -> x) ts
                      ++ mapMaybe (fromVar . (\(_, x, _, _) -> x)) ts
                      ++ map (\(_, _, x, _) -> x) ts
                      ++ mapMaybe (fromVar . (\(_, _, _, x) -> x)) ts
                 ))

-- FIXME: The results of this should probably go in the core 'freeIn' function,
-- and not just in this random module.
boundInExpExtra :: Exp ExplicitMemory -> Names
boundInExpExtra = execWriter . inExp
  where inExp :: Exp ExplicitMemory -> Writer Names ()
        inExp e = case e of
          Op (ExpMem.Inner (ExpMem.Kernel _ _ space _ _)) ->
            tell $ boundInKernelSpace space
          _ -> walkExpM walker e

        walker = identityWalker {
          walkOnBody = mapM_ (inExp . stmExp) . bodyStms
          }

bodyBindingMap :: [Stm ExplicitMemory] -> BindingMap
bodyBindingMap stms =
  concatMap createBindingStmt $ zip [0..] stms
  -- We do not need to run this recursively on any sub-bodies, since this will
  -- be run for every call to hoistInBody, which *does* run recursively on
  -- sub-bodies.

  where createBindingStmt :: (Line, Stm ExplicitMemory)
                          -> BindingMap
        createBindingStmt (line, stmt@(Let (Pattern patctxelems patvalelems) () e)) =
          let stmt_vars = S.fromList (map patElemName (patctxelems ++ patvalelems))
              frees = freeInStm stmt
              bound_extra = boundInExpExtra e
              frees' = frees `S.difference` bound_extra
              vars_binding = (stmt_vars, PrimBinding frees' (FromLine line e))

              -- Some variables exist only in a shape declaration and so will
              -- have no PrimBinding.  If we hit the Nothing case, we assume
              -- that's what happened.
              shape_sizes = S.fromList $ concatMap shapeSizes (patctxelems ++ patvalelems)
              sizes_binding = (shape_sizes, PrimBinding frees (FromLine line e))

              -- Some expressions contain special identifiers that are used in a
              -- body.
              param_vars = case e of
                Op (ExpMem.Inner (ExpMem.Kernel _ _ space _ _)) ->
                  boundInKernelSpace space
                _ -> S.empty
              params_binding = (param_vars, PrimBinding S.empty FromFParam)

              bmap = [vars_binding, sizes_binding, params_binding]

              debug = do
                putStrLn $ replicate 70 '~'
                putStrLn "createBindingStmt:"
                print param_vars
                putStrLn $ replicate 70 '~'
          in withDebug debug bmap

        shapeSizes (PatElem _ _ (ExpMem.ArrayMem _ shape _ _ _)) =
          mapMaybe fromVar $ shapeDims shape
        shapeSizes _ = []

hoistInBody :: Scope ExplicitMemory
            -> BindingMap
            -> Maybe [FParam ExplicitMemory]
            -> (Body ExplicitMemory -> Maybe [FParam ExplicitMemory] -> [VName])
            -> Body ExplicitMemory
            -> Body ExplicitMemory
hoistInBody scope_new bindingmap_old params findHoistees body =
  let hoistees = findHoistees body params

      -- We use the possibly non-empty scope to extend our BindingMap.
      bindingmap_fromscope = concatMap scopeBindingMap $ M.toList scope_new
      bindingmap_body = bodyBindingMap (bodyStms body)
      bindingmap = bindingmap_old ++ bindingmap_fromscope ++ bindingmap_body

      -- Create a new body where all hoistees have been moved as much upwards in
      -- the statement list as possible.
      (Body () bnds res, bindingmap') =
        foldl (\(body0, lbindingmap) -> hoist lbindingmap body0)
        (body, bindingmap) hoistees

      -- Touch upon any subbodies.
      bnds' = map (hoistRecursivelyStm bindingmap' findHoistees) bnds
      body' = Body () bnds' res

      debug = hoistees `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "Hoistees found in body:"
        print bindingmap
        forM_ hoistees $ \h -> putStrLn ("hoistee: " ++ pretty h)
        putStrLn $ replicate 70 '~'

  in withDebug debug body'

hoistRecursivelyStm :: BindingMap
                    -> (Body ExplicitMemory -> Maybe [FParam ExplicitMemory] -> [VName])
                    -> Stm ExplicitMemory
                    -> Stm ExplicitMemory
hoistRecursivelyStm bindingmap findHoistees (Let pat () e) =
  runIdentity (Let pat () <$> mapExpM transform e)

  where transform = identityMapper { mapOnBody = mapper }
        mapper scope_new = return . hoistInBody scope_new bindingmap' Nothing findHoistees
        -- The nested body cannot move to any of its locations of its parent's
        -- body, so we say that all its parent's bindings are parameters.
        bindingmap' = map (\(ns, PrimBinding vs _) ->
                             (ns, PrimBinding vs FromFParam))
                      bindingmap

hoist :: BindingMap
      -> Body ExplicitMemory
      -> VName
      -> (Body ExplicitMemory, BindingMap)
hoist bindingmap_cur body hoistee =
  let bindingmap = bindingmap_cur ++ bodyBindingMap (bodyStms body)

      body' = runState (moveLetUpwards hoistee body) bindingmap

      debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "hoist:"
        putStrLn ("Name: " ++ show hoistee)
        putStrLn $ replicate 70 '~'

  in withDebug debug body'

-- Move a statement as much up as possible.
moveLetUpwards :: VName -> Body ExplicitMemory
               -> State BindingMap (Body ExplicitMemory)
moveLetUpwards letname body = do
  let debug0 = do
        putStrLn $ replicate 70 '~'
        putStrLn "moveLetUpwards 0:"
        print letname
        putStrLn $ replicate 70 '~'

  PrimBinding deps letorig <- withDebug debug0 $ lookupPrimBinding letname
  case letorig of
    FromFParam -> return body
    FromLine line_cur exp_cur ->
      case exp_cur of
        -- We do not want to change the structure of the program too much, so we
        -- restrict the aggressive hoister to *stop* and not hoist loops and
        -- kernels, as hoisting these expressions might actually make a
        -- hoisting-dependent optimisation *poorer* because of some assumptions
        -- about the structure.  FIXME: Do this nicer.
        DoLoop{} -> return body
        Op ExpMem.Inner{} -> return body
        _ -> do
          -- Sort by how close they are to the beginning of the body.  The closest
          -- one should be the first one to hoist, so that the other ones can maybe
          -- exploit it.

          let debug1 = do
                putStrLn $ replicate 70 '~'
                putStrLn "moveLetUpwards 1:"
                print letname
                putStrLn $ prettySet deps
                print line_cur
                -- putStrLn $ replicate 70 '|'
                -- putStrLn $ pretty body'
                -- putStrLn $ replicate 70 '|'
                putStrLn $ replicate 70 '~'

          deps' <- sortByKeyM (\t -> pbOrigin <$> lookupPrimBinding t)
                   $ withDebug debug1 $ S.toList deps
          body' <- foldM (flip moveLetUpwards) body deps'
          origins <- mapM (\t -> pbOrigin <$> lookupPrimBinding t) deps'
          let line_dest = case foldl max FromFParam origins of
                FromFParam -> 0
                FromLine n _e -> n + 1

          PrimBinding _ letorig' <- lookupPrimBinding letname
          when (letorig' /= letorig) $ error "Assertion: This should not happen."

          stms' <- moveLetToLine letname line_cur line_dest $ bodyStms body'

          return body' { bodyStms = stms' }

-- Both move the statement to the new line and update the BindingMap.
moveLetToLine :: VName -> Line -> Line -> [Stm ExplicitMemory]
              -> State BindingMap [Stm ExplicitMemory]
moveLetToLine stm_cur_name line_cur line_dest stms
  | line_cur == line_dest = return stms
  | otherwise = do

  let stm_cur = stms !! line_cur
      stms1 = take line_cur stms ++ drop (line_cur + 1) stms
      stms2 = take line_dest stms1 ++ [stm_cur] ++ drop line_dest stms1

  modify $ map (\t@(ns, PrimBinding vars orig) ->
                   case orig of
                     FromFParam -> t
                     FromLine l e -> if l >= line_dest && l < line_cur
                                     then (ns, PrimBinding vars (FromLine (l + 1) e))
                                     else t)

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "moveLetToLine:"
        putStrLn $ pretty stm_cur_name
        putStrLn $ replicate 70 '~'

  PrimBinding vars (FromLine _ exp_cur) <- withDebug debug $ lookupPrimBinding stm_cur_name -- fixme
  modify $ replaceWhere stm_cur_name (PrimBinding vars (FromLine line_dest exp_cur))

  return stms2

replaceWhere :: VName -> PrimBinding -> BindingMap -> BindingMap
replaceWhere n pb1 =
  map (\(ns, pb) -> (ns, if n `S.member` ns
                         then pb1
                         else pb))
