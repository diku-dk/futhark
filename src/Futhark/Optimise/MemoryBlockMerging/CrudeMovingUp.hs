-- | Move variables as much as possible upwards in a program.
module Futhark.Optimise.MemoryBlockMerging.CrudeMovingUp
  ( moveUpInFunDef
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
data PrimBinding = PrimBinding { pbFrees :: Names
                               , _pbConsumed :: Names
                               , pbOrigin :: Origin
                               }
  deriving (Show)

-- A mapping from names to PrimBinding.  The key is a collection of names, since
-- a statement can have multiple patterns.
type BindingMap = [(Names, PrimBinding)]

-- | Call 'findHoistees' for every body, and then hoist every one of the found
-- hoistees (variables).
moveUpInFunDef :: FunDef ExplicitMemory
               -> (Body ExplicitMemory -> Maybe [FParam ExplicitMemory] -> [VName])
               -> FunDef ExplicitMemory
moveUpInFunDef fundef findHoistees =
  let scope_new = scopeOf fundef
      bindingmap_cur = []
      body' = hoistInBody scope_new bindingmap_cur
              (Just (funDefParams fundef)) findHoistees (funDefBody fundef)
      fundef' = fundef { funDefBody = body' }
  in fundef'

lookupPrimBinding :: VName -> State BindingMap PrimBinding
lookupPrimBinding vname =
  gets $ snd . fromJust (pretty vname ++ " was not found in BindingMap."
                         ++ "  This should not happen!")
  . L.find ((vname `S.member`) . fst)

namesDependingOn :: VName -> State BindingMap Names
namesDependingOn v =
  gets $ S.unions . map fst . filter (\(_, pb) -> v `S.member` pbFrees pb)

scopeBindingMap :: (VName, NameInfo ExplicitMemory)
                -> BindingMap
scopeBindingMap (x, _) = [(S.singleton x, PrimBinding S.empty S.empty FromFParam)]

-- Find all variables bound in a KernelSpace.
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

-- FIXME: The results of this should maybe go in the core 'freeIn' function, or
-- perhaps the ExplicitMemory module, instead of this arbitrary module.
boundInExpExtra :: Exp ExplicitMemory -> Names
boundInExpExtra = execWriter . inExp
  where inExp :: Exp ExplicitMemory -> Writer Names ()
        inExp e = case e of
          Op (ExpMem.Inner (ExpMem.Kernel _ space _ _)) ->
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
        createBindingStmt (line, stmt@(Let (Pattern patctxelems patvalelems) _ e)) =
          let stmt_vars = S.fromList (map patElemName (patctxelems ++ patvalelems))
              frees = freeInStm stmt
              consumed = case e of BasicOp (Update src _ _) -> S.singleton src
                                   _ -> mempty
              bound_extra = boundInExpExtra e
              frees' = frees `S.difference` bound_extra
              vars_binding = (stmt_vars, PrimBinding frees' consumed (FromLine line e))

              -- Some variables exist only in a shape declaration.
              shape_sizes = S.fromList $ concatMap shapeSizes (patctxelems ++ patvalelems)
              sizes_binding = (shape_sizes, PrimBinding frees' consumed (FromLine line e))

              -- Some expressions contain special identifiers that are used in a
              -- body.  This should go somewhere else than here.
              param_vars = case e of
                Op (ExpMem.Inner (ExpMem.Kernel _ space _ _)) ->
                  boundInKernelSpace space
                _ -> S.empty
              params_binding = (param_vars, PrimBinding S.empty S.empty FromFParam)

              bmap = [vars_binding, sizes_binding, params_binding]
          in bmap

        shapeSizes (PatElem _ (ExpMem.MemArray _ shape _ _)) =
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
      bindingmap_body = bodyBindingMap $ stmsToList $ bodyStms body
      bindingmap = bindingmap_old ++ bindingmap_fromscope ++ bindingmap_body

      -- Create a new body where all hoistees have been moved as much upwards in
      -- the statement list as possible.
      (Body () bnds res, bindingmap') =
        foldl (\(body0, lbindingmap) -> hoist lbindingmap body0)
        (body, bindingmap) hoistees

      -- Touch upon any subbodies.
      bnds' = fmap (hoistRecursivelyStm bindingmap' findHoistees) bnds
      body' = Body () bnds' res

  in body'

hoistRecursivelyStm :: BindingMap
                    -> (Body ExplicitMemory -> Maybe [FParam ExplicitMemory] -> [VName])
                    -> Stm ExplicitMemory
                    -> Stm ExplicitMemory
hoistRecursivelyStm bindingmap findHoistees (Let pat aux e) =
  runIdentity (Let pat aux <$> mapExpM transform e)

  where transform = identityMapper { mapOnBody = mapper }
        mapper scope_new = return . hoistInBody scope_new bindingmap' Nothing findHoistees
        -- The nested body cannot move to any of its locations of its parent's
        -- body, so we say that all its parent's bindings are parameters.
        bindingmap' = map (\(ns, PrimBinding frees consumed _) ->
                             (ns, PrimBinding frees consumed FromFParam))
                      bindingmap

-- Hoist the statement denoted by 'hoistee' as much upwards as possible in
-- 'body', and return the new body.
hoist :: BindingMap
      -> Body ExplicitMemory
      -> VName
      -> (Body ExplicitMemory, BindingMap)
hoist bindingmap_cur body hoistee =
  let bindingmap = bindingmap_cur <> bodyBindingMap (stmsToList $ bodyStms body)

      body' = runState (moveLetUpwards hoistee body) bindingmap

  in body'

-- Move a statement as much up as possible.
moveLetUpwards :: VName -> Body ExplicitMemory
               -> State BindingMap (Body ExplicitMemory)
moveLetUpwards letname body = do
  PrimBinding deps consumed letorig <- lookupPrimBinding letname

  -- Extend the dependencies with all those statements that use the consumed
  -- variables of this statement, except the current statement.
  deps' <- S.delete letname
           <$> (S.union deps
                <$> (S.unions <$> mapM namesDependingOn (S.toList consumed)))

  case letorig of
    FromFParam -> return body
    FromLine line_cur exp_cur ->
      case exp_cur of
        -- We do not want to change the structure of the program too much, so we
        -- restrict the aggressive hoister to *stop* and not hoist loops and
        -- kernels, as hoisting these expressions might actually make a
        -- hoisting-dependent optimisation *poorer* because of some assumptions
        -- about the structure.  FIXME: Do this nicer in a way where it is easy
        -- to argue for it.
        DoLoop{} -> return body
        Op ExpMem.Inner{} -> return body
        _ -> do
          -- Sort by how close they are to the beginning of the body.  The closest
          -- one should be the first one to hoist, so that the other ones can maybe
          -- exploit it.
          deps'' <- sortByKeyM (fmap pbOrigin . lookupPrimBinding)
                    $ S.toList deps'
          body' <- foldM (flip moveLetUpwards) body deps''
          origins <- mapM (fmap pbOrigin . lookupPrimBinding) deps''
          let line_dest = case foldl max FromFParam origins of
                FromFParam -> 0
                FromLine n _e -> n + 1

          PrimBinding _ _ letorig' <- lookupPrimBinding letname
          when (letorig' /= letorig) $ error "Assertion: This should not happen."

          stms' <- moveLetToLine letname line_cur line_dest $ stmsToList $ bodyStms body'

          return body' { bodyStms = stmsFromList stms' }

-- Both move the statement to the new line and update the BindingMap.
moveLetToLine :: VName -> Line -> Line -> [Stm ExplicitMemory]
              -> State BindingMap [Stm ExplicitMemory]
moveLetToLine stm_cur_name line_cur line_dest stms
  | line_cur == line_dest = return stms
  | otherwise = do

  let stm_cur = stms !! line_cur
      stms1 = take line_cur stms ++ drop (line_cur + 1) stms
      stms2 = take line_dest stms1 ++ [stm_cur] ++ drop line_dest stms1

  modify $ map (\t@(ns, PrimBinding frees consumed orig) ->
                   case orig of
                     FromFParam -> t
                     FromLine l e -> if l >= line_dest && l < line_cur
                                     then (ns, PrimBinding frees consumed
                                               (FromLine (l + 1) e))
                                     else t)

  r <- lookupPrimBinding stm_cur_name
  case r of
    PrimBinding frees consumed (FromLine _ exp_cur) ->
      modify $ replaceWhere stm_cur_name (PrimBinding frees consumed
                                          (FromLine line_dest exp_cur))
    _ -> error "moveLetToLine: unhandled case" -- fixme
  return stms2

replaceWhere :: VName -> PrimBinding -> BindingMap -> BindingMap
replaceWhere n pb1 =
  map (\(ns, pb) -> (ns, if n `S.member` ns
                         then pb1
                         else pb))
