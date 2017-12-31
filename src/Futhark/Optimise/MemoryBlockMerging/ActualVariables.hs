{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find the actual variables that need updating when a variable attribute
-- needs updating.  This is different than variable aliasing: Variable aliasing
-- is a theoretical concept, while this module has the practical purpose of
-- finding any extra variables that also need a change when a variable has a
-- change of memory block.
--
-- If and DoLoop statements have special requirements, as do some aliasing
-- expressions.  We don't want to (just) use the obvious statement variable;
-- sometimes updating the memory block of one variable actually means updating
-- the memory block of other variables as well.

module Futhark.Optimise.MemoryBlockMerging.ActualVariables
  ( findActualVariables
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (
  ExplicitMemorish, ExplicitMemory, InKernel)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.AllExpVars


data Context = Context
  { ctxVarToMem :: VarMemMappings MemorySrc
  , ctxFirstUses :: FirstUses
  }
  deriving (Show)

newtype FindM lore a = FindM { unFindM :: RWS Context () ActualVariables a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadState ActualVariables)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore,
                             LookInKernelExp lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

recordActuals :: VName -> Names -> FindM lore ()
recordActuals stmt_var more_actuals = do
  -- If S.empty has already been recorded, keep it at that.  This is because the
  -- ActualVariables system is currently also used for disabling memory block
  -- optimisations -- if a variables resolves to the empty set, don't touch it.
  -- This keeps some edge cases simple.  FIXME at some point.
  current_actuals <- M.lookup stmt_var <$> get
  case S.null <$> current_actuals of
    Just True -> return ()
    _ -> modify (insertOrUpdateMany stmt_var more_actuals)

-- Find all the actual variables in a function definition.
findActualVariables :: VarMemMappings MemorySrc -> FirstUses ->
                       FunDef ExplicitMemory -> ActualVariables
findActualVariables var_mem_mappings first_uses fundef =
  let context = Context var_mem_mappings first_uses
      m = unFindM $ lookInBody $ funDefBody fundef
      actual_variables = fst $ execRWS m context M.empty
  in actual_variables

lookInFParam :: LoreConstraints lore =>
                FParam lore -> FindM lore ()
lookInFParam (Param v _) =
  recordActuals v $ S.singleton v

lookInLParam :: LoreConstraints lore =>
                LParam lore -> FindM lore ()
lookInLParam (Param v _) =
  recordActuals v $ S.singleton v

lookInLambda :: LoreConstraints lore =>
                Lambda lore -> FindM lore ()
lookInLambda (Lambda params body _) = do
  forM_ params lookInLParam
  lookInBody body

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm stm@(Let (Pattern patctxelems patvalelems) _ e) = do
  forM_ patvalelems $ \(PatElem var bindage _) ->
    case bindage of
      BindInPlace orig _ -> do
        -- Record that when coalescing an in-place update statement, also look
        -- at the original array.
        let actuals = S.fromList [var, orig]
        recordActuals var actuals
      _ -> return ()

  -- Special handling of loops, ifs, etc.
  case e of
    DoLoop _mergectxparams mergevalparams loopform body -> do
      let body_vars0 = mapMaybe (fromVar . snd) mergevalparams
          body_vars1 = map (paramName . fst) mergevalparams
          body_vars2 = S.toList $ findAllExpVars e
          body_vars = body_vars0 ++ body_vars1 ++ body_vars2
      forM_ patvalelems $ \(PatElem var _ membound) -> do
        case membound of
          ExpMem.MemArray _ _ _ (ExpMem.ArrayIn mem _) -> do
            -- If mem is existential, we need to find the return memory that it
            -- refers to.  We cannot just look at its memory aliases, since it
            -- likely aliases both the initial memory and the final memory.

            let zipped = zip patctxelems (bodyResult body)
                mem_search = case L.find ((== mem) . patElemName . fst) zipped of
                  Just (_, Var res_mem) -> res_mem
                  _ -> mem
            -- Find the ones using the same memory as the result of the loop
            -- expression.
            body_vars' <- filterM (lookupGivesMem mem_search) body_vars
            -- Not only the result variable needs to change its memory block in
            -- case of a future memory merging with it; also the variables
            -- extracted above.
            let actuals = var : body_vars'
            forM_ actuals $ \a -> recordActuals a (S.fromList actuals)
            -- Some of these can be changed later on to have an actual variable
            -- set of S.empty, e.g. if one of the variables using the memory is
            -- a rearrange operation.  This is fine, and will occur in the walk
            -- later on.

            -- If you extend this loop handling, make sure not to target existential
            -- memory blocks.  We want those to stay.
          _ -> return ()

        -- It seems wrong to change the memory of merge variables, so we disable
        -- it.  If we were to accept it, we would need to record what other
        -- variables to change as well.  Seems hard.
        recordActuals var S.empty

      case loopform of
        ForLoop _ _ _ loop_vars ->
          -- Link 'array' to 'lvar' in 'for lvar in array' loop expressions.
          forM_ loop_vars $ \(Param lvar _, array) ->
            aliasOpHandleVar array lvar
        WhileLoop _ -> return ()

    If _se body_then body_else _types ->
      -- We don't want to coalesce the existiential memory block of the if.
      -- However, if a branch result has a memory block that is firstly used
      -- inside the branch, it is okay to coalesce that in a future statement.
      forM_ (zip3 patvalelems (bodyResult body_then) (bodyResult body_else))
        $ \(PatElem var _ membound, res_then, res_else) -> do
        let body_vars = S.toList $ findAllExpVars e
        case membound of
          ExpMem.MemArray _ _ _ (ExpMem.ArrayIn mem _) ->
            if mem `L.elem` map patElemName patctxelems
              then
              -- If the memory block is existential, we say that the If result
              -- refers to all results in the If.
              recordActuals var
              $ S.fromList (var : catMaybes [fromVar res_then, fromVar res_else])

              else do
              -- If the memory block is not existential, we need to find all the
              -- variables in any sub-bodies using the same memory block (like
              -- with loops).
              body_vars' <- filterM (lookupGivesMem mem) body_vars

              first_uses <- asks ctxFirstUses
              case filter ((mem `S.member`) . (`lookupEmptyable` first_uses)) body_vars' of
                [] ->
                  -- Not just the result variable needs to change its memory
                  -- block in case of a future memory block merging with it;
                  -- also the variables extracted above.
                  recordActuals var $ S.fromList (var : body_vars')
                _ ->
                  -- If we come across a non-existential If which can be said to
                  -- create a new array *and* which has one or more bodies which
                  -- can also be said to create a new array *in the same memory*
                  -- (i.e. has first memory uses), then we disable it.  This is
                  -- not at all an impossible case to handle, but such an If is
                  -- weird, since it would make more sense if it had existential
                  -- memory, so maybe something needs to be done somewhere else
                  -- in the compiler?  If this is naively enabled, we can get an
                  -- error because the sub-body results are first uses while the
                  -- main result is not.  This can be "fixed" by stating that
                  -- the If as a whole is also a first use of the memory, but
                  -- this seems too conservative.  FIXME.
                  forM_ (var : body_vars') $ \v -> recordActuals v S.empty

          _ -> return ()

    BasicOp (Index orig _) -> do
      let ielem = head patvalelems -- Should be okay.
          var = patElemName ielem
      case patElemAttr ielem of
        ExpMem.MemArray{} ->
          -- Disable merging for index expressions that return arrays.  Maybe
          -- too restrictive.  Make sure the source also updates the memory of
          -- the index when updated.  The array might be an aliasing operation,
          -- in which case we try to find the original array.
          aliasOpHandleVar orig var
        _ -> return ()

    -- Support reusing the memory of reshape operations by recording the origin
    -- array that is being reshaped.  Only partial support for reshape
    -- operations: If the shape is more than one-dimensional, mark the statement
    -- as disabled for memory merging operations.
    BasicOp (Reshape shapechange_var orig) ->
      forM_ (map patElemName patvalelems) $ \var -> do
        orig' <- aliasOpRoot' orig
        mem_orig <- M.lookup orig' <$> asks ctxVarToMem
        case (shapechange_var, mem_orig) of
          ([_], Just (MemorySrc _ _ (Shape [_]))) ->
            recordActuals var $ S.fromList [var, orig]
            -- Works, but only in limited cases where the reshape is not even
            -- that useful to begin with; mostly cases where a reshape was
            -- inserted by the compiler in an assert-like manner.
          _ ->
            recordActuals var S.empty
            -- FIXME: The problem with these more complex cases with more than
            -- one dimension is that a slice is relative to the shape of the
            -- reshaped array, and not the original array.  Disabled for now.
        recordActuals orig' $ S.fromList [orig', var]

    -- For the other aliasing operations, disable their use for now.  If the
    -- source has a change of memory block, make sure to change this as well.
    BasicOp (Rearrange _ orig) ->
      aliasOpHandle orig patvalelems

    BasicOp (Rotate _ orig) ->
      aliasOpHandle orig patvalelems

    BasicOp (Opaque (Var orig)) ->
      aliasOpHandle orig patvalelems

    _ -> forM_ patvalelems $ \(PatElem var _ membound) -> do
      let body_vars = S.toList $ findAllExpVars e
      case membound of
        ExpMem.MemArray _ _ _ (ExpMem.ArrayIn mem _) -> do
          body_vars' <- filterM (lookupGivesMem mem) body_vars
          recordActuals var $ S.fromList (var : body_vars')
        _ -> return ()

  -- If we are inside a kernel, check for actual variables in the KernelExp of
  -- the statement.
  lookInKernelExp stm

  -- Recurse over any sub-bodies.
  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody
          , walkOnFParam = lookInFParam
          , walkOnLParam = lookInLParam
          }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInLambda
          , walkOnKernelLParam = lookInLParam
          }

-- If we have a rotate or similar, we want to find the original array and
-- associate *that* with this aliasing array, so that changes to the original
-- array will affect this one as well.
aliasOpHandle :: VName -> [PatElem lore] -> FindM lore ()
aliasOpHandle orig patvalelems =
  forM_ (map patElemName patvalelems) $ aliasOpHandleVar orig

aliasOpHandleVar :: VName -> VName -> FindM lore ()
aliasOpHandleVar orig var = do
  recordActuals var S.empty
  orig' <- aliasOpRoot' orig
  recordActuals orig' $ S.fromList [orig', var]

aliasOpRoot :: VName -> FindM lore (Maybe VName)
aliasOpRoot orig = do
  current_actuals <- get
  return $ case S.null <$> M.lookup orig current_actuals of
    -- If the original array is itself an aliasing operation, find the *actual*
    -- original array.  There can be more than one reference.  We just pick the
    -- first one -- any one should do, since there is a transitive closure
    -- calculation later on.
    Just True -> case M.keys (M.filter (orig `S.member`) current_actuals) of
      orig' : _ -> Just orig'
      _ -> Nothing
    -- Else, just return orig.
    _ -> Just orig

aliasOpRoot' :: VName -> FindM lore VName
aliasOpRoot' orig =
  fromJust ("at some point there will have been a proper statement: "
            ++ pretty orig) <$> aliasOpRoot orig

-- Is the memory block of 'v' the same as 'mem'?
lookupGivesMem :: MName -> VName -> FindM lore Bool
lookupGivesMem mem v = do
  m <- M.lookup v <$> asks ctxVarToMem
  return (Just mem == (memSrcName <$> m))

class LookInKernelExp lore where
  -- Find actual vars in 'KernelExp's.
  lookInKernelExp :: Stm lore -> FindM lore ()

instance LookInKernelExp ExplicitMemory where
  lookInKernelExp (Let (Pattern _ patvalelems) _ e) = case e of
    Op (ExpMem.Inner (Kernel _ _ _ (KernelBody _ _ ress))) ->
      zipWithM_ (\(PatElem var _ _) res -> case res of
                    WriteReturn _ arr _ _ ->
                      recordActuals arr $ S.singleton var
                    _ -> return ()
                ) patvalelems ress
    _ -> return ()

instance LookInKernelExp InKernel where
  lookInKernelExp (Let _ _ e) = case e of
    Op (ExpMem.Inner ke) -> case ke of
      ExpMem.GroupReduce _ _ input -> do
        let arrs = map snd input
        extendActualVarsInKernel e arrs
      ExpMem.GroupScan _ _ input -> do
        let arrs = map snd input
        extendActualVarsInKernel e arrs
      ExpMem.GroupStream _ _ _ _ arrs ->
        extendActualVarsInKernel e arrs
      _ -> return ()
    _ -> return ()

-- Record actual variables for input arrays to 'KernelExp's.
extendActualVarsInKernel :: Exp InKernel -> [VName] -> FindM InKernel ()
extendActualVarsInKernel e arrs = forM_ arrs $ \var -> do
  -- The array might be an aliasing operation, in which case we try to find the
  -- original array.
  var' <- fromMaybe var <$> aliasOpRoot var
  varmem <- M.lookup var <$> asks ctxVarToMem
  case varmem of
    Just mem -> do
      let body_vars = findAllExpVars e
      body_vars' <- filterSetM (lookupGivesMem $ memSrcName mem) body_vars
      let actuals = S.insert var' body_vars'

      dbg_body_vars_mems <- mapM (\v -> M.lookup v <$> asks ctxVarToMem) (S.toList body_vars)
      let debug = do
            putStrLn $ replicate 70 '~'
            putStrLn "extendActualVarsInKernel:"
            putStrLn $ pretty var
            putStrLn $ pretty var'
            putStrLn $ prettySet body_vars
            print dbg_body_vars_mems
            putStrLn $ prettySet body_vars'
            putStrLn $ replicate 70 '~'
      withDebug debug $ recordActuals var' actuals
    Nothing -> return ()
