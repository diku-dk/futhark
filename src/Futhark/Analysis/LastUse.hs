{-# LANGUAGE TypeFamilies #-}

-- | Last use analysis for array short circuiting
--
-- Last-Use analysis of a Futhark program in aliased explicit-memory lore form.
-- Takes as input such a program or a function and produces a 'M.Map VName
-- Names', in which the key identified the let stmt, and the list argument
-- identifies the variables that were lastly used in that stmt.  Note that the
-- results of a body do not have a last use, and neither do a function
-- parameters if it happens to not be used inside function's body.  Such cases
-- are supposed to be treated separately.
module Futhark.Analysis.LastUse
  ( lastUseSeqMem,
    lastUseGPUMem,
    lastUseMCMem,
    lastUseGPUNoMem,
    LUTabFun,
    LUTabProg,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence (Seq (..))
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.GPUMem qualified as GPU
import Futhark.IR.MCMem
import Futhark.IR.MCMem qualified as MC
import Futhark.IR.SeqMem
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Util

-- | Maps a name indentifying a Stm to the last uses in that Stm.
type LUTabFun = M.Map VName Names

-- | LU-table for the constants, and for each function.
type LUTabProg = (LUTabFun, M.Map Name LUTabFun)

type LastUseOp rep = Op (Aliases rep) -> Names -> LastUseM rep (LUTabFun, Names, Names)

-- | 'LastUseReader' allows us to abstract over representations by supplying the
-- 'onOp' function.
data LastUseReader rep = LastUseReader
  { onOp :: LastUseOp rep,
    scope :: Scope (Aliases rep)
  }

-- | Maps a variable or memory block to its aliases.
type AliasTab = M.Map VName Names

newtype LastUseM rep a = LastUseM (StateT AliasTab (Reader (LastUseReader rep)) a)
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader (LastUseReader rep),
      MonadState AliasTab
    )

instance (RepTypes (Aliases rep)) => HasScope (Aliases rep) (LastUseM rep) where
  askScope = asks scope

instance (RepTypes (Aliases rep)) => LocalScope (Aliases rep) (LastUseM rep) where
  localScope sc (LastUseM m) = LastUseM $ do
    local (\rd -> rd {scope = scope rd <> sc}) m

type Constraints rep =
  ( LocalScope (Aliases rep) (LastUseM rep),
    HasMemBlock (Aliases rep),
    AliasableRep rep
  )

runLastUseM :: LastUseOp rep -> LastUseM rep a -> a
runLastUseM onOp (LastUseM m) =
  runReader (evalStateT m mempty) (LastUseReader onOp mempty)

aliasLookup :: VName -> LastUseM rep Names
aliasLookup vname =
  gets $ fromMaybe mempty . M.lookup vname

lastUseProg ::
  (Constraints rep) =>
  Prog (Aliases rep) ->
  LastUseM rep LUTabProg
lastUseProg prog =
  let bound_in_consts =
        progConsts prog
          & concatMap (patNames . stmPat)
          & namesFromList
      consts = progConsts prog
      funs = progFuns prog
   in inScopeOf consts $ do
        (consts_lu, _) <- lastUseStms consts mempty mempty
        lus <- mapM (lastUseFun bound_in_consts) funs
        pure (consts_lu, M.fromList $ zip (map funDefName funs) lus)

lastUseFun ::
  (Constraints rep) =>
  Names ->
  FunDef (Aliases rep) ->
  LastUseM rep LUTabFun
lastUseFun bound_in_consts f =
  inScopeOf f $ fst <$> lastUseBody (funDefBody f) (mempty, bound_in_consts)

-- | Perform last-use analysis on a 'Prog' in 'SeqMem'
lastUseSeqMem :: Prog (Aliases SeqMem) -> LUTabProg
lastUseSeqMem = runLastUseM lastUseSeqOp . lastUseProg

-- | Perform last-use analysis on a 'Prog' in 'GPUMem'
lastUseGPUMem :: Prog (Aliases GPUMem) -> LUTabProg
lastUseGPUMem = runLastUseM (lastUseMemOp lastUseGPUOp) . lastUseProg

-- | Perform last-use analysis on a 'Prog' in 'MCMem'
lastUseMCMem :: Prog (Aliases MCMem) -> LUTabProg
lastUseMCMem = runLastUseM (lastUseMemOp lastUseMCOp) . lastUseProg

-- | Perform last-use analysis on a 'Prog' in 'GPU'
lastUseGPUNoMem :: Prog (Aliases GPU) -> LUTabProg
lastUseGPUNoMem = runLastUseM lastUseGPUNoMemOp . lastUseProg

-- | Performing the last-use analysis on a body.
--
-- The implementation consists of a bottom-up traversal of the body's statements
-- in which the the variables lastly used in a statement are computed as the
-- difference between the free-variables in that stmt and the set of variables
-- known to be used after that statement.
lastUseBody ::
  (Constraints rep) =>
  -- | The body of statements
  Body (Aliases rep) ->
  -- | The current last-use table, tupled with the known set of already used names
  (LUTabFun, Names) ->
  -- | The result is:
  --      (i) an updated last-use table,
  --     (ii) an updated set of used names (including the binding).
  LastUseM rep (LUTabFun, Names)
lastUseBody bdy@(Body _ stms result) (lutab, used_nms) =
  -- perform analysis bottom-up in bindings: results are known to be used,
  -- hence they are added to the used_nms set.
  inScopeOf stms $ do
    (lutab', _) <-
      lastUseStms stms (lutab, used_nms) $
        namesToList $
          freeIn $
            map resSubExp result
    -- Clean up the used names by recomputing the aliasing transitive-closure
    -- of the free names in body based on the current alias table @alstab@.
    used_in_body <- aliasTransitiveClosure $ freeIn bdy
    pure (lutab', used_nms <> used_in_body)

-- | Performing the last-use analysis on a body.
--
-- The implementation consists of a bottom-up traversal of the body's statements
-- in which the the variables lastly used in a statement are computed as the
-- difference between the free-variables in that stmt and the set of variables
-- known to be used after that statement.
lastUseKernelBody ::
  (Constraints rep) =>
  -- | The body of statements
  KernelBody (Aliases rep) ->
  -- | The current last-use table, tupled with the known set of already used names
  (LUTabFun, Names) ->
  -- | The result is:
  --      (i) an updated last-use table,
  --     (ii) an updated set of used names (including the binding).
  LastUseM rep (LUTabFun, Names)
lastUseKernelBody bdy@(KernelBody _ stms result) (lutab, used_nms) =
  inScopeOf stms $ do
    -- perform analysis bottom-up in bindings: results are known to be used,
    -- hence they are added to the used_nms set.
    (lutab', _) <-
      lastUseStms stms (lutab, used_nms) $ namesToList $ freeIn result
    -- Clean up the used names by recomputing the aliasing transitive-closure
    -- of the free names in body based on the current alias table @alstab@.
    used_in_body <- aliasTransitiveClosure $ freeIn bdy
    pure (lutab', used_nms <> used_in_body)

lastUseStms ::
  (Constraints rep) =>
  Stms (Aliases rep) ->
  (LUTabFun, Names) ->
  [VName] ->
  LastUseM rep (LUTabFun, Names)
lastUseStms Empty (lutab, nms) res_nms = do
  aliases <- concatMapM aliasLookup res_nms
  pure (lutab, nms <> aliases <> namesFromList res_nms)
lastUseStms (stm@(Let pat _ e) :<| stms) (lutab, nms) res_nms =
  inScopeOf stm $ do
    let extra_alias = case e of
          BasicOp (Update _ old _ _) -> oneName old
          BasicOp (FlatUpdate old _ _) -> oneName old
          _ -> mempty
    -- We build up aliases top-down
    updateAliasing extra_alias pat
    -- But compute last use bottom-up
    (lutab', nms') <- lastUseStms stms (lutab, nms) res_nms
    (lutab'', nms'') <- lastUseStm stm (lutab', nms')
    pure (lutab'', nms'')

lastUseStm ::
  (Constraints rep) =>
  Stm (Aliases rep) ->
  (LUTabFun, Names) ->
  LastUseM rep (LUTabFun, Names)
lastUseStm (Let pat _ e) (lutab, used_nms) = do
  -- analyse the expression and get the
  --  (i)  a new last-use table (in case the @e@ contains bodies of stmts)
  -- (ii) the set of variables lastly used in the current binding.
  -- (iii)  aliased transitive-closure of used names, and
  (lutab', last_uses, used_nms') <- lastUseExp e used_nms
  sc <- asks scope
  let lu_mems =
        namesToList last_uses
          & mapMaybe (`getScopeMemInfo` sc)
          & map memName
          & namesFromList
          & flip namesSubtract used_nms

  -- filter-out the binded names from the set of used variables,
  -- since they go out of scope, and update the last-use table.
  let patnms = patNames pat
      used_nms'' = used_nms' `namesSubtract` namesFromList patnms
      lutab'' =
        M.union lutab' $ M.insert (head patnms) (last_uses <> lu_mems) lutab
  pure (lutab'', used_nms'')

--------------------------------

-- | Last-Use Analysis for an expression.
lastUseExp ::
  (Constraints rep) =>
  -- | The expression to analyse
  Exp (Aliases rep) ->
  -- | The set of used names "after" this expression
  Names ->
  -- | Result:
  --    1. an extra LUTab recording the last use for expression's inner bodies,
  --    2. the set of last-used vars in the expression at this level,
  --    3. the updated used names, now including expression's free vars.
  LastUseM rep (LUTabFun, Names, Names)
lastUseExp (Match _ cases body _) used_nms = do
  -- For an if-then-else, we duplicate the last use at each body level, meaning
  -- we record the last use of the outer statement, and also the last use in the
  -- statement in the inner bodies. We can safely ignore the if-condition as it is
  -- a boolean scalar.
  (lutab_cases, used_cases) <-
    bimap mconcat mconcat . unzip
      <$> mapM (flip lastUseBody (M.empty, used_nms) . caseBody) cases
  (lutab', body_used_nms) <- lastUseBody body (M.empty, used_nms)
  let free_in_body = freeIn body
  let free_in_cases = freeIn cases
  let used_nms' = used_cases <> body_used_nms
  (_, last_used_arrs) <- lastUsedInNames used_nms $ free_in_body <> free_in_cases
  pure (lutab_cases <> lutab', last_used_arrs, used_nms')
lastUseExp (Loop var_ses form body) used_nms0 = localScope (scopeOfLoopForm form) $ do
  free_in_body <- aliasTransitiveClosure $ freeIn body
  -- compute the aliasing transitive closure of initializers that are not last-uses
  var_inis <- catMaybes <$> mapM (initHelper (free_in_body <> used_nms0)) var_ses
  let -- To record last-uses inside the loop body, we call 'lastUseBody' with used-names
      -- being:  (free_in_body - loop-variants-a) + used_nms0. As such we disable cases b)
      -- and c) to produce loop-variant last uses inside the loop, and also we prevent
      -- the free-loop-variables to having last uses inside the loop.
      free_in_body' = free_in_body `namesSubtract` namesFromList (map fst var_inis)
      used_nms = used_nms0 <> free_in_body' <> freeIn (bodyResult body)
  (body_lutab, _) <- lastUseBody body (mempty, used_nms)

  -- add var_inis_a to the body_lutab, i.e., record the last-use of
  -- initializer in the corresponding loop variant.
  let lutab_res = body_lutab <> M.fromList var_inis

      -- the result used names are:
      fpar_nms = namesFromList $ map (identName . paramIdent . fst) var_ses
      used_nms' = (free_in_body <> freeIn (map snd var_ses)) `namesSubtract` fpar_nms
      used_nms_res = used_nms0 <> used_nms' <> freeIn (bodyResult body)

      -- the last-uses at loop-statement level are the loop free variables that
      -- do not belong to @used_nms0@; this includes the initializers of b), @lu_ini_b@
      lu_arrs = used_nms' `namesSubtract` used_nms0
  pure (lutab_res, lu_arrs, used_nms_res)
  where
    initHelper free_and_used (fp, se) = do
      names <- aliasTransitiveClosure $ maybe mempty oneName $ subExpVar se
      if names `namesIntersect` free_and_used
        then pure Nothing
        else pure $ Just (identName $ paramIdent fp, names)
lastUseExp (Op op) used_nms = do
  on_op <- reader onOp
  on_op op used_nms
lastUseExp e used_nms = do
  let free_in_e = freeIn e
  (used_nms', lu_vars) <- lastUsedInNames used_nms free_in_e
  pure (M.empty, lu_vars, used_nms')

lastUseMemOp ::
  (inner (Aliases rep) -> Names -> LastUseM rep (LUTabFun, Names, Names)) ->
  MemOp inner (Aliases rep) ->
  Names ->
  LastUseM rep (LUTabFun, Names, Names)
lastUseMemOp _ (Alloc se sp) used_nms = do
  let free_in_e = freeIn se <> freeIn sp
  (used_nms', lu_vars) <- lastUsedInNames used_nms free_in_e
  pure (M.empty, lu_vars, used_nms')
lastUseMemOp onInner (Inner op) used_nms = onInner op used_nms

lastUseSegOp ::
  (Constraints rep) =>
  SegOp lvl (Aliases rep) ->
  Names ->
  LastUseM rep (LUTabFun, Names, Names)
lastUseSegOp (SegMap _ _ tps kbody) used_nms = do
  (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn tps
  (body_lutab, used_nms'') <- lastUseKernelBody kbody (mempty, used_nms')
  pure (body_lutab, lu_vars, used_nms' <> used_nms'')
lastUseSegOp (SegRed _ _ sbos tps kbody) used_nms = do
  (lutab_sbo, lu_vars_sbo, used_nms_sbo) <- lastUseSegBinOp sbos used_nms
  (used_nms', lu_vars) <- lastUsedInNames used_nms_sbo $ freeIn tps
  (body_lutab, used_nms'') <- lastUseKernelBody kbody (mempty, used_nms')
  pure (M.union lutab_sbo body_lutab, lu_vars <> lu_vars_sbo, used_nms_sbo <> used_nms' <> used_nms'')
lastUseSegOp (SegScan _ _ sbos tps kbody) used_nms = do
  (lutab_sbo, lu_vars_sbo, used_nms_sbo) <- lastUseSegBinOp sbos used_nms
  (used_nms', lu_vars) <- lastUsedInNames used_nms_sbo $ freeIn tps
  (body_lutab, used_nms'') <- lastUseKernelBody kbody (mempty, used_nms')
  pure (M.union lutab_sbo body_lutab, lu_vars <> lu_vars_sbo, used_nms_sbo <> used_nms' <> used_nms'')
lastUseSegOp (SegHist _ _ hos tps kbody) used_nms = do
  (lutab_sbo, lu_vars_sbo, used_nms_sbo) <- lastUseHistOp hos used_nms
  (used_nms', lu_vars) <- lastUsedInNames used_nms_sbo $ freeIn tps
  (body_lutab, used_nms'') <- lastUseKernelBody kbody (mempty, used_nms')
  pure (M.union lutab_sbo body_lutab, lu_vars <> lu_vars_sbo, used_nms_sbo <> used_nms' <> used_nms'')

lastUseGPUOp :: HostOp NoOp (Aliases GPUMem) -> Names -> LastUseM GPUMem (LUTabFun, Names, Names)
lastUseGPUOp (GPU.OtherOp NoOp) used_nms =
  pure (mempty, mempty, used_nms)
lastUseGPUOp (SizeOp sop) used_nms = do
  (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn sop
  pure (mempty, lu_vars, used_nms')
lastUseGPUOp (GPUBody tps body) used_nms = do
  (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn tps
  (body_lutab, used_nms'') <- lastUseBody body (mempty, used_nms')
  pure (body_lutab, lu_vars, used_nms' <> used_nms'')
lastUseGPUOp (SegOp op) used_nms =
  lastUseSegOp op used_nms

lastUseGPUNoMemOp :: HostOp SOAC (Aliases GPU) -> Names -> LastUseM GPU (LUTabFun, Names, Names)
lastUseGPUNoMemOp (GPU.OtherOp _) used_nms =
  pure (mempty, mempty, used_nms)
lastUseGPUNoMemOp (SizeOp sop) used_nms = do
  (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn sop
  pure (mempty, lu_vars, used_nms')
lastUseGPUNoMemOp (GPUBody tps body) used_nms = do
  (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn tps
  (body_lutab, used_nms'') <- lastUseBody body (mempty, used_nms')
  pure (body_lutab, lu_vars, used_nms' <> used_nms'')
lastUseGPUNoMemOp (SegOp op) used_nms =
  lastUseSegOp op used_nms

lastUseMCOp :: MCOp NoOp (Aliases MCMem) -> Names -> LastUseM MCMem (LUTabFun, Names, Names)
lastUseMCOp (MC.OtherOp NoOp) used_nms =
  pure (mempty, mempty, used_nms)
lastUseMCOp (MC.ParOp par_op op) used_nms = do
  (lutab_par_op, lu_vars_par_op, used_names_par_op) <-
    maybe (pure mempty) (`lastUseSegOp` used_nms) par_op
  (lutab_op, lu_vars_op, used_names_op) <-
    lastUseSegOp op used_nms
  pure
    ( lutab_par_op <> lutab_op,
      lu_vars_par_op <> lu_vars_op,
      used_names_par_op <> used_names_op
    )

lastUseSegBinOp ::
  (Constraints rep) =>
  [SegBinOp (Aliases rep)] ->
  Names ->
  LastUseM rep (LUTabFun, Names, Names)
lastUseSegBinOp sbos used_nms = do
  (lutab, lu_vars, used_nms') <- unzip3 <$> mapM helper sbos
  pure (mconcat lutab, mconcat lu_vars, mconcat used_nms')
  where
    helper (SegBinOp _ l@(Lambda _ _ body) neutral shp) = inScopeOf l $ do
      (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn neutral <> freeIn shp
      (body_lutab, used_nms'') <- lastUseBody body (mempty, used_nms')
      pure (body_lutab, lu_vars, used_nms'')

lastUseHistOp ::
  (Constraints rep) =>
  [HistOp (Aliases rep)] ->
  Names ->
  LastUseM rep (LUTabFun, Names, Names)
lastUseHistOp hos used_nms = do
  (lutab, lu_vars, used_nms') <- unzip3 <$> mapM helper hos
  pure (mconcat lutab, mconcat lu_vars, mconcat used_nms')
  where
    helper (HistOp shp rf dest neutral shp' l@(Lambda _ _ body)) = inScopeOf l $ do
      (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn shp <> freeIn rf <> freeIn dest <> freeIn neutral <> freeIn shp'
      (body_lutab, used_nms'') <- lastUseBody body (mempty, used_nms')
      pure (body_lutab, lu_vars, used_nms'')

lastUseSeqOp :: Op (Aliases SeqMem) -> Names -> LastUseM SeqMem (LUTabFun, Names, Names)
lastUseSeqOp (Alloc se sp) used_nms = do
  let free_in_e = freeIn se <> freeIn sp
  (used_nms', lu_vars) <- lastUsedInNames used_nms free_in_e
  pure (mempty, lu_vars, used_nms')
lastUseSeqOp (Inner NoOp) used_nms = do
  pure (mempty, mempty, used_nms)

------------------------------------------------------

-- | Given already used names and newly encountered 'Names', return an updated
-- set used names and the set of names that were last used here.
--
-- For a given name @x@ in the new uses, if neither @x@ nor any of its aliases
-- are present in the set of used names, this is a last use of @x@.
lastUsedInNames ::
  -- | Used names
  Names ->
  -- | New uses
  Names ->
  LastUseM rep (Names, Names)
lastUsedInNames used_nms new_uses = do
  -- a use of an argument x is also a use of any variable in x alias set
  -- so we update the alias-based transitive-closure of used names.
  new_uses_with_aliases <- aliasTransitiveClosure new_uses
  -- if neither a variable x, nor any of its alias set have been used before (in
  -- the backward traversal), then it is a last use of both that variable and
  -- all other variables in its alias set
  last_uses <- filterM isLastUse $ namesToList new_uses
  last_uses' <- aliasTransitiveClosure $ namesFromList last_uses
  pure (used_nms <> new_uses_with_aliases, last_uses')
  where
    isLastUse x = do
      with_aliases <- aliasTransitiveClosure $ oneName x
      pure $ not $ with_aliases `namesIntersect` used_nms

-- | Compute the transitive closure of the aliases of a set of 'Names'.
aliasTransitiveClosure :: Names -> LastUseM rep Names
aliasTransitiveClosure args = do
  res <- foldl (<>) args <$> mapM aliasLookup (namesToList args)
  if res == args
    then pure res
    else aliasTransitiveClosure res

-- | For each 'PatElem' in the 'Pat', add its aliases to the 'AliasTab' in
-- 'LastUseM'. Additionally, 'Names' are added as aliases of all the 'PatElemT'.
updateAliasing ::
  (AliasesOf dec) =>
  -- | Extra names that all 'PatElem' should alias.
  Names ->
  -- | Pattern to process
  Pat dec ->
  LastUseM rep ()
updateAliasing extra_aliases =
  mapM_ update . patElems
  where
    update :: (AliasesOf dec) => PatElem dec -> LastUseM rep ()
    update (PatElem name dec) = do
      let aliases = aliasesOf dec
      aliases' <- aliasTransitiveClosure $ extra_aliases <> aliases
      modify $ M.insert name aliases'
