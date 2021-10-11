{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
--
-- Last-Use analysis of a Futhark program in aliased explicit-memory lore form.
-- Takes as input such a program or a function and produces a 'M.Map VName
-- Names', in which the key identified the let stmt, and the list argument
-- identifies the variables that were lastly used in that stmt.  Note that the
-- results of a body do not have a last use, and neither do a function
-- parameters if it happens to not be used inside function's body.  Such cases
-- are supposed to be treated separately.
--
-- This pass is different from 'Futhark.Analysis.LastUse' in that memory blocks
-- are used to alias arrays. For instance, an 'Update' will not result in a last
-- use of the array being updated, because the result lives in the same memory.
module Futhark.Optimise.ArrayShortCircuiting.LastUse (lastUseSeqMem, lastUsePrg, lastUsePrgGPU, lastUseGPUMem) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Sequence (Seq (..))
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import Futhark.IR.SeqMem
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Prelude

newtype LastUseReader rep = LastUseReader
  { onOp :: Op (Aliases rep) -> Names -> LastUseM rep (LUTabFun, Names, Names)
  }

type LastUseM rep a = StateT AliasTab (Reader (LastUseReader rep)) a

aliasLookup :: VName -> LastUseM rep Names
aliasLookup vname =
  gets $ fromMaybe mempty . M.lookup vname

-- | Perform last-use analysis on a 'Prog'
lastUsePrg :: Prog (Aliases SeqMem) -> LUTabPrg
lastUsePrg prg = M.fromList $ map lastUseSeqMem $ progFuns prg

-- | Perform last-use analysis on a 'Prog'
lastUsePrgGPU :: Prog (Aliases GPUMem) -> LUTabPrg
lastUsePrgGPU prg = M.fromList $ map lastUseGPUMem $ progFuns prg

-- | Perform last-use analysis on a 'FunDef'
lastUseSeqMem :: FunDef (Aliases SeqMem) -> (Name, LUTabFun)
lastUseSeqMem (FunDef _ _ fname _ _ body) =
  let (res, _) =
        runReader
          (evalStateT (lastUseBody body (mempty, mempty)) mempty)
          (LastUseReader lastUseSeqOp)
   in (fname, res)

-- | Perform last-use analysis on a 'FunDef'
lastUseGPUMem :: FunDef (Aliases GPUMem) -> (Name, LUTabFun)
lastUseGPUMem (FunDef _ _ fname _ _ body) =
  let (res, _) =
        runReader
          (evalStateT (lastUseBody body (mempty, mempty)) mempty)
          (LastUseReader lastUseGPUOp)
   in (fname, res)

-- | Performing the last-use analysis on a body.
--
-- The implementation consists of a bottom-up traversal of the body's statements
-- in which the the variables lastly used in a statement are computed as the
-- difference between the free-variables in that stmt and the set of variables
-- known to be used after that statement.
lastUseBody ::
  (ASTRep rep, FreeIn (OpWithAliases (Op rep))) =>
  -- | The body of statements
  Body (Aliases rep) ->
  -- | The current last-use table, tupled with the known set of already used names
  (LUTabFun, Names) ->
  -- | The result is:
  --      (i) an updated last-use table,
  --     (ii) an updated set of used names (including the binding).
  LastUseM rep (LUTabFun, Names)
lastUseBody bdy@(Body _ stms result) (lutab, used_nms) = do
  -- perform analysis bottom-up in bindings: results are known to be used,
  -- hence they are added to the used_nms set.
  (lutab', _) <-
    lastUseStms stms (lutab, used_nms) $
      namesToList $
        freeIn $ map resSubExp result
  -- Clean up the used names by recomputing the aliasing transitive-closure
  -- of the free names in body based on the current alias table @alstab@.
  used_in_body <- aliasTransitiveClosure $ freeIn bdy
  return (lutab', used_nms <> used_in_body)

-- | Performing the last-use analysis on a body.
--
-- The implementation consists of a bottom-up traversal of the body's statements
-- in which the the variables lastly used in a statement are computed as the
-- difference between the free-variables in that stmt and the set of variables
-- known to be used after that statement.
lastUseKernelBody ::
  (CanBeAliased (Op rep), ASTRep rep) =>
  -- | The body of statements
  KernelBody (Aliases rep) ->
  -- | The current last-use table, tupled with the known set of already used names
  (LUTabFun, Names) ->
  -- | The result is:
  --      (i) an updated last-use table,
  --     (ii) an updated set of used names (including the binding).
  LastUseM rep (LUTabFun, Names)
lastUseKernelBody bdy@(KernelBody _ stms result) (lutab, used_nms) = do
  -- perform analysis bottom-up in bindings: results are known to be used,
  -- hence they are added to the used_nms set.
  (lutab', _) <-
    lastUseStms stms (lutab, used_nms) $ namesToList $ freeIn result
  -- Clean up the used names by recomputing the aliasing transitive-closure
  -- of the free names in body based on the current alias table @alstab@.
  used_in_body <- aliasTransitiveClosure $ freeIn bdy
  return (lutab', used_nms <> used_in_body)

lastUseStms ::
  (ASTRep rep, FreeIn (OpWithAliases (Op rep))) =>
  Stms (Aliases rep) ->
  (LUTabFun, Names) ->
  [VName] ->
  LastUseM rep (LUTabFun, Names)
lastUseStms Empty (lutab, nms) res_nms = do
  aliases <- mconcat <$> mapM aliasLookup res_nms
  return (lutab, nms <> aliases)
lastUseStms (stm@(Let pat _ e) :<| stms) (lutab, nms) res_nms = do
  let extra_alias = case e of
        BasicOp (Update _ old _ _) -> oneName old
        BasicOp (FlatUpdate old _ _) -> oneName old
        _ -> mempty
  -- We build up aliases top-down
  updateAliasing extra_alias pat
  -- But compute last use bottom-up
  (lutab', nms') <- lastUseStms stms (lutab, nms) res_nms
  (lutab'', nms'') <- lastUseStm stm (lutab', nms')
  return (lutab'', nms'')

lastUseStm ::
  (ASTRep rep, FreeIn (OpWithAliases (Op rep))) =>
  Stm (Aliases rep) ->
  (LUTabFun, Names) ->
  LastUseM rep (LUTabFun, Names)
lastUseStm (Let pat _ e) (lutab, used_nms) =
  do
    -- analyse the expression and get the
    --  (i)  a new last-use table (in case the @e@ contains bodies of stmts)
    -- (ii) the set of variables lastly used in the current binding.
    --(iii)  aliased transitive-closure of used names, and
    (lutab', last_uses, used_nms') <- lastUseExp e used_nms
    -- filter-out the binded names from the set of used variables,
    -- since they go out of scope, and update the last-use table.
    let patnms = patNames pat
        used_nms'' = used_nms' `namesSubtract` namesFromList patnms
        lutab'' =
          M.union lutab' $ M.insert (head patnms) last_uses lutab
    return (lutab'', used_nms'')

--------------------------------

-- | Last-Use Analysis for an expression.
lastUseExp ::
  (ASTRep rep, FreeIn (OpWithAliases (Op rep))) =>
  -- | The expression to analyse
  Exp (Aliases rep) ->
  -- | The set of used names "after" this expression
  Names ->
  -- | Result:
  --    1. an extra LUTab recording the last use for expression's inner bodies,
  --    2. the set of last-used vars in the expression at this level,
  --    3. the updated used names, now including expression's free vars.
  LastUseM rep (LUTabFun, Names, Names)
lastUseExp (If _ then_body else_body _) used_nms = do
  -- For an if-then-else, we duplicate the last use at each body level, meaning
  -- we record the last use of the outer statement, and also the last use in the
  -- statement in the inner bodies. We can safely ignore the if-condition as it is
  -- a boolean scalar.
  (then_lutab, then_used_nms) <- lastUseBody then_body (M.empty, used_nms)
  (else_lutab, else_used_nms) <- lastUseBody else_body (M.empty, used_nms)
  let free_in_body_then = freeIn then_body
  let free_in_body_else = freeIn else_body
  let used_nms' = then_used_nms <> else_used_nms
  (_, last_used_arrs) <- lastUsedInNames used_nms $ free_in_body_then <> free_in_body_else
  return (then_lutab <> else_lutab, last_used_arrs, used_nms')
lastUseExp (DoLoop var_ses _ body) used_nms0 = do
  free_in_body <- aliasTransitiveClosure $ freeIn body
  -- compute the aliasing transitive closure of initializers that are not last-uses
  var_inis <- catMaybes <$> mapM (initHelper (free_in_body <> used_nms0)) var_ses
  let -- To record last-uses inside the loop body, we call 'lastUseBody' with used-names
      -- being:  (free_in_body - loop-variants-a) + used_nms0. As such we disable cases b)
      -- and c) to produce loop-variant last uses inside the loop, and also we prevent
      -- the free-loop-variables to having last uses inside the loop.
      free_in_body' = free_in_body `namesSubtract` namesFromList (map fst var_inis)
      used_nms = used_nms0 <> free_in_body'
  (body_lutab, _) <- lastUseBody body (mempty, used_nms)

  -- add var_inis_a to the body_lutab, i.e., record the last-use of
  -- initializer in the corresponding loop variant.
  let lutab_res = body_lutab <> M.fromList var_inis

      -- the result used names are:
      fpar_nms = namesFromList $ map (identName . paramIdent . fst) var_ses
      used_nms' = free_in_body `namesSubtract` fpar_nms
      used_nms_res = used_nms0 <> used_nms'

      -- the last-uses at loop-statement level are the loop free variables that
      -- do not belong to @used_nms0@; this includes the initializers of b), @lu_ini_b@
      lu_arrs = used_nms' `namesSubtract` used_nms0
  return (lutab_res, lu_arrs, used_nms_res)
  where
    initHelper free_and_used (fp, se) = do
      names <- aliasTransitiveClosure $ maybe mempty oneName $ subExpVar se
      if names `namesIntersect` free_and_used
        then return Nothing
        else return $ Just (identName $ paramIdent fp, names)
lastUseExp (Op op) used_nms = do
  on_op <- reader onOp
  on_op op used_nms
lastUseExp e used_nms = do
  let free_in_e = freeIn e
  (used_nms', lu_vars) <- lastUsedInNames used_nms free_in_e
  return (M.empty, lu_vars, used_nms')

lastUseGPUOp :: Op (Aliases GPUMem) -> Names -> LastUseM GPUMem (LUTabFun, Names, Names)
lastUseGPUOp (Alloc se sp) used_nms = do
  let free_in_e = freeIn se <> freeIn sp
  (used_nms', lu_vars) <- lastUsedInNames used_nms free_in_e
  return (M.empty, lu_vars, used_nms')
lastUseGPUOp (Inner (OtherOp ())) used_nms =
  return (mempty, mempty, used_nms)
lastUseGPUOp (Inner (SizeOp sop)) used_nms = do
  (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn sop
  return (mempty, lu_vars, used_nms')
lastUseGPUOp (Inner (SegOp (SegMap _ _ tps kbody))) used_nms = do
  (used_nms', lu_vars) <- lastUsedInNames used_nms $ freeIn tps
  (body_lutab, _) <- lastUseKernelBody kbody (mempty, used_nms')
  return (body_lutab, lu_vars, used_nms')
lastUseGPUOp (Inner (SegOp _)) _ =
  undefined

lastUseSeqOp :: Op (Aliases SeqMem) -> Names -> LastUseM SeqMem (LUTabFun, Names, Names)
lastUseSeqOp (Alloc se sp) used_nms = do
  let free_in_e = freeIn se <> freeIn sp
  (used_nms', lu_vars) <- lastUsedInNames used_nms free_in_e
  return (mempty, lu_vars, used_nms')
lastUseSeqOp (Inner ()) used_nms = do
  return (mempty, mempty, used_nms)

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
  return (used_nms <> new_uses_with_aliases, last_uses')
  where
    isLastUse x = do
      with_aliases <- aliasTransitiveClosure $ oneName x
      return $ not $ with_aliases `namesIntersect` used_nms

-- | Compute the transitive closure of the aliases of a set of 'Names'.
aliasTransitiveClosure :: Names -> LastUseM rep Names
aliasTransitiveClosure args = do
  res <- foldl (<>) args <$> mapM aliasLookup (namesToList args)
  if res == args
    then return res
    else aliasTransitiveClosure res

-- | For each 'PatElemT' in the 'PatT', add its aliases to the 'AliasTab' in
-- 'LastUseM'. Additionally, 'Names' are added as aliases of all the 'PatElemT'.
updateAliasing ::
  AliasesOf dec =>
  -- | Extra names that all 'PatElemT' should alias.
  Names ->
  -- | Pattern to process
  PatT dec ->
  LastUseM rep ()
updateAliasing extra_aliases =
  mapM_ update . patElems
  where
    update :: AliasesOf dec => PatElemT dec -> LastUseM rep ()
    update (PatElem name dec) = do
      let aliases = aliasesOf dec
      aliases' <- aliasTransitiveClosure $ extra_aliases <> aliases
      modify $ M.insert name aliases'
