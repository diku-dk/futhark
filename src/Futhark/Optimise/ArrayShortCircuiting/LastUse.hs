{-# LANGUAGE FlexibleContexts #-}
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
module Futhark.Optimise.ArrayShortCircuiting.LastUse (lastUseFun, lastUsePrg) where

import qualified Data.Map.Strict as M
import Futhark.IR.Aliases
import qualified Futhark.IR.SeqMem as ExpMem
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Prelude

-- | Perform last-use analysis on a 'Prog'
lastUsePrg :: Prog (Aliases ExpMem.SeqMem) -> LUTabPrg
lastUsePrg prg = M.fromList $ map lastUseFun $ progFuns prg

-- | Perform last-use analysis on a 'FunDef'
lastUseFun :: FunDef (Aliases ExpMem.SeqMem) -> (Name, LUTabFun)
lastUseFun (FunDef _ _ fname _ _ body) =
  let (_, res, _) = lastUseAnBdy M.empty body (M.empty, mempty)
   in (fname, res)

-- | Performing the last-use analysis on a body.
--
-- The implementation consists of a bottom-up traversal of the body's statements
-- in which the the variables lastly used in a statement are computed as the
-- difference between the free-variables in that stmt and the set of variables
-- known to be used after that statement.
lastUseAnBdy ::
  -- | The aliasing table
  AliasTab ->
  -- | The body of statements
  Body (Aliases ExpMem.SeqMem) ->
  -- | The current last-use table, tupled with the known set of already used names
  (LUTabFun, Names) ->
  -- | The result is:
  --      (i) the body's free variables (optimization),
  --     (ii) an updated last-use table,
  --    (iii) an updated set of used names (including the binding).
  (Names, LUTabFun, Names)
lastUseAnBdy alstab bdy@(Body _ bnds result) (lutab, used_nms) =
  -- perform analysis bottom-up in bindings: results are known to be used,
  -- hence they are added to the used_nms set.
  let (lutab', _) =
        traverseBindings alstab (stmsToList bnds) (lutab, used_nms) $
          namesToList $
            freeIn $ map resSubExp result
      -- Clean up the used names by recomputing the aliasing transitive-closure
      -- of the free names in body based on the current alias table @alstab@.
      free_in_body = freeIn bdy
      maybe_used = foldl mkUnion free_in_body $ map (`M.lookup` alstab) $ namesToList free_in_body
   in (free_in_body, lutab', used_nms <> maybe_used)
  where
    mkUnion acc Nothing = acc
    mkUnion acc (Just al) = acc <> al
    traverseBindings ::
      AliasTab ->
      [Stm (Aliases ExpMem.SeqMem)] ->
      (LUTabFun, Names) ->
      [VName] ->
      (LUTabFun, Names)
    traverseBindings stab [] (lutab1, nms) res_nms =
      (lutab1, foldl mkUnion nms $ map (`M.lookup` stab) res_nms)
    traverseBindings stab (bd@(Let pat _ e) : bds) (lutab1, nms) res_nms =
      let m_v = case e of
            BasicOp (Update _ old _ _) -> Just old
            BasicOp (FlatUpdate old _ _) -> Just old
            _ -> Nothing
          stab' = updateAliasing stab pat m_v
          (lutab1', nms') = traverseBindings stab' bds (lutab1, nms) res_nms
          (lutab1'', nms'') = lastUseAnBnd stab' bd (lutab1', nms')
       in (lutab1'', nms'')

lastUseAnBnd ::
  AliasTab ->
  Stm (Aliases ExpMem.SeqMem) ->
  (LUTabFun, Names) ->
  (LUTabFun, Names)
lastUseAnBnd alstab (Let pat _ e) (lutab, used_nms) =
  -- analyse the expression and get the
  --  (i)  a new last-use table (in case the @e@ contains bodies of stmts)
  -- (ii) the set of variables lastly used in the current binding.
  --(iii)  aliased transitive-closure of used names, and
  let (lutab', last_uses, used_nms') = lastUseAnExp alstab e used_nms
      -- filter-out the binded names from the set of used variables,
      -- since they go out of scope, and update the last-use table.
      patnms = patNames pat
      used_nms'' = namesSubtract used_nms' $ namesFromList patnms
      lutab'' = M.union lutab' $
        case patnms of
          [] -> lutab
          nm : _ ->
            if last_uses == mempty
              then lutab
              else M.insert nm last_uses lutab
   in --trace ("LU Pattern: "++(pretty nm)++" last use: "++pretty (S.toList last_uses)++" orig used nms: "++(pretty $ S.toList used_nms)) last_uses
      -- treating last use case
      (lutab'', used_nms'')

--------------------------------

-- | Last-Use Analysis for an expression. Arguments are:
--   1. the aliasing symbol table,
--   2. the current last-use table + set of used names "after" this expression
--   Result: 1. an extra LUTab recording the last use for expression's inner bodies,
--           2. the set of last-used vars in the expression at this level,
--           3. the updated used names, now including expression's free vars.
lastUseAnExp ::
  AliasTab ->
  Exp (Aliases ExpMem.SeqMem) ->
  Names ->
  (LUTabFun, Names, Names)

-- | For an if-then-else, we duplicate the last use at each body level,
--   meaning we record the last use of the outer statement, and also
--   the last use in the statement in the inner bodies.
lastUseAnExp alstab (If _ then_body else_body _) used_nms =
  let -- ignore the condition as it is a boolean scalar
      (free_in_body_then, then_lutab, then_used_nms) = lastUseAnBdy alstab then_body (M.empty, used_nms)
      (free_in_body_else, else_lutab, else_used_nms) = lastUseAnBdy alstab else_body (M.empty, used_nms)
      used_nms' = then_used_nms <> else_used_nms
      (_, last_used_arrs) =
        lastUseAnVars alstab used_nms $
          free_in_body_then <> free_in_body_else
   in (then_lutab `M.union` else_lutab, last_used_arrs, used_nms')
lastUseAnExp alstab (DoLoop var_ses _ body) used_nms0 =
  let free_in_body = aliasTransitiveClosure alstab $ freeIn body
      -- compute the alising transitive closure of initializers
      var_inis = map transClosInis var_ses
      free_and_used = free_in_body <> used_nms0
      var_inis_a = filter (\(_, nms) -> mempty == namesIntersection nms free_and_used) var_inis

      -- To record last-uses inside the loop body, we call @lastUseAnBdy@ with used-names
      -- being:  (free_in_body - loop-variants-a) + used_nms0. As such we disable cases b)
      -- and c) to produce loop-variant last uses inside the loop, and also we prevent
      -- the free-loop-variables to having last uses inside the loop.
      free_in_body' = namesSubtract free_in_body $ namesFromList $ map fst var_inis_a
      used_nms = used_nms0 <> free_in_body'
      (_, body_lutab, _) = lastUseAnBdy alstab body (M.empty, used_nms)

      -- add var_inis_a to the body_lutab, i.e., record the last-use of
      -- initializer in the corresponding loop variant.
      lutab_res = M.union body_lutab $ M.fromList var_inis_a

      -- the result used names are:
      fpar_nms = namesFromList $ map (identName . paramIdent . fst) var_ses
      used_nms' = namesSubtract free_in_body fpar_nms
      used_nms_res = used_nms0 <> used_nms'

      -- the last-uses at loop-statement level are the loop free variables that
      -- do not belong to @used_nms0@; this includes the initializers of b), @lu_ini_b@
      lu_arrs = namesSubtract used_nms' used_nms0
   in (lutab_res, lu_arrs, used_nms_res)
  where
    transClosInis (fp, Constant _) = (identName $ paramIdent fp, mempty)
    transClosInis (fp, Var nm) =
      let fpnm = identName $ paramIdent fp
       in case M.lookup nm alstab of
            Nothing -> (fpnm, oneName nm)
            Just al -> (fpnm, al <> oneName nm)
--lastUseAnExp alstab e@(Op{}) _ =
--  error $ "In LastUse.hs, function lastUseAnExp, Op expressions are not handled! Input expression is: "++pretty e

lastUseAnExp alstab e used_nms =
  let free_in_e = freeIn e
      (used_nms', lu_vars) = lastUseAnVars alstab used_nms free_in_e
   in (M.empty, lu_vars, used_nms')

------------------------------------------------------

-- | Analysis of a list of names requires conservative handling
--   of its alias set, i.e., a last use of a variable corresponds
--   to the last use of any of its aliases.
lastUseAnVars :: AliasTab -> Names -> Names -> (Names, Names)
lastUseAnVars alstab used_nms args =
  let -- a use of an argument x is also a use of any variable in x alias set
      -- so we update the alias-based transitive-closure of used names.
      args_nms = aliasTransitiveClosure alstab args
      -- if neither a variable x, nor any of its alias set have been used
      -- before (in the backward traversal), then it is a last use of both
      --  that variable and all other variables in its alias set
      lu_arrs = foldl (<>) mempty $ map luSet $ namesToList args
   in (used_nms <> args_nms, lu_arrs)
  where
    luSet x =
      let x_alias = case M.lookup x alstab of
            Nothing -> oneName x
            Just al -> al <> oneName x
       in if mempty == namesIntersection used_nms x_alias --x not previously used
            then x_alias
            else mempty
