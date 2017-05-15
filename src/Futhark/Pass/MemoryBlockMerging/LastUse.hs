{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Playground for work on merging memory blocks
module Futhark.Pass.MemoryBlockMerging.LastUse
  ( lastUseFun
  , lastUsePrg
  ) where

import Prelude
import qualified Data.Map.Strict as M
import qualified Data.Set as S

--import Futhark.Representation.AST.Syntax

import Futhark.Representation.Aliases
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Pass.MemoryBlockMerging.DataStructs

-- | Last-Use analysis of a Futhark program in aliased explicit-memory lore form.
--   Takes as input such a program and produces a `M.Map VName [VName]`,
--   in which the key identifies the let stmt, and the list argument
--   identifies the variables that were lastly used in that stmt.
--   Note that the results of a body do not have a last use, and neither
--   do a function parameter if it happens to not be used inside function's body.
--   Such cases are supposed to be treated separately.
lastUsePrg :: Prog (Aliases ExpMem.ExplicitMemory) -> LUTabPrg
lastUsePrg prg = M.fromList $ map lastUseFun $ progFunctions prg

lastUseFun :: FunDef (Aliases ExpMem.ExplicitMemory) -> (Name,LUTabFun)
lastUseFun (FunDef _ fname _ _ body) =
  let (_,res,_) = lastUseAnBdy M.empty body (M.empty,S.empty)
  in  (fname, res)

-- | Performing the last-use analysis on a body. Arguments are:
--     (i) the aliasing table,
--    (ii) the body of statements,
--   (iii) the current last-use table, tupled with the known set of already used names.
--   The result is:
--     (i) the body's free variables (optimization),
--    (ii) an updated last-use table,
--   (iii) an updated set of used names (including the binding).
--   The implementation consists of a bottom-up traversal of the body's statements
--   in which the the variables lastly used in a statement are computed as the
--   difference between the free-variables in that stmt and the set of variables
--   known to be used after that statement.
lastUseAnBdy :: AliasTab -> Body (Aliases ExpMem.ExplicitMemory)
             -> (LUTabFun, Names) -> (Names, LUTabFun, Names)
lastUseAnBdy alstab bdy@(Body _ bnds result) (lutab,used_nms) =
  let res_nms   = getNamesFromSubExps result

      -- perform analysis bottom-up in bindings: results are known to be used,
      -- hence they are added to the used_nms set.
      (lutab',_) = traverseBindings alstab bnds (lutab, used_nms) res_nms

      -- Clean up the used names by recomputing the aliasing tenasitive-closure
      -- of the free names in body based on the current alias table @alstab@.
      free_in_body = freeInBody bdy
      maybe_used = S.foldl' (\acc x -> case M.lookup x alstab of
                                          Nothing -> acc
                                          Just al -> acc `S.union` al
                             ) free_in_body free_in_body

  in ( free_in_body, lutab', used_nms `S.union` maybe_used )

  where traverseBindings :: AliasTab -> [Stm (Aliases ExpMem.ExplicitMemory)] -> (LUTabFun, Names) -> [VName]
                                     -> (LUTabFun, Names)
        -- | after the last statement we place the aliasing transitive-closure
        --   of the results as used names
        traverseBindings stab [] (lutab1,nms) res_nms =
          let nms' = foldl (\acc x -> case M.lookup x stab of
                                        Nothing -> acc
                                        Just al -> acc `S.union` al
                           ) nms res_nms
          in  (lutab1,nms')
        traverseBindings stab (bd@(Let pat _ _):bds) (lutab1,nms) res_nms =
          let stab' = updateAliasing stab pat
              (lutab1',  nms' ) = traverseBindings stab'  bds (lutab1,nms) res_nms
              (lutab1'', nms'') = lastUseAnBnd     stab'  bd  (lutab1',nms')
          in  (lutab1'', nms'')


lastUseAnBnd :: AliasTab -> Stm (Aliases ExpMem.ExplicitMemory)
             -> (LUTabFun,Names) -> (LUTabFun,Names)
lastUseAnBnd alstab (Let pat _ e) (lutab,used_nms) =
      -- analyse the expression and get the
      --  (i)  a new last-use table (in case the @e@ contains bodies of stmts)
      -- (ii) the set of variables lastly used in the current binding.
      --(iii)  aliased transitive-closure of used names, and
  let (lutab', last_uses, used_nms') = lastUseAnExp alstab e used_nms

      -- filter-out the binded names from the set of used variables,
      -- since they go out of scope, and update the last-use table.
      patnms    = patternNames pat
      used_nms''= S.difference used_nms' $ S.fromList patnms
      lutab''   = M.union lutab' $
                  case patnms of
                    []   -> lutab
                    nm:_ -> if S.null last_uses then lutab
                            else M.insert nm last_uses lutab
--trace ("LU Pattern: "++(pretty nm)++" last use: "++pretty (S.toList last_uses)++" orig used nms: "++(pretty $ S.toList used_nms)) last_uses
      -- treating last use case

  in (lutab'', used_nms'')


--------------------------------
-- | Last-Use Analysis for an expression. Arguments are:
--   1. the aliasing symbol table,
--   2. the current last-use table + set of used names "after" this expression
--   Result: 1. an extra LUTab recording the last use for expression's inner bodies,
--           2. the set of last-used vars in the expression at this level,
--           3. the updated used names, now including expression's free vars.
lastUseAnExp :: AliasTab -> Exp (Aliases ExpMem.ExplicitMemory)
             -> Names -> (LUTabFun,Names,Names)

-- | For an if-then-else, we duplicate the last use at each body level,
--   meaning we record the last use of the outer statement, and also
--   the last use in the statement in the inner bodies.
lastUseAnExp alstab (If _ then_body else_body _) used_nms =
  let -- ignore the condition as it is a boolean scalar
      (free_in_body_then, then_lutab, then_used_nms) = lastUseAnBdy alstab then_body (M.empty,used_nms)
      (free_in_body_else, else_lutab, else_used_nms) = lastUseAnBdy alstab else_body (M.empty,used_nms)
      used_nms' = S.union then_used_nms else_used_nms
      (_, last_used_arrs) = lastUseAnVars alstab used_nms $
                            free_in_body_then `S.union` free_in_body_else
  in ( then_lutab `M.union` else_lutab, last_used_arrs, used_nms' )


-- | For a DoLoop:
--   1. For the moment we ignore circular aliasing between loop-variant
--        parameters due to results (being switched), as in
--        loop(a,b,c,d) = for i < n do (a,c,b,d)
--        This should be handled separately by aliasing analysis.
--   2. We ignore the scalar values in loop's form
--   3. Three cases depending on the last use of the loop-variant init symbol:
--      a) Common case is when the loop-variant initializer is lastly used there:
--          @let a2 = loop(a1 = a0^{lu}) = for i < n do@
--          @             ... a1^{lu} ... in res@
--          @let b = ... a2^{lu} ...@
--          In this case we have three last uses: one for @a0@, @a1@ and @a2@,
--            albeit all three vars are potentially aliased.
--            The last use of @a0@ is recorded with key @a1@, i.e., lastly used
--            in the def of @a1@. The last use of @a1@ is inside the loop body.
--          Identification: AlTrCl(a0) INTERSECTED ( AlTrCl(freeVarsLoopBody) UNION
--                                                   AlTrCl(usedVarsAfterLoop) ) = empty
--          Encoded: a1 |lu-> { ... AlTrCl(a0) ... }
--      b) @a0@ is lastly used inside the loop body. In this case @a1@ does not have
--           a last use inside the loop.
--         Identification: AlTrCl(a0) INTERSECTED AlTrCl(usedVarsAfterLoop) = empty
--                         AlTrCl(a0) INTERSECTED AlTrCl(freevarsloopbdy) = not empty
--         Encoded: a2 |lu-> { ... AlTrCl(a0) ... }
--      c) @a0@ is lastly used after the loop. In this case @a1@ does not have
--           last use inside the loop.
--         Identification: AlTrCl(a0) INTERSECTED AlTrCl(usedVarsAfterLoop) = not empty.
--         Encoded: @a0@ is not part of the last uses encoded in neither @a1@ nor @a2@.
lastUseAnExp alstab (DoLoop _ var_ses _ body) used_nms0 =
  let free_in_body = aliasTransClos alstab $ freeInBody body
      -- compute the alising transitive closure of initializers
      var_inis = map (\(fp,se) -> let fpnm = identName $ paramIdent fp
                                  in  case se of
                                        Constant _ -> (fpnm,S.empty)
                                        Var nm     -> case M.lookup nm alstab of
                                                        Nothing -> (fpnm,S.singleton nm)
                                                        Just al -> (fpnm,S.insert nm al)
                     ) var_ses
      var_inis_a = filter (\(_,nms) -> null $ S.intersection nms $
                                       S.union free_in_body used_nms0) var_inis

      -- To record last-uses inside the loop body, we call @lastUseAnBdy@ with used-names
      -- being:  (free_in_body - loop-variants-a) + used_nms0. As such we disable cases b)
      -- and c) to produce loop-variant last uses inside the loop, and also we prevent
      -- the free-loop-variables to having last uses inside the loop.
      free_in_body' = S.difference free_in_body $ S.fromList $ map fst var_inis_a
      used_nms = S.union used_nms0 free_in_body'
      (_, body_lutab, _) = lastUseAnBdy alstab body (M.empty,used_nms)

      -- add var_inis_a to the body_lutab, i.e., record the last-use of
      -- initializer in the corresponding loop variant.
      lutab_res = M.union body_lutab $ M.fromList var_inis_a

      -- the result used names are:
      fpar_nms = S.fromList $ map (identName . paramIdent . fst) var_ses
      used_nms' = S.difference free_in_body fpar_nms
      used_nms_res = S.union used_nms0 used_nms'

      -- the last-uses at loop-statement level are the loop free variables that
      -- do not belong to @used_nms0@; this includes the initializers of b), @lu_ini_b@
      lu_arrs = S.difference used_nms' used_nms0

  in  (lutab_res, lu_arrs, used_nms_res)

--lastUseAnExp alstab (Op (ExpMem.Inner (ExpMem.Kernel str cs ker_space tps ker_bdy))) used_nms = (M.empty, S.empty, used_nms)

-- | Default case: get the free vars in the expression and call
--    `lastUseAnVars` to conservatively handle theirs alias set.
lastUseAnExp alstab e used_nms =
  let free_in_e = freeInExp e
      (used_nms', lu_vars) = lastUseAnVars alstab used_nms free_in_e
  in (M.empty, lu_vars, used_nms')

{-
myFun :: AliasTab -> Exp (Aliases ExpMem.InKernel)
             -> Names -> (LUTabFun,Names,Names)
myFun alstab (Op (ExpMem.Inner (ExpMem.SplitArray _ _ _ _ _ _))) used_nms = (M.empty, S.empty, used_nms)
-}

------------------------------------------------------
-- | Analysis of n list of names requires conservative handling
--   of its alias set, i.e., a last use of a variable corresponds
--   to the last use of any of its aliases.
lastUseAnVars :: AliasTab -> Names -> Names -> (Names, Names)
lastUseAnVars alstab used_nms args =
  let -- a use of an argument x is also a use of any variable in x alias set
      -- so we update the alias-based transitive-closure of used names.
      args_nms = aliasTransClos alstab args
      -- if neither a variable x, nor any of its alias set have been used
      -- before, then it is a last use of both that variable and all other
      -- variables in its alias set
      lu_arrs = S.foldl' (\acc x ->
                             let x_alias = case M.lookup x alstab of
                                             Nothing -> S.singleton x
                                             Just al -> S.insert x al
                                 is_unused = null $ S.intersection used_nms x_alias
                             in  if is_unused then acc `S.union` x_alias else acc
                          ) S.empty args
  in  (used_nms `S.union` args_nms, lu_arrs)
