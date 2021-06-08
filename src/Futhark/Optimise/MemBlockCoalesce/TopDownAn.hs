{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.MemBlockCoalesce.TopDownAn
       ( TopDnEnv(..), ScopeTab, InhibitTab
       , topdwnTravBinding, topDownLoop )
       where

import qualified Data.Map.Strict as M

import Futhark.IR.Aliases
import qualified Futhark.IR.SeqMem as ExpMem
--import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Optimise.MemBlockCoalesce.DataStructs
import qualified Futhark.IR.Mem.IxFun as IxFun


type ScopeTab = Scope (Aliases ExpMem.SeqMem)
-- ^ maps array-variable names to various info, including
--   types, memory block and index function, etc.

type InhibitTab = M.Map VName Names
-- ^ inhibited memory-block mergings from the key (memory block)
--   to the value (set of memory blocks)

data TopDnEnv = TopDnEnv { alloc   :: AllocTab
                         -- ^ contains the already allocated memory blocks
                         , scope   :: ScopeTab
                         -- ^ variable info, including var-to-memblock assocs
                         , inhibited :: InhibitTab
                         -- ^ the inherited inhibitions from the previous try
                         , v_alias :: M.Map VName (VName, ExpMem.IxFun -> ExpMem.IxFun)
                         -- ^ for statements such as transpose, reshape, index, etc., that alias
                         --   an array variable: maps var-names to pair of aliased var name
                         --   and index function transformation. For example, for
                         --   @let b = a[slc]@ it should add the binding
                         --   @ b |-> (a, `slice` slc )@
                         , m_alias :: M.Map VName Names
                         -- ^ keeps track of memory block aliasing.
                         --   this needs to be implemented
                         }

getAliasFromExp :: Exp (Aliases ExpMem.SeqMem) -> Maybe (VName, ExpMem.IxFun -> ExpMem.IxFun)
getAliasFromExp (BasicOp (SubExp (Var x))) = Just (x, id)
getAliasFromExp (BasicOp (Opaque (Var x))) = Just (x, id)
getAliasFromExp (BasicOp (Reshape shp_chg x)) = Just (x, (`IxFun.reshape` (map (fmap ExpMem.pe64) shp_chg)))
getAliasFromExp (BasicOp (Rearrange perm x)) = Just (x, (`IxFun.permute` perm))
getAliasFromExp (BasicOp (Rotate rs x)) = Just (x, (`IxFun.rotate` (fmap ExpMem.pe64 rs)))
getAliasFromExp (BasicOp (Index x slc)) = Just (x, (`IxFun.slice` (map (fmap ExpMem.pe64) slc)))
getAliasFromExp _ = Nothing


-- | fills in the TopDnEnv table
topdwnTravBinding :: TopDnEnv -> Stm (Aliases ExpMem.SeqMem) -> TopDnEnv
topdwnTravBinding env (Let (Pattern [] [pe]) _ (Op (ExpMem.Alloc _ _)) ) =
  env { alloc = (alloc env) <> (oneName (patElemName pe)) }
topdwnTravBinding env stm@(Let (Pattern [] [pe]) _ e)
  | Just (x, ixfn) <- getAliasFromExp e =
    let env' = env { v_alias = M.insert (patElemName pe) (x,ixfn) (v_alias env)}
    in  env' { scope = scope env <> scopeOf stm }
topdwnTravBinding env stm =
  -- ToDo: remember to update scope info appropriately
  --       for compound statements such as if, do-loop, etc.
  env { scope = scope env <> scopeOf stm }


topDownLoop :: TopDnEnv -> Stm (Aliases ExpMem.SeqMem) -> TopDnEnv
topDownLoop td_env (Let pat _ (DoLoop arginis_ctx arginis lform body)) =
  let scopetab = scope td_env                              <>
                 (scopeOfFParams (fst $ unzip arginis_ctx) <>
                 (scopeOfFParams (fst $ unzip arginis    ) <>
                 scopeOf lform )) --scopeOfLoopForm lform))
      scopetab_loop = scopetab <> scopeOf (bodyStms body)
      bdy_ress = drop (length arginis_ctx) $ bodyResult body
      m_alias' = foldl (foldfun scopetab_loop) (m_alias td_env) $
                  zip3 (patternValueElements pat) arginis bdy_ress
  in  td_env { scope = scopetab, m_alias = m_alias' }
  where
    updateAlias (m, m_al) tab =
      let m_al' = case M.lookup m tab of
                    Just m_al0 -> m_al <> m_al0
                    Nothing    -> m_al
      in  if m_al == mempty
          then tab
          else M.insert m m_al' tab
    --
    foldfun scopetab m_tab (p_el, (f_el, i_se), r_se)
      | (ExpMem.MemArray _ptp _shp _u (ExpMem.ArrayIn m_p _p_ixfn)) <- snd (patElemDec p_el),
        Var f0 <- i_se,
        Var r  <- r_se =
        let f = paramName f_el
        in case ( getScopeMemInfo f  scopetab
                , getScopeMemInfo f0 scopetab
                , getScopeMemInfo r  scopetab ) of
            ((Just (MemBlock _ _ m_f _), Just (MemBlock _ _ m_f0 _), Just (MemBlock _ _ m_r _))) ->
                -- ToDo: this does not support the double buffering case such as:
                --       @loop (a,b) = (a0,b0) for i<n do@
                --       @    let x = map (stencil a) (iota n)@
                --       @    let b[0:n] = x@
                --       @    in  (b,a)@
                --   where we denoted by @n@ the legth of @a@ and @b@
                --   The idea is to observe that if the memory blocks of @a0@ and @b0@
                --     are not aliased, then the memory blocks of @a@ and @b@ are not
                --     aliased either, because we always switch them in the result.
                --   Obviously the current implementation does not supports that because
                --     it uses a simple fold and does not check for these kind of inversion
                --     patterns. Obviously, this can be generalized to a klick of size > 2.
                let m_alias_p = oneName m_f0 <> oneName m_f <> oneName m_r
                    m_alias_f = oneName m_f0 <> oneName m_r
                    m_alias_r = oneName m_f0 <> oneName m_f
                    m_alias'  = updateAlias (m_p, m_alias_p) $
                                updateAlias (m_f, m_alias_f) $
                                updateAlias (m_r, m_alias_r) m_tab
                in  m_alias'
            (_, _, _) -> error "Impossible case reached in file MemBlockCoalesce.TopDownAn.hs, fun topDownLoop!"
    foldfun _ m_tab _ = m_tab
topDownLoop _ _ =
  error "MemBlockCoalesce.TopDownAn: function topDownLoop should only be called on Loops!"

{--
topDownIf :: TopDnEnv -> CoalsTab -> Stm (Aliases ExpMem.SeqMem) -> TopDnEnv
topDownIf td_env act_tab stm@(Let patt _ (If _ body_then body_else _)) =
  -- dummy implementation, please fill in
  td_env
--}
