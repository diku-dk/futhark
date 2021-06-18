{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.MemBlockCoalesce.TopDownAn
       ( TopDnEnv(..), ScopeTab, InhibitTab
       , topdwnTravBinding, topDownLoop
       , getDirAliasedIxfn, addInvAliassesVarTab
       , areAnyAliased )
       where

import qualified Data.Map.Strict as M
import qualified Control.Exception.Base as Exc

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

type DirAlias = ExpMem.IxFun -> ExpMem.IxFun
type InvAlias = Maybe (ExpMem.IxFun -> ExpMem.IxFun)
type VarAliasTab = M.Map VName (VName, DirAlias, InvAlias)
type MemAliasTab = M.Map VName Names

data TopDnEnv = TopDnEnv { alloc   :: AllocTab
                         -- ^ contains the already allocated memory blocks
                         , scope   :: ScopeTab
                         -- ^ variable info, including var-to-memblock assocs
                         , inhibited :: InhibitTab
                         -- ^ the inherited inhibitions from the previous try
                         , v_alias :: VarAliasTab
                         -- ^ for statements such as transpose, reshape, index, etc., that alias
                         --   an array variable: maps var-names to pair of aliased var name
                         --   and index function transformation. For example, for
                         --   @let b = a[slc]@ it should add the binding
                         --   @ b |-> (a, `slice` slc )@
                         , m_alias :: MemAliasTab
                         -- ^ keeps track of memory block aliasing.
                         --   this needs to be implemented
                         }

getDirAliasFromExp :: Exp (Aliases ExpMem.SeqMem) -> Maybe (VName, ExpMem.IxFun -> ExpMem.IxFun)
getDirAliasFromExp (BasicOp (SubExp (Var x))) = Just (x, id)
getDirAliasFromExp (BasicOp (Opaque (Var x))) = Just (x, id)
getDirAliasFromExp (BasicOp (Reshape shp_chg x)) = Just (x, (`IxFun.reshape` (map (fmap ExpMem.pe64) shp_chg)))
getDirAliasFromExp (BasicOp (Rearrange perm x)) = Just (x, (`IxFun.permute` perm))
getDirAliasFromExp (BasicOp (Rotate rs x)) = Just (x, (`IxFun.rotate` (fmap ExpMem.pe64 rs)))
getDirAliasFromExp (BasicOp (Index x slc)) = Just (x, (`IxFun.slice` (map (fmap ExpMem.pe64) slc)))
getDirAliasFromExp _ = Nothing

-- | This was former @createsAliasedArrOK@ from DataStructs
--   While Rearrange and Rotate create aliased arrays, we
--   do not yet support them because it would mean we have
--   to "reverse" the index function, for example to support
--   coalescing in the case below,
--       @let a = map f a0   @
--       @let b = transpose a@
--       @let y[4] = copy(b) @
--   we would need to assign to @a@ as index function, the
--   inverse of the transpose, such that, when creating @b@
--   by transposition we get a directly-mapped array, which
--   is expected by the copying in y[4].
--   For the moment we support only transposition and VName-expressions,
--     but rotations and full slices could also be supported.
getInvAliasFromExp :: Exp (Aliases ExpMem.SeqMem) -> Maybe (VName, ExpMem.IxFun -> ExpMem.IxFun)
getInvAliasFromExp (BasicOp (SubExp (Var x))) = Just (x, id)
getInvAliasFromExp (BasicOp (Opaque (Var x))) = Just (x, id)
getInvAliasFromExp (BasicOp (Rearrange perm arr_nm)) =
  let perm' = IxFun.permuteInv perm [0..length perm - 1]
  in  Just (arr_nm, (`IxFun.permute` perm'))
getInvAliasFromExp _ = Nothing


-- | fills in the TopDnEnv table
topdwnTravBinding :: TopDnEnv -> Stm (Aliases ExpMem.SeqMem) -> TopDnEnv
topdwnTravBinding env (Let (Pattern [] [pe]) _ (Op (ExpMem.Alloc _ _)) ) =
  env { alloc = (alloc env) <> (oneName (patElemName pe)) }
topdwnTravBinding env stm@(Let (Pattern [] [pe]) _ e)
  | Just (x, ixfn) <- getDirAliasFromExp e =
    let ixfn_inv = case getInvAliasFromExp e of
                    Nothing -> Nothing
                    Just (_, xfn) -> Just xfn
        env' = env { v_alias = M.insert (patElemName pe) (x,ixfn,ixfn_inv) (v_alias env)}
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
    -- This is perhaps not ideal for many reasons, so Philip is gonna figure it out.
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

{--
getTransitiveAlias :: M.Map VName (VName, ExpMem.IxFun -> ExpMem.IxFun) ->
                      (M.Map VName Coalesced) -> VName -> (ExpMem.IxFun -> ExpMem.IxFun)
                   -> Maybe ExpMem.IxFun
getTransitiveAlias _ vtab b ixfn
  | Just (Coalesced _ (MemBlock _ _ _ b_indfun) _) <- M.lookup b vtab =
    Just (ixfn b_indfun)
getTransitiveAlias alias_tab vtab b ixfn
  | Just (x,alias_fn) <- M.lookup b alias_tab =
  -- ^ 1. case: @b = alias x@;
  --   we find a potential alias of @b@, named @(x,ixfn0)@, by lookup in @td_env@
  case M.lookup x vtab of
    Just (Coalesced _ (MemBlock _ _ _ x_indfun) _) ->
      -- ^ 2. if @x@ is in @vtab@, then we know the root index function
      --      and we can compose it with the remaining ixfuns to compute
      --      the index function of @b@
      Just ((ixfn . alias_fn) x_indfun)
    Nothing -> getTransitiveAlias alias_tab vtab x (ixfn . alias_fn)
getTransitiveAlias _ _ _ _ = Nothing
--}

getDirAliasedIxfn :: TopDnEnv -> CoalsTab -> VName -> Maybe (VName, VName, ExpMem.IxFun)
getDirAliasedIxfn td_env coals_tab x =
  case getScopeMemInfo x (scope td_env) of
    Just (MemBlock _ _ m_x orig_ixfun) ->
      case M.lookup m_x coals_tab of
        Nothing -> Just (x, m_x, orig_ixfun)
                    -- fine, this is not subject to a coalesced point
                    -- just return the original index function
        Just coal_etry ->
          case walkAliasTab (v_alias td_env) (vartab coal_etry) x of
            Nothing -> Nothing
            Just (m, ixf) -> Just (x, m, ixf)
    Nothing -> Nothing

walkAliasTab :: VarAliasTab -> M.Map VName Coalesced -> VName
             -> Maybe (VName, ExpMem.IxFun)
walkAliasTab _ vtab x
  | Just (Coalesced _ (MemBlock _ _ new_mem new_ixfun) _subs) <- M.lookup x vtab =
    Just (new_mem, new_ixfun) -- ^ @x@ is in @vartab@ together with its new ixfun
walkAliasTab alias_tab vtab x
  | Just (x0, alias0, _) <- M.lookup x alias_tab,
    Just (m, ixfn0) <- walkAliasTab alias_tab vtab x0 =
        -- ^ x = alias0 x0, the new ixfun of @x0@ is recorded in @vartab@ as ixfn0
        Just (m, alias0 ixfn0)
walkAliasTab _ _ _ = Exc.assert False Nothing

-- | We assume @x@ is in @vartab@ and we add the variables that @x@ aliases
--   for as long as possible following a chain of direct-aliasing operators,
--   i.e., without considering aliasing of if-then-else, loops, etc. For example:
--     @ x0 = if c then ... else ...@
--     @ x1 = rearrange r1 x0 @
--     @ x2 = reverse x1@
--     @ y[slc] = x2 @
--   We assume @vartab@ constains a binding for @x2@, and calling this function
--     with @x2@ as argument should also insert entries for @x1@ and @x0@ to
--     @vartab@, of course if their aliasing operations are invertible.
--   We assume inverting aliases has been performed by the top-down pass.
addInvAliassesVarTab :: TopDnEnv -> M.Map VName Coalesced -> VName
                     -> Maybe (M.Map VName Coalesced)
addInvAliassesVarTab td_env vtab x
  | Just (Coalesced _ (MemBlock _ _ m_y x_ixfun) fv_subs) <- M.lookup x vtab =
    case M.lookup x (v_alias td_env) of
      Nothing -> Just vtab
      Just (_, _, Nothing) -> Nothing -- can't invert ixfun, conservatively fail!
      Just (x0, _, Just inv_alias0) ->
        let x_ixfn0 = inv_alias0 x_ixfun
        in case getScopeMemInfo x0 (scope td_env) of
            Nothing -> Exc.assert False Nothing
            Just (MemBlock ptp shp _ _) ->
              let coal = Coalesced Trans (MemBlock ptp shp m_y x_ixfn0) fv_subs
                  vartab' = M.insert x0 coal vtab
              in  addInvAliassesVarTab td_env vartab' x0
addInvAliassesVarTab _ _ _ = Exc.assert False Nothing

areAliased :: TopDnEnv -> VName -> VName -> Bool
areAliased _ m_x m_y =
    -- ^ this is a dummy implementation
    m_x == m_y

areAnyAliased :: TopDnEnv -> VName -> [VName] -> Bool
areAnyAliased td_env m_x m_ys =
    any (areAliased td_env m_x) m_ys
