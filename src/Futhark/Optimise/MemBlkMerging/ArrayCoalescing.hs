{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Playground for work on merging memory blocks
module Futhark.Optimise.MemBlkMerging.ArrayCoalescing
       ( mkCoalsTab )
       where

import Prelude
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Control.Exception.Base as Exc
import Debug.Trace

import Futhark.Representation.AST.Attributes.Scope
import Futhark.Analysis.PrimExp.Convert
import Futhark.Representation.Aliases
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Optimise.MemBlkMerging.DataStructs
import Futhark.Optimise.MemBlkMerging.LastUse

data TopDnEnv = TopDnEnv { alloc :: AllocTab
                         -- ^ contains the already allocated memory blocks
                         , v2mem :: V2MemTab
                         -- ^ contains var-to-memblock associations
                         , scope :: Scope (Aliases ExpMem.ExplicitMemory)
                         -- ^ info about variables
                         }

data BotUpEnv = BotUpEnv { scals :: ScalarTab
                         -- ^ records variable names to memory block mapping;
                         --   a var might map to several mem blocks due to aliasing???
                         , activeCoals :: CoalsTab
                         -- ^ optimistic coalescing info
                         , successCoals :: CoalsTab
                         -- ^ committed (successfull) coalescing info
                         }

emptyTopDnEnv :: TopDnEnv
emptyTopDnEnv = TopDnEnv { alloc = HS.empty, v2mem = HM.empty, scope = HM.empty }

emptyBotUpEnv :: BotUpEnv
emptyBotUpEnv = BotUpEnv { scals = HM.empty, activeCoals = HM.empty, successCoals = HM.empty }


-- | basic conversion of a Var Expression to a PrimExp
basePMconv :: Scope (Aliases ExpMem.ExplicitMemory)
           -> ScalarTab
           -> Exp (Aliases ExpMem.ExplicitMemory)
           -> Maybe (ExpMem.PrimExp VName)
basePMconv scopetab scaltab (BasicOp (SubExp (Var v))) =
  case HM.lookup v scaltab of
    Just pexp -> Just pexp
    Nothing -> case HM.lookup v scopetab of
                Just info ->
                    case typeOf info of
                      Prim tp -> Just $ ExpMem.LeafExp v tp
                      _ -> Nothing
                _ -> Nothing
basePMconv _ _ _ = Nothing

mkCoalsTab :: Prog (Aliases ExpMem.ExplicitMemory) -> CoalsTab
mkCoalsTab prg = foldl HM.union HM.empty $ map mkCoalsTabFun $ progFunctions prg

mkCoalsTabFun :: FunDef (Aliases ExpMem.ExplicitMemory) -> CoalsTab
mkCoalsTabFun fun@(FunDef _ _ _ fpars body) =
  let (_, lutab) = lastUseFun fun
      unique_mems = HS.fromList $ HM.keys $ getUniqueMemFParam fpars
      topenv = emptyTopDnEnv { scope = scopeOfFParams fpars
                             , alloc = unique_mems
                             , v2mem = HM.fromList $ getArrMemAssocFParam fpars
                             }
      succ_tab = successCoals $ mkCoalsTabBdy lutab body topenv emptyBotUpEnv
  in  fixPointFilterDeps succ_tab
  where fixPointFilterDeps :: CoalsTab -> CoalsTab
        fixPointFilterDeps tab =
          let tab' = foldl(\acc mb ->
                            case HM.lookup mb tab of
                              Nothing -> acc
                              Just (CoalsEntry _ _ _ _ opts) ->
                                if all (`HM.member` acc) $ HS.toList opts
                                then acc else HM.delete mb acc
                          ) tab (HM.keys tab)
          in  if HM.keys tab == HM.keys tab' then tab'
              else fixPointFilterDeps tab'

mkCoalsTabBdy :: LUTabFun -> Body (Aliases ExpMem.ExplicitMemory)
              -> TopDnEnv -> BotUpEnv -> BotUpEnv
mkCoalsTabBdy lutab (Body _ bnds _) =
  traverseBindings bnds
  where
    traverseBindings :: [Stm (Aliases ExpMem.ExplicitMemory)]
                     -> TopDnEnv -> BotUpEnv -> BotUpEnv
    traverseBindings [] _ bu_env = bu_env
    traverseBindings (bd:bds) td_env bu_env =
      let td_env' = topdwnTravBinding bd td_env
          bu_env' = traverseBindings bds td_env' bu_env
          res_env = mkCoalsTabBnd lutab bd td_env' bu_env'
      in  res_env
    topdwnTravBinding :: Stm (Aliases ExpMem.ExplicitMemory)
                      -> TopDnEnv -> TopDnEnv
    topdwnTravBinding (Let (Pattern [] [pe]) _ (Op (ExpMem.Alloc _ _)) ) env =
        env { alloc = HS.insert (patElemName pe) (alloc env)}
    topdwnTravBinding stm@(Let pat _ _) env =
      -- ToDo: remember to update scope and v2mem info appropriately
      --       for compound statements such as if, do-loop, etc.
      let v2mem' = HM.union (v2mem env) $ HM.fromList $
                   map (\(x,y,_)->(x,y)) $ getArrMemAssoc pat
          scope' = scope env <> scopeOf stm
       in env { v2mem = v2mem', scope = scope' }

-- | Array (register) coalescing can have one of three shapes:
--      a) @let y    = copy(b^{lu})@
--      b) @let y    = concat(a, b^{lu})@
--      c) @let y[i] = b^{lu}@
--   The intent is to use the memory block of the left-hand side
--     for the right-hand side variable, meaning to store @b@ in
--     @m_y@ (rather than @m_b@).
--   The following five safety conditions are necessary:
--      1. the right-hand side is lastly-used in the current statement
--      2. the allocation of @m_y@ dominates the creation of @b@
--      3. there is no use of the left-hand side memory block @m_y@
--           during the liveness of @b@, i.e., in between its last use
--           and its creation.
--      4. @b@ is a newly created array, i.e., does not aliases anything
--      5. the new index function of @b@ corresponding to memory block @m_y@
--           can be translated at the definition of @b@, and the
--           same for all variables aliasing @b@.
--   Observation: during the live range of @b@, @m_b@ can only be used by
--                variables aliased with @b@, because @b@ is newly created.
mkCoalsTabBnd :: LUTabFun -> Stm (Aliases ExpMem.ExplicitMemory)
              -> TopDnEnv -> BotUpEnv -> BotUpEnv

mkCoalsTabBnd _ (Let (Pattern [] [pe]) _ e) td_env bu_env
  | Just primexp <- primExpFromExp (basePMconv (scope td_env) (scals bu_env)) e =
  bu_env { scals = HM.insert (patElemName pe) primexp (scals bu_env) }

mkCoalsTabBnd lutab (Let pat _ (If cond body_then body_else _)) td_env bu_env =
  --  i) Filter @activeCoals@ by the 2ND, 3rd AND 5th safety conditions:
  let (activeCoals00,_) = mkCoalsHelper1FilterActive pat (BasicOp (SubExp cond)) td_env bu_env
      successCoals0 = successCoals bu_env
      pat_val_elms  = patternValueElements pat
      activeCoals0  = filterSafetyCond2and5 activeCoals00 (scals bu_env) (alloc td_env)
                                                          (scope td_env) pat_val_elms

  -- ii) extend @activeCoals@ by transfering the pattern-elements bindings existent
  --     in @activeCoals@ to the body results of the then and else branches, but only
  --     if the current pattern element can be potentially coalesced and also
  --     if the current pattern element satisfies safety conditions 2 & 5
      res_mem_then = findMemBodyResult activeCoals0 (scope td_env) pat_val_elms body_then
      res_mem_else = findMemBodyResult activeCoals0 (scope td_env) pat_val_elms body_else

      actv_then = foldl transferCoalsToIfBody activeCoals0 res_mem_then
      actv_else = foldl transferCoalsToIfBody activeCoals0 res_mem_else

  --iii) process the then and else bodies
      res_then = mkCoalsTabBdy lutab body_then td_env (bu_env {activeCoals = actv_then})
      res_else = mkCoalsTabBdy lutab body_else td_env (bu_env {activeCoals = actv_else})
      (actv_then0, succ_then0) = (activeCoals res_then, successCoals res_then)
      (actv_else0, succ_else0) = (activeCoals res_else, successCoals res_else)

  -- iv) eliminate unsuccessful coalescing from successCoals0, i.e.,
  --     coalescing of the if result is successful iff coalescing succeeded
  --     in both the then and else bodies.
      yuppi = foldl (\ acc ( (m_b,_,_,m_r1), (_,_,_,m_r2) ) ->
                            case (HM.lookup m_r1 succ_then0, HM.lookup m_r2 succ_else0) of
                              (Just _, Just _) -> HS.insert m_b acc -- succeeded in both branches,
                              (_,_) -> acc
                    ) HS.empty (zip res_mem_then res_mem_else)

      (activeCoals1, successCoals1) =
        foldl (\ (act,succc) (m_b,b,_,_) ->
                if HS.member m_b yuppi
                then case HM.lookup m_b act of
                       Nothing   -> (act,succc) -- impossible, but ...
                       Just info ->
                          -- promote and APPEND!
                          trace ("COALESCING: successful promotion from if-then-else: "++pretty b++pretty m_b)
                                (HM.delete m_b act, appendCoalsInfo m_b info succc)
                else (HM.delete m_b act, succc)
              ) (activeCoals0, successCoals0) res_mem_then

      (actv_then1, succ_then1, actv_else1, succ_else1) =
            if HS.size yuppi == length res_mem_then
            then -- all successful hence no redundant work
                (actv_then0, succ_then0, actv_else0, succ_else0)
            else -- if failed not empty, deregister the faulty tuples and re-try
                let (res_mem_then', res_mem_else') = unzip $
                        filter (\( (_,m_b,_,_), (_,_,_,_) ) -> HS.member m_b yuppi )
                               (zip res_mem_then res_mem_else)
                    actv_then' = foldl transferCoalsToIfBody activeCoals0 res_mem_then'
                    actv_else' = foldl transferCoalsToIfBody activeCoals0 res_mem_else'
                    res_then' = mkCoalsTabBdy lutab body_then td_env (bu_env {activeCoals = actv_then'})
                    res_else' = mkCoalsTabBdy lutab body_else td_env (bu_env {activeCoals = actv_else'})
                in  ( activeCoals res_then', successCoals res_then'
                    , activeCoals res_else', successCoals res_else' )

  --  v) evaluate successful coalescing:
  --     1. active coalescing info corresponds to the intersection of the current,
  --        then and else tables,
  --     2. successful coalescing info corresponds to the union of ...
      actv_res  = HM.intersectionWith unionCoalsEntry activeCoals1 $
                  HM.intersectionWith unionCoalsEntry actv_then1 actv_else1
      succ_res  = HM.unionWith unionCoalsEntry successCoals1 $
                  HM.unionWith unionCoalsEntry succ_then1 succ_else1
  in  bu_env { activeCoals = actv_res, successCoals = succ_res }

    where -- | Check safety conditions 2 and 5
          --      and update new substitutions:
          filterSafetyCond2and5 :: CoalsTab -> ScalarTab -> AllocTab
                                -> Scope (Aliases ExpMem.ExplicitMemory)
                                -> [PatElem (Aliases ExpMem.ExplicitMemory)]
                                -> CoalsTab
          filterSafetyCond2and5 act_coal scals_env allocs_env scope_env =
            foldl (\ acc patel ->
                    case (patElemName patel, patElemAttr patel) of
                      (b, (_,ExpMem.ArrayMem _ _ _ m_b _)) ->
                         case HM.lookup m_b acc of
                           Nothing -> acc
                           Just info@(CoalsEntry x_mem _ _ vtab _) ->
                             case HM.lookup b vtab of
                               Nothing -> HM.delete m_b acc -- too drastic ?
                               Just (Coalesced k mblk@(MemBlock _ _ _ new_indfun) _) ->
                                 let (safe_5, fv_subst) = translateIndFunFreeVar scope_env scals_env new_indfun
                                     safe_2 = HS.member x_mem allocs_env
                                 in  if safe_5 && safe_2
                                     then let mem_info = Coalesced k mblk fv_subst
                                              info' = info { vartab = HM.insert b mem_info vtab }
                                          in  HM.insert m_b info' acc
                                     else HM.delete m_b acc
                      _ -> acc
                  ) act_coal

          -- | Results in pairs of pattern-blockresult pairs of (var name, mem block)
          --   for those if-patterns that are candidates for coalescing.
          findMemBodyResult :: CoalsTab
                            -> Scope (Aliases ExpMem.ExplicitMemory)
                            -> [PatElem (Aliases ExpMem.ExplicitMemory)]
                            -> Body (Aliases ExpMem.ExplicitMemory)
                            -> [(VName, VName, VName, VName)]
          findMemBodyResult activeCoals_tab scope_env patelms bdy =
            let scope_env' = scope_env <> scopeOf (bodyStms bdy)
            in  mapMaybe (\(patel, se_r) ->
                              case (patElemName patel, patElemAttr patel, se_r) of
                                (b, (_,ExpMem.ArrayMem _ _ _ m_b _), Var r) ->
                                  case getScopeMemInfo r scope_env' of
                                    Just (MemBlock _ _ m_r _) ->
                                      case HM.lookup m_b activeCoals_tab of
                                        Nothing -> Nothing
                                        Just (CoalsEntry _ _ _ vtab _) ->
                                          case HM.lookup b vtab of
                                            Nothing -> Nothing
                                            Just _  -> Just (m_b,b,r,m_r)
                                    _ -> Nothing
                                _ ->     Nothing
                         ) (zip patelms (bodyResult bdy))

          -- | transfers coalescing from if-pattern to then|else body result
          --   in the active coalesced table
          transferCoalsToIfBody :: CoalsTab
                                -> (VName, VName, VName, VName)
                                -> CoalsTab
          transferCoalsToIfBody activeCoals_tab (m_b, b, r, m_r) =
            case HM.lookup m_b activeCoals_tab of
              Nothing -> activeCoals_tab -- cannot happen
              Just etry@(CoalsEntry m_x ind_x als_x vtab opts_x) ->
                case HM.lookup b vtab of
                  Nothing -> activeCoals_tab
                  Just (Coalesced knd (MemBlock btp shp _ ind_b) subst_b) ->
                    -- by definition of if-stmt, r and b have the same basic type, shape and
                    -- index function, hence, for example, do not need to rebase
                    let ind_r = ind_b --case mslc_b of
                                      --  Nothing  -> ind_b
                                      --  Just slc -> updateIndFunSlice ind_b slc
                        mem_info = Coalesced knd (MemBlock btp shp m_x ind_r) subst_b
                        coal_etry= if m_r == m_b
                                   then etry { vartab = HM.insert r mem_info vtab}
                                   else CoalsEntry m_x ind_x als_x (HM.singleton r mem_info) opts_x
                    in  HM.insert m_r coal_etry activeCoals_tab

--mkCoalsTabBnd lutab (Let pat _ (Op (ExpMem.Inner (ExpMem.Kernel str cs ker_space tps ker_bdy)))) td_env bu_env =
--  bu_env

mkCoalsTabBnd lutab (Let pat _ e) td_env bu_env =
  --   i) Filter @activeCoals@ by the 3rd safety condition:
  let (activeCoals', _) = mkCoalsHelper1FilterActive pat e td_env bu_env

  --  ii) promote any of the entries in @activeCoals@ to @successCoals@ as long as
  --        - this statement defined a variable consumed in a coalesced statement
  --        - and safety conditions 2, 4, and 5 are satisfied.
  --      AND extend @activeCoals@ table for any definition of a variable that
  --      aliases a coalesced variable.
      safe_4    = createsNewArrOK     e
      (activeCoals'', successCoals') =
        foldl (\(a_acc,s_acc) (b,MemBlock tp shp mb b_indfun, bnd) ->
                  case HM.lookup mb a_acc of
                    Just info@(CoalsEntry x_mem x_indfun _ vtab _) ->
                      case HM.lookup b vtab of
                        Just (Coalesced k mblk@(MemBlock _ _ _ new_indfun) _) ->
                          -- we are at the definition of the coalesced variable @b@
                          -- if 2,4,5 hold promote it to successful coalesced table,
                          -- or if e = reshape/transpose/rotate then postpone decision
                          --    for later on
                          let safe_2 = HS.member x_mem (alloc td_env)
                              (safe_5, fv_subst) = translateIndFunFreeVar (scope td_env) (scals bu_env) new_indfun
                              alias_var = case bnd of
                                            BindInPlace _ b_al _ -> Just b_al
                                            BindVar -> createsAliasedArrOK e
                              -- For Sanity Purposes we treat cases such as:
                              -- in-place: @let b2 <- b1 with [i] = e@ defers verification of b
                              --           until the creation of @b1@ or recursively
                              -- aliases:  @let b = rotate a@ defers versification of b
                              --           until the creation of @a@ or recursively
                          in  case (safe_2, safe_4, safe_5, alias_var) of
                                (True, True, True, Nothing) ->
                                  -- great, new array creation point AND safety conditions
                                  --  2,4,5 hold => promote and append
                                  let mem_info = Coalesced k mblk fv_subst
                                      info' = info { vartab = HM.insert b mem_info vtab }
                                  in  (HM.delete mb a_acc, appendCoalsInfo mb info' s_acc)
                                (True, _, True, Just b_al) ->
                                  -- We are in case b = transpose/reshape/rotate b_al
                                  --        or case b <- b_al with [i] = e
                                  -- postpone decision until b_al is verified
                                  let mem_info = Coalesced k mblk fv_subst
                                      info' = info { vartab = HM.insert b mem_info vtab }
                                      -- update the b forward substitution info as before
                                  in  case HM.lookup b_al (v2mem td_env) of
                                        Nothing -> (HM.delete mb a_acc, s_acc) -- remove from active
                                        Just (MemBlock b_al_tp b_al_shp m_b_al indfun_b_al) ->
                                          case Exc.assert (m_b_al == mb) $ tryRebase x_indfun indfun_b_al of
                                            Nothing -> (HM.delete mb a_acc, s_acc)
                                            Just new_indfun_b_al ->
                                              let mem_info_al = Coalesced k (MemBlock b_al_tp b_al_shp x_mem new_indfun_b_al) HM.empty
                                                  info'' = info' { vartab = HM.insert b_al mem_info_al vtab }
                                                  -- create a new entry for the aliased of b (@b_al@)
                                                  -- promoted when @b_al@ definition is validated or recursively.
                                              in  trace ("COALESCING: postponed promotion: "++pretty b)
                                                        (HM.insert mb info'' a_acc, s_acc)
                                _ -> (HM.delete mb a_acc, s_acc) -- remove from active

                        Nothing ->
                          -- we are at the definition of some variable aliased with
                          -- the coalesced variable @b@, hence extend @activeCoals@,
                          -- e.g., @let a = map f arr  @
                          --       @let b = transpose a@ <- current statement
                          --       @ ... use of b ...  @
                          --       @let x[i] = a       @
                          --       we need to add variable @b@ to the entry of @m_a@
                          case tryRebase x_indfun b_indfun of
                            Nothing -> (HM.delete mb a_acc, s_acc)
                            Just new_indfun ->
                              case translateIndFunFreeVar (scope td_env) (scals bu_env) new_indfun of
                                (False, _) -> (HM.delete mb a_acc, s_acc)
                                (True, fv_subst) ->
                                   let mem_info = Coalesced Trans (MemBlock tp shp x_mem new_indfun) fv_subst
                                       info' = info { vartab = HM.insert b mem_info vtab }
                                   in  (HM.insert mb info' a_acc, s_acc)
                    Nothing -> (a_acc,s_acc)
              ) (activeCoals', successCoals bu_env) (getArrMemAssoc pat)

  -- iii) record a potentially coalesced statement in @activeCoals@
      activeCoals''' = mkCoalsHelper3PatternMatch pat e lutab td_env successCoals' activeCoals''

  in  bu_env { activeCoals = activeCoals''', successCoals = successCoals' }

--trace ("COALESCING success? "++pretty safe_2++" "++pretty safe_4++" "++pretty safe_5++" "++pretty b++" "++pretty mb) $ safe_2 && safe_4 && safe_5


-- | 1. Filters @activeCoals@ by the 3rd safety condition, namely:
--      "there is no use of the left-hand side memory block @m_y@
--       during the liveness of right-hand side @b@, i.e., in between
--       its creation and its last use."
--   2. Treats pattern in-place updates by updating the optdeps field
--       of the dst variable with the info of the fist.
--   3. Returns a new active-coalescing table, together with a list of
--       (Maybe) index functions corresponding to expression's results,
--       in which the elements correspond to the result of @getArrMemAssoc@.
--   4. Assumption: the in-place assigned expression creates a new array!
mkCoalsHelper1FilterActive :: Pattern (Aliases ExpMem.ExplicitMemory)
                           -> Exp (Aliases ExpMem.ExplicitMemory)
                           -> TopDnEnv -> BotUpEnv
                           -> (CoalsTab, [ExpMem.IxFun])
mkCoalsHelper1FilterActive pat e td_env bu_env =
  let (v2mem_tab, active_tab) = (v2mem td_env, activeCoals bu_env)
      e_mems  = mapMaybe (`HM.lookup` v2mem_tab) $ HS.toList $ freeInExp e
      memasoc = getArrMemAssoc pat
      -- get memory-block names that are used in the current stmt.
      stm_mems= HS.fromList $ map (\(MemBlock _ _ mnm _) -> mnm) $
                e_mems ++ map (\(_,mb,_)->mb) memasoc

      -- BUG: take the aliasing transitive closure of @all_mems@
      all_mems = stm_mems
      -- trace ("COALESCING: for pattern "++pretty pat++", cond3 disables "++pretty (HS.toList stm_mems)) $ stm_mems

      -- keep only the entries that do not overlap with the memory
      -- blocks defined in @pat@ or used in expression @e@.
      active_tab1 = HM.filter (null . HS.intersection all_mems . alsmem) active_tab

      -- if necessary insert new vartab entries for the in-place "old" names
      -- and compute slice-index functions for the in-place assigned expressions
      -- and for the normal bindings results in normal index functions.
      (active_tab2, rev_lst_indfuns) =
        foldl (\(act_tab, indfuns) (b, MemBlock _ _ mem_nm indf, bnd) ->
                case HM.lookup mem_nm active_tab of
                  Nothing ->
                    case bnd of
                      BindVar -> (act_tab, indf : indfuns)
                      BindInPlace _  _ slice_b ->
                        (act_tab, updateIndFunSlice indf slice_b : indfuns)

                  Just info@(CoalsEntry _ _ _ vtab _) ->
                    case HM.lookup b vtab of
                      Nothing -> Exc.assert False $ -- impossible case
                                 case bnd of
                                   BindVar -> (HM.delete mem_nm act_tab, indf : indfuns)
                                   BindInPlace _  _ slice_b ->
                                     ( HM.delete mem_nm act_tab
                                     , updateIndFunSlice indf slice_b : indfuns
                                     )

                      Just (Coalesced k mblk@(MemBlock _ _ _ b_indfun) _) ->
                        case bnd of
                          BindVar -> (act_tab, b_indfun : indfuns)
                          BindInPlace _  b' slice_b ->
                            let b_ind_slice  = Exc.assert (createsNewArrOK e) $
                                               updateIndFunSlice b_indfun slice_b
                                indfuns'     = b_ind_slice : indfuns
                                (ok,fv_subs) = translateIndFunFreeVar (scope td_env) (scals bu_env) b_indfun
                                coal_etry_b  = Coalesced k mblk fv_subs
                                coal_etry_b' = Coalesced k mblk HM.empty
                                info' = info { vartab = HM.insert b' coal_etry_b' $
                                                        HM.insert b  coal_etry_b vtab }
                            in  if not ok then (HM.delete mem_nm act_tab, indfuns')
                                else (HM.insert mem_nm info' act_tab, indfuns')
              ) (active_tab1,[]) memasoc

  in  (active_tab2, reverse rev_lst_indfuns)

-- |   Pattern matches a potentially coalesced statement and
--     records a new association @activeCoals@
mkCoalsHelper3PatternMatch :: Pattern (Aliases ExpMem.ExplicitMemory)
                           -> Exp (Aliases ExpMem.ExplicitMemory)
                           -> LUTabFun -> TopDnEnv -> CoalsTab -> CoalsTab
                           -> CoalsTab
mkCoalsHelper3PatternMatch  pat e lutab td_env successCoals_tab activeCoals_tab =
  case genCoalStmtInfo lutab (v2mem td_env) (scope td_env) pat e of
    Nothing   -> activeCoals_tab
    Just clst ->
      foldl (\ acc (knd,_,m_x,ind_x,b,m_b,ind_b,tp_b,shp_b) ->
              -- test whether we are in a transitive coalesced case, i.e.,
              --      @let x[j] = b@
              --      @let y[i] = x@
              -- and compose the index function of @x@ with that of @y@,
              -- and update aliasing with @m_y@ and @m_x@, i.e.,
              -- validity condition 3) should check that @m_y@ and @m_x@ ...
              -- are not used during the liveness of @b@.
              let proper_coals_tab = case knd of
                                       InPl -> activeCoals_tab
                                       _    -> successCoals_tab
                  (success0, m_yx, ind_yx, mem_yx_al, x_deps) =
                    case HM.lookup m_x proper_coals_tab of
                      Nothing -> (True, m_x, ind_x, HS.singleton m_x, HS.empty)
                      Just (CoalsEntry m_y ind_y y_al _ x_deps0) ->
                        case tryRebase ind_y ind_x of
                          Just new_fun -> (True,  m_y, new_fun, HS.insert m_x y_al, x_deps0)
                          Nothing      -> (False, m_y, ind_y, y_al, x_deps0)
              in  case (success0, tryRebase ind_yx ind_b, m_b /= m_yx, HS.member m_yx (alloc td_env)) of
                    (True, Just new_ind, True, True) ->
                      -- finally update the @activeCoals@ table with a fresh
                      -- binding for @m_b@; if such one exists then overwrite.
                      let mem_info  = Coalesced knd (MemBlock tp_b shp_b m_yx new_ind) HM.empty
                          coal_etry = CoalsEntry m_yx ind_yx mem_yx_al (HM.singleton b mem_info) $
                                        case (knd, m_yx == m_x) of
                                          -- if in-place update then the coalescing is dependent
                                          -- on the coalescing success of the parent node (if any)
                                          (InPl, False) -> HS.insert m_x x_deps
                                          _             -> HS.empty -- or x_deps?
                      in  HM.insert m_b coal_etry acc
                    _ -> acc
            ) activeCoals_tab clst

translateIndFunFreeVar :: Scope (Aliases ExpMem.ExplicitMemory)
                       -> ScalarTab -> ExpMem.IxFun
                       -> (Bool,FreeVarSubsts)
translateIndFunFreeVar scope0 scals0 indfun =
  let fv_indfun     = HS.toList $ freeIn indfun
      fv_trans_vars = trace ("COALESCING: free vars in indexfun: "++pretty indfun++" are: "++pretty fv_indfun) $ filter (\x -> not $ HM.member x scope0) fv_indfun
      fv_trans_exps = mapMaybe (`HM.lookup` scals0) fv_trans_vars
  in  if  length fv_trans_exps == length fv_trans_vars
      then trace ("COALESCING translation: vars: "++pretty fv_trans_vars++" exps: "++pretty fv_trans_exps) (True , HM.fromList $ zip fv_trans_vars fv_trans_exps)
      else (False, HM.empty)

-- | merges entries in the coalesced table.
appendCoalsInfo :: VName -> CoalsEntry -> CoalsTab -> CoalsTab
appendCoalsInfo mb info_new coalstab =
  case HM.lookup mb coalstab of
    Nothing -> HM.insert mb info_new coalstab
    Just info_old ->
        let vartab2 = HM.union (vartab info_old) (vartab info_new)
            info2   = info_new { vartab = vartab2 }
        in  HM.insert mb info2 coalstab

tryRebase :: ExpMem.IxFun -> ExpMem.IxFun -> Maybe ExpMem.IxFun
tryRebase x_indfun b_indfun =
  let new_fun = IxFun.rebase x_indfun b_indfun
  in  if IxFun.rank x_indfun == length (IxFun.base b_indfun)
      then Just new_fun
      else Nothing

genCoalStmtInfo :: LUTabFun -> V2MemTab
                -> Scope (Aliases ExpMem.ExplicitMemory)
                -> Pattern (Aliases ExpMem.ExplicitMemory)
                -> Exp (Aliases ExpMem.ExplicitMemory)
                -> Maybe [(CoalescedKind,VName,VName,ExpMem.IxFun,VName,VName,ExpMem.IxFun,PrimType,Shape)]
genCoalStmtInfo lutab v2memtab scopetab pat e =
  case (patternValueElements pat,e) of
    -- CASE a) @let y = copy(b^{lu})@
    ( [PatElem x BindVar (_,ExpMem.ArrayMem _ _ _ m_x ind_x)], BasicOp (Copy b) ) ->
        case (HM.lookup x lutab, HM.lookup b v2memtab) of
            (Just last_uses, Just (MemBlock tpb shpb m_b ind_b)) ->
                if not (HS.member b last_uses) then Nothing
                else Just [(Ccopy,x,m_x,ind_x,b,m_b,ind_b,tpb,shpb)]
            _ -> Nothing

    -- CASE c) @let y[i] = b^{lu}@
    ( [PatElem x' (BindInPlace _ x slice_x) (_,ExpMem.ArrayMem _ _ _ m_x ind_x)], BasicOp (Copy b) ) ->
        -- do something to also cover @e == BasicOp (SubExp (Var b))@
        case (HM.lookup x' lutab, HM.lookup b v2memtab) of
            (Just last_uses, Just (MemBlock tpb shpb m_b ind_b)) ->
                if trace ("COALESCING: in-place pattern found for "++pretty b) $ not (HS.member b last_uses) then Nothing
                else let ind_x_slice = updateIndFunSlice ind_x slice_x
                     in  Just [(InPl,x,m_x,ind_x_slice,b,m_b,ind_b,tpb,shpb)]
            _ -> Nothing

    -- CASE b) @let y = concat(a, b^{lu})@
    ( [PatElem x BindVar (_,ExpMem.ArrayMem _ _ _ m_x ind_x)], BasicOp (Concat _ 0 b0 bs _) ) ->
      case HM.lookup x lutab of
        Nothing -> Nothing
        Just last_uses ->
          let zero = primExpFromSubExp (IntType Int32) (constant (0::Int32))
              (res,_,_) =
                foldl (\ (acc,offs,succ0) b ->
                        if not succ0 then (acc, offs, succ0)
                        else
                          case (HM.lookup b v2memtab, HM.lookup b scopetab) of
                            (Just (MemBlock tpb shpb m_b ind_b), Just scp_info) ->
                              let (succ1,offs_b) =
                                    case typeOf scp_info of
                                      Array _ shp _ ->
                                        case shapeDims shp of
                                          []   -> (False, zero)
                                          se:_ -> (succ0, primExpFromSubExp (IntType Int32) se)
                                      _ -> (False, zero)
                                  offs' = BinOpExp (Add Int32) offs_b offs
                              in  if HS.member b last_uses && succ1
                                  then let ind_x_slice = IxFun.offsetIndex ind_x offs'
                                       in  (acc++[(Conc,x,m_x,ind_x_slice,b,m_b,ind_b,tpb,shpb)],offs',succ1)
                                  else (acc,offs',succ1)
                            _ ->  (acc,offs,False)
                      ) ([],zero,True) (b0:bs)
          in  if null res then Nothing
              else Just res
    -- CASE other than a), b), or c).
    _ -> Nothing

updateIndFunSlice :: ExpMem.IxFun -> Slice SubExp -> ExpMem.IxFun
updateIndFunSlice ind_fun slice_x =
  let slice_x' = map (fmap (primExpFromSubExp (IntType Int32))) slice_x
  in  IxFun.slice ind_fun slice_x'
