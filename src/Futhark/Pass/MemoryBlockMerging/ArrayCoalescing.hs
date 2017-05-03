{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Playground for work on merging memory blocks
module Futhark.Pass.MemoryBlockMerging.ArrayCoalescing
  ( mkCoalsTab
  ) where

import Prelude
import Data.Maybe
import Data.Monoid
import Data.List
import Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Exception.Base as Exc
import Debug.Trace

import Futhark.Representation.AST.Attributes.Scope
import Futhark.Analysis.PrimExp.Convert
import Futhark.Representation.Aliases
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun

import Futhark.Pass.MemoryBlockMerging.DataStructs
import Futhark.Pass.MemoryBlockMerging.LastUse


type ScopeTab = Scope (Aliases ExpMem.ExplicitMemory)
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
                         }

data BotUpEnv = BotUpEnv { scals :: ScalarTab
                         -- ^ records variable names to memory block mapping;
                         --   a var might map to several mem blocks due to aliasing???
                         , activeCoals :: CoalsTab
                         -- ^ optimistic coalescing info
                         , successCoals :: CoalsTab
                         -- ^ committed (successfull) coalescing info
                         , inhibit :: InhibitTab
                         -- ^ the coalescing failures from this pass
                         }

emptyTopDnEnv :: TopDnEnv
emptyTopDnEnv = TopDnEnv { alloc = S.empty, scope = M.empty, inhibited = M.empty }

emptyBotUpEnv :: BotUpEnv
emptyBotUpEnv = BotUpEnv { scals = M.empty, activeCoals = M.empty
                         , successCoals = M.empty, inhibit = M.empty }


-- | basic conversion of a Var Expression to a PrimExp
basePMconv :: ScopeTab
           -> ScalarTab
           -> Exp (Aliases ExpMem.ExplicitMemory)
           -> Maybe (ExpMem.PrimExp VName)
basePMconv scopetab scaltab (BasicOp (SubExp (Var v))) =
  case M.lookup v scaltab of
    Just pexp -> Just pexp
    Nothing -> case M.lookup v scopetab of
                Just info ->
                    case typeOf info of
                      Prim tp -> Just $ ExpMem.LeafExp v tp
                      _ -> Nothing
                _ -> Nothing
basePMconv _ _ _ = Nothing

-- | Memory-block removal from active-coalescing table
--   should only be handled via this function, it is easy
--   to run into infinite execution problem; i.e., the
--   fix-pointed iteration of coalescing transformation
--   assumes that whenever a coalescing fails it is
--   recorded in the @inhibit@ table.
markFailedCoal :: (CoalsTab, InhibitTab)
               -> VName
               -> (CoalsTab, InhibitTab)
markFailedCoal (coal_tab,inhb_tab) src_mem =
  case M.lookup src_mem coal_tab of
         Nothing   -> (coal_tab,inhb_tab)
         Just (CoalsEntry dst_mem _ _ _ _) ->
           let failed_set = case M.lookup src_mem inhb_tab of
                              Nothing  -> S.singleton dst_mem
                              Just fld -> S.insert dst_mem fld
           in  ( M.delete src_mem coal_tab
               , M.insert src_mem failed_set inhb_tab )

-- | promotion from active-to-successful coalescing tables
--   should be handled with this function (for clarity).
markSuccessCoal :: (CoalsTab, CoalsTab) -> VName -> CoalsEntry
                -> (CoalsTab, CoalsTab)
markSuccessCoal (actv,succc) m_b info_b =
  ( M.delete m_b actv
  , appendCoalsInfo m_b info_b succc )

prettyInhibitTab :: InhibitTab -> String
prettyInhibitTab tab =
  let list_tups = map (second S.toList) (M.toList tab)
  in  pretty list_tups

--------------------------------------------------------------------------------
--- Main Coalescing Transformation computes a successful coalescing table    ---
--------------------------------------------------------------------------------

mkCoalsTab :: Prog (Aliases ExpMem.ExplicitMemory) -> CoalsTab
mkCoalsTab prg = foldl M.union M.empty $ map mkCoalsTabFun $ progFunctions prg

mkCoalsTabFun :: FunDef (Aliases ExpMem.ExplicitMemory) -> CoalsTab
mkCoalsTabFun fun@(FunDef _ _ _ fpars body) =
  let (_, lutab) = lastUseFun fun
      unique_mems = S.fromList $ M.keys $ getUniqueMemFParam fpars
      topenv = emptyTopDnEnv { scope = scopeOfFParams fpars
                             , alloc = unique_mems
                             }
  in  fixPointCoalesce lutab fpars body topenv

  where fixPointCoalesce :: LUTabFun
                         -> [FParam (Aliases ExpMem.ExplicitMemory)]
                         -> Body (Aliases ExpMem.ExplicitMemory)
                         -> TopDnEnv -> CoalsTab
        fixPointCoalesce lutab fpars0 body0 topenv0 =
          let buenv' = mkCoalsTabBdy lutab body0 topenv0 emptyBotUpEnv
              (succ_tab1, actv_tab11, inhb_tab11) = (successCoals buenv', activeCoals buenv', inhibit buenv')

              -- remove fpars from actv_tab1, as function's parameters cannot be merged
              (actv_tab1, inhb_tab1) = foldl markFailedCoal (actv_tab11, inhb_tab11) $
                                       map ((\(MemBlock _ _ m _) -> m) . snd) $ getArrMemAssocFParam fpars0

              (succ_tab, failed_optdeps) = fixPointFilterDeps succ_tab1 M.empty
              inhb_tab = M.unionWith S.union failed_optdeps inhb_tab1
              new_inhibited = M.unionWith S.union inhb_tab (inhibited topenv0)
          in  if not (M.null actv_tab1)
              then trace ("COALESCING ROOT: BROKEN INV, active not empty: " ++ pretty (M.keys actv_tab1) ) M.empty
              else if   trace ("COALESCING ROOT, new inhibitions : "++prettyInhibitTab inhb_tab) $
                        M.null inhb_tab
                   then succ_tab
                   else fixPointCoalesce lutab fpars0 body0 (topenv0{ inhibited = new_inhibited })

        fixPointFilterDeps :: CoalsTab -> InhibitTab -> (CoalsTab, InhibitTab)
        fixPointFilterDeps coaltab0 inhbtab0=
          let (coaltab,inhbtab) =
                foldl(\(coal,inhb) mb ->
                        case M.lookup mb coal of
                          Nothing -> (coal,inhb)
                          Just (CoalsEntry _ _ _ _ opts) ->
                            let failed = M.filterWithKey
                                  (\ r mr -> case M.lookup mr coal of
                                               Nothing -> True
                                               Just (CoalsEntry _ _ _ vtab _) ->
                                                 case M.lookup r vtab of
                                                   Nothing -> True
                                                   Just _  -> False
                                  ) opts
                            in  if   M.null failed
                                then (coal,inhb) -- all ok
                                else -- optimistic dependencies failed for the current
                                     -- memblock; extend inhibited mem-block mergings.
                                     trace ("COALESCING: OPTDEPS FAILED for "++pretty mb++" set: "++pretty (M.keys failed)) $
                                     markFailedCoal (coal,inhb) mb
                     ) (coaltab0,inhbtab0) (M.keys coaltab0)
          in  if length (M.keys coaltab0) == length (M.keys coaltab)
              then (coaltab,inhbtab)
              else fixPointFilterDeps coaltab inhbtab

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
        env { alloc = S.insert (patElemName pe) (alloc env)}
    topdwnTravBinding stm env =
      -- ToDo: remember to update scope info appropriately
      --       for compound statements such as if, do-loop, etc.
      env { scope = scope env <> scopeOf stm }

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
  bu_env { scals = M.insert (patElemName pe) primexp (scals bu_env) }

-- | Assume the if-result is a candidate for coalescing (further in the code).
--   For now we conservatively treat only the case when both then and else
--   result arrays are created inside their corresponding branches of the @if@.
--   Otherwise we need more complex analysis, and may open pandora box.
--   For example, the code below:
--        @let a = map f a0                @
--        @let (c,x) = if cond             @
--        @            then (a,x)          @
--        @            else let b = map g a@
--        @                 let x[1] = a   @
--        @                 in (b,x)       @
--        @let y[3] = c                    @
--  If coalescing of the if-result succeeds than both @a@ and @b@ are placed
--  to @m_y[3]@. First, this is buggy because @b = map g a@ on the else branch.
--  Second this is buggy because @a@ will be associated to @m_y[3]@ on the then
--  branch, and with @m_x[1]@ on the else branch. Currently, unifying two
--  mappings with different memory destinations, just picks one of them.
--  If @m_x[1]@ is picked than we have an inconsisstent state. All these
--  because we allow such cases, which is unclear whether appear and can
--  be optimized in practice.
mkCoalsTabBnd lutab (Let patt _ (If _ body_then body_else _)) td_env bu_env =
  let pat_val_elms  = patternValueElements patt
      not_ip_pat = all notInPlace pat_val_elms
  --  i) Filter @activeCoals@ by the 2ND AND 5th safety conditions:
      (activeCoals0, inhibit0) =
        filterSafetyCond2and5 (activeCoals bu_env) (inhibit bu_env) (scals bu_env)
                              (alloc td_env) (scope td_env) pat_val_elms
      successCoals0 = successCoals bu_env

  -- ii) extend @activeCoals@ by transfering the pattern-elements bindings existent
  --     in @activeCoals@ to the body results of the then and else branches, but only
  --     if the current pattern element can be potentially coalesced and also
  --     if the current pattern element satisfies safety conditions 2 & 5.
      res_mem_then = findMemBodyResult activeCoals0 (scope td_env) pat_val_elms body_then
      res_mem_else = findMemBodyResult activeCoals0 (scope td_env) pat_val_elms body_else

      actv_then_i = foldl transferCoalsToBody activeCoals0 res_mem_then
      actv_else_i = foldl transferCoalsToBody activeCoals0 res_mem_else

      -- eliminate the original pattern binding of the if statement,
      -- @let x = if y[0,0] > 0 then map (+y[0,0) a else map (+1) b@
      -- @let y[0] = x@
      -- should succeed because @m_y@ is used before @x@ is created.
      (actv_then, actv_else) = foldl (\(acth,acel) (m_b,_,_,m_r) ->
                                            if m_b == m_r
                                            then (acth, acel)
                                            else (M.delete m_b acth, M.delete m_b acel)
                                     ) (actv_then_i, actv_else_i) res_mem_then

  --iii) process the then and else bodies
      res_then = mkCoalsTabBdy lutab body_then td_env (bu_env {activeCoals = actv_then})
      res_else = mkCoalsTabBdy lutab body_else td_env (bu_env {activeCoals = actv_else})
      (actv_then0, succ_then0, inhb_then0) = (activeCoals res_then, successCoals res_then, inhibit res_then)
      (actv_else0, succ_else0, inhb_else0) = (activeCoals res_else, successCoals res_else, inhibit res_else)

  -- iv) optimistically mark the pattern succesful:
      ((activeCoals1, inhibit1), successCoals1) =
        foldl (\ ((act,inhb),succc) ((m_b,b,r1,mr1),(_,_,r2,mr2)) ->
                case M.lookup m_b act of
                  Nothing   -> Exc.assert False ((act,inhb),succc) -- impossible
                  Just info ->
                    case (M.lookup mr1 succ_then0, M.lookup mr2 succ_else0) of
                      (Just _, Just _) -> -- Optimistically promote and append!
                        let info' = info { optdeps = M.insert r2 mr2 $
                                           M.insert r1 mr1 $ optdeps info }
                            (act',succc') = markSuccessCoal (act,succc) m_b info'
                        in trace ("COALESCING: if-then-else promotion: "++pretty b++pretty m_b)
                                 ((act',inhb), succc')
                      _ -> if m_b == mr1 && m_b == mr2
                           then -- special case resembling:
                                -- @let x0 = map (+1) a                                  @
                                -- @let x3 = if cond then let x1 = x0 with [0] <- 2 in x1@
                                -- @                 else let x2 = x0 with [1] <- 3 in x2@
                                -- @let z[1] = y                                         @
                                -- In this case the result active table should be the union
                                -- of the @m_x@ entries of the then and else active tables.
                                case (M.lookup mr1 actv_then0, M.lookup mr2 actv_else0) of
                                  (Just info_then, Just info_else) -> -- add it to active
                                    let info' = unionCoalsEntry info $
                                                unionCoalsEntry info_then info_else
                                        act'  = M.insert m_b info' act
                                    in  ((act',inhb),succc)
                                  -- otherwise remove the coalescing result
                                  _ -> trace ("COALESCING: if-then-else fails m_b == mr1 == mr2 "++pretty b++" "++pretty m_b)
                                             (markFailedCoal (act,inhb) m_b, succc)
                           else -- one of the branches has failed coalescing, hence remove
                                -- the coalescing of the result.
                                trace ("COALESCING: if-then-else fails "++pretty b++pretty m_b)
                                      (markFailedCoal (act,inhb) m_b, succc)
              ) ((activeCoals0, inhibit0), successCoals0) (zip res_mem_then res_mem_else)

  --  v) unify coalescing results of all branches by taking the union
  --     of all entries in the current/then/else success tables.
      actv_com_then = M.intersectionWith unionCoalsEntry actv_then0 activeCoals0
      then_diff_com = M.difference actv_then0 actv_com_then
      (_,inhb_then1)= --trace ("DIFF COM: "++prettyCoalTab then_diff_com ++ " INHIBIT: "++prettyInhibitTab inhb_then0) $
                      foldl markFailedCoal (then_diff_com,inhb_then0) (M.keys then_diff_com)

      actv_com_else = M.intersectionWith unionCoalsEntry actv_else0 activeCoals0
      else_diff_com = M.difference actv_else0 actv_com_else
      (_,inhb_else1)= foldl markFailedCoal (else_diff_com,inhb_else0) (M.keys else_diff_com)

      actv_res0 = M.intersectionWith unionCoalsEntry actv_then0 $
                  M.intersectionWith unionCoalsEntry actv_else0 activeCoals1

      succ_res  = M.unionWith unionCoalsEntry succ_then0 $
                  M.unionWith unionCoalsEntry succ_else0 successCoals1

  -- vi) Finally filter active result by 3rd SAFETY condition.
  --     Note that this step needs to be the last one otherwise
  --     valid pattern elements corresponding to the if-def may
  --     be (too) conservatively invalidated, i.e., we gave a chance
  --     to coalescing to be promoted from inside the then-and-else
  --     bodies before invalidating active based on free names
  --     (e.g., that can occur at the very beginning of the then-else bodies).
      body_free_vars = freeInBody body_then `S.union` freeInBody body_else
      ((actv_res, inhibit_res0),_) =
        Exc.assert not_ip_pat $
        trace ("COALESCING IF: active == "++prettyCoalTab actv_res0) $
        mkCoalsHelper1FilterActive patt body_free_vars (scope td_env)
                                   (scals bu_env) actv_res0 inhibit1

      inhibit_res = --trace ("COALESCING IF inhibits: " ++ prettyInhibitTab inhibit_res0 ++ " " ++ prettyInhibitTab inhb_then1 ++ " " ++ prettyInhibitTab inhb_else1) $
                    M.unionWith S.union inhibit_res0 $
                    M.unionWith S.union inhb_then1 inhb_else1
  in  bu_env { activeCoals = actv_res, successCoals = succ_res, inhibit = inhibit_res }

--mkCoalsTabBnd lutab (Let pat _ (Op (ExpMem.Inner (ExpMem.Kernel str cs ker_space tps ker_bdy)))) td_env bu_env =
--  bu_env

mkCoalsTabBnd lutab (Let pat _ (DoLoop arginis_ctx arginis lform body)) td_env bu_env =
  let pat_val_elms  = patternValueElements pat
      not_ip_pat = all notInPlace pat_val_elms

  --  i) Filter @activeCoals@ by the 2nd, 3rd AND 5th safety conditions:
      (activeCoals00,inhibit00) =
        filterSafetyCond2and5 (activeCoals bu_env) (inhibit bu_env) (scals bu_env)
                              (alloc td_env) (scope td_env) pat_val_elms
      loop_fv = freeInBody body `S.union`
                  S.fromList (mapMaybe (\(_,i)->case i of
                                                     Var v -> Just v
                                                     _     -> Nothing
                                        ) arginis)
      ((actv0,inhibit0),_) =
        Exc.assert not_ip_pat $
        mkCoalsHelper1FilterActive pat loop_fv (scope td_env)
                                   (scals bu_env) activeCoals00 inhibit00
      scopetab = scope td_env                              <>
                 (scopeOfFParams (fst $ unzip arginis_ctx) <>
                 (scopeOfFParams (fst $ unzip arginis    ) <>
                  scopeOfLoopForm lform))
  -- ii) Extend @activeCoals@ by transfering the pattern-elements bindings existent
  --     in @activeCoals@ to the loop-body results, but only if:
  --       (a) the pattern element is a candidate for coalescing,     and
  --       (b) the pattern element satisfies safety conditions 2 & 5,
  --           (conditions (a) and (b) have already been checked above), and
  --       (c) the memory block of the corresponding body result is
  --           allocated outside the loop, i.e., non-existential,     and
  --       (d) the init name is lastly-used in the initialization
  --           of the loop variant.
  --     Otherwise fail and remove from active-coalescing table!
      scopetab_loop = scopetab <> scopeOf (bodyStms body)
      bdy_ress = drop (length arginis_ctx) $ bodyResult body
      (patmems, argmems, inimems, resmems) =
        trace ("COALESCING loop input: "++pretty (map patElemName pat_val_elms)) $
        Data.List.unzip4 $
        mapMaybe (\(patel, (arg,ini), bdyres) ->
                   case ( patElemName patel, patElemAttr patel, patElemBindage patel
                        , paramName arg, ini, bdyres) of
                     (b, (_,ExpMem.ArrayMem _ _ _ m_b _), BindVar, a, Var a0, Var r) ->
                       case M.lookup m_b actv0 of
                         Nothing -> Nothing
                         Just (CoalsEntry _ _ _ vtab _) ->
                           -- if here, then a) and b) have been satisfied.
                           case M.lookup b vtab of
                             Nothing -> Nothing
                             Just _  ->
                               -- ok, b is in active coalesced table
                               case ( getScopeMemInfo a  scopetab_loop
                                    , getScopeMemInfo a0 scopetab_loop
                                    , getScopeMemInfo r  scopetab_loop ) of
                                 (Just (MemBlock _ _ m_a _), Just (MemBlock _ _ m_a0 _), Just (MemBlock _ _ m_r _)) ->
                                   -- checking c) and d)
                                   let is_lu_a0 = case M.lookup a lutab of
                                                    Nothing -> False
                                                    Just nms-> S.member a0 nms
                                   in  if is_lu_a0 && S.member m_r (alloc td_env)
                                       then trace ("COALESCING loop candidate: "++pretty b++" "++pretty m_b++" "++pretty r++" "++pretty m_r) $
                                                  Just ((b,m_b), (a,m_a), (a0,m_a0), (r,m_r))
                                       else Nothing
                                 _ -> trace ("COALESCING loop FAILED "++pretty b++" "++pretty m_b) Nothing
                     _ -> Nothing
                ) (zip3 pat_val_elms arginis bdy_ress)

      -- remove the other pattern elements from the active coalescing table:
      coal_pat_names = S.fromList $ map fst patmems
      (actv1,inhibit1) =
        foldl (\(act,inhb) (b, MemBlock _ _ m_b _, _) ->
                 if   S.member b coal_pat_names then (act,inhb) -- ok
                 else markFailedCoal (act,inhb) m_b -- remove from active
              ) (actv0,inhibit0) (getArrMemAssoc pat)

  -- iii) Process the loop's body.
  --      If the memory blocks of the loop result and loop variant param differ
  --      then make the original memory block of the loop result conflict with
  --      the original memory block of the loop parameter. This is done in
  --      order to prevent the coalescing of @a1@, @a0@, @x@ and @db@ in the
  --      same memory block of @y@ in the example below:
  --      @loop(a1 = a0) = for i < n do @
  --      @    let x = map (+1) a1      @
  --      @    let db = copy x          @
  --      @    in db                    @
  --      @let y[0] = a1                @
  --      Meaning the coalescing of @x@ in @let db = copy x@ should fail because
  --      @a1@ appears in the definition of @let x = map (+1) a1@.
      res_mem_bdy = zipWith (\(b,m_b) (r,m_r) -> (m_b,b,r,m_r)) patmems resmems
      res_mem_arg = zipWith (\(b,m_b) (r,m_r) -> (m_b,b,r,m_r)) patmems argmems
      res_mem_ini = zipWith (\(b,m_b) (r,m_r) -> (m_b,b,r,m_r)) patmems inimems
      actv2 = foldl transferCoalsToBody actv1 (res_mem_bdy++res_mem_arg++res_mem_ini)
      actv3 = foldl (\ tab ((_,_,_,m_r),(_,_,_,m_a)) ->
                        if m_r == m_a then tab
                        else case M.lookup m_r tab of
                                Nothing   -> tab
                                Just etry -> M.insert m_r (etry { alsmem = S.insert m_a (alsmem etry) }) tab
                    ) actv2 (zip res_mem_bdy res_mem_arg)

      res_env_body = mkCoalsTabBdy lutab body (td_env { scope    = scopetab })
                                              (bu_env { activeCoals = actv3
                                                      , inhibit = inhibit1  })

  -- iv) optimistically mark the pattern succesful if there is any chance to succeed
      (res_actv, res_succ, res_inhb) = (activeCoals res_env_body, successCoals res_env_body, inhibit res_env_body)
      ((fin_actv1,fin_inhb1), fin_succ1) =
        optimPromotion argmems resmems inimems $
        optimPromotion patmems resmems inimems ((res_actv,res_inhb),res_succ)

  in bu_env { activeCoals = fin_actv1, successCoals = fin_succ1, inhibit = fin_inhb1 }

    where optimPromotion othmems resmems inimems ((actv_tab,inhb_tab),succ_tab) =
            -- the use of @inimems@ and the test for @m_i@ are probably redundant
            foldl (\ ((act,inhb),succc) ((b,m_b), (r,m_r), (_,m_i)) ->
                    case M.lookup m_b act of
                      Nothing   -> trace "IMPOSSIBLE!!!" ((act,inhb), succc) -- impossible
                      Just info -> -- Optimistically promote and append!
                        let failed = markFailedCoal (act,inhb) m_b
                            has_no_chance = case (M.lookup m_r act, M.lookup m_r succc, M.lookup m_i act) of
                                              (Nothing, Nothing,_) -> True
                                              (_, _, Nothing)      -> True
                                              _                    -> False
                        in  if has_no_chance
                               -- the result has already failed coalescing,
                               -- hence the loop-result already failed coalescing; remove it.
                               -- we can additionally do some filtering of the loop tables,
                               -- but I feel lazy so we will carry them around.
                            then trace ("COALESCING: optimistic loop fails "++pretty has_no_chance)
                                 (failed, succc)
                            else let info' = info { optdeps = M.insert r m_r $ optdeps info }
                                     (act1, succc1) = markSuccessCoal (act,succc) m_b info'
                                 in  trace ("COALESCING: optimistic loop promotion: "++pretty b++" "++pretty m_b)
                                           ((act1,inhb), succc1)
                  ) ((actv_tab,inhb_tab), succ_tab) (zip3 othmems resmems inimems)

mkCoalsTabBnd lutab (Let pat _ e) td_env bu_env =
  --   i) Filter @activeCoals@ by the 3rd safety condition:
  let ((activeCoals',inhibit'), _) =
        mkCoalsHelper1FilterActive pat (freeInExp e) (scope td_env)
                                   (scals bu_env) (activeCoals bu_env) (inhibit bu_env)

  --  ii) promote any of the entries in @activeCoals@ to @successCoals@ as long as
  --        - this statement defined a variable consumed in a coalesced statement
  --        - and safety conditions 2, 4, and 5 are satisfied.
  --      AND extend @activeCoals@ table for any definition of a variable that
  --      aliases a coalesced variable.
      safe_4    = createsNewArrOK     e
      ((activeCoals'',inhibit''), successCoals') =
        foldl (\((a_acc,inhb),s_acc) (b,MemBlock tp shp mb b_indfun, bnd) ->
                  case M.lookup mb a_acc of
                    Nothing -> ((a_acc,inhb),s_acc)
                    Just info@(CoalsEntry x_mem x_indfun _ vtab _) ->
                      let failed = markFailedCoal (a_acc,inhb) mb
                      in
                       case M.lookup b vtab of
                        Nothing ->
                          -- we are at the definition of some variable aliased with
                          -- the coalesced variable @b@, hence extend @activeCoals@,
                          -- e.g., @let a = map f arr  @
                          --       @let b = reshape a  @ <- current statement
                          --       @ ... use of b ...  @
                          --       @let x[i] = a       @
                          --       we need to add variable @b@ to the entry of @m_a@
                          case tryRebase x_indfun b_indfun of
                            Nothing -> (failed,s_acc)
                            Just new_indfun ->
                              case translateIndFunFreeVar (scope td_env) (scals bu_env) new_indfun of
                                (False, _) -> (failed, s_acc)
                                (True, fv_subst) ->
                                   let mem_info = Coalesced Trans (MemBlock tp shp x_mem new_indfun) fv_subst
                                       info' = info { vartab = M.insert b mem_info vtab }
                                   in  ((M.insert mb info' a_acc,inhb), s_acc)
                        Just (Coalesced k mblk@(MemBlock _ _ _ new_indfun) _) ->
                          -- we are at the definition of the coalesced variable @b@
                          -- if 2,4,5 hold promote it to successful coalesced table,
                          -- or if e = reshape/transpose/rotate then postpone decision
                          --    for later on
                          let safe_2 = S.member x_mem (alloc td_env)
                              (safe_5, fv_subst) = translateIndFunFreeVar (scope td_env) (scals bu_env) new_indfun
                              (alias_var, in_place) =
                                  case bnd of
                                    BindInPlace{} -> Exc.assert safe_4 (Nothing, True)
                                    BindVar       -> (createsAliasedArrOK e, False)
                          in  case (safe_2, safe_4, safe_5, alias_var, in_place) of
                                (_, _, _, Just _, True) -> Exc.assert False ((a_acc,inhb), s_acc) --error!
                                (_, _, _, _, True) -> ((a_acc,inhb), s_acc) -- already treated.
                                (True, True, True, Nothing, False) ->
                                  -- great, new array creation point AND safety conditions
                                  --  2,4,5 hold => promote and append
                                  let mem_info = Coalesced k mblk fv_subst
                                      info' = info { vartab = M.insert b mem_info vtab }
                                      (a_acc',s_acc') = markSuccessCoal (a_acc,s_acc) mb info'
                                  in  trace ("COALESCING: successfull promote: "++pretty b++" "++pretty mb)
                                            ((a_acc',inhb), s_acc')
                                (True, _, True, Just b_al, False) ->
                                  -- We treat aliased cases such as:
                                  --   @let b    = rotate a@
                                  --   @let x[i] = b@
                                  --   by deferring versification of b
                                  --   until the creation of @a@ or recursively.
                                  -- Note that it cannot be @b[i] = rotate a@, see assert abve.
                                  -- in-place: has been treated in mkCoalsHelper1FilterActive
                                  let mem_info = Coalesced k mblk fv_subst
                                      info' = info { vartab = M.insert b mem_info vtab }
                                  in  -- update the b forward substitution info as before
                                      -- for example a case like below should succeed
                                      -- @let a = if cond then e1 else e2@
                                      -- @let b = transpose a@
                                      -- @let x[i] = b@
                                      case getScopeMemInfo b_al (scope td_env) of
                                        Nothing -> Exc.assert False (failed, s_acc)
                                        Just (MemBlock b_al_tp b_al_shp m_b_al indfun_b_al) ->
                                          case Exc.assert (m_b_al == mb && bnd == BindVar) (tryRebase x_indfun indfun_b_al) of
                                            Nothing -> (failed, s_acc)
                                            Just new_indfun_b_al ->
                                              -- create a new entry for the aliased of b (@b_al@)
                                              -- promoted when @b_al@ definition is validated or recursively.
                                              let mem_info_al = Coalesced k (MemBlock b_al_tp b_al_shp x_mem new_indfun_b_al) M.empty
                                                  info'' = info' { vartab = M.insert b_al mem_info_al vtab }
                                              in  trace ("COALESCING: postponed promotion: "++pretty b)
                                                        ((M.insert mb info'' a_acc,inhb), s_acc)
                                _ -> (failed, s_acc) -- conservatively remove from active

              ) ((activeCoals',inhibit'), successCoals bu_env) (getArrMemAssoc pat)

  -- iii) record a potentially coalesced statement in @activeCoals@
      activeCoals''' = mkCoalsHelper3PatternMatch pat e lutab td_env successCoals' activeCoals'' (inhibited td_env)

  in  bu_env { activeCoals = activeCoals''', inhibit = inhibit'', successCoals = successCoals' }

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
                           -> Names
                           -> ScopeTab -> ScalarTab -> CoalsTab -> InhibitTab
                           -> ((CoalsTab,InhibitTab), [ExpMem.IxFun])
mkCoalsHelper1FilterActive pat inner_free_vars scope_tab scals_tab active_tab inhibit_tab =
  let e_mems  = mapMaybe (`getScopeMemInfo` scope_tab) $ S.toList inner_free_vars
      memasoc = getArrMemAssoc pat
      -- get memory-block names that are used in the current stmt.
      stm_mems= S.fromList $ map (\(MemBlock _ _ mnm _) -> mnm) $
                e_mems ++ map (\(_,mb,_)->mb) memasoc

      -- BUG: take the aliasing transitive closure of @all_mems@
      all_mems = stm_mems
      -- trace ("COALESCING: for pattern "++pretty pat++", cond3 disables "++pretty (S.toList stm_mems)) $ stm_mems

      -- keep only the entries that do not overlap with the memory
      -- blocks defined in @pat@ or used in expression @e@.
      -- the others must be recorded in inhibit_tab
      active_tab1 = M.filter (null . S.intersection all_mems . alsmem) active_tab
      failed_tab  = M.difference active_tab active_tab1
      (_,inhibit_tab1) = foldl markFailedCoal (failed_tab,inhibit_tab) (M.keys failed_tab)

      -- if necessary insert new vartab entries for the in-place "old" names
      -- and compute slice-index functions for the in-place assigned expressions
      -- and for the normal bindings results in normal index functions.
      ((active_tab2, inhibit_tab2), rev_lst_indfuns) =
        foldl (\((act_tab,inhb_tab), indfuns) (b, MemBlock _ _ mem_nm indf, bnd) ->
                case M.lookup mem_nm active_tab of
                  Nothing ->
                    case bnd of
                      BindVar -> ((act_tab,inhb_tab), indf : indfuns)
                      BindInPlace _  _ slice_b ->
                        ((act_tab,inhb_tab), updateIndFunSlice indf slice_b : indfuns)

                  Just info@(CoalsEntry _ _ _ vtab _) ->
                    let failed = markFailedCoal (act_tab,inhb_tab) mem_nm
                    in
                     case M.lookup b vtab of
                      Nothing -> Exc.assert False $ -- impossible case
                                 case bnd of
                                   BindVar -> (failed, indf : indfuns)
                                   BindInPlace _  _ slice_b -> trace ("COALESCING: FAILS : "++pretty b++" "++pretty mem_nm)
                                     ( failed, updateIndFunSlice indf slice_b : indfuns )

                      Just (Coalesced k mblk@(MemBlock _ _ _ b_indfun) _) ->
                        case bnd of
                          BindVar -> ((act_tab,inhb_tab), b_indfun : indfuns)
                          BindInPlace _  b' slice_b ->
                            let b_ind_slice  = updateIndFunSlice b_indfun slice_b
                                indfuns'     = b_ind_slice : indfuns
                                (ok,fv_subs) = -- CORRECTNESS ASSUMPTION: the righ-hand side of
                                               -- an in-place update should create a new array!
                                               -- Exc.assert (createsNewArrOK e) $
                                               translateIndFunFreeVar scope_tab scals_tab b_indfun
                            in  if not ok then (failed, indfuns')
                                else let coal_etry_b  = Coalesced k mblk fv_subs
                                         info' = info { vartab = M.insert b' coal_etry_b $
                                                        M.insert b  coal_etry_b vtab }
                                     in  ((M.insert mem_nm info' act_tab,inhb_tab), indfuns')
              ) ((active_tab1,inhibit_tab1), []) memasoc

  in  ((active_tab2, inhibit_tab2), reverse rev_lst_indfuns)

-- |   Pattern matches a potentially coalesced statement and
--     records a new association @activeCoals@
mkCoalsHelper3PatternMatch :: Pattern (Aliases ExpMem.ExplicitMemory)
                           -> Exp (Aliases ExpMem.ExplicitMemory)
                           -> LUTabFun -> TopDnEnv -> CoalsTab -> CoalsTab -> InhibitTab
                           -> CoalsTab
mkCoalsHelper3PatternMatch  pat e lutab td_env successCoals_tab activeCoals_tab inhibit_tab =
  case genCoalStmtInfo lutab (scope td_env) pat e of
    Nothing   -> activeCoals_tab
    Just clst ->
      foldl (\ acc (knd,x,m_x,ind_x, b,m_b,ind_b,tp_b,shp_b) ->
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
                    case M.lookup m_x proper_coals_tab of
                      Nothing -> (True, m_x, ind_x, S.singleton m_x, M.empty)
                      Just (CoalsEntry m_y ind_y y_al _ x_deps0) ->
                        case tryRebase ind_y ind_x of
                          Just new_fun -> (True,  m_y, new_fun, S.insert m_x y_al, x_deps0)
                          Nothing      -> (False, m_y, ind_y, y_al, x_deps0)
              in  case (success0, tryRebase ind_yx ind_b, m_b /= m_yx, S.member m_yx (alloc td_env)) of
                    (True, Just new_ind, True, True) ->
                      -- finally update the @activeCoals@ table with a fresh
                      -- binding for @m_b@; if such one exists then overwrite.
                      let mem_info  = Coalesced knd (MemBlock tp_b shp_b m_yx new_ind) M.empty
                          coal_etry = CoalsEntry m_yx ind_yx mem_yx_al (M.singleton b mem_info) $
                                        -- coalescing is dependent on the coalescing
                                        -- success of the parent node (if any)
                                        if m_yx == m_x then M.empty
                                        else M.insert x m_x x_deps
                          updt_acc  = M.insert m_b coal_etry acc
                      in  case M.lookup m_b inhibit_tab of
                            Nothing -> updt_acc
                            Just nms-> if S.member m_yx nms then acc -- inhibited coalescing.
                                       else updt_acc
                    _ -> acc
            ) activeCoals_tab clst

translateIndFunFreeVar :: ScopeTab -> ScalarTab -> ExpMem.IxFun
                       -> (Bool,FreeVarSubsts)
translateIndFunFreeVar scope0 scals0 indfun =
  let fv_indfun     = S.toList $ freeIn indfun
      fv_trans_vars = trace ("COALESCING: free vars in indexfun: "++pretty indfun++" are: "++pretty fv_indfun) $ filter (\x -> not $ M.member x scope0) fv_indfun
      fv_trans_exps = mapMaybe (`M.lookup` scals0) fv_trans_vars
  in  if  length fv_trans_exps == length fv_trans_vars
      then trace ("COALESCING translation: vars: "++pretty fv_trans_vars++" exps: "++pretty fv_trans_exps) (True , M.fromList $ zip fv_trans_vars fv_trans_exps)
      else (False, M.empty)

-- | merges entries in the coalesced table.
appendCoalsInfo :: VName -> CoalsEntry -> CoalsTab -> CoalsTab
appendCoalsInfo mb info_new coalstab =
  case M.lookup mb coalstab of
    Nothing       -> M.insert mb info_new coalstab
    Just info_old -> M.insert mb (unionCoalsEntry info_old info_new) coalstab

tryRebase :: ExpMem.IxFun -> ExpMem.IxFun -> Maybe ExpMem.IxFun
tryRebase x_indfun b_indfun =
  let new_fun = IxFun.rebase x_indfun b_indfun
  in  if IxFun.rank x_indfun == length (IxFun.base b_indfun)
      then Just new_fun
      else Nothing

genCoalStmtInfo :: LUTabFun
                -> ScopeTab
                -> Pattern (Aliases ExpMem.ExplicitMemory)
                -> Exp (Aliases ExpMem.ExplicitMemory)
                -> Maybe [(CoalescedKind,VName,VName,ExpMem.IxFun,VName,VName,ExpMem.IxFun,PrimType,Shape)]
genCoalStmtInfo lutab scopetab pat e =
  case (patternValueElements pat,e) of
    -- CASE a) @let y = copy(b^{lu})@
    ( [PatElem x BindVar (_,ExpMem.ArrayMem _ _ _ m_x ind_x)], BasicOp (Copy b) ) ->
        case (M.lookup x lutab, getScopeMemInfo b scopetab) of
            (Just last_uses, Just (MemBlock tpb shpb m_b ind_b)) ->
                if not (S.member b last_uses) then Nothing
                else Just [(Ccopy,x,m_x,ind_x,b,m_b,ind_b,tpb,shpb)]
            _ -> Nothing

    -- CASE c) @let y[i] = b^{lu}@
    ( [PatElem x' (BindInPlace _ x slice_x) (_,ExpMem.ArrayMem _ _ _ m_x ind_x)], BasicOp (Copy b) ) ->
        -- do something to also cover @e == BasicOp (SubExp (Var b))@
        case (M.lookup x' lutab, getScopeMemInfo b scopetab) of
            (Just last_uses, Just (MemBlock tpb shpb m_b ind_b)) ->
                if trace ("COALESCING: in-place pattern found for "++pretty b) $ not (S.member b last_uses) then Nothing
                else let ind_x_slice = updateIndFunSlice ind_x slice_x
                     in  Just [(InPl,x,m_x,ind_x_slice,b,m_b,ind_b,tpb,shpb)]
            _ -> Nothing

    -- CASE b) @let y = concat(a, b^{lu})@
    ( [PatElem x BindVar (_,ExpMem.ArrayMem _ _ _ m_x ind_x)], BasicOp (Concat _ 0 b0 bs _) ) ->
      case M.lookup x lutab of
        Nothing -> Nothing
        Just last_uses ->
          let zero = primExpFromSubExp (IntType Int32) (constant (0::Int32))
              (res,_,_) =
                foldl (\ (acc,offs,succ0) b ->
                        if not succ0 then (acc, offs, succ0)
                        else
                          case (getScopeMemInfo b scopetab, M.lookup b scopetab) of
                            (Just (MemBlock tpb shpb m_b ind_b), Just scp_info) ->
                              let (succ1,offs_b) =
                                    case typeOf scp_info of
                                      Array _ shp _ ->
                                        case shapeDims shp of
                                          []   -> (False, zero)
                                          se:_ -> (succ0, primExpFromSubExp (IntType Int32) se)
                                      _ -> (False, zero)
                                  offs' = BinOpExp (Add Int32) offs_b offs
                              in  if S.member b last_uses && succ1
                                  then let ind_x_slice = IxFun.offsetIndex ind_x offs'
                                       in  (acc++[(Conc,x,m_x,ind_x_slice,b,m_b,ind_b,tpb,shpb)],offs',succ1)
                                  else (acc,offs',succ1)
                            _ ->  (acc,offs,False)
                      ) ([],zero,True) (b0:bs)
          in  if null res then Nothing
              else Just res
    -- CASE other than a), b), or c).
    _ -> Nothing

-- | Results in pairs of pattern-blockresult pairs of (var name, mem block)
--   for those if-patterns that are candidates for coalescing.
findMemBodyResult :: CoalsTab
                  -> ScopeTab
                  -> [PatElem (Aliases ExpMem.ExplicitMemory)]
                  -> Body (Aliases ExpMem.ExplicitMemory)
                  -> [(VName, VName, VName, VName)]
findMemBodyResult activeCoals_tab scope_env patelms bdy =
  let scope_env' = scope_env <> scopeOf (bodyStms bdy)
  in  mapMaybe (\(patel, se_r) ->
                  case (patElemName patel, patElemAttr patel, se_r) of
                    (b, (_,ExpMem.ArrayMem _ _ _ m_b _), Var r) ->
                      case getScopeMemInfo r scope_env' of
                        Nothing -> Nothing
                        Just (MemBlock _ _ m_r _) ->
                          case M.lookup m_b activeCoals_tab of
                            Nothing -> Nothing
                            Just (CoalsEntry _ _ _ vtab _) ->
                              case M.lookup b vtab of
                                Nothing -> Nothing
                                Just _  -> Just (m_b,b,r,m_r)
                    _ ->     Nothing
               ) (zip patelms (bodyResult bdy))

-- | Check safety conditions 2 and 5
--      and update new substitutions:
filterSafetyCond2and5 :: CoalsTab -> InhibitTab -> ScalarTab -> AllocTab
                      -> ScopeTab -> [PatElem (Aliases ExpMem.ExplicitMemory)]
                      -> (CoalsTab, InhibitTab)
filterSafetyCond2and5 act_coal inhb_coal scals_env allocs_env scope_env =
  foldl (\ (acc,inhb) patel ->
           case (patElemName patel, patElemAttr patel) of
             (b, (_,ExpMem.ArrayMem _ _ _ m_b _)) ->
               case M.lookup m_b acc of
                 Nothing -> (acc,inhb)
                 Just info@(CoalsEntry x_mem _ _ vtab _) ->
                   let failed = markFailedCoal (acc,inhb) m_b
                   in
                    case M.lookup b vtab of
                     Nothing -> failed -- too drastic ?
                     Just (Coalesced k mblk@(MemBlock _ _ _ new_indfun) _) ->
                       let (safe_5, fv_subst) = translateIndFunFreeVar scope_env scals_env new_indfun
                           safe_2 = S.member x_mem allocs_env
                       in  if safe_5 && safe_2
                           then let mem_info = Coalesced k mblk fv_subst
                                    info' = info { vartab = M.insert b mem_info vtab }
                                in  (M.insert m_b info' acc, inhb)
                           else failed
             _ -> (acc,inhb)
        ) (act_coal,inhb_coal)

-- | transfers coalescing from if-pattern to then|else body result
--   in the active coalesced table. The transfer involves, among
--   others, inserting @(r,m_r)@ in the optimistically-dependency
--   set of @m_b@'s entry and inserting @(b,m_b)@ in the opt-deps
--   set of @m_r@'s entry. Meaning, ultimately, @m_b@ can be merged
--   if @m_r@ can be merged (and vice-versa). This is checked by a
--   fix point iteration at the function-definition level.
transferCoalsToBody :: CoalsTab
                      -> (VName, VName, VName, VName)
                      -> CoalsTab
transferCoalsToBody activeCoals_tab (m_b, b, r, m_r) =
  -- the @Nothing@ pattern for the two @case ... of@ cannot happen
  -- because they were already chaked in @findMemBodyResult@
  case M.lookup m_b activeCoals_tab of
    Nothing -> Exc.assert False activeCoals_tab -- cannot happen
    Just etry@(CoalsEntry m_x ind_x als_x vtab opts_x) ->
      case M.lookup b vtab of
        Nothing -> Exc.assert False activeCoals_tab -- cannot happen
        Just (Coalesced knd (MemBlock btp shp _ ind_b) subst_b) ->
          -- by definition of if-stmt, r and b have the same basic type, shape and
          -- index function, hence, for example, do not need to rebase
          let ind_r = ind_b
              mem_info = Coalesced knd (MemBlock btp shp m_x ind_r) subst_b
          in  if m_r == m_b -- already unified, just add binding for @r@
              then let etry' = etry { optdeps = M.insert r m_r opts_x
                                    , vartab  = M.insert r mem_info vtab }
                   in  M.insert m_r etry' activeCoals_tab
              else -- make them both optimistically depend on each other
                   let opts_x_new = M.insert r m_r opts_x
                       coal_etry  = CoalsEntry m_x ind_x als_x (M.singleton r mem_info) $
                                    M.insert b m_b opts_x
                   in  M.insert m_b (etry{optdeps = opts_x_new}) $
                       M.insert m_r coal_etry activeCoals_tab


updateIndFunSlice :: ExpMem.IxFun -> Slice SubExp -> ExpMem.IxFun
updateIndFunSlice ind_fun slice_x =
  let slice_x' = map (fmap (primExpFromSubExp (IntType Int32))) slice_x
  in  IxFun.slice ind_fun slice_x'

notInPlace :: PatElem (Aliases ExpMem.ExplicitMemory) -> Bool
notInPlace pel = case patElemBindage pel of
                  BindVar       -> True
                  BindInPlace{} -> False
