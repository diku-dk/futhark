{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.MemBlockCoalesce.DataStructs
       ( Coalesced(..),CoalescedKind(..), ArrayMemBound(..), AllocTab
       , V2MemTab, AliasTab, LUTabFun, LUTabPrg, ScalarTab,  CoalsTab
       , CoalsEntry(..), FreeVarSubsts
       , aliasTransClos, updateAliasing, getNamesFromSubExps, unionCoalsEntry
       , getArrMemAssocFParam, createsAliasedArrOK, getScopeMemInfo, prettyCoalTab
       , createsNewArrOK, getArrMemAssoc, getUniqueMemFParam )
       where


import Prelude
import Data.Maybe
import qualified Data.Map.Strict as M

--import Debug.Trace

import Futhark.IR.Aliases
import qualified Futhark.IR.SeqMem as ExpMem

data CoalescedKind = Ccopy -- let x    = copy b^{lu}
                   | InPl  -- let x[i] = b^{lu}
                   | Conc  -- let x    = concat(a, b^{lu})
                   | Trans -- transitive, i.e., other variables aliased with b.
data ArrayMemBound = MemBlock PrimType Shape VName ExpMem.IxFun

type FreeVarSubsts = M.Map VName (ExpMem.PrimExp VName)

-- | Coalesced Access Entry
data Coalesced = Coalesced CoalescedKind -- the kind of coalescing
                           ArrayMemBound -- destination mem_block info @f_m_x[i]@ (must be ArrayMem)
                           FreeVarSubsts -- substitutions for free vars in index function

data CoalsEntry = CoalsEntry{ dstmem :: VName
                            -- ^ destination memory block
                            , dstind :: ExpMem.IxFun
                            -- ^ index function of the destination (used for rebasing)
                            , alsmem :: Names
                            -- ^ aliased destination memory blocks can appear
                            --   due to repeated (optimistical) coalescing.
                            , vartab :: M.Map VName Coalesced
                            -- ^ per variable-name coalesced entries
                            , optdeps:: M.Map VName VName
                            -- ^ keys are variable names, values are memblock names;
                            --   it records optimistically added coalesced nodes,
                            --   e.g., in the case of if-then-else expressions
                            --   The case below cannot happen because an in-place
                            --   assigned expression must create a new array,
                            --   but it is good for the purpose of exposition.
                            --       @x    = map f a@
                            --       @.. use of y ..@
                            --       @b    = map g a@
                            --       @x[i] = b      @
                            --       @y[k] = x      @
                            --   the coalescing of @b@ in @x[i]@ succeeds, but
                            --   is dependent of the success of the coalescing
                            --   of @x@ in @y[k]@, which fails in this case
                            --   because @y@ is used before the new array creation
                            --   of @x = map f@. Hence @optdeps@ of the @m_b@ CoalsEntry
                            --   records @x -> m_x@ and at the end of analysis it is removed
                            --   from the successfully coalesced table if @m_x@ is
                            --   unsuccessful. Ok, this case cannot happen because
                            --   you need a copying.
                            }

type AllocTab = Names--M.Map VName SubExp
-- ^ the allocatted memory blocks
type V2MemTab = M.Map VName ArrayMemBound
-- ^ maps array-variable names to their memory block info (including index function)
type AliasTab = M.Map VName Names
-- ^ maps a variable or memory block to its aliases
type LUTabFun = M.Map VName Names
-- ^ maps a name indentifying a stmt to the last uses in that stmt
type LUTabPrg = M.Map Name  LUTabFun
-- ^ maps function names to last-use tables
type ScalarTab= M.Map VName (ExpMem.PrimExp VName)
-- ^ maps a variable name to its PrimExp scalar expression
type CoalsTab = M.Map VName CoalsEntry
-- ^ maps a memory-block name to a Map in which each variable
--   associated to that memory block is bound to its @Coalesced@ info.

unionCoalsEntry :: CoalsEntry -> CoalsEntry -> CoalsEntry
unionCoalsEntry etry1 (CoalsEntry dstmem2 dstind2  alsmem2 vartab2 optdeps2) =
  if dstmem etry1 /= dstmem2 || dstind etry1 /= dstind2
  then etry1
  else etry1 { alsmem = alsmem  etry1 <> alsmem2
             , optdeps= optdeps etry1 `M.union` optdeps2
             , vartab = vartab  etry1 `M.union` vartab2 }

getNamesFromSubExps :: [SubExp] -> [VName]
getNamesFromSubExps = mapMaybe var2Maybe
  where
    var2Maybe (Constant _) = Nothing
    var2Maybe (Var nm) = Just nm

aliasTransClos :: AliasTab -> Names -> Names
aliasTransClos alstab args =
  foldl (<>) args $ mapMaybe (`M.lookup` alstab) $ namesToList args

updateAliasing :: AliasTab -> (Pattern (Aliases ExpMem.SeqMem), Maybe VName) -> AliasTab
updateAliasing stab (pat, m_ip) =
  -- ^ `m_ip` is maybe the old variable of an in-place update
  foldl updateTab stab $ map addNothing (patternContextElements pat) ++
                         map (addMaybe m_ip) (patternValueElements pat)
  where
    addMaybe m_v a = (a, m_v)
    addNothing a = (a,Nothing)
    -- Compute the transitive closure of current pattern
    -- name by concating all its aliases entries in stabb.
    -- In case of an IN-PLACE update (`mb_v` not Nothing)
    -- add the previous name to the alias set of the new one.
    updateTab :: AliasTab -> (PatElemT (LetDec (Aliases ExpMem.SeqMem)), Maybe VName) -> AliasTab
    updateTab stabb (patel, mb_v) =
      let (al,_)  = patElemDec patel
          al_nms  = case mb_v of
                      Nothing -> unAliases al
                      Just v  -> unAliases al <> oneName v
          al_trns = foldl (<>) al_nms $ mapMaybe (`M.lookup` stabb) $ namesToList al_nms
      in  if al_trns == mempty then stabb
          else M.insert (patElemName patel) al_trns stabb

getArrMemAssoc :: Pattern (Aliases ExpMem.SeqMem) -> [(VName,ArrayMemBound)]
getArrMemAssoc pat =
  mapMaybe (\patel -> case snd $ patElemDec patel of
                        (ExpMem.MemArray tp shp _ (ExpMem.ArrayIn mem_nm indfun) ) ->
                            -- let mem_nm' = trace ("MemLore: "++(pretty (patElemName patel))++" is ArrayMem: "++pretty tp++" , "++pretty shp++" , "++pretty u++" , "++pretty mem_nm++" , "++pretty indfun++" ("++pretty l1++") ") mem_nm
                            Just (patElemName patel, MemBlock tp shp mem_nm indfun)
                        ExpMem.MemMem _  -> Nothing
                        ExpMem.MemPrim _ -> Nothing
           ) $ patternValueElements pat

getArrMemAssocFParam :: [FParam (Aliases ExpMem.SeqMem)] -> [(VName,ArrayMemBound)]
getArrMemAssocFParam =
  mapMaybe (\param -> case paramDec param of
                        (ExpMem.MemArray tp shp _ (ExpMem.ArrayIn mem_nm indfun)) ->
                            Just (paramName param, MemBlock tp shp mem_nm indfun)
                        ExpMem.MemMem _  -> Nothing
                        ExpMem.MemPrim _ -> Nothing
           )


getUniqueMemFParam :: [FParam (Aliases ExpMem.SeqMem)] -> M.Map VName Space
getUniqueMemFParam params =
  let mems = mapMaybe (\el -> getMemMem (paramName el) (paramDec el)) params
      upms = namesFromList $ mapMaybe (getMemArray . paramDec) params
  in M.fromList $ filter (\ (k,_) -> nameIn k upms) mems
  where
    getMemMem nm (ExpMem.MemMem sp) = Just (nm, sp)
    getMemMem _ _ = Nothing
    getMemArray (ExpMem.MemArray _ _ Unique (ExpMem.ArrayIn mem_nm _)) = Just mem_nm
    getMemArray _ = Nothing

getScopeMemInfo :: VName -> Scope (Aliases ExpMem.SeqMem)
                -> Maybe ArrayMemBound
getScopeMemInfo r scope_env0 =
  case M.lookup r scope_env0 of
    Just (LetName  (_,ExpMem.MemArray tp shp _ (ExpMem.ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
    Just (FParamName (ExpMem.MemArray tp shp _ (ExpMem.ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
    Just (LParamName (ExpMem.MemArray tp shp _ (ExpMem.ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
    _ -> Nothing

createsNewArrOK :: Exp (Aliases ExpMem.SeqMem) -> Bool
createsNewArrOK (BasicOp Replicate{}) = True
createsNewArrOK (BasicOp Iota{}) = True
createsNewArrOK (BasicOp Manifest{}) = True
createsNewArrOK (BasicOp ExpMem.Copy{}) = True
createsNewArrOK (BasicOp Concat{}) = True
createsNewArrOK (BasicOp ArrayLit{}) = True
createsNewArrOK (BasicOp Scratch{}) = True
createsNewArrOK (Op _) = True
--createsNewArrOK (Op (ExpMem.Inner (ExpMem.SegOp _))) = True
--createsNewArrOK (BasicOp Partition{}) = True -- was this removed?
--createsNewArrOK (Op (ExpMem.Inner ExpMem.Kernel{})) = True -- absolete
--KernelsMem
createsNewArrOK _ = False

{--
createsNewArrIK :: Exp (Aliases ExpMem.InKernel) -> Bool
createsNewArrIK (Op (ExpMem.Inner ExpMem.GroupReduce{})) = True
createsNewArrIK (Op (ExpMem.Inner ExpMem.GroupScan{})) = True
createsNewArrIK (Op (ExpMem.Inner ExpMem.GroupStream{})) = True
createsNewArrIK (Op (ExpMem.Inner ExpMem.Combine{})) = True
createsNewArrIK (BasicOp Partition{}) = True
createsNewArrIK (BasicOp Replicate{}) = True
createsNewArrIK (BasicOp Iota{}) = True
createsNewArrIK (BasicOp Manifest{}) = True
createsNewArrIK (BasicOp ExpMem.Copy{}) = True
createsNewArrIK (BasicOp Concat{}) = True
createsNewArrIK (BasicOp ArrayLit{}) = True
createsNewArrIK _ = False
--}

-- | While Rearrange and Rotate create aliased arrays, we
--   do not yet support them because it would mean we have
--   to "revers" the index function, for example to support
--   coalescing in the case below,
--       @let a = map f a0   @
--       @let b = transpose a@
--       @let y[4] = copy(b) @
--   we would need to assign to @a@ as index function, the
--   inverse of the transpose, such that, when creating @b@
--   by transposition we get a directly-mapped array, which
--   is expected by the copying in y[4].
createsAliasedArrOK :: Exp (Aliases ExpMem.SeqMem) -> Maybe VName --ExpMem.IxFun
createsAliasedArrOK (BasicOp (Reshape _ arr_nm)) = Just arr_nm
createsAliasedArrOK (BasicOp (SubExp (Var arr_nm))) = Just arr_nm
createsAliasedArrOK (BasicOp (Rearrange _ arr_nm))  = Just arr_nm
createsAliasedArrOK (BasicOp (Rotate    _ arr_nm))  = Just arr_nm
createsAliasedArrOK (BasicOp (Opaque (Var arr_nm))) = Just arr_nm
-- ToDo: very important is to treat Index, i.e., array slices!
createsAliasedArrOK _ = Nothing

prettyCoalTab :: CoalsTab -> String
prettyCoalTab tab =
  let list_tups = map (\(m_b, CoalsEntry md _ als vtab deps) ->
                          (m_b, md, namesToList als, M.keys vtab, M.toList deps)
                      ) $ M.toList tab
  in  pretty list_tups
