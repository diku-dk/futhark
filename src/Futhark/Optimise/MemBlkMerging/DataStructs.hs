{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.MemBlkMerging.DataStructs
       ( Coalesced(..),CoalescedKind(..), ArrayMemBound(..), AllocTab
       , V2MemTab, AliasTab, LUTabFun, LUTabPrg, ScalarTab,  CoalsTab
       , CoalsEntry(..), FreeVarSubsts
       , aliasTransClos, updateAliasing, getNamesFromSubExps, unionCoalsEntry
       , getArrMemAssocFParam, createsAliasedArrOK
       , createsNewArrIK, createsNewArrOK, getArrMemAssoc, getUniqueMemFParam )
       where


import Prelude
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

--import Debug.Trace

import Futhark.Representation.Aliases
import qualified Futhark.Representation.ExplicitMemory as ExpMem

data CoalescedKind = Ccopy -- let x    = copy b^{lu}
                   | InPl  -- let x[i] = b^{lu}
                   | Conc  -- let x    = concat(a, b^{lu})
                   | Trans -- transitive, i.e., other variables aliased with b.
data ArrayMemBound = MemBlock PrimType Shape VName ExpMem.IxFun

type FreeVarSubsts = HM.HashMap VName (ExpMem.PrimExp VName)

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
                            , vartab :: HM.HashMap VName Coalesced
                            -- ^ per variable-name coalesced entries
                            , optdeps:: Names
                            -- ^ records optimistically added coalesced nodes
                            --       @x    = map f a@
                            --       @.. use of y ..@
                            --       @b    = map g a@
                            --       @x[i] = b      @
                            --       @y[k] = x      @
                            --   the coalescing of @b@ in @x[i]@ succeeds, but
                            --   is dependent of the success of the coalescing
                            --   of @x@ in @y[k]@, which fails in this case
                            --   because @y@ is used before the new array creation
                            --   of @x = map f@. Hence @optdeps@ of the @m_b@ entry
                            --   records @m_x@ and at the end of analysis it is removed
                            --   from the successfully coalesced table if @m_x@ is
                            --   unsuccessful.
                            }

type AllocTab = Names--HM.HashMap VName SubExp
-- ^ the allocatted memory blocks
type V2MemTab = HM.HashMap VName ArrayMemBound
-- ^ maps array-variable names to their memory block info (including index function)
type AliasTab = HM.HashMap VName Names
-- ^ maps a variable or memory block to its aliases
type LUTabFun = HM.HashMap VName Names
-- ^ maps a name indentifying a stmt to the last uses in that stmt
type LUTabPrg = HM.HashMap Name  LUTabFun
-- ^ maps function names to last-use tables
type ScalarTab= HM.HashMap VName (ExpMem.PrimExp VName)
-- ^ maps a variable name to its PrimExp scalar expression
type CoalsTab = HM.HashMap VName CoalsEntry
-- ^ maps a memory-block name to a Map in which each variable
--   associated to that memory block is bound to its @Coalesced@ info.

unionCoalsEntry :: CoalsEntry -> CoalsEntry -> CoalsEntry
unionCoalsEntry etry1 (CoalsEntry _ _ _ vartab2 optdeps2) =
  etry1 { vartab = vartab  etry1 `HM.union` vartab2
        , optdeps= optdeps etry1 `HS.union` optdeps2 }


getNamesFromSubExps :: [SubExp] -> [VName]
getNamesFromSubExps =
  mapMaybe (\se->case se of
                   Constant _ -> Nothing
                   Var     nm -> Just nm )

aliasTransClos :: AliasTab -> Names -> Names
aliasTransClos alstab args =
  HS.foldl' (\acc x -> case HM.lookup x alstab of
                        Nothing -> acc
                        Just al -> acc `HS.union` al
            ) args args

updateAliasing :: AliasTab -> Pattern (Aliases ExpMem.ExplicitMemory) -> AliasTab
updateAliasing stab pat =
  foldl (\stabb patel->
            -- Compute the transitive closure of current pattern
            -- name by concating all its aliases entries in stabb.
            -- In case of an IN-PLACE update, add the previous name
            -- to the alias set of the new one.
            let (al,_)  = patElemAttr patel
                al_nms0 = unNames al
                al_nms = case patElemBindage patel of
                           BindVar -> al_nms0
                           BindInPlace _ nm _ -> HS.insert nm al_nms0
                al_trns= HS.foldl' (\acc x -> case HM.lookup x stabb of
                                                Nothing -> acc
                                                Just aal -> acc `HS.union` aal
                                   ) al_nms al_nms
                -- al_trns' = trace ("ALIAS Pattern: "++(pretty (patElemName patel))++" aliases: "++pretty (HS.toList al_trns)) al_trns
            in  if null al_trns then stabb
                else HM.insert (patElemName patel) al_trns stabb
        ) stab $ patternContextElements pat ++ patternValueElements pat


getArrMemAssoc :: Pattern (Aliases ExpMem.ExplicitMemory) -> [(VName,ArrayMemBound,Bindage)]
getArrMemAssoc pat =
  mapMaybe (\patel -> case snd $ patElemAttr patel of
                        (ExpMem.ArrayMem tp shp _ mem_nm indfun) ->
                            -- let mem_nm' = trace ("MemLore: "++(pretty (patElemName patel))++" is ArrayMem: "++pretty tp++" , "++pretty shp++" , "++pretty u++" , "++pretty mem_nm++" , "++pretty indfun++" ("++pretty l1++") ") mem_nm
                            Just (patElemName patel, MemBlock tp shp mem_nm indfun, patElemBindage patel)
                        ExpMem.MemMem _ _ -> Nothing
                        ExpMem.Scalar _   -> Nothing
           ) $ patternValueElements pat

getArrMemAssocFParam :: [FParam (Aliases ExpMem.ExplicitMemory)] -> [(VName,ArrayMemBound)]
getArrMemAssocFParam =
  mapMaybe (\param -> case paramAttr param of
                        (ExpMem.ArrayMem tp shp _ mem_nm indfun) ->
                            Just (paramName param, MemBlock tp shp mem_nm indfun)
                        ExpMem.MemMem _ _ -> Nothing
                        ExpMem.Scalar _   -> Nothing
           )


getUniqueMemFParam :: [FParam (Aliases ExpMem.ExplicitMemory)] -> HM.HashMap VName (SubExp,Space)
getUniqueMemFParam params =
  let mems = mapMaybe (\el -> case paramAttr el of
                                ExpMem.MemMem sz sp -> Just (paramName el, (sz,sp))
                                _ -> Nothing
                      ) params
      upms = HS.fromList $
             mapMaybe (\el -> case paramAttr el of
                                ExpMem.ArrayMem _ _ Unique mem_nm _ ->
                                    Just mem_nm
                                _ -> Nothing
                      ) params
  in HM.fromList $ filter (\ (k,_) -> HS.member k upms) mems

createsNewArrOK :: Exp (Aliases ExpMem.ExplicitMemory) -> Bool
createsNewArrOK (BasicOp Partition{}) = True
createsNewArrOK (BasicOp Replicate{}) = True
createsNewArrOK (BasicOp Iota{}) = True
createsNewArrOK (BasicOp Manifest{}) = True
createsNewArrOK (BasicOp ExpMem.Copy{}) = True
createsNewArrOK (BasicOp Concat{}) = True
createsNewArrOK (BasicOp ArrayLit{}) = True
createsNewArrOK (Op (ExpMem.Inner ExpMem.Kernel{})) = True
createsNewArrOK _ = False

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

createsAliasedArrOK :: Exp (Aliases ExpMem.ExplicitMemory) -> Maybe VName --ExpMem.IxFun
createsAliasedArrOK (BasicOp (Rearrange _ _ arr_nm)) = Just arr_nm
createsAliasedArrOK (BasicOp (Reshape   _ _ arr_nm)) = Just arr_nm
createsAliasedArrOK (BasicOp (Rotate    _ _ arr_nm)) = Just arr_nm
--createsAliasedArrOK (BasicOp (SubExp  (Var arr_nm))) = Just arr_nm
-- funny, with the above uncommented it becomes very complicated.
createsAliasedArrOK _ = Nothing
