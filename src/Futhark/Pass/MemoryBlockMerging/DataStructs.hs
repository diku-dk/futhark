{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Pass.MemoryBlockMerging.DataStructs
       ( Coalesced(..),CoalescedKind(..), ArrayMemBound(..), AllocTab
       , V2MemTab, AliasTab, LUTabFun, ScalarTab, CoalsTab
       , CoalsEntry(..), FreeVarSubsts
       , aliasTransClos, updateAliasing, getNamesFromSubExps, unionCoalsEntry
       , getArrMemAssocFParam, createsAliasedArrOK, getScopeMemInfo, prettyCoalTab
       , createsNewArrIK, createsNewArrOK, getArrMemAssoc, getUniqueMemFParam )
       where


import Prelude
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Futhark.Representation.Aliases
import qualified Futhark.Representation.ExplicitMemory as ExpMem

data CoalescedKind = Ccopy -- let x    = copy b^{lu}
                   | InPl  -- let x[i] = b^{lu}
                   | Conc  -- let x    = concat(a, b^{lu})
                   | Trans -- transitive, i.e., other variables aliased with b.
  deriving (Show)

data ArrayMemBound = MemBlock PrimType Shape VName ExpMem.IxFun
  deriving (Show)

type FreeVarSubsts = M.Map VName (ExpMem.PrimExp VName)

-- | Coalesced Access Entry
data Coalesced = Coalesced CoalescedKind -- the kind of coalescing
                           ArrayMemBound -- destination mem_block info @f_m_x[i]@ (must be ArrayMem)
                           FreeVarSubsts -- substitutions for free vars in index function
  deriving (Show)

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
                            --   of @x = map f@. Hence @optdeps@ of the @m_b@ entry
                            --   records @m_x@ and at the end of analysis it is removed
                            --   from the successfully coalesced table if @m_x@ is
                            --   unsuccessful. Ok, this case cannot happen because
                            --   you need a copying.
                            } deriving (Show)

type AllocTab = Names--M.Map VName SubExp
-- ^ the allocatted memory blocks
type V2MemTab = M.Map VName ArrayMemBound
-- ^ maps array-variable names to their memory block info (including index function)
type AliasTab = M.Map VName Names
-- ^ maps a variable or memory block to its aliases
type LUTabFun = M.Map VName Names
-- ^ maps a name indentifying a stmt to the last uses in that stmt
type ScalarTab= M.Map VName (ExpMem.PrimExp VName)
-- ^ maps a variable name to its PrimExp scalar expression
type CoalsTab = M.Map VName CoalsEntry
-- ^ maps a memory-block name to a Map in which each variable
--   associated to that memory block is bound to its @Coalesced@ info.

unionCoalsEntry :: CoalsEntry -> CoalsEntry -> CoalsEntry
unionCoalsEntry etry1 (CoalsEntry dstmem2 dstind2  alsmem2 vartab2 optdeps2) =
  if   dstmem etry1 /= dstmem2 || dstind etry1 /= dstind2
  then etry1
  else etry1 { alsmem = alsmem  etry1 `S.union` alsmem2
             , optdeps= optdeps etry1 `M.union` optdeps2
             , vartab = vartab  etry1 `M.union` vartab2 }

getNamesFromSubExps :: [SubExp] -> [VName]
getNamesFromSubExps =
  mapMaybe (\se->case se of
                   Constant _ -> Nothing
                   Var     nm -> Just nm )

aliasTransClos :: AliasTab -> Names -> Names
aliasTransClos alstab args =
  S.foldl' (\acc x -> case M.lookup x alstab of
                        Nothing -> acc
                        Just al -> acc `S.union` al
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
                           BindInPlace _ nm _ -> S.insert nm al_nms0
                al_trns= S.foldl' (\acc x -> case M.lookup x stabb of
                                                Nothing -> acc
                                                Just aal -> acc `S.union` aal
                                   ) al_nms al_nms
                -- al_trns' = trace ("ALIAS Pattern: "++(pretty (patElemName patel))++" aliases: "++pretty (S.toList al_trns)) al_trns
            in  if null al_trns then stabb
                else M.insert (patElemName patel) al_trns stabb
        ) stab $ patternContextElements pat ++ patternValueElements pat


getArrMemAssoc :: Pattern (Aliases ExpMem.ExplicitMemory) -> [(VName,ArrayMemBound,Bindage)]
getArrMemAssoc pat =
  mapMaybe (\patel -> case snd $ patElemAttr patel of
                        (ExpMem.ArrayMem tp shp _ mem_nm indfun) ->
                            --let mem_nm' = trace ("MemLore: "++(pretty (patElemName patel))++" is ArrayMem: "++pretty tp++" , "++pretty shp++" , "++" , "++pretty mem_nm++" , "++pretty indfun) mem_nm
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


getUniqueMemFParam :: [FParam (Aliases ExpMem.ExplicitMemory)] -> M.Map VName (SubExp,Space)
getUniqueMemFParam params =
  let mems = mapMaybe (\el -> case paramAttr el of
                                ExpMem.MemMem sz sp -> Just (paramName el, (sz,sp))
                                _ -> Nothing
                      ) params
      upms = S.fromList $
             mapMaybe (\el -> case paramAttr el of
                                ExpMem.ArrayMem _ _ Unique mem_nm _ ->
                                    Just mem_nm
                                _ -> Nothing
                      ) params
  in M.fromList $ filter (\ (k,_) -> S.member k upms) mems

getScopeMemInfo :: VName -> Scope (Aliases ExpMem.ExplicitMemory)
                -> Maybe ArrayMemBound
getScopeMemInfo r scope_env0 =
  case M.lookup r scope_env0 of
    Just (LetInfo  (_,ExpMem.ArrayMem tp shp _ m idx)) -> Just (MemBlock tp shp m idx)
    Just (FParamInfo (ExpMem.ArrayMem tp shp _ m idx)) -> Just (MemBlock tp shp m idx)
    Just (LParamInfo (ExpMem.ArrayMem tp shp _ m idx)) -> Just (MemBlock tp shp m idx)
    _ -> Nothing

createsNewArrOK :: Exp (Aliases ExpMem.ExplicitMemory) -> Bool
createsNewArrOK (BasicOp Partition{}) = True
createsNewArrOK (BasicOp Replicate{}) = True
createsNewArrOK (BasicOp Iota{}) = True
createsNewArrOK (BasicOp Manifest{}) = True
createsNewArrOK (BasicOp ExpMem.Copy{}) = True
createsNewArrOK (BasicOp Concat{}) = True
createsNewArrOK (BasicOp ArrayLit{}) = True
createsNewArrOK (BasicOp Scratch{}) = True
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
createsAliasedArrOK :: Exp (Aliases ExpMem.ExplicitMemory) -> Maybe VName --ExpMem.IxFun
createsAliasedArrOK (BasicOp (Reshape   _ _ arr_nm)) = Just arr_nm
createsAliasedArrOK (BasicOp (SubExp  (Var arr_nm))) = Just arr_nm
--createsAliasedArrOK (BasicOp (Rearrange _ _ arr_nm)) = Just arr_nm
--createsAliasedArrOK (BasicOp (Rotate    _ _ arr_nm)) = Just arr_nm
createsAliasedArrOK _ = Nothing

prettyCoalTab :: CoalsTab -> String
prettyCoalTab tab =
  let list_tups = map (\(m_b, CoalsEntry md _ als vtab deps) ->
                          (m_b, md, S.toList als, M.keys vtab, M.toList deps)
                      ) $ M.toList tab
  in  pretty list_tups
