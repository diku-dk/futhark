{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.MemBlockCoalesce.DataStructs
  ( Coalesced (..),
    CoalescedKind (..),
    ArrayMemBound (..),
    AllocTab,
    V2MemTab,
    AliasTab,
    LUTabFun,
    LUTabPrg,
    ScalarTab,
    CoalsTab,
    CoalsEntry (..),
    FreeVarSubsts,
    aliasTransClos,
    updateAliasing,
    getNamesFromSubExps,
    unionCoalsEntry,
    getArrMemAssocFParam,
    createsAliasedArrOK,
    getScopeMemInfo,
    prettyCoalTab,
    createsNewArrIK,
    createsNewArrOK,
    getArrMemAssoc,
    getUniqueMemFParam,
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
--import Debug.Trace

import Futhark.IR.Aliases
import qualified Futhark.IR.SeqMem as Mem
import Prelude

data CoalescedKind
  = Ccopy -- let x    = copy b^{lu}
  | InPl -- let x[i] = b^{lu}
  | Conc -- let x    = concat(a, b^{lu})
  | Trans -- transitive, i.e., other variables aliased with b.

data ArrayMemBound = MemBlock PrimType Shape VName Mem.IxFun

type FreeVarSubsts = M.Map VName (Mem.PrimExp VName)

-- | Coalesced Access Entry
data Coalesced
  = Coalesced
      CoalescedKind -- the kind of coalescing
      ArrayMemBound -- destination mem_block info @f_m_x[i]@ (must be ArrayMem)
      FreeVarSubsts -- substitutions for free vars in index function

data CoalsEntry = CoalsEntry
  { -- | destination memory block
    dstmem :: VName,
    -- | index function of the destination (used for rebasing)
    dstind :: Mem.IxFun,
    -- | aliased destination memory blocks can appear
    --   due to repeated (optimistical) coalescing.
    alsmem :: Names,
    -- | per variable-name coalesced entries
    vartab :: M.Map VName Coalesced,
    -- | keys are variable names, values are memblock names;
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
    optdeps :: M.Map VName VName
  }

type AllocTab = Names --M.Map VName SubExp

-- ^ the allocatted memory blocks

type V2MemTab = M.Map VName ArrayMemBound
-- ^ maps array-variable names to their memory block info (including index function)

type AliasTab = M.Map VName Names
-- ^ maps a variable or memory block to its aliases

type LUTabFun = M.Map VName Names
-- ^ maps a name indentifying a stmt to the last uses in that stmt

type LUTabPrg = M.Map Name LUTabFun
-- ^ maps function names to last-use tables

type ScalarTab = M.Map VName (Mem.PrimExp VName)
-- ^ maps a variable name to its PrimExp scalar expression

type CoalsTab = M.Map VName CoalsEntry
-- ^ maps a memory-block name to a Map in which each variable
--   associated to that memory block is bound to its @Coalesced@ info.

unionCoalsEntry :: CoalsEntry -> CoalsEntry -> CoalsEntry
unionCoalsEntry etry1 (CoalsEntry dstmem2 dstind2 alsmem2 vartab2 optdeps2) =
  if dstmem etry1 /= dstmem2 || dstind etry1 /= dstind2
    then etry1
    else
      etry1
        { alsmem = alsmem etry1 <> alsmem2,
          optdeps = optdeps etry1 <> optdeps2,
          vartab = vartab etry1 <> vartab2
        }

getNamesFromSubExps :: [SubExp] -> [VName]
getNamesFromSubExps =
  mapMaybe
    ( \case
        Constant _ -> Nothing
        Var nm -> Just nm
    )

aliasTransClos :: AliasTab -> Names -> Names
aliasTransClos alstab args =
  foldlNames
    ( \acc x -> case M.lookup x alstab of
        Nothing -> acc
        Just al -> acc <> al
    )
    args
    args

updateAliasing :: AliasTab -> Pattern (Aliases Mem.SeqMem) -> AliasTab
updateAliasing = undefined

-- updateAliasing :: AliasTab -> Pattern (Aliases Mem.SeqMem) -> AliasTab
-- updateAliasing stab pat =
--   foldl
--     ( \stabb patel ->
--         -- Compute the transitive closure of current pattern
--         -- name by concating all its aliases entries in stabb.
--         -- In case of an IN-PLACE update, add the previous name
--         -- to the alias set of the new one.
--         let (al, _) = patElemAttr patel
--             al_nms0 = unNames al
--             -- ToDo: in-place updates may not be treated as aliases
--             --       and they should with customized LastUse
--             --       but we have changed representation and
--             --       "bindage" does not exist anymore, i.e.,
--             --       not related to pattern
--             al_nms = case patElemBindage patel of
--               BindVar -> al_nms0
--               BindInPlace _ nm _ -> S.insert nm al_nms0

--             --al_nms = al_nms0
--             al_trns =
--               S.foldl'
--                 ( \acc x -> case M.lookup x stabb of
--                     Nothing -> acc
--                     Just aal -> acc `S.union` aal
--                 )
--                 al_nms
--                 al_nms
--          in -- al_trns' = trace ("ALIAS Pattern: "++(pretty (patElemName patel))++" aliases: "++pretty (S.toList al_trns)) al_trns
--             if null al_trns
--               then stabb
--               else M.insert (patElemName patel) al_trns stabb
--     )
--     stab
--     $ patternContextElements pat ++ patternValueElements pat

getArrMemAssoc :: Pattern (Aliases Mem.SeqMem) -> [(VName, ArrayMemBound, a)]
getArrMemAssoc = undefined

-- getArrMemAssoc :: Pattern (Aliases Mem.SeqMem) -> [(VName, ArrayMemBound, Bindage)]
-- getArrMemAssoc pat =
--   mapMaybe
--     ( \patel -> case snd $ patElemAttr patel of
--         (Mem.MemArray tp shp _ (Mem.ArrayIn mem_nm indfun)) ->
--           -- let mem_nm' = trace ("MemLore: "++(pretty (patElemName patel))++" is ArrayMem: "++pretty tp++" , "++pretty shp++" , "++pretty u++" , "++pretty mem_nm++" , "++pretty indfun++" ("++pretty l1++") ") mem_nm
--           Just (patElemName patel, MemBlock tp shp mem_nm indfun, patElemBindage patel)
--         Mem.MemMem _ _ -> Nothing
--         Mem.MemPrim _ -> Nothing
--     )
--     $ patternValueElements pat

getArrMemAssocFParam :: [FParam (Aliases Mem.SeqMem)] -> [(VName, ArrayMemBound)]
getArrMemAssocFParam =
  mapMaybe
    ( \param -> case paramDec param of
        (Mem.MemArray tp shp _ (Mem.ArrayIn mem_nm indfun)) ->
          Just (paramName param, MemBlock tp shp mem_nm indfun)
        Mem.MemMem _ -> Nothing
        Mem.MemPrim _ -> Nothing
    )

getUniqueMemFParam :: [FParam (Aliases Mem.SeqMem)] -> M.Map VName (SubExp, Space)
getUniqueMemFParam params =
  let mems =
        mapMaybe
          ( \el -> case paramDec el of
              Mem.MemMem sp -> Just (paramName el, (undefined, sp))
              _ -> Nothing
          )
          params
      upms =
        S.fromList $
          mapMaybe
            ( \el -> case paramDec el of
                Mem.MemArray _ _ Unique (Mem.ArrayIn mem_nm _) ->
                  Just mem_nm
                _ -> Nothing
            )
            params
   in M.fromList $ filter (\(k, _) -> S.member k upms) mems

getScopeMemInfo ::
  VName ->
  Scope (Aliases Mem.SeqMem) ->
  Maybe ArrayMemBound
getScopeMemInfo r scope_env0 =
  case M.lookup r scope_env0 of
    Just (LetName (_, Mem.MemArray tp shp _ (Mem.ArrayIn m idx))) ->
      Just (MemBlock tp shp m idx)
    Just (FParamName (Mem.MemArray tp shp _ (Mem.ArrayIn m idx))) ->
      Just (MemBlock tp shp m idx)
    Just (LParamName (Mem.MemArray tp shp _ (Mem.ArrayIn m idx))) ->
      Just (MemBlock tp shp m idx)
    _ -> Nothing

createsNewArrOK :: Exp (Aliases Mem.SeqMem) -> Bool
createsNewArrOK (BasicOp Replicate {}) = True
createsNewArrOK (BasicOp Iota {}) = True
createsNewArrOK (BasicOp Manifest {}) = True
createsNewArrOK (BasicOp Mem.Copy {}) = True
createsNewArrOK (BasicOp Concat {}) = True
createsNewArrOK (BasicOp ArrayLit {}) = True
createsNewArrOK (Op (Mem.Inner ())) = True
createsNewArrOK _ = False

createsNewArrIK = undefined

{--
createsNewArrIK :: Exp (Aliases Mem.InKernel) -> Bool
createsNewArrIK (Op (Mem.Inner Mem.GroupReduce{})) = True
createsNewArrIK (Op (Mem.Inner Mem.GroupScan{})) = True
createsNewArrIK (Op (Mem.Inner Mem.GroupStream{})) = True
createsNewArrIK (Op (Mem.Inner Mem.Combine{})) = True
createsNewArrIK (BasicOp Partition{}) = True
createsNewArrIK (BasicOp Replicate{}) = True
createsNewArrIK (BasicOp Iota{}) = True
createsNewArrIK (BasicOp Manifest{}) = True
createsNewArrIK (BasicOp Mem.Copy{}) = True
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
createsAliasedArrOK :: Exp (Aliases Mem.SeqMem) -> Maybe VName --Mem.IxFun
createsAliasedArrOK (BasicOp (Reshape _ arr_nm)) = Just arr_nm
createsAliasedArrOK (BasicOp (SubExp (Var arr_nm))) = Just arr_nm
--createsAliasedArrOK (BasicOp (Rearrange _ _ arr_nm)) = Just arr_nm
--createsAliasedArrOK (BasicOp (Rotate    _ _ arr_nm)) = Just arr_nm
createsAliasedArrOK _ = Nothing

prettyCoalTab :: CoalsTab -> String
prettyCoalTab tab =
  let list_tups =
        map
          ( \(m_b, CoalsEntry md _ als vtab deps) ->
              (m_b, md, namesToList als, M.keys vtab, M.toList deps)
          )
          $ M.toList tab
   in pretty list_tups
