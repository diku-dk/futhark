{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Futhark.Optimise.MemBlockCoalesce.DataStructs
       ( Coalesced(..),CoalescedKind(..), ArrayMemBound(..), AllocTab
       , V2MemTab, AliasTab, LUTabFun, LUTabPrg, ScalarTab,  CoalsTab
       , CoalsEntry(..), FreeVarSubsts, LmadRef, MemRefs(..), AccsSum(..)
       , BotUpEnv(..), InhibitTab
       , aliasTransClos, updateAliasing, getNamesFromSubExps, unionCoalsEntry
       , getArrMemAssocFParam, getScopeMemInfo, prettyCoalTab, mem_empty
       , createsNewArrOK, getArrMemAssoc, getUniqueMemFParam, unionMemRefs
       , markFailedCoal
       )
       where


import Prelude
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Futhark.Util.Pretty hiding (float, line, sep, string, (</>), (<|>))

--import Debug.Trace

import Futhark.IR.Aliases
import qualified Futhark.IR.SeqMem as ExpMem
import qualified Futhark.IR.Mem.IxFun as IxFun

type LmadRef = IxFun.LMAD (ExpMem.TPrimExp Int64 VName)
data AccsSum = Top | Over (S.Set LmadRef)

data MemRefs = MemRefs { dstrefs :: AccsSum
                       , srcwrts :: AccsSum
                       }

mem_empty :: MemRefs
mem_empty = MemRefs (Over mempty) (Over mempty)


data CoalescedKind = Ccopy -- let x    = copy b^{lu}
                   | InPl  -- let x[i] = b^{lu}
                   | Conc  -- let x    = concat(a, b^{lu})
                   | Trans -- transitive, i.e., other variables aliased with b.
data ArrayMemBound = MemBlock PrimType Shape VName ExpMem.IxFun

type FreeVarSubsts = M.Map VName (ExpMem.TPrimExp Int64 VName) --(ExpMem.PrimExp VName)

-- | Coalesced Access Entry
data Coalesced = Coalesced CoalescedKind -- the kind of coalescing
                           ArrayMemBound -- destination mem_block info @f_m_x[i]@ (must be ArrayMem)
                           -- (Maybe ExpMem.IxFun) -- the inverse ixfun of a coalesced array, such that
                           --                     --  ixfuns can be correctly constructed for aliases; 
                           FreeVarSubsts -- substitutions for free vars in index function

data CoalsEntry = CoalsEntry{ dstmem :: VName
                            -- ^ destination memory block
                            , dstind :: ExpMem.IxFun
                            -- ^ index function of the destination (used for rebasing)
                            , alsmem :: Names
                            -- ^ aliased destination memory blocks can appear
                            --   due to repeated (optimistic) coalescing.
                            , vartab :: M.Map VName Coalesced
                            -- ^ per variable-name coalesced entries
                            , optdeps:: M.Map VName VName
                            -- ^ keys are variable names, values are memblock names;
                            --   it records optimistically added coalesced nodes, e.g.,
                            --   in the case of if-then-else expressions. For example:
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
                            --   unsuccessful. 
                            --   Storing @m_x@ would probably be sufficient if memory would
                            --     not be reused--e.g., by register allocation on arrays--the
                            --     @x@ discriminates between memory being reused across semantically
                            --     different arrays (searched in @vartab@ field).
                            , memrefs :: MemRefs
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

type InhibitTab = M.Map VName Names
-- ^ inhibited memory-block mergings from the key (memory block)
--   to the value (set of memory blocks)

data BotUpEnv = BotUpEnv { scals :: ScalarTab
                         -- ^ maps scalar variables to theirs PrimExp expansion
                         , activeCoals :: CoalsTab
                         -- ^ optimistic coalescing info
                         , successCoals :: CoalsTab
                         -- ^ committed (successfull) coalescing info
                         , inhibit :: InhibitTab
                         -- ^ the coalescing failures from this pass
                         }


instance Pretty (AccsSum) where
  --show :: AccsSum -> String
  ppr Top = "Top"
  ppr (Over a) = "Access-Overestimate: " <> ppr a

instance Pretty (MemRefs) where
  --show :: MemRefs -> String
  ppr (MemRefs a b) = "( Use-Sum:" <+> ppr a <+> "Write-Sum:" <+> ppr b <> ")"

unionMemRefs :: MemRefs -> MemRefs -> MemRefs
unionMemRefs (MemRefs d1 s1) (MemRefs d2 s2) =
  MemRefs (unionAccss d1 d2) (unionAccss s1 s2)
  where
    unionAccss :: AccsSum -> AccsSum -> AccsSum
    unionAccss Top _ = Top
    unionAccss _ Top = Top
    unionAccss (Over a) (Over b) =
      Over $ S.union a b

unionCoalsEntry :: CoalsEntry -> CoalsEntry -> CoalsEntry
unionCoalsEntry etry1 (CoalsEntry dstmem2 dstind2  alsmem2 vartab2 optdeps2 memrefs2) =
  if dstmem etry1 /= dstmem2 || dstind etry1 /= dstind2
  then etry1
  else etry1 { alsmem = alsmem  etry1 <> alsmem2
             , optdeps= optdeps etry1 `M.union` optdeps2
             , vartab = vartab  etry1 `M.union` vartab2
             , memrefs= unionMemRefs (memrefs etry1) memrefs2
             }

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
         Just coale->
           let failed_set = case M.lookup src_mem inhb_tab of
                              Nothing  -> oneName (dstmem coale)
                              Just fld -> fld <> oneName (dstmem coale)
           in  ( M.delete src_mem coal_tab
               , M.insert src_mem failed_set inhb_tab )

-- | A poor attempt at a pretty printer of the Coalescing Table
prettyCoalTab :: CoalsTab -> String
prettyCoalTab tab =
  let list_tups = map (\(m_b, CoalsEntry md _ als vtab deps (MemRefs d s)) ->
                          (m_b, md, namesToList als, M.keys vtab, M.toList deps, (d,s))
                      ) $ M.toList tab
  in  pretty list_tups
