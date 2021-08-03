{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
    LmadRef,
    MemRefs (..),
    AccessSummary (..),
    BotUpEnv (..),
    InhibitTab,
    aliasTransClos,
    updateAliasing,
    unionCoalsEntry,
    getArrMemAssocFParam,
    getScopeMemInfo,
    prettyCoalTab,
    createsNewArrOK,
    getArrMemAssoc,
    getUniqueMemFParam,
    markFailedCoal,
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Futhark.IR.Aliases
import qualified Futhark.IR.Mem.IxFun as IxFun
import qualified Futhark.IR.SeqMem as ExpMem
import Futhark.Util.Pretty hiding (float, line, sep, string, (</>), (<|>))
import Prelude

-- | An LMAD specialized to TPrimExps (a typed primexp)
type LmadRef = IxFun.LMAD (ExpMem.TPrimExp Int64 VName)

-- | Summary of all memory accesses at a given point in the code
data AccessSummary
  = -- | The access summary was statically undeterminable, for instance by
    -- having multiple lmads. In this case, we should conservatively avoid all
    -- coalescing.
    Undeterminable
  | -- | A conservative estimate of the set of accesses up until this point.
    Set (S.Set LmadRef)

instance Semigroup AccessSummary where
  Undeterminable <> _ = Undeterminable
  _ <> Undeterminable = Undeterminable
  (Set a) <> (Set b) =
    Set $ S.union a b

instance Monoid AccessSummary where
  mempty = Set mempty

data MemRefs = MemRefs
  { dstrefs :: AccessSummary,
    srcwrts :: AccessSummary
  }

instance Semigroup MemRefs where
  m1 <> m2 =
    MemRefs (dstrefs m1 <> dstrefs m2) (srcwrts m1 <> srcwrts m2)

instance Monoid MemRefs where
  mempty = MemRefs mempty mempty

data CoalescedKind
  = -- | let x    = copy b^{lu}
    CopyCoal
  | -- | let x[i] = b^{lu}
    InPlaceCoal
  | -- | let x    = concat(a, b^{lu})
    ConcatCoal
  | -- | transitive, i.e., other variables aliased with b.
    TransitiveCoal

data ArrayMemBound = MemBlock PrimType Shape VName ExpMem.IxFun

type FreeVarSubsts = M.Map VName (ExpMem.TPrimExp Int64 VName) --(ExpMem.PrimExp VName)

-- | Coalesced Access Entry
data Coalesced
  = Coalesced
      CoalescedKind
      -- ^ the kind of coalescing
      ArrayMemBound
      -- ^ destination mem_block info @f_m_x[i]@ (must be ArrayMem)
      -- (Maybe ExpMem.IxFun) -- the inverse ixfun of a coalesced array, such that
      --                     --  ixfuns can be correctly constructed for aliases;
      FreeVarSubsts
      -- ^ substitutions for free vars in index function

data CoalsEntry = CoalsEntry
  { -- | destination memory block
    dstmem :: VName,
    -- | index function of the destination (used for rebasing)
    dstind :: ExpMem.IxFun,
    -- | aliased destination memory blocks can appear
    --   due to repeated (optimistic) coalescing.
    alsmem :: Names,
    -- | per variable-name coalesced entries
    vartab :: M.Map VName Coalesced,
    -- | keys are variable names, values are memblock names;
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
    optdeps :: M.Map VName VName,
    memrefs :: MemRefs
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

type ScalarTab = M.Map VName (ExpMem.PrimExp VName)
-- ^ maps a variable name to its PrimExp scalar expression

type CoalsTab = M.Map VName CoalsEntry
-- ^ maps a memory-block name to a Map in which each variable
--   associated to that memory block is bound to its @Coalesced@ info.

type InhibitTab = M.Map VName Names
-- ^ inhibited memory-block mergings from the key (memory block)
--   to the value (set of memory blocks)

data BotUpEnv = BotUpEnv
  { -- | maps scalar variables to theirs PrimExp expansion
    scals :: ScalarTab,
    -- | optimistic coalescing info
    activeCoals :: CoalsTab,
    -- | committed (successfull) coalescing info
    successCoals :: CoalsTab,
    -- | the coalescing failures from this pass
    inhibit :: InhibitTab
  }

instance Pretty AccessSummary where
  --show :: AccessSummary -> String
  ppr Undeterminable = "Undeterminable"
  ppr (Set a) = "Access-Set:" <+/> ppr a <+/> " "

instance Pretty MemRefs where
  --show :: MemRefs -> String
  ppr (MemRefs a b) = "( Use-Sum:" <+> ppr a <+> "Write-Sum:" <+> ppr b <> ")"

instance Pretty CoalescedKind where
  ppr CopyCoal = "Copy"
  ppr InPlaceCoal = "InPlace"
  ppr ConcatCoal = "Concat"
  ppr TransitiveCoal = "Transitive"

instance Pretty ArrayMemBound where
  ppr (MemBlock ptp shp m_nm ixfn) =
    "{" <> ppr ptp <> "," <+> ppr shp <> "," <+> ppr m_nm <> "," <+/> ppr ixfn <> "}"

instance Pretty Coalesced where
  ppr (Coalesced knd mbd subs) =
    "(Kind:" <+> ppr knd <> ", membds:" <+> ppr mbd <> ", subs:" <+> ppr subs <> ")" <+> "\n"

instance Pretty CoalsEntry where
  ppr etry =
    "{" <+/> "Dstmem:" <+> ppr (dstmem etry)
      <> ", AliasMems:" <+> ppr (alsmem etry)
      <+/> ", optdeps:" <+> ppr (optdeps etry)
      <+/> ", memrefs:" <+> ppr (memrefs etry)
      <+/> ", vartab:" <+> ppr (vartab etry)
      <+/> "}"
      <+/> "\n"

unionCoalsEntry :: CoalsEntry -> CoalsEntry -> CoalsEntry
unionCoalsEntry etry1 (CoalsEntry dstmem2 dstind2 alsmem2 vartab2 optdeps2 memrefs2) =
  if dstmem etry1 /= dstmem2 || dstind etry1 /= dstind2
    then etry1
    else
      etry1
        { alsmem = alsmem etry1 <> alsmem2,
          optdeps = optdeps etry1 <> optdeps2,
          vartab = vartab etry1 <> vartab2,
          memrefs = memrefs etry1 <> memrefs2
        }

aliasTransClos :: AliasTab -> Names -> Names
aliasTransClos alstab args =
  foldl (<>) args $ mapMaybe (`M.lookup` alstab) $ namesToList args

updateAliasing ::
  AliasTab ->
  Pat (Aliases ExpMem.SeqMem) ->
  -- | Maybe the old variable of an in-place update
  Maybe VName ->
  AliasTab
updateAliasing stab pat m_ip =
  foldl updateTab stab $
    map addMaybe $ patElems pat
  where
    addMaybe a = if not $ patElemName a `nameIn` freeIn pat then (a, m_ip) else (a, Nothing)
    -- Compute the transitive closure of current pattern
    -- name by concating all its aliases entries in stabb.
    -- In case of an IN-PLACE update (`mb_v` not Nothing)
    -- add the previous name to the alias set of the new one.
    updateTab :: AliasTab -> (PatElemT (LetDec (Aliases ExpMem.SeqMem)), Maybe VName) -> AliasTab
    updateTab stabb (patel, mb_v) =
      let (al, _) = patElemDec patel
          al_nms = case mb_v of
            Nothing -> unAliases al
            Just v -> unAliases al <> oneName v
          al_trns = foldl (<>) al_nms $ mapMaybe (`M.lookup` stabb) $ namesToList al_nms
       in if al_trns == mempty
            then stabb
            else M.insert (patElemName patel) al_trns stabb

getArrMemAssoc :: Pat (Aliases ExpMem.SeqMem) -> [(VName, ArrayMemBound)]
getArrMemAssoc pat =
  mapMaybe
    ( \patel -> case snd $ patElemDec patel of
        (ExpMem.MemArray tp shp _ (ExpMem.ArrayIn mem_nm indfun)) ->
          Just (patElemName patel, MemBlock tp shp mem_nm indfun)
        ExpMem.MemMem _ -> Nothing
        ExpMem.MemPrim _ -> Nothing
        ExpMem.MemAcc {} -> Nothing
    )
    $ patElems pat

getArrMemAssocFParam :: [FParam (Aliases ExpMem.SeqMem)] -> [(VName, ArrayMemBound)]
getArrMemAssocFParam =
  mapMaybe
    ( \param -> case paramDec param of
        (ExpMem.MemArray tp shp _ (ExpMem.ArrayIn mem_nm indfun)) ->
          Just (paramName param, MemBlock tp shp mem_nm indfun)
        ExpMem.MemMem _ -> Nothing
        ExpMem.MemPrim _ -> Nothing
        ExpMem.MemAcc {} -> Nothing
    )

getUniqueMemFParam :: [FParam (Aliases ExpMem.SeqMem)] -> M.Map VName Space
getUniqueMemFParam params =
  let mems = mapMaybe (\el -> getMemMem (paramName el) (paramDec el)) params
      upms = namesFromList $ mapMaybe (getMemArray . paramDec) params
   in M.fromList $ filter (\(k, _) -> nameIn k upms) mems
  where
    getMemMem nm (ExpMem.MemMem sp) = Just (nm, sp)
    getMemMem _ _ = Nothing
    getMemArray (ExpMem.MemArray _ _ Unique (ExpMem.ArrayIn mem_nm _)) = Just mem_nm
    getMemArray _ = Nothing

getScopeMemInfo ::
  VName ->
  Scope (Aliases ExpMem.SeqMem) ->
  Maybe ArrayMemBound
getScopeMemInfo r scope_env0 =
  case M.lookup r scope_env0 of
    Just (LetName (_, ExpMem.MemArray tp shp _ (ExpMem.ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
    Just (FParamName (ExpMem.MemArray tp shp _ (ExpMem.ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
    Just (LParamName (ExpMem.MemArray tp shp _ (ExpMem.ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
    _ -> Nothing

createsNewArrOK :: Exp (Aliases ExpMem.SeqMem) -> Bool
createsNewArrOK (BasicOp Replicate {}) = True
createsNewArrOK (BasicOp Iota {}) = True
createsNewArrOK (BasicOp Manifest {}) = True
createsNewArrOK (BasicOp ExpMem.Copy {}) = True
createsNewArrOK (BasicOp Concat {}) = True
createsNewArrOK (BasicOp ArrayLit {}) = True
createsNewArrOK (BasicOp Scratch {}) = True
createsNewArrOK (Op _) = True
createsNewArrOK _ = False

-- | Memory-block removal from active-coalescing table
--   should only be handled via this function, it is easy
--   to run into infinite execution problem; i.e., the
--   fix-pointed iteration of coalescing transformation
--   assumes that whenever a coalescing fails it is
--   recorded in the @inhibit@ table.
markFailedCoal ::
  (CoalsTab, InhibitTab) ->
  VName ->
  (CoalsTab, InhibitTab)
markFailedCoal (coal_tab, inhb_tab) src_mem =
  case M.lookup src_mem coal_tab of
    Nothing -> (coal_tab, inhb_tab)
    Just coale ->
      let failed_set = case M.lookup src_mem inhb_tab of
            Nothing -> oneName (dstmem coale)
            Just fld -> fld <> oneName (dstmem coale)
       in trace
            ("Failed Coalesce: " ++ pretty src_mem ++ "->" ++ pretty (dstmem coale))
            ( M.delete src_mem coal_tab,
              M.insert src_mem failed_set inhb_tab
            )

-- | A poor attempt at a pretty printer of the Coalescing Table
prettyCoalTab :: CoalsTab -> String
prettyCoalTab tab =
  let list_tups =
        map
          ( \(m_b, CoalsEntry md _ als vtab deps (MemRefs d s)) ->
              (m_b, md, namesToList als, M.keys vtab, M.toList deps, (d, s))
          )
          $ M.toList tab
   in pretty list_tups
