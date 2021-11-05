{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.ArrayShortCircuiting.DataStructs
  ( Coalesced (..),
    CoalescedKind (..),
    ArrayMemBound (..),
    AllocTab,
    AliasTab,
    LUTabFun,
    CreatesNewArrOp,
    HasMemBlock,
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
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.IR.SeqMem
import Futhark.Util.Pretty hiding (float, line, sep, string, (</>), (<|>))
import Prelude

-- | An LMAD specialized to TPrimExps (a typed primexp)
type LmadRef = IxFun.LMAD (TPrimExp Int64 VName)

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

instance FreeIn AccessSummary where
  freeIn' Undeterminable = mempty
  freeIn' (Set s) = freeIn' s

data MemRefs = MemRefs
  { -- | The access summary of all references (reads
    -- and writes) to the destination of a coalescing entry
    dstrefs :: AccessSummary,
    -- | The access summary of all writes to the source of a coalescing entry
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

-- | Information about a memory block: type, shape, name and ixfun.
data ArrayMemBound = MemBlock
  { primType :: PrimType,
    shape :: Shape,
    memName :: VName,
    ixfun :: IxFun
  }

-- | Free variable substitutions
type FreeVarSubsts = M.Map VName (TPrimExp Int64 VName)

-- | Coalesced Access Entry
data Coalesced
  = Coalesced
      CoalescedKind
      -- ^ the kind of coalescing
      ArrayMemBound
      -- ^ destination mem_block info @f_m_x[i]@ (must be ArrayMem)
      -- (Maybe IxFun) -- the inverse ixfun of a coalesced array, such that
      --                     --  ixfuns can be correctly constructed for aliases;
      FreeVarSubsts
      -- ^ substitutions for free vars in index function

data CoalsEntry = CoalsEntry
  { -- | destination memory block
    dstmem :: VName,
    -- | index function of the destination (used for rebasing)
    dstind :: IxFun,
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
    -- | Access summaries of uses and writes of destination and source
    -- respectively.
    memrefs :: MemRefs
  }

type AllocTab = Names
-- ^ the allocatted memory blocks

type AliasTab = M.Map VName Names
-- ^ maps a variable or memory block to its aliases

type LUTabFun = M.Map VName Names
-- ^ maps a name indentifying a stmt to the last uses in that stmt

type LUTabPrg = M.Map Name LUTabFun
-- ^ maps function names to last-use tables

type ScalarTab = M.Map VName (PrimExp VName)
-- ^ maps a variable name to its PrimExp scalar expression

type CoalsTab = M.Map VName CoalsEntry
-- ^ maps a memory-block name to a 'CoalsEntry'. Among other things, it contains
--   @vartab@, a map in which each variable associated to that memory block is
--   bound to its 'Coalesced' info.

type InhibitTab = M.Map VName Names
-- ^ inhibited memory-block mergings from the key (memory block)
--   to the value (set of memory blocks).

data BotUpEnv = BotUpEnv
  { -- | maps scalar variables to theirs PrimExp expansion
    scals :: ScalarTab,
    -- | Optimistic coalescing info. We are currently trying to coalesce these
    -- memory blocks.
    activeCoals :: CoalsTab,
    -- | Committed (successfull) coalescing info. These memory blocks have been
    -- successfully coalesced.
    successCoals :: CoalsTab,
    -- | The coalescing failures from this pass. We will no longer try to merge
    -- these memory blocks.
    inhibit :: InhibitTab
  }

instance Pretty AccessSummary where
  ppr Undeterminable = "Undeterminable"
  ppr (Set a) = "Access-Set:" <+/> ppr a <+/> " "

instance Pretty MemRefs where
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

-- | Compute the union of two 'CoalsEntry'. If two 'CoalsEntry' do not refer to
-- the same destination memory and use the same index function, the first
-- 'CoalsEntry' is returned.
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

-- | Get the names of array 'PatElem's in a 'Pat' and the corresponding
-- 'ArrayMemBound' information for each array.
getArrMemAssoc :: PatT (aliases, LetDecMem) -> [(VName, ArrayMemBound)]
getArrMemAssoc pat =
  mapMaybe
    ( \patel -> case snd $ patElemDec patel of
        (MemArray tp shp _ (ArrayIn mem_nm indfun)) ->
          Just (patElemName patel, MemBlock tp shp mem_nm indfun)
        MemMem _ -> Nothing
        MemPrim _ -> Nothing
        MemAcc {} -> Nothing
    )
    $ patElems pat

-- | Get the names of arrays in a list of 'FParam' and the corresponding
-- 'ArrayMemBound' information for each array.
getArrMemAssocFParam :: [Param FParamMem] -> [(VName, ArrayMemBound)]
getArrMemAssocFParam =
  mapMaybe
    ( \param -> case paramDec param of
        (MemArray tp shp _ (ArrayIn mem_nm indfun)) ->
          Just (paramName param, MemBlock tp shp mem_nm indfun)
        MemMem _ -> Nothing
        MemPrim _ -> Nothing
        MemAcc {} -> Nothing
    )

-- | Get memory blocks in a list of 'FParam' that are used for unique arrays in
-- the same list of 'FParam'.
getUniqueMemFParam :: [Param FParamMem] -> M.Map VName Space
getUniqueMemFParam params =
  let mems = M.fromList $ mapMaybe justMem params
      arrayMems = S.fromList $ mapMaybe (justArrayMem . paramDec) params
   in mems `M.restrictKeys` arrayMems
  where
    justMem (Param _ nm (MemMem sp)) = Just (nm, sp)
    justMem _ = Nothing
    justArrayMem (MemArray _ _ Unique (ArrayIn mem_nm _)) = Just mem_nm
    justArrayMem _ = Nothing

class HasMemBlock rep where
  -- | Looks up 'VName' in the given scope. If it is a 'MemArray', return the
  -- 'ArrayMemBound' information for the array.
  getScopeMemInfo :: VName -> Scope rep -> Maybe ArrayMemBound

instance HasMemBlock (Aliases SeqMem) where
  getScopeMemInfo r scope_env0 =
    case M.lookup r scope_env0 of
      Just (LetName (_, MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      Just (FParamName (MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      Just (LParamName (MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      _ -> Nothing

instance HasMemBlock (Aliases GPUMem) where
  getScopeMemInfo r scope_env0 =
    case M.lookup r scope_env0 of
      Just (LetName (_, MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      Just (FParamName (MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      Just (LParamName (MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      _ -> Nothing

-- | @True@ if the expression returns a "fresh" array.
createsNewArrOK :: CreatesNewArrOp (Op rep) => Exp rep -> Bool
createsNewArrOK (BasicOp Replicate {}) = True
createsNewArrOK (BasicOp Iota {}) = True
createsNewArrOK (BasicOp Manifest {}) = True
createsNewArrOK (BasicOp Copy {}) = True
createsNewArrOK (BasicOp Concat {}) = True
createsNewArrOK (BasicOp ArrayLit {}) = True
createsNewArrOK (BasicOp Scratch {}) = True
createsNewArrOK (Op op) = createsNewArrOp op
createsNewArrOK _ = False

class CreatesNewArrOp rep where
  createsNewArrOp :: rep -> Bool

instance CreatesNewArrOp () where
  createsNewArrOp () = False

instance CreatesNewArrOp inner => CreatesNewArrOp (MemOp inner) where
  createsNewArrOp (Alloc _ _) = True
  createsNewArrOp (Inner inner) = createsNewArrOp inner

instance CreatesNewArrOp inner => CreatesNewArrOp (HostOp (Aliases GPUMem) inner) where
  createsNewArrOp (OtherOp op) = createsNewArrOp op
  createsNewArrOp (SegOp (SegMap _ _ _ kbody)) = all isReturns $ kernelBodyResult kbody
  createsNewArrOp (SizeOp _) = False
  createsNewArrOp _ = undefined

isReturns :: KernelResult -> Bool
isReturns Returns {} = True
isReturns _ = False

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
      let failed_set = oneName $ dstmem coale
          failed_set' = failed_set <> fromMaybe mempty (M.lookup src_mem inhb_tab)
       in ( M.delete src_mem coal_tab,
            M.insert src_mem failed_set' inhb_tab
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
