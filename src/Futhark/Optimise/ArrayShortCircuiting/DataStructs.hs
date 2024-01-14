{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.ArrayShortCircuiting.DataStructs
  ( Coalesced (..),
    CoalescedKind (..),
    ArrayMemBound (..),
    AllocTab,
    HasMemBlock,
    ScalarTab,
    CoalsTab,
    ScopeTab,
    CoalsEntry (..),
    FreeVarSubsts,
    LmadRef,
    MemRefs (..),
    AccessSummary (..),
    BotUpEnv (..),
    InhibitTab,
    unionCoalsEntry,
    vnameToPrimExp,
    getArrMemAssocFParam,
    getScopeMemInfo,
    createsNewArrOK,
    getArrMemAssoc,
    getUniqueMemFParam,
    markFailedCoal,
    accessSubtract,
    markSuccessCoal,
  )
where

import Control.Applicative
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.IR.Aliases
import Futhark.IR.GPUMem as GPU
import Futhark.IR.MCMem as MC
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.SeqMem
import Futhark.Util.Pretty hiding (line, sep, (</>))
import Prelude

-- | maps array-variable names to various info, including
--   types, memory block and index function, etc.
type ScopeTab rep = Scope (Aliases rep)

-- | An LMAD specialized to TPrimExps (a typed primexp)
type LmadRef = LMAD.LMAD (TPrimExp Int64 VName)

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

accessSubtract :: AccessSummary -> AccessSummary -> AccessSummary
accessSubtract Undeterminable _ = Undeterminable
accessSubtract _ Undeterminable = Undeterminable
accessSubtract (Set s1) (Set s2) = Set $ s1 S.\\ s2

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
  | MapCoal

-- | Information about a memory block: type, shape, name and ixfun.
data ArrayMemBound = MemBlock
  { primType :: PrimType,
    shape :: Shape,
    memName :: VName,
    ixfun :: LMAD
  }

-- | Free variable substitutions
type FreeVarSubsts = M.Map VName (TPrimExp Int64 VName)

-- | Coalesced Access Entry
data Coalesced
  = Coalesced
      -- | the kind of coalescing
      CoalescedKind
      -- | destination mem_block info @f_m_x[i]@ (must be ArrayMem)
      -- (Maybe IxFun) -- the inverse ixfun of a coalesced array, such that
      --                     --  ixfuns can be correctly constructed for aliases;
      ArrayMemBound
      -- | substitutions for free vars in index function
      FreeVarSubsts

data CoalsEntry = CoalsEntry
  { -- | destination memory block
    dstmem :: VName,
    -- | index function of the destination (used for rebasing)
    dstind :: LMAD,
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
    memrefs :: MemRefs,
    -- | Certificates of the destination, which must be propagated to
    -- the source. When short-circuiting reaches the array creation
    -- point, we must check whether the certs are in scope for
    -- short-circuiting to succeed.
    certs :: Certs
  }

-- | the allocatted memory blocks
type AllocTab = M.Map VName Space

-- | maps a variable name to its PrimExp scalar expression
type ScalarTab = M.Map VName (PrimExp VName)

-- | maps a memory-block name to a 'CoalsEntry'. Among other things, it contains
--   @vartab@, a map in which each variable associated to that memory block is
--   bound to its 'Coalesced' info.
type CoalsTab = M.Map VName CoalsEntry

-- | inhibited memory-block mergings from the key (memory block)
--   to the value (set of memory blocks).
type InhibitTab = M.Map VName Names

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

instance Pretty CoalsTab where
  pretty = pretty . M.toList

instance Pretty AccessSummary where
  pretty Undeterminable = "Undeterminable"
  pretty (Set a) = "Access-Set:" <+> pretty (S.toList a) <+> " "

instance Pretty MemRefs where
  pretty (MemRefs a b) = "( Use-Sum:" <+> pretty a <+> "Write-Sum:" <+> pretty b <> ")"

instance Pretty CoalescedKind where
  pretty CopyCoal = "Copy"
  pretty InPlaceCoal = "InPlace"
  pretty ConcatCoal = "Concat"
  pretty TransitiveCoal = "Transitive"
  pretty MapCoal = "Map"

instance Pretty ArrayMemBound where
  pretty (MemBlock ptp shp m_nm ixfn) =
    "{" <> pretty ptp <> "," <+> pretty shp <> "," <+> pretty m_nm <> "," <+> pretty ixfn <> "}"

instance Pretty Coalesced where
  pretty (Coalesced knd mbd _) =
    "(Kind:"
      <+> pretty knd
      <> ", membds:"
        <+> pretty mbd -- <> ", subs:" <+> pretty subs
      <> ")"
        <+> "\n"

instance Pretty CoalsEntry where
  pretty etry =
    "{"
      <+> "Dstmem:"
      <+> pretty (dstmem etry)
      <> ", AliasMems:"
        <+> pretty (alsmem etry)
        <+> ", optdeps:"
        <+> pretty (M.toList $ optdeps etry)
        <+> ", memrefs:"
        <+> pretty (memrefs etry)
        <+> ", vartab:"
        <+> pretty (M.toList $ vartab etry)
        <+> "}"
        <+> "\n"

-- | Compute the union of two 'CoalsEntry'. If two 'CoalsEntry' do not refer to
-- the same destination memory and use the same index function, the first
-- 'CoalsEntry' is returned.
unionCoalsEntry :: CoalsEntry -> CoalsEntry -> CoalsEntry
unionCoalsEntry etry1 (CoalsEntry dstmem2 dstind2 alsmem2 vartab2 optdeps2 memrefs2 certs2) =
  if dstmem etry1 /= dstmem2 || dstind etry1 /= dstind2
    then etry1
    else
      etry1
        { alsmem = alsmem etry1 <> alsmem2,
          optdeps = optdeps etry1 <> optdeps2,
          vartab = vartab etry1 <> vartab2,
          memrefs = memrefs etry1 <> memrefs2,
          certs = certs etry1 <> certs2
        }

-- | Get the names of array 'PatElem's in a 'Pat' and the corresponding
-- 'ArrayMemBound' information for each array.
getArrMemAssoc :: Pat (aliases, LetDecMem) -> [(VName, ArrayMemBound)]
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
getArrMemAssocFParam :: [Param FParamMem] -> [(VName, Uniqueness, ArrayMemBound)]
getArrMemAssocFParam =
  mapMaybe
    ( \param -> case paramDec param of
        (MemArray tp shp u (ArrayIn mem_nm indfun)) ->
          Just (paramName param, u, MemBlock tp shp mem_nm indfun)
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

instance HasMemBlock (Aliases MCMem) where
  getScopeMemInfo r scope_env0 =
    case M.lookup r scope_env0 of
      Just (LetName (_, MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      Just (FParamName (MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      Just (LParamName (MemArray tp shp _ (ArrayIn m idx))) -> Just (MemBlock tp shp m idx)
      _ -> Nothing

-- | @True@ if the expression returns a "fresh" array.
createsNewArrOK :: Exp rep -> Bool
createsNewArrOK (BasicOp Replicate {}) = True
createsNewArrOK (BasicOp Iota {}) = True
createsNewArrOK (BasicOp Manifest {}) = True
createsNewArrOK (BasicOp Concat {}) = True
createsNewArrOK (BasicOp ArrayLit {}) = True
createsNewArrOK (BasicOp Scratch {}) = True
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
      let failed_set = oneName $ dstmem coale
          failed_set' = failed_set <> fromMaybe mempty (M.lookup src_mem inhb_tab)
       in ( M.delete src_mem coal_tab,
            M.insert src_mem failed_set' inhb_tab
          )

-- | promotion from active-to-successful coalescing tables
--   should be handled with this function (for clarity).
markSuccessCoal ::
  (CoalsTab, CoalsTab) ->
  VName ->
  CoalsEntry ->
  (CoalsTab, CoalsTab)
markSuccessCoal (actv, succc) m_b info_b =
  ( M.delete m_b actv,
    appendCoalsInfo m_b info_b succc
  )

-- | merges entries in the coalesced table.
appendCoalsInfo :: VName -> CoalsEntry -> CoalsTab -> CoalsTab
appendCoalsInfo mb info_new coalstab =
  case M.lookup mb coalstab of
    Nothing -> M.insert mb info_new coalstab
    Just info_old -> M.insert mb (unionCoalsEntry info_old info_new) coalstab

-- | Attempt to convert a 'VName' to a PrimExp.
--
-- First look in 'ScalarTab' to see if we have recorded the scalar value of the
-- argument. Otherwise look up the type of the argument and return a 'LeafExp'
-- if it is a 'PrimType'.
vnameToPrimExp ::
  (AliasableRep rep) =>
  ScopeTab rep ->
  ScalarTab ->
  VName ->
  Maybe (PrimExp VName)
vnameToPrimExp scopetab scaltab v =
  M.lookup v scaltab
    <|> ( M.lookup v scopetab
            >>= toPrimType . typeOf
            <&> LeafExp v
        )

-- | Attempt to extract the 'PrimType' from a 'TypeBase'.
toPrimType :: TypeBase shp u -> Maybe PrimType
toPrimType (Prim pt) = Just pt
toPrimType _ = Nothing
