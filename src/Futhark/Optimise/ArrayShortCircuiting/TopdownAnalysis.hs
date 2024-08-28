{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.ArrayShortCircuiting.TopdownAnalysis
  ( TopdownEnv (..),
    ScopeTab,
    TopDownHelper,
    InhibitTab,
    updateTopdownEnv,
    updateTopdownEnvLoop,
    getDirAliasedIxfn,
    getDirAliasedIxfn',
    addInvAliasesVarTab,
    areAnyAliased,
    isInScope,
    nonNegativesInPat,
  )
where

import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.GPUMem as GPU
import Futhark.IR.MCMem as MC
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Optimise.ArrayShortCircuiting.DataStructs

type DirAlias = LMAD -> Maybe LMAD
-- ^ A direct aliasing transformation

type InvAlias = Maybe (LMAD -> LMAD)
-- ^ An inverse aliasing transformation

type VarAliasTab = M.Map VName (VName, DirAlias, InvAlias)

type MemAliasTab = M.Map VName Names

data TopdownEnv rep = TopdownEnv
  { -- | contains the already allocated memory blocks
    alloc :: AllocTab,
    -- | variable info, including var-to-memblock assocs
    scope :: ScopeTab rep,
    -- | the inherited inhibitions from the previous try
    inhibited :: InhibitTab,
    -- | for statements such as transpose, reshape, index, etc., that alias
    --   an array variable: maps var-names to pair of aliased var name
    --   and index function transformation. For example, for
    --   @let b = a[slc]@ it should add the binding
    --   @ b |-> (a, `slice` slc )@
    v_alias :: VarAliasTab,
    -- | keeps track of memory block aliasing.
    --   this needs to be implemented
    m_alias :: MemAliasTab,
    -- | Contains symbol information about the variables in the program. Used to
    -- determine if a variable is non-negative.
    nonNegatives :: Names,
    scalarTable :: M.Map VName (PrimExp VName),
    -- | A list of known relations of the form 'VName' @<@ 'SubExp', typically
    -- gotten from 'LoopForm' and 'SegSpace'.
    knownLessThan :: [(VName, PrimExp VName)],
    -- | A list of the asserts encountered so far
    td_asserts :: [SubExp]
  }

isInScope :: TopdownEnv rep -> VName -> Bool
isInScope td_env m = m `M.member` scope td_env

-- | Get alias and (direct) index function mapping from expression
getDirAliasFromExp :: Exp (Aliases rep) -> Maybe (VName, DirAlias)
getDirAliasFromExp (BasicOp (SubExp (Var x))) = Just (x, Just)
getDirAliasFromExp (BasicOp (Opaque _ (Var x))) = Just (x, Just)
getDirAliasFromExp (BasicOp (Reshape ReshapeCoerce shp x)) =
  Just (x, Just . (`LMAD.coerce` shapeDims (fmap pe64 shp)))
getDirAliasFromExp (BasicOp (Reshape ReshapeArbitrary shp x)) =
  Just (x, (`LMAD.reshape` shapeDims (fmap pe64 shp)))
getDirAliasFromExp (BasicOp (Rearrange _ _)) =
  Nothing
getDirAliasFromExp (BasicOp (Index x slc)) =
  Just (x, Just . (`LMAD.slice` (Slice $ map (fmap pe64) $ unSlice slc)))
getDirAliasFromExp (BasicOp (Update _ x _ _elm)) = Just (x, Just)
getDirAliasFromExp (BasicOp (FlatIndex x (FlatSlice offset idxs))) =
  Just
    ( x,
      Just . (`LMAD.flatSlice` FlatSlice (pe64 offset) (map (fmap pe64) idxs))
    )
getDirAliasFromExp (BasicOp (FlatUpdate x _ _)) = Just (x, Just)
getDirAliasFromExp _ = Nothing

-- | This was former @createsAliasedArrOK@ from DataStructs
--   While Rearrange creates aliased arrays, we
--   do not yet support them because it would mean we have
--   to "reverse" the index function, for example to support
--   coalescing in the case below,
--       @let a = map f a0   @
--       @let b = transpose a@
--       @let y[4] = copy(b) @
--   we would need to assign to @a@ as index function, the
--   inverse of the transpose, such that, when creating @b@
--   by transposition we get a directly-mapped array, which
--   is expected by the copying in y[4].
--   For the moment we support only transposition and VName-expressions,
--     but rotations and full slices could also be supported.
--
-- This function complements 'getDirAliasFromExp' by returning a function that
-- applies the inverse index function transformation.
getInvAliasFromExp :: Exp (Aliases rep) -> InvAlias
getInvAliasFromExp (BasicOp (SubExp (Var _))) = Just id
getInvAliasFromExp (BasicOp (Opaque _ (Var _))) = Just id
getInvAliasFromExp (BasicOp Update {}) = Just id
getInvAliasFromExp (BasicOp (Rearrange perm _)) =
  Just (`LMAD.permute` rearrangeInverse perm)
getInvAliasFromExp _ = Nothing

class TopDownHelper inner where
  innerNonNegatives :: [VName] -> inner -> Names

  innerKnownLessThan :: inner -> [(VName, PrimExp VName)]

  scopeHelper :: inner -> Scope rep

instance TopDownHelper (SegOp lvl rep) where
  innerNonNegatives _ op =
    foldMap (oneName . fst) $ unSegSpace $ segSpace op

  innerKnownLessThan op =
    map (fmap $ primExpFromSubExp $ IntType Int64) $ unSegSpace $ segSpace op

  scopeHelper op = scopeOfSegSpace $ segSpace op

instance TopDownHelper (HostOp NoOp (Aliases GPUMem)) where
  innerNonNegatives vs (SegOp op) = innerNonNegatives vs op
  innerNonNegatives [vname] (SizeOp (GetSize _ _)) = oneName vname
  innerNonNegatives [vname] (SizeOp (GetSizeMax _)) = oneName vname
  innerNonNegatives _ _ = mempty

  innerKnownLessThan (SegOp op) = innerKnownLessThan op
  innerKnownLessThan _ = mempty

  scopeHelper (SegOp op) = scopeHelper op
  scopeHelper _ = mempty

instance (TopDownHelper (inner (Aliases MCMem))) => TopDownHelper (MC.MCOp inner (Aliases MCMem)) where
  innerNonNegatives vs (ParOp par_op op) =
    maybe mempty (innerNonNegatives vs) par_op
      <> innerNonNegatives vs op
  innerNonNegatives vs (MC.OtherOp op) =
    innerNonNegatives vs op
  innerKnownLessThan (ParOp par_op op) =
    maybe mempty innerKnownLessThan par_op <> innerKnownLessThan op
  innerKnownLessThan (MC.OtherOp op) =
    innerKnownLessThan op
  scopeHelper (ParOp par_op op) =
    maybe mempty scopeHelper par_op <> scopeHelper op
  scopeHelper MC.OtherOp {} = mempty

instance TopDownHelper (NoOp rep) where
  innerNonNegatives _ NoOp = mempty
  innerKnownLessThan NoOp = mempty
  scopeHelper NoOp = mempty

-- | fills in the TopdownEnv table
updateTopdownEnv ::
  (ASTRep rep, Op rep ~ MemOp inner rep, TopDownHelper (inner (Aliases rep))) =>
  TopdownEnv rep ->
  Stm (Aliases rep) ->
  TopdownEnv rep
updateTopdownEnv env stm@(Let (Pat [pe]) _ (Op (Alloc (Var vname) sp))) =
  env
    { alloc = M.insert (patElemName pe) sp $ alloc env,
      scope = scope env <> scopeOf stm,
      nonNegatives = nonNegatives env <> oneName vname
    }
updateTopdownEnv env stm@(Let pat _ (Op (Inner inner))) =
  env
    { scope = scope env <> scopeOf stm <> scopeHelper inner,
      nonNegatives = nonNegatives env <> innerNonNegatives (patNames pat) inner,
      knownLessThan = knownLessThan env <> innerKnownLessThan inner
    }
updateTopdownEnv env stm@(Let (Pat _) _ (BasicOp (Assert se _ _))) =
  env
    { scope = scope env <> scopeOf stm,
      td_asserts = se : td_asserts env
    }
updateTopdownEnv env stm@(Let (Pat [pe]) _ e)
  | Just (x, ixfn) <- getDirAliasFromExp e =
      let ixfn_inv = getInvAliasFromExp e
       in env
            { v_alias = M.insert (patElemName pe) (x, ixfn, ixfn_inv) (v_alias env),
              scope = scope env <> scopeOf stm,
              nonNegatives = nonNegatives env <> nonNegativesInPat (stmPat stm)
            }
updateTopdownEnv env stm =
  env
    { scope = scope env <> scopeOf stm,
      nonNegatives = nonNegatives env <> nonNegativesInPat (stmPat stm)
    }

nonNegativesInPat :: (Typed rep) => Pat rep -> Names
nonNegativesInPat (Pat elems) =
  foldMap (namesFromList . mapMaybe subExpVar . arrayDims . typeOf) elems

-- | The topdown handler for loops.
updateTopdownEnvLoop :: TopdownEnv rep -> [(FParam rep, SubExp)] -> LoopForm -> TopdownEnv rep
updateTopdownEnvLoop td_env arginis lform =
  let scopetab =
        scope td_env
          <> scopeOfFParams (map fst arginis)
          <> scopeOfLoopForm lform
      non_negatives =
        nonNegatives td_env <> case lform of
          ForLoop v _ _ -> oneName v
          _ -> mempty
      less_than =
        case lform of
          ForLoop v _ b -> [(v, primExpFromSubExp (IntType Int64) b)]
          _ -> mempty
   in td_env
        { scope = scopetab,
          nonNegatives = non_negatives,
          knownLessThan = less_than <> knownLessThan td_env
        }

-- | Get direct aliased index function.  Returns a triple of current memory
-- block to be coalesced, the destination memory block and the index function of
-- the access in the space of the destination block.
getDirAliasedIxfn :: (HasMemBlock (Aliases rep)) => TopdownEnv rep -> CoalsTab -> VName -> Maybe (VName, VName, LMAD)
getDirAliasedIxfn td_env coals_tab x =
  case getScopeMemInfo x (scope td_env) of
    Just (MemBlock _ _ m_x orig_ixfun) ->
      case M.lookup m_x coals_tab of
        Just coal_etry -> do
          (Coalesced _ (MemBlock _ _ m ixf) _) <- walkAliasTab (v_alias td_env) (vartab coal_etry) x
          pure (m_x, m, ixf)
        Nothing ->
          -- This value is not subject to coalescing at the moment. Just return the
          -- original index function
          Just (m_x, m_x, orig_ixfun)
    Nothing -> Nothing

-- | Like 'getDirAliasedIxfn', but this version returns 'Nothing' if the value
-- is not currently subject to coalescing.
getDirAliasedIxfn' :: (HasMemBlock (Aliases rep)) => TopdownEnv rep -> CoalsTab -> VName -> Maybe (VName, VName, LMAD)
getDirAliasedIxfn' td_env coals_tab x =
  case getScopeMemInfo x (scope td_env) of
    Just (MemBlock _ _ m_x _) ->
      case M.lookup m_x coals_tab of
        Just coal_etry -> do
          (Coalesced _ (MemBlock _ _ m ixf) _) <- walkAliasTab (v_alias td_env) (vartab coal_etry) x
          pure (m_x, m, ixf)
        Nothing ->
          -- This value is not subject to coalescing at the moment. Just return the
          -- original index function
          Nothing
    Nothing -> Nothing

-- | Given a 'VName', walk the 'VarAliasTab' until found in the 'Map'.
walkAliasTab ::
  VarAliasTab ->
  M.Map VName Coalesced ->
  VName ->
  Maybe Coalesced
walkAliasTab _ vtab x
  | Just c <- M.lookup x vtab =
      Just c -- @x@ is in @vartab@ together with its new ixfun
walkAliasTab alias_tab vtab x
  | Just (x0, alias0, _) <- M.lookup x alias_tab = do
      Coalesced knd (MemBlock pt shp vname ixf) substs <- walkAliasTab alias_tab vtab x0
      ixf' <- alias0 ixf
      pure $ Coalesced knd (MemBlock pt shp vname ixf') substs
walkAliasTab _ _ _ = Nothing

-- | We assume @x@ is in @vartab@ and we add the variables that @x@ aliases
--   for as long as possible following a chain of direct-aliasing operators,
--   i.e., without considering aliasing of if-then-else, loops, etc. For example:
--     @ x0 = if c then ... else ...@
--     @ x1 = rearrange r1 x0 @
--     @ x2 = reverse x1@
--     @ y[slc] = x2 @
--   We assume @vartab@ constains a binding for @x2@, and calling this function
--     with @x2@ as argument should also insert entries for @x1@ and @x0@ to
--     @vartab@, of course if their aliasing operations are invertible.
--   We assume inverting aliases has been performed by the top-down pass.
addInvAliasesVarTab ::
  (HasMemBlock (Aliases rep)) =>
  TopdownEnv rep ->
  M.Map VName Coalesced ->
  VName ->
  Maybe (M.Map VName Coalesced)
addInvAliasesVarTab td_env vtab x
  | Just (Coalesced _ (MemBlock _ _ m_y x_ixfun) fv_subs) <- M.lookup x vtab =
      case M.lookup x (v_alias td_env) of
        Nothing -> Just vtab
        Just (_, _, Nothing) -> Nothing -- can't invert ixfun, conservatively fail!
        Just (x0, _, Just inv_alias0) ->
          let x_ixfn0 = inv_alias0 x_ixfun
           in case getScopeMemInfo x0 (scope td_env) of
                Nothing -> error "impossible"
                Just (MemBlock ptp shp _ _) ->
                  let coal = Coalesced TransitiveCoal (MemBlock ptp shp m_y x_ixfn0) fv_subs
                      vartab' = M.insert x0 coal vtab
                   in addInvAliasesVarTab td_env vartab' x0
addInvAliasesVarTab _ _ _ = Nothing

areAliased :: TopdownEnv rep -> VName -> VName -> Bool
areAliased _ m_x m_y =
  -- this is a dummy implementation
  m_x == m_y

areAnyAliased :: TopdownEnv rep -> VName -> [VName] -> Bool
areAnyAliased td_env m_x =
  any (areAliased td_env m_x)
