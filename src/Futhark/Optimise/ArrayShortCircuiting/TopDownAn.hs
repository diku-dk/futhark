{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.ArrayShortCircuiting.TopDownAn
  ( TopDnEnv (..),
    ScopeTab,
    TopDownHelper,
    InhibitTab,
    RangeTab,
    topdwnTravBinding,
    topDownLoop,
    getDirAliasedIxfn,
    addInvAliassesVarTab,
    areAnyAliased,
    isInScope,
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Optimise.ArrayShortCircuiting.DataStructs

type ScopeTab rep = Scope (Aliases rep)
-- ^ maps array-variable names to various info, including
--   types, memory block and index function, etc.

type DirAlias = IxFun -> IxFun
-- ^ A direct aliasing transformation

type InvAlias = Maybe (IxFun -> IxFun)
-- ^ An inverse aliasing transformation

type RangeTab = M.Map VName (PrimExp VName, PrimExp VName)

type VarAliasTab = M.Map VName (VName, DirAlias, InvAlias)

type MemAliasTab = M.Map VName Names

data TopDnEnv rep = TopDnEnv
  { -- | contains the already allocated memory blocks
    alloc :: AllocTab,
    -- | variable info, including var-to-memblock assocs
    scope :: ScopeTab rep,
    -- | the inherited inhibitions from the previous try
    inhibited :: InhibitTab,
    -- | table holding closed ranges for i64-integral scalars
    ranges :: RangeTab,
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
    knownLessThan :: [(VName, SubExp)]
  }

isInScope :: TopDnEnv rep -> VName -> Bool
isInScope td_env m =
  m `M.member` scope td_env

-- | Get alias and (direct) index function mapping from expression
--
-- For instance, if the expression is a 'Rotate', returns the value being
-- rotated as well as a function for rotating an index function the appropriate
-- amount.
getDirAliasFromExp :: Exp (Aliases rep) -> Maybe (VName, DirAlias)
getDirAliasFromExp (BasicOp (SubExp (Var x))) = Just (x, id)
getDirAliasFromExp (BasicOp (Opaque _ (Var x))) = Just (x, id)
getDirAliasFromExp (BasicOp (Reshape shp_chg x)) =
  Just (x, (`IxFun.reshape` map (fmap pe64) shp_chg))
getDirAliasFromExp (BasicOp (Rearrange perm x)) =
  Just (x, (`IxFun.permute` perm))
getDirAliasFromExp (BasicOp (Rotate rs x)) =
  Just (x, (`IxFun.rotate` fmap pe64 rs))
getDirAliasFromExp (BasicOp (Index x slc)) =
  Just (x, (`IxFun.slice` (Slice $ map (fmap pe64) $ unSlice slc)))
getDirAliasFromExp (BasicOp (Update _ x _ _elm)) = Just (x, id)
getDirAliasFromExp (BasicOp (FlatIndex x (FlatSlice offset idxs))) =
  Just
    ( x,
      ( `IxFun.flatSlice`
          ( FlatSlice (pe64 offset) $
              map (fmap pe64) idxs
          )
      )
    )
getDirAliasFromExp (BasicOp (FlatUpdate x _ _)) = Just (x, id)
getDirAliasFromExp _ = Nothing

-- | This was former @createsAliasedArrOK@ from DataStructs
--   While Rearrange and Rotate create aliased arrays, we
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
  let perm' = IxFun.permuteInv perm [0 .. length perm - 1]
   in Just (`IxFun.permute` perm')
getInvAliasFromExp _ = Nothing

class TopDownHelper inner where
  innerNonNegatives :: [VName] -> inner -> Names

  innerKnownLessThan :: inner -> [(VName, SubExp)]

  scopeHelper :: inner -> Scope rep

instance TopDownHelper (HostOp (Aliases GPUMem) ()) where
  innerNonNegatives _ (SegOp seg_op) =
    foldMap (oneName . fst) $ unSegSpace $ segSpace seg_op
  innerNonNegatives [vname] (SizeOp _) = oneName vname
  innerNonNegatives _ _ = mempty

  innerKnownLessThan (SegOp seg_op) =
    unSegSpace $ segSpace seg_op
  innerKnownLessThan _ = mempty

  scopeHelper (SegOp seg_op) = scopeOfSegSpace $ segSpace seg_op
  scopeHelper _ = mempty

instance TopDownHelper () where
  innerNonNegatives _ () = mempty
  innerKnownLessThan () = mempty
  scopeHelper () = mempty

-- | fills in the TopDnEnv table
topdwnTravBinding ::
  (ASTRep rep, Op rep ~ MemOp inner, TopDownHelper (OpWithAliases inner)) =>
  TopDnEnv rep ->
  Stm (Aliases rep) ->
  TopDnEnv rep
topdwnTravBinding env stm@(Let (Pat [pe]) _ (Op (Alloc (Var vname) sp))) =
  env
    { alloc = M.insert (patElemName pe) sp $ alloc env,
      scope = scope env <> scopeOf stm,
      nonNegatives = nonNegatives env <> oneName vname
    }
topdwnTravBinding env stm@(Let pat _ (Op (Inner inner))) =
  env
    { scope = scope env <> scopeOf stm <> scopeHelper inner,
      nonNegatives = nonNegatives env <> innerNonNegatives (patNames pat) inner,
      knownLessThan = knownLessThan env <> innerKnownLessThan inner
    }
-- topdwnTravBinding env stm@(Let pat _ e@(If cond then_body else_body _)) =
--   let res = getDirAliasFromExp e
--       new_aliases =
--         foldMap
--           ( \(pe, then_res, else_res) -> case (getScopeMemInfo (patElemName pe) (scope env <> scopeOf stm), then_res, else_res) of
--               (Just (MemBlock _ _ m_x ixf), Var v1, Var v2) ->
--                 if not $ m_x `elem` patNames pat && v1 == v2
--                   then M.singleton (patElemName pe) (v1, id, Nothing)
--                   else mempty
--               Nothing -> mempty
--           )
--           $ zip3 (patElems pat) (map resSubExp $ bodyResult then_body) (trace ("diralias: " <> res) $ map resSubExp $ bodyResult else_body)
--    in env
--         { v_alias = v_alias env <> new_aliases,
--           scope = scope env <> scopeOf stm,
--           usage_table = usageInStm stm <> usage_table env
--         }
topdwnTravBinding env stm@(Let (Pat [pe]) _ e)
  | Just (x, ixfn) <- getDirAliasFromExp e =
    let ixfn_inv = getInvAliasFromExp e
     in env
          { v_alias = M.insert (patElemName pe) (x, ixfn, ixfn_inv) (v_alias env),
            scope = scope env <> scopeOf stm,
            nonNegatives =
              nonNegatives env
                <> nonNegativesInPat (stmPat stm)
          }
topdwnTravBinding env stm =
  env
    { scope = scope env <> scopeOf stm,
      nonNegatives =
        nonNegatives env
          <> nonNegativesInPat (stmPat stm)
    }

nonNegativesInPat :: Typed rep => PatT rep -> Names
nonNegativesInPat (Pat elems) =
  foldMap (namesFromList . mapMaybe subExpVar . arrayDims . typeOf) elems

topDownLoop :: TopDnEnv rep -> Stm (Aliases rep) -> TopDnEnv rep
topDownLoop td_env (Let _pat _ (DoLoop arginis lform _)) =
  let scopetab =
        scope td_env
          <> scopeOfFParams (map fst arginis)
          <> scopeOf lform
      non_negatives =
        nonNegatives td_env <> case lform of
          ForLoop v _ _ _ -> oneName v
          _ -> mempty
      less_than =
        case lform of
          ForLoop v _ b _ -> [(v, b)]
          _ -> mempty
   in td_env
        { scope = scopetab,
          nonNegatives = non_negatives,
          knownLessThan = less_than <> knownLessThan td_env
        }
topDownLoop _ _ =
  error "ArrayShortCircuiting.TopDownAn: function topDownLoop should only be called on Loops!"

-- | Get direct aliased index function?  Returns a triple of current memory
-- block to be coalesced, the destination memory block and the index function of
-- the access in the space of the destination block.
getDirAliasedIxfn :: HasMemBlock (Aliases rep) => TopDnEnv rep -> CoalsTab -> VName -> Maybe (VName, VName, IxFun)
getDirAliasedIxfn td_env coals_tab x =
  case getScopeMemInfo x (scope td_env) of
    Just (MemBlock _ _ m_x orig_ixfun) ->
      case M.lookup m_x coals_tab of
        Just coal_etry -> do
          (Coalesced _ (MemBlock _ _ m ixf) _) <- walkAliasTab (v_alias td_env) (vartab coal_etry) x
          return (m_x, m, ixf)
        Nothing ->
          -- This value is not subject to coalescing at the moment. Just return the
          -- original index function
          Just (m_x, m_x, orig_ixfun)
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
    return $ Coalesced knd (MemBlock pt shp vname $ alias0 ixf) substs
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
addInvAliassesVarTab ::
  HasMemBlock (Aliases rep) =>
  TopDnEnv rep ->
  M.Map VName Coalesced ->
  VName ->
  Maybe (M.Map VName Coalesced)
addInvAliassesVarTab td_env vtab x
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
                 in addInvAliassesVarTab td_env vartab' x0
addInvAliassesVarTab _ _ _ = error "impossible"

areAliased :: TopDnEnv rep -> VName -> VName -> Bool
areAliased _ m_x m_y =
  -- this is a dummy implementation
  m_x == m_y

areAnyAliased :: TopDnEnv rep -> VName -> [VName] -> Bool
areAnyAliased td_env m_x =
  any (areAliased td_env m_x)
