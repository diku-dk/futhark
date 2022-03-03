{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Futhark.IR.GPU.Op
  ( -- * Size operations
    SizeOp (..),

    -- * Host operations
    HostOp (..),
    traverseHostOpStms,
    typeCheckHostOp,

    -- * SegOp refinements
    SegLevel (..),

    -- * Reexports
    module Futhark.IR.GPU.Sizes,
    module Futhark.IR.SegOp,
  )
where

import Futhark.Analysis.Metrics
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.IR
import Futhark.IR.Aliases (Aliases)
import Futhark.IR.GPU.Sizes
import Futhark.IR.Prop.Aliases
import Futhark.IR.SegOp
import qualified Futhark.IR.TypeCheck as TC
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.Pretty
  ( commasep,
    parens,
    ppr,
    text,
    (<+>),
  )
import qualified Futhark.Util.Pretty as PP
import Prelude hiding (id, (.))

-- | At which level the *body* of a t'SegOp' executes.
data SegLevel
  = SegThread
      { segNumGroups :: Count NumGroups SubExp,
        segGroupSize :: Count GroupSize SubExp,
        segVirt :: SegVirt
      }
  | SegGroup
      { segNumGroups :: Count NumGroups SubExp,
        segGroupSize :: Count GroupSize SubExp,
        segVirt :: SegVirt
      }
  deriving (Eq, Ord, Show)

instance PP.Pretty SegLevel where
  ppr lvl =
    PP.parens
      ( lvl' <> PP.semi
          <+> text "#groups=" <> ppr (segNumGroups lvl) <> PP.semi
          <+> text "groupsize=" <> ppr (segGroupSize lvl) <> virt
      )
    where
      lvl' = case lvl of
        SegThread {} -> "thread"
        SegGroup {} -> "group"
      virt = case segVirt lvl of
        SegNoVirt -> mempty
        SegNoVirtFull dims -> PP.semi <+> text "full" <+> ppr (segSeqDims dims)
        SegVirt -> PP.semi <+> text "virtualise"

instance Engine.Simplifiable SegLevel where
  simplify (SegThread num_groups group_size virt) =
    SegThread <$> traverse Engine.simplify num_groups
      <*> traverse Engine.simplify group_size
      <*> pure virt
  simplify (SegGroup num_groups group_size virt) =
    SegGroup <$> traverse Engine.simplify num_groups
      <*> traverse Engine.simplify group_size
      <*> pure virt

instance Substitute SegLevel where
  substituteNames substs (SegThread num_groups group_size virt) =
    SegThread
      (substituteNames substs num_groups)
      (substituteNames substs group_size)
      virt
  substituteNames substs (SegGroup num_groups group_size virt) =
    SegGroup
      (substituteNames substs num_groups)
      (substituteNames substs group_size)
      virt

instance Rename SegLevel where
  rename = substituteRename

instance FreeIn SegLevel where
  freeIn' (SegThread num_groups group_size _) =
    freeIn' num_groups <> freeIn' group_size
  freeIn' (SegGroup num_groups group_size _) =
    freeIn' num_groups <> freeIn' group_size

-- | A simple size-level query or computation.
data SizeOp
  = -- | @SplitSpace o w i elems_per_thread@.
    --
    -- Computes how to divide array elements to
    -- threads in a kernel.  Returns the number of
    -- elements in the chunk that the current thread
    -- should take.
    --
    -- @w@ is the length of the outer dimension in
    -- the array. @i@ is the current thread
    -- index. Each thread takes at most
    -- @elems_per_thread@ elements.
    --
    -- If the order @o@ is 'SplitContiguous', thread with index @i@
    -- should receive elements
    -- @i*elems_per_tread, i*elems_per_thread + 1,
    -- ..., i*elems_per_thread + (elems_per_thread-1)@.
    --
    -- If the order @o@ is @'SplitStrided' stride@,
    -- the thread will receive elements @i,
    -- i+stride, i+2*stride, ...,
    -- i+(elems_per_thread-1)*stride@.
    SplitSpace SplitOrdering SubExp SubExp SubExp
  | -- | Produce some runtime-configurable size.
    GetSize Name SizeClass
  | -- | The maximum size of some class.
    GetSizeMax SizeClass
  | -- | Compare size (likely a threshold) with some integer value.
    CmpSizeLe Name SizeClass SubExp
  | -- | @CalcNumGroups w max_num_groups group_size@ calculates the
    -- number of GPU workgroups to use for an input of the given size.
    -- The @Name@ is a size name.  Note that @w@ is an i64 to avoid
    -- overflow issues.
    CalcNumGroups SubExp Name SubExp
  deriving (Eq, Ord, Show)

instance Substitute SizeOp where
  substituteNames subst (SplitSpace o w i elems_per_thread) =
    SplitSpace
      (substituteNames subst o)
      (substituteNames subst w)
      (substituteNames subst i)
      (substituteNames subst elems_per_thread)
  substituteNames substs (CmpSizeLe name sclass x) =
    CmpSizeLe name sclass (substituteNames substs x)
  substituteNames substs (CalcNumGroups w max_num_groups group_size) =
    CalcNumGroups
      (substituteNames substs w)
      max_num_groups
      (substituteNames substs group_size)
  substituteNames _ op = op

instance Rename SizeOp where
  rename (SplitSpace o w i elems_per_thread) =
    SplitSpace
      <$> rename o
      <*> rename w
      <*> rename i
      <*> rename elems_per_thread
  rename (CmpSizeLe name sclass x) =
    CmpSizeLe name sclass <$> rename x
  rename (CalcNumGroups w max_num_groups group_size) =
    CalcNumGroups <$> rename w <*> pure max_num_groups <*> rename group_size
  rename x = pure x

instance IsOp SizeOp where
  safeOp _ = True
  cheapOp _ = True

instance TypedOp SizeOp where
  opType SplitSpace {} = pure [Prim int64]
  opType (GetSize _ _) = pure [Prim int64]
  opType (GetSizeMax _) = pure [Prim int64]
  opType CmpSizeLe {} = pure [Prim Bool]
  opType CalcNumGroups {} = pure [Prim int64]

instance AliasedOp SizeOp where
  opAliases _ = [mempty]
  consumedInOp _ = mempty

instance FreeIn SizeOp where
  freeIn' (SplitSpace o w i elems_per_thread) =
    freeIn' o <> freeIn' [w, i, elems_per_thread]
  freeIn' (CmpSizeLe _ _ x) = freeIn' x
  freeIn' (CalcNumGroups w _ group_size) = freeIn' w <> freeIn' group_size
  freeIn' _ = mempty

instance PP.Pretty SizeOp where
  ppr (SplitSpace SplitContiguous w i elems_per_thread) =
    text "split_space"
      <> parens (commasep [ppr w, ppr i, ppr elems_per_thread])
  ppr (SplitSpace (SplitStrided stride) w i elems_per_thread) =
    text "split_space_strided"
      <> parens (commasep [ppr stride, ppr w, ppr i, ppr elems_per_thread])
  ppr (GetSize name size_class) =
    text "get_size" <> parens (commasep [ppr name, ppr size_class])
  ppr (GetSizeMax size_class) =
    text "get_size_max" <> parens (commasep [ppr size_class])
  ppr (CmpSizeLe name size_class x) =
    text "cmp_size" <> parens (commasep [ppr name, ppr size_class])
      <+> text "<="
      <+> ppr x
  ppr (CalcNumGroups w max_num_groups group_size) =
    text "calc_num_groups" <> parens (commasep [ppr w, ppr max_num_groups, ppr group_size])

instance OpMetrics SizeOp where
  opMetrics SplitSpace {} = seen "SplitSpace"
  opMetrics GetSize {} = seen "GetSize"
  opMetrics GetSizeMax {} = seen "GetSizeMax"
  opMetrics CmpSizeLe {} = seen "CmpSizeLe"
  opMetrics CalcNumGroups {} = seen "CalcNumGroups"

typeCheckSizeOp :: TC.Checkable rep => SizeOp -> TC.TypeM rep ()
typeCheckSizeOp (SplitSpace o w i elems_per_thread) = do
  case o of
    SplitContiguous -> return ()
    SplitStrided stride -> TC.require [Prim int64] stride
  mapM_ (TC.require [Prim int64]) [w, i, elems_per_thread]
typeCheckSizeOp GetSize {} = return ()
typeCheckSizeOp GetSizeMax {} = return ()
typeCheckSizeOp (CmpSizeLe _ _ x) = TC.require [Prim int64] x
typeCheckSizeOp (CalcNumGroups w _ group_size) = do
  TC.require [Prim int64] w
  TC.require [Prim int64] group_size

-- | A host-level operation; parameterised by what else it can do.
data HostOp rep op
  = -- | A segmented operation.
    SegOp (SegOp SegLevel rep)
  | SizeOp SizeOp
  | OtherOp op
  deriving (Eq, Ord, Show)

-- | A helper for defining 'TraverseOpStms'.
traverseHostOpStms ::
  Monad m =>
  OpStmsTraverser m op rep ->
  OpStmsTraverser m (HostOp rep op) rep
traverseHostOpStms _ f (SegOp segop) = SegOp <$> traverseSegOpStms f segop
traverseHostOpStms _ _ (SizeOp sizeop) = pure $ SizeOp sizeop
traverseHostOpStms onOtherOp f (OtherOp other) = OtherOp <$> onOtherOp f other

instance (ASTRep rep, Substitute op) => Substitute (HostOp rep op) where
  substituteNames substs (SegOp op) =
    SegOp $ substituteNames substs op
  substituteNames substs (OtherOp op) =
    OtherOp $ substituteNames substs op
  substituteNames substs (SizeOp op) =
    SizeOp $ substituteNames substs op

instance (ASTRep rep, Rename op) => Rename (HostOp rep op) where
  rename (SegOp op) = SegOp <$> rename op
  rename (OtherOp op) = OtherOp <$> rename op
  rename (SizeOp op) = SizeOp <$> rename op

instance (ASTRep rep, IsOp op) => IsOp (HostOp rep op) where
  safeOp (SegOp op) = safeOp op
  safeOp (OtherOp op) = safeOp op
  safeOp (SizeOp op) = safeOp op

  cheapOp (SegOp op) = cheapOp op
  cheapOp (OtherOp op) = cheapOp op
  cheapOp (SizeOp op) = cheapOp op

instance TypedOp op => TypedOp (HostOp rep op) where
  opType (SegOp op) = opType op
  opType (OtherOp op) = opType op
  opType (SizeOp op) = opType op

instance (Aliased rep, AliasedOp op, ASTRep rep) => AliasedOp (HostOp rep op) where
  opAliases (SegOp op) = opAliases op
  opAliases (OtherOp op) = opAliases op
  opAliases (SizeOp op) = opAliases op

  consumedInOp (SegOp op) = consumedInOp op
  consumedInOp (OtherOp op) = consumedInOp op
  consumedInOp (SizeOp op) = consumedInOp op

instance (ASTRep rep, FreeIn op) => FreeIn (HostOp rep op) where
  freeIn' (SegOp op) = freeIn' op
  freeIn' (OtherOp op) = freeIn' op
  freeIn' (SizeOp op) = freeIn' op

instance (CanBeAliased (Op rep), CanBeAliased op, ASTRep rep) => CanBeAliased (HostOp rep op) where
  type OpWithAliases (HostOp rep op) = HostOp (Aliases rep) (OpWithAliases op)

  addOpAliases aliases (SegOp op) = SegOp $ addOpAliases aliases op
  addOpAliases aliases (OtherOp op) = OtherOp $ addOpAliases aliases op
  addOpAliases _ (SizeOp op) = SizeOp op

  removeOpAliases (SegOp op) = SegOp $ removeOpAliases op
  removeOpAliases (OtherOp op) = OtherOp $ removeOpAliases op
  removeOpAliases (SizeOp op) = SizeOp op

instance (CanBeWise (Op rep), CanBeWise op, ASTRep rep) => CanBeWise (HostOp rep op) where
  type OpWithWisdom (HostOp rep op) = HostOp (Wise rep) (OpWithWisdom op)

  removeOpWisdom (SegOp op) = SegOp $ removeOpWisdom op
  removeOpWisdom (OtherOp op) = OtherOp $ removeOpWisdom op
  removeOpWisdom (SizeOp op) = SizeOp op

  addOpWisdom (SegOp op) = SegOp $ addOpWisdom op
  addOpWisdom (OtherOp op) = OtherOp $ addOpWisdom op
  addOpWisdom (SizeOp op) = SizeOp op

instance (ASTRep rep, ST.IndexOp op) => ST.IndexOp (HostOp rep op) where
  indexOp vtable k (SegOp op) is = ST.indexOp vtable k op is
  indexOp vtable k (OtherOp op) is = ST.indexOp vtable k op is
  indexOp _ _ _ _ = Nothing

instance (PrettyRep rep, PP.Pretty op) => PP.Pretty (HostOp rep op) where
  ppr (SegOp op) = ppr op
  ppr (OtherOp op) = ppr op
  ppr (SizeOp op) = ppr op

instance (OpMetrics (Op rep), OpMetrics op) => OpMetrics (HostOp rep op) where
  opMetrics (SegOp op) = opMetrics op
  opMetrics (OtherOp op) = opMetrics op
  opMetrics (SizeOp op) = opMetrics op

checkSegLevel ::
  TC.Checkable rep =>
  Maybe SegLevel ->
  SegLevel ->
  TC.TypeM rep ()
checkSegLevel Nothing lvl = do
  TC.require [Prim int64] $ unCount $ segNumGroups lvl
  TC.require [Prim int64] $ unCount $ segGroupSize lvl
checkSegLevel (Just SegThread {}) _ =
  TC.bad $ TC.TypeError "SegOps cannot occur when already at thread level."
checkSegLevel (Just x) y
  | x == y = TC.bad $ TC.TypeError $ "Already at at level " ++ pretty x
  | segNumGroups x /= segNumGroups y || segGroupSize x /= segGroupSize y =
    TC.bad $ TC.TypeError "Physical layout for SegLevel does not match parent SegLevel."
  | otherwise =
    return ()

typeCheckHostOp ::
  TC.Checkable rep =>
  (SegLevel -> OpWithAliases (Op rep) -> TC.TypeM rep ()) ->
  Maybe SegLevel ->
  (op -> TC.TypeM rep ()) ->
  HostOp (Aliases rep) op ->
  TC.TypeM rep ()
typeCheckHostOp checker lvl _ (SegOp op) =
  TC.checkOpWith (checker $ segLevel op) $
    typeCheckSegOp (checkSegLevel lvl) op
typeCheckHostOp _ _ f (OtherOp op) = f op
typeCheckHostOp _ _ _ (SizeOp op) = typeCheckSizeOp op
