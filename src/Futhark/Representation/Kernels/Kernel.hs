{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.Representation.Kernels.Kernel
  ( -- * Size operations
    SizeOp(..)

    -- * Host operations
  , HostOp(..)
  , typeCheckHostOp

    -- * SegOp refinements
  , SegLevel(..)

    -- * Reexports
  , module Futhark.Representation.Kernels.Sizes
  , module Futhark.Representation.SegOp
  )
where

import Futhark.Representation.AST
import qualified Futhark.Analysis.ScalExp as SE
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Util.Pretty as PP
import Futhark.Util.Pretty
  ((</>), (<+>), ppr, commasep, parens, text)
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplify.Lore
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Representation.Ranges
  (Ranges)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases
  (Aliases)
import Futhark.Representation.SegOp
import Futhark.Representation.Kernels.Sizes
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics

-- | At which level the *body* of a 'SegOp' executes.
data SegLevel = SegThread { segNumGroups :: Count NumGroups SubExp
                          , segGroupSize :: Count GroupSize SubExp
                          , segVirt :: SegVirt }
              | SegGroup { segNumGroups :: Count NumGroups SubExp
                         , segGroupSize :: Count GroupSize SubExp
                         , segVirt :: SegVirt }
              deriving (Eq, Ord, Show)


instance PP.Pretty SegLevel where
  ppr lvl =
    lvl' </>
    PP.parens (text "#groups=" <> ppr (segNumGroups lvl) <> PP.semi <+>
               text "groupsize=" <> ppr (segGroupSize lvl) <>
               case segVirt lvl of
                 SegNoVirt -> mempty
                 SegVirt -> PP.semi <+> text "virtualise")

    where lvl' = case lvl of SegThread{} -> "_thread"
                             SegGroup{} -> "_group"

instance Engine.Simplifiable SegLevel where
  simplify (SegThread num_groups group_size virt) =
    SegThread <$> traverse Engine.simplify num_groups <*>
    traverse Engine.simplify group_size <*> pure virt
  simplify (SegGroup num_groups group_size virt) =
    SegGroup <$> traverse Engine.simplify num_groups <*>
    traverse Engine.simplify group_size <*> pure virt

instance Substitute SegLevel where
  substituteNames substs (SegThread num_groups group_size virt) =
    SegThread
    (substituteNames substs num_groups) (substituteNames substs group_size) virt
  substituteNames substs (SegGroup num_groups group_size virt) =
    SegGroup
    (substituteNames substs num_groups) (substituteNames substs group_size) virt

instance Rename SegLevel where
  rename = substituteRename

instance FreeIn SegLevel where
  freeIn' (SegThread num_groups group_size _) =
    freeIn' num_groups <> freeIn' group_size
  freeIn' (SegGroup num_groups group_size _) =
    freeIn' num_groups <> freeIn' group_size

-- | A simple size-level query or computation.
data SizeOp
  = SplitSpace SplitOrdering SubExp SubExp SubExp
    -- ^ @SplitSpace o w i elems_per_thread@.
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
  | GetSize Name SizeClass
    -- ^ Produce some runtime-configurable size.
  | GetSizeMax SizeClass
    -- ^ The maximum size of some class.
  | CmpSizeLe Name SizeClass SubExp
    -- ^ Compare size (likely a threshold) with some integer value.
  | CalcNumGroups SubExp Name SubExp
    -- ^ @CalcNumGroups w max_num_groups group_size@ calculates the
    -- number of GPU workgroups to use for an input of the given size.
    -- The @Name@ is a size name.  Note that @w@ is an i64 to avoid
    -- overflow issues.
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
  opType SplitSpace{} = pure [Prim int32]
  opType (GetSize _ _) = pure [Prim int32]
  opType (GetSizeMax _) = pure [Prim int32]
  opType CmpSizeLe{} = pure [Prim Bool]
  opType CalcNumGroups{} = pure [Prim int32]

instance AliasedOp SizeOp where
  opAliases _ = [mempty]
  consumedInOp _ = mempty

instance RangedOp SizeOp where
  opRanges (SplitSpace _ _ _ elems_per_thread) =
    [(Just (ScalarBound 0),
      Just (ScalarBound (SE.subExpToScalExp elems_per_thread int32)))]
  opRanges _ = [unknownRange]

instance FreeIn SizeOp where
  freeIn' (SplitSpace o w i elems_per_thread) =
    freeIn' o <> freeIn' [w, i, elems_per_thread]
  freeIn' (CmpSizeLe _ _ x) = freeIn' x
  freeIn' (CalcNumGroups w _ group_size) = freeIn' w <> freeIn' group_size
  freeIn' _ = mempty

instance PP.Pretty SizeOp where
  ppr (SplitSpace o w i elems_per_thread) =
    text "splitSpace" <> suff <>
    parens (commasep [ppr w, ppr i, ppr elems_per_thread])
    where suff = case o of SplitContiguous     -> mempty
                           SplitStrided stride -> text "Strided" <> parens (ppr stride)

  ppr (GetSize name size_class) =
    text "get_size" <> parens (commasep [ppr name, ppr size_class])

  ppr (GetSizeMax size_class) =
    text "get_size_max" <> parens (commasep [ppr size_class])

  ppr (CmpSizeLe name size_class x) =
    text "get_size" <> parens (commasep [ppr name, ppr size_class]) <+>
    text "<=" <+> ppr x

  ppr (CalcNumGroups w max_num_groups group_size) =
    text "calc_num_groups" <> parens (commasep [ppr w, ppr max_num_groups, ppr group_size])

instance OpMetrics SizeOp where
  opMetrics SplitSpace{} = seen "SplitSpace"
  opMetrics GetSize{} = seen "GetSize"
  opMetrics GetSizeMax{} = seen "GetSizeMax"
  opMetrics CmpSizeLe{} = seen "CmpSizeLe"
  opMetrics CalcNumGroups{} = seen "CalcNumGroups"

typeCheckSizeOp :: TC.Checkable lore => SizeOp -> TC.TypeM lore ()
typeCheckSizeOp (SplitSpace o w i elems_per_thread) = do
  case o of
    SplitContiguous     -> return ()
    SplitStrided stride -> TC.require [Prim int32] stride
  mapM_ (TC.require [Prim int32]) [w, i, elems_per_thread]
typeCheckSizeOp GetSize{} = return ()
typeCheckSizeOp GetSizeMax{} = return ()
typeCheckSizeOp (CmpSizeLe _ _ x) = TC.require [Prim int32] x
typeCheckSizeOp (CalcNumGroups w _ group_size) = do TC.require [Prim int64] w
                                                    TC.require [Prim int32] group_size

-- | A host-level operation; parameterised by what else it can do.
data HostOp lore op
  = SegOp (SegOp SegLevel lore)
    -- ^ A segmented operation.
  | SizeOp SizeOp
  | OtherOp op
  deriving (Eq, Ord, Show)

instance (Attributes lore, Substitute op) => Substitute (HostOp lore op) where
  substituteNames substs (SegOp op) =
    SegOp $ substituteNames substs op
  substituteNames substs (OtherOp op) =
    OtherOp $ substituteNames substs op
  substituteNames substs (SizeOp op) =
    SizeOp $ substituteNames substs op

instance (Attributes lore, Rename op) => Rename (HostOp lore op) where
  rename (SegOp op) = SegOp <$> rename op
  rename (OtherOp op) = OtherOp <$> rename op
  rename (SizeOp op) = SizeOp <$> rename op

instance (Attributes lore, IsOp op) => IsOp (HostOp lore op) where
  safeOp (SegOp op) = safeOp op
  safeOp (OtherOp op) = safeOp op
  safeOp (SizeOp op) = safeOp op

  cheapOp (SegOp op) = cheapOp op
  cheapOp (OtherOp op) = cheapOp op
  cheapOp (SizeOp op) = cheapOp op

instance TypedOp op => TypedOp (HostOp lore op) where
  opType (SegOp op) = opType op
  opType (OtherOp op) = opType op
  opType (SizeOp op) = opType op

instance (Aliased lore, AliasedOp op, Attributes lore) => AliasedOp (HostOp lore op) where
  opAliases (SegOp op) = opAliases op
  opAliases (OtherOp op) = opAliases op
  opAliases (SizeOp op) = opAliases op

  consumedInOp (SegOp op) = consumedInOp op
  consumedInOp (OtherOp op) = consumedInOp op
  consumedInOp (SizeOp op) = consumedInOp op

instance (Attributes lore, RangedOp op) => RangedOp (HostOp lore op) where
  opRanges (SegOp op) = opRanges op
  opRanges (OtherOp op) = opRanges op
  opRanges (SizeOp op) = opRanges op

instance (Attributes lore, FreeIn op) => FreeIn (HostOp lore op) where
  freeIn' (SegOp op) = freeIn' op
  freeIn' (OtherOp op) = freeIn' op
  freeIn' (SizeOp op) = freeIn' op

instance (CanBeAliased (Op lore), CanBeAliased op, Attributes lore) => CanBeAliased (HostOp lore op) where
  type OpWithAliases (HostOp lore op) = HostOp (Aliases lore) (OpWithAliases op)

  addOpAliases (SegOp op) = SegOp $ addOpAliases op
  addOpAliases (OtherOp op) = OtherOp $ addOpAliases op
  addOpAliases (SizeOp op) = SizeOp op

  removeOpAliases (SegOp op) = SegOp $ removeOpAliases op
  removeOpAliases (OtherOp op) = OtherOp $ removeOpAliases op
  removeOpAliases (SizeOp op) = SizeOp op

instance (CanBeRanged (Op lore), CanBeRanged op, Attributes lore) => CanBeRanged (HostOp lore op) where
  type OpWithRanges (HostOp lore op) = HostOp (Ranges lore) (OpWithRanges op)

  addOpRanges (SegOp op) = SegOp $ addOpRanges op
  addOpRanges (OtherOp op) = OtherOp $ addOpRanges op
  addOpRanges (SizeOp op) = SizeOp op

  removeOpRanges (SegOp op) = SegOp $ removeOpRanges op
  removeOpRanges (OtherOp op) = OtherOp $ removeOpRanges op
  removeOpRanges (SizeOp op) = SizeOp op

instance (CanBeWise (Op lore), CanBeWise op, Attributes lore) => CanBeWise (HostOp lore op) where
  type OpWithWisdom (HostOp lore op) = HostOp (Wise lore) (OpWithWisdom op)

  removeOpWisdom (SegOp op) = SegOp $ removeOpWisdom op
  removeOpWisdom (OtherOp op) = OtherOp $ removeOpWisdom op
  removeOpWisdom (SizeOp op) = SizeOp op

instance (Attributes lore, ST.IndexOp op) => ST.IndexOp (HostOp lore op) where
  indexOp vtable k (SegOp op) is = ST.indexOp vtable k op is
  indexOp vtable k (OtherOp op) is = ST.indexOp vtable k op is
  indexOp _ _ _ _ = Nothing

instance (PrettyLore lore, PP.Pretty op) => PP.Pretty (HostOp lore op) where
  ppr (SegOp op) = ppr op
  ppr (OtherOp op) = ppr op
  ppr (SizeOp op) = ppr op

instance (OpMetrics (Op lore), OpMetrics op) => OpMetrics (HostOp lore op) where
  opMetrics (SegOp op) = opMetrics op
  opMetrics (OtherOp op) = opMetrics op
  opMetrics (SizeOp op) = opMetrics op

checkSegLevel :: Maybe SegLevel -> SegLevel -> TC.TypeM lore ()
checkSegLevel Nothing _ =
  return ()
checkSegLevel (Just SegThread{}) _ =
  TC.bad $ TC.TypeError "SegOps cannot occur when already at thread level."
checkSegLevel (Just x) y
  | x == y = TC.bad $ TC.TypeError $ "Already at at level " ++ pretty x
  | segNumGroups x /= segNumGroups y || segGroupSize x /= segGroupSize y =
      TC.bad $ TC.TypeError "Physical layout for SegLevel does not match parent SegLevel."
  | otherwise =
      return ()

typeCheckHostOp :: TC.Checkable lore =>
                   (SegLevel -> OpWithAliases (Op lore) -> TC.TypeM lore ())
                -> Maybe SegLevel
                -> (op -> TC.TypeM lore ())
                -> HostOp (Aliases lore) op
                -> TC.TypeM lore ()
typeCheckHostOp checker lvl _ (SegOp op) =
  TC.checkOpWith (checker $ segLevel op) $
  typeCheckSegOp (checkSegLevel lvl) op
typeCheckHostOp _ _ f (OtherOp op) = f op
typeCheckHostOp _ _ _ (SizeOp op) = typeCheckSizeOp op
