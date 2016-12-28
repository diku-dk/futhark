{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
-- | A representation of nested-parallel in-kernel per-workgroup
-- expressions.
module Futhark.Representation.Kernels.KernelExp
  ( KernelExp (..)
  , GroupStreamLambda(..)
  , typeCheckKernelExp
  )
  where

import Control.Applicative
import Data.Traversable hiding (mapM)
import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Prelude

import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.Range as Range
import Futhark.Representation.Aliases
import Futhark.Representation.Ranges
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplifier.Lore
import Futhark.Analysis.Usage
import Futhark.Analysis.Metrics
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Util.Pretty
  ((<+>), (</>), ppr, commasep, Pretty, parens, text, apply, braces, annot, indent)
import qualified Futhark.TypeCheck as TC

data KernelExp lore = SplitArray StreamOrd SubExp SubExp SubExp SubExp [VName]
                      -- ^ @SplitArray o w i num_is elems_per_thread arrs@.
                      --
                      -- Distributes array elements to threads in a kernel.
                      -- @w@ is the length of the outer dimension in each of the
                      -- input arrays @arrs@.
                      --
                      -- A thread only takes elements if @i < num_is@
                      --
                      -- Each thread takes at most @elems_per_thread@ elements.
                      --
                      -- If the order @o@ is @InOrder@, thread with index @i@
                      -- will receive elements
                      -- @i*elems_per_tread, i*elems_per_thread + 1,
                      -- ..., i*elems_per_thread + (elems_per_thread-1)@.
                      -- This access pattern can give rise to a transpose being
                      -- generated in a later stage of the compiler.
                      --
                      -- If the order @o@ is @Disorder@, threads will get up to
                      -- @elems_per_thread@ elements in _some_ order.
                    | SplitSpace StreamOrd SubExp SubExp SubExp SubExp
                    | Combine [(VName,SubExp)] [Type] SubExp (Body lore)
                      -- ^ @Combine cspace ts active body@ will combine values
                      -- from threads to a single (multidimensional) array.
                      -- If we define @(is, ws) = unzip cspace@, then @ws@
                      -- is defined the same accross all threads.
                      -- Only threads for which
                      -- @active && all (\(i,w) -> i < w) cspace@ is true will
                      -- provide a value (of type @ts@), which is generated
                      -- by @body@.
                      --
                      -- The result of a combine is always stored in local
                      -- memory (OpenCL terminology)
                    | GroupReduce SubExp
                      (Lambda lore) [(SubExp,VName)]
                      -- ^ @GroupReduce w lam input@ (with @(nes, arrs) = unzip input@),
                      -- will perform a reduction of the arrays @arrs@ using the
                      -- associative reduction operator @lam@ and the neutral
                      -- elements @nes@.
                      --
                      -- The arrays @arrs@ must all have outer
                      -- dimension @w@, which must not be larger than
                      -- the group size.
                      --
                      -- Currently a GroupReduce consumes the input arrays, as
                      -- it uses them for scratch space to store temporary
                      -- results
                      --
                      -- All threads in a group must participate in a
                      -- GroupReduce (due to barriers)
                      --
                      -- The length of the arrays @w@ can be smaller than the
                      -- number of elements in a group (neutral element will be
                      -- filled in), but @w@ can never be larger than the group
                      -- size.
                    | GroupScan SubExp
                      (Lambda lore) [(SubExp,VName)]
                      -- ^ Same restrictions as with 'GroupReduce'.
                    | GroupStream SubExp SubExp
                      (GroupStreamLambda lore) [SubExp] [VName]
                      -- Morally a StreamSeq
                      -- First  SubExp is the outersize of the array
                      -- Second SubExp is the maximal chunk size
                      -- [SubExp] is the accumulator, [VName] are the input arrays
                    deriving (Eq, Ord, Show)

data GroupStreamLambda lore = GroupStreamLambda
  { groupStreamChunkSize :: VName
  , groupStreamChunkOffset :: VName
  , groupStreamAccParams :: [LParam lore]
  , groupStreamArrParams :: [LParam lore]
  , groupStreamLambdaBody :: Body lore
  }

deriving instance Annotations lore => Eq (GroupStreamLambda lore)
deriving instance Annotations lore => Show (GroupStreamLambda lore)
deriving instance Annotations lore => Ord (GroupStreamLambda lore)

instance Attributes lore => IsOp (KernelExp lore) where
  safeOp _ = False

instance Attributes lore => TypedOp (KernelExp lore) where
  opType (SplitArray _ _ _ _ _ arrs) =
    traverse (fmap setRetType . lookupType) arrs
    where setRetType arr_t =
            let chunk_shape = Ext 0 : map Free (drop 1 $ arrayDims arr_t)
            in arr_t `setArrayShape` ExtShape chunk_shape
  opType SplitSpace{} =
    pure $ staticShapes [Prim int32]
  opType (Combine ispace ts _ _) =
    pure $ staticShapes $ map (`arrayOfShape` shape) ts
    where shape = Shape $ map snd ispace
  opType (GroupReduce _ lam _) =
    pure $ staticShapes $ lambdaReturnType lam
  opType (GroupScan w lam _) =
    pure $ staticShapes $ map (`arrayOfRow` w) (lambdaReturnType lam)
  opType (GroupStream _ _ lam _ _) =
    pure $ staticShapes $ map paramType $ groupStreamAccParams lam

instance Attributes lore => FreeIn (KernelExp lore) where
  freeIn (SplitArray _ w i num_is elems_per_thread vs) =
    freeIn [w, i, num_is, elems_per_thread] <> freeIn vs
  freeIn (SplitSpace _ w i num_is elems_per_thread) =
    freeIn [w, i, num_is, elems_per_thread]
  freeIn (Combine cspace ts active body) =
    freeIn cspace <> freeIn ts <> freeIn active <> freeInBody body
  freeIn (GroupReduce w lam input) =
    freeIn w <> freeInLambda lam <> freeIn input
  freeIn (GroupScan w lam input) =
    freeIn w <> freeInLambda lam <> freeIn input
  freeIn (GroupStream w maxchunk lam accs arrs) =
    freeIn w <> freeIn maxchunk <> freeIn lam <> freeIn accs <> freeIn arrs

instance Attributes lore => FreeIn (GroupStreamLambda lore) where
  freeIn (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
    freeInBody body `HS.difference` bound_here
    where bound_here = HS.fromList $
                       chunk_offset : chunk_size :
                       map paramName (acc_params ++ arr_params)

instance Ranged inner => RangedOp (KernelExp inner) where
  opRanges (SplitSpace _ _ _ _ elems_per_thread) =
    [(Just (ScalarBound 0),
      Just (ScalarBound (SE.subExpToScalExp elems_per_thread int32)))]
  opRanges _ = repeat unknownRange

instance (Attributes lore, Aliased lore) => AliasedOp (KernelExp lore) where
  opAliases (SplitArray _ _ _ _ _ arrs) =
    map HS.singleton arrs
  opAliases SplitSpace{} =
    [mempty]
  opAliases Combine{} =
    [mempty]
  opAliases (GroupReduce _ lam _) =
    replicate (length (lambdaReturnType lam)) mempty
  opAliases (GroupScan _ lam _) =
    replicate (length (lambdaReturnType lam)) mempty
  opAliases (GroupStream _ _ lam _ _) =
    map (const mempty) $ groupStreamAccParams lam

  consumedInOp (GroupReduce _ _ input) =
    HS.fromList $ map snd input
  consumedInOp (GroupScan _ _ input) =
    HS.fromList $ map snd input
  consumedInOp (GroupStream _ _ lam nes arrs) =
    HS.map consumedArray $ consumedInBody body
    where GroupStreamLambda _ _ acc_params arr_params body = lam
          consumedArray v = fromMaybe v $ subExpVar =<< lookup v params_to_arrs
          params_to_arrs = zip (map paramName $ acc_params ++ arr_params) $
                           nes ++ map Var arrs

  consumedInOp SplitArray{} = mempty
  consumedInOp SplitSpace{} = mempty
  consumedInOp Combine{} = mempty

instance Attributes lore => Substitute (KernelExp lore) where
  substituteNames subst (SplitArray o w i max_is elems_per_thread vs) =
    SplitArray o
    (substituteNames subst w)
    (substituteNames subst i)
    (substituteNames subst max_is)
    (substituteNames subst elems_per_thread)
    (substituteNames subst vs)
  substituteNames subst (SplitSpace o w i max_is elems_per_thread) =
    SplitSpace o
    (substituteNames subst w)
    (substituteNames subst i)
    (substituteNames subst max_is)
    (substituteNames subst elems_per_thread)
  substituteNames subst (Combine cspace ts active v) =
    Combine (substituteNames subst cspace) ts
    (substituteNames subst active) (substituteNames subst v)
  substituteNames subst (GroupReduce w lam input) =
    GroupReduce (substituteNames subst w)
    (substituteNames subst lam) (substituteNames subst input)
  substituteNames subst (GroupScan w lam input) =
    GroupScan (substituteNames subst w)
    (substituteNames subst lam) (substituteNames subst input)
  substituteNames subst (GroupStream w maxchunk lam accs arrs) =
    GroupStream
    (substituteNames subst w) (substituteNames subst maxchunk)
    (substituteNames subst lam)
    (substituteNames subst accs) (substituteNames subst arrs)

instance Attributes lore => Substitute (GroupStreamLambda lore) where
  substituteNames
    subst (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
    GroupStreamLambda
    (substituteNames subst chunk_size)
    (substituteNames subst chunk_offset)
    (substituteNames subst acc_params)
    (substituteNames subst arr_params)
    (substituteNames subst body)

instance (Attributes lore, Renameable lore) => Rename (KernelExp lore) where
  rename (SplitArray o w i num_is elems_per_thread vs) =
    SplitArray
    <$> pure o
    <*> rename w
    <*> rename i
    <*> rename num_is
    <*> rename elems_per_thread
    <*> rename vs
  rename (SplitSpace o w i num_is elems_per_thread) =
    SplitSpace
    <$> pure o
    <*> rename w
    <*> rename i
    <*> rename num_is
    <*> rename elems_per_thread
  rename (Combine cspace ts active v) =
    Combine <$> rename cspace <*> rename ts <*> rename active <*> rename v
  rename (GroupReduce w lam input) =
    GroupReduce <$> rename w <*> rename lam <*> rename input
  rename (GroupScan w lam input) =
    GroupScan <$> rename w <*> rename lam <*> rename input
  rename (GroupStream w maxchunk lam accs arrs) =
    GroupStream <$> rename w <*> rename maxchunk <*>
    rename lam <*> rename accs <*> rename arrs

instance (Attributes lore, Renameable lore) => Rename (GroupStreamLambda lore) where
  rename (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
    bindingForRename (chunk_size : chunk_offset :
                       map paramName (acc_params++arr_params)) $
    GroupStreamLambda <$>
    rename chunk_size <*>
    rename chunk_offset <*>
    rename acc_params <*>
    rename arr_params <*>
    rename body

instance (Attributes lore,
          Attributes (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (KernelExp lore) where
  type OpWithAliases (KernelExp lore) = KernelExp (Aliases lore)

  addOpAliases (SplitArray o w i num_is elems_per_thread arrs) =
    SplitArray o w i num_is elems_per_thread arrs
  addOpAliases (SplitSpace o w i num_is elems_per_thread) =
    SplitSpace o w i num_is elems_per_thread
  addOpAliases (GroupReduce w lam input) =
    GroupReduce w (Alias.analyseLambda lam) input
  addOpAliases (GroupScan w lam input) =
    GroupScan w (Alias.analyseLambda lam) input
  addOpAliases (GroupStream w maxchunk lam accs arrs) =
    GroupStream w maxchunk lam' accs arrs
    where lam' = analyseGroupStreamLambda lam
          analyseGroupStreamLambda (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
            GroupStreamLambda chunk_size chunk_offset acc_params arr_params $
            Alias.analyseBody body
  addOpAliases (Combine ispace ts active body) =
    Combine ispace ts active $ Alias.analyseBody body

  removeOpAliases (GroupReduce w lam input) =
    GroupReduce w (removeLambdaAliases lam) input
  removeOpAliases (GroupScan w lam input) =
    GroupScan w (removeLambdaAliases lam) input
  removeOpAliases (GroupStream w maxchunk lam accs arrs) =
    GroupStream w maxchunk (removeGroupStreamLambdaAliases lam) accs arrs
    where removeGroupStreamLambdaAliases (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
            GroupStreamLambda chunk_size chunk_offset acc_params arr_params $
            removeBodyAliases body

  removeOpAliases (Combine ispace ts active body) =
    Combine ispace ts active $ removeBodyAliases body
  removeOpAliases (SplitArray o w i num_is elems_per_thread arrs) =
    SplitArray o w i num_is elems_per_thread arrs
  removeOpAliases (SplitSpace o w i num_is elems_per_thread) =
    SplitSpace o w i num_is elems_per_thread

instance (Attributes lore,
          Attributes (Ranges lore),
          CanBeRanged (Op lore)) => CanBeRanged (KernelExp lore) where
  type OpWithRanges (KernelExp lore) = KernelExp (Ranges lore)

  addOpRanges (SplitArray o w i num_is elems_per_thread arrs) =
    SplitArray o w i num_is elems_per_thread arrs
  addOpRanges (SplitSpace o w i num_is elems_per_thread) =
    SplitSpace o w i num_is elems_per_thread
  addOpRanges (GroupReduce w lam input) =
    GroupReduce w (Range.runRangeM $ Range.analyseLambda lam) input
  addOpRanges (GroupScan w lam input) =
    GroupScan w (Range.runRangeM $ Range.analyseLambda lam) input
  addOpRanges (Combine ispace ts active body) =
    Combine ispace ts active $ Range.runRangeM $ Range.analyseBody body
  addOpRanges (GroupStream w maxchunk lam accs arrs) =
    GroupStream w maxchunk lam' accs arrs
    where lam' = analyseGroupStreamLambda lam
          analyseGroupStreamLambda (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
            GroupStreamLambda chunk_size chunk_offset acc_params arr_params $
            Range.runRangeM $ Range.analyseBody body

  removeOpRanges (GroupReduce w lam input) =
    GroupReduce w (removeLambdaRanges lam) input
  removeOpRanges (GroupScan w lam input) =
    GroupScan w (removeLambdaRanges lam) input
  removeOpRanges (GroupStream w maxchunk lam accs arrs) =
    GroupStream w maxchunk (removeGroupStreamLambdaRanges lam) accs arrs
    where removeGroupStreamLambdaRanges (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
            GroupStreamLambda chunk_size chunk_offset acc_params arr_params $
            removeBodyRanges body
  removeOpRanges (Combine ispace ts active body) =
    Combine ispace ts active $ removeBodyRanges body
  removeOpRanges (SplitArray o w i num_is elems_per_thread arrs) =
    SplitArray o w i num_is elems_per_thread arrs
  removeOpRanges (SplitSpace o w i num_is elems_per_thread) =
    SplitSpace o w i num_is elems_per_thread

instance (Attributes lore, CanBeWise (Op lore)) => CanBeWise (KernelExp lore) where
  type OpWithWisdom (KernelExp lore) = KernelExp (Wise lore)

  removeOpWisdom (GroupReduce w lam input) =
    GroupReduce w (removeLambdaWisdom lam) input
  removeOpWisdom (GroupScan w lam input) =
    GroupScan w (removeLambdaWisdom lam) input
  removeOpWisdom (GroupStream w maxchunk lam accs arrs) =
    GroupStream w maxchunk (removeGroupStreamLambdaWisdom lam) accs arrs
    where removeGroupStreamLambdaWisdom
            (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
            GroupStreamLambda chunk_size chunk_offset acc_params arr_params $
            removeBodyWisdom body
  removeOpWisdom (Combine ispace ts active body) =
    Combine ispace ts active $ removeBodyWisdom body
  removeOpWisdom (SplitArray o w i num_is elems_per_thread arrs) =
    SplitArray o w i num_is elems_per_thread arrs
  removeOpWisdom (SplitSpace o w i num_is elems_per_thread) =
    SplitSpace o w i num_is elems_per_thread

instance (Attributes lore, Aliased lore, UsageInOp (Op lore)) => UsageInOp (KernelExp lore) where
  usageInOp _ = mempty

instance OpMetrics (Op lore) => OpMetrics (KernelExp lore) where
  opMetrics SplitArray{} = seen "SplitArray"
  opMetrics SplitSpace{} = seen "SplitSpace"
  opMetrics Combine{} = seen "Combine"
  opMetrics (GroupReduce _ lam _) = inside "GroupReduce" $ lambdaMetrics lam
  opMetrics (GroupScan _ lam _) = inside "GroupScan" $ lambdaMetrics lam
  opMetrics (GroupStream _ _ lam _ _) =
    inside "GroupStream" $ groupStreamLambdaMetrics lam
    where groupStreamLambdaMetrics =
            bodyMetrics . groupStreamLambdaBody

typeCheckKernelExp :: TC.Checkable lore => KernelExp (Aliases lore) -> TC.TypeM lore ()
typeCheckKernelExp (SplitArray _ w i num_is elems_per_thread arrs) = do
  mapM_ (TC.require [Prim int32]) [w, i, num_is, elems_per_thread]
  void $ TC.checkSOACArrayArgs w arrs

typeCheckKernelExp (SplitSpace _ w i num_is elems_per_thread) =
  mapM_ (TC.require [Prim int32]) [w, i, num_is, elems_per_thread]

typeCheckKernelExp (Combine cspace ts active body) = do
  mapM_ (TC.requireI [Prim int32]) is
  mapM_ TC.checkType ts
  mapM_ (TC.require [Prim int32]) ws
  TC.require [Prim Bool] active
  TC.checkLambdaBody ts body
  where (is, ws) = unzip cspace

typeCheckKernelExp (GroupReduce w lam input) =
  checkScanOrReduce w lam input

typeCheckKernelExp (GroupScan w lam input) =
  checkScanOrReduce w lam input

typeCheckKernelExp (GroupStream w maxchunk lam accs arrs) = do
  TC.require [Prim int32] w
  TC.require [Prim int32] maxchunk

  acc_args <- mapM TC.checkArg accs
  arr_args <- TC.checkSOACArrayArgs w arrs

  checkGroupStreamLambda acc_args arr_args
  where GroupStreamLambda block_size _ acc_params arr_params body = lam
        checkGroupStreamLambda acc_args arr_args = do
          unless (map TC.argType acc_args == map paramType acc_params) $
            TC.bad $ TC.TypeError
            "checkGroupStreamLambda: wrong accumulator arguments."

          let arr_block_ts =
                map ((`arrayOfRow` Var block_size) . TC.argType) arr_args
          unless (map paramType arr_params == arr_block_ts) $
            TC.bad $ TC.TypeError
            "checkGroupStreamLambda: wrong array arguments."

          let acc_consumable =
                zip (map paramName acc_params) (map TC.argAliases acc_args)
              arr_consumable =
                zip (map paramName arr_params) (map TC.argAliases arr_args)
              consumable = acc_consumable ++ arr_consumable
          TC.binding (scopeOf lam) $ TC.consumeOnlyParams consumable $ do
            TC.checkLambdaParams acc_params
            TC.checkLambdaParams arr_params
            TC.checkLambdaBody (map TC.argType acc_args) body


checkScanOrReduce :: TC.Checkable lore =>
                     SubExp -> Lambda (Aliases lore) -> [(SubExp, VName)]
                  -> TC.TypeM lore ()
checkScanOrReduce w lam input = do
  TC.require [Prim int32] w
  let (nes, arrs) = unzip input
      asArg t = (t, mempty)
  neargs <- mapM TC.checkArg nes
  arrargs <- TC.checkSOACArrayArgs w arrs
  TC.checkLambda lam $
    map asArg [Prim int32, Prim int32] ++
    map TC.noArgAliases (neargs ++ arrargs)

instance LParamAttr lore1 ~ LParamAttr lore2 =>
         Scoped lore1 (GroupStreamLambda lore2) where
  scopeOf (GroupStreamLambda chunk_size chunk_offset acc_params arr_params _) =
    HM.insert chunk_size (IndexInfo Int32) $
    HM.insert chunk_offset (IndexInfo Int32) $
    scopeOfLParams (acc_params ++ arr_params)

instance PrettyLore lore => Pretty (KernelExp lore) where
  ppr (SplitArray o w i num_is elems_per_thread arrs) =
    text ("splitArray" <> suff) <>
    parens (commasep $ ppr w : ppr elems_per_thread :
            (ppr i <+> text "<" <+> ppr num_is) :
            map ppr arrs)
    where suff = case o of InOrder -> ""
                           Disorder -> "Unordered"
  ppr (SplitSpace o w i num_is elems_per_thread) =
    text ("splitSpace" <> suff) <>
    parens (commasep [ppr w, ppr elems_per_thread,
                      ppr i <+> text "<" <+> ppr num_is])
    where suff = case o of InOrder -> ""
                           Disorder -> "Unordered"
  ppr (Combine cspace ts active body) =
    text "combine" <>
    apply (map f cspace ++ [apply (map ppr ts), ppr active]) <+> text "{" </>
    indent 2 (ppr body) </>
    text "}"
    where f (i, w) = ppr i <+> text "<" <+> ppr w
  ppr (GroupReduce w lam input) =
    text "reduce" <> parens (commasep [ppr w,
                                       ppr lam,
                                       braces (commasep $ map ppr nes),
                                       commasep $ map ppr els])
    where (nes,els) = unzip input
  ppr (GroupScan w lam input) =
    text "scan" <> parens (commasep [ppr w,
                                     ppr lam,
                                     braces (commasep $ map ppr nes),
                                     commasep $ map ppr els])
    where (nes,els) = unzip input
  ppr (GroupStream w maxchunk lam accs arrs) =
    text "stream" <>
    parens (commasep [ppr w,
                      ppr maxchunk,
                      ppr lam,
                      braces (commasep $ map ppr accs),
                      commasep $ map ppr arrs])

instance PrettyLore lore => Pretty (GroupStreamLambda lore) where
  ppr (GroupStreamLambda block_size block_offset acc_params arr_params body) =
    annot (mapMaybe ppAnnot params) $
    text "fn" <+>
    parens (commasep (block_size' : block_offset' : map ppr params)) <+>
    text "=>" </> indent 2 (ppr body)
    where params = acc_params ++ arr_params
          block_size' = text "int" <+> ppr block_size
          block_offset' = text "int" <+> ppr block_offset
