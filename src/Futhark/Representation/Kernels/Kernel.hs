{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Futhark.Representation.Kernels.Kernel
       ( Kernel(..)
       , KernelBody(..)
       , KernelStm(..)
       , KernelResult(..)

       , KernelInput(..)
       , kernelInputName
       , kernelInputType
       , kernelInputIdent
       , KernelSize(..)
       , chunkedKernelNonconcatOutputs

       , typeCheckKernel

         -- * Generic traversal
       , KernelMapper(..)
       , identityKernelMapper
       , mapKernelM
       )
       where

import Control.Arrow (first, (&&&))
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Identity
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Maybe

import Prelude

import Futhark.Representation.AST
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.UsageTable as UT
import qualified Futhark.Util.Pretty as PP
import Futhark.Util.Pretty
  ((</>), (<+>), ppr, comma, commasep, Pretty, parens, text)
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplifier.Lore
import Futhark.Representation.Ranges
  (Ranges, removeLambdaRanges, removeBodyRanges)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases
  (Aliases, removeLambdaAliases, removeBodyAliases, Names'(..))
import Futhark.Analysis.Usage
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics
import Futhark.Tools (partitionChunkedKernelLambdaParameters)
import qualified Futhark.Analysis.Range as Range

data Kernel lore =
    MapKernel Certificates SubExp VName [(VName, SubExp)] [KernelInput lore]
    [(Type, [Int])] (Body lore)
  | ScanKernel Certificates SubExp
    KernelSize
    (LambdaT lore)
    (LambdaT lore)
    [SubExp]
    [VName]
  | WriteKernel Certificates SubExp
    (LambdaT lore)
    [VName]
    [(SubExp, VName)]
    -- See SOAC.hs for what the different WriteKernel arguments mean.
  | NumGroups
  | GroupSize

  | Kernel Certificates
    (SubExp,SubExp,SubExp) -- #workgroups, group size, #threads
    [Type]
    VName -- thread ID (binding position)
    (KernelBody lore)

    deriving (Eq, Show, Ord)

data KernelBody lore = KernelBody { kernelBodyStms :: [KernelStm lore]
                                  , kernelBodyResult :: [KernelResult]
                                  }
                deriving (Eq, Show, Ord)

data KernelResult = AllThreadsReturn SubExp
                  | ThisThreadReturns
                    SubExp -- Which one.
                    SubExp -- What.
                  | ConcatReturns
                    StreamOrd -- Permuted?
                    SubExp -- The final size.
                    SubExp -- Per-thread (max) chunk size.
                    VName -- Chunk by this thread.
                  deriving (Eq, Show, Ord)

data KernelStm lore = SplitArray (VName, [PatElem (LetAttr lore)]) StreamOrd SubExp SubExp [VName]
                    | Thread [PatElem (LetAttr lore)] (Body lore)
                    | Combine (PatElem (LetAttr lore)) SubExp
                    | GroupReduce [PatElem (LetAttr lore)] SubExp (Lambda lore) [(SubExp,VName)]

deriving instance Annotations lore => Eq (KernelStm lore)
deriving instance Annotations lore => Show (KernelStm lore)
deriving instance Annotations lore => Ord (KernelStm lore)

boundByKernelStm :: KernelStm lore -> Names
boundByKernelStm = HS.fromList . HM.keys . scopeOf

data KernelInput lore = KernelInput { kernelInputParam :: LParam lore
                                    , kernelInputArray :: VName
                                    , kernelInputIndices :: [SubExp]
                                    }

deriving instance Annotations lore => Eq (KernelInput lore)
deriving instance Annotations lore => Show (KernelInput lore)
deriving instance Annotations lore => Ord (KernelInput lore)

kernelInputName :: KernelInput lore -> VName
kernelInputName = paramName . kernelInputParam

kernelInputType :: Typed (LParamAttr lore) =>
                   KernelInput lore -> Type
kernelInputType = typeOf . kernelInputParam

kernelInputIdent :: Typed (LParamAttr lore) =>
                    KernelInput lore -> Ident
kernelInputIdent = paramIdent . kernelInputParam

data KernelSize = KernelSize { kernelWorkgroups :: SubExp
                             , kernelWorkgroupSize :: SubExp
                             , kernelElementsPerThread :: SubExp
                             , kernelTotalElements :: SubExp
                             , kernelThreadOffsetMultiple :: SubExp
                             , kernelNumThreads :: SubExp
                             }
                deriving (Eq, Ord, Show)

-- | Like 'Mapper', but just for 'Kernel's.
data KernelMapper flore tlore m = KernelMapper {
    mapOnKernelSubExp :: SubExp -> m SubExp
  , mapOnKernelLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnKernelBody :: Body flore -> m (Body tlore)
  , mapOnKernelVName :: VName -> m VName
  , mapOnKernelCertificates :: Certificates -> m Certificates
  , mapOnKernelLParam :: LParam flore -> m (LParam tlore)
  , mapOnKernelKernelBody :: KernelBody flore -> m (KernelBody tlore)
  }

-- | A mapper that simply returns the 'Kernel' verbatim.
identityKernelMapper :: Monad m => KernelMapper lore lore m
identityKernelMapper = KernelMapper { mapOnKernelSubExp = return
                                    , mapOnKernelLambda = return
                                    , mapOnKernelBody = return
                                    , mapOnKernelVName = return
                                    , mapOnKernelCertificates = return
                                    , mapOnKernelLParam = return
                                    , mapOnKernelKernelBody = return
                                    }

-- | Map a monadic action across the immediate children of a
-- Kernel.  The mapping does not descend recursively into subexpressions
-- and is done left-to-right.
mapKernelM :: (Applicative m, Monad m) =>
              KernelMapper flore tlore m -> Kernel flore -> m (Kernel tlore)
mapKernelM tv (MapKernel cs w index ispace inps rettype body) =
  MapKernel <$>
  mapOnKernelCertificates tv cs <*>
  mapOnKernelSubExp tv w <*>
  mapOnKernelVName tv index <*>
  (zip iparams <$> mapM (mapOnKernelSubExp tv) bounds) <*>
  mapM (mapOnKernelInput tv) inps <*>
  (zip <$> mapM (mapOnType $ mapOnKernelSubExp tv) ts <*> pure perms) <*>
  mapOnKernelBody tv body
  where (iparams, bounds) = unzip ispace
        (ts, perms) = unzip rettype
mapKernelM tv (ScanKernel cs w kernel_size fun fold_fun nes arrs) =
  ScanKernel <$>
  mapOnKernelCertificates tv cs <*>
  mapOnKernelSubExp tv w <*>
  mapOnKernelSize tv kernel_size <*>
  mapOnKernelLambda tv fun <*>
  mapOnKernelLambda tv fold_fun <*>
  mapM (mapOnKernelSubExp tv) nes <*>
  mapM (mapOnKernelVName tv) arrs
mapKernelM tv (WriteKernel cs len lam ivs as) =
  WriteKernel <$>
  mapOnKernelCertificates tv cs <*>
  mapOnKernelSubExp tv len <*>
  mapOnKernelLambda tv lam <*>
  mapM (mapOnKernelVName tv) ivs <*>
  mapM (\(aw,a) -> (,) <$> mapOnKernelSubExp tv aw <*> mapOnKernelVName tv a) as
mapKernelM _ NumGroups = pure NumGroups
mapKernelM _ GroupSize = pure GroupSize
mapKernelM tv (Kernel cs (num_groups, group_size, num_threads) ts thread_id kernel_body) =
  Kernel <$> mapOnKernelCertificates tv cs <*>
  (do num_groups' <- mapOnKernelSubExp tv num_groups
      group_size' <- mapOnKernelSubExp tv group_size
      num_threads' <- mapOnKernelSubExp tv num_threads
      return (num_groups', group_size', num_threads')) <*>
  mapM (mapOnKernelType tv) ts <*>
  pure thread_id <*>
  mapOnKernelKernelBody tv kernel_body

mapOnKernelType :: (Monad m, Applicative m, Functor m) =>
                   KernelMapper flore tlore m -> Type -> m Type
mapOnKernelType _tv (Prim pt) = pure $ Prim pt
mapOnKernelType tv (Array pt shape u) = Array pt <$> f shape <*> pure u
  where f (Shape dims) = Shape <$> mapM (mapOnKernelSubExp tv) dims
mapOnKernelType _tv (Mem se s) = pure $ Mem se s

mapOnKernelSize :: (Monad m, Applicative m) =>
                   KernelMapper flore tlore m -> KernelSize -> m KernelSize
mapOnKernelSize tv (KernelSize num_workgroups workgroup_size
                    per_thread_elements num_elements offset_multiple num_threads) =
  KernelSize <$>
  mapOnKernelSubExp tv num_workgroups <*>
  mapOnKernelSubExp tv workgroup_size <*>
  mapOnKernelSubExp tv per_thread_elements <*>
  mapOnKernelSubExp tv num_elements <*>
  mapOnKernelSubExp tv offset_multiple <*>
  mapOnKernelSubExp tv num_threads

mapOnKernelInput :: (Monad m, Applicative m) =>
                    KernelMapper flore tlore m -> KernelInput flore
                 -> m (KernelInput tlore)
mapOnKernelInput tv (KernelInput param arr is) =
  KernelInput <$> mapOnKernelLParam tv param <*>
                  mapOnKernelVName tv arr <*>
                  mapM (mapOnKernelSubExp tv) is

instance FreeIn KernelSize where
  freeIn (KernelSize num_workgroups workgroup_size elems_per_thread
          num_elems thread_offset num_threads) =
    mconcat $ map freeIn [num_workgroups,
                          workgroup_size,
                          elems_per_thread,
                          num_elems,
                          thread_offset,
                          num_threads]

instance (FreeIn (LParamAttr lore)) =>
         FreeIn (KernelInput lore) where
  freeIn (KernelInput param arr is) =
    freeIn param <> freeIn arr <> freeIn is

instance (Attributes lore, FreeIn (LParamAttr lore)) =>
         FreeIn (Kernel lore) where
  freeIn (MapKernel cs w index ispace inps returns body) =
    freeIn w <> freeIn cs <> freeIn index <> freeIn (map snd ispace) <>
    freeIn (map fst returns) <>
    ((freeIn inps <> freeInBody body) `HS.difference` bound)
    where bound = HS.fromList $
                  [index] ++ map fst ispace ++ map kernelInputName inps

  freeIn e = execWriter $ mapKernelM free e
    where walk f x = tell (f x) >> return x
          free = KernelMapper { mapOnKernelSubExp = walk freeIn
                              , mapOnKernelLambda = walk freeInLambda
                              , mapOnKernelBody = walk freeInBody
                              , mapOnKernelVName = walk freeIn
                              , mapOnKernelCertificates = walk freeIn
                              , mapOnKernelLParam = walk freeIn
                              , mapOnKernelKernelBody = walk freeIn
                              }

instance Attributes lore => FreeIn (KernelBody lore) where
  freeIn kernel_body = free_in_stms `HS.difference` bound_in_stms
    where free_in_stms = mconcat $ map freeIn $ kernelBodyStms kernel_body
          bound_in_stms = mconcat $ map boundByKernelStm $ kernelBodyStms kernel_body

instance Attributes lore => FreeIn (KernelStm lore) where
  freeIn (SplitArray (n,chunks) _ w elems_per_thread vs) =
    freeIn n <> freeIn chunks <> freeIn w <> freeIn elems_per_thread <> freeIn vs
  freeIn (Thread pes body) =
    freeIn pes <> freeInBody body
  freeIn (Combine pe v) =
    freeIn pe <> freeIn v
  freeIn (GroupReduce pes w lam input) =
    freeIn pes <> freeIn w <> freeInLambda lam <> freeIn input

instance Attributes lore => Substitute (KernelBody lore) where
  substituteNames subst (KernelBody stms res) =
    KernelBody (substituteNames subst stms) $ substituteNames subst res

instance Substitute KernelResult where
  substituteNames subst (AllThreadsReturn se) =
    AllThreadsReturn $ substituteNames subst se
  substituteNames subst (ThisThreadReturns who what) =
    ThisThreadReturns (substituteNames subst who) (substituteNames subst what)
  substituteNames subst (ConcatReturns ord w per_thread_elems v) =
    ConcatReturns
    ord
    (substituteNames subst w)
    (substituteNames subst per_thread_elems)
    (substituteNames subst v)

instance Attributes lore => Substitute (KernelStm lore) where
  substituteNames subst (SplitArray (n,arrs) o w elems_per_thread vs) =
    SplitArray (n,arrs) o
    (substituteNames subst w)
    (substituteNames subst elems_per_thread)
    (substituteNames subst vs)
  substituteNames subst (Thread pes body) =
    Thread (substituteNames subst pes) (substituteNames subst body)
  substituteNames subst (Combine pe v) =
    Combine (substituteNames subst pe) (substituteNames subst v)
  substituteNames subst (GroupReduce pes w lam input) =
    GroupReduce (substituteNames subst pes) (substituteNames subst w)
    (substituteNames subst lam) (substituteNames subst input)

instance Attributes lore => Substitute (Kernel lore) where
  substituteNames subst =
    runIdentity . mapKernelM substitute
    where substitute =
            KernelMapper { mapOnKernelSubExp = return . substituteNames subst
                         , mapOnKernelLambda = return . substituteNames subst
                         , mapOnKernelBody = return . substituteNames subst
                         , mapOnKernelVName = return . substituteNames subst
                         , mapOnKernelCertificates = return . substituteNames subst
                         , mapOnKernelLParam = return . substituteNames subst
                         , mapOnKernelKernelBody = return . substituteNames subst
                         }

instance Renameable lore => Rename (KernelBody lore) where
  rename (KernelBody [] res) =
    KernelBody [] <$> rename res
  rename (KernelBody (stm:stms) res) =
    bindingForRename (HS.toList $ boundByKernelStm stm) $ do
      stm' <- rename stm
      KernelBody stms' res' <- rename $ KernelBody stms res
      return $ KernelBody (stm':stms') res'

instance Renameable lore => Rename (KernelStm lore) where
  rename (SplitArray (n,chunks) o w elems_per_thread vs) =
    SplitArray <$> ((,) <$> rename n <*> rename chunks)
    <*> pure o
    <*> rename w
    <*> rename elems_per_thread
    <*> rename vs
  rename (GroupReduce pes w lam input) =
    GroupReduce <$> rename pes <*> rename w <*> rename lam <*> rename input
  rename (Combine pe v) =
    Combine <$> rename pe <*> rename v
  rename (Thread pes body) =
    Thread <$> rename pes <*> rename body

instance Rename KernelResult where
  rename (AllThreadsReturn se) =
    AllThreadsReturn <$> rename se
  rename (ThisThreadReturns who what) =
    ThisThreadReturns <$> rename who <*> rename what
  rename (ConcatReturns ord w per_thread_elems v) =
    ConcatReturns ord <$> rename w <*> rename per_thread_elems <*> rename v

instance Renameable lore => Rename (KernelInput lore) where
  rename (KernelInput param arr is) =
    KernelInput <$> rename param <*> rename arr <*> rename is

instance Scoped lore (KernelInput lore) where
  scopeOf inp = scopeOfLParams [kernelInputParam inp]

instance Scoped lore (KernelStm lore) where
  scopeOf (SplitArray (size, chunks) _ _ _ _) =
    HM.fromList $
    (size, IndexInfo) : map (patElemName &&& LetInfo . patElemAttr) chunks
  scopeOf (Thread pes _) =
    HM.fromList $ map entry pes
    where entry pe = (patElemName pe, LetInfo $ patElemAttr pe)
  scopeOf (Combine pe _) = scopeOf pe
  scopeOf (GroupReduce pes _ _ _) =
    HM.fromList $ map entry pes
    where entry pe = (patElemName pe, LetInfo $ patElemAttr pe)

instance Attributes lore => Rename (Kernel lore) where
  rename (MapKernel cs w index ispace inps returns body) = do
    cs' <- rename cs
    w' <- rename w
    returns' <- forM returns $ \(t, perm) -> do
      t' <- rename t
      return (t', perm)
    bindingForRename (index : map fst ispace ++ map kernelInputName inps) $
      MapKernel cs' w' <$>
      rename index <*> rename ispace <*>
      rename inps <*> pure returns' <*> rename body

  rename e = mapKernelM renamer e
    where renamer = KernelMapper rename rename rename rename rename rename rename

kernelType :: Kernel lore -> [Type]
kernelType (MapKernel _ _ _ is _ returns _) =
  [ rearrangeType perm (arrayOfShape t outer_shape)
  | (t, perm) <- returns ]
  where outer_shape = Shape $ map snd is
kernelType (ScanKernel _ w size lam foldlam nes _) =
  let arr_row_tp = drop (length nes) $ lambdaReturnType foldlam
  in map (`arrayOfRow` w) (lambdaReturnType lam) ++
     map (`arrayOfRow` kernelWorkgroups size) (lambdaReturnType lam) ++
     map (`arrayOfRow` kernelTotalElements size) arr_row_tp
kernelType (WriteKernel _ _ lam _ input) =
  zipWith arrayOfRow (snd $ splitAt (n `div` 2) lam_ts) ws
  where lam_ts = lambdaReturnType lam
        n = length lam_ts
        ws = map fst input
kernelType (Kernel _ (num_groups, _, num_threads) ts _ body) =
  zipWith resultShape ts $ kernelBodyResult body
  where resultShape t AllThreadsReturn{} =
          t `arrayOfRow` num_threads
        resultShape t ThisThreadReturns{} =
          t `arrayOfRow` num_groups
        resultShape t (ConcatReturns _ w _ _) =
          t `arrayOfRow` w

kernelType NumGroups =
  [Prim int32]
kernelType GroupSize =
  [Prim int32]

chunkedKernelNonconcatOutputs :: Lambda lore -> Int
chunkedKernelNonconcatOutputs fun =
  length $ takeWhile (not . outerSizeIsChunk) $ lambdaReturnType fun
  where outerSizeIsChunk = (==Var (paramName chunk)) . arraySize 0
        (_, chunk, _) = partitionChunkedKernelLambdaParameters $ lambdaParams fun

instance TypedOp (Kernel lore) where
  opType = pure . staticShapes . kernelType

instance (Attributes lore, Aliased lore) => AliasedOp (Kernel lore) where
  opAliases = map (const mempty) . kernelType

  consumedInOp (MapKernel _ _ _ _ inps _ body) =
    HS.fromList $
    map kernelInputArray $
    filter ((`HS.member` consumed) . kernelInputName) inps
    where consumed = consumedInBody body
  consumedInOp (Kernel _ _ _ _ kbody) =
    consumedInKernelBody kbody
  consumedInOp _ = mempty

instance (Attributes lore,
          Attributes (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (Kernel lore) where
  type OpWithAliases (Kernel lore) = Kernel (Aliases lore)

  addOpAliases = runIdentity . mapKernelM alias
    where alias = KernelMapper return (return . Alias.analyseLambda)
                  (return . Alias.analyseBody) return return return
                  (return . aliasAnalyseKernelBody)
          aliasAnalyseKernelBody (KernelBody stms res) =
            KernelBody (map analyseStm stms) res
          analyseStm (SplitArray (size, chunks) o w elems_per_thread arrs) =
            SplitArray (size, chunks') o w elems_per_thread arrs
            where chunks' = [ fmap (Names' $ HS.singleton arr,) chunk
                              | (chunk, arr) <- zip chunks arrs ]
          analyseStm (Thread pes body) =
            Thread (zipWith annot pes $ bodyAliases body') body'
            where body' = Alias.analyseBody body
                  annot pe als = (Names' als,) <$> pe
          analyseStm (Combine pe v) =
            Combine ((mempty,) <$> pe) v
          analyseStm (GroupReduce pes w lam input) =
            GroupReduce pes' w lam' input
            where pes' = map (fmap (mempty,)) pes
                  lam' = Alias.analyseLambda lam

  removeOpAliases = runIdentity . mapKernelM remove
    where remove = KernelMapper return (return . removeLambdaAliases)
                   (return . removeBodyAliases) return return return
                   (return . removeKernelBodyAliases)
          removeKernelBodyAliases (KernelBody stms res) =
            KernelBody (map removeStmAliases stms) res
          removeStmAliases (SplitArray (size, chunks) o w elems_per_thread arrs) =
            SplitArray (size, chunks') o w elems_per_thread arrs
            where chunks' = map (fmap snd) chunks
          removeStmAliases (Thread pes body) =
            Thread (map (fmap snd) pes) (removeBodyAliases body)
          removeStmAliases (Combine pe v) =
            Combine (snd <$> pe) v
          removeStmAliases (GroupReduce pes w lam input) =
            GroupReduce (map (fmap snd) pes) w (removeLambdaAliases lam) input

instance Attributes lore => IsOp (Kernel lore) where
  safeOp _ = False

instance Ranged inner => RangedOp (Kernel inner) where
  opRanges op = replicate (length $ kernelType op) unknownRange

instance (Attributes lore, CanBeRanged (Op lore)) => CanBeRanged (Kernel lore) where
  type OpWithRanges (Kernel lore) = Kernel (Ranges lore)

  removeOpRanges = runIdentity . mapKernelM remove
    where remove = KernelMapper return (return . removeLambdaRanges)
                   (return . removeBodyRanges) return return return
                   (return . removeKernelBodyRanges)
          removeKernelBodyRanges = undefined
  addOpRanges = Range.runRangeM . mapKernelM add
    where add = KernelMapper return Range.analyseLambda
                Range.analyseBody return return return addKernelBodyRanges
          addKernelBodyRanges = undefined

instance (Attributes lore, CanBeWise (Op lore)) => CanBeWise (Kernel lore) where
  type OpWithWisdom (Kernel lore) = Kernel (Wise lore)

  removeOpWisdom = runIdentity . mapKernelM remove
    where remove = KernelMapper return
                   (return . removeLambdaWisdom)
                   (return . removeBodyWisdom)
                   return return return
                   (return . removeKernelBodyWisdom)
          removeKernelBodyWisdom (KernelBody stms res) =
            KernelBody (map removeKernelStatementWisdom stms) res
          removeKernelStatementWisdom (Thread pes body) =
            Thread (map removePatElemWisdom pes) (removeBodyWisdom body)
          removeKernelStatementWisdom (Combine pe v) =
            Combine (removePatElemWisdom pe) v
          removeKernelStatementWisdom (SplitArray (size,chunks) o w elems_per_thread arrs) =
            SplitArray (size, map removePatElemWisdom chunks) o w elems_per_thread arrs
          removeKernelStatementWisdom (GroupReduce pes w lam input) =
            GroupReduce (map removePatElemWisdom pes) w (removeLambdaWisdom lam) input

instance (Attributes lore, Aliased lore, UsageInOp (Op lore)) => UsageInOp (Kernel lore) where
  usageInOp (ScanKernel _ _ _ _ foldfun _ arrs) =
    usageInLambda foldfun arrs
  usageInOp (MapKernel _ _ _ _ inps _ body) =
    mconcat $
    map (UT.consumedUsage . kernelInputArray) $
    filter ((`HS.member` consumed_in_body) . kernelInputName) inps
    where consumed_in_body = consumedInBody body
  usageInOp (WriteKernel _ _ _ _ as) =
    mconcat $ map (UT.consumedUsage . snd) as
  usageInOp (Kernel _ _ _ _ kbody) =
    mconcat $ map UT.consumedUsage $ HS.toList $ consumedInKernelBody kbody
  usageInOp NumGroups = mempty
  usageInOp GroupSize = mempty

consumedInKernelBody :: (Attributes lore, Aliased lore) =>
                     KernelBody lore -> Names
consumedInKernelBody (KernelBody stms _) =
  -- We need to figure out what is consumed in stms.  We do this by
  -- moving backwards through the stms, using the alias information to
  -- update.
  let consumed = foldr update mempty stms
  in consumed `HS.difference` bound_in_stms
  where bound_in_stms = HS.fromList $ HM.keys $ scopeOf stms

        update stm consumed =
          let aliasmap = HM.map nameAndAliases $ scopeOf stm
          in aliasClosure aliasmap consumed <> consumedByKernelStm stm

        aliasClosure aliasmap names =
          names `HS.union` mconcat (map look $ HS.toList names)
          where look k = HM.lookupDefault mempty k aliasmap

        nameAndAliases (LetInfo attr) = aliasesOf attr
        nameAndAliases _ = mempty

consumedByKernelStm :: (Attributes lore, Aliased lore) =>
                       KernelStm lore -> Names
consumedByKernelStm (Thread _ body) = consumedInBody body
consumedByKernelStm Combine{} = mempty
consumedByKernelStm SplitArray{} = mempty
consumedByKernelStm (GroupReduce _ _ _ input) =
  HS.fromList $ map snd input

typeCheckKernel :: TC.Checkable lore => Kernel (Aliases lore) -> TC.TypeM lore ()

typeCheckKernel (MapKernel cs w index ispace inps returns body) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  TC.require [Prim int32] w
  mapM_ (TC.require [Prim int32]) bounds

  index_param <- TC.primLParam index int32
  iparams' <- forM iparams $ \iparam -> TC.primLParam iparam int32
  forM_ returns $ \(t, perm) ->
    let return_rank = arrayRank t + rank
    in unless (sort perm == [0..return_rank - 1]) $
       TC.bad $ TC.TypeError $
       "Permutation " ++ pretty perm ++
       " not valid for returning " ++ pretty t ++
       " from a rank " ++ pretty rank ++ " kernel."

  inps_als <- mapM (TC.lookupAliases . kernelInputArray) inps
  let consumable_inps = consumableInputs (map fst ispace) $
                        zip inps inps_als

  TC.checkFun' (nameFromString "<kernel body>",
                map (`toDecl` Nonunique) $ staticShapes rettype,
                lamParamsToNameInfos (index_param : iparams') ++
                kernelInputsToNameInfos inps,
                body) consumable_inps $ do
    TC.checkLambdaParams $ map kernelInputParam inps
    mapM_ checkKernelInput inps
    TC.checkBody body
    bodyt <- bodyExtType body
    unless (map rankShaped bodyt ==
            map rankShaped (staticShapes rettype)) $
      TC.bad $
      TC.ReturnTypeError (nameFromString "<kernel body>")
      (staticShapes rettype) bodyt
  where (iparams, bounds) = unzip ispace
        rank = length ispace
        (rettype, _) = unzip returns
        checkKernelInput inp = do
          TC.checkExp $ PrimOp $ Index []
            (kernelInputArray inp) (kernelInputIndices inp)

          arr_t <- lookupType $ kernelInputArray inp
          unless (stripArray (length $ kernelInputIndices inp) arr_t ==
                  kernelInputType inp) $
            TC.bad $ TC.TypeError $
            "Kernel input " ++ pretty inp ++ " has inconsistent type."

typeCheckKernel (ScanKernel cs w kernel_size fun foldfun nes arrs) = do
  checkKernelCrud cs w kernel_size

  let index_arg = (Prim int32, mempty)
  arrargs <- TC.checkSOACArrayArgs w arrs
  accargs <- mapM TC.checkArg nes
  TC.checkLambda foldfun $ index_arg : index_arg : accargs ++ arrargs

  TC.checkLambda fun $ index_arg : index_arg : accargs ++ accargs
  let startt      = map TC.argType accargs
      funret      = lambdaReturnType fun
      foldret     = lambdaReturnType foldfun
      (fold_accret, _fold_arrret) = splitAt (length nes) foldret
  unless (startt == funret) $
    TC.bad $ TC.TypeError $
    "Neutral value is of type " ++ prettyTuple startt ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."
  unless (startt == fold_accret) $
    TC.bad $ TC.TypeError $
    "Neutral value is of type " ++ prettyTuple startt ++
    ", but scan function returns type " ++ prettyTuple foldret ++ "."

typeCheckKernel (WriteKernel cs w lam _ivs as) = do
  -- Requirements:
  --
  --   0. @lambdaReturnType@ of @lam@ must be a list
  --      [index types..., value types].
  --
  --   1. The number of index types must be equal to the number of value types
  --      and the number of arrays in @as@.
  --
  --   2. Each index type must have the type i32.
  --
  --   3. Each array pair in @as@ and the value types must have the same type
  --      (though not necessarily the same length).
  --
  --   4. Each array in @as@ is consumed.  This is not really a check, but more
  --      of a requirement, so that e.g. the source is not hoisted out of a
  --      loop, which will mean it cannot be consumed.
  --
  -- Code:

  -- First check the certificates and input size.
  mapM_ (TC.requireI [Prim Cert]) cs
  TC.require [Prim int32] w

  -- 0.
  let rts = lambdaReturnType lam
      rtsLen = length rts `div` 2
      rtsI = take rtsLen rts
      rtsV = drop rtsLen rts

  -- 1.
  unless (rtsLen == length as)
    $ TC.bad $ TC.TypeError "Write: Uneven number of index types, value types, and I/O arrays."

  -- 2.
  forM_ rtsI $ \rtI -> unless (Prim int32 == rtI)
                       $ TC.bad $ TC.TypeError "Write: Index return type must be i32."

  forM_ (zip rtsV as) $ \(rtV, (aw, a)) -> do
    -- All lengths must have type i32.
    TC.require [Prim int32] aw

    -- 3.
    aType <- lookupType a
    case (rtV, rowType aType) of
      (Prim pt0, Prim pt1) | pt0 == pt1 ->
        return ()
      (Array pt0 _ _, Array pt1 _ _) | pt0 == pt1 ->
        return ()
      _ ->
        TC.bad $ TC.TypeError
        "Write values and input arrays do not have the same primitive type"

    -- 4.
    TC.consume =<< TC.lookupAliases a

typeCheckKernel NumGroups = return ()
typeCheckKernel GroupSize = return ()

typeCheckKernel (Kernel cs (groups, group_size, num_threads) ts thread_id (KernelBody stms res)) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  mapM_ (TC.require [Prim int32]) [groups, group_size, num_threads]
  mapM_ TC.checkType ts
  TC.binding (HM.singleton thread_id IndexInfo) $ checkKernelStms stms $
    zipWithM_ checkKernelResult res ts
  where checkKernelResult (AllThreadsReturn what) t =
          TC.require [t] what
        checkKernelResult (ThisThreadReturns who what) t = do
          TC.require [Prim int32] who
          TC.require [t] what
        checkKernelResult (ConcatReturns _ w per_thread_elems v) t = do
          TC.require [Prim int32] w
          TC.require [Prim int32] per_thread_elems
          vt <- lookupType v
          unless (vt == t `arrayOfRow` arraySize 0 vt) $
            TC.bad $ TC.TypeError $ "Invalid type for ConcatReturns " ++ pretty v

        checkKernelStms [] m = m
        checkKernelStms (stm:stms') m = do
          checkKernelStm stm
          TC.binding (scopeOf stm) $ checkKernelStms stms' m

        checkKernelStm (Thread pes body) = do
          TC.checkBody body
          body_ts <- bodyExtType body
          let pes_ts = staticShapes $ map patElemType pes
          unless (body_ts `subtypesOf` pes_ts) $
            TC.bad $ TC.TypeError $ "Kernel thread statement returns type " ++
            prettyTuple body_ts ++ ", but pattern has type " ++
            prettyTuple pes_ts
        checkKernelStm (SplitArray (size, chunks) _ w elems_per_thread arrs) = do
          TC.require [Prim int32] elems_per_thread
          TC.require [Prim int32] w
          arrts <- map TC.argType <$> TC.checkSOACArrayArgs w arrs
          forM_ (zip arrts chunks) $ \(arrt, chunk) -> do
            let chunk_t = arrt `arrayOfRow` Var size
            unless (chunk_t == patElemType chunk) $
              TC.bad $ TC.TypeError "Invalid type annotation for splitArray chunk."
        checkKernelStm (Combine pe arr) = do
          TC.require [rowType $ patElemType pe] arr
          unless (arraySize 0 (patElemType pe) == group_size) $
            TC.bad $ TC.TypeError $ "Outer size of " ++ pretty pe
            ++ " must be " ++ pretty group_size
        checkKernelStm (GroupReduce pes w lam input) = do
          TC.require [Prim int32] w
          let (nes, arrs) = unzip input
              asArg t = (t, mempty)
          neargs <- mapM TC.checkArg nes
          arrargs <- TC.checkSOACArrayArgs w arrs
          TC.checkLambda lam $
            map asArg [Prim int32, Prim int32] ++
            map TC.noArgAliases (neargs ++ arrargs)
          unless (lambdaReturnType lam == map patElemType pes) $
            TC.bad $ TC.TypeError "Invalid type annotation for kernel reduction."

checkKernelCrud :: TC.Checkable lore =>
                   [VName] -> SubExp -> KernelSize -> TC.TypeM lore ()
checkKernelCrud cs w kernel_size = do
  mapM_ (TC.requireI [Prim Cert]) cs
  TC.require [Prim int32] w
  typeCheckKernelSize kernel_size

typeCheckKernelSize :: TC.Checkable lore =>
                       KernelSize -> TC.TypeM lore ()
typeCheckKernelSize (KernelSize num_groups workgroup_size per_thread_elements
                     num_elements offset_multiple num_threads) = do
  TC.require [Prim int32] num_groups
  TC.require [Prim int32] workgroup_size
  TC.require [Prim int32] per_thread_elements
  TC.require [Prim int32] num_elements
  TC.require [Prim int32] offset_multiple
  TC.require [Prim int32] num_threads

lamParamsToNameInfos :: [LParam lore]
                     -> [(VName, NameInfo lore)]
lamParamsToNameInfos = map nameTypeAndLore
  where nameTypeAndLore fparam = (paramName fparam,
                                  LParamInfo $ paramAttr fparam)

kernelInputsToNameInfos :: [KernelInput lore]
                        -> [(VName, NameInfo lore)]
kernelInputsToNameInfos = map nameTypeAndLore
  where nameTypeAndLore input =
          (kernelInputName input,
           LParamInfo $ paramAttr $ kernelInputParam input)

-- | A kernel input is consumable iff its indexing is a permutation of
-- the full index space.
consumableInputs :: [VName] -> [(KernelInput lore, Names)] -> [(VName, Names)]
consumableInputs is = map (first kernelInputName) .
                      filter ((==is_sorted) . sort . kernelInputIndices . fst)
  where is_sorted = sort (map Var is)

instance OpMetrics (Op lore) => OpMetrics (Kernel lore) where
  opMetrics (MapKernel _ _ _ _ _ _ body) =
    inside "MapKernel" $ bodyMetrics body
  opMetrics (ScanKernel _ _ _ lam foldfun _ _) =
    inside "ScanKernel" $ lambdaMetrics lam >> lambdaMetrics foldfun
  opMetrics (WriteKernel _cs _len lam _ivs _as) =
    inside "WriteKernel" $ lambdaMetrics lam
  opMetrics (Kernel _ _ _ _ kbody) =
    inside "Kernel" $ kernelBodyMetrics kbody
    where kernelBodyMetrics = mapM_ kernelStmMetrics . kernelBodyStms
          kernelStmMetrics SplitArray{} =
            seen "SplitArray"
          kernelStmMetrics (Thread _ body) =
            inside "Thread" $ bodyMetrics body
          kernelStmMetrics Combine{} =
            seen "Combine"
          kernelStmMetrics (GroupReduce _ _ lam _) =
            inside "GroupReduce" $ lambdaMetrics lam
  opMetrics NumGroups = seen "NumGroups"
  opMetrics GroupSize = seen "GroupSize"

instance PrettyLore lore => PP.Pretty (Kernel lore) where
  ppr (MapKernel cs w index ispace inps returns body) =
    ppCertificates' cs <> text "mapKernel" <+>
    PP.align (parens (text "width:" <+> ppr w) </>
           parens (text "index:" <+> ppr index) </>
           parens (PP.stack $ PP.punctuate PP.semi $ map ppBound ispace) </>
           parens (PP.stack $ PP.punctuate PP.semi $ map ppr inps) </>
           parens (PP.stack $ PP.punctuate PP.semi $ map ppRet returns) </>
           text "do") </>
    PP.indent 2 (ppr body)
    where ppBound (name, bound) =
            ppr name <+> text "<" <+> ppr bound
          ppRet (t, perm) =
            ppr t <+> text "permuted" <+> PP.apply (map ppr perm)
  ppr (ScanKernel cs w kernel_size fun foldfun nes arrs) =
    ppCertificates' cs <> text "scanKernel" <>
    parens (ppr w <> comma </>
            ppr kernel_size <> comma </>
            PP.braces (commasep $ map ppr nes) <> comma </>
            commasep (map ppr arrs) <> comma </>
            ppr fun <> comma </> ppr foldfun)
  ppr (WriteKernel cs len lam ivs as) =
    ppCertificates' cs <> text "writeKernel" <>
    parens (ppr len <> comma </>
            commasep (map ppr ivs) <> comma </>
            commasep (map ppr as) <> comma </>
            ppr lam)
  ppr NumGroups = text "$num_groups()"
  ppr GroupSize = text "$group_size()"

  ppr (Kernel cs (num_groups,group_size,num_threads) ts thread_id body) =
    ppCertificates' cs <>
    text "kernel" <>
    parens (commasep [text "num_groups:" <+> ppr num_groups,
                      text "group_size:" <+> ppr group_size,
                      text "num_threads:" <+> ppr num_threads,
                      text "thread id ->" <+> ppr thread_id]) <+>
    PP.colon <+> ppTuple' ts <+> text "{" </>
    PP.indent 2 (ppr body) </>
    text "}"

instance PrettyLore lore => Pretty (KernelBody lore) where
  ppr (KernelBody stms res) =
    PP.stack (map ppr stms) </>
    text "return" <+> PP.braces (PP.commasep $ map ppr res)

instance PrettyLore lore => Pretty (KernelStm lore) where
  ppr (SplitArray (n,chunks) o w elems_per_thread arrs) =
    PP.annot (mapMaybe ppAnnot chunks) $
    text "let" <+> parens (commasep $ ppr n : map ppr chunks) <+> PP.equals <+>
    text ("splitArray" <> suff) <> parens (commasep $ ppr w : ppr elems_per_thread : map ppr arrs)
    where suff = case o of InOrder -> ""
                           Disorder -> "Unordered"
  ppr (Thread pes body) =
    PP.annot (mapMaybe ppAnnot pes) $
    text "let" <+> PP.braces (PP.commasep $ map ppr pes) <+> PP.equals <+> text "thread {" </>
    PP.indent 2 (ppr body) </>
    text "}"
  ppr (Combine pe what) =
    PP.annot (mapMaybe ppAnnot [pe]) $
    text "let" <+> PP.braces (ppr pe) <+> PP.equals <+>
    text "combine" <> PP.parens (ppr what)
  ppr (GroupReduce pes w lam input) =
    PP.annot (mapMaybe ppAnnot pes) $
    text "let" <+> PP.braces (PP.commasep $ map ppr pes) <+> PP.equals </>
    PP.indent 2 (text "reduce" <> parens (commasep [ppr w,
                                                     ppr lam,
                                                     PP.braces (commasep $ map ppr nes),
                                                     commasep $ map ppr els]))
    where (nes,els) = unzip input

instance Pretty KernelResult where
  ppr (AllThreadsReturn se) =
    ppr se
  ppr (ThisThreadReturns who what) =
    text "thread" <+> ppr who <+> text "returns" <+> ppr what
  ppr (ConcatReturns o w per_thread_elems v) =
    text "concat" <> suff <>
    parens (commasep [ppr w, ppr per_thread_elems]) <+>
    ppr v
    where suff = case o of InOrder -> ""
                           Disorder -> "Permuted"


instance Pretty KernelSize where
  ppr (KernelSize
       num_chunks workgroup_size per_thread_elements
       num_elements offset_multiple num_threads) =
    PP.braces $ commasep [ppr num_chunks,
                          ppr workgroup_size,
                          ppr per_thread_elements,
                          ppr num_elements,
                          ppr offset_multiple,
                          ppr num_threads
                         ]

instance PrettyLore lore => Pretty (KernelInput lore) where
  ppr inp = ppr (kernelInputType inp) <+>
            ppr (kernelInputName inp) <+>
            text "<-" <+>
            ppr (kernelInputArray inp) <>
            PP.brackets (commasep (map ppr $ kernelInputIndices inp))
