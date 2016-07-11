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
       , KernelSpace(..)
       , scopeOfKernelSpace
       , GroupStreamLambda(..)
       , WhichThreads(..)
       , KernelResult(..)

       , KernelSize(..)
       , chunkedKernelNonconcatOutputs

       , typeCheckKernel

         -- * Generic traversal
       , KernelMapper(..)
       , identityKernelMapper
       , mapKernelM
       )
       where

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
    ScanKernel Certificates SubExp
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
    KernelSpace -- thread ID (binding position)
    (KernelBody lore)

    deriving (Eq, Show, Ord)

data KernelSpace = KernelSpace { spaceGlobalId :: VName
                               , spaceLocalId :: VName
                               , spaceGroupId :: VName
                               , spaceDimensions :: [(VName, SubExp)]
                               }
                 deriving (Eq, Show, Ord)

data KernelBody lore = KernelBody { kernelBodyStms :: [KernelStm lore]
                                  , kernelBodyResult :: [KernelResult]
                                  }
                deriving (Eq, Show, Ord)

data KernelResult = ThreadsReturn WhichThreads SubExp
                  | ConcatReturns
                    StreamOrd -- Permuted?
                    SubExp -- The final size.
                    SubExp -- Per-thread (max) chunk size.
                    VName -- Chunk by this thread.
                  deriving (Eq, Show, Ord)

data WhichThreads = AllThreads
                  | OneThreadPerGroup SubExp -- Which one.
                  | ThreadsPerGroup SubExp -- All threads before this one.
                  | ThreadsInSpace
                  deriving (Eq, Show, Ord)

data KernelStm lore = SplitArray (VName, [PatElem (LetAttr lore)]) StreamOrd SubExp SubExp [VName]
                    | Thread [PatElem (LetAttr lore)] WhichThreads (Body lore)
                    | Combine (PatElem (LetAttr lore)) SubExp SubExp
                    | GroupReduce [PatElem (LetAttr lore)] SubExp
                      (Lambda lore) [(SubExp,VName)]
                    | GroupStream [PatElem (LetAttr lore)] SubExp
                      (GroupStreamLambda lore) [SubExp] [VName]

deriving instance Annotations lore => Eq (KernelStm lore)
deriving instance Annotations lore => Show (KernelStm lore)
deriving instance Annotations lore => Ord (KernelStm lore)

boundByKernelStm :: KernelStm lore -> Names
boundByKernelStm = HS.fromList . HM.keys . scopeOf

data GroupStreamLambda lore = GroupStreamLambda
  { groupStreamChunkSize :: VName
  , groupStreamChunkOffset :: VName
  , groupStreamAccParams :: [LParam lore]
  , groupStreamArrParams :: [LParam lore]
  , groupStreamLambdaBody :: KernelBody lore
  }

deriving instance Annotations lore => Eq (GroupStreamLambda lore)
deriving instance Annotations lore => Show (GroupStreamLambda lore)
deriving instance Annotations lore => Ord (GroupStreamLambda lore)

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

instance FreeIn KernelSize where
  freeIn (KernelSize num_workgroups workgroup_size elems_per_thread
          num_elems thread_offset num_threads) =
    mconcat $ map freeIn [num_workgroups,
                          workgroup_size,
                          elems_per_thread,
                          num_elems,
                          thread_offset,
                          num_threads]

instance (Attributes lore, FreeIn (LParamAttr lore)) =>
         FreeIn (Kernel lore) where
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

instance FreeIn KernelResult where
  freeIn (ThreadsReturn which what) = freeIn which <> freeIn what
  freeIn (ConcatReturns _ w per_thread_elems v) =
    freeIn w <> freeIn per_thread_elems <> freeIn v

instance FreeIn WhichThreads where
  freeIn AllThreads = mempty
  freeIn (OneThreadPerGroup which) = freeIn which
  freeIn (ThreadsPerGroup limit) = freeIn limit
  freeIn ThreadsInSpace = mempty

instance Attributes lore => FreeIn (KernelBody lore) where
  freeIn (KernelBody stms res) =
    (free_in_stms <> free_in_res) `HS.difference` bound_in_stms
    where free_in_stms = mconcat $ map freeIn stms
          free_in_res = freeIn res
          bound_in_stms = mconcat $ map boundByKernelStm stms

instance Attributes lore => FreeIn (KernelStm lore) where
  freeIn (SplitArray (n,chunks) _ w elems_per_thread vs) =
    freeIn n <> freeIn chunks <> freeIn w <> freeIn elems_per_thread <> freeIn vs
  freeIn (Thread pes which body) =
    freeIn pes <> freeIn which <> freeInBody body
  freeIn (Combine pe w v) =
    freeIn pe <> freeIn w <> freeIn v
  freeIn (GroupReduce pes w lam input) =
    freeIn pes <> freeIn w <> freeInLambda lam <> freeIn input
  freeIn (GroupStream pes w lam accs arrs) =
    freeIn pes <> freeIn w <> freeIn lam <> freeIn accs <> freeIn arrs

instance Attributes lore => FreeIn (GroupStreamLambda lore) where
  freeIn (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
    freeIn body `HS.difference` bound_here
    where bound_here = HS.fromList $
                       chunk_offset : chunk_size :
                       map paramName (acc_params ++ arr_params)

instance Attributes lore => Substitute (KernelBody lore) where
  substituteNames subst (KernelBody stms res) =
    KernelBody (substituteNames subst stms) $ substituteNames subst res

instance Substitute KernelResult where
  substituteNames subst (ThreadsReturn who se) =
    ThreadsReturn (substituteNames subst who) (substituteNames subst se)
  substituteNames subst (ConcatReturns ord w per_thread_elems v) =
    ConcatReturns
    ord
    (substituteNames subst w)
    (substituteNames subst per_thread_elems)
    (substituteNames subst v)

instance Substitute WhichThreads where
  substituteNames _ AllThreads =
    AllThreads
  substituteNames subst (OneThreadPerGroup which) =
    OneThreadPerGroup $ substituteNames subst which
  substituteNames subst (ThreadsPerGroup limit) =
    ThreadsPerGroup $ substituteNames subst limit
  substituteNames _ ThreadsInSpace =
    ThreadsInSpace

instance Attributes lore => Substitute (KernelStm lore) where
  substituteNames subst (SplitArray (n,arrs) o w elems_per_thread vs) =
    SplitArray (n,arrs) o
    (substituteNames subst w)
    (substituteNames subst elems_per_thread)
    (substituteNames subst vs)
  substituteNames subst (Thread pes which body) =
    Thread
    (substituteNames subst pes)
    (substituteNames subst which)
    (substituteNames subst body)
  substituteNames subst (Combine pe w v) =
    Combine (substituteNames subst pe)
    (substituteNames subst w) (substituteNames subst v)
  substituteNames subst (GroupReduce pes w lam input) =
    GroupReduce (substituteNames subst pes) (substituteNames subst w)
    (substituteNames subst lam) (substituteNames subst input)
  substituteNames subst (GroupStream pes w lam accs arrs) =
    GroupStream (substituteNames subst pes) (substituteNames subst w)
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

instance Attributes lore => Rename (KernelBody lore) where
  rename (KernelBody [] res) =
    KernelBody [] <$> rename res
  rename (KernelBody (stm:stms) res) =
    bindingForRename (HS.toList $ boundByKernelStm stm) $ do
      stm' <- rename stm
      KernelBody stms' res' <- rename $ KernelBody stms res
      return $ KernelBody (stm':stms') res'

instance (Attributes lore, Renameable lore) => Rename (KernelStm lore) where
  rename (SplitArray (n,chunks) o w elems_per_thread vs) =
    SplitArray <$> ((,) <$> rename n <*> rename chunks)
    <*> pure o
    <*> rename w
    <*> rename elems_per_thread
    <*> rename vs
  rename (GroupReduce pes w lam input) =
    GroupReduce <$> rename pes <*> rename w <*> rename lam <*> rename input
  rename (Combine pe w v) =
    Combine <$> rename pe <*> rename w <*> rename v
  rename (Thread pes which body) =
    Thread <$> rename pes <*> rename which <*> rename body
  rename (GroupStream pes w lam accs arrs) =
    GroupStream <$> rename pes <*> rename w <*>
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

instance Rename KernelResult where
  rename = substituteRename

instance Rename WhichThreads where
  rename = substituteRename

scopeOfKernelSpace :: KernelSpace -> Scope lore
scopeOfKernelSpace (KernelSpace global_tid local_tid group_id space) =
  HM.fromList $
  zip (global_tid : local_tid : group_id : map fst space) $
  repeat IndexInfo

instance LParamAttr lore1 ~ LParamAttr lore2 =>
         Scoped lore1 (GroupStreamLambda lore2) where
  scopeOf (GroupStreamLambda chunk_size chunk_offset acc_params arr_params _) =
    HM.insert chunk_size IndexInfo $
    HM.insert chunk_offset IndexInfo $
    scopeOfLParams (acc_params ++ arr_params)

instance Scoped lore (KernelStm lore) where
  scopeOf (SplitArray (size, chunks) _ _ _ _) =
    mconcat (map scopeOf chunks) <>
    HM.singleton size IndexInfo
  scopeOf (Thread pes _ _) =
    mconcat $ map scopeOf pes
  scopeOf (Combine pe _ _) = scopeOf pe
  scopeOf (GroupReduce pes _ _ _) =
    mconcat $ map scopeOf pes
  scopeOf (GroupStream pes _ _ _ _) =
    mconcat $ map scopeOf pes

instance Attributes lore => Rename (Kernel lore) where
  rename = mapKernelM renamer
    where renamer = KernelMapper rename rename rename rename rename rename rename

kernelType :: Kernel lore -> [Type]
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
kernelType (Kernel _ (num_groups, _, num_threads) ts space body) =
  zipWith resultShape ts $ kernelBodyResult body
  where dims = map snd $ spaceDimensions space
        resultShape t (ThreadsReturn AllThreads _) =
          t `arrayOfRow` num_threads
        resultShape t (ThreadsReturn OneThreadPerGroup{} _) =
          t `arrayOfRow` num_groups
        resultShape t (ThreadsReturn (ThreadsPerGroup limit) _) =
          t `arrayOfRow` limit `arrayOfRow` num_groups
        resultShape t (ThreadsReturn ThreadsInSpace _) =
          foldr (flip arrayOfRow) t dims
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
          analyseStm (Thread pes which body) =
            Thread (zipWith annot pes $ bodyAliases body') which body'
            where body' = Alias.analyseBody body
                  annot pe als = (Names' als,) <$> pe
          analyseStm (Combine pe w v) =
            Combine ((mempty,) <$> pe) w v
          analyseStm (GroupReduce pes w lam input) =
            GroupReduce pes' w lam' input
            where pes' = map (fmap (mempty,)) pes
                  lam' = Alias.analyseLambda lam
          analyseStm (GroupStream pes w lam accs arrs) =
            GroupStream pes' w lam' accs arrs
            where pes' = map (fmap (mempty,)) pes
                  lam' = analyseGroupStreamLambda lam

          analyseGroupStreamLambda (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
            GroupStreamLambda chunk_size chunk_offset acc_params arr_params $
            aliasAnalyseKernelBody body

  removeOpAliases = runIdentity . mapKernelM remove
    where remove = KernelMapper return (return . removeLambdaAliases)
                   (return . removeBodyAliases) return return return
                   (return . removeKernelBodyAliases)
          removeKernelBodyAliases (KernelBody stms res) =
            KernelBody (map removeStmAliases stms) res
          removeStmAliases (SplitArray (size, chunks) o w elems_per_thread arrs) =
            SplitArray (size, chunks') o w elems_per_thread arrs
            where chunks' = map (fmap snd) chunks
          removeStmAliases (Thread pes which body) =
            Thread (map (fmap snd) pes) which (removeBodyAliases body)
          removeStmAliases (Combine pe w v) =
            Combine (snd <$> pe) w v
          removeStmAliases (GroupReduce pes w lam input) =
            GroupReduce (map (fmap snd) pes) w (removeLambdaAliases lam) input
          removeStmAliases (GroupStream pes w lam accs arrs) =
            GroupStream (map (fmap snd) pes) w (removeGroupStreamLambdaAliases lam) accs arrs

          removeGroupStreamLambdaAliases (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
            GroupStreamLambda chunk_size chunk_offset acc_params arr_params $
            removeKernelBodyAliases body

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
          removeKernelStatementWisdom (Thread pes which body) =
            Thread (map removePatElemWisdom pes) which (removeBodyWisdom body)
          removeKernelStatementWisdom (Combine pe w v) =
            Combine (removePatElemWisdom pe) w v
          removeKernelStatementWisdom (SplitArray (size,chunks) o w elems_per_thread arrs) =
            SplitArray (size, map removePatElemWisdom chunks) o w elems_per_thread arrs
          removeKernelStatementWisdom (GroupReduce pes w lam input) =
            GroupReduce (map removePatElemWisdom pes) w (removeLambdaWisdom lam) input
          removeKernelStatementWisdom (GroupStream pes w lam accs arrs) =
            GroupStream (map removePatElemWisdom pes) w (removeGroupStreamLambdaWisdom lam) accs arrs

          removeGroupStreamLambdaWisdom (GroupStreamLambda chunk_size chunk_offset acc_params arr_params body) =
            GroupStreamLambda chunk_size chunk_offset acc_params arr_params $
            removeKernelBodyWisdom body

instance (Attributes lore, Aliased lore, UsageInOp (Op lore)) => UsageInOp (Kernel lore) where
  usageInOp (ScanKernel _ _ _ _ foldfun _ arrs) =
    usageInLambda foldfun arrs
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
consumedByKernelStm (Thread _ _ body) = consumedInBody body
consumedByKernelStm Combine{} = mempty
consumedByKernelStm SplitArray{} = mempty
consumedByKernelStm (GroupReduce _ _ _ input) =
  HS.fromList $ map snd input
consumedByKernelStm (GroupStream _ _ lam _ arrs) =
  HS.map consumedArray $ consumedInKernelBody body
  where GroupStreamLambda _ _ _ arr_params body = lam
        consumedArray v = fromMaybe v $ lookup v params_to_arrs
        params_to_arrs = zip (map paramName arr_params) arrs

typeCheckKernel :: TC.Checkable lore => Kernel (Aliases lore) -> TC.TypeM lore ()

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

typeCheckKernel (Kernel cs (groups, group_size, num_threads) kts space kbody) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  mapM_ (TC.require [Prim int32]) [groups, group_size, num_threads]
  mapM_ TC.checkType kts
  mapM_ (TC.require [Prim int32] . snd) $ spaceDimensions space

  TC.binding (scopeOfKernelSpace space) $
    checkKernelBody kts kbody
  where checkKernelBody ts (KernelBody stms res) =
          checkKernelStms stms $ zipWithM_ checkKernelResult res ts

        checkKernelResult (ThreadsReturn which what) t = do
          checkWhich which
          TC.require [t] what
        checkKernelResult (ConcatReturns _ w per_thread_elems v) t = do
          TC.require [Prim int32] w
          TC.require [Prim int32] per_thread_elems
          vt <- lookupType v
          unless (vt == t `arrayOfRow` arraySize 0 vt) $
            TC.bad $ TC.TypeError $ "Invalid type for ConcatReturns " ++ pretty v

        checkWhich AllThreads =
          return ()
        checkWhich (OneThreadPerGroup which) =
          TC.require [Prim int32] which
        checkWhich (ThreadsPerGroup limit) =
          TC.require [Prim int32] limit
        checkWhich ThreadsInSpace =
          return ()

        checkKernelStms [] m = m
        checkKernelStms (stm:stms') m = do
          checkKernelStm stm
          TC.binding (scopeOf stm) $ checkKernelStms stms' m

        checkKernelStm (Thread pes which body) = do
          checkWhich which
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

        checkKernelStm (Combine pe w arr) = do
          TC.require [rowType $ patElemType pe] arr
          unless (arraySize 0 (patElemType pe) == w) $
            TC.bad $ TC.TypeError $ "Outer size of " ++ pretty pe
            ++ " must be " ++ pretty w

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
            TC.bad $ TC.TypeError
            "Invalid type annotation for kernel reduction."

        checkKernelStm (GroupStream pes w lam accs arrs) = do
          TC.require [Prim int32] w

          acc_args <- mapM (fmap TC.noArgAliases . TC.checkArg) accs
          arr_args <- TC.checkSOACArrayArgs w arrs

          checkGroupStreamLambda lam acc_args arr_args
          unless (map TC.argType acc_args == map patElemType pes) $
            TC.bad $ TC.TypeError
            "Invalid type annotations for kernel group stream pattern."

        checkGroupStreamLambda lam@(GroupStreamLambda block_size _ acc_params arr_params body) acc_args arr_args = do
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
            checkKernelBody (map TC.argType acc_args) body

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

instance OpMetrics (Op lore) => OpMetrics (Kernel lore) where
  opMetrics (ScanKernel _ _ _ lam foldfun _ _) =
    inside "ScanKernel" $ lambdaMetrics lam >> lambdaMetrics foldfun
  opMetrics (WriteKernel _cs _len lam _ivs _as) =
    inside "WriteKernel" $ lambdaMetrics lam
  opMetrics (Kernel _ _ _ _ kbody) =
    inside "Kernel" $ kernelBodyMetrics kbody
    where kernelBodyMetrics = mapM_ kernelStmMetrics . kernelBodyStms
          kernelStmMetrics SplitArray{} =
            seen "SplitArray"
          kernelStmMetrics (Thread _ _ body) =
            inside "Thread" $ bodyMetrics body
          kernelStmMetrics Combine{} =
            seen "Combine"
          kernelStmMetrics (GroupReduce _ _ lam _) =
            inside "GroupReduce" $ lambdaMetrics lam
          kernelStmMetrics (GroupStream _ _ lam _ _) =
            inside "GroupStream" $ groupStreamLambdaMetrics lam

          groupStreamLambdaMetrics =
            kernelBodyMetrics . groupStreamLambdaBody
  opMetrics NumGroups = seen "NumGroups"
  opMetrics GroupSize = seen "GroupSize"

instance PrettyLore lore => PP.Pretty (Kernel lore) where
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

  ppr (Kernel cs (num_groups,group_size,num_threads) ts space body) =
    ppCertificates' cs <>
    text "kernel" <>
    PP.align (parens (commasep [text "num_groups:" <+> ppr num_groups,
                                text "group_size:" <+> ppr group_size,
                                text "num_threads:" <+> ppr num_threads]) </>
              ppr space) <+>
    PP.colon <+> ppTuple' ts <+> text "{" </>
    PP.indent 2 (ppr body) </>
    text "}"

instance Pretty KernelSpace where
  ppr (KernelSpace global_tid local_tid group_id space) =
    parens (commasep [text "global TID ->" <+> ppr global_tid,
                      text "local TID ->" <+> ppr local_tid,
                      text "group ID ->" <+> ppr group_id]) </>
    parens (commasep $ do (i,d) <- space
                          return $ ppr i <+> "<" <+> ppr d)

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
  ppr (Thread pes threads body) =
    PP.annot (mapMaybe ppAnnot pes) $
    text "let" <+> PP.braces (PP.commasep $ map ppr pes) <+> PP.equals <+>
    text "thread" <> threads' <+> text "{" </>
    PP.indent 2 (ppr body) </>
    text "}"
    where threads' = case threads of
                       AllThreads -> mempty
                       OneThreadPerGroup which -> mempty <+> ppr which
                       ThreadsPerGroup limit -> text " <" <+> ppr limit
                       ThreadsInSpace -> text " active"
  ppr (Combine pe w what) =
    PP.annot (mapMaybe ppAnnot [pe]) $
    text "let" <+> PP.braces (ppr pe) <+> PP.equals <+>
    text "combine" <> PP.apply [ppr w, ppr what]
  ppr (GroupReduce pes w lam input) =
    PP.annot (mapMaybe ppAnnot pes) $
    text "let" <+> PP.braces (PP.commasep $ map ppr pes) <+> PP.equals </>
    PP.indent 2 (text "reduce" <> parens (commasep [ppr w,
                                                     ppr lam,
                                                     PP.braces (commasep $ map ppr nes),
                                                     commasep $ map ppr els]))
    where (nes,els) = unzip input
  ppr (GroupStream pes w lam accs arrs) =
    PP.annot (mapMaybe ppAnnot pes) $
    text "let" <+> PP.braces (PP.commasep $ map ppr pes) <+> PP.equals </>
    PP.indent 2
    (text "stream" <>
      parens (commasep [ppr w,
                         ppr lam,
                         PP.braces (commasep $ map ppr accs),
                         commasep $ map ppr arrs]))

instance PrettyLore lore => Pretty (GroupStreamLambda lore) where
  ppr (GroupStreamLambda block_size block_offset acc_params arr_params body) =
    PP.annot (mapMaybe ppAnnot params) $
    text "fn" <+>
    parens (commasep (block_size' : block_offset' : map ppr params)) <+>
    text "=>" </> PP.indent 2 (ppr body)
    where params = acc_params ++ arr_params
          block_size' = text "int" <+> ppr block_size
          block_offset' = text "int" <+> ppr block_offset

instance Pretty KernelResult where
  ppr (ThreadsReturn AllThreads what) =
    ppr what
  ppr (ThreadsReturn (OneThreadPerGroup who) what) =
    text "thread" <+> ppr who <+> text "returns" <+> ppr what
  ppr (ThreadsReturn (ThreadsPerGroup limit) what) =
    text "thread <" <+> ppr limit <+> text "returns" <+> ppr what
  ppr (ThreadsReturn ThreadsInSpace what) =
    text "thread in space returns" <+> ppr what
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
