{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.Representation.Kernels.Kernel
       ( Kernel(..)
       , KernelBody(..)
       , KernelSpace(..)
       , spaceDimensions
       , SpaceStructure(..)
       , scopeOfKernelSpace
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
  (Aliases, removeLambdaAliases, removeBodyAliases, removeBindingAliases)
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
  | NumGroups
  | GroupSize
  | TileSize

  | Kernel Certificates
    KernelSpace
    [Type]
    (KernelBody lore)

    deriving (Eq, Show, Ord)

data KernelSpace = KernelSpace { spaceGlobalId :: VName
                               , spaceLocalId :: VName
                               , spaceGroupId :: VName
                               , spaceNumThreads :: SubExp
                               , spaceNumGroups :: SubExp
                               , spaceGroupSize :: SubExp -- flat group size
                               , spaceStructure :: SpaceStructure
                               }
                 deriving (Eq, Show, Ord)

data SpaceStructure = FlatSpace
                      [(VName, SubExp)] -- gtids and dim sizes
                    | NestedSpace
                      [(VName, -- gtid
                        SubExp, -- global dim size
                        VName, -- ltid
                        SubExp -- local dim sizes
                       )]
                    deriving (Eq, Show, Ord)

-- | Global thread IDs and their upper bound.
spaceDimensions :: KernelSpace -> [(VName, SubExp)]
spaceDimensions = structureDimensions . spaceStructure
  where structureDimensions (FlatSpace dims) = dims
        structureDimensions (NestedSpace dims) =
          let (gtids, gdim_sizes, _, _) = unzip4 dims
          in zip gtids gdim_sizes

-- | The body of a 'Kernel'.
data KernelBody lore = KernelBody { kernelBodyLore :: BodyAttr lore
                                  , kernelBodyStms :: [Binding lore]
                                  , kernelBodyResult :: [KernelResult]
                                  }

deriving instance Annotations lore => Ord (KernelBody lore)
deriving instance Annotations lore => Show (KernelBody lore)
deriving instance Annotations lore => Eq (KernelBody lore)

data KernelResult = ThreadsReturn WhichThreads SubExp
                  | WriteReturn
                    SubExp -- Size of array
                    VName -- Which array
                    SubExp -- The index
                    SubExp -- The value
                  | ConcatReturns
                    StreamOrd -- Permuted?
                    SubExp -- The final size.
                    SubExp -- Per-thread (max) chunk size.
                    VName -- Chunk by this thread.
                  deriving (Eq, Show, Ord)

data WhichThreads = AllThreads
                  | OneThreadPerGroup SubExp -- Which one.
                  | ThreadsPerGroup [(VName,SubExp)] -- All threads before this one.
                  | ThreadsInSpace
                  deriving (Eq, Show, Ord)

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
mapKernelM _ NumGroups = pure NumGroups
mapKernelM _ GroupSize = pure GroupSize
mapKernelM _ TileSize = pure TileSize
mapKernelM tv (Kernel cs space ts kernel_body) =
  Kernel <$> mapOnKernelCertificates tv cs <*>
  mapOnKernelSpace space <*>
  mapM (mapOnKernelType tv) ts <*>
  mapOnKernelKernelBody tv kernel_body
  where mapOnKernelSpace (KernelSpace gtid ltid gid num_threads num_groups group_size structure) =
          KernelSpace gtid ltid gid -- all in binding position
          <$> mapOnKernelSubExp tv num_threads
          <*> mapOnKernelSubExp tv num_groups
          <*> mapOnKernelSubExp tv group_size
          <*> mapOnKernelStructure structure
        mapOnKernelStructure (FlatSpace dims) =
          FlatSpace <$> (zip gtids <$> mapM (mapOnKernelSubExp tv) gdim_sizes)
          where (gtids, gdim_sizes) = unzip dims
        mapOnKernelStructure (NestedSpace dims) =
            NestedSpace <$> (zip4 gtids
                             <$> mapM (mapOnKernelSubExp tv) gdim_sizes
                             <*> pure ltids
                             <*> mapM (mapOnKernelSubExp tv) ldim_sizes)
          where (gtids, gdim_sizes, ltids, ldim_sizes) = unzip4 dims

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
  freeIn (WriteReturn rw arr i e) = freeIn rw <> freeIn arr <> freeIn i <> freeIn e
  freeIn (ConcatReturns _ w per_thread_elems v) =
    freeIn w <> freeIn per_thread_elems <> freeIn v

instance FreeIn WhichThreads where
  freeIn AllThreads = mempty
  freeIn (OneThreadPerGroup which) = freeIn which
  freeIn (ThreadsPerGroup limit) = freeIn limit
  freeIn ThreadsInSpace = mempty

instance Attributes lore => FreeIn (KernelBody lore) where
  freeIn (KernelBody attr stms res) =
    (freeIn attr <> free_in_stms <> free_in_res) `HS.difference` bound_in_stms
    where free_in_stms = mconcat $ map freeInBinding stms
          free_in_res = freeIn res
          bound_in_stms = mconcat $ map boundByBinding stms

instance Attributes lore => Substitute (KernelBody lore) where
  substituteNames subst (KernelBody attr stms res) =
    KernelBody
    (substituteNames subst attr)
    (substituteNames subst stms)
    (substituteNames subst res)

instance Substitute KernelResult where
  substituteNames subst (ThreadsReturn who se) =
    ThreadsReturn (substituteNames subst who) (substituteNames subst se)
  substituteNames subst (WriteReturn rw arr i e) =
    WriteReturn
    (substituteNames subst rw) (substituteNames subst arr)
    (substituteNames subst i) (substituteNames subst e)
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

instance Substitute KernelSpace where
  substituteNames subst (KernelSpace gtid ltid gid num_threads num_groups group_size structure) =
    KernelSpace (substituteNames subst gtid)
    (substituteNames subst ltid)
    (substituteNames subst gid)
    (substituteNames subst num_threads)
    (substituteNames subst num_groups)
    (substituteNames subst group_size)
    (substituteNames subst structure)

instance Substitute SpaceStructure where
  substituteNames subst (FlatSpace dims) =
    FlatSpace (map (substituteNames subst) dims)
  substituteNames subst (NestedSpace dims) =
    NestedSpace (map (substituteNames subst) dims)

instance Attributes lore => Substitute (Kernel lore) where
  substituteNames subst (Kernel cs space ts kbody) =
    Kernel
    (substituteNames subst cs)
    (substituteNames subst space)
    (substituteNames subst ts)
    (substituteNames subst kbody)
  substituteNames subst k = runIdentity $ mapKernelM substitute k
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
  rename (KernelBody attr [] res) =
    KernelBody <$> rename attr <*> pure [] <*> rename res
  rename (KernelBody attr (stm:stms) res) =
    bindingForRename (HS.toList $ boundByBinding stm) $ do
      stm' <- rename stm
      KernelBody attr' stms' res' <- rename $ KernelBody attr stms res
      return $ KernelBody attr' (stm':stms') res'

instance Rename KernelResult where
  rename = substituteRename

instance Rename WhichThreads where
  rename = substituteRename

scopeOfKernelSpace :: KernelSpace -> Scope lore
scopeOfKernelSpace (KernelSpace gtid ltid gid _ _ _ structure) =
  HM.fromList $ zip ([gtid, ltid, gid] ++ structure') $ repeat IndexInfo
  where structure' = case structure of
                       FlatSpace dims -> map fst dims
                       NestedSpace dims ->
                         let (gtids, _, ltids, _) = unzip4 dims
                         in gtids ++ ltids

instance Attributes lore => Rename (Kernel lore) where
  rename = mapKernelM renamer
    where renamer = KernelMapper rename rename rename rename rename rename rename

kernelType :: Kernel lore -> [Type]
kernelType (ScanKernel _ w size lam foldlam nes _) =
  let arr_row_tp = drop (length nes) $ lambdaReturnType foldlam
  in map (`arrayOfRow` w) (lambdaReturnType lam) ++
     map (`arrayOfRow` kernelWorkgroups size) (lambdaReturnType lam) ++
     map (`arrayOfRow` kernelTotalElements size) arr_row_tp
kernelType (Kernel _ space ts body) =
  zipWith resultShape ts $ kernelBodyResult body
  where dims = map snd $ spaceDimensions space
        num_groups = spaceNumGroups space
        num_threads = spaceNumThreads space
        resultShape t (WriteReturn rw _ _ _) =
          t `arrayOfRow` rw
        resultShape t (ThreadsReturn AllThreads _) =
          t `arrayOfRow` num_threads
        resultShape t (ThreadsReturn OneThreadPerGroup{} _) =
          t `arrayOfRow` num_groups
        resultShape t (ThreadsReturn (ThreadsPerGroup limit) _) =
          t `arrayOfShape` Shape (map snd limit) `arrayOfRow` num_groups
        resultShape t (ThreadsReturn ThreadsInSpace _) =
          foldr (flip arrayOfRow) t dims
        resultShape t (ConcatReturns _ w _ _) =
          t `arrayOfRow` w

kernelType NumGroups =
  [Prim int32]
kernelType GroupSize =
  [Prim int32]
kernelType TileSize =
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

  consumedInOp (Kernel _ _ _ kbody) =
    consumedInKernelBody kbody <>
    mconcat (map consumedByReturn (kernelBodyResult kbody))
    where consumedByReturn (WriteReturn _ a _ _) = HS.singleton a
          consumedByReturn _                     = mempty
  consumedInOp _ = mempty

aliasAnalyseKernelBody :: (Attributes lore,
                           Attributes (Aliases lore),
                           CanBeAliased (Op lore)) =>
                          KernelBody lore
                       -> KernelBody (Aliases lore)
aliasAnalyseKernelBody (KernelBody attr stms res) =
  let Body attr' stms' _ = Alias.analyseBody $ Body attr stms []
  in KernelBody attr' stms' res

instance (Attributes lore,
          Attributes (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (Kernel lore) where
  type OpWithAliases (Kernel lore) = Kernel (Aliases lore)

  addOpAliases = runIdentity . mapKernelM alias
    where alias = KernelMapper return (return . Alias.analyseLambda)
                  (return . Alias.analyseBody) return return return
                  (return . aliasAnalyseKernelBody)

  removeOpAliases = runIdentity . mapKernelM remove
    where remove = KernelMapper return (return . removeLambdaAliases)
                   (return . removeBodyAliases) return return return
                   (return . removeKernelBodyAliases)
          removeKernelBodyAliases :: KernelBody (Aliases lore)
                                  -> KernelBody lore
          removeKernelBodyAliases (KernelBody (_, attr) stms res) =
            KernelBody attr (map removeBindingAliases stms) res

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
          removeKernelBodyRanges = error "removeKernelBodyRanges"
  addOpRanges = Range.runRangeM . mapKernelM add
    where add = KernelMapper return Range.analyseLambda
                Range.analyseBody return return return addKernelBodyRanges
          addKernelBodyRanges = error "addKernelBodyRanges"

instance (Attributes lore, CanBeWise (Op lore)) => CanBeWise (Kernel lore) where
  type OpWithWisdom (Kernel lore) = Kernel (Wise lore)

  removeOpWisdom = runIdentity . mapKernelM remove
    where remove = KernelMapper return
                   (return . removeLambdaWisdom)
                   (return . removeBodyWisdom)
                   return return return
                   (return . removeKernelBodyWisdom)
          removeKernelBodyWisdom :: KernelBody (Wise lore)
                                 -> KernelBody lore
          removeKernelBodyWisdom (KernelBody attr stms res) =
            let Body attr' stms' _ = removeBodyWisdom $ Body attr stms []
            in KernelBody attr' stms' res

instance (Attributes lore, Aliased lore, UsageInOp (Op lore)) => UsageInOp (Kernel lore) where
  usageInOp (ScanKernel _ _ _ _ foldfun _ arrs) =
    usageInLambda foldfun arrs
  usageInOp (Kernel _ _ _ kbody) =
    mconcat $ map UT.consumedUsage $ HS.toList $ consumedInKernelBody kbody
  usageInOp NumGroups = mempty
  usageInOp GroupSize = mempty
  usageInOp TileSize = mempty

consumedInKernelBody :: (Attributes lore, Aliased lore) =>
                        KernelBody lore -> Names
consumedInKernelBody (KernelBody attr stms _) =
  consumedInBody $ Body attr stms []

typeCheckKernel :: TC.Checkable lore => Kernel (Aliases lore) -> TC.TypeM lore ()

typeCheckKernel (ScanKernel cs w kernel_size fun foldfun nes arrs) = do
  checkKernelCrud cs w kernel_size

  let index_arg = (Prim int32, mempty)
  arrargs <- TC.checkSOACArrayArgs w arrs
  accargs <- mapM (fmap TC.noArgAliases . TC.checkArg) nes
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

typeCheckKernel NumGroups = return ()
typeCheckKernel GroupSize = return ()
typeCheckKernel TileSize = return ()

typeCheckKernel (Kernel cs space kts kbody) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  checkSpace space
  mapM_ TC.checkType kts
  mapM_ (TC.require [Prim int32] . snd) $ spaceDimensions space

  TC.binding (scopeOfKernelSpace space) $
    checkKernelBody kts kbody
  where checkSpace (KernelSpace _ _ _ num_threads num_groups group_size structure) = do
          mapM_ (TC.require [Prim int32]) [num_threads,num_groups,group_size]
          case structure of
            FlatSpace dims ->
              mapM_ (TC.require [Prim int32] . snd) dims
            NestedSpace dims ->
              let (_, gdim_sizes, _, ldim_sizes) = unzip4 dims
              in mapM_ (TC.require [Prim int32]) $ gdim_sizes ++ ldim_sizes

        checkKernelBody ts (KernelBody (_, attr) stms res) = do
          TC.checkBodyLore attr
          TC.checkBindings stms $
            zipWithM_ checkKernelResult res ts

        checkKernelResult (ThreadsReturn which what) t = do
          checkWhich which
          TC.require [t] what
        checkKernelResult (WriteReturn rw arr i e) t = do
          TC.require [Prim int32] rw
          TC.require [Prim int32] i
          TC.require [t] e
          arr_t <- lookupType arr
          unless (arr_t == t `arrayOfRow` rw) $
            TC.bad $ TC.TypeError "Invalid type of array destination for WriteReturn."
          TC.consume =<< TC.lookupAliases arr
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
        checkWhich (ThreadsPerGroup limit) = do
          mapM_ (TC.requireI [Prim int32] . fst) limit
          mapM_ (TC.require [Prim int32] . snd) limit
        checkWhich ThreadsInSpace =
          return ()

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
  opMetrics (Kernel _ _ _ kbody) =
    inside "Kernel" $ kernelBodyMetrics kbody
    where kernelBodyMetrics :: KernelBody lore -> MetricsM ()
          kernelBodyMetrics = mapM_ bindingMetrics . kernelBodyStms
  opMetrics NumGroups = seen "NumGroups"
  opMetrics GroupSize = seen "GroupSize"
  opMetrics TileSize = seen "TileSize"

instance PrettyLore lore => PP.Pretty (Kernel lore) where
  ppr (ScanKernel cs w kernel_size fun foldfun nes arrs) =
    ppCertificates' cs <> text "scanKernel" <>
    parens (ppr w <> comma </>
            ppr kernel_size <> comma </>
            PP.braces (commasep $ map ppr nes) <> comma </>
            commasep (map ppr arrs) <> comma </>
            ppr fun <> comma </> ppr foldfun)
  ppr NumGroups = text "$num_groups()"
  ppr GroupSize = text "$group_size()"
  ppr TileSize = text "$tile_size()"

  ppr (Kernel cs space ts body) =
    ppCertificates' cs <>
    text "kernel" <>
    PP.align (ppr space) <+>
    PP.colon <+> ppTuple' ts <+> text "{" </>
    PP.indent 2 (ppr body) </>
    text "}"

instance Pretty KernelSpace where
  ppr (KernelSpace f_gtid f_ltid gid num_threads num_groups group_size structure) =
    parens (commasep [text "num groups:" <+> ppr num_groups,
                      text "group size:" <+> ppr group_size,
                      text "num threads:" <+> ppr num_threads,
                      text "global TID ->" <+> ppr f_gtid,
                      text "local TID ->" <+> ppr f_ltid,
                      text "group ID ->" <+> ppr gid]) </> structure'
    where structure' =
            case structure of
              FlatSpace space ->
                parens (commasep $ do
                           (i,d) <- space
                           return $ ppr i <+> "<" <+> ppr d)
              NestedSpace space ->
                parens (commasep $ do
                           (gtid,gd,ltid,ld) <- space
                           return $ ppr (gtid,ltid) <+> "<" <+> ppr (gd,ld))

instance PrettyLore lore => Pretty (KernelBody lore) where
  ppr (KernelBody _ stms res) =
    PP.stack (map ppr stms) </>
    text "return" <+> PP.braces (PP.commasep $ map ppr res)

instance Pretty KernelResult where
  ppr (ThreadsReturn AllThreads what) =
    ppr what
  ppr (ThreadsReturn (OneThreadPerGroup who) what) =
    text "thread" <+> ppr who <+> text "returns" <+> ppr what
  ppr (ThreadsReturn (ThreadsPerGroup limit) what) =
    text "thread <" <+> ppr limit <+> text "returns" <+> ppr what
  ppr (ThreadsReturn ThreadsInSpace what) =
    text "thread in space returns" <+> ppr what
  ppr (WriteReturn rw arr i e) =
    ppr arr <+> text "with" <+>
    PP.brackets (ppr i <+> text "<" <+> ppr rw) <+> text "<-" <+> ppr e
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
