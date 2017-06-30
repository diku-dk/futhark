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
       , KernelDebugHints(..)
       , KernelBody(..)
       , KernelSpace(..)
       , spaceDimensions
       , SpaceStructure(..)
       , scopeOfKernelSpace
       , WhichThreads(..)
       , KernelResult(..)

       , chunkedKernelNonconcatOutputs

       , typeCheckKernel

         -- * Generic traversal
       , KernelMapper(..)
       , identityKernelMapper
       , mapKernelM
       )
       where

import Control.Applicative
import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Identity hiding (mapM_)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
import Data.Foldable (mapM_) -- for stack LTS 1.15

import Prelude hiding (mapM_)

import Futhark.Representation.AST
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.UsageTable as UT
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Util.Pretty as PP
import Futhark.Util.Pretty
  ((</>), (<+>), ppr, commasep, Pretty, parens, text)
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplifier.Lore
import Futhark.Representation.Ranges
  (Ranges, removeLambdaRanges, removeBodyRanges, mkBodyRanges)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases
  (Aliases, removeLambdaAliases, removeBodyAliases, removeStmAliases)
import Futhark.Representation.Kernels.KernelExp (SplitOrdering(..))
import Futhark.Analysis.Usage
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics
import Futhark.Tools (partitionChunkedKernelLambdaParameters)
import qualified Futhark.Analysis.Range as Range

-- | Some information about what goes into a kernel, and where it came
-- from.  Has no semantic meaning; only used for debugging generated
-- code.
data KernelDebugHints =
  KernelDebugHints { kernelName :: String
                   , kernelHints :: [(String, SubExp)]
                     -- ^ A mapping from a description to some
                     -- PrimType value.
                   }
  deriving (Eq, Show, Ord)

data Kernel lore =
    NumGroups
  | GroupSize
  | TileSize
  | SufficientParallelism SubExp -- ^ True if enough parallelism.

  | Kernel KernelDebugHints
    Certificates
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
                               -- TODO: document what this spaceStructure is
                               -- used for
                               }
                 deriving (Eq, Show, Ord)
-- ^ first three bound in the kernel, the rest are params to kernel

-- | Indices computed for each thread (or group) inside the kernel.
-- This is an arbitrary-dimensional space that is generated from the
-- flat GPU thread space.
data SpaceStructure = FlatThreadSpace
                      [(VName, SubExp)] -- gtids and dim sizes
                    | NestedThreadSpace
                      [(VName, -- gtid
                        SubExp, -- global dim size
                        VName, -- ltid
                        SubExp -- local dim sizes
                       )]
                    deriving (Eq, Show, Ord)

-- | Global thread IDs and their upper bound.
spaceDimensions :: KernelSpace -> [(VName, SubExp)]
spaceDimensions = structureDimensions . spaceStructure
  where structureDimensions (FlatThreadSpace dims) = dims
        structureDimensions (NestedThreadSpace dims) =
          let (gtids, gdim_sizes, _, _) = unzip4 dims
          in zip gtids gdim_sizes

-- | The body of a 'Kernel'.
data KernelBody lore = KernelBody { kernelBodyLore :: BodyAttr lore
                                  , kernelBodyStms :: [Stm lore]
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
                    SplitOrdering -- Permuted?
                    SubExp -- The final size.
                    SubExp -- Per-thread (max) chunk size.
                    (Maybe SubExp) -- Optional precalculated offset.
                    VName -- Chunk by this thread.
                  | KernelInPlaceReturn VName -- HACK!
                  deriving (Eq, Show, Ord)

kernelResultSubExp :: KernelResult -> SubExp
kernelResultSubExp (ThreadsReturn _ se) = se
kernelResultSubExp (WriteReturn _ _ _ se) = se
kernelResultSubExp (ConcatReturns _ _ _ _ v) = Var v
kernelResultSubExp (KernelInPlaceReturn v) = Var v

data WhichThreads = AllThreads
                  | OneThreadPerGroup SubExp -- Which one.
                  | ThreadsPerGroup [(VName,SubExp)] -- All threads before this one.
                  | ThreadsInSpace
                  deriving (Eq, Show, Ord)

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
mapKernelM _ NumGroups = pure NumGroups
mapKernelM _ GroupSize = pure GroupSize
mapKernelM _ TileSize = pure TileSize
mapKernelM tv (SufficientParallelism se) =
  SufficientParallelism <$> mapOnKernelSubExp tv se
mapKernelM tv (Kernel desc cs space ts kernel_body) =
  Kernel <$> mapOnKernelDebugHints desc <*>
  mapOnKernelCertificates tv cs <*>
  mapOnKernelSpace space <*>
  mapM (mapOnKernelType tv) ts <*>
  mapOnKernelKernelBody tv kernel_body
  where mapOnKernelDebugHints (KernelDebugHints name kvs) =
          KernelDebugHints name <$>
          (zip (map fst kvs) <$> mapM (mapOnKernelSubExp tv . snd) kvs)
        mapOnKernelSpace (KernelSpace gtid ltid gid num_threads num_groups group_size structure) =
          KernelSpace gtid ltid gid -- all in binding position
          <$> mapOnKernelSubExp tv num_threads
          <*> mapOnKernelSubExp tv num_groups
          <*> mapOnKernelSubExp tv group_size
          <*> mapOnKernelStructure structure
        mapOnKernelStructure (FlatThreadSpace dims) =
          FlatThreadSpace <$> (zip gtids <$> mapM (mapOnKernelSubExp tv) gdim_sizes)
          where (gtids, gdim_sizes) = unzip dims
        mapOnKernelStructure (NestedThreadSpace dims) =
            NestedThreadSpace <$> (zip4 gtids
                             <$> mapM (mapOnKernelSubExp tv) gdim_sizes
                             <*> pure ltids
                             <*> mapM (mapOnKernelSubExp tv) ldim_sizes)
          where (gtids, gdim_sizes, ltids, ldim_sizes) = unzip4 dims

mapOnKernelType :: Monad m =>
                   KernelMapper flore tlore m -> Type -> m Type
mapOnKernelType _tv (Prim pt) = pure $ Prim pt
mapOnKernelType tv (Array pt shape u) = Array pt <$> f shape <*> pure u
  where f (Shape dims) = Shape <$> mapM (mapOnKernelSubExp tv) dims
mapOnKernelType _tv (Mem se s) = pure $ Mem se s

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
  freeIn (ConcatReturns o w per_thread_elems moffset v) =
    freeIn o <> freeIn w <> freeIn per_thread_elems <> freeIn moffset <> freeIn v
  freeIn (KernelInPlaceReturn what) = freeIn what

instance FreeIn WhichThreads where
  freeIn AllThreads = mempty
  freeIn (OneThreadPerGroup which) = freeIn which
  freeIn (ThreadsPerGroup limit) = freeIn limit
  freeIn ThreadsInSpace = mempty

instance Attributes lore => FreeIn (KernelBody lore) where
  freeIn (KernelBody attr stms res) =
    (freeIn attr <> free_in_stms <> free_in_res) `S.difference` bound_in_stms
    where free_in_stms = mconcat $ map freeInStm stms
          free_in_res = freeIn res
          bound_in_stms = mconcat $ map boundByStm stms

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
  substituteNames subst (ConcatReturns o w per_thread_elems moffset v) =
    ConcatReturns
    (substituteNames subst o)
    (substituteNames subst w)
    (substituteNames subst per_thread_elems)
    (substituteNames subst moffset)
    (substituteNames subst v)
  substituteNames subst (KernelInPlaceReturn what) =
    KernelInPlaceReturn (substituteNames subst what)

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
  substituteNames subst (FlatThreadSpace dims) =
    FlatThreadSpace (map (substituteNames subst) dims)
  substituteNames subst (NestedThreadSpace dims) =
    NestedThreadSpace (map (substituteNames subst) dims)

instance Attributes lore => Substitute (Kernel lore) where
  substituteNames subst (Kernel desc cs space ts kbody) =
    Kernel desc
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
    bindingForRename (S.toList $ boundByStm stm) $ do
      stm' <- rename stm
      KernelBody attr' stms' res' <- rename $ KernelBody attr stms res
      return $ KernelBody attr' (stm':stms') res'

instance Rename KernelResult where
  rename = substituteRename

instance Rename WhichThreads where
  rename = substituteRename

scopeOfKernelSpace :: KernelSpace -> Scope lore
scopeOfKernelSpace (KernelSpace gtid ltid gid _ _ _ structure) =
  M.fromList $ zip ([gtid, ltid, gid] ++ structure') $ repeat $ IndexInfo Int32
  where structure' = case structure of
                       FlatThreadSpace dims -> map fst dims
                       NestedThreadSpace dims ->
                         let (gtids, _, ltids, _) = unzip4 dims
                         in gtids ++ ltids

instance Attributes lore => Rename (Kernel lore) where
  rename = mapKernelM renamer
    where renamer = KernelMapper rename rename rename rename rename rename rename

kernelType :: Kernel lore -> [Type]
kernelType (Kernel _ _ space ts body) =
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
        resultShape t (ConcatReturns _ w _ _ _) =
          t `arrayOfRow` w
        resultShape t KernelInPlaceReturn{} =
          t

kernelType NumGroups =
  [Prim int32]
kernelType GroupSize =
  [Prim int32]
kernelType TileSize =
  [Prim int32]
kernelType SufficientParallelism{} =
  [Prim Bool]

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
    consumedInKernelBody kbody <>
    mconcat (map consumedByReturn (kernelBodyResult kbody))
    where consumedByReturn (WriteReturn _ a _ _) = S.singleton a
          consumedByReturn _                     = mempty
  consumedInOp _ = mempty

aliasAnalyseKernelBody :: (Attributes lore,
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
            KernelBody attr (map removeStmAliases stms) res

instance Attributes lore => IsOp (Kernel lore) where
  safeOp _ = True
  cheapOp Kernel{} = False
  cheapOp _ = True

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
          addKernelBodyRanges (KernelBody attr stms res) =
            Range.analyseStms stms $ \stms' ->
            let attr' = (mkBodyRanges stms $ map kernelResultSubExp res, attr)
            in return $ KernelBody attr' stms' res

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

instance ST.IndexOp (Kernel lore) where

instance Aliased lore => UsageInOp (Kernel lore) where
  usageInOp (Kernel _ _ _ _ kbody) =
    mconcat $ map UT.consumedUsage $ S.toList $ consumedInKernelBody kbody
  usageInOp NumGroups = mempty
  usageInOp GroupSize = mempty
  usageInOp TileSize = mempty
  usageInOp SufficientParallelism{} = mempty

consumedInKernelBody :: Aliased lore =>
                        KernelBody lore -> Names
consumedInKernelBody (KernelBody attr stms _) =
  consumedInBody $ Body attr stms []

typeCheckKernel :: TC.Checkable lore => Kernel (Aliases lore) -> TC.TypeM lore ()

typeCheckKernel NumGroups = return ()
typeCheckKernel GroupSize = return ()
typeCheckKernel TileSize = return ()
typeCheckKernel SufficientParallelism{} = return ()

typeCheckKernel (Kernel _ cs space kts kbody) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  checkSpace space
  mapM_ TC.checkType kts
  mapM_ (TC.require [Prim int32] . snd) $ spaceDimensions space

  TC.binding (scopeOfKernelSpace space) $
    checkKernelBody kts kbody
  where checkSpace (KernelSpace _ _ _ num_threads num_groups group_size structure) = do
          mapM_ (TC.require [Prim int32]) [num_threads,num_groups,group_size]
          case structure of
            FlatThreadSpace dims ->
              mapM_ (TC.require [Prim int32] . snd) dims
            NestedThreadSpace dims ->
              let (_, gdim_sizes, _, ldim_sizes) = unzip4 dims
              in mapM_ (TC.require [Prim int32]) $ gdim_sizes ++ ldim_sizes

        checkKernelBody ts (KernelBody (_, attr) stms res) = do
          TC.checkBodyLore attr
          TC.checkStms stms $ do
            unless (length ts == length res) $
              TC.bad $ TC.TypeError $ "Kernel return type is " ++ prettyTuple ts ++
              ", but body returns " ++ show (length res) ++ " values."
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
        checkKernelResult (ConcatReturns o w per_thread_elems moffset v) t = do
          case o of
            SplitContiguous     -> return ()
            SplitStrided stride -> TC.require [Prim int32] stride
          TC.require [Prim int32] w
          TC.require [Prim int32] per_thread_elems
          mapM_ (TC.require [Prim int32]) moffset
          vt <- lookupType v
          unless (vt == t `arrayOfRow` arraySize 0 vt) $
            TC.bad $ TC.TypeError $ "Invalid type for ConcatReturns " ++ pretty v
        checkKernelResult (KernelInPlaceReturn what) t =
          TC.requireI [t] what

        checkWhich AllThreads =
          return ()
        checkWhich (OneThreadPerGroup which) =
          TC.require [Prim int32] which
        checkWhich (ThreadsPerGroup limit) = do
          mapM_ (TC.requireI [Prim int32] . fst) limit
          mapM_ (TC.require [Prim int32] . snd) limit
        checkWhich ThreadsInSpace =
          return ()

instance OpMetrics (Op lore) => OpMetrics (Kernel lore) where
  opMetrics (Kernel _ _ _ _ kbody) =
    inside "Kernel" $ kernelBodyMetrics kbody
    where kernelBodyMetrics :: KernelBody lore -> MetricsM ()
          kernelBodyMetrics = mapM_ bindingMetrics . kernelBodyStms
  opMetrics NumGroups = seen "NumGroups"
  opMetrics GroupSize = seen "GroupSize"
  opMetrics TileSize = seen "TileSize"
  opMetrics SufficientParallelism{} = seen "SufficientParallelism"

instance PrettyLore lore => PP.Pretty (Kernel lore) where
  ppr NumGroups = text "$num_groups()"
  ppr GroupSize = text "$group_size()"
  ppr TileSize = text "$tile_size()"
  ppr (SufficientParallelism se) = text "$sufficientParallelism" <> parens (ppr se)

  ppr (Kernel desc cs space ts body) =
    ppCertificates' cs <>
    text "kernel" <+> text (kernelName desc) <>
    PP.align (ppr space) <+>
    PP.colon <+> ppTuple' ts <+> PP.nestedBlock "{" "}" (ppr body)

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
              FlatThreadSpace dims -> flat dims
              NestedThreadSpace space ->
                parens (commasep $ do
                           (gtid,gd,ltid,ld) <- space
                           return $ ppr (gtid,ltid) <+> "<" <+> ppr (gd,ld))
          flat dims = parens $ commasep $ do
            (i,d) <- dims
            return $ ppr i <+> "<" <+> ppr d

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
  ppr (ConcatReturns o w per_thread_elems offset v) =
    text "concat" <> suff <>
    parens (commasep [ppr w, ppr per_thread_elems] <> offset_text) <+>
    ppr v
    where suff = case o of SplitContiguous     -> mempty
                           SplitStrided stride -> text "Strided" <> parens (ppr stride)
          offset_text = case offset of Nothing -> ""
                                       Just se -> "," <+> "offset=" <> ppr se
  ppr (KernelInPlaceReturn what) =
    text "kernel returns" <+> ppr what
