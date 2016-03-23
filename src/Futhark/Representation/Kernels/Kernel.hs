{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Futhark.Representation.Kernels.Kernel
       ( Kernel(..)

       , KernelInput(..)
       , kernelInputName
       , kernelInputType
       , kernelInputIdent
       , KernelSize(..)
       , ScanKernelOrder(..)
       , chunkedKernelNonconcatOutputs

       , typeCheckKernel

         -- * Generic traversal
       , KernelMapper(..)
       , identityKernelMapper
       , mapKernelM
       )
       where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Identity
import qualified Data.HashSet as HS
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
  (Aliases, removeLambdaAliases, removeBodyAliases)
import Futhark.Analysis.Usage
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics
import Futhark.Tools (partitionChunkedLambdaParameters)
import qualified Futhark.Analysis.Range as Range

data Kernel lore =
    MapKernel Certificates SubExp VName [(VName, SubExp)] [KernelInput lore]
    [(Type, [Int])] (Body lore)
  | ReduceKernel Certificates SubExp
    KernelSize
    Commutativity
    (LambdaT lore)
    (LambdaT lore)
    [SubExp]
    [VName]
  | ScanKernel Certificates SubExp
    KernelSize
    ScanKernelOrder
    (LambdaT lore)
    [(SubExp, VName)]
  | ChunkedMapKernel Certificates SubExp
    KernelSize
    StreamOrd
    (LambdaT lore)
    [VName]
  | WriteKernel Certificates VName VName VName

  | NumGroups
  | GroupSize
    deriving (Eq, Show, Ord)

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

data ScanKernelOrder = ScanTransposed
                     | ScanFlat
                     deriving (Eq, Ord, Show)

-- | Like 'Mapper', but just for 'Kernel's.
data KernelMapper flore tlore m = KernelMapper {
    mapOnKernelSubExp :: SubExp -> m SubExp
  , mapOnKernelLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnKernelBody :: Body flore -> m (Body tlore)
  , mapOnKernelVName :: VName -> m VName
  , mapOnKernelCertificates :: Certificates -> m Certificates
  , mapOnKernelLParam :: LParam flore -> m (LParam tlore)
  }

-- | A mapper that simply returns the 'Kernel' verbatim.
identityKernelMapper :: Monad m => KernelMapper lore lore m
identityKernelMapper = KernelMapper { mapOnKernelSubExp = return
                                    , mapOnKernelLambda = return
                                    , mapOnKernelBody = return
                                    , mapOnKernelVName = return
                                    , mapOnKernelCertificates = return
                                    , mapOnKernelLParam = return
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
mapKernelM tv (ReduceKernel cs w kernel_size comm red_fun fold_fun accs arrs) =
  ReduceKernel <$>
  mapOnKernelCertificates tv cs <*>
  mapOnKernelSubExp tv w <*>
  mapOnKernelSize tv kernel_size <*>
  pure comm <*>
  mapOnKernelLambda tv red_fun <*>
  mapOnKernelLambda tv fold_fun <*>
  mapM (mapOnKernelSubExp tv) accs <*>
  mapM (mapOnKernelVName tv) arrs
mapKernelM tv (ScanKernel cs w kernel_size order fun input) =
  ScanKernel <$>
  mapOnKernelCertificates tv cs <*>
  mapOnKernelSubExp tv w <*>
  mapOnKernelSize tv kernel_size <*>
  pure order <*>
  mapOnKernelLambda tv fun <*>
  (zip <$> mapM (mapOnKernelSubExp tv) nes <*>
   mapM (mapOnKernelVName tv) arrs)
  where (nes, arrs) = unzip input
mapKernelM tv (ChunkedMapKernel cs w kernel_size ordering fun arrs) =
  ChunkedMapKernel <$>
  mapOnKernelCertificates tv cs <*>
  mapOnKernelSubExp tv w <*>
  mapOnKernelSize tv kernel_size <*>
  pure ordering <*>
  mapOnKernelLambda tv fun <*>
  mapM (mapOnKernelVName tv) arrs
mapKernelM _ NumGroups = pure NumGroups
mapKernelM _ GroupSize = pure GroupSize

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
                              }

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
                         }

instance Renameable lore => Rename (KernelInput lore) where
  rename (KernelInput param arr is) =
    KernelInput <$> rename param <*> rename arr <*> rename is

instance Scoped lore (KernelInput lore) where
  scopeOf inp = scopeOfLParams [kernelInputParam inp]

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
    where renamer = KernelMapper rename rename rename rename rename rename

kernelType :: Kernel lore -> [Type]
kernelType (MapKernel _ _ _ is _ returns _) =
  [ rearrangeType perm (arrayOfShape t outer_shape)
  | (t, perm) <- returns ]
  where outer_shape = Shape $ map snd is
kernelType (ReduceKernel _ _ size _ redlam foldlam _ _) =
  let acc_tp = map (`arrayOfRow` kernelWorkgroups size) $ lambdaReturnType redlam
      arr_row_tp = drop (length acc_tp) $ lambdaReturnType foldlam
  in acc_tp ++
     map (`setOuterSize` kernelTotalElements size) arr_row_tp
kernelType (ScanKernel _ w size _ lam _) =
  map (`arrayOfRow` w) (lambdaReturnType lam) ++
  map ((`arrayOfRow` kernelWorkgroups size) .
       (`arrayOfRow` kernelWorkgroupSize size))
  (lambdaReturnType lam)
kernelType (ChunkedMapKernel _ _ size _ fun _) =
  map (`arrayOfRow` kernelNumThreads size) nonconcat_ret <>
  map (`setOuterSize` kernelTotalElements size) concat_ret
  where (nonconcat_ret, concat_ret) =
          splitAt (chunkedKernelNonconcatOutputs fun) $ lambdaReturnType fun
kernelType NumGroups =
  [Prim int32]
kernelType GroupSize =
  [Prim int32]

chunkedKernelNonconcatOutputs :: Lambda lore -> Int
chunkedKernelNonconcatOutputs fun =
  length $ takeWhile (not . outerSizeIsChunk) $ lambdaReturnType fun
  where outerSizeIsChunk = (==Var (paramName chunk)) . arraySize 0
        (chunk, _) = partitionChunkedLambdaParameters $ lambdaParams fun

instance Attributes lore => TypedOp (Kernel lore) where
  opType = pure . staticShapes . kernelType

instance (Attributes lore, Aliased lore) => AliasedOp (Kernel lore) where
  opAliases = map (const mempty) . kernelType

  consumedInOp (MapKernel _ _ _ _ inps _ body) =
    HS.fromList $
    map kernelInputArray $
    filter ((`HS.member` consumed) . kernelInputName) inps
    where consumed = consumedInBody body
  consumedInOp _ = mempty

instance (Attributes lore,
          Attributes (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (Kernel lore) where
  type OpWithAliases (Kernel lore) = Kernel (Aliases lore)

  addOpAliases = runIdentity . mapKernelM alias
    where alias = KernelMapper return (return . Alias.analyseLambda)
                  (return . Alias.analyseBody) return return return

  removeOpAliases = runIdentity . mapKernelM remove
    where remove = KernelMapper return (return . removeLambdaAliases)
                   (return . removeBodyAliases) return return return

instance Attributes lore => IsOp (Kernel lore) where
  safeOp _ = False

instance (Attributes inner, Ranged inner) => RangedOp (Kernel inner) where
  opRanges op = replicate (length $ kernelType op) unknownRange

instance (Attributes lore, CanBeRanged (Op lore)) => CanBeRanged (Kernel lore) where
  type OpWithRanges (Kernel lore) = Kernel (Ranges lore)

  removeOpRanges = runIdentity . mapKernelM remove
    where remove = KernelMapper return (return . removeLambdaRanges)
                   (return . removeBodyRanges) return return return
  addOpRanges = Range.runRangeM . mapKernelM add
    where add = KernelMapper return Range.analyseLambda
                Range.analyseBody return return return

instance (Attributes lore, CanBeWise (Op lore)) => CanBeWise (Kernel lore) where
  type OpWithWisdom (Kernel lore) = Kernel (Wise lore)

  removeOpWisdom = runIdentity . mapKernelM remove
    where remove = KernelMapper return
                   (return . removeLambdaWisdom)
                   (return . removeBodyWisdom)
                   return return return

instance (Aliased lore, UsageInOp (Op lore)) => UsageInOp (Kernel lore) where
  usageInOp (ReduceKernel _ _ _ _ _ foldfun _ arrs) =
    usageInLambda foldfun arrs
  usageInOp (ScanKernel _ _ _ _ fun input) =
    usageInLambda fun arrs
    where arrs = map snd input
  usageInOp (ChunkedMapKernel _ _ _ _ fun arrs) =
    usageInLambda fun arrs
  usageInOp (MapKernel _ _ _ _ inps _ body) =
    mconcat $
    map (UT.consumedUsage . kernelInputArray) $
    filter ((`HS.member` consumed_in_body) . kernelInputName) inps
    where consumed_in_body = consumedInBody body
  usageInOp NumGroups = mempty
  usageInOp GroupSize = mempty

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

typeCheckKernel (ReduceKernel cs w kernel_size _ parfun seqfun accexps arrexps) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  TC.require [Prim int32] w
  typeCheckKernelSize kernel_size
  arrargs <- TC.checkSOACArrayArgs w arrexps
  accargs <- mapM TC.checkArg accexps

  let (fold_acc_ret, _) =
        splitAt (length accexps) $ lambdaReturnType seqfun

  case lambdaParams seqfun of
    [] -> TC.bad $ TC.TypeError "Fold function takes no parameters."
    chunk_param : _
      | Prim (IntType Int32) <- paramType chunk_param -> do
          let seq_args = (Prim int32, mempty) :
                         [ (t `arrayOfRow` Var (paramName chunk_param), als)
                         | (t, als) <- arrargs ]
          TC.checkLambda seqfun seq_args
      | otherwise ->
          TC.bad $ TC.TypeError "First parameter of fold function is not int32-typed."

  let asArg t = (t, mempty)
  TC.checkLambda parfun $ map asArg $ Prim int32 : fold_acc_ret ++ fold_acc_ret
  let acct = map TC.argType accargs
      parRetType = lambdaReturnType parfun
  unless (acct == fold_acc_ret) $
    TC.bad $ TC.TypeError $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomap fold function returns type " ++ prettyTuple fold_acc_ret ++ "."
  unless (acct == parRetType) $
    TC.bad $ TC.TypeError $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomap reduction function returns type " ++ prettyTuple parRetType ++ "."

typeCheckKernel (ScanKernel cs w kernel_size _ fun input) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  TC.require [Prim int32] w
  typeCheckKernelSize kernel_size
  let (nes, arrs) = unzip input
      other_index_arg = (Prim int32, mempty)
  arrargs <- TC.checkSOACArrayArgs w arrs
  accargs <- mapM TC.checkArg nes
  TC.checkLambda fun $ other_index_arg : accargs ++ arrargs
  let startt      = map TC.argType accargs
      intupletype = map TC.argType arrargs
      funret      = lambdaReturnType fun
  unless (startt == funret) $
    TC.bad $ TC.TypeError $
    "Initial value is of type " ++ prettyTuple startt ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."
  unless (intupletype == funret) $
    TC.bad $ TC.TypeError $
    "Array element value is of type " ++ prettyTuple intupletype ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."

typeCheckKernel (ChunkedMapKernel cs w kernel_size _ fun arrs) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  TC.require [Prim int32] w
  typeCheckKernelSize kernel_size

  arrargs <- TC.checkSOACArrayArgs w arrs

  case lambdaParams fun of
    [] -> TC.bad $ TC.TypeError "Chunked map function takes no parameters."
    chunk_param : _
      | Prim (IntType Int32) <- paramType chunk_param -> do
          let args = (Prim int32, mempty) :
                     [ (t `arrayOfRow` Var (paramName chunk_param), als)
                     | (t, als) <- arrargs ]
          TC.checkLambda fun args
      | otherwise ->
          TC.bad $ TC.TypeError "First parameter of chunked map function is not int32-typed."

typeCheckKernel NumGroups = return ()
typeCheckKernel GroupSize = return ()

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
  opMetrics (ReduceKernel _ _ _ _ lam1 lam2 _ _) =
    inside "ReduceKernel" $ lambdaMetrics lam1 >> lambdaMetrics lam2
  opMetrics (ScanKernel _ _ _ _ lam _) =
    inside "ScanKernel" $ lambdaMetrics lam
  opMetrics (ChunkedMapKernel _ _ _ _ fun _) =
    inside "ChunkedMapKernel" $ lambdaMetrics fun
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
  ppr (ReduceKernel cs w kernel_size comm parfun seqfun es as) =
    ppCertificates' cs <> text "reduceKernel" <>
    parens (ppr w <> comma </>
            ppr kernel_size </>
            PP.braces (commasep $ map ppr es) <> comma </>
            commasep (map ppr as) <> comma </>
            ppr comm </>
            ppr parfun <> comma </> ppr seqfun)
  ppr (ScanKernel cs w kernel_size order fun input) =
    ppCertificates' cs <> text "scanKernel" <>
    parens (ppr w <> comma </>
            ppr kernel_size <> comma </>
            ppr order <> comma </>
            PP.braces (commasep $ map ppr es) <> comma </>
            commasep (map ppr as) <> comma </>
            ppr fun)
    where (es, as) = unzip input
  ppr (ChunkedMapKernel cs w kernel_size ordering fun arrs) =
    ppCertificates' cs <> text ("chunkedMapKernel"++ord_str) <>
    parens (ppr w <> comma </>
            ppr kernel_size <> comma </>
            commasep (map ppr arrs) <> comma </>
            ppr fun)
    where ord_str = if ordering == Disorder then "Per" else ""
  ppr NumGroups = text "$num_groups()"
  ppr GroupSize = text "$group_size()"

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

instance Pretty ScanKernelOrder where
  ppr ScanFlat = text "flat"
  ppr ScanTransposed = text "transposed"

instance PrettyLore lore => Pretty (KernelInput lore) where
  ppr inp = ppr (kernelInputType inp) <+>
            ppr (kernelInputName inp) <+>
            text "<-" <+>
            ppr (kernelInputArray inp) <>
            PP.brackets (commasep (map ppr $ kernelInputIndices inp))
