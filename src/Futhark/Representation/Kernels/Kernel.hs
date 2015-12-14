{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Futhark.Representation.Kernels.Kernel
       ( Kernel(..)

       , KernelInput(..)
       , kernelInputName
       , kernelInputType
       , kernelInputIdent
       , KernelSize(..)
       , ScanKernelOrder(..)

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
import Data.Loc (noLoc)

import Prelude

import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Binder.Class (Proper)
import Futhark.Representation.AST
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Util.Pretty as PP
import Futhark.Util.Pretty
  ((</>), (<+>), ppr, comma, commasep, Pretty, parens, text)
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplifier.Lore
import Futhark.Representation.Ranges
  (Ranges, removeLambdaRanges, removeBodyRanges)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.Aliases
  (Aliases, removeLambdaAliases, removeBodyAliases)
import Futhark.Analysis.Usage
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics
import qualified Futhark.Analysis.Range as Range

data Kernel lore =
    MapKernel Certificates SubExp VName [(VName, SubExp)] [KernelInput lore]
    [(Type, [Int])] (Body lore)
  | ReduceKernel Certificates SubExp
    KernelSize
    (LambdaT lore)
    (LambdaT lore)
    [SubExp]
    [VName]
  | ScanKernel Certificates SubExp
    KernelSize
    ScanKernelOrder
    (LambdaT lore)
    [(SubExp, VName)]
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

kernelInputType :: Typed (Annotations.LParam lore) =>
                   KernelInput lore -> Type
kernelInputType = typeOf . kernelInputParam

kernelInputIdent :: Typed (Annotations.LParam lore) =>
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
mapKernelM tv (ReduceKernel cs w kernel_size red_fun fold_fun accs arrs) =
  ReduceKernel <$>
  mapOnKernelCertificates tv cs <*>
  mapOnKernelSubExp tv w <*>
  mapOnKernelSize tv kernel_size <*>
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

instance (FreeIn (Annotations.LParam lore)) =>
         FreeIn (KernelInput lore) where
  freeIn (KernelInput param arr is) =
    freeIn param <> freeIn arr <> freeIn is

instance (Proper lore, FreeIn (Annotations.LParam lore)) =>
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

instance Proper lore => Substitute (Kernel lore) where
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

instance Proper lore => Rename (Kernel lore) where
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
kernelType (ReduceKernel _ _ size parlam _ _ _) =
  map (`arrayOfRow` kernelWorkgroups size) $ lambdaReturnType parlam
kernelType (ScanKernel _ w size _ lam _) =
  map (`arrayOfRow` w) (lambdaReturnType lam) ++
  map ((`arrayOfRow` kernelWorkgroups size) .
       (`arrayOfRow` kernelWorkgroupSize size))
  (lambdaReturnType lam)

instance Proper lore => TypedOp (Kernel lore) where
  opType = pure . staticShapes . kernelType

instance (Proper lore, Aliased lore) => AliasedOp (Kernel lore) where
  opAliases (MapKernel _ _ _ _ _ returns _) =
    map (const mempty) returns
  opAliases (ReduceKernel _ _ _ _ _ nes _) =
    map (const mempty) nes
  opAliases (ScanKernel _ _ _ _ lam _) =
    replicate (length (lambdaReturnType lam) * 2) mempty

  consumedInOp _ = mempty -- FIXME

instance (Proper lore,
          Proper (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (Kernel lore) where
  type OpWithAliases (Kernel lore) = Kernel (Aliases lore)

  addOpAliases = runIdentity . mapKernelM alias
    where alias = KernelMapper return (return . Alias.analyseLambda)
                  (return . Alias.analyseBody) return return return

  removeOpAliases = runIdentity . mapKernelM remove
    where remove = KernelMapper return (return . removeLambdaAliases)
                   (return . removeBodyAliases) return return return

removeKernelInputAliases :: KernelInput (Aliases lore) -> KernelInput lore
removeKernelInputAliases inp =
  inp { kernelInputParam = kernelInputParam inp }

instance Proper lore => IsOp (Kernel lore) where
  safeOp _ = False

instance (Proper inner, Ranged inner) => RangedOp (Kernel inner) where
  opRanges op = replicate (length $ kernelType op) unknownRange

instance (Proper lore, CanBeRanged (Op lore)) => CanBeRanged (Kernel lore) where
  type OpWithRanges (Kernel lore) = Kernel (Ranges lore)

  removeOpRanges = runIdentity . mapKernelM remove
    where remove = KernelMapper return (return . removeLambdaRanges)
                   (return . removeBodyRanges) return return return
  addOpRanges = Range.runRangeM . mapKernelM add
    where add = KernelMapper return Range.analyseLambda
                Range.analyseBody return return return

instance (Proper lore, CanBeWise (Op lore)) => CanBeWise (Kernel lore) where
  type OpWithWisdom (Kernel lore) = Kernel (Wise lore)

  removeOpWisdom = runIdentity . mapKernelM remove
    where remove = KernelMapper return
                   (return . removeLambdaWisdom)
                   (return . removeBodyWisdom)
                   return return return

instance (Aliased lore, UsageInOp (Op lore)) => UsageInOp (Kernel lore) where
  usageInOp _ = mempty -- FIXME

typeCheckKernel :: TC.Checkable lore => Kernel (Aliases lore) -> TC.TypeM lore ()

typeCheckKernel (MapKernel cs w index ispace inps returns body) = do
  mapM_ (TC.requireI [Basic Cert]) cs
  TC.require [Basic Int] w
  mapM_ (TC.require [Basic Int]) bounds
  index_param <- TC.basicLParamM index Int
  iparams' <- forM iparams $ \iparam -> TC.basicLParamM iparam Int
  forM_ returns $ \(t, perm) ->
    let return_rank = arrayRank t + rank
    in unless (sort perm == [0..return_rank - 1]) $
       TC.bad $ TC.TypeError noLoc $
       "Permutation " ++ pretty perm ++
       " not valid for returning " ++ pretty t ++
       " from a rank " ++ pretty rank ++ " kernel."
  inps' <- kernelInputsToNamesTypesAndLores $ map removeKernelInputAliases inps
  TC.checkFun' (nameFromString "<kernel body>",
                map (`toDecl` Nonunique) $ staticShapes rettype,
                lamParamsToNamesTypesAndLores (index_param : iparams') ++ inps',
                body) $ do
    TC.checkLambdaParams $ map kernelInputParam inps
    mapM_ checkKernelInput inps
    inps_als <- mapM (TC.lookupAliases . kernelInputArray) inps
    let consumable_inps = consumableInputs (map fst ispace) $
                          zip inps inps_als
    TC.consumeOnlyParams consumable_inps $ TC.checkBody body
    bodyt <- bodyExtType body
    unless (map rankShaped bodyt ==
            map rankShaped (staticShapes rettype)) $
      TC.bad $
      TC.ReturnTypeError noLoc (nameFromString "<kernel body>")
      (TC.Several $ staticShapes rettype)
      (TC.Several bodyt)
  where (iparams, bounds) = unzip ispace
        rank = length ispace
        (rettype, _) = unzip returns
        checkKernelInput inp = do
          TC.checkExp $ PrimOp $ Index []
            (kernelInputArray inp) (kernelInputIndices inp)

          arr_t <- lookupType $ kernelInputArray inp
          unless (stripArray (length $ kernelInputIndices inp) arr_t ==
                  kernelInputType inp) $
            TC.bad $ TC.TypeError noLoc $
            "Kernel input " ++ pretty inp ++ " has inconsistent type."

typeCheckKernel (ReduceKernel cs w kernel_size parfun seqfun accexps arrexps) = do
  mapM_ (TC.requireI [Basic Cert]) cs
  TC.require [Basic Int] w
  typeCheckKernelSize kernel_size
  arrargs <- TC.checkSOACArrayArgs w arrexps
  accargs <- mapM TC.checkArg accexps

  case lambdaParams seqfun of
    [] -> TC.bad $ TC.TypeError noLoc "Fold function takes no parameters."
    chunk_param : _
      | Basic Int <- paramType chunk_param -> do
          let seq_args = (Basic Int, mempty) :
                         [ (t `arrayOfRow` Var (paramName chunk_param), als)
                         | (t, als) <- arrargs ]
          TC.checkLambda seqfun seq_args
      | otherwise ->
          TC.bad $ TC.TypeError noLoc "First parameter of fold function is not integer-typed."

  let seqRetType = lambdaReturnType seqfun
      asArg t = (t, mempty)
  TC.checkLambda parfun $ map asArg $ Basic Int : seqRetType ++ seqRetType
  let acct = map TC.argType accargs
      parRetType = lambdaReturnType parfun
  unless (acct == seqRetType) $
    TC.bad $ TC.TypeError noLoc $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomap fold function returns type " ++ prettyTuple seqRetType ++ "."
  unless (acct == parRetType) $
    TC.bad $ TC.TypeError noLoc $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomap reduction function returns type " ++ prettyTuple parRetType ++ "."

typeCheckKernel (ScanKernel cs w kernel_size _ fun input) = do
  mapM_ (TC.requireI [Basic Cert]) cs
  TC.require [Basic Int] w
  typeCheckKernelSize kernel_size
  let (nes, arrs) = unzip input
      other_index_arg = (Basic Int, mempty)
  arrargs <- TC.checkSOACArrayArgs w arrs
  accargs <- mapM TC.checkArg nes
  TC.checkLambda fun $ other_index_arg : accargs ++ arrargs
  let startt      = map TC.argType accargs
      intupletype = map TC.argType arrargs
      funret      = lambdaReturnType fun
  unless (startt == funret) $
    TC.bad $ TC.TypeError noLoc $
    "Initial value is of type " ++ prettyTuple startt ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."
  unless (intupletype == funret) $
    TC.bad $ TC.TypeError noLoc $
    "Array element value is of type " ++ prettyTuple intupletype ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."

typeCheckKernelSize :: TC.Checkable lore =>
                       KernelSize -> TC.TypeM lore ()
typeCheckKernelSize (KernelSize num_groups workgroup_size per_thread_elements
                     num_elements offset_multiple num_threads) = do
  TC.require [Basic Int] num_groups
  TC.require [Basic Int] workgroup_size
  TC.require [Basic Int] per_thread_elements
  TC.require [Basic Int] num_elements
  TC.require [Basic Int] offset_multiple
  TC.require [Basic Int] num_threads

lamParamsToNamesTypesAndLores :: TC.Checkable lore =>
                                 [LParam lore]
                              -> [(VName, DeclType, TC.VarBindingLore lore)]
lamParamsToNamesTypesAndLores = map nameTypeAndLore
  where nameTypeAndLore fparam = (paramName fparam,
                                  toDecl (paramType fparam) Unique,
                                  TC.LambdaBound $ paramAttr fparam)

kernelInputsToNamesTypesAndLores :: TC.Checkable lore =>
                                    [KernelInput lore]
                                 -> TC.TypeM lore
                                    [(VName, DeclType, TC.VarBindingLore lore)]
kernelInputsToNamesTypesAndLores = mapM nameTypeAndLore
  where nameTypeAndLore input =
          return (kernelInputName input,
                  toDecl (kernelInputType input) Unique,
                  TC.LambdaBound $ paramAttr $ kernelInputParam input)

-- | A kernel input is consumable iff its indexing is a permutation of
-- the full index space.
consumableInputs :: [VName] -> [(KernelInput lore, Names)] -> [(VName, Names)]
consumableInputs is = map (first kernelInputName) .
                      filter ((==is_sorted) . sort . kernelInputIndices . fst)
  where is_sorted = sort (map Var is)

instance OpMetrics (Op lore) => OpMetrics (Kernel lore) where
  opMetrics (MapKernel _ _ _ _ _ _ body) =
    inside "MapKernel" $ bodyMetrics body
  opMetrics (ReduceKernel _ _ _ lam1 lam2 _ _) =
    inside "ReduceKernel" $ lambdaMetrics lam1 >> lambdaMetrics lam2
  opMetrics (ScanKernel _ _ _ _ lam _) =
    inside "ScanKernel" $ lambdaMetrics lam

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
  ppr (ReduceKernel cs w kernel_size parfun seqfun es as) =
    ppCertificates' cs <> text "reduceKernel" <>
    parens (ppr w <> comma </>
            ppr kernel_size </>
            PP.braces (commasep $ map ppr es) <> comma </>
            commasep (map ppr as) </>
            ppr parfun <> comma </> ppr seqfun)
  ppr (ScanKernel cs w kernel_size order fun input) =
    ppCertificates' cs <> text "scanKernel" <>
    parens (ppr w <> comma </>
            ppr kernel_size <> comma </>
            ppr order <> comma </>
            PP.braces (commasep $ map ppr es) <> comma </>
            commasep (map ppr as) </>
            ppr fun)
    where (es, as) = unzip input

instance Pretty KernelSize where
  ppr (KernelSize
       num_chunks workgroup_size per_thread_elements
       num_elements offset_multiple num_threads) =
    commasep [ppr num_chunks,
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
