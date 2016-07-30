{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Perform a restricted form of loop tiling within kernel streams.
module Futhark.Optimise.TileLoops
       ( tileLoops )
       where

import Control.Applicative
import Control.Monad.State
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Monoid
import Data.List

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels

import Futhark.Pass
import Futhark.Tools
import Futhark.Util (mapAccumLM)

tileLoops :: Pass Kernels Kernels
tileLoops =
  Pass { passName = "tile loops"
       , passDescription = "Tile stream loops inside kernels"
       , passFunction = intraproceduralTransformation optimiseFunDef
       }

optimiseFunDef :: MonadFreshNames m => FunDef -> m FunDef
optimiseFunDef fundec = do
  body' <- modifyNameSource $ runState m
  return fundec { funDefBody = body' }
  where m = optimiseBody $ funDefBody fundec

type TileM = State VNameSource

optimiseBody :: Body -> TileM Body
optimiseBody (Body () bnds res) =
  Body () <$> (concat <$> mapM optimiseBinding bnds) <*> pure res

optimiseBinding :: Binding -> TileM [Binding]
optimiseBinding (Let pat () (Op (Kernel cs space ts body))) = do
  (extra_bnds, space', body') <- tileInKernelBody mempty initial_variance space body
  return $ extra_bnds ++ [Let pat () $ Op $ Kernel cs space' ts body']
  where initial_variance = HM.map mempty $ scopeOfKernelSpace space
optimiseBinding (Let pat () e) =
  pure <$> (Let pat () <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = optimiseBody }

tileInKernelBody :: Names -> VarianceTable
                 -> KernelSpace -> GenKernelBody res Kernels
                 -> TileM ([Binding], KernelSpace, GenKernelBody res Kernels)
tileInKernelBody branch_variant initial_variance initial_kspace (KernelBody kstms kres) = do
  ((kspace, extra_bndss), kstms') <-
    mapAccumLM tileInKernelStatement (initial_kspace,[]) kstms
  return (extra_bndss,
          kspace,
          KernelBody kstms' kres)
  where variance = varianceInKernelStms initial_variance kstms

        tileInKernelStatement (kspace, extra_bnds) (GroupStream pes w _ lam accs arrs)
          | not $ null arrs,
            chunk_size <- Var $ groupStreamChunkSize lam,
            arr_chunk_params <- groupStreamArrParams lam,
            maybe_1d_tiles <-
              zipWith (is1dTileable branch_variant kspace variance chunk_size) arrs arr_chunk_params,
            maybe_1_5d_tiles <-
              zipWith (is1_5dTileable branch_variant kspace variance chunk_size) arrs arr_chunk_params,
            Just mk_tilings <-
              zipWithM (<|>) maybe_1d_tiles maybe_1_5d_tiles = do

          (kspaces, arr_chunk_params', tile_kstms) <- unzip3 <$> sequence mk_tilings

          let (kspace', kspace_bnds) =
                case kspaces of
                  [] -> (kspace, [])
                  new_kspace : _ -> new_kspace
              KernelBody lam_kstms lam_res = groupStreamLambdaBody lam
              lam_kstms' = concat tile_kstms ++ lam_kstms
              group_size = spaceGroupSize kspace
              lam' = lam { groupStreamLambdaBody = KernelBody lam_kstms' lam_res
                         , groupStreamArrParams = arr_chunk_params'
                         }

          return ((kspace', extra_bnds <> kspace_bnds),
                  GroupStream pes w group_size lam' accs arrs)

        tileInKernelStatement (kspace, extra_bnds) (GroupStream pes w _ lam accs arrs)
          | not $ null arrs,
            FlatSpace gspace <- spaceStructure kspace,
            chunk_size <- Var $ groupStreamChunkSize lam,
            arr_chunk_params <- groupStreamArrParams lam,

            Just mk_tilings <-
              zipWithM (is2dTileable branch_variant kspace variance chunk_size)
              arrs arr_chunk_params = do

          -- We assume that the group size is tile_size*tile_size.
          let group_size = spaceGroupSize kspace

          -- XXX: empty scope here.  Hopefully OK.
          ((tile_size, tiled_group_size), tile_size_bnds) <- flip runBinderT mempty $ do
            group_size_float <-
              letSubExp "group_size_float" $ PrimOp $ ConvOp (SIToFP Int32 Float32) group_size
            tile_size_float <-
              letSubExp "tile_size_float" $
              Apply (nameFromString "sqrt32") [(group_size_float,Observe)] $
              primRetType $ FloatType Float32
            tile_size <- letSubExp "tile_size" $
                         PrimOp $ ConvOp (FPToSI Float32 Int32) tile_size_float
            tiled_group_size <- letSubExp "tiled_group_size" $
                                PrimOp $ BinOp (Mul Int32) tile_size tile_size
            return (tile_size, tiled_group_size)

          space <- forM gspace $ \(gtid,gdim) -> do
            ltid <- newVName "ltid"
            return (gtid,gdim,
                    ltid,tile_size)
          -- We have to recalculate number of workgroups and
          -- number of threads to fit the new workgroup size.
          ((num_threads, num_groups), num_bnds) <- flip runBinderT mempty $
            sufficientGroups gspace tile_size tiled_group_size

          let kspace' = kspace { spaceStructure = NestedSpace space
                               , spaceGroupSize = tiled_group_size
                               , spaceNumThreads = num_threads
                               , spaceNumGroups = num_groups
                               }
              local_ids = map (\(_, _, ltid, _) -> ltid) space

          (arr_chunk_params', tile_kstms) <-
            fmap unzip $ forM mk_tilings $ \mk_tiling ->
              mk_tiling tile_size local_ids

          let KernelBody lam_kstms lam_res = groupStreamLambdaBody lam
              lam_kstms' = concat tile_kstms ++ lam_kstms
              lam' = lam { groupStreamLambdaBody = KernelBody lam_kstms' lam_res
                         , groupStreamArrParams = arr_chunk_params'
                         }

          return ((kspace', extra_bnds ++ tile_size_bnds ++ num_bnds),
                  GroupStream pes w tile_size lam' accs arrs)

        tileInKernelStatement (kspace, extra_bnds) (GroupStream pes w maxchunk lam accs arrs) = do
          (bnds, kspace', lam') <- tileInStreamLambda branch_variant variance kspace lam
          return ((kspace', extra_bnds ++ bnds),
                  GroupStream pes w maxchunk lam' accs arrs)

        tileInKernelStatement (kspace, extra_bnds) (GroupIf pes cond tbranch fbranch) = do
          (t_bnds, kspace', tbranch') <-
            tileInKernelBody branch_variant' variance kspace tbranch
          (f_bnds, kspace'', fbranch') <-
            tileInKernelBody branch_variant' variance kspace' fbranch
          return ((kspace'', extra_bnds<>t_bnds<>f_bnds),
                  GroupIf pes cond tbranch' fbranch')
            where branch_variant' =
                    case cond of Var v -> HM.lookupDefault mempty v variance
                                 _     -> mempty


        tileInKernelStatement acc stm =
          return (acc, stm)

tileInStreamLambda :: Names -> VarianceTable -> KernelSpace -> GroupStreamLambda Kernels
                   -> TileM ([Binding], KernelSpace, GroupStreamLambda Kernels)
tileInStreamLambda branch_variant variance kspace lam = do
  (bnds, kspace', kbody') <-
    tileInKernelBody branch_variant variance' kspace $ groupStreamLambdaBody lam
  return (bnds, kspace', lam { groupStreamLambdaBody = kbody' })
  where variance' = varianceInKernelStms variance $
                    kernelBodyStms $ groupStreamLambdaBody lam

is1dTileable :: MonadFreshNames m =>
                Names -> KernelSpace -> VarianceTable -> SubExp -> VName -> LParam
             -> Maybe (m ((KernelSpace, [Binding]), LParam, [KernelStm Kernels]))
is1dTileable branch_variant kspace variance block_size arr block_param = do
  guard $ HS.null $ HM.lookupDefault mempty arr variance
  guard $ HS.null branch_variant
  return $ do
    (outer_block_param, kstms) <- tile1d kspace block_size block_param
    return ((kspace, []), outer_block_param, kstms)

is1_5dTileable :: MonadFreshNames m =>
                  Names -> KernelSpace -> VarianceTable
               -> SubExp -> VName -> LParam
               -> Maybe (m ((KernelSpace, [Binding]), LParam, [KernelStm Kernels]))
is1_5dTileable branch_variant kspace variance block_size arr block_param = do
  (inner_gtid, inner_gdim) <- invariantToInnermostDimension
  mk_structure <-
    case spaceStructure kspace of
      NestedSpace{} -> Nothing
      FlatSpace gtids_and_gdims ->
        return $ do
          -- Force a functioning group size. XXX: not pretty.
          let n_dims = length gtids_and_gdims
          outer <- forM (take (n_dims-1) gtids_and_gdims) $ \(gtid, gdim) -> do
            ltid <- newVName "ltid"
            return (gtid, gdim, ltid, gdim)

          inner_ltid <- newVName "inner_ltid"
          smaller <- newVName "group_size_is_smaller"
          inner_ldim <- newVName "inner_ldim"
          let is_group_size_smaller =
                mkLet' [] [Ident smaller $ Prim Bool] $
                PrimOp $ CmpOp (CmpSlt Int32) (spaceGroupSize kspace) inner_gdim
              smaller_body = Body () [] [spaceGroupSize kspace]
              not_smaller_body = Body () [] [inner_gdim]
              compute_tiled_group_size =
                mkLet' [] [Ident inner_ldim $ Prim int32] $
                If (Var smaller) smaller_body not_smaller_body [Prim int32]

              structure = NestedSpace $ outer ++ [(inner_gtid, inner_gdim,
                                                   inner_ltid, Var inner_ldim)]
          ((num_threads, num_groups), num_bnds) <- flip runBinderT mempty $ do
            threads_necessary <-
              letSubExp "threads_necessary" =<<
              foldBinOp (Mul Int32)
              (constant (1::Int32)) (map snd gtids_and_gdims)
            groups_necessary <-
              letSubExp "groups_necessary" =<<
              eDivRoundingUp Int32 (eSubExp threads_necessary) (eSubExp $ Var inner_ldim)
            num_threads <-
              letSubExp "num_threads" $
              PrimOp $ BinOp (Mul Int32) groups_necessary (Var inner_ldim)
            return (num_threads, groups_necessary)

          let kspace' = kspace { spaceGroupSize = Var inner_ldim
                               , spaceNumGroups = num_groups
                               , spaceNumThreads = num_threads
                               , spaceStructure = structure
                               }
          return ([is_group_size_smaller, compute_tiled_group_size] ++ num_bnds,
                  kspace')
  return $ do
    (outer_block_param, kstms) <- tile1d kspace block_size block_param
    (structure_bnds, kspace') <- mk_structure
    return ((kspace', structure_bnds), outer_block_param, kstms)
  where invariantToInnermostDimension :: Maybe (VName, SubExp)
        invariantToInnermostDimension =
          case reverse $ spaceDimensions kspace of
            (i,d) : _
              | not $ i `HS.member` HM.lookupDefault mempty arr variance,
                not $ i `HS.member` branch_variant -> Just (i,d)
            _ -> Nothing

tile1d :: MonadFreshNames m =>
          KernelSpace
       -> SubExp
       -> LParam
       -> m (LParam, [KernelStm Kernels])
tile1d kspace block_size block_param = do
  outer_block_param <- do
    name <- newVName $ baseString (paramName block_param) ++ "_outer"
    return block_param { paramName = name }

  let ltid = spaceLocalId kspace
  read_elem_bnd <- do
    name <- newVName $ baseString (paramName outer_block_param) ++ "_elem"
    return $
      mkLet' [] [Ident name $ rowType $ paramType outer_block_param] $
      PrimOp $ Index [] (paramName outer_block_param) [Var ltid]

  let limit = ThreadsPerGroup [(ltid,block_size)]
      read_block_kstm = Thread limit read_elem_bnd
      block_cspace = [(ltid,block_size)]

  let block_pe =
        PatElem (paramName block_param) BindVar $ paramType outer_block_param
      write_block_stms =
        [ Combine block_pe block_cspace $ Var v
        | v <- patternNames $ bindingPattern read_elem_bnd ]

  return (outer_block_param, read_block_kstm : write_block_stms)

is2dTileable :: MonadFreshNames m =>
                Names -> KernelSpace -> VarianceTable -> SubExp -> VName -> LParam
           -> Maybe (SubExp -> [VName] -> m (LParam, [KernelStm Kernels]))
is2dTileable branch_variant kspace variance block_size arr block_param = do
  guard $ HS.null branch_variant
  (permute, permute_perm, permute_dims) <- invariantToAtLeastOneDimension
  Just $ \tile_size local_is -> do
    let [variant_i, invariant_i] = permute local_is
        perm = permute_perm [0,1]
    outer_block_param <- do
      name <- newVName $ baseString (paramName block_param) ++ "_outer"
      return block_param { paramName = name }

    read_elem_bnd <- do
      name <- newVName $ baseString (paramName outer_block_param) ++ "_elem"
      return $
        mkLet' [] [Ident name $ rowType $ paramType outer_block_param] $
        PrimOp $ Index [] (paramName outer_block_param) [Var invariant_i]

    let limit = ThreadsPerGroup [(variant_i,tile_size),(invariant_i,block_size)]
        read_elem_kstm = Thread limit read_elem_bnd

        block_size_2d = Shape $ permute_dims [tile_size, block_size]
        block_cspace = zip local_is $ permute_dims [tile_size,block_size]

    block_name_2d <- newVName $ baseString (paramName block_param) ++ "_2d"
    let block_pe =
          PatElem block_name_2d BindVar $
          rowType (paramType outer_block_param) `arrayOfShape` block_size_2d
        write_block_stms =
          [ Combine block_pe block_cspace $ Var v
          | v <- patternNames $ bindingPattern read_elem_bnd ]

    block_param_aux_name <- newVName $ baseString $ paramName block_param
    let block_param_aux = Ident block_param_aux_name $
                          rearrangeType perm $ patElemType block_pe
    let index_block_kstms =
          map (Thread ThreadsInSpace)
          [mkLet' [] [block_param_aux] $
            PrimOp $ Rearrange [] perm block_name_2d,
           mkLet' [] [paramIdent block_param] $
            PrimOp $ Index [] (identName block_param_aux) [Var variant_i]]

    return (outer_block_param, read_elem_kstm : write_block_stms ++ index_block_kstms)

  where invariantToAtLeastOneDimension :: Maybe ([a] -> [a], [b] -> [b], [c] -> [c])
        invariantToAtLeastOneDimension = do
          [(i,_),(j,_)] <- Just $ spaceDimensions kspace
          let variant_to = HM.lookupDefault mempty arr variance
          if i `HS.member` variant_to && not (j `HS.member` variant_to) then
            Just (id, id, id)
          else if j `HS.member` variant_to && not (i `HS.member` variant_to) then
            Just (reverse, reverse, reverse)
          else
            Nothing

-- | The variance table keeps a mapping from a variable name
-- (something produced by a 'KernelStm') to the kernel thread indices
-- that name depends on.  If a variable is not present in this table,
-- that means it is bound outside the kernel (and so can be considered
-- invariant to all dimensions).
type VarianceTable = HM.HashMap VName Names

varianceInKernelStms :: VarianceTable -> [KernelStm Kernels] -> VarianceTable
varianceInKernelStms = foldl extend
  where extend variance (Thread _ bnd) =
          varianceInBinding variance bnd

        extend variance stm =
          let free_variant = mconcat $
                             map (flip (HM.lookupDefault mempty) variance) $
                             HS.toList $ freeIn stm
              new_variance = HM.map (const free_variant) (scopeOf stm)
          in variance <> new_variance

varianceInBinding :: VarianceTable -> Binding -> VarianceTable
varianceInBinding variance bnd =
  foldl' add variance $ patternNames $ bindingPattern bnd
  where add variance' v = HM.insert v binding_variance variance'
        look variance' v = HS.insert v $ HM.lookupDefault mempty v variance'
        binding_variance = mconcat $ map (look variance) $ HS.toList (freeInBinding bnd)

sufficientGroups :: MonadBinder m =>
                    [(VName,SubExp)] -> SubExp -> SubExp
                 -> m (SubExp, SubExp)
sufficientGroups gspace tile_size group_size = do
  groups_in_dims <- forM gspace $ \(_, d) ->
    letSubExp "groups_in_dim" =<< eDivRoundingUp Int32 (eSubExp d) (eSubExp tile_size)
  num_groups <- letSubExp "num_groups" =<<
                foldBinOp (Mul Int32) (constant (1::Int32)) groups_in_dims
  num_threads <- letSubExp "num_threads" $
                 PrimOp $ BinOp (Mul Int32) num_groups group_size
  return (num_threads, num_groups)
