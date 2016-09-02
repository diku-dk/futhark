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

optimiseFunDef :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
optimiseFunDef fundec = do
  body' <- modifyNameSource $ runState m
  return fundec { funDefBody = body' }
  where m = optimiseBody $ funDefBody fundec

type TileM = State VNameSource

optimiseBody :: Body Kernels -> TileM (Body Kernels)
optimiseBody (Body () bnds res) =
  Body () <$> (concat <$> mapM optimiseBinding bnds) <*> pure res

optimiseBinding :: Binding Kernels -> TileM [Binding Kernels]
optimiseBinding (Let pat () (Op (Kernel cs space ts body))) = do
  (extra_bnds, space', body') <- tileInKernelBody mempty initial_variance space body
  return $ extra_bnds ++ [Let pat () $ Op $ Kernel cs space' ts body']
  where initial_variance = HM.map mempty $ scopeOfKernelSpace space
optimiseBinding (Let pat () e) =
  pure <$> (Let pat () <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = optimiseBody }

tileInKernelBody :: Names -> VarianceTable
                 -> KernelSpace -> KernelBody InKernel
                 -> TileM ([Binding Kernels], KernelSpace, KernelBody InKernel)
tileInKernelBody branch_variant initial_variance initial_kspace (KernelBody () kstms kres) = do
  (extra_bnds, kspace', kstms') <-
    tileInBindings branch_variant initial_variance initial_kspace kstms
  return (extra_bnds, kspace', KernelBody () kstms' kres)

tileInBody :: Names -> VarianceTable
           -> KernelSpace -> Body InKernel
           -> TileM ([Binding Kernels], KernelSpace, Body InKernel)
tileInBody branch_variant initial_variance initial_kspace (Body () stms res) = do
  (extra_bnds, kspace', stms') <-
    tileInBindings branch_variant initial_variance initial_kspace stms
  return (extra_bnds, kspace', Body () stms' res)

tileInBindings :: Names -> VarianceTable
               -> KernelSpace -> [Binding InKernel]
               -> TileM ([Binding Kernels], KernelSpace, [Binding InKernel])
tileInBindings branch_variant initial_variance initial_kspace kstms = do
  ((kspace, extra_bndss), kstms') <-
    mapAccumLM tileInKernelStatement (initial_kspace,[]) kstms
  return (extra_bndss, kspace, kstms')
  where variance = varianceInBindings initial_variance kstms

        tileInKernelStatement (kspace, extra_bnds)
          (Let pat attr (Op (GroupStream w max_chunk lam accs arrs)))
          | max_chunk == w,
            not $ null arrs,
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
              Body () lam_kstms lam_res = groupStreamLambdaBody lam
              lam_kstms' = concat tile_kstms ++ lam_kstms
              group_size = spaceGroupSize kspace
              lam' = lam { groupStreamLambdaBody = Body () lam_kstms' lam_res
                         , groupStreamArrParams = arr_chunk_params'
                         }

          return ((kspace', extra_bnds <> kspace_bnds),
                  Let pat attr $ Op $ GroupStream w group_size lam' accs arrs)

        tileInKernelStatement (kspace, extra_bnds)
          (Let pat attr (Op (GroupStream w max_chunk lam accs arrs)))
          | w == max_chunk,
            not $ null arrs,
            FlatSpace gspace <- spaceStructure kspace,
            chunk_size <- Var $ groupStreamChunkSize lam,
            arr_chunk_params <- groupStreamArrParams lam,

            Just mk_tilings <-
              zipWithM (is2dTileable branch_variant kspace variance chunk_size)
              arrs arr_chunk_params = do

          -- XXX: empty scope here.  Hopefully OK.
          ((tile_size, tiled_group_size), tile_size_bnds) <- flip runBinderT mempty $ do
            tile_size <- letSubExp "tile_size" $ Op TileSize
            tiled_group_size <- letSubExp "tiled_group_size" $
                                BasicOp $ BinOp (Mul Int32) tile_size tile_size
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

          let Body () lam_kstms lam_res = groupStreamLambdaBody lam
              lam_kstms' = concat tile_kstms ++ lam_kstms
              lam' = lam { groupStreamLambdaBody = Body () lam_kstms' lam_res
                         , groupStreamArrParams = arr_chunk_params'
                         }

          return ((kspace', extra_bnds ++ tile_size_bnds ++ num_bnds),
                  Let pat attr $ Op $ GroupStream w tile_size lam' accs arrs)

        tileInKernelStatement (kspace, extra_bnds)
          (Let pat attr (Op (GroupStream w maxchunk lam accs arrs))) = do
          (bnds, kspace', lam') <- tileInStreamLambda branch_variant variance kspace lam
          return ((kspace', extra_bnds ++ bnds),
                  Let pat attr $ Op $ GroupStream w maxchunk lam' accs arrs)

        tileInKernelStatement acc stm =
          return (acc, stm)

tileInStreamLambda :: Names -> VarianceTable -> KernelSpace -> GroupStreamLambda InKernel
                   -> TileM ([Binding Kernels], KernelSpace, GroupStreamLambda InKernel)
tileInStreamLambda branch_variant variance kspace lam = do
  (bnds, kspace', kbody') <-
    tileInBody branch_variant variance' kspace $ groupStreamLambdaBody lam
  return (bnds, kspace', lam { groupStreamLambdaBody = kbody' })
  where variance' = varianceInBindings variance $
                    bodyBindings $ groupStreamLambdaBody lam

is1dTileable :: MonadFreshNames m =>
                Names -> KernelSpace -> VarianceTable -> SubExp -> VName -> LParam InKernel
             -> Maybe (m ((KernelSpace, [Binding Kernels]),
                           LParam InKernel,
                           [Binding InKernel]))
is1dTileable branch_variant kspace variance block_size arr block_param = do
  guard $ HS.null $ HM.lookupDefault mempty arr variance
  guard $ HS.null branch_variant
  return $ do
    (outer_block_param, kstms) <- tile1d kspace block_size block_param
    return ((kspace, []), outer_block_param, kstms)

is1_5dTileable :: MonadFreshNames m =>
                  Names -> KernelSpace -> VarianceTable
               -> SubExp -> VName -> LParam InKernel
               -> Maybe (m ((KernelSpace, [Binding Kernels]),
                            LParam InKernel,
                            [Binding InKernel]))
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
                BasicOp $ CmpOp (CmpSlt Int32) (spaceGroupSize kspace) inner_gdim
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
              BasicOp $ BinOp (Mul Int32) groups_necessary (Var inner_ldim)
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
       -> LParam InKernel
       -> m (LParam InKernel, [Binding InKernel])
tile1d kspace block_size block_param = do
  outer_block_param <- do
    name <- newVName $ baseString (paramName block_param) ++ "_outer"
    return block_param { paramName = name }

  let ltid = spaceLocalId kspace
  read_elem_bnd <- do
    name <- newVName $ baseString (paramName outer_block_param) ++ "_elem"
    return $
      mkLet' [] [Ident name $ rowType $ paramType outer_block_param] $
      BasicOp $ Index [] (paramName outer_block_param) [DimFix $ Var ltid]

  let block_cspace = [(ltid,block_size)]
      block_pe =
        PatElem (paramName block_param) BindVar $ paramType outer_block_param
      write_block_stms =
        [ Let (Pattern [] [block_pe]) () $ Op $
          Combine block_cspace [patElemType pe] (constant True) $
          Body () [read_elem_bnd] [Var $ patElemName pe]
        | pe <- patternElements $ bindingPattern read_elem_bnd ]

  return (outer_block_param, write_block_stms)

is2dTileable :: MonadFreshNames m =>
                Names -> KernelSpace -> VarianceTable -> SubExp -> VName -> LParam InKernel
             -> Maybe (SubExp -> [VName] -> m (LParam InKernel, [Binding InKernel]))
is2dTileable branch_variant kspace variance block_size arr block_param = do
  guard $ HS.null branch_variant
  pt <- case rowType $ paramType block_param of
          Prim pt -> return pt
          _       -> Nothing
  (permute, permute_perm, permute_dims) <- invariantToAtLeastOneDimension
  Just $ \tile_size local_is -> do
    let [variant_i, invariant_i] = permute local_is
        perm = permute_perm [0,1]
        [(global_i,global_d),_] = rearrangeShape perm $ spaceDimensions kspace
    outer_block_param <- do
      name <- newVName $ baseString (paramName block_param) ++ "_outer"
      return block_param { paramName = name }

    do_index <- newVName "do_index"
    let do_index_bnd = mkLet' [] [Ident do_index $ Prim Bool] $
                       BasicOp $ CmpOp (CmpSlt Int32) (Var global_i) global_d
    elem_name <- newVName $ baseString (paramName outer_block_param) ++ "_elem"
    let read_elem_bnd = mkLet' [] [Ident elem_name $ Prim pt] $
                        BasicOp $ Index [] (paramName outer_block_param) $
                        fullSlice (paramType outer_block_param) [DimFix $ Var invariant_i]

    let block_size_2d = Shape $ permute_dims [tile_size, block_size]
        block_cspace = zip local_is $ permute_dims [tile_size,block_size]

    block_name_2d <- newVName $ baseString (paramName block_param) ++ "_2d"
    let block_pe =
          PatElem block_name_2d BindVar $
          rowType (paramType outer_block_param) `arrayOfShape` block_size_2d
        write_block_stm =
         Let (Pattern [] [block_pe]) () $
          Op $ Combine block_cspace [Prim pt] (Var do_index) $
          Body () [read_elem_bnd] [Var elem_name]

    block_param_aux_name <- newVName $ baseString $ paramName block_param
    let block_param_aux = Ident block_param_aux_name $
                          rearrangeType perm $ patElemType block_pe
    let index_block_kstms =
          [mkLet' [] [block_param_aux] $
            BasicOp $ Rearrange [] perm block_name_2d,
           mkLet' [] [paramIdent block_param] $
            BasicOp $ Index [] (identName block_param_aux) $
            fullSlice (identType block_param_aux) [DimFix $ Var variant_i]]

    return (outer_block_param, do_index_bnd : write_block_stm : index_block_kstms)

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
-- (something produced by a 'Binding') to the kernel thread indices
-- that name depends on.  If a variable is not present in this table,
-- that means it is bound outside the kernel (and so can be considered
-- invariant to all dimensions).
type VarianceTable = HM.HashMap VName Names

varianceInBindings :: VarianceTable -> [Binding InKernel] -> VarianceTable
varianceInBindings = foldl varianceInBinding

varianceInBinding :: VarianceTable -> Binding InKernel -> VarianceTable
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
                 BasicOp $ BinOp (Mul Int32) num_groups group_size
  return (num_threads, num_groups)
