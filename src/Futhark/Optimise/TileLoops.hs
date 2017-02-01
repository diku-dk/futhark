{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Perform a restricted form of loop tiling within kernel streams.
-- We only tile primitive types, to avoid excessive local memory use.
module Futhark.Optimise.TileLoops
       ( tileLoops )
       where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
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
  body' <- modifyNameSource $ runState $
           runReaderT m (scopeOfFParams (funDefParams fundec))
  return fundec { funDefBody = body' }
  where m = optimiseBody $ funDefBody fundec

type TileM = ReaderT (Scope Kernels) (State VNameSource)

optimiseBody :: Body Kernels -> TileM (Body Kernels)
optimiseBody (Body () bnds res) =
  Body () <$> (concat <$> mapM optimiseStm bnds) <*> pure res

optimiseStm :: Stm Kernels -> TileM [Stm Kernels]
optimiseStm (Let pat () (Op (Kernel desc cs space ts body))) = do
  (extra_bnds, space', body') <- tileInKernelBody mempty initial_variance space body
  return $ extra_bnds ++ [Let pat () $ Op $ Kernel desc cs space' ts body']
  where initial_variance = HM.map mempty $ scopeOfKernelSpace space
optimiseStm (Let pat () e) =
  pure <$> (Let pat () <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = const optimiseBody }

tileInKernelBody :: Names -> VarianceTable
                 -> KernelSpace -> KernelBody InKernel
                 -> TileM ([Stm Kernels], KernelSpace, KernelBody InKernel)
tileInKernelBody branch_variant initial_variance initial_kspace (KernelBody () kstms kres) = do
  (extra_bnds, kspace', kstms') <-
    tileInStms branch_variant initial_variance initial_kspace kstms
  return (extra_bnds, kspace', KernelBody () kstms' kres)

tileInBody :: Names -> VarianceTable
           -> KernelSpace -> Body InKernel
           -> TileM ([Stm Kernels], KernelSpace, Body InKernel)
tileInBody branch_variant initial_variance initial_kspace (Body () stms res) = do
  (extra_bnds, kspace', stms') <-
    tileInStms branch_variant initial_variance initial_kspace stms
  return (extra_bnds, kspace', Body () stms' res)

tileInStms :: Names -> VarianceTable
           -> KernelSpace -> [Stm InKernel]
           -> TileM ([Stm Kernels], KernelSpace, [Stm InKernel])
tileInStms branch_variant initial_variance initial_kspace kstms = do
  ((kspace, extra_bndss), kstms') <-
    mapAccumLM tileInKernelStatement (initial_kspace,[]) kstms
  return (extra_bndss, kspace, kstms')
  where variance = varianceInStms initial_variance kstms

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
            FlatThreadSpace gspace <- spaceStructure kspace,
            chunk_size <- Var $ groupStreamChunkSize lam,
            arr_chunk_params <- groupStreamArrParams lam,

            Just mk_tilings <-
              zipWithM (is2dTileable branch_variant kspace variance chunk_size)
              arrs arr_chunk_params = do

          ((tile_size, tiled_group_size), tile_size_bnds) <- runBinder $ do
            tile_size <- letSubExp "tile_size" $ Op TileSize
            tiled_group_size <- letSubExp "tiled_group_size" $
                                BasicOp $ BinOp (Mul Int32) tile_size tile_size
            return (tile_size, tiled_group_size)

          let (tiled_gspace,untiled_gspace) = splitAt 2 $ reverse gspace
          -- Play with reversion to ensure we get increasing IDs for
          -- ltids.  This affects readability of generated code.
          untiled_gspace' <- fmap reverse $ forM (reverse untiled_gspace) $ \(gtid,gdim) -> do
            ltid <- newVName "ltid"
            return (gtid,gdim,
                    ltid, constant (1::Int32))
          tiled_gspace' <- fmap reverse $ forM (reverse tiled_gspace) $ \(gtid,gdim) -> do
            ltid <- newVName "ltid"
            return (gtid,gdim,
                    ltid, tile_size)
          let gspace' = reverse $ tiled_gspace' ++ untiled_gspace'

          -- We have to recalculate number of workgroups and
          -- number of threads to fit the new workgroup size.
          ((num_threads, num_groups), num_bnds) <-
            runBinder $ sufficientGroups gspace' tiled_group_size

          let kspace' = kspace { spaceStructure = NestedThreadSpace gspace'
                               , spaceGroupSize = tiled_group_size
                               , spaceNumThreads = num_threads
                               , spaceNumGroups = num_groups
                               }
              local_ids = map (\(_, _, ltid, _) -> ltid) gspace'

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
                   -> TileM ([Stm Kernels], KernelSpace, GroupStreamLambda InKernel)
tileInStreamLambda branch_variant variance kspace lam = do
  (bnds, kspace', kbody') <-
    tileInBody branch_variant variance' kspace $ groupStreamLambdaBody lam
  return (bnds, kspace', lam { groupStreamLambdaBody = kbody' })
  where variance' = varianceInStms variance $
                    bodyStms $ groupStreamLambdaBody lam

is1dTileable :: MonadFreshNames m =>
                Names -> KernelSpace -> VarianceTable -> SubExp -> VName -> LParam InKernel
             -> Maybe (m ((KernelSpace, [Stm Kernels]),
                           LParam InKernel,
                           [Stm InKernel]))
is1dTileable branch_variant kspace variance block_size arr block_param = do
  guard $ HS.null $ HM.lookupDefault mempty arr variance
  guard $ HS.null branch_variant
  guard $ primType $ rowType $ paramType block_param

  return $ do
    (outer_block_param, kstms) <- tile1d kspace block_size block_param
    return ((kspace, []), outer_block_param, kstms)

is1_5dTileable :: (MonadFreshNames m, HasScope Kernels m) =>
                  Names -> KernelSpace -> VarianceTable
               -> SubExp -> VName -> LParam InKernel
               -> Maybe (m ((KernelSpace, [Stm Kernels]),
                            LParam InKernel,
                            [Stm InKernel]))
is1_5dTileable branch_variant kspace variance block_size arr block_param = do
  guard $ primType $ rowType $ paramType block_param

  (inner_gtid, inner_gdim) <- invariantToInnermostDimension
  mk_structure <-
    case spaceStructure kspace of
      NestedThreadSpace{} -> Nothing
      FlatGroupSpace{} -> Nothing
      FlatThreadSpace gtids_and_gdims ->
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

              structure = NestedThreadSpace $ outer ++ [(inner_gtid, inner_gdim,
                                                         inner_ltid, Var inner_ldim)]
          ((num_threads, num_groups), num_bnds) <- runBinder $ do
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
       -> m (LParam InKernel, [Stm InKernel])
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
             -> Maybe (SubExp -> [VName] -> m (LParam InKernel, [Stm InKernel]))
is2dTileable branch_variant kspace variance block_size arr block_param = do
  guard $ HS.null branch_variant
  guard $ primType $ rowType $ paramType block_param

  pt <- case rowType $ paramType block_param of
          Prim pt -> return pt
          _       -> Nothing
  inner_perm <- invariantToOneOfTwoInnerDims
  Just $ \tile_size local_is -> do
    let num_outer = length local_is - 2
        perm = [0..num_outer-1] ++ map (+num_outer) inner_perm
        invariant_i : variant_i : _ = reverse $ rearrangeShape perm local_is
        (global_i,global_d):_ = rearrangeShape inner_perm $ drop num_outer $ spaceDimensions kspace
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

    let block_size_2d = Shape $ rearrangeShape inner_perm [tile_size, block_size]
        block_cspace = zip (drop num_outer local_is) $ rearrangeShape inner_perm [tile_size,block_size]

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
                          rearrangeType inner_perm $ patElemType block_pe
    let index_block_kstms =
          [mkLet' [] [block_param_aux] $
            BasicOp $ Rearrange [] inner_perm block_name_2d,
           mkLet' [] [paramIdent block_param] $
            BasicOp $ Index [] (identName block_param_aux) $
            fullSlice (identType block_param_aux) [DimFix $ Var variant_i]]

    return (outer_block_param, do_index_bnd : write_block_stm : index_block_kstms)

  where invariantToOneOfTwoInnerDims :: Maybe [Int]
        invariantToOneOfTwoInnerDims = do
          (j,_) : (i,_) : _ <- Just $ reverse $ spaceDimensions kspace
          let variant_to = HM.lookupDefault mempty arr variance
          if i `HS.member` variant_to && not (j `HS.member` variant_to) then
            Just [0,1]
          else if j `HS.member` variant_to && not (i `HS.member` variant_to) then
            Just [1,0]
          else
            Nothing

-- | The variance table keeps a mapping from a variable name
-- (something produced by a 'Stm') to the kernel thread indices
-- that name depends on.  If a variable is not present in this table,
-- that means it is bound outside the kernel (and so can be considered
-- invariant to all dimensions).
type VarianceTable = HM.HashMap VName Names

varianceInStms :: VarianceTable -> [Stm InKernel] -> VarianceTable
varianceInStms = foldl varianceInStm

varianceInStm :: VarianceTable -> Stm InKernel -> VarianceTable
varianceInStm variance bnd =
  foldl' add variance $ patternNames $ bindingPattern bnd
  where add variance' v = HM.insert v binding_variance variance'
        look variance' v = HS.insert v $ HM.lookupDefault mempty v variance'
        binding_variance = mconcat $ map (look variance) $ HS.toList (freeInStm bnd)

sufficientGroups :: MonadBinder m =>
                    [(VName, SubExp, VName, SubExp)] -> SubExp
                 -> m (SubExp, SubExp)
sufficientGroups gspace group_size = do
  groups_in_dims <- forM gspace $ \(_, gd, _, ld) ->
    letSubExp "groups_in_dim" =<< eDivRoundingUp Int32 (eSubExp gd) (eSubExp ld)
  num_groups <- letSubExp "num_groups" =<<
                foldBinOp (Mul Int32) (constant (1::Int32)) groups_in_dims
  num_threads <- letSubExp "num_threads" $
                 BasicOp $ BinOp (Mul Int32) num_groups group_size
  return (num_threads, num_groups)
