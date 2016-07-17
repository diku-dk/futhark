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
import Futhark.Pass.ExtractKernels.BlockedKernel (perThread) -- FIXME

import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Pass
import Futhark.Tools

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
optimiseBinding (Let pat () (Op (Kernel cs size ts space body))) = do
  (extra_bnds, body') <- tileInKernelBody size space body
  return $ extra_bnds ++ [Let pat () $ Op $ Kernel cs size ts space body']
optimiseBinding (Let pat () e) =
  pure <$> (Let pat () <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = optimiseBody }

tileInKernelBody :: (SubExp,SubExp,SubExp)
                 -> KernelSpace -> KernelBody Kernels
                 -> TileM ([Binding], KernelBody Kernels)
tileInKernelBody (_, group_size, _) kspace (KernelBody kstms kres) = do
  (extra_bndss, kstms') <- unzip <$> mapM tileInKernelStatement kstms
  return (concat extra_bndss,
          KernelBody kstms' kres)
  where initial_variance = HM.map mempty $ scopeOfKernelSpace kspace
        variance = varianceInKernelStms initial_variance kstms

        tileInKernelStatement (GroupStream pes w _ lam accs arrs)
          | chunk_size <- Var $ groupStreamChunkSize lam,
            arr_chunk_params <- groupStreamArrParams lam,

            Just mk_tilings <-
              zipWithM (isTilingCandidate kspace variance chunk_size)
              arrs arr_chunk_params = do

          -- We assume that the group size is tile_size*tile_size.
          let flat_local_id = spaceLocalId kspace
              dims = map snd $ spaceDimensions kspace

          -- XXX: empty scope here.  Hopefully OK.
          (tile_size, tile_size_bnds) <- flip runBinderT mempty $ do
            group_size_float <-
              letSubExp "group_size_float" $ PrimOp $ ConvOp (SIToFP Int32 Float32) group_size
            tile_size_float <-
              letSubExp "tile_size_float" $
              Apply (nameFromString "sqrt32") [(group_size_float,Observe)] $
              primRetType $ FloatType Float32
            letSubExp "tile_size" $ PrimOp $ ConvOp (FPToSI Float32 Int32) tile_size_float

          (local_ids, local_id_bnds) <- fmap unzip $ forM (spaceDimensions kspace) $ \(gtid,_) -> do
            local_id <- newVName "ltid"
            return (local_id,
                    mkLet' [] [Ident local_id $ Prim int32] $ PrimOp $
                    BinOp (SRem Int32) (Var gtid) tile_size)
          (aux_pes, aux_body) <- perThread local_id_bnds
          let aux_kstm = Thread aux_pes AllThreads aux_body

          (arr_chunk_params', tile_kstms) <-
            fmap unzip $ forM mk_tilings $ \mk_tiling ->
              mk_tiling tile_size local_ids

          let KernelBody lam_kstms lam_res = groupStreamLambdaBody lam
              lam_kstms' = aux_kstm : concat tile_kstms ++ lam_kstms
              lam' = lam { groupStreamLambdaBody = KernelBody lam_kstms' lam_res
                         , groupStreamArrParams = arr_chunk_params'
                         }

          return (tile_size_bnds,
                  GroupStream pes w tile_size lam' accs arrs)

        tileInKernelStatement stm =
          return ([], stm)

isTilingCandidate :: MonadFreshNames m =>
                     KernelSpace -> VarianceTable -> SubExp -> VName -> LParam
                  -> Maybe (SubExp -> [VName] -> m (LParam, [KernelStm Kernels]))
isTilingCandidate kspace variance block_size arr block_param = do
  permute <- invariantToAtLeastOneDimension

  Just $ \tile_size local_is -> do
    let [variant_i, invariant_i] = permute local_is
    outer_block_param <- do
      name <- newVName $ baseString (paramName block_param) ++ "_outer"
      return block_param { paramName = name }

    read_elem_bnd <- do
      name <- newVName $ baseString (paramName outer_block_param) ++ "_elem"
      return $
        mkLet' [] [Ident name $ rowType $ paramType outer_block_param] $
        PrimOp $ Index [] (paramName outer_block_param) [Var invariant_i]

    (block_elem_pes, read_block_body) <- perThread [read_elem_bnd]
    let limit = ThreadsPerGroup [(variant_i,block_size),(invariant_i,block_size)]
        read_block_kstm =
          Thread block_elem_pes limit read_block_body

        block_size_2d = Shape [block_size, block_size]
        block_cspace = [(variant_i,block_size),(invariant_i,block_size)]

    block_name_2d <- newVName $ baseString (paramName block_param) ++ "_2d"
    let block_pe =
          PatElem block_name_2d BindVar $
          rowType (paramType outer_block_param) `arrayOfShape` block_size_2d
        write_block_stms =
          [ Combine block_pe block_cspace $
            Var $ patElemName block_elem_pe
          | block_elem_pe <- block_elem_pes ]

    (_, index_block_body) <- perThread [mkLet' [] [paramIdent block_param] $
                                        PrimOp $ Index [] block_name_2d [Var variant_i]]
    let index_block_kstm = Thread [PatElem (paramName block_param) BindVar $
                                    paramType block_param]
                           ThreadsInSpace index_block_body

    return (outer_block_param, read_block_kstm : write_block_stms ++ [index_block_kstm])

  where invariantToAtLeastOneDimension :: Maybe ([a] -> [a])
        invariantToAtLeastOneDimension = do
          [(i,iw),(j,jw)] <- Just $ spaceDimensions kspace
          let variant_to = HM.lookupDefault mempty arr variance
          if i `HS.member` variant_to && not (j `HS.member` variant_to) then
            Just id
          else if j `HS.member` variant_to && not (i `HS.member` variant_to) then
            Just reverse
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
  where extend variance (Thread pes _ body) =
          let new_variance = HM.fromList (zip (map patElemName pes) $
                                          varianceInBody variance body)
          in variance <> new_variance

        extend variance stm =
          let free_variant = mconcat $
                             map (flip (HM.lookupDefault mempty) variance) $
                             HS.toList $ freeIn stm
              new_variance = HM.map (const free_variant) (scopeOf stm)
          in variance <> new_variance

varianceInBody :: VarianceTable -> Body -> [Names]
varianceInBody kvariance (Body _ bnds res) =
  let variance = foldl inspect mempty bnds
  in map (resultVariance variance) res
  where known = HS.fromList $ HM.keys kvariance

        inspect variance bnd =
          foldl' add variance $ patternNames $ bindingPattern bnd
          where add variance' v = HM.insert v (freeInBinding bnd) variance'

        resultVariance _ Constant{}     = mempty
        resultVariance variance (Var v) = HM.lookupDefault mempty v variance
                                          `HS.intersection` known
