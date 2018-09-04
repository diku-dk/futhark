{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Perform stencil optimisation.
module Futhark.Optimise.Stencils ( optimiseStencils ) where
import Debug.Trace
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Either
import qualified Data.Map.Strict as M
import Data.Semigroup ((<>))
import Data.List
import Data.Maybe

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Pass
import Futhark.Tools

import Prelude

optimiseStencils :: Pass Kernels Kernels
optimiseStencils = Pass "optimise stencils" "Optimise kernels with stencil-like memory access patterns" $
                   intraproceduralTransformation optimiseFunDef

optimiseFunDef :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
optimiseFunDef fundec = do
  body' <- modifyNameSource $ runState $
           runReaderT m (scopeOfFParams (funDefParams fundec))
  return fundec { funDefBody = body' }
  where m = optimiseBody $ funDefBody fundec

type TileM = ReaderT (Scope Kernels) (State VNameSource)

optimiseBody :: Body Kernels -> TileM (Body Kernels)
optimiseBody (Body () bnds res) =
  Body () <$> (mconcat <$> mapM optimiseStm (stmsToList bnds)) <*> pure res

optimiseStm :: Stm Kernels -> TileM (Stms Kernels)
optimiseStm (Let pat aux (Op old_kernel@(Kernel desc space ts body))) = do
  (extra_bnds, space', body') <- optimiseKernelBody space body
  let new_kernel = Kernel desc space' ts body'
  -- XXX: we should not change the type of the kernel (such as by
  -- changing the number of groups being used for a kernel that
  -- returns a result-per-group).
  if kernelType old_kernel == kernelType new_kernel
    then return $ extra_bnds <> oneStm (Let pat aux $ Op new_kernel)
    else return $ oneStm $ Let pat aux $ Op old_kernel
optimiseStm (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = \scope -> local (scope<>) . optimiseBody }

optimiseKernelBody :: KernelSpace -> KernelBody InKernel
                   -> TileM (Stms Kernels, KernelSpace, KernelBody InKernel)
optimiseKernelBody kspace (KernelBody () kstms kres) = do
  scope <- ask
  let outer_vtable = M.mapMaybeWithKey fromOuter (scopeOfKernelSpace kspace <> scope)
      vtable = primExpTable outer_vtable kstms
      gtids = map fst $ spaceDimensions kspace
      stencils = detectStencils $ findCandidateIndexing (fmap typeOf . (`M.lookup` scope))
                 gtids vtable kstms

  ((), kstms') <- trace (show stencils) $ flip runBinderT (scopeOfKernelSpace kspace <> castScope scope) $ do
    let halo_size = foldl' max 0 $ map unOffset $
                    concatMap indexingOffset $ concatMap stencilIndexings stencils
    stencil_size <- letSubExp "stencil_size" $
                    BasicOp $ BinOp (Add Int32) (spaceGroupSize kspace) (constant $ halo_size * 2)
    ctid <- newVName "ctid"
    let cspace = combineSpace [(ctid, stencil_size)]
        arrs = nub $ map stencilArray stencils
    arr_ts <- mapM lookupType arrs

    readElems <- fmap (uncurry $ flip mkBody) $ runBinder $
                 forM (zip arrs arr_ts) $ \(arr, arr_t) -> do
      offset <- letSubExp "offset" $ BasicOp $
                BinOp (Mul Int32) (Var (spaceGroupId kspace)) (spaceGroupSize kspace)
      i_offbyhalo <- letSubExp "i_offbyhalo" $ BasicOp $ BinOp (Add Int32) offset (Var ctid)
      i_oob <- letSubExp "i_oob" $ BasicOp $ BinOp (Sub Int32) i_offbyhalo (constant halo_size)
      i <- letSubExp "i" $ BasicOp $ BinOp (SMod Int32) i_oob (arraySize 0 arr_t)
      letSubExp (baseString arr <> "_elem") $ BasicOp $ Index arr $
        fullSlice arr_t [DimFix i]

    stencil_chunks <- letTupExp "stencil_chunk" $ Op $
      Combine cspace (map (Prim . elemType) arr_ts) [] readElems

    let indexes = concatMap stencilIndexings stencils
        replace pe = do
          Indexing _ orig_arr [Offset k] <- find ((==pe) . indexingPatElem) indexes
          arr <- lookup orig_arr $ zip arrs stencil_chunks
          return $ do
            new_index <- letSubExp "stencil_new_index" $ BasicOp $
                         BinOp (Add Int32) (Var $ spaceLocalId kspace) $
                         constant $ k + halo_size
            return (arr, [DimFix new_index])

    mapM_ addStm =<< replaceIndexing replace kstms

  return (mempty, kspace, KernelBody () kstms' kres)
  where fromOuter k info = case typeOf info of Prim pt -> Just $ LeafExp k pt
                                               _       -> Nothing

primExpTable :: M.Map VName (PrimExp v)
             -> Stms InKernel
             -> M.Map VName (PrimExp v)
primExpTable = foldl' bindsOne
  where bindsOne vtable stm =
          case patternNames $ stmPattern stm of
            [v] | Just pe <- primExpFromExp (`M.lookup` vtable) $ stmExp stm ->
                    M.insert v pe vtable
            _   -> vtable

newtype Offset = Offset { unOffset :: Int32 }
               deriving (Show, Eq, Ord)

data Stencil = Stencil1D { stencilArray :: VName
                         , stencilArea :: [(Offset, PatElem InKernel)]
                         }
               deriving (Show)

stencilIndexings :: Stencil -> [Indexing]
stencilIndexings (Stencil1D arr area) = do
  (offset, pe) <- area
  return $ Indexing pe arr [offset]

data Indexing = Indexing { indexingPatElem :: PatElem InKernel
                         , indexingArray :: VName
                         , indexingOffset :: [Offset]
                         }
              deriving (Show)

detectStencils :: [Indexing] -> [Stencil]
detectStencils [] = []
detectStencils (indexing1 : indexings)
  | length (indexingOffset indexing1) == 1,
    (area, indexings') <-
      partitionEithers $ map partOfStencil $ indexing1 : indexings,
    length area > 1 =
      Stencil1D (indexingArray indexing1) area : detectStencils indexings'
  where partOfStencil indexing2
          | indexingArray indexing2 == indexingArray indexing1,
            [k] <- indexingOffset indexing2 =
              Left (k, indexingPatElem indexing2)
          | otherwise =
              Right indexing2
detectStencils (_ : indexings) = detectStencils indexings

isOffset :: SubExp -> VName -> PrimExp VName -> Maybe Offset
isOffset d tid e
  | Just k <- tidMod e = Just k
  | Just k <- tidOffset e = Just k
  | isTid e = Just $ Offset 0
  | otherwise = Nothing
  where isTid (LeafExp x _) | x == tid = True
        isTid _ = False

        tidOffset (BinOpExp (Add Int32)
                   (ValueExp (IntValue (Int32Value k))) x)
          | isTid x, k `elem` [-2, -1..2] = Just $ Offset k
        tidOffset _ = Nothing

        tidMod (BinOpExp (SMod Int32) x y)
          | y == primExpFromSubExp int32 d = tidOffset x
        tidMod _ = Nothing

findCandidateIndexing :: (VName -> Maybe Type)
                      -> [VName]
                      -> M.Map VName (PrimExp VName)
                      -> Stms InKernel
                      -> [Indexing]
findCandidateIndexing kernelInvariant gtids orig_vtable = concatMap (onStm orig_vtable) . stmsToList
  where onStm vtable (Let (Pattern [] [pe]) _ (BasicOp (Index arr slice)))
          | Just arr_t <- kernelInvariant arr,
            length slice == length gtids,
            Just is <- mapM onIndex $ zip3 (arrayDims arr_t) gtids slice =
              [Indexing pe arr is]
          where onIndex (d, tid, e) =
                  dimFix e >>=
                  primExpFromSubExpM (`M.lookup` vtable) >>=
                  isOffset d tid
        onStm vtable stm = execState (walkExpM (walker vtable) $ stmExp stm) mempty
        walker vtable = identityWalker { walkOnBody = modify . (<>) . onBody vtable }
        onBody vtable body =
          let vtable' = primExpTable vtable $ bodyStms body
          in concatMap (onStm vtable') $ stmsToList $ bodyStms body

replaceIndexing :: Monad m =>
                   (PatElem InKernel -> Maybe (m (VName, Slice SubExp))) -> Stms InKernel
                -> m (Stms InKernel)
replaceIndexing f = mapM onStm
  where onStm (Let (Pattern [] [pe]) aux (BasicOp Index{}))
          | Just m <- f pe = do
              (arr, slice) <- m
              return $ Let (Pattern [] [pe]) aux $ BasicOp $ Index arr slice
        onStm (Let pat aux e) = Let pat aux <$> mapExpM mapper e
        mapper = identityMapper { mapOnBody = \_ (Body aux stms res) ->
                                    Body aux <$> mapM onStm stms <*> pure res
                                }
