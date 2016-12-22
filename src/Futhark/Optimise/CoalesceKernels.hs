{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Expand allocations inside of maps when possible.
module Futhark.Optimise.CoalesceKernels
       ( coalesceKernels )
       where

-- imports -- {{{
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Data.Monoid()
import Data.Maybe
import Data.List

import Prelude hiding (div, quot)

import Futhark.Binder
import Futhark.MonadFreshNames
import Futhark.Tools
--import Futhark.Util
import Futhark.Pass
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, ExtLambda, FunDef, FParam, LParam, RetType)
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun

import Futhark.Optimise.CoalesceKernels.Strategy
-- }}}

coalesceKernels:: Pass ExplicitMemory ExplicitMemory
coalesceKernels = simplePass
                    "coalesce memory accesses in kernels"
                    "Coalesce Memory Access in Kernels" $
                    intraproceduralTransformation transformFunDef

type CoalesceM = Binder ExplicitMemory

-- AST plumbing-- {{{
transformFunDef :: MonadFreshNames m => FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
transformFunDef fundec = do
  (body', _) <- modifyNameSource $ runState (runBinderT m HM.empty)
  return fundec { funDefBody = body' }
  where m = inScopeOf fundec $ 
            transformBody $ funDefBody fundec


transformBody :: Body ExplicitMemory -> CoalesceM (Body ExplicitMemory)
transformBody (Body () bnds res) = do
  bnds' <- concat <$> mapM transformStm bnds
  return $ Body () bnds' res

transformStm :: Stm ExplicitMemory -> CoalesceM [Stm ExplicitMemory]
transformStm (Let pat () e) = do
  (bnds, e') <- transformExp =<< mapExpM transform e
  return $ bnds ++ [Let pat () e']
  where transform = identityMapper { mapOnBody = \scope -> localScope scope . transformBody }


-- }}}

type Interchange = (Maybe VName, [VName]) -- (In, Out)
type TransposeMap = M.Map VName Int -- Which index whould be new innermost

-- Transform kernels-- {{{
-- Returns a list of new bindings and the new expression
transformExp :: Exp ExplicitMemory -> CoalesceM ([Stm ExplicitMemory], Exp ExplicitMemory)
transformExp (Op (Inner (Kernel desc lvs space ts kbody))) = do
  -- Need environment to see IxFuns
  -- input lvs + result array, map over body applying ixfuns, generate strategy, apply it
  (transposebnds, body') <- applyTransposes kbody transposes
  return (transposebnds, Op (Inner (Kernel desc lvs' space ts body')))
  where 
    initial_variance                   = HM.map HS.empty $ scopeOfKernelSpace space
    summary                            = filterIndexes kbody
    vtable                             = generateVarianceTable initial_variance kbody
    (Strategy interchange' transpose') = chooseStrategy vtable lvs summary
    lvs'                               = fromMaybe (error "Illegal rotate") $ rotate lvs interchange

transformExp e =
  return ([], e)

-- }}}

-- Analyse variances -- {{{
generateVarianceTable :: KernelBody InKernel -> VarianceTable
generateVarianceTable = varianceInStms init_variance . kernelBodyStms
  where init_variance = undefined

varianceInStms :: VarianceTable -> [Stm InKernel] -> VarianceTable
varianceInStms = foldl varianceInStm

varianceInStm :: VarianceTable -> Stm InKernel -> VarianceTable
varianceInStm variance bnd =
  foldl' add variance $ patternNames $ bindingPattern bnd
  where
    add :: VarianceTable -> VName -> VarianceTable
    add variance' v = HM.insert v binding_variance variance'

    look :: VarianceTable -> VName -> HS.HashSet VName
    look variance' v = HS.insert v $ HM.lookupDefault mempty v variance'

    binding_variance :: HS.HashSet VName
    binding_variance = mconcat $ map (look variance) $ HS.toList (freeInStm bnd)

-- Find all array accesses in a kernelbody
filterIndexes :: KernelBody InKernel -> [Stm InKernel]
filterIndexes kbody = concat <$> map checkStm $ kernelBodyStms kbody
  where 
    checkBody :: [Stm InKernel] -> Body InKernel -> [Stm InKernel]
    checkBody as body = (as ++) . concat <$> map checkStm $ bodyStms body

    checkStm :: Stm InKernel -> [Stm InKernel]
    checkStm i@(Let _ () (BasicOp Index{})) = [i]
    checkStm (Let _ () e) = foldExp folder [] e

    folder = identityFolder { foldOnBody = \xs body -> Identity $ checkBody xs body }
-- }}}

-- Applying a strategy, generating bindings and transforming indexings-- {{{
-- Applies a strategy to a kernel body. Returns new bindings for transposed arrays and a new 
-- body with all indexes modified to fit the new array shapes.
applyTransposes :: KernelBody InKernel 
                -> TransposeMap      -- arr name to index to be pushed in
                -> CoalesceM ([Stm ExplicitMemory], KernelBody InKernel)
applyTransposes kbody tmap = do
  arrmap <- sequence $ M.mapWithKey (\k _ -> newName k) tmap -- Map VName VName
  bnds   <- mapM makeBnd $ M.toList arrmap
  kbody' <- transformIndexes arrmap tmap kbody
  return (bnds, kbody')

  where
    makeBnd (arr, new_name) = do
      old_type         <- lookupType arr

      let i            = fromMaybe 
                          (error $ "CoalesceKernels: Somehow " ++ pretty arr ++ " is not in tmap") 
                          $ M.lookup arr tmap

          t@(Array ptype shape _) = rotateType i old_type
            --Is this correct?
          ixfun        = IxFun.iota $ map (primExpFromSubExp int32) $ arrayDims t 
          arr_attr     = ArrayMem ptype shape NoUniqueness new_name ixfun 
          pat          = Pattern [] [PatElem new_name BindVar arr_attr]
          permutation  = makePerm (length $ arrayDims t) i

      return $ Let pat () (BasicOp (Rearrange [] permutation arr))

    rotateType i (Array ptype (Shape dims) u) = Array ptype (Shape (shiftIn dims i)) u
    rotateType _  notarr                      = error $ "CoalesceKernels: " ++ 
                                                pretty notarr ++ 
                                                "is not an array."


transformIndexes :: M.Map VName VName
                 -> M.Map VName Int
                 -> KernelBody InKernel 
                 -> CoalesceM (KernelBody InKernel)
transformIndexes = undefined

-- Shifts an element at index i to the last spot in a list
shiftIn :: [a] -> Int -> [a]
shiftIn xs i
  | i >= length xs = error $ "CoalesceKernels: illegal permutation, length " 
                             ++ pretty (length xs)
                             ++ " and index " ++ pretty i
  | otherwise      = take i xs ++ drop (i+1) xs ++ [xs !! i]
  
-- }}}

-- Permutation functions-- {{{
-- Applies the strategy of interchanging on the kernels parallel variables.
rotate :: [VName] -> Interchange -> Maybe [VName]
rotate vs (Just v, _) = do
  i <- elemIndex v vs
  return $ shiftIn vs i
rotate vs (Nothing, os) = Just $ os ++ rest
  where rest = deleteFirstsBy (==) vs os

makePerm :: Int -> Int -> [Int]
makePerm len = shiftIn [0.. len - 1]

-- }}}
