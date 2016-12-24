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
transformExp (Op (Inner k@(Kernel desc lvs space ts kbody))) = do
  -- Need environment to see IxFuns
  -- input lvs + result array, map over body applying ixfuns, generate strategy, apply it
  (transposebnds, body') <- applyTransposes kbody transpose'
  return (transposebnds, Op (Inner (Kernel desc lvs' space ts body')))
  where 
    initial_variance = HM.map (const HS.empty) $ scopeOfKernelSpace space
    summary          = filterIndexes k
    (Strategy interchange' transpose') = chooseStrategy lvs summary
    lvs'                               = fromMaybe (error "Illegal rotate") $ rotate lvs interchange'

transformExp e =
  return ([], e)

-- }}}

-- Analyse variances -- {{{

type VarianceTable = HM.HashMap VName Names
type VarianceM = State VarianceTable

-- Find all array accesses in a kernelbody and turn them into accesses.
-- Accesses carry information about which kernel variables each index is variant in.
-- The access representation might have to be updated in order to carry more sophisticated
-- information, e.g. stride sizes and more complex variance dependencies.
filterIndexes :: Kernel InKernel -> [Access]
filterIndexes (Kernel _ lvs _ _ kbody) = evalState m init_variance
  where
    m = fmap concat . mapM checkStm $ kernelBodyStms kbody
    init_variance = HM.fromList $ map (\v -> (v, HS.singleton v)) lvs

    checkBody :: Body InKernel -> VarianceM [Access]
    checkBody body = fmap concat . mapM checkStm $ bodyStms body

    checkStm :: Stm InKernel -> VarianceM [Access]
    checkStm (Let pat () (BasicOp b@BinOp{})) = processBinOp (patternNames pat) b >> return []
    checkStm (Let _ () (BasicOp i@Index{})) = processIndex i
    checkStm (Let _ () e) = foldExpM folder [] e

    folder = identityFolder { foldOnBody = \a body -> (a ++) <$> checkBody body }

    processIndex :: BasicOp InKernel -> VarianceM [Access]
    processIndex (Index cs name slice) = do
      variances <- mapM checkDim slice
      return [ Access name variances ]
    processIndex _ = error "CoalesceKernels: Index not passed to processIndex"

    processBinOp :: [VName] -> BasicOp InKernel -> VarianceM ()
    processBinOp [var] (BinOp (Add _) s1 s2) = processSubExps var s1 s2
    processBinOp [var] (BinOp (Mul _) s1 s2) = processSubExps var s1 s2

    processSubExps :: VName -> SubExp -> SubExp -> VarianceM ()
    processSubExps var (Constant _) (Var variant) = modify $ add var (HS.singleton variant)
    processSubExps var (Var variant) (Constant _) = modify $ add var (HS.singleton variant)
    processSubExps var (Var v1) (Var v2) = modify $ add var (HS.fromList [v1,v2])
    processSubExps _ _ _ = return ()

    add :: VName -> Names -> VarianceTable -> VarianceTable
    add var vs vtab = --Grab all the variances of vs and insert them into the vtab.
      let names = foldr (\v acc -> HS.union acc $ HM.lookupDefault HS.empty v vtab) HS.empty vs
      in HM.insert var names vtab

    checkDim :: DimIndex SubExp -> VarianceM Names
    checkDim (DimFix e) = checkSubExp e
    checkDim (DimSlice e1 e2) = HS.union <$> checkSubExp e1 <*> checkSubExp e2

    checkSubExp :: SubExp -> VarianceM Names
    checkSubExp (Constant _) = return HS.empty
    checkSubExp (Var name) = do
      vtab <- get
      return $ HM.lookupDefault HS.empty name vtab

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
  let kbody' = transformIndexes arrmap tmap kbody
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
                 -> KernelBody InKernel 
transformIndexes arrmap tmap kbody = kbody { kernelBodyStms = map transformStm $ kernelBodyStms kbody }
  where
    transformBody :: Body InKernel -> Body InKernel
    transformBody body = body { bodyStms = map transformStm $ bodyStms body }
    
    transformStm :: Stm InKernel -> Stm InKernel
    transformStm s@(Let pat () (BasicOp (Index cs name slice))) =
      case M.lookup name arrmap of
        Just name' -> Let pat () (BasicOp (Index cs name' slice'))
        Nothing    -> s
      where slice' = case M.lookup name tmap of
                       Just i -> shiftIn slice i
                       Nothing -> error $ "CoalesceKernels: " ++ pretty name ++ " is in arrmap but not in tmap"
    transformStm (Let pat () e) = Let pat () $ mapExp transform_mapper e

    transform_mapper :: Mapper InKernel InKernel Identity
    transform_mapper = identityMapper { mapOnBody = const $ return . transformBody }


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
