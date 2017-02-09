{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Expand allocations inside of maps when possible.
module Futhark.Optimise.CoalesceKernels
       ( coalesceKernels )
       where

-- imports -- {{{
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
{- import Data.Monoid() -}
import Data.Maybe
import Data.List

import Prelude hiding (div, quot)

import Futhark.Binder
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Substitute
--import Futhark.Util
import Futhark.Pass
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, ExtLambda, FunDef, FParam, LParam, RetType)
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun

{- import Futhark.Representation.AST.Attributes.Patterns -}

import Futhark.Optimise.CoalesceKernels.Strategy
-- }}}

coalesceKernels:: Pass ExplicitMemory ExplicitMemory
coalesceKernels = simplePass
                    "coalesce memory accesses in kernels"
                    "Coalesce Memory Access in Kernels" $
                    intraproceduralTransformation transformFunDef

type CoalesceM = StateT (Scope ExplicitMemory) (State VNameSource)

runCoalesceM m scope = modifyNameSource $ runState $ runStateT m scope 

instance MonadFreshNames CoalesceM where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance HasScope ExplicitMemory CoalesceM where
  askScope = get

instance LocalScope ExplicitMemory CoalesceM where
  localScope more_scope m = do
    old_scope <- get
    modify (`HM.union` more_scope)
    x <- m
    put old_scope
    return x

instance MonadBinder CoalesceM where
  type Lore CoalesceM = ExplicitMemory
  mkLetM pat exp = return $ Let pat () exp
  mkBodyM stms result = return $ Body () stms result
  mkLetNamesM = undefined
  addStm _ = undefined
  collectStms _ = undefined

-- AST plumbing-- {{{
transformFunDef :: MonadFreshNames m => FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
transformFunDef fundec = do
  (body', _) <- runCoalesceM m HM.empty
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
type TransposeMap = HM.HashMap VName Int -- Which index whould be new innermost

-- Transform kernels-- {{{
-- Returns a list of new bindings and the new expression
-- input lvs + result array, map over body applying ixfuns, generate strategy, apply it
transformExp :: Exp ExplicitMemory -> CoalesceM ([Stm ExplicitMemory], Exp ExplicitMemory)
transformExp (Op (Inner k@(Kernel desc lvs space ts kbody))) = do
  (transposebnds, body') <- applyTransposes kbody transpose'
  {- if summary /= [] then return . error $ "summary: " ++ pretty summary ++ "\ninterchange: " ++ pretty interchange' ++ "\ntransposes: " ++ pretty transpose' -}
                   {- else  -}
  return (transposebnds, Op (Inner (Kernel desc lvs space' ts body')))
  where
    summary                            = filterIndexes k
    thread_gids                        = map fst $ spaceDimensions space
    (Strategy interchange' transpose') = chooseStrategy $ allStrategies thread_gids summary
    space'                             = space { spaceStructure = fromMaybe (error "Illegal rotate") $ rotateSpace interchange' (spaceStructure space) }

transformExp e =
  return ([], e)

-- }}}

-- Analyse variances -- {{{

type VarianceTable = HM.HashMap VName Names
type VarianceM = State VarianceTable

-- Find all array accesses in a kernelbody and turn them into accesses.
-- Accesses carry information about which kernel variables each index is variant in.
filterIndexes :: Kernel InKernel -> [Access]
filterIndexes (Kernel _ _ space _ kbody) = evalState m init_variance
  where
    m = fmap concat . mapM checkStm $ kernelBodyStms kbody
    init_variance = HM.fromList $ map ((\v -> (v, HS.singleton v)) . fst) $ spaceDimensions space

    checkBody :: Body InKernel -> VarianceM [Access]
    checkBody = fmap concat . mapM checkStm . bodyStms

    checkStm :: Stm InKernel -> VarianceM [Access]
    checkStm (Let pat () (BasicOp b@BinOp{}))   = processBinOp (patternNames pat) b >> return []
    checkStm (Let pat () (BasicOp i@Index{}))
      | Prim Cert `notElem` patternTypes pat = processIndex i -- We filter the bound checks out.
    checkStm (Let _ () e) = foldExpM folder [] e

    folder = identityFolder { foldOnBody = \a body -> (a ++) <$> checkBody body }

    processIndex :: BasicOp InKernel -> VarianceM [Access]
    processIndex (Index _ name slice) = do
      variances <- mapM checkDim slice
      return [ Access name variances ]
      where 
        checkDim :: DimIndex SubExp -> VarianceM Names
        checkDim (DimFix e)       = checkSubExp e
        checkDim (DimSlice e1 e2) = HS.union <$> checkSubExp e1 <*> checkSubExp e2

    processBinOp :: [VName] -> BasicOp InKernel -> VarianceM ()
    processBinOp [var] (BinOp (Add _) s1 s2) = processSubExps var s1 s2
    processBinOp [var] (BinOp (Mul _) s1 s2) = processSubExps var s1 s2
    processBinOp _ _                         = return ()

    processSubExps :: VName -> SubExp -> SubExp -> VarianceM ()
    processSubExps var (Constant _) (Var variant) = modify $ add var (HS.singleton variant)
    processSubExps var (Var variant) (Constant _) = modify $ add var (HS.singleton variant)
    processSubExps var (Var v1) (Var v2)          = modify $ add var (HS.fromList [v1,v2])
    processSubExps _ _ _                          = return ()

    add :: VName -> Names -> VarianceTable -> VarianceTable
    add var vs vtab = --Grab all the variances of vs and insert them into the vtab.
      let names = foldr (\v acc -> HS.union acc $ HM.lookupDefault HS.empty v vtab) HS.empty vs
      in HM.insert var names vtab


    checkSubExp :: SubExp -> VarianceM Names
    checkSubExp (Constant _) = return HS.empty
    checkSubExp (Var name) = do
      vtab <- get
      return $ HM.lookupDefault HS.empty name vtab

-- }}}

-- Applying a strategy, generating bindings and transforming indexings
-- Applies a strategy to a kernel body. Returns new bindings for transposed arrays and a new 
-- body with all indexes modified to fit the new array shapes.
applyTransposes :: KernelBody InKernel 
                -> TransposeMap      -- arr name to index to be pushed in
                -> CoalesceM ([Stm ExplicitMemory], KernelBody InKernel)
applyTransposes kbody tmap = do
  arrmap <- sequence $ HM.mapWithKey (\k _ -> makeNames k) tmap
  bnds   <- fmap concat $ mapM makeBnds $ HM.toList arrmap
  substs <- fmap (HM.fromList . concat) . sequence $ HM.mapWithKey oneSubst arrmap
  let kbody' = kbody { kernelBodyStms = map (substituteNames substs) $ kernelBodyStms kbody }
  return (bnds, kbody')
  where
    makeNames k = do
      arr_name <- newNameFromString $ baseString k ++ "_transpose"
      mem_name <- newNameFromString $ baseString k ++ "_mem"
      return (arr_name, mem_name)

    makeBnds :: (VName, (VName, VName)) -> CoalesceM [Stm ExplicitMemory]
    makeBnds (arr, (arrname, memname)) = do
      old_type <- lookupType arr
      let i                       = fromMaybe (error $ "CoalesceKernels: Somehow " ++ pretty arr ++ " is not in tmap") $ HM.lookup arr tmap
          t@(Array ptype shape _) = rotateType i old_type

          permutation             = makePerm (length $ arrayDims t) i
          ixfun                   = IxFun.permute (IxFun.iota $ map (primExpFromSubExp int32) $ arrayDims t) permutation
          attr                    = ArrayMem ptype shape NoUniqueness memname ixfun

          copy                    = Let (Pattern [] [PatElem arrname BindVar attr]) () $ BasicOp $ Copy arr
      allocs <- makeAlloc arr memname
      return $ allocs ++ [copy]

    makeAlloc :: VName -> VName -> CoalesceM [Stm ExplicitMemory]
    makeAlloc arr memname = do
      (_, ixfun) <- lookupArraySummary arr
      sizeName   <- newNameFromString $ baseString arr ++ "_size"
      sizeBnd    <- makeSizeBnd sizeName (product $ IxFun.shape ixfun)
      let attr      = MemMem (Var sizeName) DefaultSpace
          pat       = Pattern [] [PatElem memname BindVar attr]
          allocBind = Let pat () $ Op (Alloc (Var sizeName) DefaultSpace)
      return [sizeBnd, allocBind]

    makeSizeBnd :: VName -> PrimExp VName -> CoalesceM (Stm ExplicitMemory)
    makeSizeBnd name p_exp = do
      let mkExp = return . BasicOp . SubExp . Var
      expr <- primExpToExp mkExp p_exp
      return $ Let (Pattern [] [PatElem name BindVar (Scalar int32)]) () expr

    rotateType i (Array ptype (Shape dims) u) = Array ptype (Shape (shiftIn dims i)) u
    rotateType _  _                           = error "CoalesceKernels: rotateType"

    oneSubst arr (newname, memname) = do
      (mem, _) <- lookupArraySummary arr
      return [(arr, newname), (mem, memname)]

-- Permutation functions-- {{{
-- Applies the strategy of interchanging on the kernels parallel variables.

-- Shifts an element at index i to the last spot in a list

shiftIn :: [a] -> Int -> [a]
shiftIn xs i
  | i >= length xs = error $ "CoalesceKernels: ille gal permutation, length "
                             ++ pretty (length xs)
                             ++ " and index " ++ pretty i
  | otherwise      = take i xs ++ drop (i+1) xs ++ [xs !! i]

rotateSpace :: Interchange -> SpaceStructure -> Maybe SpaceStructure 
rotateSpace (Just v, _) (FlatSpace dims) = 
  case lookup v dims of
    Just t  -> Just . FlatSpace . reverse . nub $ (v,t) : dims
    Nothing -> Nothing

rotateSpace (_, vs) (FlatSpace dims) =
  let (ins, outs) = partition ((`elem` vs) . fst) dims in Just . FlatSpace . reverse $ ins ++ outs

rotateSpace _ struct = Just struct -- I dunno yet.

{- rotate :: [VName] -> Interchange -> Maybe [VName] -}
{- rotate vs (Just v, _) = do -}
  {- i <- elemIndex v vs -}
  {- return $ shiftIn vs i -}
{- rotate vs (Nothing, os) = Just $ os ++ rest -}
  {- where rest = deleteFirstsBy (==) vs os -}

makePerm :: Int -> Int -> [Int]
makePerm len = reverse . shiftIn [0.. len - 1]

-- }  
