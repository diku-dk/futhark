{-# LANGUAGE FlexibleContexts #-}
module Futhark.Analysis.Range
       ( rangeAnalysis
       , runRangeM
       , RangeM
       , analyseExp
       , analyseLambda
       , analyseExtLambda
       , analyseBody
       , analyseStms
       )
       where

import Control.Applicative
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Data.Maybe
import Data.List

import Prelude

import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.Ranges
import Futhark.Analysis.AlgSimplify as AS

-- Entry point

-- | Perform variable range analysis on the given program, returning a
-- program with embedded range annotations.
rangeAnalysis :: (Attributes lore, CanBeRanged (Op lore)) =>
                 Prog lore -> Prog (Ranges lore)
rangeAnalysis = Prog . map analyseFun . progFunctions

-- Implementation

analyseFun :: (Attributes lore, CanBeRanged (Op lore)) =>
              FunDef lore -> FunDef (Ranges lore)
analyseFun (FunDef entry fname restype params body) =
  runRangeM $ bindFunParams params $ do
    body' <- analyseBody body
    return $ FunDef entry fname restype params body'

analyseBody :: (Attributes lore, CanBeRanged (Op lore)) =>
               Body lore
            -> RangeM (Body (Ranges lore))
analyseBody (Body lore origbnds result) =
  analyseStms origbnds $ \bnds' ->
    return $ mkRangedBody lore bnds' result

analyseStms :: (Attributes lore, CanBeRanged (Op lore)) =>
               [Stm lore]
            -> ([Stm (Ranges lore)] -> RangeM a)
            -> RangeM a
analyseStms = analyseStms' []
  where analyseStms' acc [] m =
          m $ reverse acc
        analyseStms' acc (bnd:bnds) m = do
          bnd' <- analyseStm bnd
          bindPattern (bindingPattern bnd') $
            analyseStms' (bnd':acc) bnds m

analyseStm :: (Attributes lore, CanBeRanged (Op lore)) =>
              Stm lore -> RangeM (Stm (Ranges lore))
analyseStm (Let pat lore e) = do
  e' <- analyseExp e
  pat' <- simplifyPatRanges $ addRangesToPattern pat e'
  return $ Let pat' lore e'

analyseExp :: (Attributes lore, CanBeRanged (Op lore)) =>
              Exp lore
           -> RangeM (Exp (Ranges lore))
analyseExp = mapExpM analyse
  where analyse =
          Mapper { mapOnSubExp = return
                    , mapOnCertificates = return
                    , mapOnVName = return
                    , mapOnBody = const analyseBody
                    , mapOnRetType = return
                    , mapOnFParam = return
                    , mapOnLParam = return
                    , mapOnOp = return . addOpRanges
                    }

analyseLambda :: (Attributes lore, CanBeRanged (Op lore)) =>
                 Lambda lore
              -> RangeM (Lambda (Ranges lore))
analyseLambda lam = do
  body <- analyseBody $ lambdaBody lam
  return $ lam { lambdaBody = body
               , lambdaParams = lambdaParams lam
               }

analyseExtLambda :: (Attributes lore, CanBeRanged (Op lore)) =>
                    ExtLambda lore
                 -> RangeM (ExtLambda (Ranges lore))
analyseExtLambda lam = do
  body <- analyseBody $ extLambdaBody lam
  return $ lam { extLambdaBody = body
               , extLambdaParams = extLambdaParams lam
               }

-- Monad and utility definitions

type RangeEnv = M.Map VName Range

emptyRangeEnv :: RangeEnv
emptyRangeEnv = M.empty

type RangeM = Reader RangeEnv

runRangeM :: RangeM a -> a
runRangeM = flip runReader emptyRangeEnv

bindFunParams :: Typed attr =>
                 [ParamT attr] -> RangeM a -> RangeM a
bindFunParams []             m =
  m
bindFunParams (param:params) m = do
  ranges <- rangesRep
  local bindFunParam $
    local (refineDimensionRanges ranges dims) $
    bindFunParams params m
  where bindFunParam = M.insert (paramName param) unknownRange
        dims = arrayDims $ paramType param

bindPattern :: Typed attr =>
               PatternT (Range, attr) -> RangeM a -> RangeM a
bindPattern pat m = do
  ranges <- rangesRep
  local bindPatElems $
    local (refineDimensionRanges ranges dims)
    m
  where bindPatElems env =
          foldl bindPatElem env $ patternElements pat
        bindPatElem env patElem =
          M.insert (patElemName patElem) (fst $ patElemAttr patElem) env
        dims = nub $ concatMap arrayDims $ patternTypes pat

refineDimensionRanges :: AS.RangesRep -> [SubExp]
                      -> RangeEnv -> RangeEnv
refineDimensionRanges ranges = flip $ foldl refineShape
  where refineShape env (Var dim) =
          refineRange ranges dim dimBound env
        refineShape env _ =
          env
        -- A dimension is never negative.
        dimBound :: Range
        dimBound = (Just $ ScalarBound 0,
                    Nothing)

refineRange :: AS.RangesRep -> VName -> Range -> RangeEnv
            -> RangeEnv
refineRange =
  M.insertWith . refinedRange

-- New range, old range, result range.
refinedRange :: AS.RangesRep -> Range -> Range -> Range
refinedRange ranges (new_lower, new_upper) (old_lower, old_upper) =
  (simplifyBound ranges $ refineLowerBound new_lower old_lower,
   simplifyBound ranges $ refineUpperBound new_upper old_upper)

-- New bound, old bound, result bound.
refineLowerBound :: Bound -> Bound -> Bound
refineLowerBound = flip maximumBound

-- New bound, old bound, result bound.
refineUpperBound :: Bound -> Bound -> Bound
refineUpperBound = flip minimumBound

lookupRange :: VName -> RangeM Range
lookupRange = asks . M.findWithDefault unknownRange

simplifyPatRanges :: PatternT (Range, attr)
                  -> RangeM (PatternT (Range, attr))
simplifyPatRanges (Pattern context values) =
  Pattern <$> mapM simplifyPatElemRange context <*> mapM simplifyPatElemRange values
  where simplifyPatElemRange patElem = do
          let (range, innerattr) = patElemAttr patElem
          range' <- simplifyRange range
          return $ setPatElemLore patElem (range', innerattr)

simplifyRange :: Range -> RangeM Range
simplifyRange (lower, upper) = do
  ranges <- rangesRep
  lower' <- simplifyBound ranges <$> betterLowerBound lower
  upper' <- simplifyBound ranges <$> betterUpperBound upper
  return (lower', upper')

simplifyBound :: AS.RangesRep -> Bound -> Bound
simplifyBound ranges = fmap $ simplifyKnownBound ranges

simplifyKnownBound :: AS.RangesRep -> KnownBound -> KnownBound
simplifyKnownBound ranges bound
  | Just se <- boundToScalExp bound =
    ScalarBound $ AS.simplify se ranges
simplifyKnownBound ranges (MinimumBound b1 b2) =
  MinimumBound (simplifyKnownBound ranges b1) (simplifyKnownBound ranges b2)
simplifyKnownBound ranges (MaximumBound b1 b2) =
  MaximumBound (simplifyKnownBound ranges b1) (simplifyKnownBound ranges b2)
simplifyKnownBound _ bound =
  bound

betterLowerBound :: Bound -> RangeM Bound
betterLowerBound (Just (ScalarBound (SE.Id v t))) = do
  range <- lookupRange v
  return $ Just $ case range of
    (Just lower, _) -> lower
    _               -> ScalarBound $ SE.Id v t
betterLowerBound bound =
  return bound

betterUpperBound :: Bound -> RangeM Bound
betterUpperBound (Just (ScalarBound (SE.Id v t))) = do
  range <- lookupRange v
  return $ Just $ case range of
    (_, Just upper) -> upper
    _               -> ScalarBound $ SE.Id v t
betterUpperBound bound =
  return bound

-- The algebraic simplifier requires a loop nesting level for each
-- range.  We just put a zero because I don't think it's used for
-- anything in this case.
rangesRep :: RangeM AS.RangesRep
rangesRep = M.map addLeadingZero <$> ask
  where addLeadingZero (x,y) =
          (0, boundToScalExp =<< x, boundToScalExp =<< y)
