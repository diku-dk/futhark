module Futhark.Analysis.Range
       ( rangeAnalysis
       )
       where

import Control.Applicative
import qualified Data.HashMap.Lazy as HM
import Control.Monad.Reader
import Data.Maybe
import Data.List

import Prelude

import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.AST.Lore (Lore)
import qualified Futhark.Representation.AST as In
import qualified Futhark.Representation.Ranges as Out
import qualified Futhark.Analysis.AlgSimplify as AS

-- Entry point

-- | Perform variable range analysis on the given program, returning a
-- program with embedded range annotations.
rangeAnalysis :: Lore lore => In.Prog lore -> Out.Prog lore
rangeAnalysis = Out.Prog . map analyseFun . In.progFunctions

-- Implementation

analyseFun :: Lore lore => In.FunDec lore -> Out.FunDec lore
analyseFun (In.FunDec fname restype params body) =
  runRangeM $ bindFunParams params $ do
    body' <- analyseBody body
    return $ Out.FunDec fname restype params body'

analyseBody :: Lore lore =>
               In.Body lore
            -> RangeM (Out.Body lore)
analyseBody (In.Body lore origbnds result) =
  analyseBindings origbnds $ \bnds' ->
    return $ Out.mkRangedBody lore bnds' result

analyseBindings :: Lore lore =>
                   [In.Binding lore]
                -> ([Out.Binding lore] -> RangeM a)
                -> RangeM a
analyseBindings = analyseBindings' []
  where analyseBindings' acc [] m =
          m $ reverse acc
        analyseBindings' acc (bnd:bnds) m = do
          bnd' <- analyseBinding bnd
          bindPattern (Out.bindingPattern bnd') $
            analyseBindings' (bnd':acc) bnds m

analyseBinding :: Lore lore =>
                  In.Binding lore
               -> RangeM (Out.Binding lore)
analyseBinding (In.Let pat lore e) = do
  e' <- analyseExp e
  pat' <- simplifyPatRanges $ Out.addRangesToPattern pat e'
  return $ Out.Let pat' lore e'

analyseExp :: Lore lore =>
              In.Exp lore
           -> RangeM (Out.Exp lore)
analyseExp (Out.LoopOp (In.Map cs lam args)) =
  Out.LoopOp <$>
  (Out.Map cs <$> analyseLambda lam <*> pure args)
analyseExp (Out.LoopOp (In.ConcatMap cs lam args)) =
  Out.LoopOp <$>
  (Out.ConcatMap cs <$> analyseLambda lam <*> pure args)
analyseExp (Out.LoopOp (In.Reduce cs lam input)) =
  Out.LoopOp <$>
  (Out.Reduce cs <$> analyseLambda lam <*> pure input)
analyseExp (Out.LoopOp (In.Scan cs lam input)) =
  Out.LoopOp <$>
  (Out.Scan cs <$> analyseLambda lam <*> pure input)
analyseExp (Out.LoopOp (In.Redomap cs outerlam innerlam acc arr)) =
  Out.LoopOp <$>
  (Out.Redomap cs <$>
   analyseLambda outerlam <*>
   analyseLambda innerlam <*>
   pure acc <*> pure arr)
analyseExp (Out.LoopOp (In.Stream cs form lam arr ii)) =
  Out.LoopOp <$>
  (Out.Stream cs <$> analyseStreamForm form <*> analyseExtLambda lam <*>
                     pure arr <*> pure ii)
  where analyseStreamForm (In.MapLike    o  ) = return $ Out.MapLike o
        analyseStreamForm (In.Sequential acc) = return $ Out.Sequential acc
        analyseStreamForm (In.RedLike o lam0 acc) = do
            lam0' <- analyseLambda lam0
            return $ Out.RedLike o lam0' acc
analyseExp e = Out.mapExpM analyse e
  where analyse =
          Out.Mapper { Out.mapOnSubExp = return
                     , Out.mapOnCertificates = return
                     , Out.mapOnVName = return
                     , Out.mapOnBody = analyseBody
                     , Out.mapOnLambda = error "Improperly handled lambda in alias analysis"
                     , Out.mapOnExtLambda = error "Improperly handled existential lambda in alias analysis"
                     , Out.mapOnRetType = return
                     , Out.mapOnFParam = return
                     }

analyseLambda :: Lore lore =>
                 In.Lambda lore
              -> RangeM (Out.Lambda lore)
analyseLambda lam = do
  body <- analyseBody $ In.lambdaBody lam
  return $ lam { Out.lambdaBody = body
               , Out.lambdaParams = In.lambdaParams lam
               }

analyseExtLambda :: Lore lore =>
                    In.ExtLambda lore
                 -> RangeM (Out.ExtLambda lore)
analyseExtLambda lam = do
  body <- analyseBody $ In.extLambdaBody lam
  return $ lam { Out.extLambdaBody = body
               , Out.extLambdaParams = In.extLambdaParams lam
               }

-- Monad and utility definitions

type RangeEnv = HM.HashMap Out.VName Out.Range

emptyRangeEnv :: RangeEnv
emptyRangeEnv = HM.empty

type RangeM = Reader RangeEnv

runRangeM :: RangeM a -> a
runRangeM = flip runReader emptyRangeEnv

bindFunParams :: [Out.ParamT attr] -> RangeM a -> RangeM a
bindFunParams []             m =
  m
bindFunParams (param:params) m = do
  ranges <- rangesRep
  local bindFunParam $
    local (refineDimensionRanges ranges dims) $
    bindFunParams params m
  where bindFunParam = HM.insert (In.paramName param) Out.unknownRange
        dims = In.arrayDims $ In.paramType param

bindPattern :: Out.Pattern lore -> RangeM a -> RangeM a
bindPattern pat m = do
  ranges <- rangesRep
  local bindPatElems $
    local (refineDimensionRanges ranges dims)
    m
  where bindPatElems env =
          foldl bindPatElem env $ Out.patternElements pat
        bindPatElem env patElem =
          HM.insert (Out.patElemName patElem) (fst $ Out.patElemLore patElem) env
        dims = nub $ concatMap Out.arrayDims $ Out.patternTypes pat

refineDimensionRanges :: AS.RangesRep -> [Out.SubExp]
                      -> RangeEnv -> RangeEnv
refineDimensionRanges ranges = flip $ foldl refineShape
  where refineShape env (In.Var dim) =
          refineRange ranges dim dimBound env
        refineShape env _ =
          env
        -- A dimension is never negative.
        dimBound :: Out.Range
        dimBound = (Just $ Out.ScalarBound $ SE.Val $ In.IntVal 0,
                    Nothing)

refineRange :: AS.RangesRep -> Out.VName -> Out.Range -> RangeEnv
            -> RangeEnv
refineRange =
  HM.insertWith . refinedRange

-- New range, old range, result range.
refinedRange :: AS.RangesRep -> Out.Range -> Out.Range -> Out.Range
refinedRange ranges (new_lower, new_upper) (old_lower, old_upper) =
  (simplifyBound ranges $ refineLowerBound new_lower old_lower,
   simplifyBound ranges $ refineUpperBound new_upper old_upper)

-- New bound, old bound, result bound.
refineLowerBound :: Out.Bound -> Out.Bound -> Out.Bound
refineLowerBound = flip Out.maximumBound

-- New bound, old bound, result bound.
refineUpperBound :: Out.Bound -> Out.Bound -> Out.Bound
refineUpperBound = flip Out.minimumBound

lookupRange :: Out.VName -> RangeM Out.Range
lookupRange = liftM (fromMaybe Out.unknownRange) . asks . HM.lookup

simplifyPatRanges :: Out.Pattern lore
                  -> RangeM (Out.Pattern lore)
simplifyPatRanges (Out.Pattern context values) =
  Out.Pattern <$> mapM simplifyPatElemRange context <*> mapM simplifyPatElemRange values
  where simplifyPatElemRange patElem = do
          let (range, innerattr) = Out.patElemLore patElem
          range' <- simplifyRange range
          return $ Out.setPatElemLore patElem (range', innerattr)

simplifyRange :: Out.Range -> RangeM Out.Range
simplifyRange (lower, upper) = do
  ranges <- rangesRep
  lower' <- simplifyBound ranges <$> betterLowerBound lower
  upper' <- simplifyBound ranges <$> betterUpperBound upper
  return (lower', upper')

simplifyBound :: AS.RangesRep -> Out.Bound -> Out.Bound
simplifyBound ranges = liftM $ simplifyKnownBound ranges

simplifyKnownBound :: AS.RangesRep -> Out.KnownBound -> Out.KnownBound
simplifyKnownBound ranges bound
  | Just se <- Out.boundToScalExp bound,
    Right se' <- AS.simplify se ranges =
    Out.ScalarBound se'
simplifyKnownBound ranges (Out.MinimumBound b1 b2) =
  Out.MinimumBound (simplifyKnownBound ranges b1) (simplifyKnownBound ranges b2)
simplifyKnownBound ranges (Out.MaximumBound b1 b2) =
  Out.MaximumBound (simplifyKnownBound ranges b1) (simplifyKnownBound ranges b2)
simplifyKnownBound _ bound =
  bound

betterLowerBound :: Out.Bound -> RangeM Out.Bound
betterLowerBound (Just (Out.ScalarBound (SE.Id v t))) = do
  range <- lookupRange v
  return $ Just $ case range of
    (Just lower, _) -> lower
    _               -> Out.ScalarBound $ SE.Id v t
betterLowerBound bound =
  return bound

betterUpperBound :: Out.Bound -> RangeM Out.Bound
betterUpperBound (Just (Out.ScalarBound (SE.Id v t))) = do
  range <- lookupRange v
  return $ Just $ case range of
    (_, Just upper) -> upper
    _               -> Out.ScalarBound $ SE.Id v t
betterUpperBound bound =
  return bound

-- The algebraic simplifier requires a loop nesting level for each
-- range.  We just put a zero because I don't think it's used for
-- anything in this case.
rangesRep :: RangeM AS.RangesRep
rangesRep = HM.map addLeadingZero <$> ask
  where addLeadingZero (x,y) =
          (0, Out.boundToScalExp =<< x, Out.boundToScalExp =<< y)
