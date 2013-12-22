module L0C.HOTrans.LoopKernel
  ( FusedKer(..)
  , newKernel
  , inputs
  , setInputs
  , arrInputs
  , OutputTransform(..)
  , applyTransform
  , outputToInput
  , outputsToInput
  , optimizeKernel
  , optimizeSOAC
  )
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad

import qualified Data.HashSet as HS

import Data.Maybe
import Data.Loc

import L0C.L0
import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.HORepresentation.SOACNest (SOACNest)
import qualified L0C.HORepresentation.SOACNest as Nest

data OutputTransform = OTranspose Certificates Int Int
                       deriving (Show)

applyTransform :: OutputTransform -> Ident -> SrcLoc -> Exp
applyTransform (OTranspose cs k n)  = Transpose cs k n . Var

outputToInput :: OutputTransform -> SOAC.Input -> SOAC.Input
outputToInput (OTranspose cs k n) = SOAC.Transpose cs k n

outputsToInput :: [OutputTransform] -> SOAC.Input -> SOAC.Input
outputsToInput = foldr ((.) . outputToInput) id

data FusedKer = FusedKer {
    fsoac      :: SOAC
  -- ^ the SOAC expression, e.g., mapT( f(a,b), x, y )

  , outputs    :: [Ident]
  -- ^ The names bound to the outputs of the SOAC.

  , inplace    :: HS.HashSet VName
  -- ^ every kernel maintains a set of variables
  -- that alias vars used in in-place updates,
  -- such that fusion is prevented to move
  -- a use of an

  , fusedVars :: [Ident]
  -- ^ whether at least a fusion has been performed.

  , outputTransform :: [OutputTransform]
  }
                deriving (Show)

newKernel :: [Ident] -> SOAC -> FusedKer
newKernel idds soac =
  FusedKer { fsoac = soac
           , outputs = idds
           , inplace = HS.empty
           , fusedVars = []
           , outputTransform = []
           }


arrInputs :: FusedKer -> HS.HashSet Ident
arrInputs = HS.fromList . mapMaybe SOAC.inputArray . inputs

inputs :: FusedKer -> [SOAC.Input]
inputs = SOAC.inputs . fsoac

setInputs :: [SOAC.Input] -> FusedKer -> FusedKer
setInputs inps ker = ker { fsoac = inps `SOAC.setInputs` fsoac ker }

optimizeKernel :: Maybe [Ident] -> FusedKer -> Maybe FusedKer
optimizeKernel inp ker = do
  (resNest, resTrans) <- optimizeSOACNest inp startNest startTrans
  Just ker { fsoac = Nest.toSOAC resNest
           , outputTransform = resTrans
           }
  where startNest = Nest.fromSOAC $ fsoac ker
        startTrans = outputTransform ker

optimizeSOAC :: Maybe [Ident] -> SOAC -> Maybe (SOAC, [OutputTransform])
optimizeSOAC inp soac =
  first Nest.toSOAC <$> optimizeSOACNest inp (Nest.fromSOAC soac) []

optimizeSOACNest :: Maybe [Ident] -> SOACNest -> [OutputTransform]
                 -> Maybe (SOACNest, [OutputTransform])
optimizeSOACNest inp soac os = case foldr comb (False, (soac, os)) optimizations of
                             (False, _)           -> Nothing
                             (True, (soac', os')) -> Just (soac', os')
  where comb f (changed, (soac', os')) =
          case f inp soac' os of
            Nothing             -> (changed, (soac',  os'))
            Just (soac'', os'') -> (True,    (soac'', os''))

type Optimization = Maybe [Ident] -> SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])

optimizations :: [Optimization]
optimizations = [iswim, pushTranspose]

mapDepth :: SOACNest -> Int
mapDepth nest = case Nest.operation nest of
                  Nest.MapT _ _ levels _ -> length levels + 1
                  _                      -> 0

pushTranspose :: Maybe [Ident] -> SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])
pushTranspose _ nest ots
  | Just (n, k, cs, inputs') <- transposedInputs $ Nest.inputs nest,
    n+k < mapDepth nest = Just (inputs' `Nest.setInputs` nest,
                                ots ++ [OTranspose cs n k])
  where transposedInputs (SOAC.Transpose cs n k input:args) =
          foldM comb (n, k, cs, [input]) args
          where comb (n1, k1, cs1, idds) (SOAC.Transpose cs2 n2 k2 input')
                  | n1==n2, k1==k2   = Just (n1,k1,cs1++cs2,idds++[input'])
                comb _             _ = Nothing
        transposedInputs _ = Nothing
pushTranspose inpIds nest ots
  | Just inpIds' <- inpIds,
    Just (ts, inputs') <- fixupInputs inpIds' (Nest.inputs nest),
    transposeReach ts < mapDepth nest =
      let outInvTrns = uncurry (OTranspose []) . uncurry transposeInverse
      in Just (inputs' `Nest.setInputs` nest,
               ots ++ map outInvTrns (reverse ts))
pushTranspose _ _ _ = Nothing

transposeReach :: [(Int,Int)] -> Int
transposeReach = foldr (max . transDepth) 0
  where transDepth (k,n) = max k (k+n)

fixupInputs :: [Ident] -> [SOAC.Input] -> Maybe ([(Int,Int)], [SOAC.Input])
fixupInputs inpIds inps =
  case filter (not . null) $
       map (snd . SOAC.inputTransposes) $
       filter exposable inps of
    ts:_ -> do inps' <- mapM (fixupInput (transposeReach ts) ts) inps
               return (ts, inps')
    _    -> Nothing
  where exposable = maybe False (`elem` inpIds) . SOAC.inputArray

        fixupInput d ts inp
          | exposable inp = case SOAC.inputTransposes inp of
                              (inp', ts') | ts == ts' -> Just inp'
                                          | otherwise -> Nothing
          | arrayDims (SOAC.inputType inp) > d =
              Just $ transposes inp $ map (uncurry transposeInverse) ts
          | otherwise = Nothing

transposes :: SOAC.Input -> [(Int, Int)] -> SOAC.Input
transposes = foldr inverseTrans'
  where inverseTrans' (k,n) = SOAC.Transpose [] k n

iswim :: Maybe [Ident] -> SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])
iswim _ nest ots
  | Nest.ScanT cs1 (Nest.NewNest lvl nn) [] es@[_] loc1 <- Nest.operation nest,
    Nest.MapT cs2 mb [] loc2 <- nn,
    Just es' <- mapM SOAC.inputFromExp es,
    Nest.Nesting paramIds mapArrs bndIds retTypes <- lvl,
    mapArrs == map SOAC.Var paramIds =
    let toInnerAccParam idd = idd { identType = rowType $ identType idd }
        innerAccParams = map toInnerAccParam $ take (length es) paramIds
        innerArrParams = drop (length es) paramIds
        innerScan = ScanT cs2 (Nest.bodyToLambda mb)
                          (map Var innerAccParams) (map Var innerArrParams)
                          loc1
        lam = TupleLambda {
                tupleLambdaParams = map toParam $ innerAccParams ++ innerArrParams
              , tupleLambdaReturnType = retTypes
              , tupleLambdaBody =
                  LetPat (TupId (map Id bndIds) loc2) innerScan
                  (TupLit (map Var bndIds) loc2) loc2
              , tupleLambdaSrcLoc = loc2
              }
        transposeInput (SOAC.Transpose _ 0 1 inp) = inp
        transposeInput inp                        = SOAC.Transpose cs2 0 1 inp
    in Just (Nest.SOACNest
               (es' ++ map transposeInput (Nest.inputs nest))
               (Nest.MapT cs1 (Nest.Lambda lam) [] loc2),
             ots ++ [OTranspose cs2 0 1])
iswim _ _ _ = Nothing
