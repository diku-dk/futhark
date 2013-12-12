module L0C.HOTrans.LoopKernel
  ( FusedKer(..)
  , OutputTransform(..)
  , applyTransform
  , optimizeKernel
  )
  where

import Control.Monad

import qualified Data.HashSet as HS

import Data.Maybe
import Data.Loc

import L0C.L0
import L0C.HOTrans.SOAC (SOAC)
import qualified L0C.HOTrans.SOAC as SOAC
import L0C.HOTrans.SOACNest (SOACNest)
import qualified L0C.HOTrans.SOACNest as Nest

data OutputTransform = OTranspose Certificates Int Int
                       deriving (Show)

applyTransform :: OutputTransform -> Ident -> SrcLoc -> Exp
applyTransform (OTranspose cs k n)  = Transpose cs k n . Var

data FusedKer = FusedKer {
    fsoac      :: (TupIdent, SOAC)
  -- ^ the fused SOAC statement, e.g.,
  -- (z,w) = map2( f(a,b), x, y )

  , inputs     :: HS.HashSet Ident
  -- ^ the input arrays used in the `soac'
  -- stmt, i.e., `x', `y'.

  , inplace    :: HS.HashSet VName
  -- ^ every kernel maintains a set of variables
  -- that alias vars used in in-place updates,
  -- such that fusion is prevented to move
  -- a use of an

  , fusedVars :: [Ident]
  -- ^ whether at least a fusion has been performed.

  , outputTransform :: [OutputTransform]
  }

optimizeKernel :: FusedKer -> FusedKer
optimizeKernel ker = ker { fsoac = (fst $ fsoac ker, Nest.toSOAC resNest)
                         , outputTransform = resTrans
                         }
  where (resNest, resTrans) =
          foldr ((.) . tryOptim) id optimizations (startNest, startTrans)
        startNest = Nest.fromSOAC $ snd $ fsoac ker
        startTrans = outputTransform ker
        tryOptim f x = fromMaybe x $ uncurry f x

type Optimization = SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])

optimizations :: [Optimization]
optimizations = [iswim, pushTranspose]

pushTranspose :: SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])
pushTranspose nest ots = do
  (n, k, cs, inputs') <- transposedInputs $ Nest.inputs nest
  if n+k < mapDepth then
    Just (inputs' `Nest.setInputs` nest,
          ots ++ [OTranspose cs n k])
  else Nothing
  where transposedInputs (SOAC.Transpose cs n k input:args) =
          foldM comb (n, k, cs, [input]) args
          where comb (n1, k1, cs1, idds) (SOAC.Transpose cs2 n2 k2 input')
                  | n1==n2, k1==k2         = Just (n1,k1,cs1++cs2,idds++[input'])
                comb _                   _ = Nothing
        transposedInputs _ = Nothing
        mapDepth = case Nest.operation nest of
                     Nest.Map2 _ _ levels _ -> length levels + 1
                     _                      -> 0

iswim :: SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])
iswim nest ots
  | Nest.Scan2 cs1 (Nest.NewNest lvl nn) [] es@[_] loc1 <- Nest.operation nest,
    Nest.Map2 cs2 mb [] loc2 <- nn,
    Just es' <- mapM SOAC.inputFromExp es =
    let (paramIds, bndIds, retTypes) = lvl
        toInnerAccParam idd = idd { identType = rowType $ identType idd }
        innerAccParams = map toInnerAccParam $ take (length es) paramIds
        innerArrParams = drop (length es) paramIds
        innerScan = Scan2 cs2 (Nest.bodyToLambda mb)
                          (map Var innerAccParams) (map Var innerArrParams)
                          (map (rowType . identType) innerArrParams) loc1
        lam = TupleLambda {
                tupleLambdaParams = map toParam $ innerAccParams ++ innerArrParams
              , tupleLambdaReturnType = retTypes
              , tupleLambdaBody =
                  LetPat (TupId (map Id bndIds) loc2) innerScan
                  (TupLit (map Var bndIds) loc2) loc2
              , tupleLambdaSrcLoc = loc2
              }
    in Just (Nest.SOACNest
               (es' ++ [ SOAC.Transpose cs2 0 1 e |
                         e <- Nest.inputs nest])
               (Nest.Map2 cs1 (Nest.Lambda lam) [] loc2),
             ots ++ [OTranspose cs2 0 1])
iswim _ _ = Nothing
