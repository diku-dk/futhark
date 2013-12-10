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
import L0C.HOTrans.SOACNest (SOACNest)
import qualified L0C.HOTrans.SOACNest as Nest

data OutputTransform = OTranspose Certificates Int Int

applyTransform :: OutputTransform -> Ident -> SrcLoc -> Exp
applyTransform (OTranspose cs k n)  = Transpose cs k n . Var

data FusedKer = FusedKer {
    fsoac      :: (TupIdent, SOAC)
  -- ^ the fused SOAC statement, e.g.,
  -- (z,w) = map2( f(a,b), x, y )

  , inp        :: HS.HashSet Ident
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
          fromMaybe (startNest, startTrans) $
          pushTranspose startNest startTrans
        startNest = Nest.fromSOAC $ snd $ fsoac ker
        startTrans = outputTransform ker

pushTranspose :: SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])
pushTranspose nest ots = do
  (n, k, cs, inputs') <- transposedInputs $ Nest.inputs nest
  if n+k < mapDepth then
    Just (inputs' `Nest.setInputs` nest,
          ots ++ [OTranspose cs n k])
  else Nothing
  where transposedInputs (Transpose cs n k (Var idd) _:args) =
          foldM comb (n, k, cs, [Var idd]) args
          where comb (n1, k1, cs1, idds) (Transpose cs2 n2 k2 (Var idd2) _)
                  | n1==n2, k1==k2         = Just (n1,k1,cs1++cs2,idds++[Var idd2])
                comb _                   _ = Nothing
        transposedInputs _ = Nothing
        mapDepth = case Nest.operation nest of
                     Nest.Map2 _ _ levels _ -> length levels + 1
                     _                      -> 0
