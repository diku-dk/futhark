module L0C.HOTrans.LoopKernel
  ( FusedKer(..)
  , OutputTransform(..)
  , applyTransform
  , optimizeKernel
  )
  where

import Control.Monad

import qualified Data.HashSet      as HS

import Data.Maybe
import Data.Loc

import L0C.L0
import L0C.HOTrans.SOAC (SOAC)
import qualified L0C.HOTrans.SOAC as SOAC

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
optimizeKernel ker = fromMaybe ker $ pushTranspose ker

pushTranspose :: FusedKer -> Maybe FusedKer
pushTranspose ker = do
  (n, k, cs, inputs') <- transposedInputs $ SOAC.inputs soac
  let depth = SOAC.mapNDepth soac
  if n+k < depth then
    Just ker { fsoac = (pat, inputs' `SOAC.setInputs` soac)
             , outputTransform = outputTransform ker ++ [OTranspose cs n k]
             }
  else Nothing
  where (pat, soac) = fsoac ker
        transposedInputs (Transpose cs n k (Var idd) _:args) =
          foldM comb (n, k, cs, [Var idd]) args
          where comb (n1, k1, cs1, idds) (Transpose cs2 n2 k2 (Var idd2) _)
                  | n1==n2, k1==k2         = Just (n1,k1,cs1++cs2,idds++[Var idd2])
                comb _                   _ = Nothing
        transposedInputs _ = Nothing
