{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Representation.ExplicitMemory.IndexFunctionTests
  ( tests
  )
where

import Prelude hiding (span, repeat)
import qualified Prelude as P
import qualified Data.List as DL

import Test.Tasty
import Test.Tasty.HUnit

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Syntax.Core()
import qualified Futhark.Util.Pretty as PR
import qualified Futhark.Util.IntegralExp as IE
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFunLMAD
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Alg as IxFunAlg

import qualified Futhark.Representation.ExplicitMemory.IndexFunctionWrapper as IxFunWrap
import Futhark.Representation.ExplicitMemory.IndexFunctionWrapper


instance IE.IntegralExp Int where
  quot = P.quot
  rem  = P.rem
  div  = P.div
  mod  = P.mod
  sgn  = Just . P.signum

  fromInt8  = fromInteger . toInteger
  fromInt16 = fromInteger . toInteger
  fromInt32 = fromInteger . toInteger
  fromInt64 = fromInteger . toInteger

allPoints :: [Int] -> [[Int]]
allPoints dims =
    let total = product dims
        strides = drop 1 $ DL.reverse $ scanl (*) 1 $ DL.reverse dims
    in map (unflatInd strides) [0..total-1]
    where unflatInd :: [Int] -> Int -> [Int]
          unflatInd strides x = fst $
            foldl (\(res, acc) span ->
                     (res ++ [acc `P.div` span], acc `P.mod` span))
            ([], x) strides

compareIxFuns :: IxFunLMAD.IxFun Int -> IxFunAlg.IxFun Int -> Assertion
compareIxFuns ixfunLMAD ixfunAlg =
  let lmadShape = IxFunLMAD.shape ixfunLMAD
      algShape = IxFunAlg.shape ixfunAlg
      points = allPoints lmadShape
      resLMAD = map (IxFunLMAD.index ixfunLMAD) points
      resAlg = map (IxFunAlg.index ixfunAlg) points
      errorMessage = "lmad ixfun:  " ++ PR.pretty ixfunLMAD ++ "\n" ++
                     "alg ixfun:   " ++ PR.pretty ixfunAlg ++ "\n" ++
                     "lmad shape:  " ++ show lmadShape ++ "\n" ++
                     "alg shape:   " ++ show algShape ++ "\n" ++
                     "lmad points length: " ++ show (length resLMAD) ++ "\n" ++
                     "alg points length:  " ++ show (length resAlg) ++ "\n" ++
                     "lmad points: " ++ show resLMAD ++ "\n" ++
                     "alg points:  " ++ show resAlg
  in (lmadShape == algShape && resLMAD == resAlg) @? errorMessage

compareOps :: IxFunWrap.IxFun Int -> Assertion
compareOps (ixfunLMAD, ixfunAlg) = compareIxFuns ixfunLMAD ixfunAlg

-- XXX: Clean this up.
n :: Int
n = 19
slice3 :: [DimIndex Int]
slice3 = [ DimSlice 2 (n `P.div` 3) 3
         , DimFix (n `P.div` 2)
         , DimSlice 1 (n `P.div` 2) 2
         ]


-- Actual tests.
tests :: TestTree
tests = testGroup "IndexFunctionTests"
        $ concat
        [ test_iota
        , test_slice_iota
        , test_reshape_slice_iota1
        , test_permute_slice_iota
        , test_repeat_slice_iota
        , test_rotate_rotate_permute_slice_iota
        , test_slice_rotate_permute_slice_iota1
        , test_slice_rotate_permute_slice_iota2
        , test_slice_rotate_permute_slice_iota3
        , test_permute_rotate_slice_permute_slice_iota
        , test_reshape_rotate_iota
        , test_reshape_permute_iota
        , test_reshape_slice_iota2
        , test_reshape_slice_iota3
        , test_complex1
        , test_complex2
        , test_complex3
        , test_rebase1
        , test_rebase2
        , test_rebase3
        , test_rebase4_5
        , test_rebase6
        ]

singleton :: TestTree -> [TestTree]
singleton = (: [])

test_iota :: [TestTree]
test_iota = singleton $ testCase "iota" $ compareOps $
  iota [n]

test_slice_iota :: [TestTree]
test_slice_iota = singleton $ testCase "slice . iota" $ compareOps $
  slice (iota [n, n, n]) slice3

test_reshape_slice_iota1 :: [TestTree]
test_reshape_slice_iota1 = singleton $ testCase "reshape . slice . iota 1" $ compareOps $
  reshape (slice (iota [n, n, n]) slice3)
  [DimNew (n `P.div` 2), DimNew (n `P.div` 3)]

test_permute_slice_iota :: [TestTree]
test_permute_slice_iota = singleton $ testCase "permute . slice . iota" $ compareOps $
  permute (slice (iota [n, n, n]) slice3) [1, 0]

test_repeat_slice_iota :: [TestTree]
test_repeat_slice_iota = singleton $ testCase "repeat . slice . iota" $ compareOps $
  repeat (slice (iota [n, n, n]) slice3) [[2, 3], [3, 2]] [4, 4]

test_rotate_rotate_permute_slice_iota :: [TestTree]
test_rotate_rotate_permute_slice_iota =
  singleton $ testCase "rotate . rotate . permute . slice . iota" $ compareOps $
  let ixfun = permute (slice (iota [n, n, n]) slice3) [1, 0]
  in rotate (rotate ixfun [2, 1]) [1, 2]

test_slice_rotate_permute_slice_iota1 :: [TestTree]
test_slice_rotate_permute_slice_iota1 =
  singleton $ testCase "slice . rotate . permute . slice . iota 1" $ compareOps $
  let slice2 = [ DimSlice 0 n 1
               , DimSlice 1 (n `P.div` 2) 2
               , DimSlice 0 n 1
               ]
      slice13 = [ DimSlice 2 (n `P.div` 3) 3
                , DimSlice 0 (n `P.div` 2) 1
                , DimSlice 1 (n `P.div` 2) 2
                ]
      ixfun = permute (slice (iota [n, n, n]) slice2) [2, 1, 0]
      ixfun' = slice (rotate ixfun [3, 1, 2]) slice13
  in ixfun'

test_slice_rotate_permute_slice_iota2 :: [TestTree]
test_slice_rotate_permute_slice_iota2 =
  singleton $ testCase "slice . rotate . permute . slice . iota 2" $ compareOps $
  let slice2 = [ DimSlice 0 (n `P.div` 2) 1
               , DimFix   (n `P.div` 2)
               , DimSlice 0 (n `P.div` 3) 1
               ]
      slice13 = [ DimSlice 2 (n `P.div` 3) 3
                , DimSlice 0 n 1
                , DimSlice 1 (n `P.div` 2) 2
                ]
      ixfun = permute (slice (iota [n, n, n]) slice13) [2, 1, 0]
      ixfun' = slice (rotate ixfun [3, 1, 2]) slice2
  in ixfun'

test_slice_rotate_permute_slice_iota3 :: [TestTree]
test_slice_rotate_permute_slice_iota3 =
  singleton $ testCase "slice . rotate . permute . slice . iota 3" $ compareOps $
  -- full-slice of (-1) stride
  let ixfun = permute (slice (iota [n, n, n]) slice3) [1, 0]
      ixfun' = rotate ixfun [2, 1]

      (n1, m1) = case IxFunLMAD.shape (fst ixfun') of
                   [a, b] -> (a, b)
                   _ ->  error "expecting 2 dimensions at this point!"
      negslice = [DimSlice 0 n1 1, DimSlice (m1 - 1) m1 (-1)]
      ixfun'' = rotate (slice ixfun' negslice) [1,2]
  in ixfun''

test_permute_rotate_slice_permute_slice_iota :: [TestTree]
test_permute_rotate_slice_permute_slice_iota =
  singleton $ testCase "permute . rotate . slice . permute . slice . iota" $ compareOps $
  -- contiguousness
  let slice33 = [ DimFix (n `P.div` 2)
                , DimSlice (n - 1) (n `P.div` 3) (-1)
                , DimSlice 0 n 1
                ]
      ixfun = permute (slice (iota [n, n, n]) slice33) [1, 0]
      m = n `P.div` 3
      slice1 = [DimSlice (n - 1) n (-1), DimSlice 2 (m - 2) 1]
      ixfun' = permute (rotate (slice ixfun slice1) [1, 2]) [1, 0]
  in ixfun'

test_reshape_rotate_iota :: [TestTree]
test_reshape_rotate_iota =
  -- negative reshape test
  singleton $ testCase "reshape . rotate . iota" $ compareOps $
  let newdims = [DimNew (n * n), DimCoercion n]
  in reshape (rotate (iota [n, n, n]) [1, 0, 0]) newdims

test_reshape_permute_iota :: [TestTree]
test_reshape_permute_iota =
  -- negative reshape test
  singleton $ testCase "reshape . permute . iota" $ compareOps $
  let newdims = [DimNew (n * n), DimCoercion n]
  in reshape (permute (iota [n, n, n]) [1, 2, 0]) newdims

test_reshape_slice_iota2 :: [TestTree]
test_reshape_slice_iota2 =
  -- negative reshape test
  singleton $ testCase "reshape . slice . iota 2" $ compareOps $
  let newdims = [DimNew (n*n), DimCoercion n]
      slc = [ DimFix (n `P.div` 2)
            , DimSlice (n-1) n (-1)
            , DimSlice 0 n 1
            , DimSlice (n-1) n (-1)
            ]
  in reshape (slice (iota [n, n, n, n]) slc) newdims

test_reshape_slice_iota3 :: [TestTree]
test_reshape_slice_iota3 =
  -- negative reshape test
  singleton $ testCase "reshape . slice . iota 3" $ compareOps $
  let newdims = [DimNew (n*n), DimCoercion n]
      slc = [ DimFix (n `P.div` 2)
            , DimSlice 0 n 1
            , DimSlice 0 (n `P.div` 2) 1
            , DimSlice 0 n 1
            ]
  in reshape (slice (iota [n, n, n, n]) slc) newdims

test_complex1 :: [TestTree]
test_complex1 =
  singleton $ testCase "reshape . permute . rotate . slice . permute . slice . iota 1" $ compareOps $
  let newdims = [ DimCoercion n
                , DimCoercion n
                , DimNew n
                , DimCoercion ((n `P.div` 3) - 2)
                ]
      slice33 = [ DimSlice (n-1) (n `P.div` 3) (-1)
                , DimSlice (n-1) n (-1)
                , DimSlice (n-1) n (-1)
                , DimSlice 0 n 1
                ]
      ixfun = permute (slice (iota [n, n, n, n, n]) slice33) [3, 1, 2, 0]
      m = n `P.div` 3
      slice1 = [DimSlice 0 n 1, DimSlice (n-1) n (-1), DimSlice (n-1) n (-1), DimSlice 1 (m-2) (-1)]
      ixfun' = reshape (rotate (slice ixfun slice1) [1, 2, 3, 4]) newdims
  in ixfun'

test_complex2 :: [TestTree]
test_complex2 =
  singleton $ testCase "reshape . permute . rotate . slice . permute . slice . iota 2" $ compareOps $
  let newdims = [ DimCoercion n
                , DimNew (n*n)
                , DimCoercion ((n `P.div` 3) - 2)]
      slc2 = [ DimFix (n `P.div` 2)
             , DimSlice (n-1) (n `P.div` 3) (-1)
             , DimSlice (n-1) n (-1)
             , DimSlice (n-1) n (-1)
             , DimSlice 0 n 1
             ]
      ixfun = permute (slice (iota [n, n, n, n, n]) slc2) [3, 1, 2, 0]
      m = n `P.div` 3
      slice1 = [DimSlice 0 n 1, DimSlice (n-1) n (-1), DimSlice (n-1) n (-1), DimSlice 1 (m-2) (-1)]
      ixfun' = reshape (rotate (slice ixfun slice1) [1, 0, 0, 2]) newdims
  in ixfun'

test_complex3 :: [TestTree]
test_complex3 =
  singleton $ testCase "reshape . permute . rotate . slice . permute . slice . iota 3" $ compareOps $
  let newdims = [ DimCoercion 1
                , DimCoercion n
                , DimNew (n*n)
                , DimCoercion 2
                , DimCoercion ((n `P.div` 3) - 2)
                ]
      slc3 = [ DimFix (n `P.div` 2)
             , DimSlice (n-1) (n `P.div` 3) (-1)
             , DimSlice (n-1) n (-1)
             , DimSlice (n-1) n (-1)
             , DimSlice 0 n 1
             ]
      ixfun = permute (slice (iota [n, n, n, n, n]) slc3) [3, 1, 2, 0]
      m = n `P.div` 3
      slice1 = [DimSlice 0 n 1, DimSlice (n-1) n (-1), DimSlice (n-1) n (-1), DimSlice 1 (m-2) (-1)]
      repeats = [[1],[],[],[2]]
      ixfun' = reshape (repeat (rotate (slice ixfun slice1) [1, 0, 0, 2]) repeats []) newdims
  in ixfun'

test_rebase1 :: [TestTree]
test_rebase1 =
  singleton $ testCase "rebase 1" $ compareOps $
    let slice_base = [ DimFix (n `P.div` 2)
                     , DimSlice 2 (n-2) 1
                     , DimSlice 3 (n-3) 1
                     ]
        ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
        ixfn_orig = rotate (permute (iota [n-3, n-2]) [1, 0]) [1, 2]
        ixfn_rebase = rebase ixfn_base ixfn_orig
    in ixfn_rebase

test_rebase2 :: [TestTree]
test_rebase2 =
  singleton $ testCase "rebase 2" $ compareOps $
    let slice_base = [ DimFix (n `P.div` 2)
                     , DimSlice (n-1) (n-2) (-1)
                     , DimSlice (n-1) (n-3) (-1)
                     ]
        slice_orig = [ DimSlice (n-4) (n-3) (-1)
                     , DimSlice (n-3) (n-2) (-1)
                     ]
        ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
        ixfn_orig = rotate (permute (slice (iota [n-3, n-2]) slice_orig) [1, 0]) [1, 2]
        ixfn_rebase = rebase ixfn_base ixfn_orig
    in ixfn_rebase

test_rebase3 :: [TestTree]
test_rebase3 =
  singleton $ testCase "rebase full orig but not monotonic" $ compareOps $
  let n2 = (n-2) `P.div` 3
      n3 = (n-3) `P.div` 2
      slice_base = [ DimFix (n `P.div` 2)
                   , DimSlice (n-1) n2 (-3)
                   , DimSlice (n-1) n3 (-2)
                   ]
      slice_orig = [ DimSlice (n3-1) n3 (-1)
                   , DimSlice (n2-1) n2 (-1)
                   ]
      ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
      ixfn_orig = rotate (permute (slice (iota [n3, n2]) slice_orig) [1, 0]) [1, 2]
      ixfn_rebase = rebase ixfn_base ixfn_orig
  in ixfn_rebase

test_rebase4_5 :: [TestTree]
test_rebase4_5 =
  let n2 = (n-2) `P.div` 3
      n3 = (n-3) `P.div` 2
      slice_base = [ DimFix (n `P.div` 2)
                   , DimSlice (n-1) n2 (-3)
                   , DimSlice 3 n3 2
                   ]
      slice_orig = [ DimSlice (n3-1) n3 (-1)
                   , DimSlice 0 n2 1
                   ]
      ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
      ixfn_orig = rotate (permute (slice (iota [n3, n2]) slice_orig) [1, 0]) [1, 2]
  in [ testCase "rebase mixed monotonicities" $ compareOps $
       rebase ixfn_base ixfn_orig

     , testCase "rebase repetitions and mixed monotonicities 1" $ compareOps $
       let ixfn_orig' = repeat ixfn_orig [[2, 2], [3, 3]] [2, 3]
       in rebase ixfn_base ixfn_orig'
     ]

test_rebase6 :: [TestTree]
test_rebase6 =
  singleton $ testCase "rebase repetitions and mixed monotonicities 2" $ compareOps $
  let n2 = (n-2) `P.div` 3
      n3 = (n-3) `P.div` 2
      slice_base = [ DimFix (n `P.div` 2)
                   , DimSlice (n-1) n2 (-3)
                   ]
      slice_orig = [ DimSlice (n3-1) n3 (-1)
                   , DimSlice 0 n2 1
                   ]
      ixfn_base = permute (repeat (rotate (slice (iota [n, n]) slice_base) [1]) [[], []] [n3]) [1, 0]
      ixfn_orig = rotate (permute (slice (iota [n3, n2]) slice_orig) [1, 0]) [1, 2]
      ixfn_orig' = repeat ixfn_orig [[2, 2],[3, 3]] [2, 3]
      ixfn_rebase = rebase ixfn_base ixfn_orig'
  in ixfn_rebase
