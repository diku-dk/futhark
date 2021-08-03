{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.Mem.IxFunTests
  ( tests,
  )
where

import Data.Function ((&))
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import Futhark.Analysis.PrimExp
import qualified Futhark.IR.Mem.IxFun as IxFunLMAD
import qualified Futhark.IR.Mem.IxFun.Alg as IxFunAlg
import Futhark.IR.Mem.IxFunWrapper
import qualified Futhark.IR.Mem.IxFunWrapper as IxFunWrap
import Futhark.IR.Syntax
import Futhark.IR.Syntax.Core ()
import qualified Futhark.Util.IntegralExp as IE
import qualified Futhark.Util.Pretty as PR
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (span)
import qualified Prelude as P

instance IE.IntegralExp Int where
  quot = P.quot
  rem = P.rem
  div = P.div
  mod = P.mod
  sgn = Just . P.signum

allPoints :: [Int] -> [[Int]]
allPoints dims =
  let total = product dims
      strides = drop 1 $ DL.reverse $ scanl (*) 1 $ DL.reverse dims
   in map (unflatInd strides) [0 .. total -1]
  where
    unflatInd :: [Int] -> Int -> [Int]
    unflatInd strides x =
      fst $
        foldl
          ( \(res, acc) span ->
              (res ++ [acc `P.div` span], acc `P.mod` span)
          )
          ([], x)
          strides

compareIxFuns :: IxFunLMAD.IxFun Int -> IxFunAlg.IxFun Int -> Assertion
compareIxFuns ixfunLMAD ixfunAlg =
  let lmadShape = IxFunLMAD.shape ixfunLMAD
      algShape = IxFunAlg.shape ixfunAlg
      points = allPoints lmadShape
      resLMAD = map (IxFunLMAD.index ixfunLMAD) points
      resAlg = map (IxFunAlg.index ixfunAlg) points
      errorMessage =
        "lmad ixfun:  " ++ PR.pretty ixfunLMAD ++ "\n"
          ++ "alg ixfun:   "
          ++ PR.pretty ixfunAlg
          ++ "\n"
          ++ "lmad shape:  "
          ++ show lmadShape
          ++ "\n"
          ++ "alg shape:   "
          ++ show algShape
          ++ "\n"
          ++ "lmad points length: "
          ++ show (length resLMAD)
          ++ "\n"
          ++ "alg points length:  "
          ++ show (length resAlg)
          ++ "\n"
          ++ "lmad points: "
          ++ show resLMAD
          ++ "\n"
          ++ "alg points:  "
          ++ show resAlg
   in (lmadShape == algShape && resLMAD == resAlg) @? errorMessage

compareOps :: IxFunWrap.IxFun Int -> Assertion
compareOps (ixfunLMAD, ixfunAlg) = compareIxFuns ixfunLMAD ixfunAlg

-- XXX: Clean this up.
n :: Int
n = 19

slice3 :: Slice Int
slice3 =
  Slice
    [ DimSlice 2 (n `P.div` 3) 3,
      DimFix (n `P.div` 2),
      DimSlice 1 (n `P.div` 2) 2
    ]

-- Actual tests.
tests :: TestTree
tests =
  testGroup "IxFunTests" $
    concat
      [ test_iota,
        test_slice_iota,
        test_reshape_slice_iota1,
        test_permute_slice_iota,
        test_rotate_rotate_permute_slice_iota,
        test_slice_rotate_permute_slice_iota1,
        test_slice_rotate_permute_slice_iota2,
        test_slice_rotate_permute_slice_iota3,
        test_permute_rotate_slice_permute_slice_iota,
        test_reshape_rotate_iota,
        test_reshape_permute_iota,
        test_reshape_slice_iota2,
        test_reshape_slice_iota3,
        test_complex1,
        test_complex2,
        test_rebase1,
        test_rebase2,
        test_rebase3,
        test_rebase4_5,
        test_invIxFun,
        test_flatSlice_iota,
        test_slice_flatSlice_iota,
        test_flatSlice_flatSlice_iota,
        test_flatSlice_slice_iota,
        test_flatSlice_rotate_iota,
        test_flatSlice_rotate_slice_iota,
        test_flatSlice_transpose_slice_iota,
        test_rotate_flatSlice_transpose_slice_iota
      ]

singleton :: TestTree -> [TestTree]
singleton = (: [])

test_iota :: [TestTree]
test_iota =
  singleton $
    testCase "iota" $
      compareOps $
        iota [n]

test_slice_iota :: [TestTree]
test_slice_iota =
  singleton $
    testCase "slice . iota" $
      compareOps $
        slice (iota [n, n, n]) slice3

test_reshape_slice_iota1 :: [TestTree]
test_reshape_slice_iota1 =
  singleton $
    testCase "reshape . slice . iota 1" $
      compareOps $
        reshape
          (slice (iota [n, n, n]) slice3)
          [DimNew (n `P.div` 2), DimNew (n `P.div` 3)]

test_permute_slice_iota :: [TestTree]
test_permute_slice_iota =
  singleton $
    testCase "permute . slice . iota" $
      compareOps $
        permute (slice (iota [n, n, n]) slice3) [1, 0]

test_rotate_rotate_permute_slice_iota :: [TestTree]
test_rotate_rotate_permute_slice_iota =
  singleton $
    testCase "rotate . rotate . permute . slice . iota" $
      compareOps $
        let ixfun = permute (slice (iota [n, n, n]) slice3) [1, 0]
         in rotate (rotate ixfun [2, 1]) [1, 2]

test_slice_rotate_permute_slice_iota1 :: [TestTree]
test_slice_rotate_permute_slice_iota1 =
  singleton $
    testCase "slice . rotate . permute . slice . iota 1" $
      compareOps $
        let slice2 =
              Slice
                [ DimSlice 0 n 1,
                  DimSlice 1 (n `P.div` 2) 2,
                  DimSlice 0 n 1
                ]
            slice13 =
              Slice
                [ DimSlice 2 (n `P.div` 3) 3,
                  DimSlice 0 (n `P.div` 2) 1,
                  DimSlice 1 (n `P.div` 2) 2
                ]
            ixfun = permute (slice (iota [n, n, n]) slice2) [2, 1, 0]
            ixfun' = slice (rotate ixfun [3, 1, 2]) slice13
         in ixfun'

test_slice_rotate_permute_slice_iota2 :: [TestTree]
test_slice_rotate_permute_slice_iota2 =
  singleton $
    testCase "slice . rotate . permute . slice . iota 2" $
      compareOps $
        let slice2 =
              Slice
                [ DimSlice 0 (n `P.div` 2) 1,
                  DimFix (n `P.div` 2),
                  DimSlice 0 (n `P.div` 3) 1
                ]
            slice13 =
              Slice
                [ DimSlice 2 (n `P.div` 3) 3,
                  DimSlice 0 n 1,
                  DimSlice 1 (n `P.div` 2) 2
                ]
            ixfun = permute (slice (iota [n, n, n]) slice13) [2, 1, 0]
            ixfun' = slice (rotate ixfun [3, 1, 2]) slice2
         in ixfun'

test_slice_rotate_permute_slice_iota3 :: [TestTree]
test_slice_rotate_permute_slice_iota3 =
  singleton $
    testCase "slice . rotate . permute . slice . iota 3" $
      compareOps $
        -- full-slice of (-1) stride
        let ixfun = permute (slice (iota [n, n, n]) slice3) [1, 0]
            ixfun' = rotate ixfun [2, 1]

            (n1, m1) = case IxFunLMAD.shape (fst ixfun') of
              [a, b] -> (a, b)
              _ -> error "expecting 2 dimensions at this point!"
            negslice =
              Slice
                [ DimSlice 0 n1 1,
                  DimSlice (m1 - 1) m1 (-1)
                ]
            ixfun'' = rotate (slice ixfun' negslice) [1, 2]
         in ixfun''

test_permute_rotate_slice_permute_slice_iota :: [TestTree]
test_permute_rotate_slice_permute_slice_iota =
  singleton $
    testCase "permute . rotate . slice . permute . slice . iota" $
      compareOps $
        -- contiguousness
        let slice33 =
              Slice
                [ DimFix (n `P.div` 2),
                  DimSlice (n - 1) (n `P.div` 3) (-1),
                  DimSlice 0 n 1
                ]
            ixfun = permute (slice (iota [n, n, n]) slice33) [1, 0]
            m = n `P.div` 3
            slice1 =
              Slice
                [ DimSlice (n - 1) n (-1),
                  DimSlice 2 (m - 2) 1
                ]
            ixfun' = permute (rotate (slice ixfun slice1) [1, 2]) [1, 0]
         in ixfun'

test_reshape_rotate_iota :: [TestTree]
test_reshape_rotate_iota =
  -- negative reshape test
  singleton $
    testCase "reshape . rotate . iota" $
      compareOps $
        let newdims = [DimNew (n * n), DimCoercion n]
         in reshape (rotate (iota [n, n, n]) [1, 0, 0]) newdims

test_reshape_permute_iota :: [TestTree]
test_reshape_permute_iota =
  -- negative reshape test
  singleton $
    testCase "reshape . permute . iota" $
      compareOps $
        let newdims = [DimNew (n * n), DimCoercion n]
         in reshape (permute (iota [n, n, n]) [1, 2, 0]) newdims

test_reshape_slice_iota2 :: [TestTree]
test_reshape_slice_iota2 =
  -- negative reshape test
  singleton $
    testCase "reshape . slice . iota 2" $
      compareOps $
        let newdims = [DimNew (n * n), DimCoercion n]
            slc =
              Slice
                [ DimFix (n `P.div` 2),
                  DimSlice (n -1) n (-1),
                  DimSlice 0 n 1,
                  DimSlice (n -1) n (-1)
                ]
         in reshape (slice (iota [n, n, n, n]) slc) newdims

test_reshape_slice_iota3 :: [TestTree]
test_reshape_slice_iota3 =
  -- negative reshape test
  singleton $
    testCase "reshape . slice . iota 3" $
      compareOps $
        let newdims = [DimNew (n * n), DimCoercion n]
            slc =
              Slice
                [ DimFix (n `P.div` 2),
                  DimSlice 0 n 1,
                  DimSlice 0 (n `P.div` 2) 1,
                  DimSlice 0 n 1
                ]
         in reshape (slice (iota [n, n, n, n]) slc) newdims

test_complex1 :: [TestTree]
test_complex1 =
  singleton $
    testCase "reshape . permute . rotate . slice . permute . slice . iota 1" $
      compareOps $
        let newdims =
              [ DimCoercion n,
                DimCoercion n,
                DimNew n,
                DimCoercion ((n `P.div` 3) - 2)
              ]
            slice33 =
              Slice
                [ DimSlice (n -1) (n `P.div` 3) (-1),
                  DimSlice (n -1) n (-1),
                  DimSlice (n -1) n (-1),
                  DimSlice 0 n 1
                ]
            ixfun = permute (slice (iota [n, n, n, n, n]) slice33) [3, 1, 2, 0]
            m = n `P.div` 3
            slice1 =
              Slice
                [ DimSlice 0 n 1,
                  DimSlice (n -1) n (-1),
                  DimSlice (n -1) n (-1),
                  DimSlice 1 (m -2) (-1)
                ]
            ixfun' = reshape (rotate (slice ixfun slice1) [1, 2, 3, 4]) newdims
         in ixfun'

test_complex2 :: [TestTree]
test_complex2 =
  singleton $
    testCase "reshape . permute . rotate . slice . permute . slice . iota 2" $
      compareOps $
        let newdims =
              [ DimCoercion n,
                DimNew (n * n),
                DimCoercion ((n `P.div` 3) - 2)
              ]
            slc2 =
              Slice
                [ DimFix (n `P.div` 2),
                  DimSlice (n -1) (n `P.div` 3) (-1),
                  DimSlice (n -1) n (-1),
                  DimSlice (n -1) n (-1),
                  DimSlice 0 n 1
                ]
            ixfun = permute (slice (iota [n, n, n, n, n]) slc2) [3, 1, 2, 0]
            m = n `P.div` 3
            slice1 =
              Slice
                [ DimSlice 0 n 1,
                  DimSlice (n -1) n (-1),
                  DimSlice (n -1) n (-1),
                  DimSlice 1 (m -2) (-1)
                ]
            ixfun' = reshape (rotate (slice ixfun slice1) [1, 0, 0, 2]) newdims
         in ixfun'

test_rebase1 :: [TestTree]
test_rebase1 =
  singleton $
    testCase "rebase 1" $
      compareOps $
        let slice_base =
              Slice
                [ DimFix (n `P.div` 2),
                  DimSlice 2 (n -2) 1,
                  DimSlice 3 (n -3) 1
                ]
            ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
            ixfn_orig = rotate (permute (iota [n -3, n -2]) [1, 0]) [1, 2]
            ixfn_rebase = rebase ixfn_base ixfn_orig
         in ixfn_rebase

test_rebase2 :: [TestTree]
test_rebase2 =
  singleton $
    testCase "rebase 2" $
      compareOps $
        let slice_base =
              Slice
                [ DimFix (n `P.div` 2),
                  DimSlice (n -1) (n -2) (-1),
                  DimSlice (n -1) (n -3) (-1)
                ]
            slice_orig =
              Slice
                [ DimSlice (n -4) (n -3) (-1),
                  DimSlice (n -3) (n -2) (-1)
                ]
            ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
            ixfn_orig = rotate (permute (slice (iota [n -3, n -2]) slice_orig) [1, 0]) [1, 2]
            ixfn_rebase = rebase ixfn_base ixfn_orig
         in ixfn_rebase

test_rebase3 :: [TestTree]
test_rebase3 =
  singleton $
    testCase "rebase full orig but not monotonic" $
      compareOps $
        let n2 = (n -2) `P.div` 3
            n3 = (n -3) `P.div` 2
            slice_base =
              Slice
                [ DimFix (n `P.div` 2),
                  DimSlice (n -1) n2 (-3),
                  DimSlice (n -1) n3 (-2)
                ]
            slice_orig =
              Slice
                [ DimSlice (n3 -1) n3 (-1),
                  DimSlice (n2 -1) n2 (-1)
                ]
            ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
            ixfn_orig = rotate (permute (slice (iota [n3, n2]) slice_orig) [1, 0]) [1, 2]
            ixfn_rebase = rebase ixfn_base ixfn_orig
         in ixfn_rebase

test_rebase4_5 :: [TestTree]
test_rebase4_5 =
  let n2 = (n -2) `P.div` 3
      n3 = (n -3) `P.div` 2
      slice_base =
        Slice
          [ DimFix (n `P.div` 2),
            DimSlice (n -1) n2 (-3),
            DimSlice 3 n3 2
          ]
      slice_orig =
        Slice
          [ DimSlice (n3 -1) n3 (-1),
            DimSlice 0 n2 1
          ]
      ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
      ixfn_orig = rotate (permute (slice (iota [n3, n2]) slice_orig) [1, 0]) [1, 2]
   in [ testCase "rebase mixed monotonicities" $
          compareOps $
            rebase ixfn_base ixfn_orig
      ]

test_invIxFun :: [TestTree]
test_invIxFun =
  [ testCase "Iota" $
      -- let a = iota n
      -- let b = a
      -- let y[i] = b
      let i_0 = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
          i_b = i_0
          i_y = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
       in case IxFunLMAD.invIxFun i_y i_b of
            Just i_0' ->
              let i_b' = IxFunLMAD.rebase i_b i_0'
               in i_y @=? i_b'
            Nothing -> assertFailure "Couldn't invert i_b on i_y",
    testCase "Simple rotation" $
      -- let a = iota n
      -- let b = rotate 4 a
      -- let y[i] = b
      let i_0 = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
          i_b = IxFunLMAD.rebase (IxFunLMAD.rotate i_0 [TPrimExp $ ValueExp (IntValue (Int64Value 4))]) i_0
          i_y = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
       in case IxFunLMAD.invIxFun i_y i_b of
            Just i_0' ->
              let i_b' = IxFunLMAD.rebase i_b i_0'
               in i_y @=? i_b'
            Nothing -> assertFailure "Couldn't invert i_b on i_y",
    testCase "Both rotated rotation" $
      -- let a = iota n
      -- let b = rotate 4 a
      -- let y = map (rotate 20) y
      -- let y[i] = b
      let i_0 = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
          i_b = IxFunLMAD.rebase (IxFunLMAD.rotate i_0 [TPrimExp $ ValueExp (IntValue (Int64Value 4))]) i_0
          i_y = IxFunLMAD.rebase (IxFunLMAD.rotate i_0 [TPrimExp $ ValueExp (IntValue (Int64Value 20))]) i_0
       in case IxFunLMAD.invIxFun i_y i_b of
            Just i_0' ->
              let i_b' = IxFunLMAD.rebase i_b i_0'
               in i_y @=? i_b'
            Nothing -> assertFailure "Couldn't invert i_b on i_y",
    testCase "Simple negative rotation" $
      -- let a = iota n
      -- let b = rotate (-4) a
      -- let y[i] = b
      let i_0 = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
          i_b = IxFunLMAD.rebase (IxFunLMAD.rotate i_0 [TPrimExp $ ValueExp (IntValue (Int64Value (-4)))]) i_0
          i_y = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
       in case IxFunLMAD.invIxFun i_y i_b of
            Just i_0' ->
              let i_b' = IxFunLMAD.rebase i_b i_0'
               in i_y @=? i_b'
            Nothing -> assertFailure "Couldn't invert i_b on i_y",
    testCase "Rotation on 2-dimensional index function" $
      -- let a = replicate n (iota (n * 2))
      -- let b = rotate -1 (map (rotate 5) a)
      -- let y[i] = b
      let i_0 =
            IxFunLMAD.iota
              [ TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64,
                TPrimExp $
                  BinOpExp
                    (Mul Int64 OverflowUndef)
                    (ValueExp (IntValue (Int64Value 2)))
                    (LeafExp (VName (nameFromString "n") 0) $ IntType Int64)
              ]
          i_b =
            IxFunLMAD.rebase
              ( IxFunLMAD.rotate
                  (i_0)
                  [ TPrimExp $ ValueExp (IntValue (Int64Value (-1))),
                    TPrimExp $ ValueExp (IntValue (Int64Value 5))
                  ]
              )
              i_0
          i_y =
            IxFunLMAD.iota
              [ TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64,
                TPrimExp $
                  BinOpExp
                    (Mul Int64 OverflowUndef)
                    (ValueExp (IntValue (Int64Value 2)))
                    (LeafExp (VName (nameFromString "n") 0) $ IntType Int64)
              ]
       in case IxFunLMAD.invIxFun i_y i_b of
            Just i_0' ->
              let i_b' = IxFunLMAD.rebase i_b i_0'
               in i_y @=? i_b'
            Nothing -> assertFailure "Couldn't invert i_b on i_y",
    testCase "Cannot invert lossy strides" $
      -- let a = iota 20
      -- let b = a[0:20:2]
      -- let y[i] = b
      let i_0 = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
          i_b =
            IxFunLMAD.slice
              i_0
              $ Slice
                [ DimSlice
                    (TPrimExp $ ValueExp (IntValue (Int64Value 0)))
                    (TPrimExp $ ValueExp (IntValue (Int64Value 10)))
                    (TPrimExp $ ValueExp (IntValue (Int64Value 2)))
                ]
          i_y = IxFunLMAD.iota [TPrimExp $ ValueExp (IntValue (Int64Value 10))]
       in Nothing @=? IxFunLMAD.invIxFun i_y i_b,
    testCase "Cannot invert lossy slices" $
      -- let a = iota 20
      -- let b = a[5:10:1]
      -- let y[i] = b
      let i_0 = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
          i_b =
            IxFunLMAD.slice
              i_0
              $ Slice
                [ DimSlice
                    (TPrimExp $ ValueExp (IntValue (Int64Value 5)))
                    (TPrimExp $ ValueExp (IntValue (Int64Value 10)))
                    (TPrimExp $ ValueExp (IntValue (Int64Value 1)))
                ]
          i_y = IxFunLMAD.iota [TPrimExp $ ValueExp (IntValue (Int64Value 10))]
       in Nothing @=? IxFunLMAD.invIxFun i_y i_b,
    testCase "Inverting permutations" $
      -- let a = replicate n (iota n)
      -- let b = transpose a
      -- let y[i] = b
      let i_0 = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64, TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
          i_b =
            IxFunLMAD.permute i_0 [1, 0]
          i_y =
            IxFunLMAD.iota
              [ TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64,
                TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64
              ]
       in case IxFunLMAD.invIxFun i_y i_b of
            Just i_0' ->
              let i_b' = IxFunLMAD.rebase i_b i_0'
               in i_y @=? i_b'
            Nothing -> assertFailure $ "Couldn't invert i_b on i_y\n" ++ pretty i_0 ++ "\n" ++ pretty i_b,
    testCase
      "Cannot invert multi-lmad index functions"
      $ let i_b =
              IxFunLMAD.IxFun
                ( IxFunLMAD.LMAD
                    (TPrimExp $ ValueExp (IntValue (Int64Value 0)))
                    [IxFunLMAD.LMADDim (TPrimExp $ ValueExp (IntValue (Int64Value 1))) (TPrimExp $ ValueExp (IntValue (Int64Value 0))) (TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64) 0 IxFunLMAD.Inc]
                    :| [IxFunLMAD.LMAD 0 [IxFunLMAD.LMADDim (TPrimExp $ ValueExp (IntValue (Int64Value 1))) (TPrimExp $ ValueExp (IntValue (Int64Value 0))) (TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64) 0 IxFunLMAD.Inc]]
                )
                [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
                True
            i_y = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
         in Nothing @=? IxFunLMAD.invIxFun i_y i_b,
    testCase "Base change" $
      -- let a = iota n
      -- let b = rotate 4 a
      -- let y[i] = b
      let i_b = IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64]
          i_y =
            IxFunLMAD.slice
              (IxFunLMAD.iota [TPrimExp $ LeafExp (VName (nameFromString "m") 0) $ IntType Int64])
              $ Slice
                [ DimSlice
                    (TPrimExp $ ValueExp (IntValue (Int64Value 5)))
                    (TPrimExp $ LeafExp (VName (nameFromString "n") 0) $ IntType Int64)
                    (TPrimExp $ ValueExp (IntValue (Int64Value 1)))
                ]
       in case IxFunLMAD.invIxFun i_y i_b of
            Just i_0' ->
              let i_b' = IxFunLMAD.rebase i_b i_0'
               in i_y @=? i_b'
            Nothing -> assertFailure "Couldn't invert i_b on i_y"
  ]
    & testGroup "invIxFun"
    & singleton

test_flatSlice_iota :: [TestTree]
test_flatSlice_iota =
  singleton $
    testCase "flatSlice . iota" $
      compareOps $
        flatSlice (iota [n * n * n * n]) $
          FlatSlice 2 [FlatDimIndex (n * 2) 4, FlatDimIndex n 3, FlatDimIndex 1 2]

test_slice_flatSlice_iota :: [TestTree]
test_slice_flatSlice_iota =
  singleton $
    testCase "slice . flatSlice . iota " $
      compareOps $
        slice (flatSlice (iota [2 + n * n * n]) flat_slice) $
          Slice [DimFix 2, DimSlice 0 n 1, DimFix 0]
  where
    flat_slice = FlatSlice 2 [FlatDimIndex (n * n) 1, FlatDimIndex n 1, FlatDimIndex 1 1]

test_flatSlice_flatSlice_iota :: [TestTree]
test_flatSlice_flatSlice_iota =
  singleton $
    testCase "flatSlice . flatSlice . iota " $
      compareOps $
        flatSlice (flatSlice (iota [10 * 10]) flat_slice_1) flat_slice_2
  where
    flat_slice_1 = FlatSlice 17 [FlatDimIndex 3 27, FlatDimIndex 3 10, FlatDimIndex 3 1]
    flat_slice_2 = FlatSlice 2 [FlatDimIndex 2 (-2)]

test_flatSlice_slice_iota :: [TestTree]
test_flatSlice_slice_iota =
  singleton $
    testCase "flatSlice . slice . iota " $
      compareOps $
        flatSlice (slice (iota [210, 100]) $ Slice [DimSlice 10 100 2, DimFix 10]) flat_slice_1
  where
    flat_slice_1 = FlatSlice 17 [FlatDimIndex 3 27, FlatDimIndex 3 10, FlatDimIndex 3 1]

test_flatSlice_rotate_iota :: [TestTree]
test_flatSlice_rotate_iota =
  singleton $
    testCase "flatSlice . rotate . iota " $
      compareOps $
        flatSlice (rotate (iota [10, 10]) [2, 5]) flat_slice_1
  where
    flat_slice_1 = FlatSlice 3 [FlatDimIndex 2 2, FlatDimIndex 2 1]

test_flatSlice_rotate_slice_iota :: [TestTree]
test_flatSlice_rotate_slice_iota =
  singleton $
    testCase "flatSlice . rotate . slice . iota " $
      compareOps $
        flatSlice (rotate (slice (iota [20, 20]) $ Slice [DimSlice 1 5 2, DimSlice 0 5 2]) [2, 3]) flat_slice_1
  where
    flat_slice_1 = FlatSlice 1 [FlatDimIndex 2 2]

test_flatSlice_transpose_slice_iota :: [TestTree]
test_flatSlice_transpose_slice_iota =
  singleton $
    testCase "flatSlice . transpose . slice . iota " $
      compareOps $
        flatSlice (permute (slice (iota [20, 20]) $ Slice [DimSlice 1 5 2, DimSlice 0 5 2]) [1, 0]) flat_slice_1
  where
    flat_slice_1 = FlatSlice 1 [FlatDimIndex 2 2]

test_rotate_flatSlice_transpose_slice_iota :: [TestTree]
test_rotate_flatSlice_transpose_slice_iota =
  singleton $
    testCase "flatSlice . transpose . slice . iota " $
      compareOps $
        rotate (flatSlice (permute (slice (iota [20, 20]) $ Slice [DimSlice 1 5 2, DimSlice 1 5 2]) [1, 0]) flat_slice_1) [2, 1]
  where
    flat_slice_1 = FlatSlice 1 [FlatDimIndex 2 2]
