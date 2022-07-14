{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.Mem.IxFunTests
  ( tests,
  )
where

import Data.Function ((&))
import qualified Data.List as DL
import qualified Data.Map as M
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.IR.Mem.IxFun as IxFunLMAD
import qualified Futhark.IR.Mem.IxFun.Alg as IxFunAlg
import Futhark.IR.Mem.IxFunWrapper
import qualified Futhark.IR.Mem.IxFunWrapper as IxFunWrap
import Futhark.IR.Prop
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
  pow = (P.^)
  sgn = Just . P.signum

allPoints :: [Int] -> [[Int]]
allPoints dims =
  let total = product dims
      strides = drop 1 $ DL.reverse $ scanl (*) 1 $ DL.reverse dims
   in map (unflatInd strides) [0 .. total - 1]
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
        "lmad ixfun:  "
          ++ PR.pretty ixfunLMAD
          ++ "\n"
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
        test_flatSlice_iota,
        test_slice_flatSlice_iota,
        test_flatSlice_flatSlice_iota,
        test_flatSlice_slice_iota,
        test_flatSlice_rotate_iota,
        test_flatSlice_rotate_slice_iota,
        test_flatSlice_transpose_slice_iota,
        test_rotate_flatSlice_transpose_slice_iota,
        -- test_disjoint2,
        -- test_lessThanish,
        -- test_lessThanOrEqualish,
        test_disjoint3
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
                  DimSlice (n - 1) n (-1),
                  DimSlice 0 n 1,
                  DimSlice (n - 1) n (-1)
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
                [ DimSlice (n - 1) (n `P.div` 3) (-1),
                  DimSlice (n - 1) n (-1),
                  DimSlice (n - 1) n (-1),
                  DimSlice 0 n 1
                ]
            ixfun = permute (slice (iota [n, n, n, n, n]) slice33) [3, 1, 2, 0]
            m = n `P.div` 3
            slice1 =
              Slice
                [ DimSlice 0 n 1,
                  DimSlice (n - 1) n (-1),
                  DimSlice (n - 1) n (-1),
                  DimSlice 1 (m - 2) (-1)
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
                  DimSlice (n - 1) (n `P.div` 3) (-1),
                  DimSlice (n - 1) n (-1),
                  DimSlice (n - 1) n (-1),
                  DimSlice 0 n 1
                ]
            ixfun = permute (slice (iota [n, n, n, n, n]) slc2) [3, 1, 2, 0]
            m = n `P.div` 3
            slice1 =
              Slice
                [ DimSlice 0 n 1,
                  DimSlice (n - 1) n (-1),
                  DimSlice (n - 1) n (-1),
                  DimSlice 1 (m - 2) (-1)
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
                  DimSlice 2 (n - 2) 1,
                  DimSlice 3 (n - 3) 1
                ]
            ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
            ixfn_orig = rotate (permute (iota [n - 3, n - 2]) [1, 0]) [1, 2]
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
                  DimSlice (n - 1) (n - 2) (-1),
                  DimSlice (n - 1) (n - 3) (-1)
                ]
            slice_orig =
              Slice
                [ DimSlice (n - 4) (n - 3) (-1),
                  DimSlice (n - 3) (n - 2) (-1)
                ]
            ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
            ixfn_orig = rotate (permute (slice (iota [n - 3, n - 2]) slice_orig) [1, 0]) [1, 2]
            ixfn_rebase = rebase ixfn_base ixfn_orig
         in ixfn_rebase

test_rebase3 :: [TestTree]
test_rebase3 =
  singleton $
    testCase "rebase full orig but not monotonic" $
      compareOps $
        let n2 = (n - 2) `P.div` 3
            n3 = (n - 3) `P.div` 2
            slice_base =
              Slice
                [ DimFix (n `P.div` 2),
                  DimSlice (n - 1) n2 (-3),
                  DimSlice (n - 1) n3 (-2)
                ]
            slice_orig =
              Slice
                [ DimSlice (n3 - 1) n3 (-1),
                  DimSlice (n2 - 1) n2 (-1)
                ]
            ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
            ixfn_orig = rotate (permute (slice (iota [n3, n2]) slice_orig) [1, 0]) [1, 2]
            ixfn_rebase = rebase ixfn_base ixfn_orig
         in ixfn_rebase

test_rebase4_5 :: [TestTree]
test_rebase4_5 =
  let n2 = (n - 2) `P.div` 3
      n3 = (n - 3) `P.div` 2
      slice_base =
        Slice
          [ DimFix (n `P.div` 2),
            DimSlice (n - 1) n2 (-3),
            DimSlice 3 n3 2
          ]
      slice_orig =
        Slice
          [ DimSlice (n3 - 1) n3 (-1),
            DimSlice 0 n2 1
          ]
      ixfn_base = rotate (permute (slice (iota [n, n, n]) slice_base) [1, 0]) [2, 1]
      ixfn_orig = rotate (permute (slice (iota [n3, n2]) slice_orig) [1, 0]) [1, 2]
   in [ testCase "rebase mixed monotonicities" $
          compareOps $
            rebase ixfn_base ixfn_orig
      ]

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

-- test_disjoint2 :: [TestTree]
-- test_disjoint2 =
--   let add_nw64 = (+)

--       mul_nw64 = (*)

--       sub64 = (-)

--       vname s i = VName (nameFromString s) i
--    in [ let gtid_8472 = TPrimExp $ LeafExp (vname "gtid" 8472) $ IntType Int64

--             gtid_8473 = TPrimExp $ LeafExp (vname "gtid" 8473) $ IntType Int64

--             gtid_8474 = TPrimExp $ LeafExp (vname "gtid" 8474) $ IntType Int64

--             num_blocks_8284 = TPrimExp $ LeafExp (vname "num_blocks" 8284) $ IntType Int64

--             nonnegs = freeIn [gtid_8472, gtid_8473, gtid_8474, num_blocks_8284]

--             j_m_i_8287 :: TPrimExp Int64 VName
--             j_m_i_8287 = num_blocks_8284 - 1

--             lessthans :: [(VName, PrimExp VName)]
--             lessthans =
--               [ (head $ namesToList $ freeIn gtid_8472, untyped j_m_i_8287),
--                 (head $ namesToList $ freeIn gtid_8473, untyped j_m_i_8287),
--                 (head $ namesToList $ freeIn gtid_8474, untyped (16 :: TPrimExp Int64 VName))
--               ]

--             lm1 :: IxFunLMAD.LMAD (TPrimExp Int64 VName)
--             lm1 =
--               IxFunLMAD.LMAD
--                 256
--                 [ IxFunLMAD.LMADDim 256 0 (sub64 (num_blocks_8284) 1) 0 IxFunLMAD.Inc,
--                   IxFunLMAD.LMADDim 1 0 16 1 IxFunLMAD.Inc,
--                   IxFunLMAD.LMADDim 16 0 16 2 IxFunLMAD.Inc
--                 ]
--             lm2 :: IxFunLMAD.LMAD (TPrimExp Int64 VName)
--             lm2 =
--               IxFunLMAD.LMAD
--                 (add_nw64 (add_nw64 (add_nw64 (add_nw64 (mul_nw64 (256) (num_blocks_8284)) (256)) (mul_nw64 (gtid_8472) (mul_nw64 (256) (num_blocks_8284)))) (mul_nw64 (gtid_8473) (256))) (mul_nw64 (gtid_8474) (16)))
--                 [IxFunLMAD.LMADDim 1 0 16 0 IxFunLMAD.Inc]
--          in testCase (pretty lm1 <> " and " <> pretty lm2) $ IxFunLMAD.disjoint2 lessthans nonnegs lm1 lm2 @? "Failed"
--       ]

-- test_lessThanish :: [TestTree]
-- test_lessThanish =
--   [testCase "0 < 1" $ IxFunLMAD.lessThanish mempty mempty 0 1 @? "Failed"]

-- test_lessThanOrEqualish :: [TestTree]
-- test_lessThanOrEqualish =
--   [testCase "1 <= 1" $ IxFunLMAD.lessThanOrEqualish mempty mempty 1 1 @? "Failed"]

test_disjoint3 :: [TestTree]
test_disjoint3 =
  let foo s i = VName (nameFromString s) i
      add_nw64 = (+)
      add64 = (+)
      mul_nw64 = (*)
      mul64 = (*)
      sub64 = (-)
      sdiv64 = IE.div
      sub_nw64 = (-)
      disjointTester asserts lessthans lm1 lm2 =
        let nonnegs = map (`LeafExp` IntType Int64) $ namesToList $ freeIn lm1 <> freeIn lm2

            scmap =
              M.fromList $
                map (\x -> (x, Prim $ IntType Int64)) $
                  namesToList $
                    freeIn lm1 <> freeIn lm2 <> freeIn lessthans <> freeIn asserts
         in IxFunLMAD.disjoint3 scmap asserts lessthans nonnegs lm1 lm2
   in [ testCase "lm1 and lm2" $ do
          let lessthans =
                [ ( i_12214,
                    sdiv64 (sub64 n_blab 1) block_size_12121
                  ),
                  (gtid_12553, add64 1 i_12214)
                ]
                  & map (\(v, p) -> (head $ namesToList $ freeIn v, untyped p))

              asserts =
                [ untyped ((2 * block_size_12121 :: TPrimExp Int64 VName) .<. n_blab :: TPrimExp Bool VName),
                  untyped ((3 :: TPrimExp Int64 VName) .<. n_blab :: TPrimExp Bool VName)
                ]

              block_size_12121 = TPrimExp $ LeafExp (foo "block_size" 12121) $ IntType Int64
              i_12214 = TPrimExp $ LeafExp (foo "i" 12214) $ IntType Int64
              n_blab = TPrimExp $ LeafExp (foo "n" 1337) $ IntType Int64
              gtid_12553 = TPrimExp $ LeafExp (foo "gtid" 12553) $ IntType Int64

          let lm1 =
                IxFunLMAD.LMAD
                  (add_nw64 (mul64 block_size_12121 i_12214) (mul_nw64 (add_nw64 gtid_12553 1) (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) 0 (sub_nw64 (sub_nw64 (add64 1 i_12214) gtid_12553) 1) 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 (block_size_12121 + 1) 1 IxFunLMAD.Inc
                  ]

              lm2 =
                IxFunLMAD.LMAD
                  (block_size_12121 * i_12214)
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) 0 gtid_12553 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 (1 + block_size_12121) 1 IxFunLMAD.Inc
                  ]

              lm_w =
                IxFunLMAD.LMAD
                  (add_nw64 (add64 (add64 1 n_blab) (mul64 block_size_12121 i_12214)) (mul_nw64 gtid_12553 (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim n_blab 0 block_size_12121 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 block_size_12121 1 IxFunLMAD.Inc
                  ]

              lm_blocks =
                IxFunLMAD.LMAD
                  (block_size_12121 * i_12214 + n_blab + 1)
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) 0 (i_12214 + 1) 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim n_blab 0 block_size_12121 1 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 block_size_12121 2 IxFunLMAD.Inc
                  ]

              lm_lower_per =
                IxFunLMAD.LMAD
                  (block_size_12121 * i_12214)
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) 0 (i_12214 + 1) 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 (block_size_12121 + 1) 1 IxFunLMAD.Inc
                  ]

          res1 <- disjointTester asserts lessthans lm1 lm_w
          res2 <- disjointTester asserts lessthans lm2 lm_w
          res3 <- disjointTester asserts lessthans lm_lower_per lm_blocks

          res1 && res2 && res3 @? "Failed",
        testCase "nw second half" $ do
          let lessthans =
                [ ( i_12214,
                    sdiv64 (sub64 n_blab 1) block_size_12121
                  ),
                  (gtid_12553, add64 1 i_12214)
                ]
                  & map (\(v, p) -> (head $ namesToList $ freeIn v, untyped p))

              asserts =
                [ untyped ((2 * block_size_12121 :: TPrimExp Int64 VName) .<. n_blab :: TPrimExp Bool VName),
                  untyped ((3 :: TPrimExp Int64 VName) .<. n_blab :: TPrimExp Bool VName)
                ]

              block_size_12121 = TPrimExp $ LeafExp (foo "block_size" 12121) $ IntType Int64
              i_12214 = TPrimExp $ LeafExp (foo "i" 12214) $ IntType Int64
              n_blab = TPrimExp $ LeafExp (foo "n" 1337) $ IntType Int64
              gtid_12553 = TPrimExp $ LeafExp (foo "gtid" 12553) $ IntType Int64

              lm1 =
                IxFunLMAD.LMAD
                  (add_nw64 (add64 n_blab (sub64 (sub64 (mul64 n_blab (add64 1 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) 1)) (mul_nw64 (add_nw64 gtid_12553 1) (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) 0 (sub_nw64 (sub_nw64 (sub64 (sub64 (sdiv64 (sub64 n_blab 1) block_size_12121) i_12214) 1) gtid_12553) 1) 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim n_blab 0 block_size_12121 1 IxFunLMAD.Inc
                  ]

              lm2 =
                IxFunLMAD.LMAD
                  (add_nw64 (sub64 (sub64 (mul64 n_blab (add64 1 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) 1) (mul_nw64 (add_nw64 gtid_12553 1) (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) 0 (sub_nw64 (sub_nw64 (sub64 (sub64 (sdiv64 (sub64 n_blab 1) block_size_12121) i_12214) 1) gtid_12553) 1) 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 (1 + block_size_12121) 1 IxFunLMAD.Inc
                  ]

              lm3 =
                IxFunLMAD.LMAD
                  (add64 n_blab (sub64 (sub64 (mul64 n_blab (add64 1 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) 1))
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) 0 gtid_12553 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim n_blab 0 block_size_12121 1 IxFunLMAD.Inc
                  ]

              lm4 =
                IxFunLMAD.LMAD
                  (sub64 (sub64 (mul64 n_blab (add64 1 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) 1)
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) 0 gtid_12553 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 (1 + block_size_12121) 1 IxFunLMAD.Inc
                  ]

              lm_w =
                IxFunLMAD.LMAD
                  (add_nw64 (sub64 (mul64 n_blab (add64 2 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) (mul_nw64 gtid_12553 (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim n_blab 0 block_size_12121 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 block_size_12121 1 IxFunLMAD.Inc
                  ]

          res1 <- disjointTester asserts lessthans lm1 lm_w
          res2 <- disjointTester asserts lessthans lm2 lm_w
          res3 <- disjointTester asserts lessthans lm3 lm_w
          res4 <- disjointTester asserts lessthans lm4 lm_w
          res1 && res2 && res3 && res4 @? "Failed " <> pretty [res1, res2, res3, res4],
        testCase "lud long" $ do
          let lessthans =
                [ (step, num_blocks - 1 :: TPrimExp Int64 VName)
                ]
                  & map (\(v, p) -> (head $ namesToList $ freeIn v, untyped p))

              step = TPrimExp $ LeafExp (foo "step" 1337) $ IntType Int64

              num_blocks = TPrimExp $ LeafExp (foo "n" 1338) $ IntType Int64

              lm1 =
                IxFunLMAD.LMAD
                  (1024 * num_blocks * (1 + step) + 1024 * step)
                  [ IxFunLMAD.LMADDim (1024 * num_blocks) 0 (num_blocks - step - 1) 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 32 0 32 1 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 32 2 IxFunLMAD.Inc
                  ]

              lm_w1 =
                IxFunLMAD.LMAD
                  (1024 * num_blocks * step + 1024 * step)
                  [ IxFunLMAD.LMADDim 32 0 32 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 32 1 IxFunLMAD.Inc
                  ]

              lm_w2 =
                IxFunLMAD.LMAD
                  ((1 + step) * 1024 * num_blocks + (1 + step) * 1024)
                  [ IxFunLMAD.LMADDim (1024 * num_blocks) 0 (num_blocks - step - 1) 0 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1024 0 (num_blocks - step - 1) 1 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1024 0 1 2 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 32 0 1 3 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 128 0 8 4 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 4 0 8 5 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 32 0 4 6 IxFunLMAD.Inc,
                    IxFunLMAD.LMADDim 1 0 4 7 IxFunLMAD.Inc
                  ]

              asserts =
                [ untyped ((1 :: TPrimExp Int64 VName) .<. num_blocks :: TPrimExp Bool VName)
                ]

          res1 <- disjointTester asserts lessthans lm1 lm_w1
          res2 <- disjointTester asserts lessthans lm1 lm_w2
          res1 && res2 @? "Failed"
      ]
