{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.Mem.IxFunTests
  ( tests,
  )
where

import Data.Bifunctor
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.IxFun.Alg qualified as IxFunAlg
import Futhark.IR.Mem.IxFunWrapper
import Futhark.IR.Mem.IxFunWrapper qualified as IxFunWrap
import Futhark.IR.Mem.LMAD qualified as IxFunLMAD
import Futhark.IR.Prop
import Futhark.IR.Syntax
import Futhark.IR.Syntax.Core ()
import Futhark.Util.IntegralExp qualified as IE
import Futhark.Util.Pretty
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (span)
import Prelude qualified as P

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
      strides = drop 1 $ L.reverse $ scanl (*) 1 $ L.reverse dims
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

compareIxFuns :: Maybe (IxFunLMAD.LMAD Int) -> IxFunAlg.IxFun Int -> Assertion
compareIxFuns (Just ixfunLMAD) ixfunAlg =
  let lmadShape = IxFunLMAD.shape ixfunLMAD
      algShape = IxFunAlg.shape ixfunAlg
      points = allPoints lmadShape
      resLMAD = map (IxFunLMAD.index ixfunLMAD) points
      resAlg = map (IxFunAlg.index ixfunAlg) points
      errorMessage =
        T.unpack . docText $
          "lmad ixfun:  "
            <> pretty ixfunLMAD
              </> "alg ixfun:   "
            <> pretty ixfunAlg
              </> "lmad shape:  "
            <> pretty lmadShape
              </> "alg shape:   "
            <> pretty algShape
              </> "lmad points length: "
            <> pretty (length resLMAD)
              </> "alg points length:  "
            <> pretty (length resAlg)
              </> "lmad points: "
            <> pretty resLMAD
              </> "alg points:  "
            <> pretty resAlg
   in (lmadShape == algShape && resLMAD == resAlg) @? errorMessage
compareIxFuns Nothing ixfunAlg =
  assertFailure $
    unlines
      [ "lmad ixfun: Nothing",
        "alg ixfun:  " <> prettyString ixfunAlg
      ]

compareOps :: IxFunWrap.IxFun Int -> Assertion
compareOps (ixfunLMAD, ixfunAlg) = compareIxFuns ixfunLMAD ixfunAlg

compareOpsFailure :: IxFunWrap.IxFun Int -> Assertion
compareOpsFailure (Nothing, _) = pure ()
compareOpsFailure (Just ixfunLMAD, ixfunAlg) =
  assertFailure . T.unpack . docText $
    "Not supposed to be representable as LMAD."
      </> "lmad ixfun: "
      <> pretty ixfunLMAD
        </> "alg ixfun:  "
      <> pretty ixfunAlg

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
        test_slice_reshape_iota1,
        test_permute_slice_iota,
        test_reshape_iota,
        test_reshape_permute_iota,
        test_slice_reshape_iota2,
        test_reshape_slice_iota3,
        test_flatten_strided,
        test_complex1,
        test_complex2,
        test_expand1,
        test_expand2,
        test_expand3,
        test_expand4,
        test_flatSlice_iota,
        test_slice_flatSlice_iota,
        test_flatSlice_flatSlice_iota,
        test_flatSlice_slice_iota,
        test_flatSlice_transpose_slice_iota
        -- TODO: Without z3, these tests fail. Ideally, our internal simplifier
        -- should be able to handle them:
        --
        -- test_disjoint3
      ]

singleton :: TestTree -> [TestTree]
singleton = (: [])

test_iota :: [TestTree]
test_iota =
  singleton . testCase "iota" . compareOps $
    iota [n]

test_slice_iota :: [TestTree]
test_slice_iota =
  singleton . testCase "slice . iota" . compareOps $
    slice (iota [n, n, n]) slice3

test_slice_reshape_iota1 :: [TestTree]
test_slice_reshape_iota1 =
  singleton . testCase "slice . reshape . iota 1" . compareOps $
    slice (reshape (iota [n, n, n]) [n `P.div` 2, n `P.div` 3, 1]) slice3

test_permute_slice_iota :: [TestTree]
test_permute_slice_iota =
  singleton . testCase "permute . slice . iota" . compareOps $
    permute (slice (iota [n, n, n]) slice3) [1, 0]

test_reshape_iota :: [TestTree]
test_reshape_iota =
  -- This tests a pattern that occurs with ScalarSpace.
  singleton . testCase "reshape . zeroslice . iota" . compareOps $
    let s = Slice [DimSlice 0 n 0, DimSlice 0 n 1]
     in reshape (slice (iota [n, n]) s) [1, n, 1, n]

test_reshape_permute_iota :: [TestTree]
test_reshape_permute_iota =
  -- negative reshape test
  singleton . testCase "reshape . permute . iota" . compareOpsFailure $
    let newdims = [n * n, n]
     in reshape (permute (iota [n, n, n]) [1, 2, 0]) newdims

test_slice_reshape_iota2 :: [TestTree]
test_slice_reshape_iota2 =
  singleton . testCase "slice . reshape . iota 2" . compareOps $
    let newdims = [n * n, n]
        slc =
          Slice
            [ DimFix (n `P.div` 2),
              DimSlice 0 n 1
            ]
     in slice (reshape (iota [n, n, n, n]) newdims) slc

test_reshape_slice_iota3 :: [TestTree]
test_reshape_slice_iota3 =
  -- negative reshape test
  singleton . testCase "reshape . slice . iota 3" . compareOpsFailure $
    let newdims = [n * n, n]
        slc =
          Slice
            [ DimFix (n `P.div` 2),
              DimSlice 0 n 1,
              DimSlice 0 (n `P.div` 2) 1,
              DimSlice 0 n 1
            ]
     in reshape (slice (iota [n, n, n, n]) slc) newdims

-- Tests flattening something that is strided - this can occur after
-- memory expansion.
test_flatten_strided :: [TestTree]
test_flatten_strided =
  singleton . testCase "reshape . fix . iota 3d" . compareOps $
    let slc = Slice [DimSlice 0 n 1, DimSlice 0 2 1, DimFix 1]
     in reshape (slice (iota [n, 2, n * n]) slc) [2 * 10]

test_complex1 :: [TestTree]
test_complex1 =
  singleton . testCase "permute . slice . permute . slice . iota 1" . compareOps $
    let slice33 =
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
        ixfun' = slice ixfun slice1
     in ixfun'

test_complex2 :: [TestTree]
test_complex2 =
  singleton . testCase "permute . slice . permute . slice . iota 2" . compareOps $
    let slc2 =
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
        ixfun' = slice ixfun slice1
     in ixfun'

-- Imitates a case from memory expansion.
test_expand1 :: [TestTree]
test_expand1 =
  [ testCase "expand . iota1d" . compareOps $
      expand t nt (iota [n])
  ]
  where
    t = 3
    nt = 7

-- Imitates another case from memory expansion.
test_expand2 :: [TestTree]
test_expand2 =
  [ testCase "expand . iota2d" . compareOps $
      expand t nt (iota [n, n])
  ]
  where
    t = 3
    nt = 7

test_expand3 :: [TestTree]
test_expand3 =
  [ testCase "expand . permute . iota2d" . compareOps $
      expand t nt (permute (iota [n, n `div` 2]) [1, 0])
  ]
  where
    t = 3
    nt = 7

test_expand4 :: [TestTree]
test_expand4 =
  [ testCase "expand . slice . iota1d" . compareOps $
      expand t nt (slice (iota [n]) (Slice [DimSlice (n `div` 2) (n `div` 2) 1]))
  ]
  where
    t = 3
    nt = 7

test_flatSlice_iota :: [TestTree]
test_flatSlice_iota =
  singleton . testCase "flatSlice . iota" . compareOps $
    flatSlice (iota [n * n * n * n]) $
      FlatSlice 2 [FlatDimIndex (n * 2) 4, FlatDimIndex n 3, FlatDimIndex 1 2]

test_slice_flatSlice_iota :: [TestTree]
test_slice_flatSlice_iota =
  singleton . testCase "slice . flatSlice . iota " . compareOps $
    slice (flatSlice (iota [2 + n * n * n]) flat_slice) $
      Slice [DimFix 2, DimSlice 0 n 1, DimFix 0]
  where
    flat_slice = FlatSlice 2 [FlatDimIndex (n * n) 1, FlatDimIndex n 1, FlatDimIndex 1 1]

test_flatSlice_flatSlice_iota :: [TestTree]
test_flatSlice_flatSlice_iota =
  singleton . testCase "flatSlice . flatSlice . iota " . compareOps $
    flatSlice (flatSlice (iota [10 * 10]) flat_slice_1) flat_slice_2
  where
    flat_slice_1 = FlatSlice 17 [FlatDimIndex 3 27, FlatDimIndex 3 10, FlatDimIndex 3 1]
    flat_slice_2 = FlatSlice 2 [FlatDimIndex 2 (-2)]

test_flatSlice_slice_iota :: [TestTree]
test_flatSlice_slice_iota =
  singleton . testCase "flatSlice . slice . iota " . compareOps $
    flatSlice (slice (iota [210, 100]) $ Slice [DimSlice 10 100 2, DimFix 10]) flat_slice_1
  where
    flat_slice_1 = FlatSlice 17 [FlatDimIndex 3 27, FlatDimIndex 3 10, FlatDimIndex 3 1]

test_flatSlice_transpose_slice_iota :: [TestTree]
test_flatSlice_transpose_slice_iota =
  singleton . testCase "flatSlice . transpose . slice . iota " . compareOps $
    flatSlice (permute (slice (iota [20, 20]) $ Slice [DimSlice 1 5 2, DimSlice 0 5 2]) [1, 0]) flat_slice_1
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
--                 [ IxFunLMAD.LMADDim 256 0 (sub64 (num_blocks_8284) 1) 0 ,
--                   IxFunLMAD.LMADDim 1 0 16 1 ,
--                   IxFunLMAD.LMADDim 16 0 16 2
--                 ]
--             lm2 :: IxFunLMAD.LMAD (TPrimExp Int64 VName)
--             lm2 =
--               IxFunLMAD.LMAD
--                 (add_nw64 (add_nw64 (add_nw64 (add_nw64 (mul_nw64 (256) (num_blocks_8284)) (256)) (mul_nw64 (gtid_8472) (mul_nw64 (256) (num_blocks_8284)))) (mul_nw64 (gtid_8473) (256))) (mul_nw64 (gtid_8474) (16)))
--                 [IxFunLMAD.LMADDim 1 0 16 0 ]
--          in testCase (pretty lm1 <> " and " <> pretty lm2) $ IxFunLMAD.disjoint2 lessthans nonnegs lm1 lm2 @? "Failed"
--       ]

-- test_lessThanish :: [TestTree]
-- test_lessThanish =
--   [testCase "0 < 1" $ IxFunLMAD.lessThanish mempty mempty 0 1 @? "Failed"]

-- test_lessThanOrEqualish :: [TestTree]
-- test_lessThanOrEqualish =
--   [testCase "1 <= 1" $ IxFunLMAD.lessThanOrEqualish mempty mempty 1 1 @? "Failed"]

_test_disjoint3 :: [TestTree]
_test_disjoint3 =
  let foo s = VName (nameFromString s)
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
   in [ testCase "lm1 and lm2" $
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
                  (add_nw64 (mul64 block_size_12121 i_12214) (mul_nw64 (add_nw64 gtid_12553 1) (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) (sub_nw64 (sub_nw64 (add64 1 i_12214) gtid_12553) 1),
                    IxFunLMAD.LMADDim 1 (block_size_12121 + 1)
                  ]

              lm2 =
                IxFunLMAD.LMAD
                  (block_size_12121 * i_12214)
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) gtid_12553,
                    IxFunLMAD.LMADDim 1 (1 + block_size_12121)
                  ]

              lm_w =
                IxFunLMAD.LMAD
                  (add_nw64 (add64 (add64 1 n_blab) (mul64 block_size_12121 i_12214)) (mul_nw64 gtid_12553 (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim n_blab block_size_12121,
                    IxFunLMAD.LMADDim 1 block_size_12121
                  ]

              lm_blocks =
                IxFunLMAD.LMAD
                  (block_size_12121 * i_12214 + n_blab + 1)
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) (i_12214 + 1),
                    IxFunLMAD.LMADDim n_blab block_size_12121,
                    IxFunLMAD.LMADDim 1 block_size_12121
                  ]

              lm_lower_per =
                IxFunLMAD.LMAD
                  (block_size_12121 * i_12214)
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) (i_12214 + 1),
                    IxFunLMAD.LMADDim 1 (block_size_12121 + 1)
                  ]

              res1 = disjointTester asserts lessthans lm1 lm_w
              res2 = disjointTester asserts lessthans lm2 lm_w
              res3 = disjointTester asserts lessthans lm_lower_per lm_blocks
           in res1 && res2 && res3 @? "Failed",
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
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) (sub_nw64 (sub_nw64 (sub64 (sub64 (sdiv64 (sub64 n_blab 1) block_size_12121) i_12214) 1) gtid_12553) 1),
                    IxFunLMAD.LMADDim n_blab block_size_12121
                  ]

              lm2 =
                IxFunLMAD.LMAD
                  (add_nw64 (sub64 (sub64 (mul64 n_blab (add64 1 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) 1) (mul_nw64 (add_nw64 gtid_12553 1) (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) (sub_nw64 (sub_nw64 (sub64 (sub64 (sdiv64 (sub64 n_blab 1) block_size_12121) i_12214) 1) gtid_12553) 1),
                    IxFunLMAD.LMADDim 1 (1 + block_size_12121)
                  ]

              lm3 =
                IxFunLMAD.LMAD
                  (add64 n_blab (sub64 (sub64 (mul64 n_blab (add64 1 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) 1))
                  [ IxFunLMAD.LMADDim (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121)) gtid_12553,
                    IxFunLMAD.LMADDim n_blab block_size_12121
                  ]

              lm4 =
                IxFunLMAD.LMAD
                  (sub64 (sub64 (mul64 n_blab (add64 1 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) 1)
                  [ IxFunLMAD.LMADDim
                      (add_nw64 (mul_nw64 block_size_12121 n_blab) (mul_nw64 (-1) block_size_12121))
                      gtid_12553,
                    IxFunLMAD.LMADDim
                      1
                      (1 + block_size_12121)
                  ]

              lm_w =
                IxFunLMAD.LMAD
                  (add_nw64 (sub64 (mul64 n_blab (add64 2 (mul64 block_size_12121 (add64 1 i_12214)))) block_size_12121) (mul_nw64 gtid_12553 (sub64 (mul64 block_size_12121 n_blab) block_size_12121)))
                  [ IxFunLMAD.LMADDim n_blab block_size_12121,
                    IxFunLMAD.LMADDim 1 block_size_12121
                  ]

              res1 = disjointTester asserts lessthans lm1 lm_w
              res2 = disjointTester asserts lessthans lm2 lm_w
              res3 = disjointTester asserts lessthans lm3 lm_w
              res4 = disjointTester asserts lessthans lm4 lm_w
           in res1 && res2 && res3 && res4 @? "Failed " <> show [res1, res2, res3, res4],
        testCase "lud long" $
          let lessthans =
                [ bimap
                    (head . namesToList . freeIn)
                    untyped
                    (step, num_blocks - 1 :: TPrimExp Int64 VName)
                ]

              step = TPrimExp $ LeafExp (foo "step" 1337) $ IntType Int64

              num_blocks = TPrimExp $ LeafExp (foo "n" 1338) $ IntType Int64

              lm1 =
                IxFunLMAD.LMAD
                  (1024 * num_blocks * (1 + step) + 1024 * step)
                  [ IxFunLMAD.LMADDim (1024 * num_blocks) (num_blocks - step - 1),
                    IxFunLMAD.LMADDim 32 32,
                    IxFunLMAD.LMADDim 1 32
                  ]

              lm_w1 =
                IxFunLMAD.LMAD
                  (1024 * num_blocks * step + 1024 * step)
                  [ IxFunLMAD.LMADDim 32 32,
                    IxFunLMAD.LMADDim 1 32
                  ]

              lm_w2 =
                IxFunLMAD.LMAD
                  ((1 + step) * 1024 * num_blocks + (1 + step) * 1024)
                  [ IxFunLMAD.LMADDim (1024 * num_blocks) (num_blocks - step - 1),
                    IxFunLMAD.LMADDim 1024 (num_blocks - step - 1),
                    IxFunLMAD.LMADDim 1024 1,
                    IxFunLMAD.LMADDim 32 1,
                    IxFunLMAD.LMADDim 128 8,
                    IxFunLMAD.LMADDim 4 8,
                    IxFunLMAD.LMADDim 32 4,
                    IxFunLMAD.LMADDim 1 4
                  ]

              asserts =
                [ untyped ((1 :: TPrimExp Int64 VName) .<. num_blocks :: TPrimExp Bool VName)
                ]

              res1 = disjointTester asserts lessthans lm1 lm_w1
              res2 = disjointTester asserts lessthans lm1 lm_w2
           in res1 && res2 @? "Failed"
      ]
