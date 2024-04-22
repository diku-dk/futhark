module Futhark.Pass.OptimiseArrayLayout.AnalysePrimExpTests (tests) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Futhark.Analysis.AnalysePrimExp
import Futhark.Analysis.PrimExp
import Futhark.IR.GPU
import Futhark.IR.GPUTests ()
import Futhark.IR.MC
import Futhark.IR.MCTests ()
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "AnalyzePrim" [stmToPrimExpsTests]

stmToPrimExpsTests :: TestTree
stmToPrimExpsTests =
  testGroup
    "stmToPrimExps"
    [stmToPrimExpsTestsGPU, stmToPrimExpsTestsMC]

stmToPrimExpsTestsGPU :: TestTree
stmToPrimExpsTestsGPU =
  testGroup
    "GPU"
    $ do
      let scope =
            M.fromList
              [ ("n_5142", FParamName "i64"),
                ("m_5143", FParamName "i64"),
                ("xss_5144", FParamName "[n_5142][m_5143]i64"),
                ("segmap_group_size_5201", LetName "i64"),
                ("segmap_usable_groups_5202", LetName "i64"),
                ("defunc_0_map_res_5203", LetName "[n_5142]i64"),
                ("defunc_0_f_res_5207", LetName "i64"),
                ("i_5208", IndexName Int64),
                ("acc_5209", FParamName "i64"),
                ("b_5210", LetName "i64"),
                ("defunc_0_f_res_5211", LetName "i64")
              ]
      [ testCase "BinOp" $ do
          let stm = "let {defunc_0_f_res_5211 : i64} = add64(acc_5209, b_5210)"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected =
                M.fromList
                  [ ( "defunc_0_f_res_5211",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp "acc_5209" (IntType Int64))
                            (LeafExp "b_5210" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected,
        testCase "Index" $ do
          let stm = "let {b_5210 : i64} = xss_5144[gtid_5204, i_5208]"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected = M.fromList [("b_5210", Nothing)]
          res @?= expected,
        testCase "Loop" $ do
          let stm = "let {defunc_0_f_res_5207 : i64} = loop {acc_5209 : i64} = {0i64} for i_5208:i64 < m_5143 do { {defunc_0_f_res_5211} }"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_f_res_5207", Nothing),
                    ("i_5208", Just (LeafExp "m_5143" (IntType Int64))),
                    ("acc_5209", Just (LeafExp "acc_5209" (IntType Int64)))
                  ]
          res @?= expected,
        testCase "Loop body" $ do
          let stm = "let {defunc_0_f_res_5207 : i64} = loop {acc_5209 : i64} = {0i64} for i_5208:i64 < m_5143 do { let {b_5210 : i64} = xss_5144[gtid_5204, i_5208] let {defunc_0_f_res_5211 : i64} = add64(acc_5209, b_5210) in {defunc_0_f_res_5211} }"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_f_res_5207", Nothing),
                    ("i_5208", Just (LeafExp "m_5143" (IntType Int64))),
                    ("acc_5209", Just (LeafExp "acc_5209" (IntType Int64))),
                    ("b_5210", Nothing),
                    ( "defunc_0_f_res_5211",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp "acc_5209" (IntType Int64))
                            (LeafExp "b_5210" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected,
        testCase "SegMap" $
          do
            let stm =
                  "let {defunc_0_map_res_5125 : [n_5142]i64} =\
                  \  segmap(thread; ; grid=segmap_usable_groups_5124; blocksize=segmap_group_size_5123)\
                  \  (gtid_5126 < n_5142) (~phys_tid_5127) : {i64} {\
                  \  return {returns lifted_lambda_res_5129} \
                  \}"
            let res = execState (stmToPrimExps scope stm) mempty
            let expected =
                  M.fromList
                    [ ("defunc_0_map_res_5125", Nothing),
                      ("gtid_5126", Just (LeafExp "gtid_5126" (IntType Int64)))
                    ]
            res @?= expected,
        testCase "SegMap body" $
          do
            let stm :: Stm GPU
                stm =
                  "let {defunc_0_map_res_5125 : [n_5142]i64} =\
                  \  segmap(thread; ; grid=segmap_usable_groups_5124; blocksize=segmap_group_size_5123)\
                  \  (gtid_5126 < n_5142) (~phys_tid_5127) : {i64} {\
                  \    let {eta_p_5128 : i64} =\
                  \      xs_5093[gtid_5126]\
                  \    let {lifted_lambda_res_5129 : i64} =\
                  \      add64(2i64, eta_p_5128)\
                  \    return {returns lifted_lambda_res_5129}\
                  \  }"
            let res = execState (stmToPrimExps scope stm) mempty
            let expected =
                  M.fromList
                    [ ("defunc_0_map_res_5125", Nothing),
                      ("gtid_5126", Just (LeafExp "gtid_5126" (IntType Int64))),
                      ("eta_p_5128", Nothing),
                      ( "lifted_lambda_res_5129",
                        Just
                          ( BinOpExp
                              (Add Int64 OverflowWrap)
                              (ValueExp (IntValue (Int64Value 2)))
                              (LeafExp "eta_p_5128" (IntType Int64))
                          )
                      )
                    ]
            res @?= expected
        ]

stmToPrimExpsTestsMC :: TestTree
stmToPrimExpsTestsMC =
  testGroup
    "MC"
    $ do
      let scope =
            M.fromList
              [ ("n_5142", FParamName "i64"),
                ("m_5143", FParamName "i64"),
                ("xss_5144", FParamName "[n_5142][5143]i64"),
                ("segmap_group_size_5201", LetName "i64"),
                ("segmap_usable_groups_5202", LetName "i64"),
                ("defunc_0_map_res_5203", LetName "[n_5142]i64"),
                ("defunc_0_f_res_5207", LetName "i64"),
                ("i_5208", IndexName Int64),
                ("acc_5209", FParamName "i64"),
                ("b_5210", LetName "i64"),
                ("defunc_0_f_res_5211", LetName "i64")
              ]
      [ testCase "BinOp" $ do
          let stm = "let {defunc_0_f_res_5211 : i64} = add64(acc_5209, b_5210)"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected =
                M.fromList
                  [ ( "defunc_0_f_res_5211",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp "acc_5209" (IntType Int64))
                            (LeafExp "b_5210" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected,
        testCase "Index" $ do
          let stm = "let {b_5210 : i64} = xss_5144[gtid_5204, i_5208]"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected = M.fromList [("b_5210", Nothing)]
          res @?= expected,
        testCase "Loop" $ do
          let stm = "let {defunc_0_f_res_5207 : i64} = loop {acc_5209 : i64} = {0i64} for i_5208:i64 < m_5143 do { {defunc_0_f_res_5211} }"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_f_res_5207", Nothing),
                    ("i_5208", Just (LeafExp "m_5143" (IntType Int64))),
                    ("acc_5209", Just (LeafExp "acc_5209" (IntType Int64)))
                  ]
          res @?= expected,
        testCase "Loop body" $ do
          let stm =
                "\
                \let {defunc_0_f_res_5207 : i64} =\
                \  loop {acc_5209 : i64} = {0i64}\
                \  for i_5208:i64 < m_5143 do {\
                \    let {b_5210 : i64} =\
                \      xss_5144[gtid_5204, i_5208]\
                \    let {defunc_0_f_res_5211 : i64} =\
                \      add64(acc_5209, b_5210)\
                \    in {defunc_0_f_res_5211}\
                \  }"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_f_res_5207", Nothing),
                    ("i_5208", Just (LeafExp "m_5143" (IntType Int64))),
                    ("acc_5209", Just (LeafExp "acc_5209" (IntType Int64))),
                    ("b_5210", Nothing),
                    ( "defunc_0_f_res_5211",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp "acc_5209" (IntType Int64))
                            (LeafExp "b_5210" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected,
        testCase "SegMap" $ do
          let stm =
                "let {defunc_0_map_res_5125 : [n_5142]i64} =\
                \  segmap()\
                \  (gtid_5126 < n_5142) (~flat_tid_5112) : {i64} {\
                \    return {returns lifted_lambda_res_5129}\
                \  }"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_map_res_5125", Nothing),
                    ("gtid_5126", Just (LeafExp "gtid_5126" (IntType Int64)))
                  ]
          res @?= expected,
        testCase "SegMap body" $ do
          let stm :: Stm MC
              stm =
                "let {defunc_0_map_res_5125 : [n_5142]i64} =\
                \  segmap()\
                \  (gtid_5126 < n_5142) (~flat_tid_5112) : {i64} {\
                \    let {eta_p_5128 : i64} =\
                \      xs_5093[gtid_5126]\
                \    let {lifted_lambda_res_5129 : i64} =\
                \      add64(2i64, eta_p_5128)\
                \    return {returns lifted_lambda_res_5129}\
                \  }"
          let res = execState (stmToPrimExps scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_map_res_5125", Nothing),
                    ("gtid_5126", Just (LeafExp "gtid_5126" (IntType Int64))),
                    ("eta_p_5128", Nothing),
                    ( "lifted_lambda_res_5129",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (ValueExp (IntValue (Int64Value 2)))
                            (LeafExp "eta_p_5128" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected
        ]
