module Futhark.Optimise.ArrayLayout.LayoutTests (tests) where

import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.PrimExp
import Futhark.FreshNames
import Futhark.IR.GPU (GPU)
import Futhark.IR.GPUTests ()
import Futhark.Optimise.ArrayLayout.Layout
import Language.Futhark.Core
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Layout"
    [commonPermutationEliminatorsTests]

commonPermutationEliminatorsTests :: TestTree
commonPermutationEliminatorsTests =
  testGroup
    "commonPermutationEliminators"
    [permutationTests, nestTests, dimAccessTests, constIndexElimTests]

permutationTests :: TestTree
permutationTests =
  testGroup "Permutations" $
    do
      -- This isn't the way to test this, in reality we should provide realistic
      -- access patterns that might result in the given permutations.
      -- Luckily we only use the original access for one check atm.
      [ testCase (unwords [show perm, "->", show res]) $
          commonPermutationEliminators perm [] @?= res
        | (perm, res) <-
            [ ([0], True),
              ([1, 0], False),
              ([0, 1], True),
              ([0, 0], True),
              ([1, 1], True),
              ([1, 2, 0], False),
              ([2, 0, 1], False),
              ([0, 1, 2], True),
              ([1, 0, 2], True),
              ([2, 1, 0], True),
              ([2, 2, 0], True),
              ([2, 1, 1], True),
              ([1, 0, 1], True),
              ([0, 0, 0], True),
              ([0, 1, 2, 3, 4], True),
              ([1, 0, 2, 3, 4], True),
              ([2, 3, 0, 1, 4], True),
              ([3, 4, 2, 0, 1], True),
              ([2, 3, 4, 0, 1], False),
              ([1, 2, 3, 4, 0], False),
              ([3, 4, 0, 1, 2], False)
            ]
        ]

nestTests :: TestTree
nestTests = testGroup "Nests" $
  do
    let names = generateNames 2
    [ testCase (unwords [args, "->", show res]) $
        commonPermutationEliminators [1, 0] nest @?= res
      | (args, nest, res) <-
          [ ("[]", [], False),
            ("[CondBodyName]", [CondBodyName] <*> names, False),
            ("[SegOpName]", [SegOpName . SegmentedMap] <*> names, True),
            ("[LoopBodyName]", [LoopBodyName] <*> names, False),
            ("[SegOpName, CondBodyName]", [SegOpName . SegmentedMap, CondBodyName] <*> names, True),
            ("[CondBodyName, LoopBodyName]", [CondBodyName, LoopBodyName] <*> names, False)
          ]
      ]

dimAccessTests :: TestTree
dimAccessTests = testGroup "DimAccesses" [] -- TODO: Write tests for the part of commonPermutationEliminators that checks the complexity of the DimAccesses.

constIndexElimTests :: TestTree
constIndexElimTests =
  testGroup
    "constIndexElimTests"
    [ testCase "gpu eliminates indexes with constant in any dim" $ do
        let primExpTable =
              M.fromList
                [ ("gtid_4", Just (LeafExp "n_4" (IntType Int64))),
                  ("i_5", Just (LeafExp "n_4" (IntType Int64)))
                ]
        layoutTableFromIndexTable primExpTable accessTableGPU @?= mempty,
      testCase "gpu ignores when not last" $ do
        let primExpTable =
              M.fromList
                [ ("gtid_4", Just (LeafExp "gtid_4" (IntType Int64))),
                  ("gtid_5", Just (LeafExp "gtid_5" (IntType Int64))),
                  ("i_6", Just (LeafExp "i_6" (IntType Int64)))
                ]
        layoutTableFromIndexTable primExpTable accessTableGPUrev
          @?= M.fromList
            [ ( SegmentedMap "mapres_1",
                M.fromList
                  [ ( ("a_2", [], [0, 1, 2, 3]),
                      M.fromList [("A_3", [2, 3, 0, 1])]
                    )
                  ]
              )
            ]
    ]
  where
    accessTableGPU :: IndexTable GPU
    accessTableGPU =
      singleAccess
        [ singleParAccess 0 "gtid_4",
          DimAccess mempty Nothing,
          singleSeqAccess 1 "i_5"
        ]

    accessTableGPUrev :: IndexTable GPU
    accessTableGPUrev =
      singleAccess
        [ singleParAccess 1 "gtid_4",
          singleParAccess 2 "gtid_5",
          singleSeqAccess 0 "i_5",
          singleSeqAccess 2 "gtid_4"
        ]

singleAccess :: [DimAccess rep] -> IndexTable rep
singleAccess dims =
  M.fromList
    [ ( sgOp,
        M.fromList
          [ ( ("A_2", [], [0, 1, 2, 3]),
              M.fromList
                [ ( "a_3",
                    dims
                  )
                ]
            )
          ]
      )
    ]
  where
    sgOp = SegmentedMap {vnameFromSegOp = "mapres_1"}

singleParAccess :: Int -> VName -> DimAccess rep
singleParAccess level name =
  DimAccess
    (M.singleton name $ Dependency level ThreadID)
    (Just name)

singleSeqAccess :: Int -> VName -> DimAccess rep
singleSeqAccess level name =
  DimAccess
    (M.singleton name $ Dependency level LoopVar)
    (Just name)

generateNames :: Int -> [VName]
generateNames count = do
  let (name, source) = newName blankNameSource "i_0"
  fst $ foldl f ([name], source) [1 .. count - 1]
  where
    f (names, source) _ = do
      let (name, source') = newName source (last names)
      (names ++ [name], source')
