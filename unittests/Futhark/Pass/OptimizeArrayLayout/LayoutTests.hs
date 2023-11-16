module Futhark.Pass.OptimizeArrayLayout.LayoutTests (tests) where

import Data.IntMap.Strict qualified as S
import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern
import Futhark.FreshNames
import Futhark.IR.GPU (GPU)
import Futhark.Pass.OptimizeArrayLayout.Layout
import Language.Futhark.Core
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "LayoutTests"
    [commonPermutationEliminatorsTests]

commonPermutationEliminatorsTests :: TestTree
commonPermutationEliminatorsTests =
  testGroup
    "commonPermutationEliminators"
    [permutationTests, nestTests, dimAccessTests, constInLastIndexElimTests]

permutationTests :: TestTree
permutationTests =
  testGroup "Permutations" $
    do
      let names = generateNames 2
      let dimAccesses = [singleParAccess 0 0, singleSeqAccess 1 1] <*> names
      [ testCase (unwords [show perm, "->", show res]) $
          commonPermutationEliminators perm [] dimAccesses @?= res
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
              ([0, 0, 0], True)
            ]
        ]

nestTests :: TestTree
nestTests = testGroup "Nests" $
  do
    let names = generateNames 2
    let dimAccesses = [singleParAccess 0 0, singleSeqAccess 1 1] <*> names
    [ testCase (unwords [args, "->", show res]) $
        commonPermutationEliminators [1, 0] nest dimAccesses @?= res
      | (args, nest, res) <-
          [ ("[]", [], False),
            ("[CondBodyName]", [CondBodyName] <*> names, False),
            ("[SegOpName]", [SegOpName . SegmentedMap] <*> names, True),
            ("[LoopBodyName]", [LoopBodyName] <*> names, True),
            ("[SegOpName, CondBodyName]", [SegOpName . SegmentedMap, CondBodyName] <*> names, True),
            ("[CondBodyName, LoopBodyName]", [CondBodyName, LoopBodyName] <*> names, True)
          ]
      ]

dimAccessTests :: TestTree
dimAccessTests = testGroup "DimAccesses" [] -- TODO: Write tests for the part of commonPermutationEliminators that checks the complexity of the DimAccesses.

constInLastIndexElimTests :: TestTree
constInLastIndexElimTests =
  testGroup
    "constantInLastIndexElimination"
    [ testCase "gpu eliminates indexes with constant in last dim" $
        permutationTableFromIndexTable accessTableGPU @?= mempty,
      testCase "gpu ignores when not last" $
        let res = permutationTableFromIndexTable accessTableGPUrev
         in assertBool ("Expected not mempty, got " ++ show (M.toList res)) $
              not $
                null res
    ]
  where
    accessTableGPU :: IndexTable GPU
    accessTableGPU =
      singleAccess
        [ singleParAccess 0 0 $ VName "gtid" 4,
          singleSeqAccess 1 1 $ VName "i" 5,
          DimAccess mempty 2
        ]

    accessTableGPUrev :: IndexTable GPU
    accessTableGPUrev =
      singleAccess
        [ singleParAccess 0 1 $ VName "gtid" 4,
          singleParAccess 1 2 $ VName "gtid" 5,
          DimAccess mempty 2,
          singleSeqAccess 3 1 $ VName "i" 6
        ]

singleAccess :: [DimAccess rep] -> IndexTable rep
singleAccess dims =
  M.fromList
    [ ( sgOp,
        M.fromList
          [ ( (VName "A" 2, []),
              M.fromList
                [ ( VName "a" 3,
                    dims
                  )
                ]
            )
          ]
      )
    ]
  where
    sgOp = SegmentedMap {vnameFromSegOp = VName "mapres" 1}

singleParAccess :: Int -> Int -> VName -> DimAccess rep
singleParAccess origDim level name =
  DimAccess
    (S.singleton 0 (name, level, Parallel, ThreadID))
    origDim

singleSeqAccess :: Int -> Int -> VName -> DimAccess rep
singleSeqAccess origDim level name =
  DimAccess
    (S.singleton 0 (name, level, Sequential, LoopVar))
    origDim

generateNames :: Int -> [VName]
generateNames count = do
  let (name, source) = newName blankNameSource (VName "i" 0)
  fst $ foldl f ([name], source) [1 .. count - 1]
  where
    f (names, source) _ = do
      let (name, source') = newName source (last names)
      (names ++ [name], source')
