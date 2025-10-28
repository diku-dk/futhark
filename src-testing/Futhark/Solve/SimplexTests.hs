{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Futhark.Solve.SimplexTests
  ( tests,
  )
where

import Data.Vector.Unboxed qualified as V
import Futhark.Solve.LP
import Futhark.Solve.Matrix qualified as M
import Futhark.Solve.Simplex
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (or)

tests :: TestTree
tests =
  testGroup
    "SimplexTests"
    [ testCase "1" $
        let lpe =
              LPE
                { pc = V.fromList [1, 1, 0, 0, 0],
                  pA =
                    M.fromLists
                      [ [-1, 1, 1, 0, 0],
                        [1, 0, 0, 1, 0],
                        [0, 1, 0, 0, 1]
                      ],
                  pd = V.fromList [1, 3, 2]
                }
         in simplex lpe @?= Just (5 :: Double, V.fromList [3, 2, 2, 0, 0]),
      testCase "2" $
        let lp =
              LP
                { lpc = V.fromList [40, 30],
                  lpA =
                    M.fromLists
                      [ [1, 1],
                        [2, 1]
                      ],
                  lpd = V.fromList [12, 16]
                }
         in simplexLP lp @?= Just (400 :: Double, V.fromList [4, 8]),
      testCase "3" $
        let lp =
              LP
                { lpc = V.fromList [1, 2, 3],
                  lpA =
                    M.fromLists
                      [ [1, 1, 1],
                        [2, 1, 3]
                      ],
                  lpd = V.fromList [12, 18]
                }
         in simplexLP lp @?= Just (27 :: Double, V.fromList [0, 9, 3]),
      testCase "4" $
        let lp =
              LP
                { lpc = V.fromList [5.5, 2.1],
                  lpA =
                    M.fromLists
                      [ [-1, 1],
                        [8, 2]
                      ],
                  lpd = V.fromList [2, 17]
                }
         in assertBool (show $ simplexLP lp) $
              case simplexLP lp of
                Nothing -> False
                Just (z, sol) ->
                  (z `approxEq` (14.08 :: Double))
                    && and (zipWith approxEq (V.toList sol) [1.3, 3.3]),
      testCase "5" $
        let lp =
              LP
                { lpc = V.fromList [0],
                  lpA =
                    M.fromLists
                      [ [1],
                        [-1]
                      ],
                  lpd = V.fromList [0, 0]
                }
         in assertBool (show $ simplexLP lp) $
              case simplexLP lp of
                Nothing -> False
                Just (z, sol) ->
                  (z `approxEq` (0 :: Double))
                    && and (zipWith approxEq (V.toList sol) [0]),
      testCase "6" $
        let lp =
              LP
                { lpc = V.fromList [1],
                  lpA =
                    M.fromLists
                      [ [1],
                        [-1]
                      ],
                  lpd = V.fromList [5, 5]
                }
         in assertBool (show $ simplexLP lp) $
              case simplexLP lp of
                Nothing -> False
                Just (z, sol) ->
                  z `approxEq` (5 :: Double)
                    && and (zipWith approxEq (V.toList sol) [5]),
      testCase "7" $
        let prog =
              LinearProg
                { optType = Maximize,
                  objective = var "x1",
                  constraints =
                    [ var "x1" ~<=~ 10 ~*~ var "b1",
                      var "b1" ~+~ var "b2" ~<=~ constant 1
                    ]
                }
            (lp, _idxmap) = linearProgToLP prog
         in assertBool
              (unlines [show $ simplexLP lp])
              $ case simplexLP lp of
                Nothing -> False
                Just (z, sol) ->
                  (z `approxEq` (10 :: Double))
                    && and (zipWith (==) (V.toList sol) [1, 0, 10]),
      testCase "8" $
        let prog =
              LinearProg
                { optType = Maximize,
                  objective = var "x1" ~+~ var "x2",
                  constraints =
                    [ var "x1" ~<=~ constant 10,
                      var "x2" ~<=~ constant 5
                    ]
                      <> oneIsZero ("b1", "x1") ("b2", "x2")
                }
            (lp, _idxmap) = linearProgToLP prog
         in assertBool
              (unlines [show $ simplexLP lp])
              $ case simplexLP lp of
                Nothing -> False
                Just (z, _sol) ->
                  and
                    [ z `approxEq` (15 :: Double)
                    ],
      -- testCase "9" $
      --  let prog =
      --        LinearProg
      --          { optType = Maximize,
      --            objective = var "x1" ~+~ var "x2",
      --            constraints =
      --              [ var "x1" ~<=~ constant 10,
      --                var "x2" ~<=~ constant 5
      --              ]
      --                <> or "b1" "b2" (var "x1" ~==~ constant 0) (var "x2" ~==~ constant 0)
      --          }
      --      (lp, idxmap) = linearProgToLP prog
      --      lpe = convert lp
      --   in trace
      --        (unlines [show prog, show lp, show idxmap, show lpe])
      --        ( assertBool
      --            (unlines [show $ simplexLP lp])
      --            $ case simplexLP lp of
      --              Nothing -> False
      --              Just (z, sol) ->
      --                and
      --                  [ z `approxEq` (15 :: Double)
      --                  ]
      --        ),
      testCase "10" $
        let prog =
              LinearProg
                { optType = Minimize,
                  objective = var "R2" ~+~ var "M3",
                  constraints =
                    [ var "artifical4" ~==~ constant 1 ~+~ var "t0",
                      constant 1 ~+~ var "num1" ~==~ constant 1 ~+~ var "t0",
                      var "b_R2" ~<=~ constant 1,
                      var "b_M3" ~<=~ constant 1,
                      var "R2" ~<=~ 1000 ~*~ var "b_R2",
                      var "M3" ~<=~ 1000 ~*~ var "b_M3",
                      var "b_R2" ~+~ var "b_M3" ~<=~ constant 1
                    ]
                }
            (lp, _idxmap) = linearProgToLP prog
         in assertBool
              (unlines [show $ simplexLP lp])
              $ case simplexLP lp of
                Nothing -> False
                Just (z, _sol) ->
                  and
                    [ z `approxEq` (0 :: Double)
                    ],
      testCase "11" $
        let prog =
              LinearProg
                { optType = Minimize,
                  objective = var "4R" ~+~ var "5M",
                  constraints =
                    [ var "6artifical" ~==~ constant 1 ~+~ var "2t",
                      constant 1 ~+~ var "3num" ~==~ constant 1 ~+~ var "2t",
                      var "0b_R" ~<=~ constant 1,
                      var "1b_M" ~<=~ constant 1,
                      var "4R" ~<=~ 1000 ~*~ var "0b_R",
                      var "5M" ~<=~ 1000 ~*~ var "1b_M",
                      var "0b_R" ~+~ var "1b_M" ~<=~ constant 1
                    ]
                }
            (lp, _idxmap) = linearProgToLP prog
         in assertBool
              (unlines [show $ simplexLP lp])
              $ case simplexLP lp of
                Nothing -> False
                Just (z, _sol) ->
                  and
                    [ z `approxEq` (0 :: Double)
                    ]
    ]

approxEq :: (Fractional a, Ord a) => a -> a -> Bool
approxEq x1 x2 = abs (x1 - x2) < 10 ^^ (-10 :: Int)
