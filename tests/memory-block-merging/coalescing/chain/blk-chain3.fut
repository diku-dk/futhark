-- Example of chaining coalescing inside a block
-- ==
-- input { 0
--         0
--         0
--         [1, 2]
--         [[1, 2], [3, 4]]
--         [ [ [1, 2], [3, 4] ]
--         , [ [5, 6], [7, 8] ] ]
--       }
-- output { [ [ [1, 2], [1, 2] ]
--          , [ [5, 6], [7, 8] ]
--          ]
--        }
-- structure seq-mem { Alloc 0 }
-- structure gpu-mem { Alloc 0 }

-- This is blk-chain2.fut with the alternative ind1

let main [m] [n] (i1: i32) (i2: i32) (k: i32)
                 (a: [n]i32) (v: [m][n]i32)
                 (z: *[n][m][n]i32)
                 : *[n][m][n]i32 =
  let u          = map (\x -> map (+1) x) v

  let b          = map (+i1) a
  let ind1       = i1 + 1
  let u[ind1]    = b -- Coalescing.

  let c          = map (+i2) a
  let u[i1+i2]   = c -- Coalescing

  let z[i1+i2+k] = u
  in  z
