-- Example of chaining coalescing inside a block
-- ==
-- input { 0
--         0
--         0
--         [1, 2, 3, 4]
--         [[1, 2, 4, 5], [3, 4, 5 ,6]]
--         [ [ [1, 2, 3, 4], [3, 4, 5, 6] ]
--         , [ [5, 6, 7, 8], [7, 8, 9, 9] ]
--         , [ [9, 9, 9, 9], [8, 8, 8, 8] ]
--         , [ [7, 7, 7, 7], [6, 6, 6, 6] ]
--         ]
--       }
-- output { [ [ [1, 2, 3, 4], [3, 4, 5, 6] ]
--          , [ [5, 6, 7, 8], [7, 8, 9, 9] ]
--          , [ [9, 9, 9, 9], [8, 8, 8, 8] ]
--          , [ [1, 2, 3, 4], [4, 5, 6, 7] ]
--          ]
--        }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 3 }

-- Needs two-dimensional overlap checking

def main [m] [n]
         (i1: i32)
         (i2: i32)
         (k: i32)
         (a: [n]i32)
         (v: [m][n]i32)
         (z: *[n][m][n]i32) : *[n][m][n]i32 =
  let u = map (\x -> map (+ 1) x) v
  let b = map (+ i1) a
  let ind1 = z[k, i1, i2] - b[0]
  let u[ind1] = b
  -- This should not coalesce

  let c = map (+ i2) a
  let u[i1 + i2] = c
  -- Coalescing

  -- let z[i1+i2+k] = u
  let z[k + i32.i64 m + 1] = u
  -- Coalescing
  in z
