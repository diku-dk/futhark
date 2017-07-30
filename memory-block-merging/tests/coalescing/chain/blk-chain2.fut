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
-- output { [ [ [1, 2], [4, 5] ]
--          , [ [5, 6], [7, 8] ]
--          ]
--        }
-- structure cpu { Alloc 1 }

-- With the current code, there are only 2 coalescing opportunities, namely
-- `u[i1+i2] = c` and `u[ind1] = b`.  This is because the use of `z` in the
-- definition of `ind1` prevents the coalescing of `z[i1+i2+k] = u`.
--
-- However if `ind1` is computed as `i1+1` (uncomment) then all three coalescing
-- opportunities should be exploited, i.e., including `z[i1+i2+k] = u`.

let main (i1: i32, i2: i32, k: i32
         , a: [#n]i32, v: [#m][#n]i32
         , z: *[#n][#m][#n]i32)
         : *[n][m][n]i32 =
  let u          = map (\x -> map (+1) x) v

  let b          = map (+i1) a
  let ind1       = z[k,i1,i2] - b[0] -- i1 + 1
  let u[ind1]    = b -- Coalescing.

  let c          = map (+i2) a
  let u[i1+i2]   = c -- Coalescing

  let z[i1+i2+k] = u
  in  z
