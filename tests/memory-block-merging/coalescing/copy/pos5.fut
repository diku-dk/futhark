-- Memory block merging with a copy into a multidimensional array given as a
-- function parameter.
-- ==
-- input { [[[0, 1, 2],
--           [0, 1, 2],
--           [0, 1, 2]],
--          [[1, 2, 3],
--           [4, 5, 6],
--           [7, 8, 9]],
--          [[0, 0, 0],
--           [0, 0, 0],
--           [0, 0, 0]]]
--         1i64
--         [7, 0, 7]
--       }
-- output { [[[0, 1, 2],
--            [0, 1, 2],
--            [0, 1, 2]],
--           [[77, 77, 77],
--            [0, 0, 0],
--            [77, 77, 77]],
--           [[0, 0, 0],
--            [0, 0, 0],
--            [0, 0, 0]]]
--        }
-- compiled random input { [256][256][256]i32 1i64 [256]i32 }
-- auto output
-- structure seq-mem { /Alloc 1 }
-- structure gpu-mem { /Alloc 0 }

let main [n] (t1: *[n][n][n]i32) (i: i64) (xs: [n]i32): *[n][n][n]i32 =
  #[incremental_flattening(only_intra)]
  let t0 = map (\x ->
                  loop res = replicate n x for j < 10 do
                    map (+ x) res)
               xs -- Will use the memory of t1[i].

  -- This is the basis array in which everything will be put.
  let t1[i] = t0
  in t1
