-- Negative Example of If-Coalescing.
-- ==
-- input {  [[1,2], [3,4]]
--          [1,2]
--          [3,4]
--       }
-- output {
--          [ [1i32, 2i32], [2i32, 3i32] ]
--          3i32
--        }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

-- There should be no coalescing here because `x` is
-- used during the lifetime of `r`, which also prevents
-- coalescing of the `z` in `x`!
def main [n] (x: *[n][n]i32) (a: [n]i32) (b: [n]i32) : (*[n][n]i32, i32) =
  let (z, s) =
    if (x[0, 0]) > 0
    then let r = map (+ 1) a
         let q = x[x[0, 0], 0]
         in (r, q)
    else (map (* 2) b, 2)
  let x[n / 2] = z
  in (x, s)
