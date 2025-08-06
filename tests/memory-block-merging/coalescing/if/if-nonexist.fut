-- Tricky if-coalescing
-- ==
-- input {  [ [ [1,2], [3,4] ]
--          , [ [5,6], [7,8] ]
--          ]
--          [1,2]
--       }
-- output {
--          [ [ [1i32, 2i32], [3i32, 4i32] ]
--          , [ [0i32, 0i32], [2i32, 4i32] ]
--          ]
--        }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

-- Number of coalescing is 1, but corresponds to 4 coalescing
-- operations on the same memory block, i.e.,
--   (i) `y[1] = z2`, at the very bottom
--  (ii) `z2 = z0` where `z0` is the result of the then branch,
-- (iii) `z2 = z1` where `z1` is the result of the else branch,
--  (iv) and finaly the creation of `z` (transitive closure
--       added by `z2` and `z1`).
-- Basically, since the memory block of the if-result is not
-- existensial then we can track the creation of `z` outside
-- the branches. Note that `z[0] = x2` and `z[1] = x2` are not
-- coalesced.
def main [n] (y: *[n][n][n]i32) (a: [n]i32) : *[n][n][n]i32 =
  let z = replicate n (replicate n 0)
  let x2 = map (* 2) a
  -- The sole allocation.  This could be stored in either
  -- z[0] or z[1], but both might need it, so we do not
  -- merge memory.
  let z2 =
    if (n > 3)
    then let z[0] = x2
         in z
    else let z[1] = x2
         in z
  let y[1] = z2
  in y
