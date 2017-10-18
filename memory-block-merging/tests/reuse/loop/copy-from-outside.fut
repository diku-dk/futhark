-- When an loop contains a use of an array that is created before the loop, it
-- must not reuse that memory, because there are cycles in loops.  This program
-- has such a case, where acc is copied inside the loop.
--
-- The last use analysis needs to report a last use for the entire loop and
-- *not* for the statement inside the loop where it is seemingly lastly used.
--
-- Since this contains a nested map, and since we don't perform coalescing or
-- in-place lowering in the memory reuse tests, the CPU pipeline will have an
-- extra alloc for the inner loop.
-- ==
-- input {
--   [[[1,7],[9,4],[8,6]],
--    [[1,0],[6,4],[1,6]]]
-- }
-- output {
--   [[18, 17], [8, 10]]
-- }
-- structure cpu { Alloc 4 }
-- structure gpu { Alloc 3 }

let main [n] [m] [k] (a: [n][m][k]i32): [n][k]i32 =
  let acc = replicate k 0 in
  map (\a_r ->
         loop acc for i < m do
           map (+) acc a_r[i]
      ) a
