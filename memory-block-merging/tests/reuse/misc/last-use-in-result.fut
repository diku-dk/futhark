-- The last use of x1 is in the body result.  If body results are ignored, the
-- last use will seemingly be just before the last array creation, which will
-- then get to reuse memory, which is forbidden.
--
-- Based on tests/soacs/partition2.fut.
-- ==
-- input {
--   [2,7,1,2,8,9,1,5,0,2]
--   [6,1,0,9,8,7,9,4,2,1]
-- }
-- output {
--   [2, 2, 1, 0]
--   [6, 9, 9, 2]
--   [8]
--   [8]
--   [7, 1, 9, 5, 2]
--   [1, 0, 7, 4, 1]
-- }
-- structure cpu { Alloc 10 }

-- Many allocations, not all clear.  But the important thing is that there
-- should be no reuse in the CPU pipeline.  The GPU pipeline can have some.

let main (xs: []i32, ys: []i32): ([]i32, []i32, []i32, []i32, []i32, []i32) =
  let (x, y, z) = partition ((\(x, y) -> x < y), (\(x, y) -> x == y)) (zip xs ys)
  let (x1, x2) = unzip x
  let (y1, y2) = unzip y
  let (z1, z2) = unzip z
  in (x1, x2, y1, y2, z1, z2)
