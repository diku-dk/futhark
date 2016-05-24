-- Slightly more complicated test involving arrays of tuples.
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

fun ([(int,int)], [(int,int)], [(int,int)]) main([int] xs, [int] ys) =
  partition(<, ==, zip(xs,ys))
