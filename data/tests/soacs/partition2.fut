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

fun ([]int, []int, []int, []int, []int, []int) main([]int xs, []int ys) =
  let (x,y,z) = partition(<, ==, zip(xs,ys))
  let (x1,x2) = unzip(x)
  let (y1,y2) = unzip(y)
  let (z1,z2) = unzip(z)
  in (x1, x2, y1, y2, z1, z2)
