-- ==
-- input {
--   [3,5,-2,3,4,-30]
--   [-4,10,1,-8,2,4]
-- }
-- output {
--   [1, 4]
-- }
fun [int] main([int] a, [int] b) =
  let (c,d) = unzip(filter(fn bool (int x, int y) => x+y < 0, zip(a,b))) in
  filter(0<, d)
