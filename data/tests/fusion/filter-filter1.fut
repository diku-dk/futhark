-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
--   [10,20,30,40,50,60,70,80,90]
-- }
-- output {
--   [20, 30, 40, 60, 80, 90]
-- }
fun bool div2(int x) = x % 2 == 0

fun bool div3(int x) = x % 3 == 0

fun [int] main([int] a, [int] b) =
  let (c1,c2) = unzip(filter(fn bool (int x, int y) =>
                               div2(x) || div3(y),
                             zip(a,b))) in
  filter(div2, c2)
