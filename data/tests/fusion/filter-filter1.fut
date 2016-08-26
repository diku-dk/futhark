-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
--   [10,20,30,40,50,60,70,80,90]
-- }
-- output {
--   [20, 30, 40, 60, 80, 90]
-- }
fun div2(x: int): bool = x % 2 == 0

fun div3(x: int): bool = x % 3 == 0

fun main(a: []int, b: []int): []int =
  let (c1,c2) = unzip(filter(fn (x: int, y: int): bool  =>
                               div2(x) || div3(y),
                             zip(a,b))) in
  filter(div2, c2)
