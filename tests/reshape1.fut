-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
-- }
-- output {
--   [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- }
fun intsqrt(x: int): int =
    int(sqrt32(f32(x)))

fun main (a: [n]int): [][]int =
    reshape (intsqrt(n), intsqrt(n)) a
