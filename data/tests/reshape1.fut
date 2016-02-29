-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
-- }
-- output {
--   [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- }
fun int intsqrt(int x) =
    int(sqrt32(f32(x)))

fun [[int]] main ([int] a) =
    let n = size(0, a) in
    reshape((intsqrt(n), intsqrt(n)), a)
