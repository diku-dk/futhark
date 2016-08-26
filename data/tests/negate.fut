-- Test that negation works for both integers and f64s.
-- ==
-- input {
--   [1,2,3]
-- }
-- output {
--   [-1, -2, -3]
--   [-1.000000, -2.000000, -3.000000]
-- }
fun main(a: []int): ([]int,[]f64) =
    (map(0-, a), map(0.0-, map(f64, a)))
