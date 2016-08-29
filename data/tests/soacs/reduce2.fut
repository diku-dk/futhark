-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
--   [1,2,3,4,5,6,7,8,9]
-- }
-- output {
--   285
-- }
fun main(a: []int, b: []int): int =
    reduce (+) 0 (map (*) (zip a b))
