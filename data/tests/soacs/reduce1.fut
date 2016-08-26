-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
-- }
-- output {
--   362880
-- }
fun main(a: []int): int = reduce(*, 1, a)
