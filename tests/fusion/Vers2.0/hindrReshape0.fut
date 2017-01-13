-- ==
-- input {
-- }
-- output {
--   [1, 2, 3, 4, 5, 6, 7, 8, 9]
--   [[2, 4, 6], [8, 10, 12], [14, 16, 18]]
-- }
fun main(): ([]int,[][]int) =
  let n = 9
  let a = map (+1) (iota(n))
  let b = reshape (3,3) a
  let c = map  (\(row: []int): []int  ->
                    map  (\(x: int): int  -> x*2) row
              ) b
  in (a,c)
