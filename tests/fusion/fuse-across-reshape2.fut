-- ==
-- input {
-- }
-- output {
--   [[0, 9, 18], [27, 36, 45], [54, 63, 72]]
-- }
fun main(): [][]int =
  let n = 9
  let a = map (\(i: int): []int  ->
                replicate n i) (
              iota(n))
  let b = reshape (3,3,9) a in
  map  (\(row: [][]int): []int  ->
         map  (\(x: []int): int  -> reduce (+) 0 x) row) b
