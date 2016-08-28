-- ==
-- input {
-- }
-- output {
--   [[0, 9, 18], [27, 36, 45], [54, 63, 72]]
-- }
fun main(): [][]int =
  let n = 9 in
  let a = map(fn (i: int): []int  =>
                replicate n i,
              iota(n)) in
  let b = reshape (3,3,9) a in
  map (fn (row: [][]int): []int  =>
         map (fn (x: []int): int  => reduce((+),0,x), row),
       b)
