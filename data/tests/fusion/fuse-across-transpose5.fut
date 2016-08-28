-- ==
-- input {
--   [[1,2,3],[4,5,6],[7,8,9]]
-- }
-- output {
--   [[0, 1, 2], [0, 2, 4], [0, 3, 6]]
-- }
fun main(a: [n][m]int): [][]int =
  let foo = replicate m (iota n) in
  let bar = replicate m (iota n) in
  let b = replicate n (iota m) in
  let c = map(fn (xs: []int, ys: []int,zs: []int): []int  =>
                map(fn (x: int, y: int, z: int): int  => x+y*z, zip(xs,ys,zs)),
              zip(foo,bar,transpose(b))) in
  c
