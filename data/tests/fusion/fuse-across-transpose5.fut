-- ==
-- input {
--   [[1,2,3],[4,5,6],[7,8,9]]
-- }
-- output {
--   [[0, 1, 2], [0, 2, 4], [0, 3, 6]]
-- }
fun [][]int main([n][m]int a) =
  let foo = replicate(m, iota(n)) in
  let bar = replicate(m, iota(n)) in
  let b = replicate(n, iota(m)) in
  let c = map(fn []int ([]int xs, []int ys,[]int zs) =>
                map(fn int (int x, int y, int z) => x+y*z, zip(xs,ys,zs)),
              zip(foo,bar,transpose(b))) in
  c
