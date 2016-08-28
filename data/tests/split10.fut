-- Split apart an array and return the pieces in another order.
-- ==
-- input {
--   2
--   3
--   [[4,3],[3,2],[2,1],[1,0]]
-- }
-- output {
--   [[1,0]]
--   [[4,3],[3,2]]
--   [[2,1]]
-- }
fun main(n: int, m: int, a: [][]int): ([][]int, [][]int, [][]int) =
  let (xs,ys,zs) = split (n,m) a
  in (zs,xs,ys)
