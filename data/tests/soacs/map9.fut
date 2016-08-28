-- This test fails if the ISWIM transformation messes up the size
-- annotations.
-- ==
-- input {
--   [1,2,3]
--   [[1,2,3],[4,5,6],[7,8,9],[0,1,2],[3,4,5]]
--   [[1,2,3],[4,5,6],[7,8,9],[0,1,2],[3,4,5]]
-- }
-- output {
--   [[3, 12, 33], [54, 324, 1254], [2754, 21384, 104082], [5508, 64152, 624492], [60588, 1154736, 16861284]]
-- }
fun combineVs(n_row: []int): []int =
  map (*) (zip(n_row, n_row))

fun main(md_starts: []int, md_vols: [][]int, md_drifts: [][]int): [][]int =
  let e_rows = map (fn (x: []int): []int  => map (+2) x) (
                   map combineVs (md_vols))
  in  scan (fn (x: []int) (y: []int): []int  => map (*) (zip(x, y))) (md_starts) (e_rows )
