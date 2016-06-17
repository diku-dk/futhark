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
fun []int combineVs([]int n_row) =
  map(*, zip(n_row, n_row))

fun [][]int main([]int md_starts, [][]int md_vols, [][]int md_drifts) =
  let e_rows = map(fn []int ([]int x) => map(+ (2), x),
                   map(combineVs, md_vols))
  in  scan( fn []int ([]int x, []int y) => map(*, zip(x, y)), md_starts, e_rows )
