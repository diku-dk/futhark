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
fun combineVs(n_row: []i32): []i32 =
  map (*) n_row n_row

fun main(md_starts: []i32, md_vols: [][]i32, md_drifts: [][]i32): [][]i32 =
  let e_rows = map (\(x: []i32): []i32  -> map (+2) x) (
                   map combineVs (md_vols))
  in  scan (\(x: []i32) (y: []i32): []i32 -> map (*) x y) (md_starts) (e_rows )
