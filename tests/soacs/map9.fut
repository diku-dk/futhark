-- This test fails if the ISWIM transformation messes up the size
-- annotations.
-- ==
-- input {
--   [1,2,3]
--   [[1,2,3],[4,5,6],[7,8,9],[0,1,2],[3,4,5]]
-- }
-- output {
--   [[3, 12, 33], [54, 324, 1254], [2754, 21384, 104082], [5508, 64152, 624492], [60588, 1154736, 16861284]]
-- }
let combineVs [n] (n_row: [n]i32): [n]i32 =
  map2 (*) n_row n_row

let main [n][m] (md_starts: [m]i32) (md_vols: [n][m]i32): [][]i32 =
  let e_rows = map (\x -> map (+2) x) (map combineVs md_vols)
  in scan (\x y -> map2 (*) x y) md_starts e_rows
