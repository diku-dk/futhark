-- This test fails if the ISWIM transformation messes up the size
-- annotations.
-- ==
-- input {
--   [1,1,1]
--   [[1,2,3],[4,5,6],[7,8,9],[0,1,2],[3,4,5]]
-- }
-- output {
--   [[3i32, 6i32, 11i32], [54i32, 162i32, 418i32], [2754i32, 10692i32, 34694i32], [5508i32, 32076i32, 208164i32], [60588i32, 577368i32, 5620428i32]]
-- }
def combineVs [n] (n_row: [n]i32) : [n]i32 =
  map2 (*) n_row n_row

def main [n] [m] (md_starts: [m]i32) (md_vols: [n][m]i32) : [][]i32 =
  let e_rows = map (\x -> map (+ 2) x) (map combineVs md_vols)
  in scan (\x y -> map2 (*) x y) md_starts e_rows
