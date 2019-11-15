-- Expected distributed structure:
--
-- map
--   map
--     scan
-- map
--   map
--     scan
--
-- ==
-- compiled random input { [10][16][16]i32 } auto output
-- compiled random input { [10][8][32]i32 } auto output
-- structure distributed { SegScan 2 }

let main [k][n][m] (a: [k][n][m]i32): [][][]i32 =
  map (\(a_row: [][]i32): [m][n]i32  ->
        let b = map (\(a_row_row: []i32)  ->
                      scan (+) 0 (a_row_row)
                   ) (a_row) in
        map (\(b_col: []i32)  ->
             scan (+) 0 (b_col))
            (transpose b)
     ) a
