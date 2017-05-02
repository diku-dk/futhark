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
--
-- structure distributed { Kernel 2 }

let main(a: [][#n][#m]i32): [][][]i32 =
  map (\(a_row: [][]i32): [m][n]i32  ->
        let b = map (\(a_row_row: []i32): []i32  ->
                      scan (+) 0 (a_row_row)
                   ) (a_row) in
        map (\(b_col: []i32): []i32  ->
             scan (+) 0 (b_col))
            (rearrange (1,0) b)
     ) a
