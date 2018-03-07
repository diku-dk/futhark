-- Expected distributed structure:
--
-- map
--   map
-- map
--   map
--     map
-- concat
-- map
--   map
--     reduce (which becomes a segmented reduction)
--
-- ==
--
-- structure distributed {
--   Kernel 8
-- }

let main [n][an][bn] (a: [n][an][]i32, b: [n][bn]i32): ([][]i32,[][]i32) =
  unzip(map (\(a_row: [][]i32) (b_row: []i32): ([bn]i32,[an]i32)  ->
                  (map (\x -> x-1) (b_row),
                   map (\(a_row_row: []i32): i32  ->
                         let x = map (+1) (a_row_row) in
                         reduce (+) 0 (concat x x)
                      ) a_row)) a b)
