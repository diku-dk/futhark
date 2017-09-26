-- Expected distributed structure:
--
-- map
--   map
-- map
--   map
--
-- ==
-- structure distributed { Kernel 2 }

let main [n][an] [bn] (a: [n][an]i32, b: [n][bn]i32): ([][]i32,[][]i32) =
  unzip(map (\(a_row: []i32) (b_row: []i32): ([an]i32,[bn]i32)  ->
                  (map (+1) a_row,
                   map (-1) b_row)) a b)
