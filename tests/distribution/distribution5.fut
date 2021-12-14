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
-- structure gpu {
--   SegMap 2 SegRed 1
-- }

def main [k][n][an][bn] (a: [n][an][k]i32) (b: [n][bn]i32): ([][]i32,[][]i32) =
  #[incremental_flattening(only_inner)]
  unzip(
    #[incremental_flattening(only_inner)]
    map2 (\(a_row: [][]i32) (b_row: []i32): ([bn]i32,[an]i32)  ->
            #[incremental_flattening(only_inner)]
            (map (\x -> x-1) (b_row),
             map (\(a_row_row: []i32): i32  ->
                    let x = map (+1) (a_row_row) in
                    reduce (+) 0 (concat x x)
                 ) a_row)) a b)
