-- Expected distributed structure:
--
-- map
--   map
-- map
--   map
--
-- ==
-- structure gpu { SegMap 5 }

def main [n] [an] [bn] (a: [n][an]i32, b: [n][bn]i32) : ([][]i32, [][]i32) =
  unzip (map2 (\(a_row: []i32) (b_row: []i32) : ([an]i32, [bn]i32) ->
                 ( map (+ 1) a_row
                 , map (\x -> x - 1) b_row
                 ))
              a
              b)
