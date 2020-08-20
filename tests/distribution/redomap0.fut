-- Distribute a redomap inside of a map.
-- ==
-- structure distributed { SegRed 1 }

let main(a: [][]i32): []i32 =
  map (\(a_r: []i32): i32  ->
        reduce (+) 0 (map (+1) (a_r))) a
