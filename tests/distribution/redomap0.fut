-- Distribute a redomap inside of a map.
-- ==
-- structure gpu { SegRed 1 }

def main (a: [][]i32) : []i32 =
  map (\(a_r: []i32) : i32 ->
         reduce (+) 0 (map (+ 1) (a_r)))
      a
