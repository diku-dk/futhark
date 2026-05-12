-- ==
-- structure gpu { SegMap 1 }
--

def main (outer_loop_count: i64, a: []i64) : [][]i64 =
  map (\(i: i64) ->
         let x = 10 * i
         in map (* x) a)
      (iota (outer_loop_count))
