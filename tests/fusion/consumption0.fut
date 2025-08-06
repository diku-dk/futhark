-- After fusion, consumes a0s.  See issue #224.
--
-- ==
-- structure { /Screma 1 }

def main [m] [b] (d: i32, a0s: [m][b][b]f32) : *[m][b][b]f32 =
  let a1s = map (\(x: [][]f32) : [b][b]f32 -> transpose x) a0s
  in map (\(a1: [][]f32) : *[b][b]f32 ->
            map (\(row: []f32) : *[b]f32 ->
                   copy row with [d] = 0f32)
                a1)
         a1s
