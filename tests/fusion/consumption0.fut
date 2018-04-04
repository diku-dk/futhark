-- After fusion, consumes a0s.  See issue #224.
--
-- ==
-- structure { Map 2 }

let main [m][b] (d: i32, a0s: [m][b][b]f32): *[m][b][b]f32 =
  let a1s = map (\(x: [][]f32): [b][b]f32  -> rearrange (1,0) x) a0s
  in map  (\(a1: [][]f32): *[b][b]f32  ->
            map  (\(row: []f32): *[b]f32  ->
                    copy row with [d] <- 0f32
                ) a1
         ) a1s
