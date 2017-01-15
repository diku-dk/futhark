-- After fusion, consumes a0s.  See issue #224.
--
-- ==
-- structure { Map 2 }

fun main(d: i32, a0s: [m][b][b]f32): *[m][b][b]f32 =
  let a1s = map (\(x: [b][b]f32): [b][b]f32  -> transpose(x)) a0s
  in map  (\(a1: *[b][b]f32): *[b][b]f32  ->
            map  (\(row: *[b]f32): *[b]f32  ->
                    let row[d] = 0f32
                    in row
                ) a1
         ) a1s
