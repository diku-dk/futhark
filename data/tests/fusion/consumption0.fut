-- After fusion, consumes a0s.  See issue #224.
--
-- ==
-- structure { Map 2 }

fun *[m][b][b]f32 main(int d, [m][b][b]f32 a0s) =
  let a1s = map(fn [b][b]f32 ([b][b]f32 x) => transpose(x), a0s)
  in map (fn *[b][b]f32 (*[b][b]f32 a1) =>
            map ( fn *[b]f32 (*[b]f32 row) =>
                    let row[d] = 0f32
                    in row
                , a1 )
         , a1s)
