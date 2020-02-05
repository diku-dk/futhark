-- ==
-- input {
--   3 300
-- }
-- output {
--   [100.0f32, 100.0f32, 100.0f32]
-- }
-- structure distributed {
--   Iota 0
-- }

let main(n_histo: i32) (n_image: i32): [n_histo]f32 =
  let as = iota(n_image) in
  reduce_stream_per (\a b  ->
                       map2 (+) a b)
                    (\chunk (a: [chunk]i32)  ->
                       loop acc = replicate n_histo 0.0 for i < chunk do
                       let ind = a[i] % n_histo      in
                       unsafe let acc[ind] = acc[ind] + 1.0 in
                              acc)
                    as
