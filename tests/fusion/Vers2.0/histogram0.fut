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

default(f32)

fun main(n_histo: i32, n_image: i32): *[n_histo]f32 =
  let as = iota(n_image) in
  stream_red_per (\(a: []f32) (b: []f32): *[]f32  ->
                     map (+) a b
              ) (\(a: [chunk]i32): *[]f32  ->
                     loop (acc = replicate n_histo 0.0) = for i < chunk do
                         let ind = a[i] % n_histo      in
                         unsafe let acc[ind] = acc[ind] + 1.0 in
                         acc
                     in  acc
              ) as
