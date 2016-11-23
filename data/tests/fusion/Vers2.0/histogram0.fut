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

fun main(n_histo: int, n_image: int): *[n_histo]f32 =
  let as = iota(n_image) in
  streamRedPer (fn (a: []f32) (b: []f32): *[]f32  =>
                     map (+) a b
              ) (fn (chunk: int) (acc: *[]f32) (a: []int): *[]f32  =>
                     loop (acc) = for i < chunk do
                         let ind = a[i] % n_histo      in
                         unsafe let acc[ind] = acc[ind] + 1.0 in
                         acc
                     in  acc
              ) (replicate n_histo 0.0) as
