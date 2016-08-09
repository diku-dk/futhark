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

fun *[n_histo]f32 main(int n_histo, int n_image) =
  let as = iota(n_image) in
  streamRedPer( fn *[]f32 ([]f32 a, []f32 b) =>
                     zipWith( +, a, b )
              , fn *[]f32 (int chunk, *[]f32 acc, []int a) =>
                     loop (acc) = for i < chunk do
                         let ind = a[i] % n_histo      in
                         unsafe let acc[ind] = acc[ind] + 1.0 in
                         acc
                     in  acc
              , replicate(n_histo,0.0), as
              )
