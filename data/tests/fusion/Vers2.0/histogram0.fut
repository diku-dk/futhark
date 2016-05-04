-- ==
-- input {
--   (3, 300)
-- }
-- output {
--   [100.0,100.0,100.0]
-- }
-- structure distributed {
--   Iota 0
-- }
fun *[f64,n_histo] main(int n_histo, int n_image) =
  let A = iota(n_image) in
  streamRedPer( fn *[f64] ([f64] a, [f64] b) =>
                     zipWith( +, a, b )
              , fn *[f64] (int chunk, *[f64] acc, [int] a) =>
                     loop (acc) = for i < chunk do
                         let ind = a[i] % n_histo      in
                         unsafe let acc[ind] = acc[ind] + 1.0 in
                         acc
                     in  acc
              , replicate(n_histo,0.0), A
              )
