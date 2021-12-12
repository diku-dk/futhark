-- ==
-- input {
--   3i64 300i64
-- }
-- output {
--   [100.0f32, 100.0f32, 100.0f32]
-- }
-- structure gpu {
--   Iota 0
-- }

def main(n_histo: i64) (n_image: i64): [n_histo]f32 =
  let as = map i32.i64 (iota n_image) in
  reduce_stream_per (\a b  ->
                       map2 (+) a b)
                    (\chunk (a: [chunk]i32)  ->
                       loop acc = replicate n_histo 0.0 for i < chunk do
                       let ind = a[i] % i32.i64 n_histo
                       let acc[ind] = acc[ind] + 1.0
                       in acc)
                    as
