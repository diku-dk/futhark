-- ==
-- entry: main
-- "six_rows_one_step" input {
--   1i64
--   [[1i64, 1i64, 1i64, 1i64, 1i64, 1i64],
--    [2i64, 2i64, 2i64, 2i64, 2i64, 2i64],
--    [3i64, 3i64, 3i64, 3i64, 3i64, 3i64],
--    [4i64, 4i64, 4i64, 4i64, 4i64, 4i64],
--    [5i64, 5i64, 5i64, 5i64, 5i64, 5i64],
--    [6i64, 6i64, 6i64, 6i64, 6i64, 6i64]]
-- } output {
--   [[7i64, 7i64, 7i64, 7i64, 7i64, 7i64],
--    [14i64, 14i64, 14i64, 14i64, 14i64, 14i64],
--    [21i64, 21i64, 21i64, 21i64, 21i64, 21i64],
--    [28i64, 28i64, 28i64, 28i64, 28i64, 28i64],
--    [35i64, 35i64, 35i64, 35i64, 35i64, 35i64],
--    [42i64, 42i64, 42i64, 42i64, 42i64, 42i64]]
-- }

def work (steps: i64) (v: [6]i64) : [6]i64 =
  loop a = copy v
  for _i < steps do
    let s = reduce (+) 0i64 a
    in map (\x -> x + s) a

entry main (steps: i64) (vs: [][6]i64) : [][6]i64 =
  #[flattening(only_outer)]
  map (work steps) vs

-- The initial value of the loop parameter is invariant, but the loop
-- body makes it different for each thread.
-- ==
-- entry: invariant_init
-- input {
--   2i64
--   [[1i64, 1i64, 1i64, 1i64, 1i64, 1i64],
--    [2i64, 2i64, 2i64, 2i64, 2i64, 2i64],
--    [3i64, 3i64, 3i64, 3i64, 3i64, 3i64],
--    [4i64, 4i64, 4i64, 4i64, 4i64, 4i64],
--    [5i64, 5i64, 5i64, 5i64, 5i64, 5i64],
--    [6i64, 6i64, 6i64, 6i64, 6i64, 6i64]]
-- } auto output

entry invariant_init (steps: i64) (vs: [][6]i64) : [][6]i64 =
  #[flattening(only_outer)]
  map (\v ->
         loop a = replicate 6 0
         for _i < steps do
           let s = reduce (+) 0i64 a
           in map2 (\x y -> x + s + y) a v)
      vs

-- The trip count of the loop is the thread index itself.
-- ==
-- entry: thread_index_bound
-- input { 5i64 [1f32, 2f32, 3f32] [4f32, 5f32, 6f32] } auto output

entry thread_index_bound (n: i64) (xs: []f32) (ys: []f32) : []f32 =
  #[flattening(only_outer)]
  map (\i ->
         loop acc = 0f32
         for j < i do
           acc + f32.sum (map2 (\x y -> x * y + f32.i64 j) xs ys))
      (iota n)
