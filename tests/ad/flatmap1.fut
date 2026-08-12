-- As flatmap0.fut, but where the flatmap produces multidimensional arrays -
-- both the nonuniform (concatenated) result and the uniform one.
-- ==
-- entry: fwd_map fwd_vec
-- input { [1i64,2i64,3i64] [4.0,5.0,6.0] }
-- output {
-- [[[1.0, 8.0], [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], [0.0, 0.0]],
--  [[0.0, 0.0], [1.0, 10.0], [1.0, 10.0], [0.0, 0.0], [0.0, 0.0], [0.0, 0.0]],
--  [[0.0, 0.0], [0.0, 0.0], [0.0, 0.0], [1.0, 12.0], [1.0, 12.0], [1.0, 12.0]]]
-- [[[2.0, 3.0], [0.0, 0.0], [0.0, 0.0]],
--  [[0.0, 0.0], [2.0, 3.0], [0.0, 0.0]],
--  [[0.0, 0.0], [0.0, 0.0], [2.0, 3.0]]]
-- }

def primal (ks: []i64) (xs: []f64) =
  let f (k: i64, x: f64) = (replicate k [x, x * x], [x * 2, x * 3])
  let (_, _, _, a, b) = flatmap f (zip ks xs)
  in (take (i64.sum ks) a, b)

entry fwd_map [n] ks (xs: [n]f64) : ([][][]f64, [][][]f64) =
  unzip
  <| tabulate n \i ->
       jvp (primal ks) xs (replicate n 0 with [i] = 1)

entry fwd_vec [n] ks (xs: [n]f64) : ([][][]f64, [][][]f64) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in unzip <| jmp (primal ks) xs seeds
