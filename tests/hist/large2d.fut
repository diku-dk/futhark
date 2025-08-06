-- Some tests to try out very large/sparse 2D histograms.
-- ==
-- tags { no_python no_wasm }
-- compiled input { 100i64      100i64 1000000i64 } auto output
-- compiled input { 1000i64    1000i64 1000000i64 } auto output

def main (n: i64) (m: i64) (k: i64) =
  reduce_by_index_2d (replicate m (replicate n 0))
                     (+)
                     0
                     (zip (map (% n) (iota k)) (map (% m) (iota k)))
                     (map i32.i64 (iota k))
  |> flatten
  |> i32.sum
