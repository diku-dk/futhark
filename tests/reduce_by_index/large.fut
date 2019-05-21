-- Some tests to try out very large/sparse histograms.
-- ==
-- no_python compiled input { 10000000 1000 }     output { 499500i32 }
-- no_python compiled input { 100000000 10000 }   output { 49995000i32 }
-- no_python compiled input { 100000000 1000000 } output { 1783293664i32 }

let main (n: i32) (m: i32) =
  reduce_by_index (replicate n 0) (+) 0 (map (%n) (iota m)) (iota m) |> i32.sum
