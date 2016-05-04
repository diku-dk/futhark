-- Expected distributed/sequentialised structure:
--
-- map
--   map
--     sequential...
--
-- ==
--
-- structure distributed { MapKernel 1 DoLoop 2 }

fun [f64] take(int n, [f64] a) = let (first, rest) = unsafe split((n), a) in first

fun [f64] fftmp(int num_paths, [[f64]] md_c, [f64] zi) =
    map( fn f64 (int j) =>
            let x = map(*, zip(take(j+1,zi), take(j+1,unsafe md_c[j])) )
                in  reduce(+, 0.0, x)
         , iota(num_paths)
       )

fun [[f64]] correlateDeltas(int num_paths, [[f64]] md_c, [[f64]] zds) =
    map( fftmp(num_paths, md_c), zds )

fun [[[f64]]] main(int num_paths, [[f64]] md_c, [[[f64]]] bb_mat) =
  map (fn [[f64]] ([[f64]] bb_arr) =>
         correlateDeltas(num_paths, md_c, bb_arr),
      bb_mat)
