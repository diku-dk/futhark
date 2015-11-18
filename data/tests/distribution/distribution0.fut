-- Expected distributed/sequentialised structure:
--
-- map
--   map
--     sequential...
--
-- ==
--
-- structure distributed { Kernel 1 DoLoop 2 }

fun [real] take(int n, [real] a) = let {first, rest} = split((n), a) in first

fun [real] fftmp(int num_paths, [[real]] md_c, [real] zi) =
    map( fn real (int j) =>
            let x = map(*, zip(take(j+1,zi), take(j+1,md_c[j])) )
                in  reduce(+, 0.0, x)
         , iota(num_paths)
       )

fun [[real]] correlateDeltas(int num_paths, [[real]] md_c, [[real]] zds) =
    map( fftmp(num_paths, md_c), zds )

fun [[[real]]] main(int num_paths, [[real]] md_c, [[[real]]] bb_mat) =
  map (fn [[real]] ([[real]] bb_arr) =>
         correlateDeltas(num_paths, md_c, bb_arr),
      bb_mat)
