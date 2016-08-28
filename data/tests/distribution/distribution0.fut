-- Expected distributed/sequentialised structure:
--
-- map
--   map
--     sequential...
--
-- ==
--
-- structure distributed { Kernel 1 DoLoop 2 }

fun take(n: int, a: []f64): []f64 = let (first, rest) = unsafe split (n) a in first

fun fftmp(num_paths: int, md_c: [][]f64) (zi: []f64): []f64 =
    map( fn (j: int): f64  =>
            let x = map((*), zip(take(j+1,zi), take(j+1,unsafe md_c[j])) )
                in  reduce((+), 0.0, x)
         , iota(num_paths)
       )

fun correlateDeltas(num_paths: int, md_c: [][]f64, zds: [][]f64): [][]f64 =
    map( fftmp(num_paths, md_c), zds )

fun main(num_paths: int, md_c: [][]f64, bb_mat: [][][]f64): [][][]f64 =
  map (fn (bb_arr: [][]f64): [][]f64  =>
         correlateDeltas(num_paths, md_c, bb_arr),
      bb_mat)
