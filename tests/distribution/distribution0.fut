-- Expected distributed/sequentialised structure:
--
-- map
--   map
--     sequential...
--
-- ==
--
-- structure gpu { SegMap 1 Loop 2 }

def fftmp (num_paths: i64) (md_c: [][]f64) (zi: []f64): [num_paths]f64 =
  #[incremental_flattening(only_outer)]
    map (\(j: i64): f64  ->
            let x = map2 (*) (take(j+1) zi) (take (j+1) md_c[j])
            in  reduce (+) (0.0) x
         ) (iota(num_paths)
       )

def correlateDeltas [n] (num_paths: i64) (md_c: [n][]f64) (zds: [][]f64): [n][num_paths]f64 =
  #[incremental_flattening(only_inner)]
  map (fftmp num_paths md_c) zds

def main (num_paths: i64) (md_c: [][]f64) (bb_mat: [][][]f64): [][][]f64 =
  #[incremental_flattening(only_inner)]
  map (\bb_arr -> correlateDeltas num_paths md_c bb_arr)
      bb_mat
