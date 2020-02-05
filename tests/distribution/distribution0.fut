-- Expected distributed/sequentialised structure:
--
-- map
--   map
--     sequential...
--
-- ==
--
-- structure distributed { SegMap 1 DoLoop 2 }

let fftmp (num_paths: i32) (md_c: [][]f64) (zi: []f64): [num_paths]f64 =
    map (\(j: i32): f64  ->
            let x = map2 (*) (unsafe take(j+1) zi) (unsafe take (j+1) (unsafe md_c[j]))
            in  reduce (+) (0.0) x
         ) (iota(num_paths)
       )

let correlateDeltas [n] (num_paths: i32) (md_c: [n][]f64) (zds: [][]f64): [n][num_paths]f64 =
    map (fftmp num_paths md_c) zds

let main (num_paths: i32) (md_c: [][]f64) (bb_mat: [][][]f64): [][][]f64 =
  map (\bb_arr -> correlateDeltas num_paths md_c bb_arr)
      bb_mat
