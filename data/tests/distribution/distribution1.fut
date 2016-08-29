-- Expected distributed/sequentialised structure:
--
-- map
--   map
--     map
--
-- map
--   map
--     map
-- map
--   map
--     scan
--
-- ==
-- structure distributed { Kernel 2 }

fun combineVs(n_row: []f64, vol_row: []f64, dr_row: []f64): []f64 =
    zipWith (+) dr_row (zipWith (*) n_row vol_row)

fun mkPrices(md_starts: [num_und]f64, md_vols: [num_dates][num_und]f64,
	   md_drifts: [num_dates][num_und]f64, noises: [num_dates][num_und]f64): [num_dates][num_und]f64 =
  let e_rows = map (fn (x: []f64): []f64  =>
                      map exp64 x
                  ) (map combineVs (zip noises (md_vols) (md_drifts)))
  in  scan (fn (x: []f64) (y: []f64): []f64  =>
              zipWith (*) x y)
              md_starts e_rows

--[num_dates, num_paths]
fun main(md_vols: [][]f64,
                  md_drifts: [][]f64,
                  md_starts: []f64,
                  noises_mat: [][][]f64): [][][]f64 =
  map  (fn (noises: [][]f64): [][]f64  =>
         mkPrices(md_starts, md_vols, md_drifts, noises)) (
       noises_mat)
