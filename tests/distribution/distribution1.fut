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
-- structure distributed { SegMap 1 }

let combineVs [n] (n_row: [n]f64, vol_row: [n]f64, dr_row: [n]f64): [n]f64 =
    map2 (+) dr_row (map2 (*) n_row vol_row)

let mkPrices [num_und] [num_dates]
          (md_starts: [num_und]f64, md_vols: [num_dates][num_und]f64,
	   md_drifts: [num_dates][num_und]f64, noises: [num_dates][num_und]f64): [num_dates][num_und]f64 =
  let e_rows = map (\(x: []f64)  ->
                      map f64.exp x
                  ) (map combineVs (zip3 noises (md_vols) (md_drifts)))
  in  scan (\(x: []f64) (y: []f64)  ->
              map2 (*) x y)
              md_starts e_rows

--[#num_dates, num_paths]
let main(md_vols: [][]f64,
         md_drifts: [][]f64,
         md_starts: []f64,
         noises_mat: [][][]f64): [][][]f64 =
  map  (\(noises: [][]f64) ->
         mkPrices(md_starts, md_vols, md_drifts, noises)) (
       noises_mat)
