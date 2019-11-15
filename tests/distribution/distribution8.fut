-- Like distribution2.fut, but with an outer sequential loop.  Does
-- not compute anything meaningful.
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
-- structure distributed { DoLoop/SegMap 1 DoLoop 2 }


let combineVs [n] (n_row: [n]f64, vol_row: [n]f64, dr_row: [n]f64): [n]f64 =
    map2 (+) dr_row (map2 (*) n_row vol_row)

let mkPrices [num_und][num_dates]
          (md_starts: [num_und]f64, md_vols: [num_dates][num_und]f64,
	   md_drifts: [num_dates][num_und]f64, noises: [num_dates][num_und]f64): [num_dates][num_und]f64 =
  let e_rows = map (\(x: []f64)  ->
                      map f64.exp x
                  ) (map combineVs (zip3 noises (md_vols) (md_drifts)))
  in  scan (\(x: []f64) (y: []f64)  ->
              map2 (*) x y)
              md_starts e_rows

let main(n: i32,
                    md_vols: [][]f64,
                    md_drifts: [][]f64,
                    md_starts: []f64,
                    noises_mat: [][][]f64): [][][]f64 =
  loop (noises_mat) for i < n do
    map  (\(noises: [][]f64) ->
           mkPrices(md_starts, md_vols, md_drifts, noises)) (
         noises_mat)
