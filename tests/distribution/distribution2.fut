-- A combination of distribution0.fut and distribution1.fut.  AKA the
-- blackScholes computation from GenericPricing.
--
-- ==
-- structure distributed {
--   SegMap 1
--   DoLoop 2
-- }


let fftmp (num_paths: i32) (md_c: [][]f64) (zi: []f64): [num_paths]f64 =
    map (\(j: i32): f64  ->
            let x = map2 (*) (unsafe take (j+1) zi) (unsafe take (j+1) (unsafe md_c[j]))
            in  reduce (+) (0.0) x
         ) (iota num_paths)

let correlateDeltas [n] (num_paths: i32) (md_c: [][]f64) (zds: [n][]f64): [n][num_paths]f64 =
    map (fftmp num_paths md_c) zds

let combineVs [n] (n_row: [n]f64, vol_row: [n]f64, dr_row: [n]f64): [n]f64 =
    map2 (+) dr_row (map2 (*) n_row vol_row)

let mkPrices [num_und][num_dates]
          (md_starts: [num_und]f64, md_vols: [num_dates][num_und]f64,
	   md_drifts: [num_dates][num_und]f64, noises: [num_dates][num_und]f64): [num_dates][num_und]f64 =
    let e_rows = map (\(x: []f64)  -> map f64.exp x) (
                      map combineVs (zip3 noises (md_vols) (md_drifts))
                    )
    in  scan (\(x: []f64) (y: []f64)  -> map2 (*) x y) (
              md_starts) (e_rows )

--[num_dates, num_paths]
let main(num_paths: i32)
        (md_c: [][]f64)
        (md_vols: [][]f64)
        (md_drifts: [][]f64)
        (md_starts: []f64)
        (bb_mat: [][][]f64): [][][]f64 =
  map (\(bb_row: [][]f64) ->
         let noises = correlateDeltas num_paths md_c bb_row
         in  mkPrices(md_starts, md_vols, md_drifts, noises))
       bb_mat
