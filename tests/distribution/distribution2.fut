-- A combination of distribution0.fut and distribution1.fut.  AKA the
-- blackScholes computation from GenericPricing.
--
-- ==
-- structure distributed {
--   Kernel 1
--   DoLoop 2
-- }

import "/futlib/math"

let take(n: i32, a: []f64): []f64 = let (first, rest) = unsafe split (n) a in first

let fftmp(num_paths: i32, md_c: [][]f64) (zi: []f64): []f64 =
    map (\(j: i32): f64  ->
            let x = map (*) (take(j+1,zi)) (take(j+1,unsafe md_c[j]))
            in  reduce (+) (0.0) x
         ) (iota(num_paths)
       )

let correlateDeltas(num_paths: i32, md_c: [][]f64, zds: [][]f64): [][]f64 =
    map (fftmp(num_paths, md_c)) zds

let combineVs(n_row: []f64, vol_row: []f64, dr_row: []f64): []f64 =
    map (+) dr_row (map (*) n_row vol_row)

let mkPrices [num_und][num_dates]
          (md_starts: [num_und]f64, md_vols: [num_dates][num_und]f64,
	   md_drifts: [num_dates][num_und]f64, noises: [num_dates][num_und]f64): [num_dates][num_und]f64 =
    let e_rows = map (\(x: []f64): []f64  -> map f64.exp x) (
                      map combineVs (zip noises (md_vols) (md_drifts))
                    )
    in  scan (\(x: []f64) (y: []f64): []f64  -> map (*) x y) (
              md_starts) (e_rows )

--[num_dates, num_paths]
let main(num_paths: i32,
                    md_c: [][]f64,
                    md_vols: [][]f64,
                    md_drifts: [][]f64,
                    md_starts: []f64,
                    bb_mat: [][][]f64): [][][]f64 =
  map  (\(bb_row: [][]f64): [][]f64  ->
         let noises = correlateDeltas(num_paths, md_c, bb_row)
         in  mkPrices(md_starts, md_vols, md_drifts, noises)) (
       bb_mat)
