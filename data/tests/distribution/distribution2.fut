-- A combination of distribution0.fut and distribution1.fut.  AKA the
-- blackScholes computation from GenericPricing.
--
-- ==
-- structure distributed {
--   Kernel 2
--   DoLoop 2
--   Reduce 0
--   Redomap 0
-- }

fun take(n: int, a: []f64): []f64 = let (first, rest) = unsafe split((n), a) in first

fun fftmp(num_paths: int, md_c: [][]f64, zi: []f64): []f64 =
    map( fn (j: int): f64  =>
            let x = map(*, zip(take(j+1,zi), take(j+1,unsafe md_c[j])) )
                in  reduce(+, 0.0, x)
         , iota(num_paths)
       )

fun correlateDeltas(num_paths: int, md_c: [][]f64, zds: [][]f64): [][]f64 =
    map( fftmp(num_paths, md_c), zds )

fun combineVs(n_row: []f64, vol_row: []f64, dr_row: []f64): []f64 =
    map(+, zip(dr_row, map(*, zip(n_row, vol_row ) )))

fun mkPrices(md_starts: [num_und]f64, md_vols: [num_dates][num_und]f64,
	   md_drifts: [num_dates][num_und]f64, noises: [num_dates][num_und]f64): [num_dates][num_und]f64 =
    let e_rows = map( fn (x: []f64): []f64  => map(exp64, x),
                      map(combineVs, zip(noises, md_vols, md_drifts))
                    )
    in  scan( fn (x: []f64, y: []f64): []f64  => map(*, zip(x, y)),
              md_starts, e_rows )

--[num_dates, num_paths]
fun main(num_paths: int,
                    md_c: [][]f64,
                    md_vols: [][]f64,
                    md_drifts: [][]f64,
                    md_starts: []f64,
                    bb_mat: [][][]f64): [][][]f64 =
  map (fn (bb_row: [][]f64): [][]f64  =>
         let noises = correlateDeltas(num_paths, md_c, bb_row)
         in  mkPrices(md_starts, md_vols, md_drifts, noises),
       bb_mat)
