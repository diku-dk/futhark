-- A combination of distribution0.fut and distribution1.fut.  AKA the
-- blackScholes computation from GenericPricing.
--
-- ==
-- tags { no_opencl }
-- structure distributed {
--   MapKernel 4
--   Map 0
--   DoLoop 3
--   Reduce 0
--   Redomap 0
-- }

fun [real] take(int n, [real] a) = let {first, rest} = split((n), a) in first

fun [real] fftmp(int num_paths, [[real]] md_c, [real] zi) =
    map( fn real (int j) =>
            let x = map(*, zip(take(j+1,zi), take(j+1,md_c[j])) )
                in  reduce(+, 0.0, x)
         , iota(num_paths)
       )

fun [[real]] correlateDeltas(int num_paths, [[real]] md_c, [[real]] zds) =
    map( fftmp(num_paths, md_c), zds )

fun [real] combineVs([real] n_row, [real] vol_row, [real] dr_row) =
    map(+, zip(dr_row, map(*, zip(n_row, vol_row ) )))

fun [[real]] mkPrices(  [real]  md_starts, [[real]] md_vols,
                       [[real]] md_drifts, [[real]] noises ) =
    let e_rows = map( fn [real] ([real] x) => map(exp, x),
                      map(combineVs, zip(noises, md_vols, md_drifts))
                    )
    in  scan( fn [real] ([real] x, [real] y) => map(*, zip(x, y)),
              md_starts, e_rows )

--[num_dates, num_paths]
fun [[[real]]] main(int        num_paths,
                    [[real]]   md_c,
                    [[real]]   md_vols,
                    [[real]]   md_drifts,
                    [real]     md_starts,
                    [[[real]]] bb_mat) =
  map (fn [[real]] ([[real]] bb_row) =>
         let noises = correlateDeltas(num_paths, md_c, bb_row)
         in  mkPrices(md_starts, md_vols, md_drifts, noises),
       bb_mat)
