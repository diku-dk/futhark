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
-- tags { no_opencl }
-- structure distributed { DoLoop/MapKernel 4 DoLoop 2 Map 0}

fun [real] combineVs([real] n_row, [real] vol_row, [real] dr_row) =
    map(+, zip(dr_row, map(*, zip(n_row, vol_row ) )))

fun [[real]] mkPrices(  [real]  md_starts, [[real]] md_vols,
		       [[real]] md_drifts, [[real]] noises ) =
  let e_rows = map( fn [real] ([real] x) =>
                      map(exp, x)
                  , map(combineVs, zip(noises, md_vols, md_drifts)))
  in  scan( fn [real] ([real] x, [real] y) =>
              map(*, zip(x, y))
          , md_starts, e_rows )

fun [[[real]]] main(int n,
                    [[real]] md_vols,
                    [[real]] md_drifts,
                    [real]  md_starts,
                    [[[real]]] noises_mat) =
  loop (noises_mat) = for i < n do
    map (fn [[real]] ([[real]] noises) =>
           mkPrices(md_starts, md_vols, md_drifts, noises),
         noises_mat) in
  noises_mat
