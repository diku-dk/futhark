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
-- structure distributed { DoLoop/MapKernel 3 DoLoop 2 Map 0}

fun [f64] combineVs([f64] n_row, [f64] vol_row, [f64] dr_row) =
    map(+, zip(dr_row, map(*, zip(n_row, vol_row ) )))

fun [[f64,num_und],num_dates]
  mkPrices([f64,num_und] md_starts, [[f64,num_und],num_dates] md_vols,
	   [[f64,num_und],num_dates] md_drifts, [[f64,num_und],num_dates] noises) =
  let e_rows = map( fn [f64] ([f64] x) =>
                      map(exp64, x)
                  , map(combineVs, zip(noises, md_vols, md_drifts)))
  in  scan( fn [f64] ([f64] x, [f64] y) =>
              map(*, zip(x, y))
          , md_starts, e_rows )

fun [[[f64]]] main(int n,
                    [[f64]] md_vols,
                    [[f64]] md_drifts,
                    [f64]  md_starts,
                    [[[f64]]] noises_mat) =
  loop (noises_mat) = for i < n do
    map (fn [[f64]] ([[f64]] noises) =>
           mkPrices(md_starts, md_vols, md_drifts, noises),
         noises_mat) in
  noises_mat
