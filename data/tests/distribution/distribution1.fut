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
-- structure distributed { MapKernel 3 }

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

--[num_dates, num_paths]
fun [[[f64]]] main([[f64]] md_vols,
                  [[f64]] md_drifts,
                  [f64]  md_starts,
                  [[[f64]]] noises_mat) =
  map (fn [[f64]] ([[f64]] noises) =>
         mkPrices(md_starts, md_vols, md_drifts, noises),
       noises_mat)
